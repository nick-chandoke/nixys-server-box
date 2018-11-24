{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO: learn how to use HTTP/2-specific functions in Network.Wai.Handler.Warp? To what extent are these used automatically or are not so useful for me?
-- | Run a warp server, and routing types & methods
module ServerBox
( Route(..)
, stdwarp
, stdwarpLocal
, stdSettings
, stdHeaders
, setDomain
, onMethod
, onPath
, static
, staticOn
) where

-- base
import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Prelude hiding (FilePath)
import System.Environment (lookupEnv)

-- NicLib
import NicLib.Errors (err)

-- text
import qualified Data.Text as T'

import System.FilePath (pathSeparator) -- filepath
import Control.Monad.Trans.Except (ExceptT, runExceptT) -- transformers
import Data.Semigroup (Semigroup, (<>)) -- semigroups
import Data.Streaming.Network.Internal (HostPreference (Host)) -- streaming-commons
import Network.HTTP.Types (Method, hLocation, methodNotAllowed405, movedPermanently301, methodGet, ok200, noContent204, notImplemented501) -- http-types
import Network.Wai (Middleware, Request, Response, Application, rawPathInfo, requestMethod, pathInfo, responseLBS, responseFile) -- wai
import Network.Wai.Handler.Warp (run, runSettings, Settings, setServerName, defaultSettings, setHost, setTimeout, setProxyProtocolNone, setPort) -- warp
import Network.Wai.Handler.WarpTLS (runTLS, TLSSettings) -- warp-tls

-- wai-extra
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.ForceSSL (forceSSL)
import Network.Wai.Middleware.ForceDomain (forceDomain)
import Network.Wai.Middleware.Local (local)

import qualified Data.ByteString.Char8 as BS'
import qualified Data.ByteString.Lazy.Char8 as BS

{- | Route a 'Request' to a 'Response'. Compose routes with @(<>)@. This is the same as composing @ExceptT (Maybe Response) m Response@'s with @(\<|\>)@.

Obviously, do @pure resp@ to return a successful response.

@err (Just resp)@ returns a response that is unsuccessful. This is used by functions like @onMethod@ to exit route computations early.

@err Nothing@ will exit the route computation and try the next route.

Thus, a route to pass to @stdwarp@ may be:

@
    import Network.Socket (SockAddr(..)) -- network package
    let route1 req = do
            'onPath' "\/path1" req -- tries next route if requested path is not "\/path1" (returns err Nothing)
            'onMethod' methodGet req -- returns 405 if unequal methods (returns err (Just 405 response))
            pure $ 'responseLBS' ok200 [] "You're on page 1!" -- successful (normal) return!
        route2 req = do
            -- return error response if not from a particular IP address
            when
                (case remoteHost req of SockAddrInet _ addr -> addr /= tupleToHostAddress (72,181,148,78))
                (err . Just $ responseLBS forbidden403 [] "Invalid IP address.")
            pure $ responseFile ok200 [] "\/dont\/use\/static\/filepaths.txt" Nothing
    in 'stdwarp' Nothing defaultSettings id $ route1 <> route2
@

As you can see, this makes arbitrary routes arbitrarily composable, allowing for failure.
-}
newtype Route m = Route { rte :: Request -> ExceptT (Maybe Response) m Response }

-- ⊥-const monoid. Used in the Semigroup instance of @Route@ (the Alternative instance for @ExceptT e m a@, requires that @e@ be a monoid; this imples that @Maybe Response@ must be a monoid, thus implying that @Response@ must be a monoid.)
-- It can afford to be ⊥ because it's only used in searching for a route, which always ends on @mempty@ (see @routeToApp@) - notImplemented501 - in the @stdwarp@ function; thus the value of the monoid (Left Response's) is never dereferenced.
-- This being said, don't confuse the Response's semigroup instance with (a -> f b)'s semigroup instance!
-- If you're modifying StdWarp, you may want to change this instance.
instance Semigroup Response where (<>) = undefined
instance Monoid Response where
    mempty = responseLBS notImplemented501 mempty mempty

-- One cannot use Data.Monoid.Alt because Kleisli morphisms do not compose under that definition. Also, though we're non-associatively composing Kleisli's, this chaining is done by applying a common object (Request) to all in the sequence, then composing them via (<|>), not (>>=). In short: this composition looks like it can be expressed in terms of common objects, but Monoid is the most suitable object for implementing this manner of composition.
-- | Compose Alternative Kleisli's under @(\<|\>)@.
instance Monad m => Semigroup (Route m) where
    Route f <> Route g = Route $ \a -> f a <|> g a

instance Monad m => Monoid (Route m) where
    mempty = Route $ const (pure mempty)

-- In regards to Response's Semigroup instance, (<> mempty) is used here in routeToApp to guarantee that we don't bottom-out.
-- Note that, by definition of 'Application', the route you provide here must be in IO. Seeing as routeToApp will always be the last step in preparing a route, I suppose that's OK.
routeToApp :: Route IO -> Application
routeToApp r = \req resp ->
    resp . either (fromMaybe mempty) id =<< runExceptT (rte (r <> mempty) $ req)

-- | Run a warp server on 80 and 443 (or ports given by environment variables PORT and PORT_SECURE), forcing TLS (if you're providing @TLSSettings@.) See @Route@ for an example of running stdwarp.
--
-- Remember that if you're not using 'Middleware', @id@ is the dummy Middleware.
--
-- A common Middleware is @gzip def . forceSSL . autohead@
--
-- NB. You must put 'forceSSL' in yourself, if you want to include it in your middleware for HTTPS connections (I'm not sure if doing so does any good, btw.) I do this because I can't make any assumptions about the order in which you combine your middlewares.
stdwarp :: Maybe TLSSettings -- ^ TLSSettings required if you're using HTTPS
        -> Settings
        -> Middleware
        -> Route IO
        -> IO ()
stdwarp mtls s mw r = do
    port <- maybe 80 read <$> lookupEnv "PORT"
    sec_port <- maybe 443 read <$> lookupEnv "PORT_SECURE"
    case mtls of
        Nothing ->
            void . runSettings (setPort port s) . mw . routeToApp $ r
        Just tls -> do
            void . forkIO . run port . forceSSL $ \_ resp -> resp (responseLBS noContent204 [] "")
            void . runTLS tls (setPort sec_port s) . addHeaders [("Strict-Transport-Security", "max-age=31536000; includeSubdomains")] . mw $ routeToApp r

-- | Run a Route on localhost on http (allows same-host-only connections, but traffic is still plaintext! Other applications on the host computer can read that traffic!)
--
-- You are expected to set the port in the @Settings@ parameter
stdwarpLocal :: Response -- ^ response for when a non-local connection attempts to connect to the server. Remember that @mempty@ is a fine dummy value
             -> Settings
             -> Middleware
             -> Route IO
             -> IO ()
stdwarpLocal lr s mw
    = runSettings s
    . local lr
    . addHeaders [("Access-Control-Allow-Origin", "null")] -- no-hassle AJAX (I recommend PureScript's Affjax module)
    . mw
    . routeToApp

-- | 'defaultSettings' + 'setTimeout' 10. Also disables proxy protocol and sets empty /Server/ header.
stdSettings :: Settings
stdSettings =
      setTimeout 10 -- our timeout should be so short anyway! Who waits 10s for a page to load? Long back-end operations should use AJAX, btw.
    . setProxyProtocolNone -- "Do not use the PROXY protocol." TODO: does this disable accessing the server via proxies?
    . setServerName mempty
    $ defaultSettings

stdHeaders :: [(BS'.ByteString, BS'.ByteString)]
stdHeaders = [ ("X-Frame-Options", "deny")
             ]

-- | Returns a 'Settings' modifier and 'Middleware' that together ...do something.... OK, it uses 'setHost' and 'forceDomain', and I'm not even quite sure what effect that has.
--
-- Also, for some reason the middleware returned here, along with 'forceSSL' in 'stdwarp''s port 80 handler, produces infinite redirect, even when connecting via https on 443, even if there's no forceSSL used on this line. So don't use the middleware returned from @setDomain@.
setDomain :: String -> (Settings -> Settings, Middleware)
setDomain d = (setHost (Host d), forceDomain $ let bseh = BS'.pack d in bool Nothing (Just bseh) . (==bseh))

-- | Match a method, returning 405 if the desired method is not the one in the request. Probably only use after @onPath@ or using pattern matching to identify a path.
--
-- @onMethod methodGet . Route $ \\req -> ⋯@ is a very common idiom
onMethod :: Monad m => Method -> Route m -> Route m
onMethod m (Route r) = Route $ \req ->
    if requestMethod req /= m then
        err . Just $ responseLBS methodNotAllowed405 [] ("This route accepts only " <> BS.fromStrict m)
    else r req

-- | Test for path equality, exiting the route to try the successor route, if unequal
onPath :: Monad m => BS'.ByteString -> Route m -> Route m
onPath m (Route r) = Route $ \req ->
    if rawPathInfo req /= m then
        err Nothing
    else r req

-- TODO: account for 404's here, prob. using bb's JSON API, or a HEAD request. Requires http-conduit.
-- TODO: maybe not include onMethod GET herein
-- | Host a static site where pages are predictably named and stored on a 3rd party server, e.g. S3 or BackBlaze
--
-- e.g. one may serve a static site from backblaze by @staticOn bb@, or S3 by @staticOn s3@.
--
-- Note that you may as well have all links. e.g. href and src attributes point directly to the static resources on the 3rd party server; you may use the endomorphisms for building webpages like this.
staticOn :: Monad m => (BS'.ByteString -> BS'.ByteString) -> Route m
staticOn f = onMethod methodGet . Route $ \req ->
    let url' = f (BS'.tail $ rawPathInfo req) -- tail b/c we don't want the leading slash
    in do
        pure $ responseLBS movedPermanently301 [(hLocation, url')] ("<!DOCTYPE html><html><head><title>Page Stored on 3rd Party Server</title></head><body>The page you requested is actually located at <a href='" <> BS.fromStrict url' <> "'></a></body></html>")

-- | Load a file from the requested path relative to a root path, on localhost
static :: Monad m => String -> Route m
static root = onMethod methodGet . Route $ \req ->
    let ps = pathInfo req
--      aboveRoot = isNothing $ foldl' (\mb a -> mb *>= \b -> let b' = b + (case a of ".." -> (-1); "." -> 0; _ -> 1) in if b' < 0 then Nothing else Just b') (Just 0) ps -- looks like warp already accounts for this :)
    in do
--      when aboveRoot (err . Just $ responseLBS forbidden403 [] "Requested path is ancestor of server's root directory!")
        pure $ responseFile ok200 [] (root <> T'.unpack (T'.intercalate (T'.singleton pathSeparator) ps)) Nothing -- responseFile automatically returns 404 if file not found, despite the ok200 here

-- Caching! --
{- | Insert a value into a Vault, creating a new Key in the process; return the key and vault.
-- because who ever creates a key without immediately using it to put in a value thereafter?
vput :: a -> Vault -> IO (Key a, Vault)
vput val vault = (\nk -> (nk, insert nk val vault)) <$> newKey -}

-- | There're two varieties of dynamic markup:
-- 1. Markup that's easier than Html (e.g. markdown, Lucid) that gets translated into Html, then served to a browser (e.g. CodeBox)
-- 2. Markup that contains values (e.g. database variables) that must be reified per-request. These are easily identified by requiring the IO monad.
-- 3. Markup that satisfies (1) and (2); it contains per-request dynamic variables AND is (at least mostly) a precursor to Html.
-- Markup of the first kind can be translated into Html at server startup, then cached in-memory for fast use. Markup of the second kind can be partially cached in memory; we can cache everything except the reified variables; thus the cached thing is an n-ary function.
