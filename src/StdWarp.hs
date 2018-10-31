{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO: learn how to use HTTP/2-specific functions in Network.Wai.Handler.Warp? To what extent are these used automatically or are not so useful for me?
-- | template for SaaS Warp servers (more general than just static sites)
module StdWarp
( Route
, Route'
, stdwarp
, stdwarpLocal
, webrootOn
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
import Control.Monad (when, void)
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Maybe (isNothing)
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
import Network.HTTP.Types (Method, hLocation, methodNotAllowed405, movedPermanently301, methodGet, ok200, noContent204, notImplemented501, forbidden403) -- http-types
import Network.Wai (Middleware, Request, Response, Application, rawPathInfo, requestMethod, pathInfo, responseLBS, responseFile) -- wai
import Network.Wai.Handler.Warp (run, runSettings, Settings, setServerName, defaultSettings, setHost, setTimeout, setProxyProtocolNone, setPort) -- warp
import Network.Wai.Handler.WarpTLS (runTLS, TLSSettings, tlsSettingsChain) -- warp-tls

-- wai-extra
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.ForceSSL (forceSSL)
import Network.Wai.Middleware.ForceDomain (forceDomain)
import Network.Wai.Middleware.Local (local)

import qualified Data.ByteString.Char8 as BS'
import qualified Data.ByteString.Lazy.Char8 as BS

{- | Route a @Request@ to a @Response@. Compose routes with @(<>)@. This is the same as composing @ExceptT (Maybe Response) m Response@'s with @(<|>)@.
Obviously, do @pure resp@ to return a successful response.
@err (Just resp)@ returns a response that is unsuccessful. This is used by functions like @onMethod@ to exit route computations early.
@err Nothing@ will exit the route computation and try the next route.
Thus, a route to pass to @stdwarp@ may be:
@
import Network.Socket (SockAddr (SockAddrInet), ) -- "network" package
let route1 req = do
        onPath "/path1" req -- tries next route if requested path is not "/path1" (returns err Nothing)
        onMethod methodGet req -- returns 405 if unequal methods (returns err (Just 405 response))
        pure $ responseLBS ok200 [] "You're on page 1!" -- successful (normal) return!
    route2 = do
        -- return error response if not from a particular IP address
        when
            (case remoteHost req of SockAddrInet _ addr -> addr /= tupleToHostAddress (72,181,148,78))
            (err . Just $ responseLBS forbidden403 [] "Invalid IP address.")
        pure $ responseFile ok200 [] "/dont/use/static/filepaths.txt" Nothing
in stdwarp Nothing defaultSettings id $ route1 <> route2
As you can see, this makes arbitrary routes arbitrarily composable, allowing for failure.
@
-}
type Route = Route' IO
type Route' m = Request -> ExceptT (Maybe Response) m Response

-- | ⊥-const monoid. Used in the Semigroup instance of @Route'@ (the Alternative instance for @ExceptT e m a@, requires that @e@ be a monoid; this imples that @Maybe Response@ must be a monoid, thus implying that @Response@ must be a monoid.)
-- It can afford to be ⊥ because it's only used in searching for a route, which always ends on Right 501 in @stdwarp@; thus the value of the monoid (Left Response's) is never dereferenced.
-- This being said, don't confuse the Response's semigroup instance with (a -> f b)'s semigroup instance!
-- If you're modifying StdWarp, you may want to change this instance.
instance Semigroup Response where (<>) = undefined
instance Monoid Response where
    mempty = responseLBS notImplemented501 mempty mempty

-- One cannot use Data.Monoid.Alt because Kleisli morphisms do not compose under that definition. Also, though we're non-associatively composing Kleisli's, this chaining is done by applying a common object (Request) to all in the sequence, then composing them via (<|>), not (>>=). In short: this composition looks like it can be expressed in terms of common objects, but Monoid is the most suitable object for implementing this manner of composition.
-- | Compose Alternative Kleisli's under @(<|>)@.
instance {-# OVERLAPPING #-} (Alternative f) => Semigroup (a -> f b) where
    f <> g = \a -> f a <|> g a

routeToApp :: Route -> Application
routeToApp r = \req resp -> resp . either (fromMaybe mempty) id =<< runExceptT (r <> (const . pure $ mempty) $ req)

-- | Support for Let's Encrypt's webroot plugin.
-- <https://certbot.eff.org/docs/using.html>
-- e.g. @case webrootOn "myhost.com" of (settings, webRootRoute) -> stdwarp (Just settings) defaultSettings id $ webRootRoute <|> myUsualRoutes@
webrootOn :: String -> (TLSSettings, Route)
webrootOn host = (tlsSettingsChain (certDir <> "cert.pem") [certDir <> "fullchain.pem"] (certDir <> "privkey.pem"), webrootAuth)
    where
        certDir = "/etc/letsencrypt/live/" <> host <> "/" -- I'm leaving '/' literals here as pathSeparator because I'm assuming *NIX functionality of webroot; I'm unsure whether webroot has the same setup in Windows
        webrootAuth req = onMethod methodGet req >> case pathInfo req of
            (".well-known":"acme-challenge":_) -> static certDir req
            _ -> err mempty

-- | run a warp server on 80 and 443 (or ports given by environment variables PORT and PORT_SECURE), forcing TLS (if you're providing @TLSSettings@.) See @Route@ for an example of running stdwarp.
-- Remember that if you're not using Middleware, @id@ is the dummy Middleware.
-- A common Middleware is @gzip def . forceSSL . autohead@
-- NB. You must put forceSSL in yourself, if you want to include it in your middleware for HTTPS connections (I'm not sure if doing so does any good, btw.) I do this because I can't make any assumptions about the order in which you combine your middlewares.
stdwarp :: Maybe TLSSettings -> Settings -> Middleware -> Route -> IO ()
stdwarp mtls s mw r = do
    port <- maybe 80 read <$> lookupEnv "PORT"
    sec_port <- maybe 443 read <$> lookupEnv "PORT_SECURE"
    case mtls of
        Nothing ->
            void . runSettings (setPort port s) . mw . routeToApp $ r
        Just tls -> do
            void . forkIO . run port . forceSSL $ \_ resp -> resp (responseLBS noContent204 [] "")
            void . runTLS tls (setPort sec_port s) . addHeaders [("Strict-Transport-Security", "max-age=31536000; includeSubdomains")] . mw $ routeToApp r

-- | Run a Route on localhost on http (insecure)
-- You are expected to set the port in the @Settings@ parameter
-- The @Response@ parameter is the response for when a non-local connection attempts to connect to the server
stdwarpLocal :: Response -> Settings -> Middleware -> Route -> IO ()
stdwarpLocal lr s mw = runSettings s . local lr . mw . routeToApp

-- | defaultSettings + setTimeout 10, disable proxy protocol, set empty Server header.
stdSettings :: Settings
stdSettings =
      setTimeout 10 -- our timeout should be so short anyway! Who waits 10s for a page to load? Long back-end operations should use AJAX, btw.
    . setProxyProtocolNone -- "Do not use the PROXY protocol." TODO: does this disable accessing the server via proxies?
    . setServerName mempty
    $ defaultSettings

stdHeaders :: [(BS'.ByteString, BS'.ByteString)]
stdHeaders = [ ("X-Frame-Options", "deny")
             ]

-- TODO: for some reason the middleware returned here, along with forceSSL in stdwarp's port 80 handler, produces infinite redirect, even when connecting via https on 443, even if there's no forceSSL used on this line.
setDomain :: String -> (Settings -> Settings, Middleware)
setDomain d = (setHost (Host d), forceDomain $ let bseh = BS'.pack d in bool Nothing (Just bseh) . (==bseh))

-- | Match a method, returning 405 if the desired method is not the one in the request. Probably only use after @onPath@ or using pattern matching to identify a path.
onMethod :: Monad m => Method -> Request -> ExceptT (Maybe Response) m ()
onMethod m req = when (requestMethod req /= m) . err . Just $ responseLBS methodNotAllowed405 [] ("This route accepts only " <> BS.fromStrict m)

-- | Test for path equality, exiting the route to try the successor route, if unequal
onPath :: Monad m => BS'.ByteString -> Request -> ExceptT (Maybe Response) m ()
onPath m req = when (rawPathInfo req /= m) (err Nothing)

-- | Host a static site where pages are predictably named and stored on a 3rd party server, e.g. S3 or BackBlaze
-- e.g. one may serve a static site from backblaze by @staticOn bb@, or S3 by @staticOn s3@
-- Note that you may as well have all links (e.g. href and src attributes point directly to the static resources on the 3rd party server; you may use the endomorphisms for building webpages like this)
-- TODO: account for 404's here, prob. using bb's JSON API, or a HEAD request. Requires http-conduit.
-- TODO: maybe not include onMethod GET herein
staticOn :: (BS'.ByteString -> BS'.ByteString) -> Route
staticOn f req = do
    onMethod methodGet req
    pure $ responseLBS movedPermanently301 [(hLocation, url')] ("<!DOCTYPE html><html><head><title>Page Stored on 3rd Party Server</title></head><body>The page you requested is actually located at <a href='" <> BS.fromStrict url' <> "'></a></body></html>")
    where
        url' = f (BS'.tail $ rawPathInfo req) -- tail b/c we don't want the leading slash

-- | load a file from the requested path relative to a root path, on localhost
static :: String -> Route
static root req =
    let ps = pathInfo req
--      aboveRoot = isNothing $ foldl' (\mb a -> mb >>= \b -> let b' = b + (case a of ".." -> (-1); "." -> 0; _ -> 1) in if b' < 0 then Nothing else Just b') (Just 0) ps -- looks like warp already accounts for this :)
    in do
        onMethod methodGet req
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
