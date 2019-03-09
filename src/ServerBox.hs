{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO: consider adding Arrow instance to Route? This would be for prettier-looking route blocks than what semigroup provides.
-- TODO: learn how to use HTTP/2-specific functions in Network.Wai.Handler.Warp? To what extent are these used automatically or are not so useful?
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

-- req
-- import qualified Network.HTTP.Req as Req

import Control.Monad.Trans.Except (ExceptT(..), runExceptT) -- transformers
import qualified Data.Text as T' -- text
import System.FilePath (pathSeparator) -- filepath
import Data.Semigroup (Semigroup, (<>)) -- semigroups
import Data.Streaming.Network.Internal (HostPreference (Host)) -- streaming-commons
import Network.HTTP.Types (Method, hLocation, methodNotAllowed405, movedPermanently301, methodGet, ok200, noContent204, notImplemented501) -- http-types
import Network.Wai (Middleware, Request, Response, Application, rawPathInfo, requestMethod, pathInfo, responseLBS, responseFile) -- wai
import Network.Wai.Handler.Warp (run, runSettings, Settings, setServerName, defaultSettings, setHost, setTimeout, setProxyProtocolNone, setPort, getPort, Port) -- warp
import Network.Wai.Handler.WarpTLS (runTLS, TLSSettings) -- warp-tls

-- wai-extra
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.ForceSSL (forceSSL)
import Network.Wai.Middleware.ForceDomain (forceDomain)
import Network.Wai.Middleware.Local (local)

import qualified Data.ByteString.Char8 as BS'
import qualified Data.ByteString.Lazy.Char8 as BS

short = ExceptT . pure . Left

{- | Route a 'Request' to a 'Response'. Compose routes with @(<>)@. This is the same as composing @ExceptT (Maybe Response) m Response@'s with @(\<|\>)@.

Obviously, do @pure resp@ to return a successful response.

@err (Just resp)@ returns a response that is unsuccessful. This is used by functions like @onMethod@ to exit route computations early.

@err Nothing@ will exit the route computation and try the next route.

Thus, a route to pass to @stdwarp@ may be:

@
    import Network.Socket (SockAddr(..)) -- network package
    let route1 = Route $ \\req -\> do
            'onPath' (=="\/path1") req -- tries next route if requested path is not "\/path1" (returns err Nothing)
            'onMethod' (==methodGet) req -- returns 405 if unequal methods (returns err (Just 405 response))
            pure $ 'responseLBS' ok200 [] "You're on page 1!" -- successful (normal) return!
        route2 = Route $ \\req -\> do
            -- return error response if not from a particular IP address
            when
                (case remoteHost req of SockAddrInet _ addr -> addr \/= tupleToHostAddress (72,181,148,78))
                (err . Just $ responseLBS forbidden403 [] "Invalid IP address.")
            pure $ responseFile ok200 [] "\/dont\/use\/static\/filepaths.txt" Nothing
    in 'stdwarp' Nothing defaultSettings id $ route1 \<\> route2
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

-- | Run a warp server on 80 and 443 by default, forcing TLS for all connections if you provide @Just@ a @TLSSettings@. See @Route@ for an example of running stdwarp.
--
-- Remember that if you're not using 'Middleware', @id@ is the dummy Middleware.
--
-- A common Middleware is @gzip def . autohead@
--
-- btw I don't know if putting 'forceSSL' in your middleware does any good; it's already done if you pass-in @TLSSettings@ to @stdwarp@.
stdwarp :: Maybe (TLSSettings, Port) -- ^ TLSSettings required if you're using HTTPS
        -> Settings
        -> Middleware
        -> Route IO
        -> IO ()
stdwarp mtls s mw r = void $ case mtls of
    Nothing ->
        runSettings s . mw $ routeToApp r
    Just (tls, sec_port) -> do
        forkIO . run (getPort s) . forceSSL $ \_ resp -> resp (responseLBS ok200 [] "")
        runTLS tls (setPort sec_port s) . addHeaders [("Strict-Transport-Security", "max-age=31536000; includeSubdomains")] . mw $ routeToApp r

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
    . addHeaders [("Access-Control-Allow-Origin", "null")] -- no-hassle AJAX
    . mw
    . routeToApp

-- | 'defaultSettings' + 'setTimeout' 10. Also disables proxy protocol and sets empty @Server@ header.
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

-- | Modify a route so that it tries its successor route if a method predicate fails.
-- | Match a method, returning 405 if the desired method is not the one in the request. Probably only use after @onPath@ or using pattern matching to identify a path.
--
-- @onMethod (==methodGet) . Route $ \\req -> ⋯@ is a very common idiom.
onMethod :: Monad m => (Method -> Bool) -> Route m -> Route m
onMethod m (Route r) = Route $ \req -> let meth = requestMethod req in
    if m meth then r req
    else short . Just $ responseLBS methodNotAllowed405 [] ("This route does not accept the HTTP " <> BS.fromStrict meth <> " method.")

-- | Modify a route so that it tries its successor route if a path predicate fails.
--
-- @onPath (=="\/") . staticOn (const "https:\/\/mybucket.somecdn.com\/html\/home.html")@ is a common idiom for matching the homepage.
-- (By the way, if you redirect home, remember to select either "\/" or "\/home" as the canonical version.)
--
-- Note that the path is never null; it always either begins with, or entirely consists of, a forward-slash
onPath :: Monad m => (BS'.ByteString -> Bool) -> Route m -> Route m
onPath m (Route r) = Route $ \req -> if m (rawPathInfo req) then r req else short Nothing

-- | Host a static site where pages are predictably named and stored on a 3rd party server, e.g. S3 or BackBlaze, or some CDN. I'm going to assume it's a CDN. So, @staticOn@ 301-redirects to the "translated" location on the CDN.
--
-- For example, to serve on an AWS S3-compatible CDN, one may do
-- @let cdn = (\p -\> "https:\/\/mybucket.somecdn.com" \<\> p) in staticOn@, which, would redirect, /e.g./ "\/\/mysite.com\/path1" to "https:\/\/mybucket.somecdn.com\/path1". *Note that the static URL does not end with a slash.* This doesn't always need to be the case, but you must always consider that the path being transformed (the @p@ lambda variable, in this example) /does/ begin with a leading slash!
--
-- === Suggestions
--
-- * have all links – /e.g./ @href@ and @src@ attributes – point directly to the static resources on the CDN
-- * you'll almost alway want to use with @onMethod (==methodGet)@
-- * if you really are using a CDN to host all your (non-dynamically-generated) webpages (like you /should/,) then be sure to <https://support.google.com/webmasters/answer/139394?hl=en let search engines know that the pages are really yours>, rather than you just linking to someone else's page! Also, set access to /Public/ in your DigitalOcean spaces dashboard!
staticOn :: (BS'.ByteString -> BS'.ByteString)
--       -> Route IO -- ^ 404 response in case resource missing on static repository
         -> Route IO
staticOn f {- f04 -} = Route $ \req ->
    pure $ responseLBS movedPermanently301 [(hLocation, f $ rawPathInfo req)] mempty
    -- I considered sending a HEAD request to see if the file is present; else run a 404 route. But, eh.

-- | Load a local *file* from the requested path, relative to a root path, *on the local filesystem*.
-- Only triggers if method is GET.
static :: Monad m
       => String -- ^ root path
       -> Route m
static root = onMethod (==methodGet) . Route $ \req ->
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
-- Markup of the first kind can be translated into Html at server startup, then cached in-memory for fast use. Markup of the second kind can be partially cached in memory; we can cache everything except the reified variables; thus the cached thing is an n-ary function that returns the target markup (usually HTML.)
