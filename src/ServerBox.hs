{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO (now!): Partition this module into two modules: one that uses warp, and one that uses WAI only
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
, routeByPath
, normalize
, onMethod
, onPath
, static
, staticOn
, plainResponse
, vput
) where

-- base
import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Prelude hiding (FilePath)

-- rio (and deps)
import Data.ByteString.Builder (Builder, toLazyByteString, charUtf8)
import Data.Foldable (foldl')
import RIO.ByteString (ByteString)
import RIO.ByteString.Lazy (toStrict, fromStrict)
import RIO.Text (Text)
import qualified Data.ByteString.Char8 as BSC
import qualified RIO.Map as M
import qualified RIO.Text as T

import Data.Vault.Lazy
import Control.Monad.Trans.Except (ExceptT(..), runExceptT) -- transformers
import System.FilePath (pathSeparator) -- filepath
import Data.Streaming.Network.Internal (HostPreference (Host)) -- streaming-commons
import Network.HTTP.Types (Method, hLocation, methodNotAllowed405, movedPermanently301, methodGet, ok200, notImplemented501) -- http-types
import Network.Wai (Middleware, Request(..), Response, Application, rawPathInfo, requestMethod, pathInfo, responseLBS, responseFile) -- wai
import Network.Wai.Handler.Warp (run, runSettings, Settings, setServerName, defaultSettings, setHost, setTimeout, setProxyProtocolNone, setPort, getPort, Port) -- warp
import Network.Wai.Handler.WarpTLS (runTLS, TLSSettings) -- warp-tls
import qualified Data.Trie as Trie -- bytestring-trie
import Data.Trie (Trie)

-- wai-extra
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.ForceSSL (forceSSL)
import Network.Wai.Middleware.ForceDomain (forceDomain)
import Network.Wai.Middleware.Local (local)

short :: Applicative m => e -> ExceptT e m a
short = ExceptT . pure . Left

{- | Route a 'Request' to a 'Response'. Compose routes with @(<>)@. This is the same as composing @ExceptT (Maybe Response) m Response@'s with @(\<|\>)@.

* @pure resp@ returns a successful response.
* @short (Just resp)@ returns a response that is unsuccessful. This is used by functions like 'onMethod' to exit route computations early.
* @short Nothing@ will exit the route computation and try the next route.

=== Example

@
    import Network.Socket (SockAddr(..)) -- in the package called "network"
    let route1 = Route $ \\req -\> do
            'onPath' (=="\/path1") req -- tries next route if requested path is not "\/path1" (returns short Nothing)
            'onMethod' (==methodGet) req -- returns 405 if unequal methods (returns short (Just 405 response))
            pure $ 'responseLBS' ok200 [] "You're on page 1!" -- successful (normal) return!
        route2 = Route $ \\req -\> do
            -- return error response if not from a particular IP address
            when
                (case remoteHost req of SockAddrInet _ addr -> addr \/= tupleToHostAddress (72,181,148,78))
                (short . Just $ responseLBS forbidden403 [] "Invalid IP address.")
            pure $ responseFile ok200 [] "\/dont\/use\/static\/filepaths.txt" Nothing
    in 'stdwarp' Nothing defaultSettings id $ route1 \<\> route2
@

As you can see, this makes arbitrary routes arbitrarily composable, allowing for failure.
-}
newtype Route m = Route { rte :: Request -> ExceptT (Maybe Response) m Response }

-- ⊥-const monoid. Used in the Semigroup instance of @Route@ (the @Alternative@ instance for @ExceptT e m a@, requires that @e@ be a monoid; this imples that @Maybe Response@ must be a monoid, thus implying that @Response@ must be a monoid.)
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

-- | Run a warp server on 80 and 443 by default, forcing TLS for all connections if you provide @Just@ a @TLSSettings@. See 'Route' for an example of running stdwarp.
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
        void . forkIO . run (getPort s) . forceSSL $ \_ resp -> resp (responseLBS ok200 [] "")
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

stdHeaders :: [(ByteString, ByteString)]
stdHeaders = [ ("X-Frame-Options", "deny")
             ]

-- | Returns a 'Settings' modifier and 'Middleware' that together ...do something.... OK, it uses 'setHost' and 'forceDomain', and I'm not even quite sure what effect that has.
--
-- Also, for some reason the middleware returned here, along with 'forceSSL' in 'stdwarp''s port 80 handler, produces infinite redirect, even when connecting via https on 443, even if there's no forceSSL used on this line. So don't use the middleware returned from @setDomain@.
setDomain :: String -> (Settings -> Settings, Middleware)
setDomain d = (setHost (Host d), forceDomain $ let bseh = BSC.pack d in bool Nothing (Just bseh) . (==bseh))

-- | Modify a route so that it tries its successor route if a method predicate fails.
-- | Match a method, returning 405 if the desired method is not the one in the request. Probably only use after @onPath@ or using pattern matching to identify a path.
--
-- @onMethod (==methodGet) . Route $ \\req -> ⋯@ is a very common idiom.
onMethod :: Monad m => (Method -> Bool) -> Route m -> Route m
onMethod m (Route r) = Route $ \req -> let meth = requestMethod req in
    if m meth then r req
    else short . Just $ responseLBS methodNotAllowed405 [] ("This route does not accept the HTTP " <> fromStrict meth <> " method.")

-- | Modify a route so that it tries its successor route if a path predicate fails.
--
-- @onPath (=="\/") . staticOn (const "https:\/\/mybucket.somecdn.com\/html\/home.html")@ is a common idiom for matching the homepage.
-- (By the way, if you redirect home, remember to select either "\/" or "\/home" as the canonical version.)
--
-- Note that the path is never null; it always either begins with, or entirely consists of, a forward-slash
onPath :: Monad m => (ByteString -> Bool) -> Route m -> Route m
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
staticOn :: (ByteString -> ByteString)
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
--      when aboveRoot (short . Just $ responseLBS forbidden403 [] "Requested path is ancestor of server's root directory!")
        pure $ responseFile ok200 [] (root <> T.unpack (T.intercalate (T.singleton pathSeparator) ps)) Nothing -- responseFile automatically returns 404 if file not found, despite the ok200 here

-- | The most common way to route: by URL. This uses a trie for efficient storage and fast lookup. Namely, composing routes by @(\<\>)@\/@(\<|\>)@ is linearly complex, but routing on paths via a trie is probably of a logarithmic order.
--
-- === The Trie
--
-- * All items in the trie represent absolute URLs. None should lead nor end with a slash, since that'd be redundant. These are necessary preconditions.
-- * *It is recommended to 'normalize' your @Request@ before using @routeByPath@.*
--
-- Note that this only works on completely non-abstract URL paths; no variables are allowed in the URL, as the trie works on @ByteString@ only. Thus in order to use parameterized URLs, use query parameters rather than variable path segments. I know that's not commonly done, so if that's bad practice (other than "it's bad because the W3C gods decreed that query is for non-hierarchical data and paths /are/ for such data"), please let me know.
--
-- Also note that you might be able to use a trie to route variable paths, by using 'Trie.lookupBy' or 'Trie.match', but such methods quickly become messy, thus undermining the simple nature of ServerBox. Also, such methods are not general. However, if you really want parameterized paths with logarithmic lookup, it may be possible.
--
-- === Why Using Query Parameters as an Alternative to Parameterized Paths (Usually) Works
--
-- 1. @POST@ parameters go in request headers, so query parameters are not part of the URL, and are thus not considerable here.
-- 2. @GET@ parameters are good for bookmarking, so they /should/ be a part of the URL as normal.
-- 3. This should not be a concern as far as being indexed by search engines, since one can use the @canonical@ meta tag, which would be dynamically generated to reference a normalized URL, thus allowing /e.g./ hackage package pages or wikipedia article pages to be indexed non-redundantly.
-- 4. For dynamic pages that do not exist (e.g. a parameterized URL whose parameters are invalid, like a URL for a package that does not exist), dynamically generate the page with a <https://support.google.com/webmasters/answer/93710 @noindex@ meta tag or response header> so that crawlers will ignore them.
--
-- Thus most parameterized URLs can be parameterized by queries non-redundantly & canonically so that they can be bookmarked and crawled or not, thus being effectively equivalent to path-parameterized URLs. The only variety of URLs for which the variable-path\/query-param isomorphism breaks-down is those of arbitrarily-nested parameters, /i.e./ @⋯/param1/⋯/paramN@ for arbitrary /N/.
--
-- === SEO
--
-- For SEO, @routeByPath@ works with unary URLs only, considering that <https://support.google.com/webmasters/answer/7451184?hl=en hierarchical relations are important for SEO> (see the section "Navigation is important for search engines"). The parameter obviously goes in the query. This works for hackage and wikipedia, since articles aren't hierarchical. Note, in the <https://lh3.googleusercontent.com/FkXf1NMLRBDRD0-82WWHYCu7_nHxCzkUaMDFDAuGiFRYIrtgO3wqSJdtSFhpnyu3yeE=w314 above linked article's example>, that there's a false hierarchy: they're using time periods as hierarchies. Because hierarchies are defined by a total ordering on any branch, "news" and time period, or "price-guides" and time period cannot be hierarchically related, because (these) prepositional phrases compose commutatively, /e.g./ "the news in 1948" has identical meaning as "1948's news." Be careful to not assume false orderings.
routeByPath :: Monad m => Trie (Route m) -> Route m
routeByPath t = Route $ \req ->
    let !path' = toStrict . toLazyByteString . foldl' (\b a -> b <> foldText a <> charUtf8 '/') mempty $ pathInfo req
    in case Trie.match t path' of
        Just (_,Route route,"/") -> route req
        _ -> pure mempty
  where
    foldText :: Text -> Builder
    foldText = T.foldl' (\b -> (b <>) . charUtf8) mempty

-- TODO: override 'rawPathInfo' and not 'pathInfo' for speed.
-- | A suggested request normalization function: removes trailing or multiple consecutive slashes, and nubs & sorts query parameters in lexiographic order by key.
--
-- This is good for many cases, but mostly serves as a reminder to normalize your URLs for SEO.
normalize :: Request -> Request
normalize r =
    r { pathInfo = foldr (\a b -> if T.null a then b else T.toLower a : b) [] $ pathInfo r
      , queryString = M.toList . M.fromList $ queryString r
      }

-- | Mostly useful for testing. Always responds OK 200 with the provided string and no headers.
plainResponse :: Monad m => ByteString -> Route m
plainResponse bs = Route $ \_ -> pure $ responseLBS ok200 [] (fromStrict bs)

-- | Insert a value into a Vault, creating a new Key in the process; return the key and vault.
-- because who ever creates a key without immediately using it to put in a value thereafter?
vput :: a -> Vault -> IO (Key a, Vault)
vput val vault = (\nk -> (nk, insert nk val vault)) <$> newKey
