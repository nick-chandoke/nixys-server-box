-- TODO: learn how to use HTTP/2-specific functions in Network.Wai.Handler.Warp? To what extent are these used automatically or are not so useful?
-- | Run a warp server, and routing types & methods
module ServerBox
( -- * Types
  RouteArr(..)
, Route
, RouteFlag(Next, Short)
-- * Common Arrows
, constA
, guardA
, guardAM
, guardMA
, guardMAM
, boolE
, orShort
, orShortM
, next
, short
-- ** Routing
, routeByPath
, onMethod
, onPath
, getQuery
, plainResponse
, normalize
-- * Running the Server
, stdwarp
, stdwarpLocal
-- * Server Options
, stdSettings
, stdHeaders
, setDomain
-- * Sites
, static
, staticOn
-- , vput
) where

-- base
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Concurrent (forkIO)
import Control.Monad
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Bitraversable (bitraverse)
import Data.Functor.Compose
import Prelude hiding (FilePath, (.), id)

-- rio (and deps)
import Data.ByteString.Builder (Builder, toLazyByteString, charUtf8)
import Data.Foldable (foldl')
import RIO.ByteString (ByteString)
import RIO.ByteString.Lazy (toStrict, fromStrict)
import RIO.Text (Text)
import qualified Data.ByteString.Char8 as BSC
import qualified RIO.Map as M
import qualified RIO.Text as T

-- import Data.Vault.Lazy hiding (empty)
import System.FilePath (pathSeparator) -- filepath
import Data.Streaming.Network.Internal (HostPreference (Host)) -- streaming-commons
import Network.HTTP.Types (Method, hLocation, methodNotAllowed405, movedPermanently301, methodGet, ok200) -- http-types
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

{- | Routes may either fail with a response ('short',) or fail such that they try another route ('next'.) If a route fails with a response, then the computation short circuits immediately; (other failures that may have occured had the failure not been there) will occur once that failure is fixed. In other words, RouteArr does not yet support Validations-style error collecting. Obviously, this is under consideration and may be added in a later release.

=== Example

@
import Network.Socket (SockAddr(..)) -- in the package called "network"
let route1 =  'onPath' (=="\/path1") >>>
              ('onMethod' (==methodGet)  >>> constA (responseLBS ok200 [] "You're getting page 1!"))
          <+> ('onMethod' (==methodPost) >>> constA (responseLBS ok200 [] "You're posting to page 1!"))
    guardIP = guardA -- return error response if not from a particular IP address
        (responseLBS forbidden403 [] "Banned IP address.")
        (\\(remoteHost -> SockAddrInet _ addr) -> addr `elem` bannedAddrs)
    route2 = guardIP >>> static "/approot/filesystem/"
in 'stdwarp'
    Nothing -- no TLS; we're doing HTTP only
    (setPort 8080 stdSettings)
    id -- no middleware
    (responseLBS notImplemented501 mempty mempty)
    (route1 \<+\> route2) -- try route1; if it deferrs to route2, then try route2. If route2 deferrs, then respond with the above HTTP 501.
@

This translates to the following routing:

1. If path is @/path1@, respond with a "getting" or "posting" response on the respective methods.
2. If path is not @/path1@, or method is neither GET nor POST, then check if the IP address is banned; if so, return a response saying so; else serve files from the local filesystem.

-}
newtype RouteArr m a b = RouteArr { runRoute :: a -> Compose m RouteFlag b } deriving (Functor)
type Route m = RouteArr m Request Response

data RouteFlag a
    = Next -- ^ try the next route (implemented by @(<+>)@)
    | Short Response -- ^ stop route computation and respond (implemented by @(.)@)
    | Route a -- ^ usual routing (implemented by @(.)@)
    deriving (Functor)

instance Applicative RouteFlag where
    pure = Route
    Route f <*> Route x = Route (f x)
    -- we can't apply a function to either a Next or Short; thus simply return the Next or Short
    Route _ <*> Next = Next
    Route _ <*> Short resp = Short resp
    -- neither Next nor Short can contain functions; thus they must short, ignoring the right argument.
    Next <*> _ = Next
    Short resp <*> _ = Short resp
    -- TODO: consider Short a <*> Short b to accumulate à la validations Applicative. Accumulation could be, for example, adding partial responses to the <body> ofa DOM.

instance Alternative RouteFlag where
    empty = Next
    Next <|> x = x
    x <|> _ = x

-- currently unused. Should be used in the Category definiton below.
instance Monad RouteFlag where
    Route x >>= k = k x
    Next >>= _ = Next
    Short r >>= _ = Short r

-- is there any way to define this using the monad instance for RouteFlag? Probably not.
-- I'd need a function :: (Monad m, Monad n) => m (n a) -> (a -> m (n b)) -> m (n b)
instance Monad m => Category (RouteArr m) where
    id = RouteArr pure
    RouteArr g . RouteArr f = RouteArr $ \a -> Compose $ getCompose (f a) >>= \case
        Route x -> getCompose (g x)
        Short resp -> pure (Short resp)
        Next -> pure Next

instance Monad m => Arrow (RouteArr m) where
    arr = RouteArr . (pure .)
    RouteArr u *** RouteArr v = RouteArr $ \(b,d) -> liftA2 (,) (u b) (v d)

-- these monoidal arrow instances do not use Compose's Alternative instance. This is mainly in the interest of IO's Alternative
-- being that anything not fail/mzero/empty is OK, which is not what we want! We want to jump inside the category
-- to check for Next!
instance (Monad m) => ArrowZero (RouteArr m) where zeroArrow = RouteArr (const (Compose $ pure empty))
instance (Monad m) => ArrowPlus (RouteArr m) where
    RouteArr l <+> RouteArr r = RouteArr $ \a -> Compose (liftA2 (<|>) (getCompose $ l a) (getCompose $ r a))

-- trivial definition of `app`: unwraps, applies, then re-wraps the `RouteArr` newtype.
instance Monad m => ArrowApply (RouteArr m) where
    app = RouteArr (\(RouteArr a, b) -> a b)

instance Monad m => ArrowChoice (RouteArr m) where
    RouteArr a +++ RouteArr b = RouteArr (bitraverse a b)

-- | How is this not already in @Control.Arrow@? Like, so common, right?
constA :: Arrow a => c -> a b c
constA = arr . const

-- | On unsatisfied predicate, either try next route or immediately return a response.
--
-- Note that you cannot use @guardA@ to return a response if that response is a function of the arrow's input! For that, you should use 'onShort'.
guardA :: Monad m => RouteFlag b -> (b -> Bool) -> RouteArr m b b
guardA f p = RouteArr (\x -> Compose . pure $ bool f (Route x) $ p x)

-- | 'guardA' that accepts a Kleisli predicate
guardAM :: Monad m => RouteFlag b -> (b -> m Bool) -> RouteArr m b b
guardAM f p = RouteArr (\x -> Compose $ bool f (Route x) <$> p x)

-- | On @Nothing@, either try next route or immediately return a response.
--
-- Note that you cannot use @guardA@ to return a response if that response is a function of the arrow's input! For that, you should use 'onShort'.
guardMA :: Monad m => RouteFlag c -> (b -> Maybe c) -> RouteArr m b c
guardMA f p = RouteArr (Compose . pure . maybe f pure . p)

-- | 'guardMA' that accepts a Kleisli
guardMAM :: Monad m => RouteFlag c -> (b -> m (Maybe c)) -> RouteArr m b c
guardMAM f p = RouteArr (Compose . fmap (maybe f pure) . p)

-- | Useful when introducing conditions that may cause shorting
boolE :: a -- ^ value to return on False, in a Left
      -> b -- ^ value to return on True, in a Right
      -> Bool
      -> Either a b
boolE a b c = if c then Right b else Left a

{- | Convenience function. Usually useful with 'boolE', e.g.

@
orShort $ \req ->
    let resp = responseLBS ok200 [] ("You gave query params: " <> fromStrict (rawQueryString req))
    in boolE resp req (not $ null (queryString req))
@

* Note that it's easier to use 'guardA' if the response is not a function of the arrow's input.
* @orShort@ is a stupidly simple function, but the fact that it's such a common function/pattern says something about the nature of @RouteArr@.

-}
orShort :: Applicative m => (b -> Either Response c) -> RouteArr m b c
orShort f = RouteArr (Compose . pure . (Short ||| Route) . f)

orShortM :: Functor m => (b -> m (Either Response c)) -> RouteArr m b c
orShortM f = RouteArr (Compose . fmap (Short ||| Route) . f)

-- | 'Next' as an arrow. Seemingly is accounted for entirely by guardA(M) already, but just in case not, I'm leaving it here.
next :: Applicative m => RouteArr m a b
next = RouteArr (\_ -> Compose (pure Next))

-- | 'Short' as an arrow. Seemingly is accounted for entirely by orShort(M) already, but just in case not, I'm leaving it here.
short :: Applicative m => RouteArr m Response b -- yes, this type signature is correct, as odd as it looks.
short = RouteArr (Compose . pure . Short)

-- | Note that, by definition of 'Application', the route you provide here must be in ^IO@. Seeing as @routeToApp@ will always be the last step in preparing a route, that should be OK.
routeToApp :: Response -- ^ returned when the route given evaluates to "try next route"
           -> Route IO -- ^ route to convert
           -> Application
routeToApp defaultResp (RouteArr a) = \req resp -> getCompose (a req) >>= \case
    Route r -> resp r
    Short r -> resp r
    Next -> resp defaultResp

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
        -> Response -- ^ returned when the route given evaluates to "try next route"
        -> Route IO
        -> IO ()
stdwarp mtls s mw dr r = void $ case mtls of
    Nothing ->
        runSettings s . mw $ routeToApp dr r
    Just (tls, sec_port) -> do
        void . forkIO . run (getPort s) . forceSSL $ const ($ responseLBS ok200 [] "")
        runTLS tls (setPort sec_port s) . addHeaders [("Strict-Transport-Security", "max-age=31536000; includeSubdomains")] . mw $ routeToApp dr r

-- | Run a Route on localhost on http (allows same-host-only connections, but traffic is still plaintext! Other applications on the host computer can read that traffic!)
--
-- You are expected to set the port in the @Settings@ parameter
stdwarpLocal :: Response -- ^ response for when a non-local connection attempts to connect to the server. Remember that @mempty@ is a fine dummy value
             -> Settings
             -> Middleware
             -> Response -- ^ returned when the route given evaluates to "try next route"
             -> Route IO
             -> IO ()
stdwarpLocal lr s mw dr
    = runSettings s
    . local lr
    . addHeaders [("Access-Control-Allow-Origin", "null")] -- no-hassle AJAX
    . mw
    . routeToApp dr

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

-- | Try successor route if a path predicate fails.
--
-- @onPath (=="\/") >>> staticOn (const "https:\/\/mybucket.somecdn.com\/html\/home.html")@ is a common idiom for matching the homepage. This loads the page from the server if the CDN was down.
-- (By the way, if you redirect home, remember to select either "\/" or "\/home" as the canonical version.)
--
-- Note that the path is never null; it always either begins with, or entirely consists of, a forward-slash
onPath :: Monad m => (ByteString -> Bool) -> RouteArr m Request Request
onPath p = guardA Next (p . rawPathInfo)

-- | Modify a route so that it tries its successor route if a method predicate fails, returning 405 if the desired method is not the one in the request. Probably only use after @onPath@ or using pattern matching to identify a path.
onMethod :: Monad m => (Method -> Bool) -> RouteArr m Request Request
onMethod p = arr (\req ->
    let meth = requestMethod req
--      resp = responseLBS methodNotAllowed405 [] ("This route does not accept the HTTP " <> fromStrict meth <> " method.")
    in boolE undefined {- resp -} req (p meth)) >>> (next {- short -} ||| id)

-- | Retrieve a query parameter, failing the route if it's not found.
getQuery :: Monad m => ByteString -> RouteArr m Request ByteString
getQuery attr = arr (maybe (Left undefined) Right . lookup attr . queryString) >>> (next ||| arr (fromMaybe mempty))

-- | Host a static site where pages are predictably named and stored on a 3rd party server, e.g. S3 or BackBlaze, or some CDN. I'm going to assume it's a CDN. So, @staticOn@ 301-redirects to the "translated" location on the CDN.
--
-- For example, to serve on an AWS S3-compatible CDN, one may do
-- @let cdn = (\\p -\> "https:\/\/mybucket.somecdn.com" \<\> p) in staticOn@, which, would redirect, /e.g./ "\/\/mysite.com\/path1" to "https:\/\/mybucket.somecdn.com\/path1". *Note that the static URL does not end with a slash.* This doesn't always need to be the case, but you must always consider that the path being transformed (the @p@ lambda variable, in this example) /does/ begin with a leading slash!
--
-- === Suggestions
--
-- * have all links – /e.g./ @href@ and @src@ attributes – point directly to the static resources on the CDN
-- * you'll almost always want to use with @onMethod (==methodGet)@
-- * if you really are using a CDN to host all your (non-dynamically-generated) webpages (like you /should/,) then be sure to <https://support.google.com/webmasters/answer/139394?hl=en let search engines know that the pages are really yours>, rather than you just linking to someone else's page! Also, set access to /Public/ in your DigitalOcean spaces dashboard!
staticOn :: Monad m => (ByteString -> ByteString) -> Route m
staticOn f = arr (\req -> responseLBS movedPermanently301 [(hLocation, f $ rawPathInfo req)] mempty)

-- | Load a local file from the requested path, relative to a root path, on the local filesystem.
-- Only triggers if method is GET.
static :: Monad m
       => String -- ^ root path
       -> Route m
static root = onMethod (==methodGet)
           >>^ (\req -> responseFile ok200 [] (root <> T.unpack (T.intercalate (T.singleton pathSeparator) (pathInfo req))) Nothing) -- responseFile automatically returns 404 if file not found, despite the ok200 here. Below is code that guards against accessing ancestors of the root. It's currently unused b/c it seems that warp already accounts for that. However, just in case, I'm leaving it here.
-- aboveRoot = isNothing $ foldl' (\mb a -> mb >>= \b -> let b' = b + (case a of ".." -> (-1); "." -> 0; _ -> 1) in if b' < 0 then Nothing else Just b') (Just 0) (pathInfo req)
-- when aboveRoot (short $ responseLBS forbidden403 [] "Requested path is ancestor of server's root directory!")

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
-- For SEO, @routeByPath@ works with unary URLs only, considering that <https://support.google.com/webmasters/answer/7451184?hl=en hierarchical relations are important for SEO> (see the section "Navigation is important for search engines"). The parameter obviously goes in the query. This works for hackage and wikipedia, since articles aren't hierarchical. Note, in the <https://lh3.googleusercontent.com/FkXf1NMLRBDRD0-82WWHYCu7_nHxCzkUaMDFDAuGiFRYIrtgO3wqSJdtSFhpnyu3yeE=w314 above linked article's example>, that there's a false hierarchy: they're using time periods as hierarchies. Because hierarchies are defined by a total ordering on any branch, "news" and time period, or "price-guides" and time period cannot be hierarchically related, because (these) prepositional phrases compose commutatively, /e.g./ "news 1948" has identical meaning as "1948 news." Be careful to not assume false orderings.
routeByPath :: Monad m => Trie (Route m) -> Route m
routeByPath t = arr (\req ->
    let !path' = toStrict . toLazyByteString . foldl' (\b a -> b <> foldText a <> charUtf8 '/') mempty $ pathInfo req
    in case Trie.match t path' of
        Just (_, route, "/") -> (route,req)
        _ -> (next,req)) >>> app
  where
    foldText :: Text -> Builder
    foldText = T.foldl' (\b -> (b <>) . charUtf8) mempty

plainResponse :: Monad m => ByteString -> RouteArr m b Response
plainResponse s = constA (responseLBS ok200 [] (fromStrict s))

-- TODO: override 'rawPathInfo' and not 'pathInfo' for speed.
-- | A suggested request normalization function: removes trailing or multiple consecutive slashes, and nubs & sorts query parameters in lexiographic order by key.
--
-- This is good for many cases, but mostly serves as a reminder to normalize your URLs for SEO.
normalize :: Request -> Request
normalize r =
    r { pathInfo = foldr (\a b -> if T.null a then b else T.toLower a : b) [] $ pathInfo r
      , queryString = M.toList . M.fromList $ queryString r
      }

{-
-- | Insert a value into a Vault, creating a new Key in the process; return the key and vault.
-- because who ever creates a key without immediately using it to put in a value thereafter?
vput :: a -> Vault -> IO (Key a, Vault)
vput val vault = (\nk -> (nk, insert nk val vault)) <$> newKey
-}
