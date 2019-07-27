-- should I merge into <https://github.com/inzva/DOH>?
-- | DigitalOcean things that I implement as necessary. Currently just common REST requests.
--
-- Convenience wrapper around the aws packages' functions that makes use plain & simple for non-aws users, as DigitalOcean tries for interoperability with AWS transactions.
--
-- === Exceptions to Account For
--
-- * 'HttpException'
-- * 'StatusCodeException' (esp. for 404's)
--
-- === ResourceT & Manager and Strictness
--
-- It's a good idea to 'seq' ('runResourceT ⋯) @(action on the output of the ResourceT)@. I suspect that this has to do with the combination of how @Manager@ automatically closes itself and how @ResourceT@ cleans-up after itself. See 'objIsDir' for further discussion.
--
-- NB. this module uses the DigitalOcean term /space/ rather than the more generic AWS term /bucket/; they're the same thing, though.
module ServerBox.Plugins.DigitalOcean
( DOConfig(..)
, mkDOConfig
, withDOConfig
, getObject
, putObject
, delObject
, headObject
, listSpace
, objIsDir
, is404
) where

import RIO

-- base
import Data.Maybe (fromMaybe)
import Control.Monad (void)
import Control.Applicative (liftA2)

-- imports from miscellaneous packages
import Conduit -- conduit
import Data.ByteString (ByteString) -- bytestring
import Network.HTTP.Client (RequestBody, newManager, HttpExceptionContent(StatusCodeException), Response(responseStatus)) -- http-client
import Network.HTTP.Client.TLS (tlsManagerSettings) -- http-client-tls
import Network.HTTP.Conduit (Manager, responseBody) -- http-conduit
import Network.HTTP.Types (statusCode) -- http-types

-- text
import Data.Text (Text)
import qualified Data.Text as T

-- aws
import qualified Aws
import qualified Aws.Core as Aws
import qualified Aws.S3 as S3

-- | Data needed for AWS calls. Initialize one @DOConfig@ at the beginning of your program.
-- Note that the arguments are ordered by necessity.
-- You may want to use various 'Credentials', but you'll only ever need one manager.
-- The endpoint isn't likely to stay the same if you change credentials, and
-- your endpoint isn't likely to change unless you change your space name too.
--
-- This makes @DOConfig@-as-a-partially-filled function useful.
data DOConfig = DOConfig
    { manager :: Manager
    , logger :: Aws.Logger
    , creds :: Aws.Credentials
    , endpoint :: ByteString -- ^ e.g. "nyc3.digitaloceanspaces.com". btw, for DigitalOcean Spaces, do /not/ use the URL that contains "cdn"; that will HTTP 403-out!
    }

-- | Create a @DOConfig@ without needing to specify a manager nor logger
--
-- To generate an access key and secret pair, go to the DigitalOcean dashboard, select "API" the left-hand navigation pane, then go to "Spaces access keys" and click "Generate new key." You'll want to copy the secret to a secure location immediately, as this is the only time it'll be given to you! (You can simply generate a new one if you need to, though.)
--
-- === Example
--
-- With @-XOverloadedStrings@ enabled:
--
-- @
-- (key, secret) <- readCredsFromDockerSecrets
-- mkDOConfig Nothing key secret  \<*\> pure "nyc3.digitaloceanspaces.com"
-- @
--
-- As always, do /not/ put your secrets in plantext in the executable! They can be read easily via @strings(1)@; this is what an awful idea looks like:
--
-- @mkDOConfig Nothing "JL0IED8TOFDMCQSO1402" "dC2kNFD2y2pIeeQtKhZkdMUaW341arsjBImtDTrwy/s" "nyc3.digitaloceanspaces.com"@
--
-- Even if you don't know Docker,...find /something/ else besides plaintext! Storing in envvars isn't acceptable, either!
mkDOConfig :: Maybe Aws.Logger -- ^ some or no logging
           -> ByteString -- ^ access key id
           -> ByteString -- ^ secret access key
           -> ByteString -- ^ endpoint
           -> IO DOConfig
mkDOConfig ml u p e = DOConfig <$> newManager tlsManagerSettings <*> pure (fromMaybe (\_ _ -> pure ()) ml) <*> (Aws.makeCredentials u p) <*> pure e

-- | Use common AWS S3 functions with a 'DOConfig'
-- see this module's source code for usage examples
withDOConfig :: (Aws.Transaction reqObj respObj
               , Aws.ServiceConfiguration reqObj ~ S3.S3Configuration)
             => DOConfig
             -> (Aws.Configuration -> Aws.ServiceConfiguration reqObj Aws.NormalQuery -> Manager -> reqObj -> ResourceT IO b) -- ^ either 'Aws.pureAws' or 'Aws.aws'
             -> reqObj
             -> ResourceT IO b
withDOConfig (DOConfig {manager, logger, creds, endpoint}) f = f
    (Aws.Configuration Aws.Timestamp creds logger Nothing)
    (S3.s3v4 Aws.HTTPS endpoint False S3.AlwaysSigned) -- False? What does "useURI" do anyway?
    manager

-- | Get an object as a ByteString
getObject :: DOConfig -- but this does!
          -> Text -- ^ space name
          -> Text -- ^ object name
          -> ResourceT IO (Maybe ByteString)
getObject cfg space obj = withDOConfig cfg Aws.pureAws (S3.getObject space obj) >>= \gor -> runConduit (responseBody (S3.gorResponse gor) .| await)

-- | Put on object in a space
-- @putObject@ catches @HttpException@s of constructor @HttpExceptionRequest@; I assume that InvalidUrlException should never be thrown, but if you want a foolproof application, you should account for that possibility.
putObject :: DOConfig
          -> Text -- ^ space name
          -> Text -- ^ object name
          -> RequestBody -- ^ payload
          -> ResourceT IO (Maybe Text)
putObject cfg space obj payload = S3.porVersionId <$> withDOConfig cfg Aws.pureAws (S3.putObject space obj payload)

-- | Deleet an object
delObject :: DOConfig
          -> Text -- ^ space name
          -> Text -- ^ object name
          -> ResourceT IO ()
delObject cfg space obj = void $ withDOConfig cfg Aws.pureAws (S3.DeleteObject obj space)

-- | Get info about an object without retrieving its contents
headObject :: DOConfig
           -> Text -- ^ space name
           -> Text -- ^ object name
           -> ResourceT IO (Maybe S3.ObjectMetadata)
headObject cfg space obj = S3.horMetadata <$> withDOConfig cfg Aws.pureAws (S3.headObject space obj)

-- | List the contents of a space. Includes folders and subfolders.
-- Oddly, specifying a non-existant space give the following error: @XmlException {xmlErrorMessage = "Missing error Message"}@
listSpace :: DOConfig
           -> Text -- ^ space name
           -> ResourceT IO [S3.ObjectInfo]
listSpace cfg space = S3.gbrContents <$> withDOConfig cfg Aws.pureAws (S3.getBucket space)

-- | For use with @listSpace@; determines whether is a folder via the heuristic that folders have no size and end with a forward slash
--
-- @
-- do
--     objs <- runResourceT $ listSpace cfg "a_space"
--     let printObj = TIO.putStrLn . S3.objectKey
--         printObjs = mapM_ (\obj -> when (objIsDir obj) $ printObj obj) objs
--     'seq' objs printObjs
-- @
--
-- may output some directories like
--
-- @
-- css/
-- js/
-- html/
-- @
--
-- *This will not work without @seq@.* Without @seq@, I got 'ResponseTimeout'.
--
-- Interestingly, the following code didn't work at first (failing with @XmlException {xmlErrorMessage = "Missing error Code"}@ or it just took so long that I interrupted it.)
--
-- @mapM_ (\obj -> when (objIsDir obj) $ TIO.putStrLn $ S3.objectKey obj) =<< runResourceT (listSpace cfg "a_space")@
--
-- But after running the above code (do block with @seq@,) the one-liner began to work. Moral of the story: 'ResourceT/'Manager' is weird, so
--
-- 1. keep code performed inside @ResourceT@ as small as possible
-- 2. evaluate it strictly, and
-- 3. perform all operations (e.g. maps & filters) on the /result/ of running a @ResourceT@, in the @IO@ monad (easily recognizable by having the bind arrow (@<-@) immediately adjacent to a call to @runResourceT@)
--
-- I should learn more about this, but for now these seem like safe & harmless rules.
objIsDir :: S3.ObjectInfo -> Bool
objIsDir = liftA2 (&&)
    ((== 0) . S3.objectSize)
    ((=='/') . T.last . S3.objectKey) -- @last@ should be safe, assuming that no space with a null objectKey

-- | Common combinator for use in catching exceptions
is404 :: HttpExceptionContent -> Bool
is404 = \case
    StatusCodeException r _ -> statusCode (responseStatus r) == 404
    _ -> False
