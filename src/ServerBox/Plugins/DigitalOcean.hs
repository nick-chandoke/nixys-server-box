-- should I merge into <https://github.com/inzva/DOH>?
-- | DigitalOcean things that I implement as necessary. Currently just common REST requests.
--
-- Convenience wrapper around the aws packages' functions that makes use plain & simple for non-aws users, as DigitalOcean tries for interoperability with AWS transactions.
--
-- By the way, you may wonder why the functions here return in 'ResourceT'; well, when I tried runResourceT inside the blocks, I got 'ConnectionClosed' errors. So they're in @ResourceT@.
--
-- === Exceptions to Account For
-- * 'HttpException'
-- * 'StatusCodeException'
module ServerBox.Plugins.DigitalOcean
( DOConfig(..)
, mkDOConfig
, withDOConfig
, getObject
, putObject
, delObject
, headObject
) where

-- base
import Data.Maybe (fromMaybe)
import Control.Monad (void)

-- imports from miscellaneous packages
import Conduit -- conduit
import Data.ByteString (ByteString) -- bytestring
import Data.Text (Text) -- text
import Network.HTTP.Client (RequestBody, newManager) -- http-client
import Network.HTTP.Client.TLS (tlsManagerSettings) -- http-client-tls
import Network.HTTP.Conduit (Manager, responseBody) -- http-conduit

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
-- @mkDOConfig Nothing "JL0IED8TOFDMCQSO1402" "dC2kNFD2y2pIeeQtKhZkdMUaW341arsjBImtDTrwy/s" \<*\> pure "nyc3.digitaloceanspaces.com"
--
-- Even if you don't know Docker,...find /something/ else besides plaintext! Storing in envvars isn't acceptable, either!
mkDOConfig :: Maybe Aws.Logger -- ^ some or no logging
           -> ByteString -- ^ access key id
           -> ByteString -- ^ secret access key
           -> ByteString
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

delObject :: DOConfig
          -> Text -- ^ space name
          -> Text -- ^ object name
          -> ResourceT IO ()
delObject cfg space obj = void $ withDOConfig cfg Aws.pureAws (S3.DeleteObject obj space)

headObject :: DOConfig
           -> Text -- ^ space name
           -> Text -- ^ object name
           -> ResourceT IO (Maybe S3.ObjectMetadata)
headObject cfg space obj = S3.horMetadata <$> withDOConfig cfg Aws.pureAws (S3.headObject space obj)
