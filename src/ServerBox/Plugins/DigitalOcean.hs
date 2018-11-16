-- | DigitalOcean things. Usually REST requests.
-- Note that DigitalOcean tries for interoperability with AWS S3; this module will account for common functionality as necessary.
-- should I merge into <https://github.com/inzva/DOH>?
module ServerBox.Plugins.DigitalOcean
( getObject
, putObject
, testObject
) where

import Network.HTTP.Conduit (newManager, tlsManagerSettings, responseBody) -- http-conduit

import Data.Text (Text) -- text
import Data.ByteString (ByteString) -- bytestring
import Conduit -- conduit

-- aws
import qualified Aws
import qualified Aws.Core as Aws
import qualified Aws.S3 as S3

-- | Like HTTP GET.
getObject :: Aws.Credentials
          -> ByteString -- ^ endpoint (e.g. "nyc3.digitaloceanspaces.com"); (btw, for DigitalOcean Spaces, do /not/ use the URL that contains "cdn"! (That will HTTP 403-out.))
          -> Text -- ^ space name
          -> Text -- ^ object name
          -> IO ()
getObject creds endpoint space object = do
    let cfg = Aws.Configuration Aws.Timestamp creds (Aws.defaultLog Aws.Debug) Nothing
        s3cfg = S3.s3v4 Aws.HTTPS endpoint False S3.AlwaysSigned -- False? What does "useURI" do anyway?
    mgr <- newManager tlsManagerSettings
    runResourceT $ Aws.pureAws cfg s3cfg mgr (S3.getObject space object) >>= (\rsp -> runConduit $ responseBody rsp .| stdoutC) . S3.gorResponse

-- | Like HTTP PUT
putObject = undefined

-- | Like HTTP HEAD
testObject = undefined
