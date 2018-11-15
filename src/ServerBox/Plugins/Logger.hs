module ServerBox.Plugins.Logger where
import System.Log.FastLogger -- fast-logger
import Network.Wai (Request, httpVersion, rawPathInfo, remoteHost, requestMethod)
import Network.HTTP.Types (Status(statusCode))
import Data.Maybe (fromMaybe)

-- | Create a logger the easy way ;)
-- FormattedTime ~ BS'.ByteString, implements toLogStr
-- LogStr is a black box data type instancing Show, Eq, IsString, Monoid. LogStr's (<>) is O(1).
mkLogger :: LogType -> IO ((FormattedTime -> LogStr) -> IO (), IO ())
mkLogger t = flip newTimedFastLogger t =<< newTimeCache simpleTimeFormat

-- | Log requests in NCSA Common Log Format
clf :: ((FormattedTime -> LogStr) -> IO ()) -> Request -> Status -> Maybe Integer -> IO ()
clf tfl req status msize = tfl $ \(toLogStr -> time) ->
    (toLogStr . show $ remoteHost req) <>
    " - - [" <> time <> "] \"" <> toLogStr (requestMethod req) <> " " <> toLogStr (rawPathInfo req) <> " " <> (toLogStr . show $ httpVersion req) <> "\"" <> (toLogStr . show $ statusCode status) <> (toLogStr . show $ fromMaybe 0 msize)
