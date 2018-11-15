module ServerBox.Plugins.Blacklist (checkBlacklist) where

import ServerBox (Route(Route))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Database.Redis as R -- hedis
import qualified Data.ByteString.Char8 as BS'
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.Socket (SockAddr(..)) -- network
import NicLib.NStdLib (readMaybe)
import Network.Wai (remoteHost, responseLBS, responseStatus)
import Data.Bool (bool)
import Control.Monad (join)
import Network.HTTP.Types (tooManyRequests429, internalServerError500) -- http-types
import Network.HTTP.Types.Status (statusIsClientError)

-- | Check a request against a blacklist (stored in Redis;) deny clients who've made failed connections too frequently.
-- Particularly, quickly block addresses for 24 hours who make more than 3 client-error requests per minute.
checkBlacklist :: MonadIO m
               => Int -- ^ number of invalid requests before we stop serving that SockAddr entirely
               -> Integer -- ^ length of time (in seconds) to deny the offending user
               -> R.Connection -- ^ connection to the Redis database
               -> Route m -- ^ route to check
               -> Route m
checkBlacklist limit ttl redis (Route r) = Route $ \req ->
    let origWrappedResp = r req -- :: ExceptT (Maybe Response) m Response
        ipAddr = hashSock $ remoteHost req
        -- run the Redis query
--      query :: IO (R.TxResult (ExceptT (Maybe Response) m Response))
        query = R.runRedis redis . R.multiExec $ do -- in RedisTx
            qmCount <- R.get ipAddr
            qIncr <- R.setex ipAddr ttl "1"
            pure $ do -- in Queued
                mCount <- qmCount
                case mCount of
                    Nothing -> qIncr *> pure origWrappedResp
                    Just c_str -> case readMaybe @Int (BS'.unpack c_str) of
                        Nothing ->
                            pure . pure $ responseLBS internalServerError500 [] ("Failed to parse count from Redis value \"" <> BS.fromStrict c_str <> "\" in function checkBlacklist")
                        Just c ->
                            if c >= limit then
                                pure . pure $ responseLBS tooManyRequests429 [] "You've sent too many improper requests too often. You're banned from the server."
                            else pure origWrappedResp
        -- process a response. Just a continuation of query, made into a separate function for readability
--      fromResult :: R.TxResult (ExceptT (Maybe Response) m Response) -> ExceptT (Maybe Response) m Response
        fromResult = \case
            R.TxSuccess resp -> resp
            R.TxAborted      -> pure $ responseLBS internalServerError500 [] "Redis operation aborted in checkBlacklist"
            R.TxError errstr -> pure $ responseLBS internalServerError500 [] ("Redis error in checkBlacklist: " <> BS.pack errstr)
    in origWrappedResp >>= bool origWrappedResp (join . liftIO $ fromResult <$> query) . statusIsClientError . responseStatus
    where
        -- | Some hash. Don't care about the output up to isomorphism/perfection.
        hashSock :: SockAddr -> BS'.ByteString
        hashSock = \case
            SockAddrInet _ a -> sh a
            SockAddrInet6 _ _ (a,b,c,d) _ -> sh a <> sh b <> sh c <> sh d
            SockAddrUnix str -> BS'.pack str
            where
                sh :: Show a => a -> BS'.ByteString
                sh = BS'.pack . show
