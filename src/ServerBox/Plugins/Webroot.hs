module ServerBox.Plugins.Webroot where
import Network.HTTP.Types (methodGet) -- http-types
import ServerBox (onMethod, Route(..), static)
import Network.Wai (pathInfo)
import Network.Wai.Handler.WarpTLS (TLSSettings, tlsSettingsChain) -- warp-tls
import NicLib.Errors (err)

-- | Support for <https://certbot.eff.org/docs/using.html Let's Encrypt's webroot plugin>. E.g.
--
-- @
-- case webrootOn "myhost.com" of
--     (settings, webRootRoute) ->
--         'stdwarp' (Just settings) defaultSettings id $ webRootRoute \<\> myUsualRoutes
-- @
webrootOn :: Monad m => String -> (TLSSettings, Route m)
webrootOn host = (tlsSettingsChain (certDir <> "cert.pem") [certDir <> "fullchain.pem"] (certDir <> "privkey.pem"), webrootAuth)
    where
        certDir = "/etc/letsencrypt/live/" <> host <> "/" -- I'm leaving '/' literals here as pathSeparator because I'm assuming *NIX functionality of webroot; I'm unsure whether webroot has the same setup in Windows
        webrootAuth = onMethod (==methodGet) . Route $ \req -> case pathInfo req of
            (".well-known":"acme-challenge":_) -> (rte $ static certDir) req
            _ -> err mempty
