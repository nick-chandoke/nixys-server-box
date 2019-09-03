-- | A webroot plugin that I wrote. It worked. If you need a more complete solution, see [snoyberg's `warp-letsencrypt`](https://github.com/snoyberg/warp-letsencrypt).
module ServerBox.Plugins.Webroot where

import RIO
import Control.Arrow
import Network.HTTP.Types (methodGet) -- http-types
import ServerBox (onMethod, Route, static, next)
import Network.Wai (pathInfo) -- network-wai
import Network.Wai.Handler.WarpTLS (TLSSettings, tlsSettingsChain) -- warp-tls

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
        certDir = "/etc/letsencrypt/live/" <> host <> "/" -- I'm leaving '/' literals here as pathSeparator because I'm assuming *NIX functionality of webroot; I'm unsure whether webroot has the same setup in Windows, so I don't want to assume that I can just exchange '/' for '\' and it'll still work fine
        webrootAuth
            =  onMethod (==methodGet)
           >>> arr (\req -> case pathInfo req of
                    (".well-known":"acme-challenge":_) -> (static certDir, req)
                    _ -> (next, req))
           >>> app
