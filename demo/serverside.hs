{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Data.Default           (def)
import           Data.Text              (Text)
import           Network.HTTP.Conduit   (Manager, conduitManagerSettings, newManager)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Facebook.ServerSide
import qualified Yesod.Facebook as YF
import qualified Facebook as FB

data App = App
    { httpManager :: Manager
    , facebookCredentials :: FB.Credentials
    , facebookUseBetaTier :: Bool
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
    -- Note: In order to log in with BrowserID, you must correctly
    -- set your hostname here.
    approot = ApprootStatic "http://localhost:3000"

instance YesodAuth App where
    type AuthId App = Text
    getAuthId = return . Just . credsIdent

    loginDest _ = HomeR
    logoutDest _ = HomeR

    authPlugins _ =
        [ authFacebook []
        ]

    authHttpManager = httpManager

    -- The default maybeAuthId assumes a Persistent database. We're going for a
    -- simpler AuthId, so we'll just do a direct lookup in the session.
    maybeAuthId = lookupSession "_ID"

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YF.YesodFacebook App where
  fbCredentials = facebookCredentials
  fbHttpManager = httpManager
  fbUseBetaTier = facebookUseBetaTier

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]

credentials :: FB.Credentials
credentials = FB.Credentials appName appId appSecret
    where
        appName :: Text
        appName = FIXME

        appId :: Text
        appId = FIXME

        appSecret :: Text
        appSecret = FIXME

main :: IO ()
main = do
    man <- newManager conduitManagerSettings
    warp 3000 $ App man credentials True
