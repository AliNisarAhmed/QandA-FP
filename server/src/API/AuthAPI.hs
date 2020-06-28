{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API.AuthAPI where

import           Servant
import           Config                         ( App(..) )
import           Servant.Auth                  as SA
import           Servant.Auth.Server           as SAS
import           GHC.Generics                   ( Generic )
import           Database                       ( runDb )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           API.Requests                   ( SignupForm(..)
                                                , LoginForm(..)
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           API.DbQueries                  ( saveUser
                                                , validateLoginForm
                                                )
import           Data.Text                      ( Text )
import qualified Model as M



type AuthApi
  = "api" :> "auth" :> "signup" :> ReqBody '[JSON] SignupForm :> Post '[JSON] ()
  :<|>
    "api" :> "auth" :> "login"
        :> ReqBody '[JSON] LoginForm
        :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                  , Header "Set-Cookie" SetCookie]
                                  AuthenticatedUser )

authServer :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT AuthApi App
authServer cs jwts = signup :<|> login cs jwts

signup :: SignupForm -> App ()
signup (SignupForm firstName lastName userName pwd cpwd) = if pwd == cpwd
  then
    runDb $ saveUser firstName lastName userName pwd
  else throwError err400 { errBody = "Passwords do not match" }

login
  :: SAS.CookieSettings
  -> SAS.JWTSettings
  -> LoginForm
  -> App (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthenticatedUser)
login cookieSettings jwtSettings form = do
  mu <- runDb $ validateLoginForm form
  case mu of
    Nothing ->
      throwError err401
    Just user -> do
      let aUser = AUser (M.userFirstName user) (M.userLastName user)
      mApplyCookies <- liftIO $ SAS.acceptLogin cookieSettings jwtSettings aUser
      case mApplyCookies of
        Nothing ->
          throwError err401
        Just applyCookies ->
          return $ applyCookies aUser


data AuthenticatedUser = AUser
  { firstName :: Text
  , lastName :: Text
  } deriving (Show, Generic)


instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser


