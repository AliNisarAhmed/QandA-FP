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
  :<|> SAS.Auth '[SAS.Cookie, SAS.JWT] AuthenticatedUser :> CurrentUserApi
  :<|> SAS.Auth '[SAS.Cookie, SAS.JWT] AuthenticatedUser :> LogOutApi

type CurrentUserApi
  = "api" :> "auth" :> "current-user" :> Get '[JSON] (Maybe AuthenticatedUser)


type LogOutApi
  = "api" :> "auth" :> "logout" :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                  , Header "Set-Cookie" SetCookie]
                                  () )


authServer :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT AuthApi App
authServer cs jwts = signup :<|> login cs jwts :<|> currentUser :<|> logout cs


---- HANDLERS ---


logout
  :: SAS.CookieSettings
  -> SAS.AuthResult AuthenticatedUser
  -> App (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] ())
logout cs (SAS.Authenticated authUser) = return $ SAS.clearSession cs ()
logout _ _ = throwAll err401


currentUser :: SAS.AuthResult AuthenticatedUser -> App (Maybe AuthenticatedUser)
currentUser (SAS.Authenticated authUser) = do
  _ <- liftIO $ putStrLn "Authenticated"
  return $ Just authUser
currentUser (SAS.NoSuchUser) = do
  _ <- liftIO $ putStrLn "No Such User"
  return Nothing
currentUser (SAS.Indefinite ) = do
  _ <- liftIO $ putStrLn "Indefinite"
  return Nothing
currentUser (SAS.BadPassword) = do
  _ <- liftIO $ putStrLn "Bad Password"
  throwAll err401


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
    Just (id, user) -> do
      let aUser = AUser id (M.userFirstName user) (M.userLastName user)
      mApplyCookies <- liftIO $ SAS.acceptLogin cookieSettings jwtSettings aUser
      case mApplyCookies of
        Nothing ->
          throwError err401
        Just applyCookies ->
          return $ applyCookies aUser


data AuthenticatedUser = AUser
  { id :: M.Key M.User
  , firstName :: Text
  , lastName :: Text
  } deriving (Show, Generic)


instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser