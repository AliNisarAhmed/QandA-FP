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
import           API.Requests                   ( LoginForm(..) )
import           Crypto.PasswordStore           ( makePassword )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Text                      ( unpack )
import           Data.Text.Encoding             ( encodeUtf8 )
import           API.DbQueries                  ( saveUser )




type AuthApi
  = "api" :> "auth" :> "signup" :> ReqBody '[JSON] LoginForm :> Post '[JSON] ()

authServer :: ServerT AuthApi App
authServer = signup

signup :: LoginForm -> App ()
signup (LoginForm firstName lastName email pwd cpwd) = if pwd == cpwd
  then
    (do
      password <- liftIO $ makePassword (encodeUtf8 pwd) 17
      runDb $ saveUser firstName lastName email password
    )
  else throwError err400 { errBody = "Passwords do not match" }




-- data AuthenticatedUser = AUser { auID :: Int
--                                 , name :: String
--                                } deriving (Show, Generic)



-- instance ToJSON AuthenticatedUser
-- instance FromJSON AuthenticatedUser
-- instance ToJWT AuthenticatedUser
-- instance FromJWT AuthenticatedUser


