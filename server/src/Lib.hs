{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings   #-}


module Lib
  ( mkApp
  )
where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified Data.Text                     as T
import           Control.Monad.IO.Class         ( liftIO )
import           Config
import           Network.Wai.Middleware.Cors
import           Database                       ( runDb )
import           Database.Persist
import           Control.Monad.Reader           ( runReaderT )
import           Server                         ( server
                                                , API
                                                )
import           Network.Wai.Middleware.Servant.Options
import qualified Servant.Auth.Server           as SAS

-- app :: SAS.CookieSettings -> SAS.JWTSettings -> Config -> Application
-- app cs jwts cfg = cors (const $ Just policy) $ provideOptions api $ serve
--   api
--   (appToServer cfg)
--  where
--   policy = CorsResourcePolicy
--     { corsOrigins        = Nothing
--     , corsMethods        = ["OPTIONS", "GET", "PUT", "POST", "DELETE"]
--     , corsRequestHeaders = ["Authorization", "Content-Type"]
--     , corsExposedHeaders = Nothing
--     , corsMaxAge         = Nothing
--     , corsVaryOrigin     = False
--     , corsRequireOrigin  = False
--     , corsIgnoreFailures = False
--     }


api :: Proxy API
api = Proxy


-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
mkApp
  :: Context '[SAS.CookieSettings, SAS.JWTSettings]
  -> SAS.CookieSettings
  -> SAS.JWTSettings
  -> Config
  -> Application
mkApp cfg cs jwts ctx = serveWithContext api cfg $ hoistServerWithContext
  api
  (Proxy :: Proxy '[SAS.CookieSettings, SAS.JWTSettings])
  (convertApp ctx)
  (server cs jwts)

-- appToServer :: Config -> Server API
-- appToServer cfg = hoistServer api (convertApp cfg) server


convertApp :: Config -> App a -> Handler a
convertApp cfg appt = runReaderT (runApp appt) cfg
