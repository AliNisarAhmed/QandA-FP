{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib                            ( mkApp )
import           Network.Wai.Handler.Warp
import           Config                         ( Config(..) )
import           Database                       ( connectDb
                                                , migrateDb
                                                )
import qualified Servant.Auth.Server           as SAS
import           Servant.Server                 ( Context((:.), EmptyContext)
                                                , Application
                                                )
import           Network.Wai.Middleware.Cors
import qualified Network.Wai.Middleware.Servant.Options
                                               as SO



main :: IO ()
main = do
      putStrLn "Starting the server"
      let
            connectionString
                  = "host=localhost dbname=qanda user=postgres password=faMas2010 port=5432"
      pool <- connectDb connectionString
      migrateDb pool
      key <- SAS.generateKey
      let   cfg       = Config pool
            port      = 5000
            jwtConfig = SAS.defaultJWTSettings key
            cookieConfig =
                  SAS.defaultCookieSettings { SAS.cookieXsrfSetting = Nothing }
            -- Just $ SAS.defaultXsrfCookieSettings
            --                                 { SAS.xsrfCookiePath = Nothing
            --                                 }
            ctx = cookieConfig :. jwtConfig :. EmptyContext
      run port $ mkApp ctx cookieConfig jwtConfig cfg
      putStrLn $ "Server started on port: " ++ show port




-- app :: Config -> Application
-- app cfg = cors (const $ Just policy) $ SO.provideOptions api $ serve
--       api
--       (appToServer cfg)
--    where
--       policy = CorsResourcePolicy
--             { corsOrigins        = Nothing
--             , corsMethods        = ["OPTIONS", "GET", "PUT", "POST", "DELETE"]
--             , corsRequestHeaders = ["Authorization", "Content-Type"]
--             , corsExposedHeaders = Nothing
--             , corsMaxAge         = Nothing
--             , corsVaryOrigin     = False
--             , corsRequireOrigin  = False
--             , corsIgnoreFailures = False
--             }
