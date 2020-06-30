{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib                            ( mkApp )
import           Network.Wai.Handler.Warp
import           Config                         ( Config(..) )
import           Database                       ( connectDb
                                                , migrateDb
                                                )
import           Servant.Auth.Server            ( generateKey
                                                , defaultCookieSettings
                                                , defaultJWTSettings
                                                )

import           Servant.Server                 ( Context((:.), EmptyContext) )


main :: IO ()
main = do
      putStrLn "Starting the server"
      key <- generateKey
      let
            connectionString
                  = "host=localhost dbname=qanda user=postgres password=faMas2010 port=5432"
      pool <- connectDb connectionString
      migrateDb pool
      key <- generateKey
      let   cfg          = Config pool
            port         = 5000
            jwtConfig    = defaultJWTSettings key
            cookieConfig = defaultCookieSettings
            ctx          = cookieConfig :. jwtConfig :. EmptyContext
      run port $ mkApp ctx cookieConfig jwtConfig cfg
      putStrLn $ "Server started on port: " ++ show port
