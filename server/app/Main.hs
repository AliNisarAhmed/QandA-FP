{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib (app)
import           Network.Wai.Handler.Warp
import           Config                         ( Config(..) )
import           Database                       ( connectDb
                                                , migrateDb
                                                )

main :: IO ()
main = do
      putStrLn "Starting the server"
      let
            connectionString
                  = "host=localhost dbname=qanda user=postgres password=faMas2010 port=5432"
      pool <- connectDb connectionString
      migrateDb pool
      let cfg  = Config pool
      let port = 8080
      run port $ app cfg
      putStrLn $ "Server started on port: " ++ show port
