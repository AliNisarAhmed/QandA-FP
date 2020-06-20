{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators   #-}


module API.QuestionAPI (questionServer, QuestionApi) where

import Model
import Servant
import Config (App(..))
import Database.Persist (Entity(..), selectList)
import Database (runDb)

type QuestionApi =
  "api" :> "whoami" :> Get '[JSON] [Entity Question]

questionServer :: ServerT QuestionApi App
questionServer = whoami

whoami :: App [Entity Question]
whoami = runDb $ selectList [] []