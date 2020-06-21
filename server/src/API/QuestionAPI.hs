{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators   #-}


module API.QuestionAPI (questionServer, QuestionApi) where

import Model
import Servant
import Config (App(..))
import Database.Persist (Entity(..), selectList, insertEntity)
import Database (runDb, DbQuery)
import Database.Esqueleto (select, from)

type QuestionApi =
    "api" :> "questions" :>
      (
        Get '[JSON] [Entity Question] :<|>
        ReqBody '[JSON] Question :> Post '[JSON] (Entity Question)
      )

questionServer :: ServerT QuestionApi App
questionServer = getAllQuestions :<|> postQuestion

getAllQuestions :: App [Entity Question]
getAllQuestions = runDb getQuestions


postQuestion :: Question -> App (Entity Question)
postQuestion q = runDb $ insertEntity q








getQuestions :: DbQuery [Entity Question]
getQuestions =
  select $ from $ \q -> return q