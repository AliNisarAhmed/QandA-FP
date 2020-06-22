{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings   #-}

-- https://stackoverflow.com/questions/51146315/servant-queryparams-parse-error

module API.AnswerAPI (answerServer, AnswerApi) where

import GHC.Generics (Generic)
import Model
import Servant
import Config (App(..))
import Database.Persist (Entity(..), insertEntity)
import Database (runDb)
import Data.Text (Text(..))
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import API.DbQueries (getAnswersByQuestionId, checkQuestion, createAnswer)

type AnswerApi =
  "api" :> "questions" :>
    (
      Capture "questionId" (Key Question)
        :> "answers" :> Get '[JSON] [Entity Answer] :<|>
      Capture "questionId" (Key Question)
        :> "answers" :> ReqBody '[JSON] CreateAnswerRequest :> Post '[JSON] (Entity Answer)
    )

answerServer :: ServerT AnswerApi App
answerServer =
  getAllAnswers :<|> postAnswer

getAllAnswers :: Key Question -> App [Entity Answer]
getAllAnswers questionId =
  runDb $ getAnswersByQuestionId questionId


postAnswer :: Key Question -> CreateAnswerRequest -> App (Entity Answer)
postAnswer questionId req = do
  question <- runDb $ checkQuestion questionId
  now <- liftIO getCurrentTime
  case question of
    Nothing -> throwError $ err400 { errBody = "Question not found"}
    Just q ->
      runDb $ createAnswer questionId (content req) (userId req) now

data CreateAnswerRequest = CreateAnswerRequest
  { content :: Text
  , userId :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON CreateAnswerRequest
instance ToJSON CreateAnswerRequest