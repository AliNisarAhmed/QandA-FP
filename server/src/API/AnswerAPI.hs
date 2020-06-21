{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings   #-}


module API.AnswerAPI (answerServer, AnswerApi) where

import GHC.Generics (Generic)
import Model
import Servant
import Config (App(..))
import Database.Persist (Entity(..))
import Database (runDb)
import Data.Text (Text(..))
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import API.DbQueries (getAnswersByQuestionId)

type AnswerApi =
  "api" :> "questions" :> Capture "questionId" (Key Question) :>
    (
      "answers" :> Get '[JSON] [Entity Answer] :<|>
      "answers" :> ReqBody '[JSON] CreateAnswerRequest :> Post '[JSON] Answer
    )

answerServer :: ServerT AnswerApi App
answerServer =
  getAllAnswers :<|> createAnswer

getAllAnswers :: Key Question -> App [Entity Answer]
getAllAnswers questionId =
  runDb $ getAnswersByQuestionId questionId


createAnswer :: Key Question -> CreateAnswerRequest -> App Answer
createAnswer questionId req = undefined

data CreateAnswerRequest = CreateAnswerRequest
  { content :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON CreateAnswerRequest
instance ToJSON CreateAnswerRequest