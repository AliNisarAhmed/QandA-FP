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
import Database.Persist (Entity(..), insertEntity)
import Database (runDb)
import Data.Text (Text(..))
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import API.DbQueries (
  getAnswersByQuestionId, checkQuestion, createAnswer, checkAnswer, updateAnswer)
import Control.Monad.Except (MonadError)

type AnswerApi =
  "api" :> "questions" :>
    (
        Capture "questionId" (Key Question)
            :> "answers" :> Get '[JSON] [Entity Answer]
      :<|>
        Capture "questionId" (Key Question)
            :> "answers" :> ReqBody '[JSON] CreateAnswerRequest
            :> Post '[JSON] (Entity Answer)
      :<|>
        Capture "questionId" (Key Question)
            :> "answers" :> Capture "answerId" (Key Answer)
            :> ReqBody '[JSON] UpdateAnswerRequest
            :> Put '[JSON] Answer
    )

answerServer :: ServerT AnswerApi App
answerServer =
  getAllAnswers :<|> postAnswer :<|> putAnswer

getAllAnswers :: Key Question -> App [Entity Answer]
getAllAnswers questionId =
  runDb $ getAnswersByQuestionId questionId


postAnswer :: Key Question -> CreateAnswerRequest -> App (Entity Answer)
postAnswer questionId req = do
  question <- runDb (checkQuestion questionId) !?? err400 { errBody = "Question not found"}
  now <- liftIO getCurrentTime
  runDb $ createAnswer questionId (content req) (userId req) now


putAnswer :: Key Question -> Key Answer -> UpdateAnswerRequest -> App Answer
putAnswer questionId answerId req = do
  _ <- runDb (checkQuestion questionId) !?? err400 { errBody = "Question not found"}
  _ <- runDb (checkAnswer questionId answerId) !?? err400 { errBody = "Answer not found" }
  runDb $ updateAnswer answerId (newContent req)


---- REQUESTS ----

data CreateAnswerRequest = CreateAnswerRequest
  { content :: Text
  , userId :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON CreateAnswerRequest
instance ToJSON CreateAnswerRequest

data UpdateAnswerRequest = UpdateAnswerRequest
  { newContent :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON UpdateAnswerRequest
instance ToJSON UpdateAnswerRequest

(!??) :: MonadError e m => m (Maybe a) -> e -> m a
act !?? err = act >>= maybe (throwError err) return