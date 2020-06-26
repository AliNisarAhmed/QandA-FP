{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings   #-}


module API.QuestionAPI (questionServer, QuestionApi) where

import Model
import Servant
import Config (App(..))
import Database.Persist (Entity(..))
import Database (runDb)
import Data.Text (Text(..))
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import API.DbQueries ((!??), getQuestions, insertQuestion, checkQuestion, updateQuestion, deleteQuestionById, getQuestionWithAnswers)
import API.Requests

type QuestionApi =
    "api" :> "questions" :>
      (
          Get '[JSON] [Entity Question]
        :<|>
          Capture "questionId" (Key Question) :> Get '[JSON] (Maybe QuestionWithAnswers)
        :<|>
          ReqBody '[JSON] CreateQuestionRequest :> Post '[JSON] (Entity Question)
        :<|>
          Capture "questionId" (Key Question) :> ReqBody '[JSON] UpdateQuestionRequest :> Put '[JSON] Question
        :<|>
          Capture "questionId" (Key Question) :> Delete '[JSON] ()
      )


questionServer :: ServerT QuestionApi App
questionServer =
  getAllQuestions :<|> getQuestion :<|> postQuestion :<|> putQuestion :<|> deleteQuestion



getAllQuestions :: App [Entity Question]
getAllQuestions = runDb getQuestions


getQuestion :: Key Question -> App (Maybe QuestionWithAnswers)
getQuestion = runDb . getQuestionWithAnswers


postQuestion :: CreateQuestionRequest -> App (Entity Question)
postQuestion CreateQuestionRequest {..} = do
  now <- liftIO getCurrentTime
  runDb $ insertQuestion (Question title content now userId)


putQuestion :: Key Question -> UpdateQuestionRequest -> App Question
putQuestion questionId req = do
  q <- runDb (checkQuestion questionId) !?? err400 { errBody = "Question not found" }
  runDb $ updateQuestion questionId (newTitle q) (newContent q)
    where
      newTitle q = fromMaybe (questionTitle q) (updatedTitle req)
      newContent q = fromMaybe (questionContent q) (updatedContent req)


deleteQuestion :: Key Question -> App ()
deleteQuestion = runDb . deleteQuestionById
