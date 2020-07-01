{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DuplicateRecordFields #-}


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
import API.DbQueries ( (!??),
  getAnswersByQuestionId, checkQuestion, createAnswer, checkAnswer, updateAnswer, deleteAnswerFromDb)
import API.Requests
import qualified Servant.Auth.Server as SAS
import API.AuthAPI (AuthenticatedUser(..))
import Control.Monad (when)


type AnswerApi =
  "api" :> "questions" :>
    (
        Capture "questionId" (Key Question)
            :> "answers" :> Get '[JSON] [Entity Answer]
      :<|>
        Capture "questionId" (Key Question)
            :> "answers"
            :> ReqBody '[JSON] CreateAnswerRequest
            :> Post '[JSON] (Entity Answer)
      :<|>
        Capture "questionId" (Key Question)
            :> "answers"
            :> Capture "answerId" (Key Answer)
            :> ReqBody '[JSON] UpdateAnswerRequest
            :> Put '[JSON] Answer
      :<|>
        SAS.Auth '[SAS.Cookie, SAS.JWT] AuthenticatedUser
            :> Capture "questionId" (Key Question)
            :> "answers"
            :> Capture "answerId" (Key Answer)
            :> Delete '[JSON] ()
    )

answerServer :: ServerT AnswerApi App
answerServer =
  getAllAnswers :<|> postAnswer :<|> putAnswer :<|> deleteAnswer

getAllAnswers :: Key Question -> App [Entity Answer]
getAllAnswers questionId =
  runDb $ getAnswersByQuestionId questionId


postAnswer :: Key Question -> CreateAnswerRequest -> App (Entity Answer)
postAnswer questionId req = do
  question <- runDb (checkQuestion questionId) !?? err400 { errBody = "Question not found"}
  now <- liftIO getCurrentTime
  runDb $
    createAnswer
      questionId
      (content (req :: CreateAnswerRequest))
      (userId (req :: CreateAnswerRequest))
      now


putAnswer :: Key Question -> Key Answer -> UpdateAnswerRequest -> App Answer
putAnswer questionId answerId req = do
  _ <- runDb (checkQuestion questionId) !?? err400 { errBody = "Question not found"}
  _ <- runDb (checkAnswer questionId answerId) !?? err400 { errBody = "Answer not found" }
  runDb $ updateAnswer answerId (newContent req)


deleteAnswer :: SAS.AuthResult AuthenticatedUser -> Key Question -> Key Answer -> App ()
deleteAnswer (SAS.Authenticated user) questionId answerId = do
  _ <- runDb (checkQuestion questionId) !?? err400 { errBody = "Question not found" }
  answer <- runDb (checkAnswer questionId answerId) !?? err400 { errBody = "Answer not found"}
  when (API.AuthAPI.id user /= answerUserId (entityVal answer)) (throwError err401)
  runDb $ deleteAnswerFromDb answerId
deleteAnswer _ _ _ = SAS.throwAll err401
