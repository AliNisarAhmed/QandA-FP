{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances   #-}


module Database where

import           ClassyPrelude.Yesod     hiding ( Value
                                                , isNothing
                                                , on
                                                , (==.)
                                                , update
                                                , delete
                                                , insert
                                                , get
                                                )
import           Database.Esqueleto      hiding ( (=.) )
import           Model
import           Data.Aeson                     ( )
import           Requests
import           Foundation



---




-- https://stackoverflow.com/questions/35676855/represent-foreign-key-relationship-in-json-using-servant-and-persistent





--- General type synonym to describe DB Queries


type DbQuery a = ReaderT SqlBackend Handler a

---- Queries ----




getQuestions :: Handler [Entity Question]
getQuestions = runDB getAllQuestions


createQuestion :: CreateQuestionRequest -> Handler (Maybe Question)
createQuestion cqR = do
  now      <- liftIO getCurrentTime
  question <- runDB
    $ insert (Question (title cqR) (content cqR) now (userId cqR))
  runDB $ get question

checkUser :: Key User -> Handler (Maybe (Entity User))
checkUser = fmap listToMaybe . runDB . selectUserById


checkQuestion :: Key Question -> DbQuery Question
checkQuestion = get404

updateQuestion :: Key Question -> Maybe Text -> Maybe Text -> Handler Question
updateQuestion k (Just title) Nothing =
  runDB $ updateGet k [QuestionTitle =. title]
updateQuestion k Nothing (Just content) =
  runDB $ updateGet k [QuestionContent =. content]

deleteQuestion :: Key Question -> Handler ()
deleteQuestion questionId =
  runDB $ delete $ from $ \p -> where_ (p ^. QuestionId ==. val questionId)



---- ANSWERS ----




getAnswersForQuestion :: Key Question -> Handler [Entity Answer]
getAnswersForQuestion questionId = runDB $ getAllAnswers questionId

createAnswer :: Key Question -> CreateAnswerRequest -> Handler (Entity Answer)
createAnswer questionId request = do
  now <- liftIO getCurrentTime
  runDB $ insertEntity
    (Answer questionId (newContent request) (creatorId request) now)


checkAnswer :: Key Question -> Key Answer -> Handler (Maybe (Entity Answer))
checkAnswer questionId answerId =
  fmap listToMaybe . runDB $ selectAnswerById questionId answerId


updateAnswer :: Key Answer -> Text -> Handler Answer
updateAnswer answerId updatedContent =
  runDB $ updateGet answerId [AnswerContent =. updatedContent]




---  DB Queries  ----




getAllQuestions :: DbQuery [Entity Question]
getAllQuestions = select $ from $ \question -> return question

selectQuestionById :: Key Question -> DbQuery (Maybe (Entity Question))
selectQuestionById questionId = fmap listToMaybe $ select $ from $ \q -> do
  where_ (q ^. QuestionId ==. val questionId)
  return q

selectUserById :: Key User -> DbQuery [Entity User]
selectUserById userId = select $ from $ \user -> do
  where_ (user ^. UserId ==. val userId)
  return user

getQuestionById :: Key Question -> DbQuery [Entity Question]
getQuestionById questionId = select $ from $ \q -> do
  where_ (q ^. QuestionId ==. val questionId)
  return q

getAllAnswers :: Key Question -> DbQuery [Entity Answer]
getAllAnswers questionId = select $ from $ \answer -> do
  where_ (answer ^. AnswerQuestionId ==. val questionId)
  return answer

selectAnswerById :: Key Question -> Key Answer -> DbQuery [Entity Answer]
selectAnswerById questionId answerId = select $ from $ \ans -> do
  where_
    (   ans
    ^.  AnswerId
    ==. val answerId
    &&. ans
    ^.  AnswerQuestionId
    ==. val questionId
    )
  return ans
