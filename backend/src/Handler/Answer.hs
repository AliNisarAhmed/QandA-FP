{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}


module Handler.Answer where

import           Import
import           Database
import           Requests


getAnswerR :: QuestionId -> Handler Value
getAnswerR questionId = do
  _ <- return $ checkQuestion questionId
  fmap toJSON (getAnswersForQuestion questionId)


postAnswerR :: QuestionId -> Handler Value
postAnswerR questionId = do
  _    <- return $ checkQuestion questionId
  body <- requireCheckJsonBody :: Handler CreateAnswerRequest
  fmap toJSON (createAnswer questionId body)


putAnswerUpdateR :: QuestionId -> AnswerId -> Handler Value
putAnswerUpdateR questionId answerId = do
  body <- requireCheckJsonBody :: Handler UpdateAnswerRequest
  _    <- return $ checkQuestion questionId
  _    <- return $ checkAnswer questionId answerId
  fmap toJSON $ updateAnswer answerId (updatedAnswerContent body)


deleteAnswerUpdateR :: QuestionId -> AnswerId -> Handler ()
deleteAnswerUpdateR questionId answerId = do
  _    <- return $ checkAnswer questionId answerId
  body <- requireCheckJsonBody :: Handler DeleteAnswerRequest
  deleteAnswer answerId (deletorsId body)
