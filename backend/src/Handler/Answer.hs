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
  maybeQuestion <- checkQuestion questionId
  case maybeQuestion of
    Nothing -> badRequest "Question does not exist"
    Just q  -> fmap toJSON (getAnswersForQuestion questionId)

postAnswerR :: QuestionId -> Handler Value
postAnswerR questionId = do
  maybeQuestion <- checkQuestion questionId
  body <- requireCheckJsonBody :: Handler CreateAnswerRequest
  case maybeQuestion of
    Nothing -> badRequest "Question does not exist"
    Just q -> fmap toJSON (createAnswer questionId body)