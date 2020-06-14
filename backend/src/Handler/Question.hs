{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}


module Handler.Question where

import           Import
import           Database
import           Requests



getQuestionR :: Handler Value
getQuestionR = toJSON <$> getQuestions



postQuestionR :: Handler Value
postQuestionR = do
    body      <- requireCheckJsonBody :: Handler CreateQuestionRequest
    maybeUser <- checkUser (userId body)
    case maybeUser of
        Just u -> do
            maybeQ <- createQuestion body
            case maybeQ of
                Just q  -> return $ toJSON q
                Nothing -> badRequest "failed to create the user"
        Nothing -> badRequest "User does not exist"


putQuestionR :: Handler Value
putQuestionR = do
    body          <- requireCheckJsonBody :: Handler UpdateQuestionRequest
    maybeQuestion <- checkQuestion (updateQuestionId body)
    case maybeQuestion of
        Nothing -> badRequest "Question does not exist"
        Just q  -> case (updatedTitle body, updatedContent body) of
            (Nothing, Nothing) -> badRequest "At least one update is required"
            (newTitle, newContent) ->
                fmap toJSON (updateQuestion (entityKey q) newTitle newContent)
