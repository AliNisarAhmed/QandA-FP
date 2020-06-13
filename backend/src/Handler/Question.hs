{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Handler.Question where

import           Import
import           Database


import           Data.Aeson
import           GHC.Generics                   ( Generic )

data APIError = APIError { message :: Text } deriving (Generic, FromJSON, ToJSON)


getQuestionR :: Handler Value
getQuestionR = toJSON <$> getQuestions

postQuestionR :: Handler Value
postQuestionR = do
    body   <- requireCheckJsonBody :: Handler Question
    maybeQ <- createQuestion body
    case maybeQ of
        Just q  -> return $ toJSON q
        Nothing -> sendResponseStatus badRequest400
            $ toJSON (APIError "failed to create the user")
