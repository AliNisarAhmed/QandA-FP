{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Requests where

import           ClassyPrelude.Yesod
import           Model
import           GHC.Generics                   ( )
import           Data.Aeson                     ( )
import           Import

data CreateQuestionRequest = CreateQuestionRequest
    { title :: Text
    , content :: Text
    , userId :: Key User
    } deriving (Generic, ToJSON, FromJSON)


data UpdateQuestionRequest = UpdateQuestionRequest
    { updatedTitle :: Maybe Text
    , updatedContent :: Maybe Text
    } deriving (Generic, ToJSON, FromJSON)


data CreateAnswerRequest = CreateAnswerRequest
    { newContent :: Text
    , creatorId :: Key User
    } deriving (Generic, ToJSON, FromJSON)

data UpdateAnswerRequest = UpdateAnswerRequest
    { updatedAnswerContent :: Text
    } deriving (Generic, ToJSON, FromJSON)

data DeleteAnswerRequest = DeleteAnswerRequest
    { deletorsId :: Key User
    } deriving (Generic, ToJSON, FromJSON)

---


newtype APIError = APIError { message :: Text } deriving (Generic, FromJSON, ToJSON)

badRequest :: Text -> Handler Value
badRequest = sendResponseStatus badRequest400 . toJSON . APIError
