{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric   #-}


module API.Requests where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text(..) )
import           Model
import           GHC.Generics                   ( Generic )
import           Data.Time                      ( UTCTime(..) )


data CreateQuestionRequest = CreateQuestionRequest
  { title :: Text
  , content :: Text
  , userId :: Key User
  } deriving (Eq, Show, Generic)

instance FromJSON CreateQuestionRequest
instance ToJSON CreateQuestionRequest


data UpdateQuestionRequest = UpdateQuestionRequest
  { updatedTitle :: Maybe Text
  , updatedContent :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON UpdateQuestionRequest
instance ToJSON UpdateQuestionRequest

data QuestionWithAnswers = QuestionWithAnswers
  { title :: Text
  , content :: Text
  , created :: UTCTime
  , userId :: Key User
  , answers :: [Answer]
  } deriving (Eq, Show, Generic)

instance FromJSON QuestionWithAnswers
instance ToJSON QuestionWithAnswers


data CreateAnswerRequest = CreateAnswerRequest
  { content :: Text
  , userId :: Key User
  } deriving (Eq, Show, Generic)

instance FromJSON CreateAnswerRequest
instance ToJSON CreateAnswerRequest

data UpdateAnswerRequest = UpdateAnswerRequest
  { newContent :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON UpdateAnswerRequest
instance ToJSON UpdateAnswerRequest


data SignupForm = SignupForm
  { firstName :: Text
  , lastName :: Text
  , userName :: Text
  , password :: Text
  , confirmPassword :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON SignupForm
instance ToJSON SignupForm

data LoginForm = LoginForm
  { email :: Text
  , password :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON LoginForm
instance ToJSON LoginForm
