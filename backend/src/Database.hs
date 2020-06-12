{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database where

import ClassyPrelude.Yesod hiding (Value, isNothing, on, (==.))
import Database.Esqueleto (Entity(..), select, from)
import Model
import Data.Aeson
import Import

data QuestionData = QuestionData (Entity Question)

instance ToJSON QuestionData where
  toJSON (QuestionData (Entity _ Question{..})) =
    object $ [
      "questionText" .= questionText
    , "questionAnswer" .= questionAnswer
    ]


---- Queries ----

getQuestions :: Handler [QuestionData]
getQuestions = do
  questions <-
    runDB $
      select $
      from $ \question -> do
        return question
  return $ QuestionData <$> questions
