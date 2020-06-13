{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances   #-}


module Database where

import           ClassyPrelude.Yesod     hiding ( Value
                                                , isNothing
                                                , on
                                                , (==.)
                                                )
import           Database.Esqueleto             ( select
                                                , from
                                                )
import           Model
import           Data.Aeson                     ( )
import           Import
import           Requests



---- Queries ----



getQuestions :: Handler [Entity Question]
getQuestions = runDB $ select $ from $ \question -> return question


createQuestion :: CreateQuestionRequest -> Handler (Maybe Question)
createQuestion cqR = do
  now      <- liftIO getCurrentTime
  question <- runDB
    $ insert (Question (title cqR) (content cqR) now (userId cqR))
  runDB $ get question

checkUser :: Key User -> Handler (Maybe (Entity User))
checkUser userId = runDB $ selectFirst [UserId ==. userId] []
