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



---- Queries ----


getQuestions :: Handler [Entity Question]
getQuestions = runDB $ select $ from $ \question -> return question


createQuestion :: Question -> Handler (Maybe Question)
createQuestion q = do
  question <- runDB $ insert q
  runDB $ get question

