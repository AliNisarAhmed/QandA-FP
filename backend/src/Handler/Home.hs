{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Handler.Home where

import           Import
import           Data.Aeson

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

data Question = Question
    { questionText :: String
    , questionAnswer :: String
    } deriving (Generic, Show)

instance ToJSON Question where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Question


questions :: [Question]
questions = [Question "Hello" "World", Question "Haskell" "And elm"]

getHomeR :: Handler Value
getHomeR = return $ object [ "questions" .= questions]
