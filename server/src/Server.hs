{-# LANGUAGE TypeOperators #-}

module Server
  ( server
  , API
  )
where

import           Servant
import           Data.Aeson
import           API.QuestionAPI                ( questionServer
                                                , QuestionApi
                                                )
import           API.AnswerAPI                  ( answerServer
                                                , AnswerApi
                                                )
import           Config                         ( App(..) )

type API = QuestionApi :<|> AnswerApi

server :: ServerT API App
server = questionServer :<|> answerServer
