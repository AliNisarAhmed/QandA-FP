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
import           API.AuthAPI                    ( authServer
                                                , AuthApi
                                                )
import           Config                         ( App(..) )
import           GHC.Generics                   ( Generic )


type API = QuestionApi :<|> AnswerApi :<|> AuthApi

server :: ServerT API App
server = questionServer :<|> answerServer :<|> authServer

