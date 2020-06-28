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
import qualified Servant.Auth.Server           as SAS


type API = QuestionApi :<|> AnswerApi :<|> AuthApi

server :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT API App
server cs jwts = questionServer :<|> answerServer :<|> (authServer cs jwts)

