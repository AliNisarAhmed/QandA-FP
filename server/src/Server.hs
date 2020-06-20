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
import           Config                         ( App(..) )

type API = QuestionApi

server :: ServerT API App
server = questionServer
