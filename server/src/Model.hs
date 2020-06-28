{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Model where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Database.Persist.Sql
import           Database.Persist.TH            ( mkMigrate
                                                , mkPersist
                                                , persistLowerCase
                                                , share
                                                , sqlSettings
                                                )
import           GHC.Generics                   ( Generic )
import           Data.Time                      ( UTCTime )
import           Data.Text                      ( Text(..) )
import           Data.ByteString                ( ByteString )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    firstName Text
    lastName Text
    userName Text
    UniqueUserName userName
    pwd ByteString
    deriving Eq Show

Question json
    title Text
    content Text
    created UTCTime default=now()
    userId UserId
    deriving Eq Show

Answer json
    questionId QuestionId
    content Text
    userId UserId
    created UTCTime default=now()
    deriving Eq Show
|]
