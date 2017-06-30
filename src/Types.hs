{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics

newtype Unused = Unused Text
  deriving (Show, Generic, ToJSON, FromJSON)
newtype Flag = Flag Bool
  deriving (Show, Generic, ToJSON, FromJSON)
newtype Created = Created Text
  deriving (Show, Generic, ToJSON, FromJSON)
newtype Status = Status Text
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Path = Path Text
  deriving (Show, Generic, ToJSON, FromJSON)

newtype OpenRequest = OpenRequest
  { path :: Path
  } deriving (Show, Generic, ToJSON, FromJSON)

data FileData = FileData
  { path :: Path
  , watched :: Flag
  } deriving (Show, Generic, ToJSON, FromJSON)

data WatchedData = WatchedData
  { path :: Path
  , created :: Created
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype Success = Success
  { status :: Status
  } deriving (Show, Generic, ToJSON, FromJSON)
