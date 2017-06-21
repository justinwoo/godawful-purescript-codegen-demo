{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics

newtype Unused = Unused String
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Path = Path String
  deriving (Show, Generic, ToJSON, FromJSON)

newtype OpenRequest = OpenRequest
  { path :: Path
  } deriving (Show, Generic, ToJSON, FromJSON)

data FileData = FileData
  { path :: Path
  , watched :: Bool
  } deriving (Show, Generic, ToJSON, FromJSON)

data WatchedData = WatchedData
  { path :: Path
  , created :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype Success = Success
  { status :: String
  } deriving (Show, Generic, ToJSON, FromJSON)
