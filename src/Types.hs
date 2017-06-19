{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics

newtype Unused = Unused Text
  deriving (Show, Generic, Typeable, ToJSON, FromJSON)

newtype Path = Path Text
  deriving (Show, Generic, Typeable, ToJSON, FromJSON)

newtype OpenRequest = OpenRequest
  { path :: Path
  } deriving (Show, Generic, Typeable, ToJSON, FromJSON)

data FileData = FileData
  { path :: Path
  , watched :: Bool
  } deriving (Show, Generic, Typeable, ToJSON, FromJSON)

data WatchedData = WatchedData
  { path :: Path
  , created :: Text
  } deriving (Show, Generic, Typeable, ToJSON, FromJSON)

data Success = Success
  { status :: Text
  } deriving (Show, Generic, Typeable, ToJSON, FromJSON)
