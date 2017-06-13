{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Generics.SOP as SOP
import GHC.Generics (Generic)

newtype Unused = Unused Text
  deriving (Generic, ToJSON, FromJSON)
instance SOP.Generic Unused

newtype Path = Path Text
  deriving (Generic, ToJSON, FromJSON)
instance SOP.Generic Path

data OpenRequest = OpenRequest
  { path :: Path
  } deriving (Generic, ToJSON, FromJSON)
instance SOP.Generic OpenRequest

data FileData = FileData
  { path :: Path
  , watched :: Bool
  } deriving (Generic, ToJSON, FromJSON)
instance SOP.Generic FileData

data WatchedData = WatchedData
  { path :: Path
  , created :: Text
  } deriving (Generic, ToJSON, FromJSON)
instance SOP.Generic WatchedData

data Success = Success
  { status :: Text
  } deriving (Generic, ToJSON, FromJSON)
instance SOP.Generic Success
