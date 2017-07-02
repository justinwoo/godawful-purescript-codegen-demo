{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Generics.SOP
import qualified GHC.Generics as GHC

newtype Unused = Unused Text
  deriving (Show, GHC.Generic, ToJSON, FromJSON)
instance Generic Unused
instance HasDatatypeInfo Unused
newtype Flag = Flag Bool
  deriving (Show, GHC.Generic, ToJSON, FromJSON)
instance Generic Flag
instance HasDatatypeInfo Flag
newtype Created = Created Text
  deriving (Show, GHC.Generic, ToJSON, FromJSON)
instance Generic Created
instance HasDatatypeInfo Created
newtype Status = Status Text
  deriving (Show, GHC.Generic, ToJSON, FromJSON)
instance Generic Status
instance HasDatatypeInfo Status

newtype Url = Url Text
  deriving (Show, GHC.Generic, ToJSON, FromJSON)
instance Generic Url
instance HasDatatypeInfo Url

newtype Path = Path Text
  deriving (Show, GHC.Generic, ToJSON, FromJSON)
instance Generic Path
instance HasDatatypeInfo Path

newtype OpenRequest = OpenRequest
  { path :: Path
  } deriving (Show, GHC.Generic, ToJSON, FromJSON)
instance Generic OpenRequest
instance HasDatatypeInfo OpenRequest

data FileData = FileData
  { path :: Path
  , watched :: Flag
  } deriving (Show, GHC.Generic, ToJSON, FromJSON)
instance Generic FileData
instance HasDatatypeInfo FileData

data WatchedData = WatchedData
  { path :: Path
  , created :: Created
  } deriving (Show, GHC.Generic, ToJSON, FromJSON)
instance Generic WatchedData
instance HasDatatypeInfo WatchedData

newtype Success = Success
  { status :: Status
  } deriving (Show, GHC.Generic, ToJSON, FromJSON)
instance Generic Success
instance HasDatatypeInfo Success
