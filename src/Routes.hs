{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Types (Url(..), Path, WatchedData, OpenRequest, Success, FileData, Unused)
import Generics.SOP
import qualified GHC.Generics as GHC

data Method
  = GET
  | POST
  deriving (Show, GHC.Generic)
instance Generic Method
instance HasDatatypeInfo Method

data Route req res = Route
  { method :: Method
  , url :: Url
  } deriving (Show, GHC.Generic)
instance Generic (Route req res)
instance HasDatatypeInfo (Route req res)

files :: Route Unused [Path]
files = Route {method = GET, url = Url "/api/files"}

watched :: Route Unused [WatchedData]
watched = Route {method = GET, url = Url "/api/watched"}

open :: Route OpenRequest Success
open = Route {method = POST, url = Url "/api/open"}

update :: Route FileData [WatchedData]
update = Route {method = POST, url = Url "/api/update"}
