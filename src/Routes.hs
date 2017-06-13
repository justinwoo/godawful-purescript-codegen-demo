{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Data.Text (Text)
import Types (Path, WatchedData, OpenRequest, Success, FileData, Unused)

data Method
  = GET
  | POST

data Route req res = Route
  { method :: Method
  , url :: Text
  }

files :: Route Unused [Path]
files = Route {method = GET, url = "/api/files"}

watched :: Route Unused [WatchedData]
watched = Route {method = GET, url = "/api/watched"}

open :: Route OpenRequest Success
open = Route {method = POST, url = "/api/open"}

update :: Route FileData [WatchedData]
update = Route {method = POST, url = "/api/update"}