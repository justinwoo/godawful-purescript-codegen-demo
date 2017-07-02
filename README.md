# godawful-codegen-demo -- Generics-SOP version

This branch contains and implementation using Generics-SOP.

example output:

```purs
files :: Route Unused (Array Path)
files =
  { method: GET
  , url: "/api/files"
  }

watched :: Route Unused (Array WatchedData)
watched =
  { method: GET
  , url: "/api/watched"
  }

open :: Route OpenRequest Success
open =
  { method: POST
  , url: "/api/open"
  }

update :: Route FileData (Array WatchedData)
update =
  { method: POST
  , url: "/api/update"
  }

newtype Unused = Unused String

newtype Path = Path String

newtype Flag = Flag Boolean

newtype FileData = FileData
  { path :: Path
  , watched :: Flag
  }

newtype OpenRequest = OpenRequest
  { path :: Path
  }

newtype WatchedData = WatchedData
  { path :: Path
  , created :: Created
  }

newtype Success = Success
  { status :: Status
  }
```
