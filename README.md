# godawful-codegen-demo

a demo using Generics-SOP and Records-SOP to get enough information out from phantom type definitions and normal data types to output some purescript code (GHC8 Generics version moved to separate branch)

a sane approach to this would probably use the purescript compiler library to actually build up the correct AST and output those using the various utilities in the compiler. however, as a madman, i have not thought to do this in a maintainable manner -- only to teach myself that such a thing is possible.

Records-SOP especially allows the code for record handling to be as simple as

```hs
class ExtractFields f where
  extractFields :: g f -> [String]

instance
  ( ExtractFields xs
  , KnownSymbol name
  , PSTypeName a
  ) => ExtractFields ('(name, a) : xs) where
  extractFields _ = x : extractFields (Proxy @ xs)
    where
      name = symbolVal (Proxy @ name)
      value = getTypeName (Proxy @ a)
      x = name ++ " :: " ++ value

instance ExtractFields '[] where
  extractFields _ = []
```

behold, the output:

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
