# godawful-codegen-demo

a demo using GHC8 Generics, Typeable, and some other crap to get enough information out from phantom type definitions and normal data types to output some purescript code.

a sane approach to this would probably use the purescript compiler library to actually build up the correct AST and output those using the various utilities in the compiler. however, as a madman, i have not thought to do this in a maintainable manner -- only to teach myself that such a thing is possible.

also, i've been too lazy to actually do it, but i don't think there's any places where i truly need to use Typeable since all information I need could be obtained through metadata (e.g. [Stephen Diehl's example](https://gist.github.com/sdiehl/d033bfdbb02760b23e45ffeca7482957)).

example output:

```hs
files :: Route Unused (Array Path)
  { method: GET
  , url: "/api/files"
  }

watched :: Route Unused (Array WatchedData)
  { method: GET
  , url: "/api/watched"
  }

open :: Route OpenRequest Success
  { method: POST
  , url: "/api/open"
  }

update :: Route FileData (Array WatchedData)
  { method: POST
  , url: "/api/update"
  }

newtype Unused = Unused String

newtype Path = Path String

newtype OpenRequest =
  { path :: Path
  }

newtype FileData =
  { path :: Path
  , watched :: Bool
  }

newtype WatchedData =
  { path :: Path
  , created :: String
  }

newtype Success =
  { status :: String
  }
```
