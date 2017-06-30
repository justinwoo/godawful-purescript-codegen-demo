# godawful-codegen-demo

a demo using GHC8 Generics, Typeable, and some other crap to get enough information out from phantom type definitions and normal data types to output some purescript code.

a sane approach to this would probably use the purescript compiler library to actually build up the correct AST and output those using the various utilities in the compiler. however, as a madman, i have not thought to do this in a maintainable manner -- only to teach myself that such a thing is possible.

~~also, i've been too lazy to actually do it, but i don't think there's any places where i truly need to use Typeable since all information I need could be obtained through metadata (e.g. [Stephen Diehl's example](https://gist.github.com/sdiehl/d033bfdbb02760b23e45ffeca7482957)).~~ Updated this removing Typeable constraints from my top level definitions, hooray

update 2: removed Typeable usage entirely. uses some hacky tricks to do some stuff though:

* only newtypes of primitives are allowed, and record fields may only be newtypes.

this lets me use type literals for MetaSel (Maybe symbol) to figure out if i want to wrap types inside K1 with a Rep.

* uses overlapping instances for the cons constructor so that i can handle arguments to arrays. some product hackery is also required to make this work.

types of arrays have a pretty annoying representation in ghc generics, so i need to do some hacks to make things work like i want them to.

* instances are added to GTypeName for each primitive type i will support

well, this is entirely a good thing though, since now i can actually write down what types i support and their purescript representations.a

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
