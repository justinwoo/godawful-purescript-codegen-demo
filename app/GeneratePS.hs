{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module GeneratePS where

import Data.List (intercalate)
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits
import Routes
import qualified Types as T

class ExtractFields f where
  extractFields :: g f -> [(String, String)]

instance ExtractFields f => ExtractFields (D1 m f) where
  extractFields _ = extractFields (Proxy @ f)

instance ExtractFields f => ExtractFields (C1 m f) where
  extractFields _ = extractFields (Proxy @ f)

-- distinction: newtypes are 'MetaSel 'Nothing, and I only allow primtives
instance
  ( GTypeName t
  )
  => ExtractFields (S1 ('MetaSel 'Nothing z x c) (K1 r t)) where
  extractFields _ =
    pure
      ( ""
      , gtypeName (Proxy @ t)
      )

-- distinction: record fields are 'MetaSel 'Just symbol, and I only allow newtypes
instance
  ( GTypeName (Rep t)
  , KnownSymbol name
  )
  => ExtractFields (S1 ('MetaSel ('Just name) z x c) (K1 r t)) where
  extractFields _ =
    pure
      ( symbolVal (Proxy @ name)
      , gtypeName (Proxy @ (Rep t))
      )

instance (ExtractFields a, ExtractFields b) => ExtractFields (a :*: b) where
  extractFields _ =
    extractFields (Proxy @ a) ++ extractFields (Proxy @ b)

writeTypeDefinition :: forall a
  . GTypeName (Rep a)
  => ExtractFields (Rep a)
  => Proxy a
  -> String
writeTypeDefinition _ =
  "newtype " ++ name ++ " = " ++ name ++ " " ++
    contents ++ "\n"
  where
    name = gtypeName (Proxy @ (Rep a))
    fields = extractFields (Proxy @ (Rep a))
    isNormalNewtype = case fields of
      x:_ | fst x == "" -> True
      _ -> False
    format (k, t) = k ++ " :: " ++ t
    contents = if isNormalNewtype
      then snd (head fields)
      else "\n  { " ++ fields' ++ "\n  }"
    fields' = intercalate "\n  , " $ format <$> fields

nameOf :: forall a
  . Generic a
  => GTypeName (Rep a)
  => Proxy a
  -> String
nameOf _ = gtypeName (Proxy @ (Rep a))

class GTypeName f where
  gtypeName :: g f -> String

instance (GTypeName f) => GTypeName (D1 m f) where
  gtypeName _ = gtypeName (Proxy @ f)

-- go into the Cons constructed type
instance {-# OVERLAPPING #-} (GTypeName f) => GTypeName (C1 ('MetaCons ":" g s) f) where
  gtypeName _ = gtypeName (Proxy @ f)

instance (KnownSymbol n) => GTypeName (C1 ('MetaCons n g s) f) where
  gtypeName _ = symbolVal (Proxy @ n)

-- take the left side, as this is the result of a type product from Cons
-- a :*: [a] or some shit, fucking ghc generics
instance (GTypeName a) => GTypeName (a :*: b) where
  gtypeName _ =
    gtypeName (Proxy @ a)

instance (GTypeName b) => GTypeName ((C1 ('MetaCons "[]" g s) f) :+: b) where
  gtypeName _ = "(Array " ++ gtypeName (Proxy @ b) ++ ")"

instance GTypeName f => GTypeName (S1 m f) where
  gtypeName _ = gtypeName (Proxy @ f)

instance GTypeName (Rep f) => GTypeName (K1 R f) where
  gtypeName _ = gtypeName (Proxy @ (Rep f))

instance GTypeName Text where
  gtypeName _ = "String"

instance GTypeName Bool where
  gtypeName _ = "Boolean"

writeRouteDefinition :: forall req res
  . Generic req
  => Generic res
  => GTypeName (Rep req)
  => GTypeName (Rep res)
  => String
  -> Route req res
  -> String
writeRouteDefinition name route@Route{..} =
  name ++ " :: " ++ routes ++ " " ++ req ++ " " ++ res ++ "\n" ++
  name ++ " =" ++ "\n" ++
    "  { method: " ++ show method ++ "\n" ++
    "  , url: " ++ show url ++ "\n" ++
    "  }" ++ "\n"
  where
    routes = datatypeName (from route)
    req = nameOf (Proxy @ req)
    res = nameOf (Proxy @ res)

main :: IO ()
main = do
  putStrLn $ writeRouteDefinition "files" files
  putStrLn $ writeRouteDefinition "watched" watched
  putStrLn $ writeRouteDefinition "open" open
  putStrLn $ writeRouteDefinition "update" update
  putStrLn $ writeTypeDefinition (Proxy @ T.Unused)
  putStrLn $ writeTypeDefinition (Proxy @ T.Path)
  putStrLn $ writeTypeDefinition (Proxy @ T.FileData)
  putStrLn $ writeTypeDefinition (Proxy @ T.OpenRequest)
  putStrLn $ writeTypeDefinition (Proxy @ T.WatchedData)
  putStrLn $ writeTypeDefinition (Proxy @ T.Success)
