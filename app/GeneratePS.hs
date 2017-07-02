{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module GeneratePS where

import Data.List (intercalate)
import Data.Proxy
import Data.Text (Text, unpack)
import Generics.SOP
import Routes
import qualified Types as T

-- because trying to take this out of the normal extractfields is too hard
class ExtractNewtypeField f where
  extractNewtypeField :: g f -> String

instance GTypeName a => ExtractNewtypeField (SOP I ('[ '[a] ])) where
  extractNewtypeField _ = gtypeName (Proxy @ a)

writeNewtypeDefinition :: forall a
  . Generic a
  => HasDatatypeInfo a
  => GTypeName (Rep a)
  => ExtractNewtypeField (Rep a)
  => Proxy a
  -> String
writeNewtypeDefinition _ =
  "newtype " ++ name ++ " = " ++ name ++ " " ++
    content ++ "\n"
  where
    name = nameOf (Proxy @ a)
    content = extractNewtypeField (Proxy @ (Rep a))

class ExtractFields f where
  extractFields :: g f -> [String]

-- pass around the inner structure, what could go wrong??
instance ExtractFields xs => ExtractFields (SOP I '[ xs ]) where
  extractFields _ = extractFields (Proxy @ xs)

-- take name and cons it
instance
  ( ExtractFields xs
  , Generic x
  , HasDatatypeInfo x
  , GTypeName (Rep x)
  ) => ExtractFields (x ': xs) where
  extractFields _ = nameOf (Proxy @ x) : extractFields (Proxy @ xs)

-- terminus
instance ExtractFields '[] where
  extractFields _ = []

writeTypeDefinition :: forall a
  . Generic a
  => HasDatatypeInfo a
  => GTypeName (Rep a)
  => ExtractFields (Rep a)
  => Proxy a
  -> String
writeTypeDefinition _ =
  "newtype " ++ name ++ " = " ++ name ++ " " ++
     contents ++ "\n"
  where
    name = nameOf (Proxy @ a)
    fields = extractFields (Proxy @ (Rep a))
    fieldNames :: [String]
    fieldNames = case constructorInfo $ datatypeInfo (Proxy @ a) of
      (Record _ xs) :* _ -> hcollapse $ hmap (K . fieldName) xs
      _ -> []
    contents = if length fieldNames == 0
      then head fields
      else
        "\n  { " ++
          (intercalate "\n  , " $
            zipWith (\a b -> a ++ " :: " ++ b) fieldNames fields
          ) ++
          "\n  }"

nameOf :: forall a
  . Generic a
  => HasDatatypeInfo a
  => GTypeName (Rep a)
  => Proxy a
  -> String
nameOf _ =
  case datatypeName (datatypeInfo (Proxy @ a)) of
    "[]" -> gtypeName (Proxy @ (Rep a))
    x -> x

class GTypeName f where
  gtypeName :: g f -> String

-- specific case to handle lists
instance HasDatatypeInfo a => GTypeName (SOP I ('[ '[], '[a, [a]] ])) where
  gtypeName _ = "(Array " ++ name ++ ")"
    where
      name = datatypeName $ datatypeInfo (Proxy @ a)

-- yup, only here to please compiler
instance GTypeName (SOP I ('[ xs ])) where
  gtypeName _ = error "UNUSED INSTANCE TO PLEASE COMPILER"

-- instances for primitives i'm using
instance GTypeName Text where
  gtypeName _ = "String"

instance GTypeName Bool where
  gtypeName _ = "Boolean"

writeRouteDefinition :: forall req res
  . Generic req
  => Generic res
  => HasDatatypeInfo req
  => HasDatatypeInfo res
  => GTypeName (Rep req)
  => GTypeName (Rep res)
  => String
  -> Route req res
  -> String
writeRouteDefinition name Route{..} =
  name ++ " :: " ++ routes ++ " " ++ req ++ " " ++ res ++ "\n" ++
  name ++ " =" ++ "\n" ++
    "  { method: " ++ show method ++ "\n" ++
    "  , url: " ++ show url' ++ "\n" ++
    "  }" ++ "\n"
  where
    url' | T.Url txt <- url = unpack txt
    routes = datatypeName $ datatypeInfo (Proxy @ (Route req res))
    req = nameOf (Proxy @ req)
    res = nameOf (Proxy @ res)

main :: IO ()
main = do
  putStrLn $ writeRouteDefinition "files" files
  putStrLn $ writeRouteDefinition "watched" watched
  putStrLn $ writeRouteDefinition "open" open
  putStrLn $ writeRouteDefinition "update" update
  putStrLn $ writeNewtypeDefinition (Proxy @ T.Unused)
  putStrLn $ writeNewtypeDefinition (Proxy @ T.Path)
  putStrLn $ writeNewtypeDefinition (Proxy @ T.Flag)
  putStrLn $ writeTypeDefinition (Proxy @ T.FileData)
  putStrLn $ writeTypeDefinition (Proxy @ T.OpenRequest)
  putStrLn $ writeTypeDefinition (Proxy @ T.WatchedData)
  putStrLn $ writeTypeDefinition (Proxy @ T.Success)
