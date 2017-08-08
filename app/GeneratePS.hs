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
{-# LANGUAGE DefaultSignatures #-}

module GeneratePS where

import Data.List (intercalate)
import Data.Proxy
import Data.Text (Text, unpack)
import Generics.SOP
import Generics.SOP.Record
import GHC.TypeLits
import Routes
import qualified Types as T

writeNewtypeDefinition :: forall a b
   . IsNewtype a b
  => PSTypeName a
  => PSTypeName b
  => Proxy a -> String
writeNewtypeDefinition proxy =
  "newtype " ++ name ++ " = " ++ name ++ " " ++ inner
  where
    name = getTypeName proxy
    inner = getTypeName (Proxy @ b)

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

writeRecordTypeDefinition :: forall a r
   . IsRecord a r
  => PSTypeName a
  => ExtractFields r
  => Proxy a -> String
writeRecordTypeDefinition proxy =
  "newtype " ++ name ++ " = " ++ name
    ++ "\n  { " ++ fields ++ "\n  }\n"
  where
    name = getTypeName proxy
    fields = intercalate "\n  , " $ extractFields (Proxy @ r)

class PSTypeName a where
  getTypeName :: forall g. g a -> String

  default getTypeName :: forall g
    . HasDatatypeInfo a
    => g a -> String
  getTypeName = datatypeName . datatypeInfo

instance
  ( PSTypeName a
  ) => PSTypeName [a] where
  getTypeName _ = "(Array " ++ getTypeName (Proxy @ a) ++ ")"

instance PSTypeName Text where
  getTypeName _ = "String"

instance PSTypeName Bool where
  getTypeName _ = "Boolean"

instance PSTypeName T.Unused
instance PSTypeName T.Path
instance PSTypeName T.Flag
instance PSTypeName T.Created
instance PSTypeName T.Status
instance PSTypeName T.FileData
instance PSTypeName T.OpenRequest
instance PSTypeName T.WatchedData
instance PSTypeName T.Success

writeRouteDefinition :: forall req res
   . PSTypeName req
  => PSTypeName res
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
    req = getTypeName (Proxy @ req)
    res = getTypeName (Proxy @ res)

main :: IO ()
main = do
  putStrLn $ writeRouteDefinition "files" files
  putStrLn $ writeRouteDefinition "watched" watched
  putStrLn $ writeRouteDefinition "open" open
  putStrLn $ writeRouteDefinition "update" update
  putStrLn $ writeNewtypeDefinition (Proxy @ T.Unused)
  putStrLn $ writeNewtypeDefinition (Proxy @ T.Path)
  putStrLn $ writeNewtypeDefinition (Proxy @ T.Flag)
  putStrLn $ writeRecordTypeDefinition (Proxy @ T.FileData)
  putStrLn $ writeRecordTypeDefinition (Proxy @ T.OpenRequest)
  putStrLn $ writeRecordTypeDefinition (Proxy @ T.WatchedData)
  putStrLn $ writeRecordTypeDefinition (Proxy @ T.Success)
