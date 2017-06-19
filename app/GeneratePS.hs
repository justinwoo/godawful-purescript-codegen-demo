{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module GeneratePS where

import Data.List (intercalate)
import Data.Proxy
import Data.Typeable
import GHC.Generics
import Routes
import qualified Types as T

class ExtractFields f where
  extractFields :: Proxy f -> [(String, String)]

instance ExtractFields f => ExtractFields (D1 m f) where
  extractFields _ = extractFields (Proxy :: Proxy f)

instance ExtractFields f => ExtractFields (C1 m f) where
  extractFields _ = extractFields (Proxy :: Proxy f)

instance (Selector s, Typeable t) => ExtractFields (S1 s (K1 r t)) where
  extractFields _ =
    pure
      ( selName (undefined :: S1 s (K1 r t) ())
      , nameOf (undefined :: t)
      )

instance (ExtractFields a, ExtractFields b) => ExtractFields (a :*: b) where
  extractFields _ =
    extractFields (Proxy :: Proxy a) ++ extractFields (Proxy :: Proxy b)

instance ExtractFields U1 where
  extractFields _ = []

writeTypeDefinition :: forall a
  . Typeable a
  => Generic a
  => ExtractFields (Rep a)
  => Proxy a
  -> String
writeTypeDefinition _ =
  "newtype " ++ name ++ " =" ++
    contents ++ "\n"
  where
    name = tyConName . typeRepTyCon $ typeOf (undefined :: a)
    fields = extractFields (Proxy :: Proxy (Rep a))
    isNormalNewtype = case fields of
      x:_ | fst x == "" -> True
      _ -> False
    format (k, t) = k ++ " :: " ++ t
    contents = if isNormalNewtype
      then " " ++ name ++ " " ++ snd (head fields)
      else "\n  { " ++ fields' ++ "\n  }"
    fields' = intercalate "\n  , " $ format <$> fields

nameOf :: Typeable a => a -> String
nameOf x = convert result
  where
    convert y = case y of
      "Text" -> "String"
      name -> name
    result = case tyConName . typeRepTyCon . typeOf $ x of
      "[]" -> "(Array " ++ (convert . tyConName . typeRepTyCon $ head . typeRepArgs $ typeOf x) ++ ")"
      a -> a

writeRouteDefinition :: forall req res
  . Typeable req
  => Typeable res
  => String
  -> Route req res
  -> String
writeRouteDefinition name route@Route{..} =
  name ++ " :: " ++ routes ++ " " ++ req ++ " " ++ res ++ "\n" ++
    "  { method: " ++ show method ++ "\n" ++
    "  , url: " ++ show url ++ "\n" ++
    "  }" ++ "\n"
  where
    routes = datatypeName (from route)
    req = nameOf (undefined :: req)
    res = nameOf (undefined :: res)

main :: IO ()
main = do
  putStrLn $ writeRouteDefinition "files" files
  putStrLn $ writeRouteDefinition "watched" watched
  putStrLn $ writeRouteDefinition "open" open
  putStrLn $ writeRouteDefinition "update" update
  putStrLn $ writeTypeDefinition (Proxy :: Proxy T.Unused)
  putStrLn $ writeTypeDefinition (Proxy :: Proxy T.Path)
  putStrLn $ writeTypeDefinition (Proxy :: Proxy T.OpenRequest)
  putStrLn $ writeTypeDefinition (Proxy :: Proxy T.FileData)
  putStrLn $ writeTypeDefinition (Proxy :: Proxy T.WatchedData)
  putStrLn $ writeTypeDefinition (Proxy :: Proxy T.Success)
