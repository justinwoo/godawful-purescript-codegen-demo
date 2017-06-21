{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module GeneratePS where

import Data.List (intercalate)
import Data.Proxy
import Data.Typeable
import GHC.Generics
import GHC.TypeLits
import Routes
import qualified Types as T

class ExtractFields f where
  extractFields :: g f -> [(String, String)]

instance ExtractFields f => ExtractFields (D1 m f) where
  extractFields _ = extractFields (Proxy :: Proxy f)

instance ExtractFields f => ExtractFields (C1 m f) where
  extractFields _ = extractFields (Proxy :: Proxy f)

instance
  ( Selector s
  , Typeable t
  )
  => ExtractFields (S1 s (K1 r t)) where
  extractFields _ =
    pure
      ( selName (undefined :: S1 s (K1 r t) ())
      , prop
      )
    where
      prop = convert result
      convert y = case y of
        "Text" -> "String"
        "(Array Char)" -> "String"
        name -> name
      x :: t = undefined
      result = case tyConName . typeRepTyCon . typeOf $ x of
        "[]" -> "(Array " ++ (convert . tyConName . typeRepTyCon $ head . typeRepArgs . typeOf $ x) ++ ")"
        a -> a


instance (ExtractFields a, ExtractFields b) => ExtractFields (a :*: b) where
  extractFields _ =
    extractFields (Proxy :: Proxy a) ++ extractFields (Proxy :: Proxy b)

instance (ExtractFields a, ExtractFields b) => ExtractFields (a :+: b) where
  extractFields _ =
    extractFields (Proxy :: Proxy a) ++ extractFields (Proxy :: Proxy b)

instance ExtractFields U1 where
  extractFields _ = []

instance ExtractFields K1 where
  extractFields _ = []

writeTypeDefinition :: forall a
  . Typeable a
  => ExtractFields (Rep a)
  => Proxy a
  -> String
writeTypeDefinition _ =
  "newtype " ++ name ++ " =" ++
    contents ++ "\n"
  where
    name = tyConName . typeRepTyCon $ typeRep (Proxy :: Proxy a)
    fields = extractFields (Proxy :: Proxy (Rep a))
    isNormalNewtype = case fields of
      x:_ | fst x == "" -> True
      _ -> False
    format (k, t) = k ++ " :: " ++ t
    contents = if isNormalNewtype
      then " " ++ name ++ " " ++ snd (head fields)
      else "\n  { " ++ fields' ++ "\n  }"
    fields' = intercalate "\n  , " $ format <$> fields

nameOf :: forall a
  . Generic a
  => GTypeName (Rep a)
  => Proxy a
  -> String
nameOf _ = gtypeName (from (undefined :: a))

class GTypeName f where
  gtypeName :: f a -> String

instance (GTypeName a, GTypeName b) => GTypeName (a :*: b) where
  gtypeName _ =
    gtypeName (undefined :: a ())
      ++ gtypeName (undefined :: b ())

instance (GTypeName a, GTypeName b) => GTypeName (a :+: b) where
  gtypeName _ =
    gtypeName (undefined :: a ())
      ++ gtypeName (undefined :: b ()) ++ ")"

instance (Datatype m, GTypeName f) => GTypeName (D1 m f) where
  gtypeName x = case datatypeName x of
    "[]" -> gtypeName (undefined :: f ())
    a -> a

instance (KnownSymbol n, GTypeName f) => GTypeName (C1 ('MetaCons n g s) f) where
  gtypeName _ = case symbolVal (Proxy :: Proxy n) of
    "[]" -> "(Array "
    ":" -> gtypeName (undefined :: f ())
    a -> "the fuck is this really: " ++ a

instance GTypeName f => GTypeName (S1 m f) where
  gtypeName _ = gtypeName (undefined :: f ())

instance GTypeName U1 where
  gtypeName _ = ""

instance Typeable f => GTypeName (K1 m f) where
  gtypeName _ =
    sanitize $ tyConName . typeRepTyCon $ typeRep (Proxy :: Proxy f)
    where
      sanitize xs
        | sx <- reverse xs
        , take 2 sx == "][" = reverse $ drop 2 sx -- wtf typeable why you do dis
      sanitize xs = xs

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
    req = nameOf (Proxy :: Proxy req)
    res = nameOf (Proxy :: Proxy res)

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
