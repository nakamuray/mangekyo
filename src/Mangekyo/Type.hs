{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
module Mangekyo.Type
    ( Value(..)
    , Object
    , Array
    , Tuple
    , Function

    , MangekyoT
    , Namespace
    , Mangekyo
    , MangekyoIO

    , runMangekyo

    , typeName
    , object
    , array
    , string
    , string'
    , number
    , number'
    , bool
    , bool'
    , not_
    , unit
    , length_

    , module Control.Monad.Trans
    , module Control.Monad.State
    ) where

import Data.Conduit

import Control.Lens as Lens (Lens', Setter')
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.State (MonadState, StateT, evalStateT, get, modify, put)
import Data.Monoid ((<>))
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import Mangekyo.Utils


data Value = Object Object
           | Array Array
           | String Text
           | Number Scientific
           | Bool Bool
           | Null

           | Tuple Tuple
           | Function Function

           -- XXX: is it possible to provide Lenses as a "Function" type?
           | Lens (Lens' (Mangekyo Value) (Mangekyo Value))
           | Setter (Setter' (Mangekyo Value) (Mangekyo Value))

type Object = H.HashMap Text Value
type Array = V.Vector Value
type Tuple = [Value]
type Function = Value -> Mangekyo Value

instance Show Value where
    show (Object o) = "Object " ++ show o
    show (Array o) = "Array " ++ show o
    show (String o) = "String " ++ show o
    show (Number o) = "Number " ++ show o
    show (Bool o) = "Bool " ++ show o
    show Null = "Null"

    show (Tuple o) = "Tuple " ++ show o
    show (Function _) = "Function"
    show (Lens _) = "Lens"
    show (Setter _) = "Setter"

instance Eq Value where
    (Object h) == (Object h') = h == h'
    (Array v) == (Array v') = v == v'
    (String t) == (String t') = t == t'
    (Number n) == (Number n') = n == n'
    (Bool b) == (Bool b') = b == b'
    Null == Null = True

    (Tuple o) == (Tuple o') = o == o'
    _ == _ = False

instance A.FromJSON Value where
    parseJSON (A.Object h) = Object . H.fromList <$> (mapM (\(k, v) -> A.parseJSON v >>= \v' -> return (k, v')) $ H.toList h)
    parseJSON (A.Array v) = Array <$> V.mapM A.parseJSON v
    parseJSON (A.String t) = return $ String t
    parseJSON (A.Number n) = return $ Number n
    parseJSON (A.Bool b) = return $ Bool b
    parseJSON A.Null = return Null

instance A.ToJSON Value where
    toJSON (Object h) = A.Object $ H.map A.toJSON h
    toJSON (Array v) = A.Array $ V.map A.toJSON v
    toJSON (String t) = A.String t
    toJSON (Number n) = A.Number n
    toJSON (Bool b) = A.Bool b
    toJSON Null = A.Null

    toJSON (Tuple os) = A.Array . V.fromList $ map A.toJSON os
    toJSON (Function _) = A.String "<function>"
    toJSON (Lens _) = A.String "<lens>"
    toJSON (Setter _) = A.String "<setter>"

newtype MangekyoT m a = MangekyoT { runM :: StateT Namespace m a }
    deriving (MonadState Namespace, MonadTrans, MonadThrow, MonadIO, Monad, Applicative, Functor)

deriving instance MonadError e m => MonadError e (MangekyoT m)

type Namespace = Value

type Mangekyo = ConduitM Value Value (MangekyoT IO)
type MangekyoIO = MangekyoT IO

runMangekyo :: Monad m => MangekyoT m a -> Namespace -> m a
runMangekyo o n = flip evalStateT n $ runM o

typeName :: Value -> String
typeName (Object _) = "object"
typeName (Array _) = "array"
typeName (String _) = "string"
typeName (Number _) = "number"
typeName (Bool _) = "bool"
typeName Null = "null"
typeName (Tuple _) = "tuple"
typeName (Function _) = "function"
typeName (Lens _) = "lens"
typeName (Setter _) = "setter"

object :: Value -> Value
object o@(Object _) = o
object Null = Object H.empty
object v = error $ "undefined: object " <> show v

array :: Value -> Value
array a@(Array _) = a
array (Object h) = Array $ V.fromList $ map String $ H.keys h
array Null = Array V.empty
array (String t) = Array $ V.fromList $ map (String . T.singleton) $ T.unpack t
array o = error $ "undefined: list " <> show o

string :: Value -> Value
string o@(String _) = o
string (Number s) =
    -- XXX: to avoid ambiguous type warning, declare type explicitly
    case floatingOrInteger s :: Either Double Integer of
        Right i -> String $ showText i
        Left  f -> String $ showText f
string (Bool True) = String "true"
string (Bool False) = String "false"
string Null = String ""
string o@(Array _) = String $ string' o
string o@(Object _) = String $ string' o
string o = error $ "string: not implemented: " ++ typeName o

string' :: Value -> Text
string' (String t) = lb2text $ A.encode t
string' (Number s) =
    -- XXX: to avoid ambiguous type warning, declare type explicitly
    case floatingOrInteger s :: Either Double Integer of
        Right i -> showText i
        Left  f -> showText f
string' (Bool True) = "true"
string' (Bool False) = "false"
string' Null = ""
string' (Function _) = "<function>"
string' (Lens _) = "<lens>"
string' (Setter _) = "<setter>"
string' (Tuple os) = "(" <> T.intercalate ", " (map string' os) <> ")"
string' (Array v) = "[" <> T.intercalate ", " (map string' $ V.toList v) <> "]"
string' (Object h) = "{" <> T.intercalate ", " (map toKV $ H.toList h) <> "}"
  where
    toKV (k, v) = k <> ": " <> string' v

number :: Value -> Value
number o@(Number _) = o
number v = Number $ number' v

number' :: Value -> Scientific
number' (Number s) = s
number' (Object h) = fromIntegral $ H.size h
number' (Array v) = fromIntegral $ V.length v
number' (String t) = maybe 0 id $ parseNumber t
number' (Bool True) = 1
number' (Bool False) = 0
number' Null = 0
number' _ = error "numer: not implemented"

bool :: Value -> Value
bool v = Bool $ bool' v

bool' :: Value -> Bool
bool' Null = False
bool' (Bool False) = False
bool' _ = True

not_ :: Value -> Value
not_ (Bool True) = Bool False
not_ (Bool False) = Bool True
not_ v = not_ $ bool v

unit :: Value
unit = Tuple []

length_ :: Value -> Value
length_ (Array v) = Number $ fromIntegral $ V.length v
length_ (Object h) = Number $ fromIntegral $ H.size h
length_ (String t) = Number $ fromIntegral $ T.length t
length_ (Tuple l) = Number $ fromIntegral $ length l
length_ Null = Null
length_ v = error $ "undefined: length " ++ show v
