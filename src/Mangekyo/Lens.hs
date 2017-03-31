{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Mangekyo.Lens
    ( at_
    , mapped_
    ) where

import Control.Lens as Lens

import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Traversable (mapM)

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Mangekyo.Type


-- Lens for Value
-- to put user defined functions into lenses, define it using monadic type
at_ :: Value -> Lens' (Mangekyo Value) (Mangekyo Value)
at_ key = lens (_getter key) (_setter key)

_getter :: Value -> Mangekyo Value -> Mangekyo Value
_getter k v = _getter' k <$> v

_getter' :: Value -> Value -> Value
_getter' (String key) (Object h) = fromMaybe Null $ H.lookup key h
_getter' k o@(Object _) = _getter' (string k) o
_getter' (Number s) (Array v)
    | Just i <- toBoundedInteger s = v !!! i
_getter' _ (Array _) = error "array index must be integer"
_getter' _ Null = Null
_getter' _ o = error $ "can't get from " ++ typeName o

_setter :: Value -> Mangekyo Value -> Mangekyo Value -> Mangekyo Value
_setter k o v = _setter' k <$> o <*> v

_setter' :: Value -> Value -> Value -> Value
_setter' (String key) (Object h) val = Object (H.insert key val h)
_setter' k o@(Object _) val = _setter' (string k) o val
_setter' (Number s) (Array v) val
    | Just i <- toBoundedInteger s = v /// (i, val)
_setter' _ (Array _) _ = error "array index must be integer"
_setter' k v@(Null) val = _setter' k (object v) val
_setter' _ o _ = error $ "can't set to " ++ typeName o

(!!!) :: V.Vector Value -> Int -> Value
(!!!) v i | i >= 0    = if i < V.length v then v V.! i else Null
          | otherwise = if negate i <= V.length v then v V.! (V.length v + i) else Null
(///) :: V.Vector Value -> (Int, Value) -> Value
(///) v (i, val) | i < 0          = let i' = if negate i <= V.length v then V.length v + i else 0
                                    in v /// (i', val)
                 | i < V.length v = Array $ v V.// [(i, val)]
                 | otherwise       = Array $ V.concat [ v
                                                      , V.replicate (i - V.length v) Null
                                                      , V.singleton Null
                                                      ]

mapped_ :: Setter' (Mangekyo Value) (Mangekyo Value)
mapped_ = sets mapped_'

mapped_' :: (Mangekyo Value -> Mangekyo Value) -> (Mangekyo Value) -> (Mangekyo Value)
mapped_' f v = do
    v' <- v
    case v' of
        Object h -> Object <$> mapM (f . pure) h
        Array vs -> Array <$> mapM (f . pure) vs
        Null -> return Null
        -- TODO: what should I do on other types?
        _ -> return v'
