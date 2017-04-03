{-# LANGUAGE OverloadedStrings #-}
module Mangekyo.Conduit.Null where

import Data.Conduit

import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow)
import Data.Monoid ((<>))

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Text as T
import qualified Data.Vector as V

import Mangekyo.Conduit
import Mangekyo.Type as Type

format :: Format
format = Format { name = "null"
                , input = Just toValue
                , output = Just fromValue
                }

toValue :: MonadThrow m => Conduit B.ByteString m Type.Value
toValue = splitNull =$= CT.decode CT.utf8 =$= CL.map String

splitNull :: Monad m => Conduit B.ByteString m B.ByteString
splitNull =
-- copied from Data.Conduit.Binary.lines
    loop []
  where
    loop acc = await >>= maybe (finish acc) (go acc)

    finish acc =
        let final = S.concat $ reverse acc
         in unless (S.null final) (yield final)

    go acc more =
        case S.uncons second of
            Just (_, second') -> yield (S.concat $ reverse $ first:acc) >> go [] second'
            Nothing -> loop $ more:acc
      where
        (first, second) = S.break (== 0) more

fromValue :: MonadThrow m => Conduit Type.Value m B.ByteString
fromValue = CL.map fromValue' =$= CT.encode CT.utf8

fromValue' :: Value -> T.Text
fromValue' (Tuple vs) = T.intercalate " " $ map toText vs ++ ["\NUL"]
fromValue' (Array v) = fromValue' (Tuple $ V.toList v)
fromValue' v          = toText v <> "\NUL"

toText :: Value -> T.Text
toText (String t) = t
toText v = string' v
