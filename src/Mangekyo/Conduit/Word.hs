{-# LANGUAGE OverloadedStrings #-}
module Mangekyo.Conduit.Word where


import Data.Conduit

import Control.Monad.Catch (MonadThrow)
import Data.Monoid ((<>))

import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Text as T
import qualified Data.Vector as V

import Mangekyo.Conduit
import Mangekyo.Type as Type

format :: Format
format = Format { name = "word"
                , input = Just toValue
                , output = Just fromValue
                }

toValue :: MonadThrow m => Conduit B.ByteString m Type.Value
toValue = CT.decode CT.utf8 =$= CT.lines =$= CL.map (Array . V.fromList . map String . T.words)

fromValue :: MonadThrow m => Conduit Type.Value m B.ByteString
fromValue = CL.map fromValue' =$= CT.encode CT.utf8

fromValue' :: Value -> T.Text
fromValue' (Tuple vs) = T.intercalate " " $ map toText vs ++ ["\n"]
fromValue' (Array v) = fromValue' (Tuple $ V.toList v)
fromValue' v          = toText v <> "\n"

toText :: Value -> T.Text
toText (String t) = t
toText v = string' v
