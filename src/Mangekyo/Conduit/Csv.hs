{-# LANGUAGE FlexibleContexts #-}
module Mangekyo.Conduit.Csv where

import Data.Conduit

import Data.Csv (HasHeader(..), defaultEncodeOptions, defaultDecodeOptions)
import Data.Csv.Conduit (fromCsvLiftError, toCsv)

import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Vector as V

import Mangekyo.Conduit
import Mangekyo.Type as Type

format :: Format
format = Format { name = "csv"
                , input = Just toValue
                , output = Just fromValue
                }

toValue :: Conduit B.ByteString MangekyoIO Type.Value
toValue = fromCsvLiftError errorHandler defaultDecodeOptions NoHeader =$= CL.map (Array . V.fromList . map String)
  where
    errorHandler e = userError $ show e

fromValue :: Monad m => Conduit Type.Value m B.ByteString
fromValue = CL.map toRecord =$= toCsv defaultEncodeOptions

toRecord :: Value -> [T.Text]
toRecord (Tuple vs) = map toText vs
toRecord (Array v) = map toText $ V.toList v
toRecord (String t) = [t]
toRecord v = [toText v]

toText :: Value -> T.Text
toText (String t) = t
toText v = string' v
