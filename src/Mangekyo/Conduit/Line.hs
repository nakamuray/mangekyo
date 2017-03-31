{-# LANGUAGE OverloadedStrings #-}
module Mangekyo.Conduit.Line where

import Data.Conduit

import Control.Monad.Catch (MonadThrow)

import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT

import Mangekyo.Conduit
import Mangekyo.Type as Type

-- re-use word's fromValue
import Mangekyo.Conduit.Word (fromValue)

format :: Format
format = Format { name = "line"
                , input = Just toValue
                , output = Just fromValue
                }

toValue :: MonadThrow m => Conduit B.ByteString m Type.Value
toValue = CT.decode CT.utf8 =$= CT.lines =$= CL.map String
