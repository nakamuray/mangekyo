module Mangekyo.Conduit where

import Data.Conduit (Conduit)
import Data.ByteString.Char8 (ByteString)

import Mangekyo.Type

type Input = Conduit ByteString MangekyoIO Value
type Output = Conduit Value MangekyoIO ByteString

data Format = Format { name :: String
                     , input :: Maybe Input
                     , output :: Maybe Output
                     }
