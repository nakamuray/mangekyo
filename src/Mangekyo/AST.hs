{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Mangekyo.AST where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.ByteString.UTF8 (ByteString)
import Text.Trifecta.Delta (Delta)


data Program = Program [Expression]
    deriving (Eq, {- Read, -} Show)

data Expression = Function Pattern [Expression]
                | FuncCall Location Expression Expression
                | Variable Text
                | String Text
                | Number Scientific
                | Tuple [Expression]
                | Array [Expression]
                | Object [(Text, Expression)]
                | Lens Expression
                | Pipe [Expression]
    deriving (Eq, {- Read, -} Show)

data Pattern = PVariable Text (Maybe Pattern)
             | PString Text
             | PNumber Scientific
             | PTuple [Pattern]
             | PArray [Pattern]
             | PObject [(Text, Pattern)]
    deriving (Eq, Read, Show)

-- FIXME: don't store source line for every expressions.
type Location = (Delta, SourceLine) -- ^ location within source code
type SourceLine = ByteString
