{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Mangekyo.Builtin.Fmt (fmt) where

import Text.Trifecta

import Control.Applicative ((<$>), (<*), (*>), (<|>))
import Control.Monad (foldM)
import Data.Monoid ((<>), mempty)
import Data.Text (Text, unpack, snoc)

import Mangekyo.AST (Expression)
import Mangekyo.Interpreter (expr)
import Mangekyo.Parser (expression)
import Mangekyo.Type hiding (string)

-- $name .= "world"
-- fmt "hello #{name}" # => "hello world"
fmt :: Value -> Mangekyo Value
fmt (String f) =
    case expandPrep f of
        Failure e  -> error $ show e
        Success es -> String <$> foldM go "" es
  where
    go buf (Right c) = return $ buf `snoc` c
    go buf (Left e)  = (buf <>) <$> string'' <$> expr e

    string'' (String t) = t
    string'' v = string' v

fmt v = error $ "fmt: not a string: " <> show v

expandPrep :: Text -> Result [Either Expression Char]
expandPrep = parseString p_fmt mempty . unpack

p_fmt :: Parser [Either Expression Char]
p_fmt = many $ try (Right <$> p_escaped) <|> try (Left <$> p_expression) <|> (Right <$> anyChar)

p_escaped :: Parser Char
p_escaped = char '\\' >> char '#'

p_expression :: Parser Expression
p_expression = string "#{" *> expression <* char '}'

