{-# LANGUAGE OverloadedStrings #-}
{-
    Labeled Tab-separated Values

    see http://ltsv.org/
-}
module Mangekyo.Conduit.LTSV where

import Control.Applicative
import Control.Monad.Catch (MonadThrow)
import Data.Conduit
import Data.Attoparsec.ByteString.Char8 as A
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Mangekyo.Conduit
import Mangekyo.Type

format :: Format
format = Format { name = "ltsv"
                , input = Just toValue
                , output = Just fromValue
                }

toValue :: MonadThrow m => Conduit B.ByteString m Value
toValue = CA.conduitParser ltsv =$= awaitForever (yield . snd)

fromValue :: MonadThrow m => Conduit Value m B.ByteString
fromValue = CL.map fromValue' =$= CT.encode CT.utf8

fromValue' :: Value -> Text
fromValue' (Tuple vs) = T.concat $ map fromValue' vs
fromValue' (Object h) = (T.intercalate "\t" $ map (\(k, v) -> stripLabel k <> ":" <> stripValue (toText v)) $ H.toList h) <> "\n"
fromValue' v = error $ "not a object: " ++ show v

toText :: Value -> Text
toText (String t) = t
toText v = string' v

-- XXX: is there any escaping rule for these chars?
stripLabel, stripValue :: Text -> Text
stripLabel = T.filter lbyte
stripValue = T.filter fbyte

--

ltsv :: Parser Value
ltsv = Object <$> H.map String <$> H.fromList <$> (record <* nl)

record :: Parser [(Text, Text)]
record = field `sepBy1` tab

field :: Parser (Text, Text)
field = do
    l <- label
    char ':'
    v <- fieldValue
    return (l, v)

label :: Parser Text
label = decodeUtf8 <$> takeWhile1 lbyte

fieldValue :: Parser Text
fieldValue = decodeUtf8 <$> A.takeWhile fbyte

tab, nl :: Parser ()
tab = () <$ char '\t'
nl = () <$ (A.string "\r\n" <|> A.string "\n")

lbyte, fbyte :: Char -> Bool
lbyte = inClass $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "_.-"
fbyte = notInClass "\t\r\n" -- %x01-08 / %x0B / %x0C / %x0E-FF
