module Mangekyo.Conduit.JSON where

import Data.Conduit

import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans (liftIO)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Attoparsec.ByteString.Char8 (endOfInput, skipSpace)
import System.IO (hPutStrLn, stderr)

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL

import Mangekyo.Conduit
import Mangekyo.Type as Type

format :: Format
format = Format { name = "json"
                , input = Just toValue
                , output = Just fromValue
                }

toValue :: (MonadIO m, MonadThrow m) => Conduit B.ByteString m Type.Value
toValue = CA.conduitParser jsonOrEmpty =$= awaitForever (yieldObj . fmap A.fromJSON . snd)
  where
    jsonOrEmpty = (Just <$> A.json) <|> (Nothing <$ (skipSpace >> endOfInput))

    yieldObj (Just (A.Error e))     = liftIO $ hPutStrLn stderr e
    yieldObj (Just (A.Success obj)) = yield obj
    yieldObj Nothing                = return ()


fromValue :: Monad m => Conduit Type.Value m B.ByteString
fromValue = CL.map (flip B.snoc '\n' . BL.toStrict . encodePretty)
