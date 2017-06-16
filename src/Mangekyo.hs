{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
module Mangekyo
    ( run
    , runCode
    , dump

    , Option(..)
    , code
    , input
    , output

    , module Paths_mangekyo
    ) where

import Paths_mangekyo (version)

import Control.Lens
import Data.Conduit

import Data.Default (Default(def))
import System.IO (stdin, stdout)
import Text.Show.Pretty (ppShow)

import qualified Data.Conduit.Binary as CB
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Mangekyo.Type as Type
import Mangekyo.Parser (parseMangekyo)
import Mangekyo.Interpreter
import Mangekyo.Builtin
import Mangekyo.Conduit (Input, Output)

import qualified Mangekyo.Conduit.JSON as CJ


data Option = Option { _code :: String
                     , _input :: Input
                     , _output :: Output
                     , _dump :: Bool
                     }

makeLenses ''Option

instance Default Option where
    def = Option { _code = ""
                 , _input = CJ.toValue
                 , _output = CJ.fromValue
                 , _dump = False
                 }

run :: Option -> IO ()
run opt = do
    case (opt^.dump, parseMangekyo "<string>" $ T.pack (opt^.code)) of
        (_, Left e) -> error e
        (True, Right program) -> pprint program
        (_, Right program) -> do
            let mangekyo = interpret program >>= yieldIfNotUnit
                ns = Type.Object $ H.fromList builtins
            flip runMangekyo ns $ CB.sourceHandle stdin =$= opt^.input =$= mangekyo =$= opt^.output $$ CB.sinkHandle stdout

  where
    yieldIfNotUnit (Tuple []) = return ()
    yieldIfNotUnit v = yield v

runCode :: String -> IO ()
runCode s = run $ def & code .~ s

pprint :: Show a => a -> IO ()
pprint = putStrLn . ppShow
