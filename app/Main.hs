module Main where

import Options.Applicative
import Options.Applicative.Help (text)

import Data.Semigroup ((<>))
import Data.Version (showVersion)

import Mangekyo
import Mangekyo.Conduit as MC

import qualified Mangekyo.Conduit.Csv as CC
import qualified Mangekyo.Conduit.JSON as CJ
import qualified Mangekyo.Conduit.Line as CLi
import qualified Mangekyo.Conduit.LTSV as CLt
import qualified Mangekyo.Conduit.Word as CW

-- temporary data structure to parse "-i -o" and "-b"
-- XXX: can I skip it and directory create Option?
data COption = COption { cmdIO :: (Input, Output)
                       , cmdDump :: Bool
                       , cmdCode :: String
                       }

optToOpt :: COption -> Option
optToOpt (COption (i, o) d c ) = Option { _code = c
                                        , _input = i
                                        , _output = o
                                        , _dump = d
                                        }


parser :: Parser COption
parser = COption
      <$> (parseInOut <|> parseBoth)
      <*> switch (long "dump" <> short 'd' <> hidden <> help "dump AST and exit")
      <*> strArgument (metavar "CODE")
  where
    parseInOut = (,)
      <$> option readInput (long "input"
                           <> short 'i'
                           <> value CJ.toValue
                           <> metavar "FORMAT"
                           <> help "input format (default: json)")
      <*> option readOutput (long "output"
                            <> short 'o'
                            <> value CJ.fromValue
                            <> metavar "FORMAT"
                            <> help "output format (default: json)")
    parseBoth = option readBoth (long "both"
                                <> short 'b'
                                <> metavar "FORMAT"
                                <> help "set both input and output format")

    readInput = maybeReader (flip lookup inputMap)
    readOutput = maybeReader (flip lookup outputMap)
    readBoth = maybeReader (\s -> (,) <$> flip lookup inputMap s <*> flip lookup outputMap s)

    inputMap = makeMap MC.input availableFormats
    outputMap = makeMap MC.output availableFormats

    makeMap g (f:formats)
        | Just f' <- g f = (name f, f') : makeMap g formats
        | otherwise = makeMap g formats
    makeMap _ [] = []

availableFormats :: [Format]
availableFormats = [CC.format, CJ.format, CLi.format, CLt.format, CW.format]

main :: IO ()
main = run . optToOpt =<< execParser opts
  where
    opts = info (parser <**> versioner <**> helper)
        ( fullDesc
        <> header ("mangekyo - lenses within pipes [version: " ++ strVersion ++ "]")
        <> footerDoc (Just $ text formatHelp))

    strVersion = showVersion version
    versioner = infoOption ("mangekyo-" ++ strVersion) (long "version" <> short 'v' <> hidden <> help "show version and exit")

    formatHelp = unlines $ "Available FORMATs:"
                         : map formatFormat availableFormats
    formatFormat (Format n _ Nothing) = "   " ++ n ++ " (input only)"
    formatFormat (Format n Nothing _) = "   " ++ n ++ " (output only)"
    formatFormat (Format n _ _) = "   " ++ n
