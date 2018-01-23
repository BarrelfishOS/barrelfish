{-
  SockeyeMain.hs: Sockeye

  Copyright (c) 2017, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module Main where

import Control.Monad

import Data.List (intercalate)
import qualified Data.Map as Map

import System.Console.GetOpt
import System.Directory
import System.Exit
import System.Environment
import System.FilePath
import System.IO

import qualified SockeyeASTParser as ParseAST

import Text.Pretty.Simple (pPrint)

import SockeyeParser

{- Exit codes -}
usageError :: ExitCode
usageError = ExitFailure 1

fileError :: ExitCode
fileError = ExitFailure 2

parseError :: ExitCode
parseError = ExitFailure 3

checkError :: ExitCode
checkError = ExitFailure 4

buildError :: ExitCode
buildError = ExitFailure 5

{- Compilation targets -}
data Target = Prolog

{- Possible options for the Sockeye Compiler -}
data Options = Options
    { optInputFile  :: FilePath
    , optInclDirs   :: [FilePath]
    , optTarget     :: Target
    , optOutputFile :: FilePath
    , optDepFile    :: Maybe FilePath
    , optRootNs     :: Maybe String
     }

{- Default options -}
defaultOptions :: Options
defaultOptions = Options
    { optInputFile  = ""
    , optInclDirs   = [""]
    , optTarget     = Prolog
    , optOutputFile = ""
    , optDepFile    = Nothing
    , optRootNs     = Nothing
    }

{- Set the input file name -}
optSetInputFileName :: FilePath -> Options -> Options
optSetInputFileName f o = o { optInputFile = f }

optAddInclDir :: FilePath -> Options -> Options
optAddInclDir f o = o { optInclDirs = optInclDirs o ++ [f] }

{- Set the target -}
optSetTarget :: Target -> Options -> Options
optSetTarget t o = o { optTarget = t }

{- Set the output file name -}
optSetOutputFile :: FilePath -> Options -> Options
optSetOutputFile f o = o { optOutputFile = f }

{- Set the dependency file name -}
optSetDepFile :: FilePath -> Options -> Options
optSetDepFile f o = o { optDepFile = Just f }

{- Set the root namespace -}
optSetRootNs :: FilePath -> Options -> Options
optSetRootNs f o = o { optRootNs = Just f }

{- Prints usage information possibly with usage errors -}
usage :: [String] -> IO ()
usage errors = do
    prg <- getProgName
    let usageString = "Usage: " ++ prg ++ " [options] file\nOptions:"
    case errors of
        [] -> return ()
        _  -> hPutStrLn stderr $ concat errors
    hPutStrLn stderr $ usageInfo usageString options
    hPutStrLn stderr "The backend (capital letter options) specified last takes precedence."


{- Setup option parser -}
options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "P" ["Prolog"]
        (NoArg (\opts -> return $ optSetTarget Prolog opts))
        "Generate a prolog file that can be loaded into the SKB (default)."
    , Option "i" ["include"]
        (ReqArg (\f opts -> return $ optAddInclDir f opts) "DIR")
        "Add a directory to the search path where Sockeye looks for imports."
    , Option "o" ["output-file"]
        (ReqArg (\f opts -> return $ optSetOutputFile f opts) "FILE")
        "Output file in which to store the compilation result (required)."
    , Option "d" ["dep-file"]
        (ReqArg (\f opts -> return $ optSetDepFile f opts) "FILE")
        "Generate a dependency file for GNU make"
    , Option "r" ["root-ns"]
        (ReqArg (\f opts -> return $ optSetRootNs f opts) "IDENT")
        "Root namespace for generated nodes"
    , Option "h" ["help"]
        (NoArg (\_ -> do
                    usage []
                    exitWith ExitSuccess))
        "Show help."
    ]

{- Evaluates the compiler options -}
compilerOpts :: [String] -> IO (Options)
compilerOpts argv = do
    opts <- case getOpt Permute options argv of
        (actions, fs, []) -> do
            opts <- foldl (>>=) (return defaultOptions) actions
            case fs of
                []  -> do
                    usage ["No input file\n"]
                    exitWith usageError
                [f] -> return $ optSetInputFileName f opts
                _   -> do
                    usage ["Multiple input files not supported\n"]
                    exitWith usageError

        (_, _, errors) -> do
            usage errors
            exitWith $ usageError
    case optOutputFile opts of
        "" -> do
            usage ["No output file\n"]
            exitWith $ usageError
        _  -> return opts

{- Runs the parser on a single file -}
parseFile :: FilePath -> IO (ParseAST.Sockeye ParseAST.ParserMeta)
parseFile file = do
    src <- readFile file
    case parseSockeye file src of
        Left err -> do
            hPutStrLn stderr $ "Parse error at " ++ show err
            exitWith parseError
        Right ast -> return ast

main = do
    args <- getArgs
    opts <- compilerOpts args
    let
        inFile = optInputFile opts
        inclDirs = optInclDirs opts
        outFile = optOutputFile opts
        depFile = optDepFile opts
    parsedAst <- parseFile inFile
    pPrint parsedAst
