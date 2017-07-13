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

import Data.List

import System.Console.GetOpt
import System.Exit
import System.Environment
import System.IO

import qualified SockeyeASTParser as ParseAST
import qualified SockeyeAST as AST
import qualified SockeyeASTDecodingNet as NetAST

import SockeyeParser
import SockeyeChecker
import SockeyeNetBuilder

import qualified SockeyeBackendProlog as Prolog

import Debug.Trace
import Text.Groom(groom)

{- Exit codes -}
usageError :: ExitCode
usageError = ExitFailure 1

parseError :: ExitCode
parseError = ExitFailure 2

checkError :: ExitCode
checkError = ExitFailure 3

buildError :: ExitCode
buildError = ExitFailure 4

{- Compilation targets -}
data Target = None | Prolog

{- Possible options for the Sockeye Compiler -}
data Options = Options { optInputFile  :: FilePath
                       , optTarget     :: Target
                       , optOutputFile :: Maybe FilePath
                       }

{- Default options -}
defaultOptions :: Options
defaultOptions = Options { optInputFile  = ""
                         , optTarget     = Prolog
                         , optOutputFile = Nothing
                         }

{- Set the input file name -}
optSetInputFileName :: FilePath -> Options -> Options
optSetInputFileName f o = o { optInputFile = f }

{- Set the target -}
optSetTarget :: Target -> Options -> Options
optSetTarget t o = o { optTarget = t }

{- Set the outpue file name -}
optSetOutputFile :: Maybe String -> Options -> Options
optSetOutputFile f o = o { optOutputFile = f }

{- Prints usage information possibly with usage errors -}
usage :: [String] -> IO ()
usage errors = do
    prg <- getProgName
    let usageString = "Usage: " ++ prg ++ " [options] file\nOptions:"
    hPutStrLn stderr $ usageInfo (concat errors ++ usageString) options
    hPutStrLn stderr "The backend (capital letter options) specified last takes precedence."


{- Setup option parser -}
options :: [OptDescr (Options -> IO Options)]
options = 
    [ Option "P" ["Prolog"]
        (NoArg (\opts -> return $ optSetTarget Prolog opts))
        "Generate a prolog file that can be loaded into the SKB (default)."
    , Option "C" ["Check"]
        (NoArg (\opts -> return $ optSetTarget None opts))
        "Just check the file, do not compile."
    , Option "o" ["output-file"]
        (ReqArg (\f opts -> return $ optSetOutputFile (Just f) opts) "FILE")
        "If no output file is specified the compilation result is written to stdout."
    , Option "h" ["help"]
        (NoArg (\_ -> do
                    usage []
                    exitWith ExitSuccess))
        "Show help."
    ]

{- evaluates the compiler options -}
compilerOpts :: [String] -> IO (Options)
compilerOpts argv =
    case getOpt Permute options argv of
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

{- Runs the parser -}
parseFile :: FilePath -> IO (ParseAST.SockeyeSpec)
parseFile file = do
    src <- readFile file
    case parseSockeye file src of
        Left err -> do
            hPutStrLn stderr $ "Parse error at " ++ show err
            exitWith parseError
        Right ast -> return ast

{- Runs the checker -}
checkAST :: ParseAST.SockeyeSpec -> IO AST.SockeyeSpec
checkAST parsedAst = do
    case checkSockeye parsedAst of 
        Left fail -> do
            hPutStr stderr $ show fail
            exitWith checkError
        Right intermAst -> return intermAst

{- Builds the decoding net from the Sockeye AST -}
buildNet :: AST.SockeyeSpec -> IO NetAST.NetSpec
buildNet ast = do
    case sockeyeBuildNet ast of 
        Left fail -> do
            hPutStr stderr $ show fail
            exitWith buildError
        Right netAst -> return netAst

{- Compiles the AST with the appropriate backend -}
compile :: Target -> NetAST.NetSpec -> IO String
compile None     _   = return ""
compile Prolog   ast = return $ Prolog.compile ast

{- Outputs the compilation result -}
output :: Maybe FilePath -> String -> IO ()
output outFile out = do
    case outFile of
        Nothing -> putStr out
        Just f  -> writeFile f out

main = do
    args <- getArgs
    opts <- compilerOpts args
    let inFile = optInputFile opts
    parsedAst <- parseFile inFile
    ast <- checkAST parsedAst
    netAst <- buildNet ast
    trace (groom netAst) $ return ()
    out <- compile (optTarget opts) netAst
    output (optOutputFile opts) out
    