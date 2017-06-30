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

import SockeyeASTFrontend as AST1
import SockeyeASTBackend as AST2

import SockeyeParser
import SockeyeChecker
import qualified SockeyeBackendPrintAST as PrintAST
import qualified SockeyeBackendProlog as Prolog

{- Compilation targets -}
data Target = None | PrintAST | Prolog

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
    , Option "A" ["AST"]
        (NoArg (\opts -> return $ optSetTarget PrintAST opts))
        "Print the AST."
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
            case fs of []  -> do
                                usage ["No input file\n"]
                                exitWith $ ExitFailure 1
                       [f] -> return $ optSetInputFileName f opts
                       _   -> do
                                usage ["Multiple input files not supported\n"]
                                exitWith $ ExitFailure 1

        (_, _, errors) -> do
            usage errors
            exitWith $ ExitFailure 1

{- Runs the parser -}
parseFile :: FilePath -> IO (AST2.NetSpec)
parseFile file = do
    src <- readFile file
    case parseSockeye file src of
        Left err -> do
            hPutStrLn stderr $ "Parse error at " ++ show err
            exitWith $ ExitFailure 2
        Right ast -> return ast

{- Runs the checker -}
checkAST :: AST2.NetSpec -> IO ()
checkAST ast = do
    case checkSockeye ast of 
        [] -> return ()
        errors -> do
            hPutStr stderr $ unlines (foldl flattenErrors ["Failed checks:"] errors)
            exitWith $ ExitFailure 3
        where flattenErrors es (key, errors)
                = let indented = map ((replicate 4 ' ') ++) errors
                  in es ++ case key of Nothing     -> errors
                                       Just nodeId -> ("In specification of node '" ++ show nodeId ++ "':"):indented

{- Compiles the AST with the appropriate backend -}
compile :: Target -> AST2.NetSpec -> IO String
compile None     _   = return ""
compile PrintAST ast = return $ PrintAST.compile ast
compile Prolog   ast = return $ Prolog.compile ast

{- Outputs the compilation result -}
output :: Maybe FilePath -> String -> IO ()
output outFile out = do
    case outFile of Nothing -> putStr out
                    Just f  -> writeFile f out

main = do
    args <- getArgs
    opts <- compilerOpts args
    let inFile = optInputFile opts
    ast <- parseFile inFile
    checkAST ast
    out <- compile (optTarget opts) ast
    output (optOutputFile opts) out
    