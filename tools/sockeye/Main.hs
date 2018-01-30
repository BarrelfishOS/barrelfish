{-
  SockeyeMain.hs: Sockeye

  Copyright (c) 2018, ETH Zurich.

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

import qualified SockeyeParserAST as ParseAST
import qualified SockeyeSymbolTable as SymTable

import Text.Pretty.Simple (pPrint, pShowNoColor)
import Data.Text.Lazy (unpack)

import SockeyeParser
import SockeyeSymbolTableBuilder

{- Exit codes -}
usageError :: ExitCode
usageError = ExitFailure 1

fileError :: ExitCode
fileError = ExitFailure 2

parseError :: ExitCode
parseError = ExitFailure 3

checkError :: ExitCode
checkError = ExitFailure 4

compileError :: ExitCode
compileError = ExitFailure 5

{- Compilation targets -}
data Target
    = Prolog
    | Isabelle

{- Possible options for the Sockeye Compiler -}
data Options = Options
    { optInputFile    :: FilePath
    , optInclDirs     :: [FilePath]
    , optTarget       :: Target
    , optOutputFile   :: FilePath
    , optDepFile      :: Maybe FilePath
    , optParseAstDump :: String
    , optSymTableDump :: String
    , optAstDump      :: String
    }

{- Default options -}
defaultOptions :: Options
defaultOptions = Options
    { optInputFile    = ""
    , optInclDirs     = [""]
    , optTarget       = Prolog
    , optOutputFile   = ""
    , optDepFile      = Nothing
    , optParseAstDump = ""
    , optSymTableDump = ""
    , optAstDump      = ""
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

{- Set dump target for AST -}
optSetParseAstDump :: String -> Options -> Options
optSetParseAstDump t o = o { optParseAstDump = t }

{- Set dump target for symbol table -}
optSetSymTableDump :: String -> Options -> Options
optSetSymTableDump t o = o { optSymTableDump = t }

{- Set dump target for AST -}
optSetAstDump :: String -> Options -> Options
optSetAstDump t o = o { optAstDump = t }

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
    , Option "I" ["Isabelle"]
        (NoArg (\opts -> return $ optSetTarget Isabelle opts))
        "Generate Isabelle/HOL code."
    , Option "i" ["include"]
        (ReqArg (\f opts -> return $ optAddInclDir f opts) "DIR")
        "Add a directory to the search path where Sockeye looks for imports."
    , Option "o" ["output-file"]
        (ReqArg (\f opts -> return $ optSetOutputFile f opts) "FILE")
        "Output file in which to store the compilation result (required)."
    , Option "d" ["dep-file"]
        (ReqArg (\f opts -> return $ optSetDepFile f opts) "FILE")
        "Generate a dependency file for GNU make"
    , Option "" ["parser-dump"]
        (ReqArg (\f opts -> return $ optSetParseAstDump f opts) "'c'|'f'")
        "Dump parser AST to console ('c') or file ('f')."
    , Option "" ["st-dump"]
        (ReqArg (\f opts -> return $ optSetSymTableDump f opts) "'c'|'f'")
        "Dump symbol table to console ('c') or file ('f')."
    , Option "" ["ast-dump"]
        (ReqArg (\f opts -> return $ optSetAstDump f opts) "'c'|'f'")
        "Dump AST to console ('c') or file ('f')."
    , Option "h" ["help"]
        (NoArg (\_ -> usage [] >> exitWith ExitSuccess))
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
parseFile :: FilePath -> IO ParseAST.Sockeye
parseFile file = do
    src <- readFile file
    case parseSockeye file src of
        Left err -> do
            hPutStrLn stderr $ "Parse error at " ++ show err
            exitWith parseError
        Right ast -> return ast

{- Builds the symbol table from the parsed AST -}
buildSymTable :: ParseAST.Sockeye -> IO SymTable.Sockeye
buildSymTable ast = do
    case buildSymbolTable ast of
        Left fail -> do
            hPutStr stderr $ show fail
            exitWith checkError
        Right symTable -> return symTable

{- Compiles the AST with the selected backend -}
compile :: Target -> SymTable.Sockeye -> ParseAST.Sockeye -> IO String
compile Prolog symTable ast = hPutStrLn stderr "Prolog backend NYI" >> exitWith compileError
compile Isabelle symTable ast = hPutStrLn stderr "Isabelle backend NYI" >> exitWith compileError

{- Outputs the compilation result -}
output :: FilePath -> String -> IO ()
output outFile out = writeFile outFile out

{- Produce debug output -}
debugOutput :: Options -> SymTable.Sockeye -> ParseAST.Sockeye -> IO ()
debugOutput opts symTable ast = do
    let
        inFile = optInputFile opts
        astDump = optAstDump opts
        stDump = optSymTableDump opts
    case stDump of
        "c" -> putStrLn "Dumping Symbol Table..." >> putStrLn "***********************" >> pPrint symTable
        "f" -> writeFile (inFile <.> "st" <.> "txt") (unpack $ pShowNoColor symTable)
        _ -> return ()
    case astDump of
        "c" -> putStrLn "Dumping AST..." >> putStrLn "**************" >> pPrint ast
        "f" -> writeFile (inFile <.> "ast" <.> "txt") (unpack $ pShowNoColor ast)
        _ -> return ()

main :: IO ()
main = do
    args <- getArgs
    opts <- compilerOpts args
    let
        inFile = optInputFile opts
        inclDirs = optInclDirs opts
        outFile = optOutputFile opts
        depFile = optDepFile opts
        target = optTarget opts
    parsedAst <- parseFile inFile
    symTable <- buildSymTable parsedAst
    debugOutput opts symTable parsedAst
    out <- compile target symTable parsedAst
    output outFile out
