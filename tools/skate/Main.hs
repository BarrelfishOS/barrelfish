{-
  Skate: a strawman device definition DSL for Barrelfish

  Copyright (c) 2017 ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
  Attn: Systems Group.
-}



module Main where

import System.Environment
import System.Exit
import System.Console.GetOpt
import System.IO
import System.IO.Error
import System.FilePath.Posix
import Data.Maybe
import Data.List
import Control.Monad
import Control.Exception
import Text.Printf

import Text.ParserCombinators.Parsec as Parsec

import qualified SkateParser
import qualified SkateBackendCode
import qualified SkateBackendHeader
import qualified SkateBackendLatex
import qualified SkateBackendWiki
import qualified SkateSchema
import qualified SkateChecker




{- Compilation Targets -}
data Target = Header | Code | Wiki | Latex deriving (Eq, Show)

{- Architecture to build for -}
data Arch = X86_64 | ARMv7 | ARMv8 deriving (Eq, Show)

{- Possible Options -}
data Options = Options {
  opt_infilename  :: Maybe String,
  opt_outfilename :: Maybe String,
  opt_includes    :: [String],
  opt_targets     :: Target,
  opt_usage_error :: Bool,
  opt_verbosity   :: Integer,
  opt_arch        :: Arch
  } deriving (Show,Eq)

{- The default options for Skate-}
defaultOptions :: Options
defaultOptions    = Options {
  opt_infilename  = Nothing,
  opt_outfilename = Nothing,
  opt_includes    = [],
  opt_targets     = Header,
  opt_usage_error = False,
  opt_verbosity   = 0,
  opt_arch        = X86_64 }


{- Adds a new target to the list of targets -}
optSetTarget :: Target -> Options -> Options
optSetTarget t o =  o { opt_targets = t }

{- Adds a new include to the list of includes -}
optAddInclude :: String -> Options -> Options
optAddInclude s o = o { opt_includes = (opt_includes o) ++ [s] }

{- Adds a new include to the list of includes -}
optSetVerbosity :: Integer -> Options -> Options
optSetVerbosity i o = o { opt_verbosity = i }

{- Adds a new target to the list of targets -}
optSetOutFile :: String -> Options -> Options
optSetOutFile s o = o { opt_outfilename = Just s }

{- Adds a new target to the list of targets -}
optSetInFile :: String -> Options -> Options
optSetInFile s o = o { opt_infilename = Just s }

optSetArch :: String -> Options -> Options
optSetArch "x86_64" o =  o { opt_arch = X86_64 }
optSetArch "armv7" o =  o { opt_arch = ARMv7 }
optSetArch "armv8" o =  o { opt_arch = ARMv8 }



{- Set the option parser Systems.GetOpt -}
options :: [OptDescr (Options -> Options)]
options = [ --Option ['c'] ["input-file"]
    --(ReqArg (\f opts -> opts { opt_infilename = Just f } ) "file")
    --"input file",
    Option ['I']
           ["import"]
           (ReqArg (\ f opts -> optAddInclude f opts) "file.sks" )
           "Include a given file before processing",

    Option ['v']
           ["verbose"]
           (NoArg (\ opts -> optSetVerbosity 1 opts ))
           "increase verbosity level",

    Option ['o']
           ["output"]
           (ReqArg (\ f opts -> optSetOutFile f opts ) "file.out")
           "output file name",

    Option ['H']
           ["header"]
           (NoArg (\ opts -> optSetTarget Header opts))
           "Create a header file",

    Option ['C']
           ["code"]
           (NoArg (\ opts -> optSetTarget Code opts))
           "Create code",

    Option ['L']
           ["latex"]
           (NoArg (\ opts -> optSetTarget Latex opts))
           "add documentation target",

    Option ['W']
           ["wiki"]
           (NoArg (\ opts -> optSetTarget Wiki opts))
           "add documentation target",

    Option ['a']
           ["arch"]
           (ReqArg (\ a opts -> optSetArch a opts) "x86_64")
           "add architecture. one of x86_64, armv7, armv8"
  ]


{- prints an error message if wrong options are supplied -}
usageError :: [String] -> IO (Options)
usageError errs =
  ioError (userError (concat errs ++ usageInfo usage options))
  where usage = "Usage: Skate <options> <input file>"


{- evaluates the compiler options -}
compilerOpts :: [String] -> IO (Options)
compilerOpts argv =
  case getOpt Permute options argv of
    (o,[n],[]) -> return ( optSetInFile n (foldl (flip id) defaultOptions o) )
    (_,_,errs) -> usageError errs


getGenerator :: Options -> Target -> String -> String -> SkateSchema.SchemaRecord -> String
getGenerator _ Header = SkateBackendHeader.compile
getGenerator _ Code = SkateBackendCode.compile
getGenerator _ Latex = SkateBackendLatex.compile
getGenerator _ Wiki = SkateBackendWiki.compile



{- compile the backend codes -}
compile :: Options -> Target -> SkateSchema.SchemaRecord -> String -> String
           -> Handle -> IO ()
compile opts fl ast infile outfile outfiled =
    hPutStr outfiled $ (getGenerator opts fl) infile outfile ast
 -- where
 --     ast' = SkateTools.rewireTypes ast (SkateTools.collectTypes ast)



{- parses the file -}
parseFile :: String -> IO SkateParser.Schema
parseFile fname = do
    src <- readFile fname
    case (runParser SkateParser.parse () fname src) of
        Left err -> ioError $ userError ("Parse error at: " ++ (show err))
        Right x -> return x





{- Resolve the imports in the files -}
findImport :: [String] -> String -> IO SkateParser.Schema
findImport [] f =  ioError (userError $ printf "Can't find import '%s'" f)
findImport (d:t) f =  do
    catch (parseFile (d </> f))
      (\e -> (if isDoesNotExistError e then findImport t f else ioError e))

resolveImp :: [SkateParser.Schema] -> [String] -> IO [SkateParser.Schema]
resolveImp dfl path =
    let
        allimports = nub $ concat [ i | (SkateParser.Schema n _ _ i _) <- dfl ]
        gotimports = [ n | (SkateParser.Schema n _ _ i _) <- dfl ]
        required = allimports \\ gotimports
    in
        case required of
            [] -> return dfl
            (t:_) -> do {
                i <- (findImport path (t ++ ".sks"));
                resolveImp (dfl ++ [i]) path }



{- The Main Entry Point of Skate-}
main :: IO ()
main = do {
    cli <- System.Environment.getArgs;
    opts <- compilerOpts cli;
    let
        inFile = fromJust $ opt_infilename opts
        outFile = fromJust $ opt_outfilename opts
        target = opt_targets opts
        dfl  = []
    in
        do {
            printf "Start parsing '%s'\n" inFile;
            ast  <- parseFile inFile;
            dfl  <- resolveImp [ast] (opt_includes opts);
            st <- SkateSchema.make_schema_record ast (tail dfl);
            printf "output parsing '%s'\n" outFile;
            _ <- SkateChecker.run_all_checks inFile st;
            outFileD <- openFile outFile WriteMode;
            compile opts target st inFile outFile outFileD;
            hClose outFileD
        }
    }
