{- 
   Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007-2011, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}

module Main where
 
import System
import System.Exit
import System.IO
import System.Console.GetOpt
import System.FilePath
import Data.Maybe
import Text.ParserCombinators.Parsec as Parsec

import qualified MackerelParser
import qualified BitFieldDriver
import qualified ShiftDriver
import Checks
import Dev

--
-- Command line options and parsing code
--

-- Datatypes for carrying command options around
data Target = BitFieldDriver | ShiftDriver deriving (Eq, Show)

data Options = Options {
  opt_infilename :: Maybe String,
  opt_outfilename :: Maybe String,
  opt_includedirs :: [String],
  opt_target :: Target,
  opt_usage_error :: Bool,
  opt_verbosity :: Integer
  } deriving (Show,Eq)

defaultOptions = Options { 
  opt_infilename = Nothing, 
  opt_outfilename = Nothing,
  opt_includedirs = [],
  opt_target = ShiftDriver,
  opt_usage_error = False,
  opt_verbosity = 1 }

-- For driving System.GetOpt
options :: [OptDescr (Options -> Options)]
options =
  [ Option ['c'] ["input-file"] 
    (ReqArg (\f opts -> opts { opt_infilename = Just f } ) "file")
    "input file"
  , Option ['I'] ["include-dir"]
    (ReqArg (\ d opts -> opts { opt_includedirs = opt_includedirs opts ++ [d] }) "dir")
    "include directory (can be given multiple times)"
  , Option ['v'] ["verbose"]
    (NoArg (\ opts -> opts { opt_verbosity = opt_verbosity opts + 1 } ))
    "increase verbosity level"
  , Option ['o'] ["output"]
    (ReqArg (\ f opts -> opts { opt_outfilename = Just f }) "file")
     "output file name"
  , Option ['S'] ["shift-driver"]
    (NoArg (\ opts -> opts { opt_target = ShiftDriver } ))
     "use shift driver (default; preferred)"
  , Option ['B'] ["bitfield-driver"]
    (NoArg (\ opts -> opts { opt_target = BitFieldDriver } ))
     "use bitfield driver (deprecrated: do not use)"
  ]

--
-- Set the correct default input and output files
--

defaultOutput :: Options -> Options
defaultOutput opts = 
  if isJust $ opt_outfilename opts
  then opts
  else opts { opt_outfilename = 
                 Just $ replaceExtension "dev.h" $ takeFileName $ fromJust $ opt_infilename opts }

defaultInput :: Options -> [String] -> IO (Options)
defaultInput opts [f] = 
  if isNothing $ opt_infilename opts
  then return (defaultOutput (opts { opt_infilename = Just f }))
  else usageError []
defaultInput opts [] = 
  if (isJust $ opt_infilename opts)
  then return (defaultOutput opts)
  else usageError []
defaultInput opts _ = usageError []

compilerOpts :: [String] -> IO (Options)
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[])   -> defaultInput (foldl (flip id) defaultOptions o) n
    (_,_,errs) -> usageError errs

usageError :: [String] -> IO (Options)
usageError errs = 
  ioError (userError (concat errs ++ usageInfo usage options))
  where usage = "Usage: mackerel <options> <input file>" 

--
-- Processing source files
---

-- Parsing the input file into an AST
parseFile :: (String -> IO (Either Parsec.ParseError a)) -> String -> IO a
parseFile parsefn fname = do
   input <- parsefn fname
   case input of
       Left err -> do
           hPutStrLn stderr $ "Parse error at: " ++ (show err)
           exitWith $ ExitFailure 1
       Right x -> return x

-- Perform run-time checks
run_checks input_fn dev =
    case (Checks.check_all input_fn dev) of
      Just errors ->
          do { (hPutStrLn stderr (unlines [ e ++ "\n"  | e <-errors]))
             ; exitWith (ExitFailure 1)
             }
      Nothing ->do { return "" }

-- Main entry point of Mackernel 
main :: IO ()
main = do 
       cli <- System.getArgs
       opts <- compilerOpts cli
       ast  <- parseFile MackerelParser.parse $ fromJust $ opt_infilename opts
       let 
         dev = Dev.make_dev ast
         input_fn = fromJust $ opt_infilename opts
         output_fn = fromJust $ opt_outfilename opts
         in do
         run_checks input_fn dev
         outFileD <- openFile output_fn WriteMode
         hPutStr outFileD ((case (opt_target opts) of
                               ShiftDriver -> ShiftDriver.compile 
                               BitFieldDriver -> BitFieldDriver.compile)
                           input_fn output_fn dev)
         hClose outFileD
