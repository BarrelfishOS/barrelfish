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

import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import SockeyeAST
import SockeyeParser

{- Possible options for the Sockeye Compiler -}
data Options = Options { optInputFile :: String }

{- Default options -}
defaultOptions :: Options
defaultOptions = Options { optInputFile = ""}

{- Prints usage information possibly with usage errors -}
usage :: [String] -> IO ()
usage errors = do
    prg <- getProgName
    let usageString = "Usage: " ++ prg ++ " <options> <input file>"
        in hPutStrLn stderr (usageInfo (concat errors ++ usageString) options)

{- Setup option parser -}
options :: [OptDescr (Options -> IO Options)]
options = 
    [ Option "h" ["help"]
        (NoArg (\_ -> do
                    usage []
                    exitWith ExitSuccess))
        "Show help"
    ]

{- evaluates the compiler options -}
compilerOpts :: [String] -> IO (Options)
compilerOpts argv =
    case getOpt Permute options argv of
        (actions, [n], []) -> (foldl (>>=) (return defaultOptions) actions) >>= \o -> return (o { optInputFile = n })
        (actions, [], [])  -> usage ["Input file not specified\n"] >> exitWith (ExitFailure 1)
        (_, _, errors)     -> usage errors >> exitWith (ExitFailure 1)

main = do
    args <- getArgs
    opts <- compilerOpts args
    let inFile = optInputFile opts
    src <- readFile inFile
    case parseSockeye inFile src of
        Left err -> hPutStrLn stderr ("Parse error at " ++ show err) >> exitWith (ExitFailure 2)
        Right ast -> print ast
