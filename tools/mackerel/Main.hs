{- 
   Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007, 2008, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}
  

{-
  // Temp arrangement to check syntax of outb(...), a complete implementation will 
  consist of generating unions and then writing sequentially to ports 
  // FIX: Check Endianness  

-}


module Main where

 
import System
import System.Environment
import System.Exit
import System.IO
import System.Console.GetOpt
import Data.Bits
import Data.Word
import Char 
import Monad
import Numeric
import Data.List
import Text.Printf
import Text.ParserCombinators.Parsec as Parsec

import qualified MackerelParser
import qualified BitFieldDriver
import qualified ShiftDriver
import Checks
import Dev

import qualified TypeTable as TT
import qualified RegisterTable as RT
import qualified ConstTable as CT

--
-- Internal options: Not really used yet...
--
data Options = Options {
      optArch :: Maybe String,
      optTargets :: [ Target ]
}
defaultOptions = Options { optArch = Nothing, optTargets = [] }

data Target = BitFieldDriver | ShiftDriver deriving (Show)

generator :: Options -> Target -> (String -> String -> Dev.Rec -> String)
generator _ BitFieldDriver = BitFieldDriver.compile
generator _ ShiftDriver = ShiftDriver.compile


addTarget :: Target -> Options -> IO Options
addTarget t o = return o { optTargets = (optTargets o) ++ [t] }

options :: [OptDescr (Options -> IO Options)]
options = [ Option ['b'] ["bitfield-driver"] (NoArg $ addTarget BitFieldDriver) "Generate old-style bitfield-based header file (DEPRECATED)",
            Option ['s'] ["shift-driver"] (NoArg $ addTarget ShiftDriver) "Generate driver header file using shifts and masks" ]


-- main :: IO() 
-- main = do { args <- System.getArgs
--           ; result <-  parse (head args)  
--           ; let header = if(length args == 2)then 
--                              (last args) 
--                          else ""
--             in 
--               case result of 
--                 Left err -> 
--                     do{ hPutStrLn stderr "parse error at: "
--                       ; hPutStrLn stderr (show err) 
--                       ; exitWith (ExitFailure 1)
--                       }
--                 Right ast -> 
--                     let
--                         Device name bitorder args desc decls = ast
--                         dev =  Dev.make_dev ast
--                     in
--                       do { run_checks dev
--                          ; BitFieldDriver.render dev header
--                          ; exitWith ExitSuccess
--                          }
--           }

compile :: Options -> Target -> Dev.Rec -> String -> String -> Handle -> IO () 
compile opts fl ast infile outfile outfiled =
    hPutStr outfiled ( (generator opts fl) infile outfile ast )

parseFile :: (String -> IO (Either Parsec.ParseError a)) -> String -> IO a
parseFile parsefn fname = do
   input <- parsefn fname
   case input of
       Left err -> do
           hPutStrLn stderr $ "Parse error at: " ++ (show err)
           exitWith $ ExitFailure 1
       Right x -> return x

makeDevice :: MackerelParser.AST -> IO Dev.Rec
makeDevice ast = do return (Dev.make_dev ast)

-- Main entry point of Mackernel 
main :: IO ()
main = do 
       argv <- System.getArgs
       case getOpt RequireOrder options argv of
         (optf, [ inFile, outFile ], []) -> do
             opts <- foldM (flip id) defaultOptions optf
             ast <- parseFile MackerelParser.parse inFile
             dev <- makeDevice ast
             run_checks dev
             outFileD <- openFile outFile WriteMode
             sequence_ $ map (\target ->
                              compile opts target dev inFile outFile outFileD
                 ) (optTargets opts)
             hClose outFileD
         (_, _, errors) -> do
             hPutStr stderr (concat errors ++ usageInfo usage options)
             exitWith (ExitFailure 1)
      where
          usage = "Usage: mackerel [OPTION...] input.dev output.h"



-- Perform run-time checks
run_checks dev =
    case (Checks.check_all dev) of
      Just errors ->
          do { (hPutStrLn stderr (unlines [ e ++ "\n"  | e <-errors]))
             ; exitWith (ExitFailure 1)
             }
      Nothing ->do { return "" }

