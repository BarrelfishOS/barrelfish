{- 
  SkateChecker: Checker for the AST
   
  Part of Skate: a Schema specification languge
   
  Copyright (c) 2017, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-} 

module SkateChecker where

import System.FilePath.Posix
import Text.Printf

import SkateParser
import SkateSchema


{- verifies that the filename matches with the query definition -}
checkFilename :: SkateParser.Schema -> String -> IO ()
checkFilename schema fname = do
    let 
        SkateParser.Schema sname _ _ _ = schema
    if sname == takeBaseName fname 
    then return () 
    else ioError $ userError (
        "Schema name '" ++ sname ++ "' has to equal filename in " ++ fname)



{- run various checks -}
run_all_checks :: String -> SchemaRecord -> IO String
run_all_checks inFile schemaRecord = do {
    printf "Running tests on  '%s'\n" inFile;
    checkFilename (skateSchemaGetAst schemaRecord) inFile;
    return ""
}

 --   case (Checks.check_all inFile schema) of
 --     Just errors ->
 --         do { (hPutStrLn stderr (unlines [ e ++ "\n"  | e <-errors]))
 --            ; System.Exit.exitWith (ExitFailure 1)
 --            }
 --     Nothing -> do { return "" }