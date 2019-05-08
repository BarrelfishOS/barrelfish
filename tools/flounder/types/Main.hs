
{-
  Hake: a meta build system for Barrelfish

  Copyright (c) 2009, 2015, ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universitaetstasse 6, CH-8092 Zurich. Attn: Systems Group.
-}


import Data.Dynamic
import Data.List
import Data.Maybe
import Data.Char

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Debug.Trace

-- Generate enume for flounder endpoint types. do it here as
-- Hake knows all the files

makeFlounderTypes :: String -> String -> String -> IO()
makeFlounderTypes src build arch = do
    let fileName = build ++ "/" ++ arch ++ "/include/if/if_types.h"
    let dirName = build ++ "/" ++ arch ++ "/include/if"

    putStrLn src
    putStrLn build
    putStrLn arch
    putStrLn fileName
    putStrLn dirName

    createDirectoryIfMissing True dirName
    writeFile fileName ""

    h <- openFile(fileName) WriteMode

    baseDir <- getDirectoryContents (src </> "if") >>= return. filter (\c -> not $ elem c [".", ".."])
    archDir <- getDirectoryContents (src </> "if/arch") >>= return. filter (\c -> not $ elem c [".", ".."])
    hPutStrLn h "#ifndef IF_TYPES_H"
    hPutStrLn h "#define IF_TYPES_H"
    hPutStrLn h ""
    hPutStrLn h "// all the endpoint types generate from files"
    hPutStrLn h "enum endpoint_types {"
    hPutStrLn h "\tIF_TYPE_DUMMY = 0,"
    mapM_ (\x -> hPutStrLn h $ "\tIF_TYPE_" ++ ((map toUpper (takeBaseName x)) ++ ",")) baseDir
    mapM_ (\x -> hPutStrLn h $ "\tIF_TYPE_" ++ ((map toUpper (takeBaseName x)) ++ ",")) archDir
    hPutStrLn h "\tIF_TYPE_MAX"
    hPutStrLn h "};"
    hPutStrLn h "#endif"

    hFlush h
    hClose h


main :: IO ()
main =
   do
    argv <- System.Environment.getArgs
    print argv
    case argv of
     [src_dir, build_dir, arch, out] -> do
           makeFlounderTypes src_dir build_dir arch
     _ -> do
           hPutStrLn stderr "Usage: floundertypes <src_dir> <build_dir> <arch>"
           exitWith (ExitFailure 1)
