%include polycode.fmt

%if false
  Flounder2: an even more simpler IDL for Barrelfish
   
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif



> module Compiler where

> import System
> import System.Environment
> import System.Exit
> import System.Console.GetOpt
> import System.IO

> import Syntax
> import qualified CodeBackend
> import qualified HeaderBackend


> compile :: Interface -> IO ()
> compile interface = do 
>                     let Interface filename _ _ = interface
>                     fileHeader <- openFile (filename ++ "_types.h") WriteMode 
>                     fileC <- openFile (filename ++ "_stub.c") WriteMode
>                     hPutStr fileC $ CodeBackend.compile interface
>                     hPutStr fileHeader $ HeaderBackend.compile interface
>                     hClose fileC
>                     hClose fileHeader
