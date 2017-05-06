{-
  SkateBackendCode: The C code backend for Skate

  Part of Skate: a Schema specification languge

  Copyright (c) 2017, ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}

module SkateBackendCode where

import Data.Char
import Data.List
import Data.Char

import Text.ParserCombinators.Parsec.Pos

import qualified CAbsSyntax as C
import SkateParser
import SkateSchema
import SkateTypes
import SkateBackendCommon



compile :: String -> String -> SchemaRecord -> String
compile infile outfile s = unlines $ C.pp_unit $  (skate_c_body s infile)

skate_c_body :: SchemaRecord -> String -> C.Unit
skate_c_body sr infile =
    let
        Schema n d decls imps sp = (schema sr)
    in
    C.UnitList [
        (skate_c_preamble n d infile),
        C.Blank, C.Blank,
        skate_c_includes n
    ]

skate_c_includes :: String -> C.Unit
skate_c_includes sr = C.Include C.Standard (skate_c_includepath sr)
