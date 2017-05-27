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
import qualified SkateTypeTable as TT



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
        skate_c_includes n,
        C.Blank, C.Blank,
        C.MultiComment [
            "====================================================================",
            "Flags",
            "===================================================================="
        ], C.Blank,
        C.UnitList $ (skate_c_code_defs (flags sr) (types sr)),
        C.Blank, C.Blank,
        C.MultiComment [
            "====================================================================",
            "Constants",
            "===================================================================="
        ], C.Blank,
        C.UnitList $ (skate_c_code_defs (constants sr) (types sr)),
        C.Blank, C.Blank,
        C.MultiComment [
            "====================================================================",
            "Enumerations",
            "===================================================================="
        ], C.Blank,
        C.UnitList $ (skate_c_code_defs (enumerations sr) (types sr)),
        C.Blank, C.Blank,
        C.MultiComment [
            "====================================================================",
            "Facts",
            "===================================================================="
        ], C.Blank,
        C.UnitList $ (skate_c_code_defs (facts sr) (types sr)),
        C.Blank, C.Blank]


skate_c_includes :: String -> C.Unit
skate_c_includes sr = C.UnitList [
    C.Include C.Standard "barrelfish/barrelfish.h",
    C.Include C.Standard "skb/skb.h",
    C.Include C.Standard (skate_c_includepath sr)
    ]


{------------------------------------------------------------------------------
Function Add
------------------------------------------------------------------------------}

skate_c_vardecl :: C.TypeSpec -> String -> Maybe C.Expr -> C.Stmt
skate_c_vardecl t s e = C.VarDecl C.NoScope C.NonConst  t s e

skate_c_vardecl_err :: C.Stmt
skate_c_vardecl_err = skate_c_vardecl (C.TypeName "errval_t") "err" Nothing

skate_c_errvar :: C.Expr
skate_c_errvar = C.Variable "err"

skate_c_code_add :: Declaration -> [TT.TTEntry] -> [C.Unit]
skate_c_code_add def@(Fact i d attrib sp) ttbl =
    skate_c_fn_defs_facts i attrib [
        skate_c_vardecl_err,
        C.SBlank,
        C.SComment "TODO: Add some checks.",
        C.SBlank,
        C.Ex $ C.Assignment skate_c_errvar (C.Call "skb_add_fact" [
            C.DefineExpr fmt,
            C.Call (make_format_name_extract_all cname) [C.Variable "fact"]
            ]
            ),
        C.If (C.Call "err_is_fail" [skate_c_errvar]) [
            C.SComment "TODO: Add some good error message",
            C.Return skate_c_errvar]
            [],
        C.SBlank,
        C.Return skate_c_errvar
        ]
    where
        cname = identifier_to_cname i
        fmt = make_format_name_fields_pr cname



{-----------------------------------------------------------------------------
- Facts
------------------------------------------------------------------------------}

skate_c_code_fact :: Declaration -> [TT.TTEntry] -> [C.Unit]
skate_c_code_fact def@(Fact i d attrib sp) ttbl = [
    (skate_c_type_comment "fact" d i sp),
    C.Blank] ++
    (skate_c_code_add def ttbl)


skate_c_code_one_def :: Declaration -> [TT.TTEntry] -> [ C.Unit ]
skate_c_code_one_def de@(Fact i d a sp) tt = skate_c_code_fact de tt
skate_c_code_one_def de@(Flags i d w f sp) _ = [] --skate_c_header_flags i d w f sp
skate_c_code_one_def de@(Constants i d t f sp) _ = [] --skate_c_header_const i d t f sp
skate_c_code_one_def de@(Enumeration i d f sp) _ = [] --skate_c_header_enum i d f sp
skate_c_code_one_def _  _ = []


skate_c_code_defs :: [Declaration] -> [TT.TTEntry] -> [ C.Unit ]
skate_c_code_defs decls ttbl = [C.UnitList $ skate_c_code_one_def d ttbl | d <- decls]
