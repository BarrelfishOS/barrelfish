{-
  SkateBackendHeader: Backend for generating C header files

  Part of Skate: a Schema specification languge

  Copyright (c) 2017, ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}

module SkateBackendHeader where

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
compile infile outfile s = unlines $ C.pp_unit $ skate_header_file s infile

skate_header_file :: SchemaRecord -> String -> C.Unit
skate_header_file sr infile =
    let
        Schema n d decls imps sp = (schema sr)
        sym = "SKATE__" ++ (map toUpper n) ++ "_SCHEMA_H_"
    in
      C.IfNDef sym ([ C.Define sym [] "1"] ++ (skate_header_body sr infile)) []

{-----------------------------------------------------------------------------
- The Header File
------------------------------------------------------------------------------}

skate_header_body :: SchemaRecord -> String -> [ C.Unit ]
skate_header_body sr infile =
    let
        Schema n d decls imps sp = (schema sr)
    in
    [C.Blank, C.Blank] ++
    [(skate_c_preamble n d infile)] ++
    [C.Blank, C.Blank] ++
    skate_c_stdincludes
    ++
    (skate_c_headerfiles imps) ++
    [C.Blank, C.Blank] ++
    [C.MultiComment [
        "====================================================================",
        "Flags",
        "===================================================================="
    ], C.Blank] ++
    (skate_c_header_decls (flags sr) (types sr)) ++
    [C.Blank, C.Blank] ++
    [C.MultiComment [
        "====================================================================",
        "Constants",
        "===================================================================="
    ], C.Blank] ++
    (skate_c_header_decls (constants sr) (types sr)) ++
    [C.Blank, C.Blank] ++
    [C.MultiComment [
        "====================================================================",
        "Enumerations",
        "===================================================================="
    ], C.Blank] ++
    (skate_c_header_decls (enumerations sr) (types sr)) ++
    [C.Blank, C.Blank] ++
    [C.MultiComment [
        "====================================================================",
        "Facts",
        "===================================================================="
    ], C.Blank] ++
    (skate_c_header_decls (facts sr) (types sr)) ++
    [C.Blank, C.Blank]


{-----------------------------------------------------------------------------
- Premable and Imports
------------------------------------------------------------------------------}

skate_c_stdincludes :: [ C.Unit ]
skate_c_stdincludes = [
    C.Include C.Standard "stdint.h",
    C.Include C.Standard "errno.h" ]

-- Header files info
skate_c_headerfiles :: [String] -> [ C.Unit ]
skate_c_headerfiles [] = [C.MultiComment ["No Imports"]]
skate_c_headerfiles imps = [C.MultiComment ["Imports"]] ++
    map (C.Include C.Standard) [ skate_c_includepath i | i <- imps ]

{-----------------------------------------------------------------------------
- Facts
------------------------------------------------------------------------------}

skate_c_header_one_attrib :: String -> FactAttrib -> [C.Param]
skate_c_header_one_attrib p e@(FactAttrib i d t sp) = [
  C.ParamDoxyComment d,
  C.Param (typeref_to_ctype t) i]


skate_c_header_resolve_types :: TypeRef -> [TT.TTEntry] -> TypeRef
skate_c_header_resolve_types tr@(TEnum i _) ttbl = TBuiltIn (TT.get_builtin_type i ttbl)
skate_c_header_resolve_types tr@(TConstant i _) ttbl = TBuiltIn (TT.get_builtin_type i ttbl)
skate_c_header_resolve_types tr@(TFlags i _) ttbl = TBuiltIn (TT.get_builtin_type i ttbl)
skate_c_header_resolve_types tr _ = tr

skate_c_header_extract_field :: String -> TypeRef -> String
skate_c_header_extract_field s tr@(TFact i _) = fext ++ (("(&((_f)->" ++ s ++ "))"))
    where
        fext = make_format_name_extract_all (identifier_to_cname i)
skate_c_header_extract_field s _ = ("(_f)->" ++ s)


skate_c_header_fact :: String -> String -> [ FactAttrib ] -> SourcePos -> [TT.TTEntry] -> [C.Unit]
skate_c_header_fact i d attrib sp ttbl = [
    (skate_c_type_comment "fact" d i sp),
    C.StructDecl ttype $ concat (intersperse [C.ParamBlank] [skate_c_header_one_attrib i a | a <- attrib]),
    C.TypeDef (C.Struct ttype) ttype,
    C.Blank] ++ skate_c_fn_decls_facts i attrib
    ++ [C.Blank, C.Blank, (skate_c_prolog_strings i types), C.Blank,

    C.DoxyComment ("Extract fields from a struct"),
    C.Define (make_format_name_extract_all tname) ["_f"] (concat (intersperse ", " extractstr)),
    C.Blank,C.Blank

    ]
    where
        tname = (identifier_to_cname i)
        ttype = (make_type_name tname)
        types = [skate_c_header_resolve_types t ttbl | e@(FactAttrib i d t sp) <- attrib ]
        extractstr = [ (skate_c_header_extract_field i t) | e@(FactAttrib i d t sp) <- attrib ]


{-----------------------------------------------------------------------------
- Flags
------------------------------------------------------------------------------}


skate_c_header_one_flag :: String -> FlagDef -> C.TypeSpec -> C.Unit
skate_c_header_one_flag p f@(FlagDef i d v _) t = C.UnitList [
    C.DoxyComment d,
    C.Define (flagdef) [] (C.pp_expr $ C.Cast t $
            C.Binary C.LeftShift (C.NumConstant 1) (C.NumConstant v)) ]
        where
            flag = make_qualified_identifer p i
            flagdef = map toUpper (identifier_to_cname flag)


skate_c_header_flags :: String -> String -> Integer ->[ FlagDef ] -> SourcePos -> [C.Unit]
skate_c_header_flags i d w defs sp = [
    (skate_c_type_comment "flags" d i sp),
    C.TypeDef (C.TypeName ttype) tname,
    C.Blank]
    ++ [skate_c_header_one_flag i def (C.TypeName tname) | def <- defs]
    ++ [C.Blank]
    where
        ttype = "uint" ++ show(w) ++ "_t"
        tname = (make_type_name (identifier_to_cname i))


{-----------------------------------------------------------------------------
- Constants
------------------------------------------------------------------------------}


skate_c_header_one_const :: String -> ConstantDef -> C.TypeSpec -> C.Unit
skate_c_header_one_const p f@(ConstantDefInt i d v _) t = C.UnitList [
    C.DoxyComment d,
    C.Define (constdef) [] (C.pp_expr $ C.Cast t $ C.NumConstant v) ]
    where
        c = make_qualified_identifer p i
        constdef = map toUpper (identifier_to_cname c)
skate_c_header_one_const p f@(ConstantDefStr i d v _) t = C.UnitList [
    C.DoxyComment d,
    C.Define (constdef) [] (C.pp_expr $ C.Cast t $ C.StringConstant v) ]
        where
            c = make_qualified_identifer p i
            constdef = map toUpper (identifier_to_cname c)

skate_c_header_const :: String -> String -> TypeRef ->[ ConstantDef ] -> SourcePos -> [C.Unit]
skate_c_header_const i d t@(TBuiltIn tref) defs sp = [
    (skate_c_type_comment "constants" d i sp),
    C.TypeDef (typeref_to_ctype t) tname,
    C.Blank]
    ++ [skate_c_header_one_const i def (C.TypeName tname) | def <- defs]
    ++ [C.Blank]
    where
        tname = (make_type_name (identifier_to_cname i))


{-----------------------------------------------------------------------------
- Enumerations
------------------------------------------------------------------------------}


skate_c_header_one_enum :: String -> EnumDef -> C.EnumItem
skate_c_header_one_enum p e@(EnumDef i d _) = C.EnumItem name d Nothing
    where
        enum = make_qualified_identifer p i
        name = map toUpper (identifier_to_cname enum)


skate_c_header_enum :: String -> String -> [ EnumDef ] -> SourcePos -> [C.Unit]
skate_c_header_enum i d defs sp = [
    (skate_c_type_comment "enumeration" d i sp),
    C.EnumDecl ttype [skate_c_header_one_enum i def | def <- defs],
    C.Blank]
    where
        tname = (identifier_to_cname i)
        ttype = (make_type_name tname)



{-----------------------------------------------------------------------------
- Generic Declarations
------------------------------------------------------------------------------}

skate_c_header_one_decl :: Declaration -> [TT.TTEntry] -> [ C.Unit ]
skate_c_header_one_decl de@(Fact i d a sp) tt = skate_c_header_fact i d a sp tt
skate_c_header_one_decl de@(Flags i d w f sp) _ = skate_c_header_flags i d w f sp
skate_c_header_one_decl de@(Constants i d t f sp) _ = skate_c_header_const i d t f sp
skate_c_header_one_decl de@(Enumeration i d f sp) _ = skate_c_header_enum i d f sp
skate_c_header_one_decl _  _ = []


skate_c_header_decls :: [Declaration] -> [TT.TTEntry] -> [ C.Unit ]
skate_c_header_decls decls ttbl = [C.UnitList $ skate_c_header_one_decl d ttbl | d <- decls]


skate_c_prolog_strings :: String -> [TypeRef] -> C.Unit
skate_c_prolog_strings i t = C.UnitList [
    C.DoxyComment ("Prolog fields format string for " ++ i),
    C.Define (make_format_name_fields_pr cname) []  (wr_fmt),
    C.Blank,
    C.DoxyComment ("Prolog format string for " ++ i),
    C.Define (make_format_name_pr cname) []  (prolog ++  (make_format_name_fields_pr cname) ++ " \").\""),
    C.Blank,

    C.DoxyComment ("Prolog fields format string for " ++ i),
    C.Define (make_format_name_fields_rd cname) [] (rd_fmt),
    C.Blank,
    C.DoxyComment ("Prolog format string for " ++ i),
    C.Define (make_format_name_rd cname) [] (prolog ++  (make_format_name_fields_rd cname) ++ " \").\""),
    C.Blank]
    where
        cname = (identifier_to_cname i)
        wr_fmt = "\"%\" " ++ concat (intersperse " \", %\" " [fmt_wr a | a <- t])
        rd_fmt = "\"%\" " ++ concat (intersperse " \", %\" " [fmt_rd a | a <- t])
        prolog = "\"" ++ (identifier_to_prolog i) ++ "(\" "

fmt_wr :: TypeRef -> String
fmt_wr (TFact t _ ) = (make_format_name_fields_pr (identifier_to_cname t))
fmt_wr (TBuiltIn t) = builtin_fmt_wr t

fmt_rd :: TypeRef -> String
fmt_rd (TFact t _ ) = (make_format_name_fields_rd (identifier_to_cname t))
fmt_rd (TBuiltIn t) = builtin_fmt_rd t
