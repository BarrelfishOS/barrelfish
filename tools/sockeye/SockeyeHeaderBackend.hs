{- 
   GHBackend: Flounder stub generator for generic header files
   
  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module SockeyeHeaderBackend where

import Data.List
import Data.Char

import qualified CAbsSyntax as C
import SockeyeSyntax


import qualified Backend
import BackendCommon


add_fn_name n = ifscope n "add" 


------------------------------------------------------------------------
-- Language mapping: Create the generic header file for the interface
------------------------------------------------------------------------

compile :: String -> String -> Schema -> String
compile infile outfile schema = 
    unlines $ C.pp_unit $ sockeye_header_file infile schema


header_file :: String -> Schema -> [C.Unit] -> C.Unit
header_file infile schema@(Schema name _ _) body = 
    let sym = "__SCHEMA_" ++ map toUpper name ++ "_H"
    in
      C.IfNDef sym ([ C.Define sym [] "1"] ++ body) []


sockeye_header_file :: String -> Schema -> C.Unit
sockeye_header_file infile intf = 
    header_file infile intf (schema_header_body infile intf)


schema_header_body :: String -> Schema -> [C.Unit]
schema_header_body infile schema@(Schema name descr decls) = 
    let
        (types, facts, queries) = Backend.partitionTypesFactsQueries decls
        --messages = rpcs_to_msgs messagedecls
    in
      [ schema_preamble infile name descr,
        C.Blank,

        C.Include C.Standard "skb/skb.h",
        C.Blank,

        C.MultiComment [ "Concrete type definitions" ],
        C.UnitList $ define_types name types,
        C.Blank,
        C.Blank,

        C.MultiComment [ "Fact attribute fields" ],
        C.Blank,
        C.UnitList [ fact_attributes name f | f <- facts ],
        C.Blank,

        C.MultiComment [ "Fact type signatures" ],
        C.Blank,
        C.UnitList [ fact_signature name f | f <- facts ],
        C.Blank,

        C.MultiComment [ "Query type signatures" ],
        C.Blank,
        C.UnitList [ query_signature name q | q <- queries ],
        C.Blank
      ]



--
-- Generate type definitions for each fact signature
--
   



fact_signature :: String -> FactDef -> C.Unit
fact_signature sname f = C.UnitList [ 
    C.MultiDoxy (["@brief  " ++ desc,
                  ""] ++ param_desc),
    C.FunctionDecl C.NoScope (C.TypeName "errval_t") name params,
    C.Blank
  ]
  where 
    name = fact_sig_type sname "add" f 
    desc = fact_desc f 
    params = [C.Param (C.Ptr $ C.Struct $ (fact_attrib_type sname f)) "arg"]
    param_desc = [ fact_param_desc a | a <- fact_args f ]  
    payload = case f of
        Fact _ _ args -> [ fact_argdecl sname a | a <- args ]



query_signature :: String -> QueryDef -> C.Unit
query_signature sname q = 
  C.FunctionDecl C.NoScope (C.TypeName "errval_t") name params 
  where 
    name = query_sig_type sname q
    params = concat payload
    payload = case q of
        Query _ _ args -> [ query_argdecl sname a | a <- args ]


fact_attributes :: String -> FactDef -> C.Unit
fact_attributes sname f = C.UnitList [ 
    C.MultiDoxy (["Fact: " ++ name, 
                  "@brief  " ++ desc]),
    C.StructDecl name params,
    C.Blank,
    C.DoxyComment ("typedef for the " ++ name ++ " attribute type"),
    C.TypeDef (C.Struct name) (name ++ "_t"),
    C.Blank,
    fact_fmt_str sname f,
    C.Blank
  ]
  where 
    name = fact_attrib_type sname f
    desc = fact_desc f 
    params = concat payload
    payload = case f of
        Fact _ _ args -> [ fact_attrib_decl sname a | a <- args ]


attr_fmt_type_wr :: String -> FactAttribute -> String
attr_fmt_type_wr sn (FAttrib t (Name n) d) = case t of
    Builtin builtin ->  "\"%\" " ++ builtin_fmt_wr builtin
    TypeVar name -> "\"typevar\""
    FactType name -> type_c_define sn name "FMT_WRITE"
    TypeAlias alias builtin -> "\"alias_type\""

attr_fmt_type_rd :: String -> FactAttribute -> String
attr_fmt_type_rd sn (FAttrib t (Name n) d) = case t of
    Builtin builtin ->  "\"%\" " ++ builtin_fmt_rd builtin
    TypeVar name -> "\"typevar\""
    FactType name -> type_c_define sn name "FMT_READ"
    TypeAlias alias builtin -> "\"alias_type\""

attr_access_rd :: String -> String -> FactAttribute -> String
attr_access_rd arg sn (FAttrib t (Name n) d) = case t of
    FactType name -> type_c_define sn name "FIELDS(&("++"(" ++ arg ++ ")->" ++ n ++"))"
    _ -> "(" ++ arg ++ ")->" ++ n

fact_fmt_str :: String -> FactDef -> C.Unit
fact_fmt_str sname f=  C.UnitList [
    C.DoxyComment ("define for printing the " ++ name ++ "fact"),
    (C.Define (type_c_define sname (fact_name f) "FMT_WRITE") [] params_wr),
    C.DoxyComment ("define for reading the " ++ name ++ "fact"),
    (C.Define (type_c_define sname (fact_name f) "FMT_READ") [] params_rd),
    C.DoxyComment ("define for accessing the  " ++ name ++ "fact attributes"),
    (C.Define (type_c_define sname (fact_name f) "FIELDS") [ "_arg" ] field_access),
    C.Blank
  ]
  where 
    name = fact_attrib_type sname f
    desc = fact_desc f 
    params_wr = "\"" ++ name ++ "(\"" ++ (intercalate "\", \"" write) ++ "\")\""
    write = case f of
        Fact _ _ args -> [ (attr_fmt_type_wr sname a) | a <- args ]
    params_rd = "\"" ++ name ++ "(\"" ++ (intercalate "\", \"" read) ++ "\")\""
    read = case f of
        Fact _ _ args -> [ (attr_fmt_type_rd sname a) | a <- args ]
    field_access = (intercalate ", " fields)
    fields = case f of
        Fact _ _ args -> [ attr_access_rd "_arg" sname a | a <- args ]  

{-

--
-- Generate a struct to hold the arguments of a message while it's being sent.
-- 
msg_argstruct :: String -> [TypeDef] -> MessageDef -> C.Unit
msg_argstruct ifname typedefs m@(RPC n args _) = 
    C.StructDecl (msg_argstruct_name ifname n) 
         (concat [ rpc_argdecl ifname a | a <- args ])
msg_argstruct ifname typedefs m@(Message _ n [] _) = C.NoOp
msg_argstruct ifname typedefs m@(Message _ n args _) =
    let tn = msg_argstruct_name ifname n
    in
      C.StructDecl tn (concat [ msg_argstructdecl ifname typedefs a
                               | a <- args ])

--
-- Generate a union of all the above
-- 
intf_union :: String -> [MessageDef] -> C.Unit
intf_union ifn msgs = 
    C.UnionDecl (binding_arg_union_type ifn)
         ([ C.Param (C.Struct $ msg_argstruct_name ifn n) n
            | m@(Message _ n a _) <- msgs, 0 /= length a ]
          ++
          [ C.Param (C.Struct $ msg_argstruct_name ifn n) n
            | m@(RPC n a _) <- msgs, 0 /= length a ]
         )

--
-- Generate a struct defn for a vtable for the interface
--
intf_vtbl :: String -> Direction -> [MessageDef] -> C.Unit
intf_vtbl n d ml = 
    C.StructDecl (intf_vtbl_type n d) [ intf_vtbl_param n m d | m <- ml ]

intf_vtbl_param :: String -> MessageDef -> Direction ->  C.Param
intf_vtbl_param ifn m d = C.Param (C.Ptr $ C.TypeName $ msg_sig_type ifn m d) (msg_name m)

--
-----------------------------------------------------------------
-- Code to generate concrete type definitions
-----------------------------------------------------------------

-}

define_types :: String -> [TypeDef] -> [C.Unit]
define_types schemaName types = 
    [ define_type schemaName t | t <- types ]

define_type :: String -> TypeDef -> C.Unit
define_type sname (TAliasT newType originType) =
    C.TypeDef (type_c_type sname $ Builtin originType) (type_c_name1 sname newType)

{-
This enumeration:
\begin{verbatim}
typedef enum {
    foo, bar, baz
} some_enum;
\end{verbatim}

Generates the following code:
\begin{verbatim}
enum ifname_some_enum_t {
    ifname_some_enum_t_foo = 1,
    ifname_some_enum_t_bar = 2,
    ifname_some_enum_t_baz = 3,
}
\end{verbatim}
-}


define_type sname (TEnum name elements) = 
    C.EnumDecl (type_c_name1 sname name) 
         [ C.EnumItem (type_c_enum sname e) Nothing | e <- elements ]


   

{-
A typedef'd alias:
\begin{verbatim}
typedef uint32 alias_type;
\end{verbatim}

Should compile to:
\begin{verbatim}
typedef uint32_t ifname_alias_type_t;
\end{verbatim}
-}

define_type sname (TAlias newType originType) = 
    C.TypeDef (type_c_type sname originType) (type_c_name1 sname newType)


