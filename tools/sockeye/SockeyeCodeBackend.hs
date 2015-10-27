{- 
   GCBackend: Flounder stub generator for generic code
   
  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module SockeyeCodeBackend where

import Data.Char



import Data.List
import Data.Char

import qualified CAbsSyntax as C
import SockeyeSyntax


import qualified Backend
import BackendCommon

include_header_name n = "facts/" ++ ifscope n "schema.h"

add_fn_name n = ifscope n "add" 


------------------------------------------------------------------------
-- Language mapping: Create the generic header file for the interface
------------------------------------------------------------------------

compile :: String -> String -> Schema -> String
compile infile outfile schema = 
    unlines $ C.pp_unit $ sockeye_code_file infile schema


sockeye_code_file :: String -> Schema -> C.Unit
sockeye_code_file infile (Schema name descr decls) = 
      let
        (types, facts, queries) = Backend.partitionTypesFactsQueries decls
        --messages = rpcs_to_msgs messagedecls
    in
      C.UnitList [
      schema_preamble infile name descr,
      C.Blank,

      C.Include C.Standard "barrelfish/barrelfish.h",
      C.Include C.Standard "flounder/flounder_support.h",
      C.Include C.Standard ("schema/" ++ name ++ ".h"),
      C.Blank,

      C.MultiComment [ "Fact type signatures" ],
      C.Blank,
      C.UnitList [ fact_fn name f | f <- facts ],
      C.Blank,

      C.MultiComment [ "Query type signatures" ],
      C.Blank,
      C.UnitList [ query_fn name q | q <- queries ],
      C.Blank
    ]
    
  

--
-- Generate type definitions for each fact signature
--
  
fact_fn :: String -> FactDef -> C.Unit
fact_fn sname f = C.UnitList [ 
    C.MultiDoxy (["@brief  " ++ desc,
                  ""] ++ param_desc),
    C.FunctionDef C.NoScope (C.TypeName "errval_t") name params [
      C.VarDecl C.NoScope C.NonConst (C.TypeName "errval_t") "err" Nothing,
      C.SBlank,
      C.Ex $ C.Assignment errvar (C.Call "skb_client_connect" []),
      C.If (C.Call "err_is_fail" [errvar]) 
        [C.Return $ errvar] [],
      C.SBlank,
      C.Ex $ C.Assignment errvar (C.Call "skb_add_fact" [C.Variable add_fact_string, (C.Call add_fact_args [C.Variable "arg"])]),
      C.Return $ errvar
    ],
    C.Blank
  ]
  where 
    name = fact_sig_type sname "add" f 
    desc = fact_desc f 
    params = [C.Param (C.Ptr $ C.Struct $ (fact_attrib_type sname f)) "arg"]
    param_desc = [ fact_param_desc a | a <- fact_args f ]  
    payload = case f of
        Fact _ _ args -> [ fact_argdecl sname a | a <- args ]
    add_fact_string = type_c_define sname (fact_name f) "FMT_READ"
    add_fact_args = type_c_define sname (fact_name f) "FIELDS"



query_fn :: String -> QueryDef -> C.Unit
query_fn sname q = 
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
    C.TypeDef (C.Struct name) (name ++ "_t"),
    C.Blank
  ]
  where 
    name = fact_attrib_type sname f
    desc = fact_desc f 
    params = concat payload
    payload = case f of
        Fact _ _ args -> [ fact_attrib_decl sname a | a <- args ]




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


{-
import GHBackend (flounder_backends, export_fn_name, bind_fn_name, accept_fn_name, connect_fn_name)
import BackendCommon




-- name of the bind continuation function
bind_cont_name :: String -> String
bind_cont_name ifn = ifscope ifn "bind_continuation_direct"

-- name of an alternative bind continuation function
bind_cont_name2 :: String -> String
bind_cont_name2 ifn = ifscope ifn "bind_contination_multihop"



compile :: String -> String -> Interface -> String
compile infile outfile interface = 
    unlines $ C.pp_unit $ stub_body infile interface

stub_body :: String -> Interface -> C.Unit
stub_body infile (Interface ifn descr _) = C.UnitList [
    intf_preamble infile ifn descr,
    C.Blank,

    C.Include C.Standard "barrelfish/barrelfish.h",
    C.Include C.Standard "flounder/flounder_support.h",
    C.Include C.Standard ("if/" ++ ifn ++ "_defs.h"),
    C.Blank,

    C.MultiComment [ "Export function" ],
    export_fn_def ifn,
    C.Blank,

    C.MultiComment [ "Functions to accept/connect over a already shared frame" ],
    accept_fn_def ifn,
    C.Blank,

    C.MultiComment [ "Generic bind function" ],
    -- the two bind functions use the idc drivers in a different order
    bind_cont_def ifn (bind_cont_name ifn) (bind_backends ifn (bind_cont_name ifn)),
    bind_cont_def ifn (bind_cont_name2 ifn) (multihop_bind_backends ifn (bind_cont_name2 ifn)),
    bind_fn_def ifn,
    connect_fn_def ifn]


export_fn_def :: String -> C.Unit
export_fn_def n = 
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (export_fn_name n) params [
        localvar (C.Ptr $ C.Struct $ export_type n) "e"
            (Just $ C.Call "malloc" [C.SizeOfT $ C.Struct $ export_type n]),
        C.If (C.Binary C.Equals exportvar (C.Variable "NULL"))
            [C.Return $ C.Variable "LIB_ERR_MALLOC_FAIL"] [],
        C.SBlank,
        C.SComment "fill in common parts of export struct",
        C.StmtList [C.Ex $ C.Assignment dste (C.Variable srcn) | (dste, srcn) <- [
                        (exportvar `C.DerefField` "connect_cb", "connect_cb"),
                        (exportvar `C.DerefField` "waitset", "ws"),
                        (exportvar `C.DerefField` "st", "st"),
                        (commonvar `C.FieldOf` "export_callback", "export_cb"),
                        (commonvar `C.FieldOf` "flags", "flags"),
                        (commonvar `C.FieldOf` "connect_cb_st", "e"),
                        (commonvar `C.FieldOf` "export_cb_st", "st")]],
        C.SBlank,
        C.SComment "fill in connect handler for each enabled backend",
        C.StmtList [
            C.SIfDef ("CONFIG_FLOUNDER_BACKEND_" ++ (map toUpper drv))
             [C.Ex $ C.Assignment
                        (commonvar `C.FieldOf` (drv_connect_callback drv))
                        (C.Variable $ drv_connect_handler_name drv n)] []
            | drv <- flounder_backends ],
        C.SBlank,

        C.Return $ C.Call "idc_export_service" [C.AddressOf commonvar]
    ]
    where 
        params = [ C.Param (C.Ptr $ C.TypeName "void") "st",
                   C.Param (C.Ptr $ C.TypeName "idc_export_callback_fn") "export_cb",
                   C.Param (C.Ptr $ C.TypeName $ connect_callback_name n) "connect_cb",
                   C.Param (C.Ptr $ C.Struct "waitset") "ws",
                   C.Param (C.TypeName "idc_export_flags_t") "flags"]
        exportvar = C.Variable "e"
        commonvar = exportvar `C.DerefField` "common"

        -- XXX: UMP_IPI uses the UMP connect callback
        drv_connect_callback "ump_ipi" = drv_connect_callback "ump"
        drv_connect_callback drv = drv ++ "_connect_callback"

accept_fn_def :: String -> C.Unit
accept_fn_def n = 
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (accept_fn_name n) params [
        C.StmtList [
        -- #ifdef CONFIG_FLOUNDER_BACKEND_UMP
        C.SIfDef "CONFIG_FLOUNDER_BACKEND_UMP" [
            C.Return $ C.Call (drv_accept_fn_name "ump" n)
                [ C.Variable intf_frameinfo_var,
                  C.Variable "st",
                  C.Variable intf_cont_var,
                  C.Variable "ws",
                  C.Variable "flags"]
             ]
             -- #else
            [ C.StmtList [
                 C.Ex $ C.Call "assert" [
                     C.Unary C.Not $ C.StringConstant "UMP backend not enabled!"
                 ],
                 C.Return $ C.Variable "ERR_NOTIMP"
              ]
            ]
        ]
    ]
    where 
        params = [ C.Param (C.Ptr $ C.Struct $ intf_frameinfo_type n) intf_frameinfo_var,
                   C.Param (C.Ptr $ C.TypeName "void") "st",
       --          C.Param (C.Ptr $ C.TypeName "idc_export_callback_fn") "export_cb",
                   C.Param (C.Ptr $ C.TypeName $ intf_bind_cont_type n) intf_cont_var,
                   C.Param (C.Ptr $ C.Struct "waitset") "ws",
                   C.Param (C.TypeName "idc_export_flags_t") "flags"]


connect_fn_def :: String -> C.Unit
connect_fn_def n = 
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (connect_fn_name n) params [
        C.StmtList [
        -- #ifdef CONFIG_FLOUNDER_BACKEND_UMP
        C.SIfDef "CONFIG_FLOUNDER_BACKEND_UMP" [
            C.Return $ C.Call (drv_connect_fn_name "ump" n)
                [ C.Variable intf_frameinfo_var,
                  C.Variable intf_cont_var,
                  C.Variable "st",
                  C.Variable "ws",
                  C.Variable "flags" ]
        ]
        -- #else
        [ C.StmtList [
             C.Ex $ C.Call "assert" [
                 C.Unary C.Not $ C.StringConstant "UMP backend not enabled!"
             ],
             C.Return $ C.Variable "ERR_NOTIMP"
          ]
        ] ]
    ]
    where 
        params = [ C.Param (C.Ptr $ C.Struct $ intf_frameinfo_type n) intf_frameinfo_var,
                   C.Param (C.Ptr $ C.TypeName $ intf_bind_cont_type n) intf_cont_var,
                   C.Param (C.Ptr $ C.TypeName "void") "st",
                   C.Param (C.Ptr $ C.Struct "waitset") "ws",
                   C.Param (C.TypeName "idc_bind_flags_t") "flags"]


-- bind continuation function
bind_cont_def :: String -> String -> [BindBackend] -> C.Unit
bind_cont_def ifn fn_name backends =
    C.FunctionDef C.Static C.Void fn_name params [
        C.SComment "This bind cont function uses the different backends in the following order:",
        C.SComment $ unwords $ map flounder_backend backends,
        C.SBlank,

        localvar (C.Ptr $ C.Struct "flounder_generic_bind_attempt") "b"
            (Just $ C.Variable "st"),
        C.Switch driver_num cases
            [C.Ex $ C.Call "assert" [C.Unary C.Not $ C.StringConstant "invalid state"]],
        C.SBlank,
        C.Label "out",
        C.Ex $ C.CallInd (C.Cast (C.Ptr $ C.TypeName $ intf_bind_cont_type ifn)
                                (bindst `C.DerefField` "callback"))
                        [bindst `C.DerefField` "st", errvar, C.Variable intf_bind_var],
        C.Ex $ C.Call "free" [bindst]
    ]
    where
        params = [ C.Param (C.Ptr $ C.Void) "st",
                   C.Param (C.TypeName "errval_t") "err",
                   C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var]
        driver_num = bindst `C.DerefField` "driver_num"
        bindst = C.Variable "b"
        cases = [ C.Case (C.NumConstant $ toInteger n) (mkcase n)
                  | n <- [0 .. length backends] ]

        mkcase n
            | n == 0 = try_next

            | n == length backends = [
                C.SIfDef config_prev_driver
                    [C.If (test_cb_success prev_backend)
                        -- success!
                        [success_callback]
                        -- failure, but clean up attempt
                        [C.StmtList $ cleanup_bind prev_backend,
                         C.If (C.Unary C.Not $ test_cb_try_next prev_backend)
                            [fail_callback errvar]
                            []]
                    ]
                    [],
                fail_callback (C.Variable "FLOUNDER_ERR_GENERIC_BIND_NO_MORE_DRIVERS")
                ]

            | otherwise = [
                C.SIfDef config_prev_driver
                    [C.If (test_cb_success prev_backend)
                        -- success!
                        [success_callback]

                        -- failure, cleanup and decide whether to continue
                        [C.StmtList $ cleanup_bind prev_backend,
                         C.If (test_cb_try_next prev_backend)
                            [C.Goto ("try_next_" ++ show n)]
                            [C.SComment "report permanent failure to user",
                             fail_callback errvar]
                            ],

                     C.Label ("try_next_" ++ show n)
                    ] [],

                -- previous driver not enabled, just try the next
                C.StmtList try_next]
            where
                prev_backend = backends !! (n - 1)
                next_backend = backends !! n
                config_prev_driver = "CONFIG_FLOUNDER_BACKEND_"
                                ++ (map toUpper (flounder_backend prev_backend))
                config_next_driver = "CONFIG_FLOUNDER_BACKEND_"
                                ++ (map toUpper (flounder_backend next_backend))

                try_next = [C.Ex $ C.PostInc driver_num,
                            C.SIfDef config_next_driver
                                [C.SComment "try next backend",
                                 C.StmtList $ start_bind next_backend,
                                 C.If (C.Call "err_is_fail" [errvar])
                                    -- bind attempt failed
                                    [C.StmtList $ cleanup_bind next_backend,
                                     fail_callback errvar]
                                    [C.ReturnVoid]]
                                [C.SComment "skip non-enabled backend (fall through)"]]

                fail_callback err = C.StmtList $
                    (if err /= errvar
                        then [C.Ex $ C.Assignment errvar err]
                        else [])
                    ++ [
                        C.Ex $ C.Assignment (C.Variable intf_bind_var) (C.Variable "NULL"),
                        C.Goto "out"]

                success_callback = C.Goto "out"


bind_fn_def :: String -> C.Unit
bind_fn_def n = 
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (bind_fn_name n) params [
        C.SComment "allocate state",
        localvar (C.Ptr $ C.Struct "flounder_generic_bind_attempt") "b"
            (Just $ C.Call "malloc" [C.SizeOfT $ C.Struct "flounder_generic_bind_attempt"]),
        C.If (C.Binary C.Equals (C.Variable "b") (C.Variable "NULL"))
            [C.Return $ C.Variable "LIB_ERR_MALLOC_FAIL"] [],
        C.SBlank,
        C.SComment "fill in binding state",
        C.StmtList [C.Ex $ C.Assignment (C.Variable "b" `C.DerefField` dstf) srce
                    | (dstf, srce) <- [
                        ("iref", C.Variable "iref"),
                        ("waitset", C.Variable "waitset"),
                        ("driver_num", C.NumConstant 0),
                        ("callback", C.Variable intf_cont_var),
                        ("st", C.Variable "st"),
                        ("flags", C.Variable "flags")]],
        C.SBlank,
        C.If (C.Binary C.BitwiseAnd (C.Variable "flags") (C.Variable "IDC_BIND_FLAG_MULTIHOP"))
        [C.Ex $ C.Call (bind_cont_name2 n) [C.Variable "b", C.Variable "SYS_ERR_OK", C.Variable "NULL"]]
        [C.Ex $ C.Call (bind_cont_name n) [C.Variable "b", C.Variable "SYS_ERR_OK", C.Variable "NULL"]],
        C.SBlank,
        C.Return $ C.Variable "SYS_ERR_OK"
    ]
    where 
      params = [ C.Param (C.TypeName "iref_t") "iref",
                 C.Param (C.Ptr $ C.TypeName $ intf_bind_cont_type n) intf_cont_var,
                 C.Param (C.Ptr $ C.TypeName "void") "st",
                 C.Param (C.Ptr $ C.Struct "waitset") "waitset",
                 C.Param (C.TypeName "idc_bind_flags_t") "flags" ]

----------------------------------------------------------------------------
-- everything that we need to know about a backend to attempt a generic bind
----------------------------------------------------------------------------
data BindBackend = BindBackend {
    flounder_backend :: String,     -- name of the flounder backend
    start_bind :: [C.Stmt],         -- code to attempt a bind
    test_cb_success :: C.Expr,      -- expression to test if a bind succeeded (in the callback)
    test_cb_try_next :: C.Expr,     -- expression to test if a bind might succeed with another backend
    cleanup_bind :: [C.Stmt]        -- code to cleanup a failed bind
}

-- the available bind backends
-- Cation: order of list matters (we will try to bind in that order)
bind_backends :: String -> String -> [BindBackend]
bind_backends ifn cont_fn_name = map (\i -> i ifn (C.Variable cont_fn_name)) 
                    [lmp_bind_backend, 
                     ump_ipi_bind_backend, 
                     ump_bind_backend, 
                     multihop_bind_backend]
                                                     
-- backends in different order (prefer multihop over ump, etc.)
multihop_bind_backends :: String -> String -> [BindBackend]
multihop_bind_backends ifn cont_fn_name = map (\i -> i ifn (C.Variable cont_fn_name))
                    [lmp_bind_backend, 
                     multihop_bind_backend, 
                     ump_ipi_bind_backend, 
                     ump_bind_backend]

bindst = C.Variable "b"
binding = bindst `C.DerefField` "binding"
iref = bindst `C.DerefField` "iref"        
waitset = bindst `C.DerefField` "waitset"
flags = bindst `C.DerefField` "flags"

lmp_bind_backend ifn cont = 
  BindBackend {
    flounder_backend = "lmp",
    start_bind = [
        C.Ex $ C.Assignment binding $
            C.Call "malloc" [C.SizeOfT $ C.Struct $ lmp_bind_type ifn],
        C.Ex $ C.Call "assert" [C.Binary C.NotEquals binding (C.Variable "NULL")],
        C.Ex $ C.Assignment errvar $
            C.Call (lmp_bind_fn_name ifn) [binding, iref, cont, C.Variable "b", waitset,
                                           flags,
                                           C.Variable "DEFAULT_LMP_BUF_WORDS"]
    ],
    test_cb_success = C.Call "err_is_ok" [errvar],
    test_cb_try_next = C.Binary C.Equals (C.Call "err_no" [errvar])
                                         (C.Variable "MON_ERR_IDC_BIND_NOT_SAME_CORE"),
    cleanup_bind = [ C.Ex $ C.Call "free" [binding] ]
    }
  
ump_bind_backend ifn cont =   
  BindBackend {
    flounder_backend = "ump",
    start_bind = [
        C.Ex $ C.Assignment binding $
            C.Call "malloc" [C.SizeOfT $ C.Struct $ UMP.bind_type ifn],
        C.Ex $ C.Call "assert" [C.Binary C.NotEquals binding (C.Variable "NULL")],
        C.Ex $ C.Assignment errvar $
            C.Call (UMP.bind_fn_name ifn) [binding, iref, cont, C.Variable "b", waitset,
                                           flags,
                                           C.Variable "DEFAULT_UMP_BUFLEN",
                                           C.Variable "DEFAULT_UMP_BUFLEN"]
    ],
    test_cb_success = C.Call "err_is_ok" [errvar],
    test_cb_try_next = C.Variable "true",
    cleanup_bind = [ C.Ex $ C.Call "free" [binding] ]
    }
  
ump_ipi_bind_backend ifn cont = 
  BindBackend {
    flounder_backend = "ump_ipi",
    start_bind = [
        C.Ex $ C.Assignment binding $
            C.Call "malloc" [C.SizeOfT $ C.Struct $ UMP_IPI.bind_type ifn],
        C.Ex $ C.Call "assert" [C.Binary C.NotEquals binding (C.Variable "NULL")],
        C.Ex $ C.Assignment errvar $
            C.Call (UMP_IPI.bind_fn_name ifn) [binding, iref, cont, C.Variable "b", waitset,
                                           flags,
                                           C.Variable "DEFAULT_UMP_BUFLEN",
                                           C.Variable "DEFAULT_UMP_BUFLEN"]
    ],
    test_cb_success = C.Call "err_is_ok" [errvar],
    test_cb_try_next = C.Variable "true",
    cleanup_bind = [ C.Ex $ C.Call "free" [binding] ]
    }
  
multihop_bind_backend ifn cont = 
  BindBackend {
    flounder_backend = "multihop",
    start_bind = [C.Ex $ C.Assignment binding $
                         C.Call "malloc" [C.SizeOfT $ C.Struct $ Multihop.m_bind_type ifn],
                         C.Ex $ C.Call "assert" [C.Binary C.NotEquals binding (C.Variable "NULL")],
                         C.Ex $ C.Assignment errvar $
                         C.Call (Multihop.m_bind_fn_name ifn) [binding, iref, cont, C.Variable "b", waitset, flags]],
    test_cb_success = C.Call "err_is_ok" [errvar],
    test_cb_try_next = C.Variable "true",
    cleanup_bind = [ C.Ex $ C.Call "free" [binding] ]
    }

-}
