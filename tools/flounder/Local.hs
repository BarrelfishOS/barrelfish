{-
   Local.hs: Flounder stub generator for dummy local stubs

  Part of Flounder: a message passing IDL for Barrelfish

  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}

module Local where

import qualified CAbsSyntax as C
import qualified Backend
import Syntax
import BackendCommon hiding (can_send_fn_def, register_send_fn_def)

------------------------------------------------------------------------
-- Language mapping: C identifier names
------------------------------------------------------------------------

drvname = "local"

-- Name of the init function
local_init_fn_name n = ifscope n "local_init"

-- Name of the transmit vtable
local_vtbl_name ifn = ifscope ifn "local_tx_vtbl"

-- Name of the transmit function
tx_fn_name ifn mn = idscope ifn mn "local_send"

change_waitset_fn_name ifn = ifscope ifn "local_change_waitset"
get_receiving_chanstate_fn_name ifn = ifscope ifn "local_get_receiving_chanstate"
    
------------------------------------------------------------------------
-- Language mapping: Create the header file for this interconnect driver
------------------------------------------------------------------------

header :: String -> String -> Interface -> String
header infile outfile intf@(Interface name descr decls) =
    unlines $ C.pp_unit $ header_file intf header_body
    where
        header_file :: Interface -> [C.Unit] -> C.Unit
        header_file interface@(Interface name _ _) body =
            let sym = "__" ++ name ++ "_LOCAL_H"
            in C.IfNDef sym ([ C.Define sym [] "1"] ++ body) []

        header_body = [
            intf_preamble infile name descr,
            C.Blank,
            C.MultiComment [ "Local interconnect driver" ],
            C.Blank,
            local_init_function_proto name,
            local_connect_handler_proto name
            ]

local_init_function_proto :: String -> C.Unit
local_init_function_proto n =
    C.GVarDecl C.Extern C.NonConst
         (C.Function C.NoScope C.Void params) name Nothing
    where
      name = local_init_fn_name n
      params = [C.Param (C.Ptr $ C.Struct (intf_bind_type n)) "local_binding",
                C.Param (C.Ptr $ C.Struct "waitset") "waitset",
                C.Param (C.Ptr $ C.Struct (intf_bind_type n)) "my_binding"]

local_connect_handler_proto :: String -> C.Unit
local_connect_handler_proto ifn = C.GVarDecl C.Extern C.NonConst
    (C.Function C.NoScope (C.TypeName "errval_t") local_connect_handler_params)
    (drv_connect_handler_name drvname ifn) Nothing

local_connect_handler_params :: [C.Param]
local_connect_handler_params
    = [C.Param (C.Ptr $ C.Void) "st",
       C.Param (C.Ptr $ C.Void) "local_binding",
       C.Param (C.Ptr $ C.Ptr $ C.Void) "my_binding"]

------------------------------------------------------------------------
-- Language mapping: Create the stub (implementation) for this interconnect driver
------------------------------------------------------------------------

stub :: String -> String -> Interface -> String
stub infile outfile intf =
    unlines $ C.pp_unit $ local_stub_body infile intf

local_stub_body :: String -> Interface -> C.Unit
local_stub_body infile intf@(Interface ifn descr decls) = C.UnitList [
    intf_preamble infile ifn descr,
    C.Blank,
    C.MultiComment [ "Generated Local stub" ],
    C.Blank,

    C.Define "_USE_XOPEN" [] "/* for strdup() */",
    C.Include C.Standard "string.h",
    C.Include C.Standard "barrelfish/barrelfish.h",
    C.Include C.Standard "flounder/flounder_support.h",
    C.Include C.Standard ("if/" ++ ifn ++ "_defs.h"),
    C.Include C.Standard ("if/" ++ ifn ++ "_local_defs.h"),
    C.Blank,

    C.MultiComment [ "Message sender functions" ],
    C.UnitList [ tx_fn ifn m | m <- messages ],
    C.Blank,

    C.MultiComment [ "Send vtable" ],
    tx_vtbl ifn messages,

    C.MultiComment [ "Control functions" ],
    can_send_fn_def ifn,
    register_send_fn_def ifn,
    default_error_handler_fn_def drvname ifn,
    change_waitset_fn_def ifn,
    generic_control_fn_def drvname ifn,
    get_receiving_chanstate_fn_def ifn,

    C.MultiComment [ "Function to initialise the binding state" ],
    local_init_fn ifn,

    C.MultiComment [ "Connect callback for export" ],
    local_connect_handler_fn ifn
    ]
    where
        (types, messagedecls) = Backend.partitionTypesMessages decls
        messages = rpcs_to_msgs messagedecls

local_init_fn :: String -> C.Unit
local_init_fn ifn
    = C.FunctionDef C.NoScope C.Void (local_init_fn_name ifn) params [
        C.StmtList common_init,
        C.Ex $ C.Assignment (common_field "change_waitset")
                                (C.Variable $ change_waitset_fn_name ifn),
        C.Ex $ C.Assignment (common_field "control")
                                (C.Variable $ generic_control_fn_name drvname ifn),
        C.Ex $ C.Assignment (common_field "local_binding")
                                (C.Variable "local_binding"),
        C.Ex $ C.Assignment (common_field "get_receiving_chanstate")
                    (C.Variable $ get_receiving_chanstate_fn_name ifn)
    ]
    where
      params = [C.Param (C.Ptr $ C.Struct (intf_bind_type ifn)) intf_bind_var,
                C.Param (C.Ptr $ C.Struct "waitset") "waitset",
                C.Param (C.Ptr $ C.Struct (intf_bind_type ifn)) "local_binding"]
      common_field f = (C.Variable intf_bind_var) `C.DerefField` f
      common_init = binding_struct_init "local" ifn
        (C.DerefPtr $ C.Variable intf_bind_var)
        (C.Variable "waitset")
        (C.Variable $ local_vtbl_name ifn)

can_send_fn_def :: String -> C.Unit
can_send_fn_def ifn =
    C.FunctionDef C.Static (C.TypeName "bool") (can_send_fn_name drvname ifn) params [
        C.Return $ C.Variable "true"]
    where
        params = [ C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) "b" ]

register_send_fn_def :: String -> C.Unit
register_send_fn_def ifn =
    C.FunctionDef C.Static (C.TypeName "errval_t") (register_send_fn_name drvname ifn) params [
        C.Return $ C.Variable "ERR_NOTIMP"
    ]
    where
        params = [ C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) "b",
                   C.Param (C.Ptr $ C.Struct "waitset") "ws",
                   C.Param (C.Struct "event_closure") intf_cont_var ]

change_waitset_fn_def :: String -> C.Unit
change_waitset_fn_def ifn =
    C.FunctionDef C.Static (C.TypeName "errval_t") (change_waitset_fn_name ifn) params [
        C.Return $ C.Variable "ERR_NOTIMP"
    ]
    where
        params = [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var,
                  C.Param (C.Ptr $ C.Struct "waitset") "ws"]

tx_fn :: String -> MessageDef -> C.Unit
tx_fn ifn msg@(Message _ mn args _) =
    C.FunctionDef C.Static (C.TypeName "errval_t") (tx_fn_name ifn mn) params body
    where
        params = [binding_param ifn, cont_param] ++ (
                    concat [ msg_argdecl TX ifn a | a <- args ])
        cont_param = C.Param (C.Struct "event_closure") intf_cont_var
        body = [
            C.SComment "call rx handler",
            C.Ex $ C.Call "assert" [C.Binary C.NotEquals handler (C.Variable "NULL")],
            C.Ex $ C.CallInd handler ((local_binding):(concat $ map mkvars args)),
            C.SBlank,
            C.SComment "run continuation, if any",
            C.If (C.Binary C.And (C.Binary C.NotEquals
                                (C.Variable intf_cont_var `C.FieldOf` "handler")
                                (C.Variable "NULL"))
                (C.Binary C.NotEquals
                        (C.Variable intf_cont_var `C.FieldOf` "handler")
                        (C.Variable "blocking_cont")))
                [C.Ex $ C.CallInd (C.Variable intf_cont_var `C.FieldOf` "handler")
                                [C.Variable intf_cont_var `C.FieldOf` "arg"]] [],
            C.SBlank,
            C.Return $ C.Variable "SYS_ERR_OK"
            ]
        -- string and array arguments need special treatment
        mkvars (Arg _ (StringArray n l)) = [C.Variable n]
        mkvars (Arg _ (DynamicArray n l _)) = [C.Variable n, C.Variable l]
        mkvars (Arg _ (Name n)) = [C.Variable n]

        binding = C.Variable intf_bind_var
        local_binding = C.DerefField binding "local_binding"
        handler = C.FieldOf (C.DerefField local_binding "rx_vtbl") mn

tx_vtbl :: String -> [MessageDef] -> C.Unit
tx_vtbl ifn ml =
    C.StructDef C.Static (intf_vtbl_type ifn TX) (local_vtbl_name ifn) fields
    where
        fields = [let mn = msg_name m in (mn, tx_fn_name ifn mn) | m <- ml]

local_connect_handler_fn :: String -> C.Unit
local_connect_handler_fn ifn = C.FunctionDef C.NoScope (C.TypeName "errval_t")
    (drv_connect_handler_name "local" ifn) local_connect_handler_params [
    
    localvar (C.Ptr $ C.Struct $ export_type ifn) "e" $ Just $ C.Variable "st",
    localvar (C.TypeName "errval_t") "err" Nothing,
    C.SBlank,
    C.SComment "allocate storage for binding",
    localvar (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var
        $ Just $ C.Call "malloc" [C.SizeOfT $ C.Struct $ intf_bind_type ifn],
    C.If (C.Binary C.Equals (C.Variable intf_bind_var) (C.Variable "NULL"))
        [C.Return $ C.Variable "LIB_ERR_MALLOC_FAIL"] [],
    C.SBlank,

    C.Ex $ C.Call (local_init_fn_name ifn) [binding,
        C.DerefField (C.Cast (C.Ptr $ C.Struct $ intf_bind_type ifn) (C.Variable "local_binding")) "waitset",
        C.Variable "local_binding"],
    C.SComment "run user's connect handler",
    C.Ex $ C.Call "assert" [(C.DerefField exportvar "connect_cb")],
    C.Ex $ C.Assignment errvar $ C.CallInd (C.DerefField exportvar "connect_cb")
                       [C.DerefField exportvar "st", bindvar],
    C.If (C.Call "err_is_fail" [errvar])
        [C.SComment "connection refused",
         C.Return $ errvar] [],
    C.SBlank,
    C.Ex $ C.Assignment (C.DerefPtr $ C.Variable "my_binding") binding,
    C.Return $ C.Variable "SYS_ERR_OK"]
    where
        exportvar = C.Variable "e"
        binding = C.Variable intf_bind_var

get_receiving_chanstate_fn_def :: String -> C.Unit
get_receiving_chanstate_fn_def ifn =
    C.FunctionDef C.Static (C.Ptr $ C.Struct "waitset_chanstate") (get_receiving_chanstate_fn_name ifn) params [
        C.Return $ C.Variable "NULL"]
    where
        params = [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var]
