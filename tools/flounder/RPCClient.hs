{-
  RPCClient.hs: Flounder stub generator for RPC client-side stubs

  Part of Flounder: a message passing IDL for Barrelfish

  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}

module RPCClient where

import qualified CAbsSyntax as C
import qualified Backend
import BackendCommon hiding (errvar)
import GHBackend (msg_signature_generic, intf_vtbl_param)
import Syntax

------------------------------------------------------------------------
-- Language mapping: C identifier names
------------------------------------------------------------------------

-- Name of the binding struct
rpc_bind_type :: String -> String
rpc_bind_type ifn = ifscope ifn "rpc_client"

-- Name of the binding parameter
rpc_bind_var = "_rpc" :: String

-- Name of the RPC function
rpc_fn_name ifn mn = idscope ifn mn "rpc"
local_rpc_fn_name ifn mn = idscope ifn mn "local_rpc"

-- Name of the RPC vtable
rpc_vtbl_name ifn = ifscope ifn "rpc_vtbl"
local_rpc_vtbl_name ifn = ifscope ifn "local_rpc_vtbl"

-- Name of the init function
rpc_init_fn_name :: String -> String
rpc_init_fn_name ifn = ifscope ifn "rpc_client_init"

-- Name of the error handler
rpc_error_fn_name :: String -> String
rpc_error_fn_name ifn = ifscope ifn "rpc_client_error"

-- Name of the struct type for the method vtable
rpc_vtbl_type :: String -> String
rpc_vtbl_type ifn = ifscope ifn "rpc_vtbl"
local_rpc_vtbl_type :: String -> String
local_rpc_vtbl_type ifn = ifscope ifn "local_rpc_vtbl"

------------------------------------------------------------------------
-- Language mapping: Create the header file for this interconnect driver
------------------------------------------------------------------------

header :: String -> String -> Interface -> String
header infile outfile intf =
    unlines $ C.pp_unit $ header_file intf (rpc_header_body infile intf)
    where
        header_file :: Interface -> [C.Unit] -> C.Unit
        header_file interface@(Interface name _ _) body =
            let sym = "__" ++ name ++ "_RPC_CLIENT_H"
            in C.IfNDef sym ([ C.Define sym [] "1"] ++ body) []

rpc_header_body :: String -> Interface -> [C.Unit]
rpc_header_body infile interface@(Interface name descr decls) = [
    C.Blank]

rpc_vtbl_decl :: String -> [MessageDef] -> C.Unit
rpc_vtbl_decl n ml =
    C.StructDecl (rpc_vtbl_type n) [ intf_vtbl_param n m TX | m <- ml ]

rpc_binding_param :: String -> C.Param
rpc_binding_param ifname = C.Param (C.Ptr $ C.Struct $ rpc_bind_type ifname) rpc_bind_var

rpc_binding_struct :: String -> C.Unit
rpc_binding_struct name = C.StructDecl (rpc_bind_type name) fields
  where
    fields = [
        C.Param (C.Ptr $ C.Struct $ intf_bind_type name) "b",
        C.Param (C.Struct $ rpc_vtbl_type name) "vtbl"]

rpc_init_fn_proto :: String -> C.Unit
rpc_init_fn_proto n =
    C.GVarDecl C.Extern C.NonConst
         (C.Function C.NoScope (C.TypeName "errval_t") (rpc_init_fn_params n)) name Nothing
    where
      name = rpc_init_fn_name n

------------------------------------------------------------------------
-- Language mapping: Create the stub (implementation) for this interconnect driver
------------------------------------------------------------------------

stub :: String -> String -> Interface -> String
stub infile outfile intf = unlines $ C.pp_unit $ rpc_stub_body infile intf

rpc_stub_body :: String -> Interface -> C.Unit
rpc_stub_body infile intf@(Interface ifn descr decls) = C.UnitList [
    intf_preamble infile ifn descr,
    C.Blank,
    C.MultiComment [ "Generated Stub for RPC" ],
    C.Blank,

    C.Include C.Standard "barrelfish/barrelfish.h",
    C.Include C.Standard "flounder/flounder_support.h",
    C.Include C.Standard ("if/" ++ ifn ++ "_rpcclient_defs.h"),
    C.Blank]
    where
        (types, messagedecls) = Backend.partitionTypesMessages decls
        rpcs = [m | m@(RPC _ _ _) <- messagedecls]

rpc_fn :: String -> [TypeDef] -> MessageDef -> C.Unit
rpc_fn ifn typedefs msg@(RPC n args _) =
    C.FunctionDef C.Static (C.TypeName "errval_t") (rpc_fn_name ifn n) params [
        localvar (C.TypeName "errval_t") errvar_name (Just $ C.Variable "SYS_ERR_OK"),
        C.Ex $ C.Call "assert" [C.Unary C.Not rpc_progress_var],
        C.Ex $ C.Call "assert" [C.Binary C.Equals async_err_var (C.Variable "SYS_ERR_OK")],
        C.Ex $ C.Call "thread_set_rpc_in_progress" [C.Variable "true"],
        C.SBlank,
        C.SComment "set provided caprefs on underlying binding",
        binding_save_rx_slots,
        C.SBlank,
        C.SComment "call send function",
        C.Ex $ C.Assignment binding_error (C.Variable "SYS_ERR_OK"),
        C.Ex $ C.Call "thread_set_outgoing_token" [C.Call "thread_set_token" [message_chanstate]],
        C.Ex $ C.Assignment errvar $ C.CallInd tx_func tx_func_args,
        C.If (C.Call "err_is_fail" [errvar]) [
            C.Goto "out"] [],
        C.SBlank,
        C.SComment "wait for message to be sent and reply or error to be present",
        C.Ex $ C.Assignment errvar $ C.Call "wait_for_channel"
                [waitset_var, message_chanstate, C.AddressOf binding_error],
        C.SBlank,
        C.If (C.Call "err_is_fail" [errvar]) [
            C.Goto "out"] [],
        C.SBlank,

        C.StmtList [assign typedefs arg | arg <- rxargs],
        C.Ex $ C.Assignment errvar $ C.CallInd receive_next [bindvar],
        C.Label "out",
        C.Ex $ C.Call "thread_set_rpc_in_progress" [C.Variable "false"],
        C.Ex $ C.Call "thread_clear_token" [receiving_chanstate],
        C.Return errvar
    ]
    where
        params = [rpc_binding_param ifn]
                 ++ concat [rpc_argdecl2 TX ifn typedefs a | a <- args]
        rpcvar = C.Variable rpc_bind_var
        rpc_progress_var = C.Call "thread_get_rpc_in_progress" []
        async_err_var = C.Call "thread_get_async_error" []
        waitset_var = C.DerefField bindvar "waitset"
        bindvar = C.DerefField rpcvar "b"
        tx_func = C.DerefField bindvar "tx_vtbl" `C.FieldOf` (rpc_call_name n)
        tx_func_args = [bindvar, C.Variable "BLOCKING_CONT"] ++ (map C.Variable $ concat $ map mkargs txargs)
        mkargs (Arg _ (Name an)) = [an]
        mkargs (Arg _ (StringArray an _)) = [an]
        mkargs (Arg _ (DynamicArray an al _)) = [an, al]
        (txargs, rxargs) = partition_rpc_args args
        is_cap_arg (Arg (Builtin t) _) = t == Cap || t == GiveAwayCap
        is_cap_arg (Arg _ _) = False
        rx_cap_args = filter is_cap_arg rxargs
        binding_save_rx_slot (Arg tr (Name an)) = C.Ex $
            C.Call "thread_store_recv_slot" [(C.DerefPtr $ C.Variable an)]
        binding_save_rx_slots = C.StmtList [ binding_save_rx_slot c | c <- rx_cap_args ]
        token_name = "token"
        outgoing_token = bindvar `C.DerefField` "outgoing_token"
        receiving_chanstate = C.CallInd (bindvar `C.DerefField` "get_receiving_chanstate") [bindvar]
        binding_error = C.DerefField bindvar "error"
        message_chanstate = C.Binary C.Plus (C.DerefField bindvar "message_chanstate") (C.Variable $ msg_enum_elem_name ifn (rpc_resp_name n))
        receive_next = C.DerefField bindvar "receive_next"
        assign td (Arg tr (Name an)) = case lookup_typeref typedefs tr of
            TArray t n _ -> C.If (rpc_rx_union_elem n an) [ C.Ex $ C.Call "mem__cpy" [
                                (rpc_rx_union_elem n an),
                                (C.Variable an),
                                C.SizeOfT $ C.TypeName (type_c_name1 ifn n)]][]
            _ -> C.If (C.Variable an) [
                    C.Ex $ C.Assignment (C.DerefPtr $ C.Variable an) (rpc_rx_union_elem n an)] []
        assign _ (Arg _ (StringArray an l)) =  C.If (C.Variable an) [
                C.Ex $ C.Call "strncpy" [(C.Variable an), (rpc_rx_union_elem n an), C.NumConstant l]
            ] []
        assign _ (Arg _ (DynamicArray an al l)) =  C.If (C.Binary C.And (C.Variable an) (C.Variable al)) [
                C.Ex $ C.Assignment (C.DerefPtr $ C.Variable al) (rpc_rx_union_elem n al),
                C.Ex $ C.Call "memcpy" [(C.Variable an), (rpc_rx_union_elem n an), C.DerefPtr $ C.Variable al]
            ] []


local_rpc_fn :: String -> [TypeDef] -> MessageDef -> C.Unit
local_rpc_fn ifn typedefs msg@(RPC n args _) =
    C.FunctionDef C.Static (C.TypeName "errval_t") (local_rpc_fn_name ifn n) params [
        C.Return $ C.CallInd tx_func (localbindvar:(map C.Variable $ concat $ map mkargs rpc_args))
    ]
    where
        params = [rpc_binding_param ifn]
                 ++ concat [rpc_argdecl2 TX ifn typedefs a | a <- args]
        rpc_args = map rpc_arg args
        tx_func = C.DerefField localbindvar "rpc_rx_vtbl" `C.FieldOf` (rpc_call_name n)
        rpcvar = C.Variable rpc_bind_var
        bindvar = C.DerefField rpcvar "b"
        localbindvar = C.DerefField bindvar "local_binding"
        rpc_arg (RPCArgIn t v) = Arg t v
        rpc_arg (RPCArgOut t v) = Arg t v
        mkargs (Arg _ (Name an)) = [an]
        mkargs (Arg _ (StringArray an _)) = [an]
        mkargs (Arg _ (DynamicArray an al _)) = [an, al]
        (txargs, rxargs) = partition_rpc_args args

rpc_vtbl :: String -> [MessageDef] -> C.Unit
rpc_vtbl ifn ml =
    C.StructDef C.Static (rpc_vtbl_type ifn) (rpc_vtbl_name ifn) fields
    where
        fields = [let mn = msg_name m in (mn, rpc_fn_name ifn mn) | m <- ml]

local_rpc_vtbl :: String -> [MessageDef] -> C.Unit
local_rpc_vtbl ifn ml =
    C.StructDef C.Static (rpc_vtbl_type ifn) (local_rpc_vtbl_name ifn) fields
    where
        fields = [let mn = msg_name m in (mn, local_rpc_fn_name ifn mn) | m <- ml]


arg_names :: MessageArgument -> [String]
arg_names (Arg _ v) = var_names v
    where
        var_names (Name n) = [n]
        var_names (StringArray n _) = [n]
        var_names (DynamicArray n1 n2 _) = [n1, n2]

rpc_error_fn :: String -> C.Unit
rpc_error_fn ifn = C.FunctionDef C.Static C.Void (rpc_error_fn_name ifn)
    [binding_param ifn, C.Param (C.TypeName "errval_t") errvar_name]
    [C.SComment "get RPC client state pointer",
     localvar (C.Ptr $ C.Struct $ rpc_bind_type ifn) rpc_bind_var $
        Just $ C.DerefField bindvar "st",
     C.SBlank,
     C.If (C.Call "thread_get_rpc_in_progress" [])
        [C.Ex $ C.Call "assert" [C.Call "err_is_fail" [errvar]],
         C.Ex $ C.Call "thread_set_async_error" [errvar],
         C.SComment "kick waitset with dummy event"]
        [C.Ex $ C.Call "USER_PANIC_ERR" [errvar, C.StringConstant "async error in RPC"]]
    ]
    where
        rpcvar = C.Variable rpc_bind_var

rpc_init_fn :: String -> [MessageDef] -> C.Unit
rpc_init_fn ifn ml = C.FunctionDef C.NoScope (C.TypeName "errval_t")
                            (rpc_init_fn_name ifn) (rpc_init_fn_params ifn) $
    [
     C.SBlank,
     C.SComment "Setup state of RPC client object",
     C.Ex $ C.Assignment (C.DerefField rpcvar "b") bindvar,



     C.If (C.DerefField bindvar "local_binding") [
        C.Ex $ C.Assignment (C.DerefField rpcvar "vtbl") (C.Variable $ local_rpc_vtbl_name ifn)
     ][
        C.Ex $ C.Assignment (C.DerefField rpcvar "vtbl") (C.Variable $ rpc_vtbl_name ifn)
     ],
     C.SBlank,
     C.SComment "Set RX handlers on binding object for RPCs",
     C.StmtList [C.Ex $ C.Assignment (C.FieldOf (C.DerefField bindvar "rx_vtbl")
                                        (rpc_resp_name mn))
         (C.Variable "NULL") | RPC mn _ _ <- ml],
     
     
     
     
     C.Ex $ C.Assignment (C.DerefField bindvar "st") rpcvar,
     C.SBlank,
     C.SComment "Set RX handlers on binding object for RPCs",
     C.StmtList [C.Ex $ C.Assignment (C.FieldOf (C.DerefField bindvar "rx_vtbl")
                                        (rpc_resp_name mn))
         (C.Variable "NULL") | RPC mn _ _ <- ml],
     C.SBlank,
     C.SComment "Set error handler on binding object",
     C.Ex $ C.Assignment (bindvar `C.DerefField` "error_handler") (C.Variable "NULL"),
    --  C.Ex $ C.Assignment (bindvar `C.DerefField` "error_handler")
    --                       (C.Variable $ rpc_error_fn_name ifn),
     C.SBlank,
     C.Return $ C.Variable "SYS_ERR_OK"]
    where
        rpcvar = C.Variable "rpc"
        bindvar = C.Variable "binding"
        waitset_addr = C.AddressOf $ C.DerefField bindvar "waitset"

rpc_init_fn_params n = [C.Param (C.Ptr $ C.Struct (rpc_bind_type n)) "rpc",
                        C.Param (C.Ptr $ C.Struct (intf_bind_type n)) "binding"]

rpc_rx_union_elem :: String -> String -> C.Expr
rpc_rx_union_elem mn fn =
   C.FieldOf (C.FieldOf (C.DerefField (C.DerefField (C.Variable rpc_bind_var) "b") "rx_union")
                    (rpc_resp_name mn)) fn

errvar_name = "_err"
errvar = C.Variable errvar_name
