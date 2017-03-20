{-
   GCBackend: Flounder stub generator for generic code

  Part of Flounder: a message passing IDL for Barrelfish

  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}

module GCBackend where

import Data.Char

import qualified CAbsSyntax as C
import Syntax
import GHBackend (flounder_backends, export_fn_name, bind_fn_name, accept_fn_name, connect_fn_name, connect_handlers_fn_name, disconnect_handlers_fn_name, rpc_tx_vtbl_type, rpc_init_fn_name)
import qualified Backend
import BackendCommon
import LMP (lmp_bind_type, lmp_bind_fn_name)
import qualified UMP (bind_type, bind_fn_name)
import qualified UMP_IPI (bind_type, bind_fn_name)
import qualified Multihop (m_bind_type, m_bind_fn_name)
import Local (local_init_fn_name)


-- import GHBackend (msg_signature_generic, intf_vtbl_param)

-- name of the bind continuation function
bind_cont_name :: String -> String
bind_cont_name ifn = ifscope ifn "bind_continuation_direct"

-- name of an alternative bind continuation function
bind_cont_name2 :: String -> String
bind_cont_name2 ifn = ifscope ifn "bind_contination_multihop"

-- Name of the RPC function
rpc_fn_name ifn mn = idscope ifn mn "rpc"
local_rpc_fn_name ifn mn = idscope ifn mn "local_rpc"

-- Name of the RPC vtable
rpc_vtbl_name ifn = ifscope ifn "rpc_vtbl"
local_rpc_vtbl_name ifn = ifscope ifn "local_rpc_vtbl"

-- Name of the error handler
rpc_error_fn_name :: String -> String
rpc_error_fn_name ifn = ifscope ifn "rpc_client_error"

compile :: String -> String -> Interface -> String
compile infile outfile interface =
    unlines $ C.pp_unit $ stub_body infile interface

stub_body :: String -> Interface -> C.Unit
stub_body infile (Interface ifn descr decls) = C.UnitList [
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


compile_message_handlers :: String -> String -> Interface -> String
compile_message_handlers infile outfile interface =
    unlines $ C.pp_unit $ stub_body_message_handlers infile interface

stub_body_message_handlers :: String -> Interface -> C.Unit
stub_body_message_handlers infile (Interface ifn descr decls) = C.UnitList [
    intf_preamble infile ifn descr,
    C.Blank,

    C.Include C.Standard "barrelfish/barrelfish.h",
    C.Include C.Standard "flounder/flounder_support.h",
    C.Include C.Standard ("if/" ++ ifn ++ "_defs.h"),
    C.Blank,

    C.MultiComment [ "Message handlers" ],
    C.UnitList [ msg_handler ifn m types | m@(Message MMessage _ _ _) <- messages ],
    C.UnitList [ msg_handler ifn m types | m@(Message MResponse _ _ _) <- messages ],
    C.UnitList [ msg_handler ifn m types | m <- rpcs ],
    C.Blank,

    C.MultiComment [ "Connect handlers function" ],
    connect_handlers_fn_def ifn messages,
    C.Blank,

    C.MultiComment [ "Disconnect handlers function" ],
    disconnect_handlers_fn_def ifn messages,
    C.Blank,

    C.MultiComment [ "RPC wrapper functions" ],
    C.UnitList [ rpc_fn ifn types m | m <- rpcs ],
    C.UnitList [ local_rpc_fn ifn types m | m <- rpcs ],
    C.Blank,

    C.MultiComment [ "RPC Vtable" ],
    rpc_vtbl ifn rpcs,
    local_rpc_vtbl ifn rpcs,
        
    C.MultiComment [ "RPC init function" ],
    rpc_init_fn ifn rpcs,
        
    C.Blank]

    where
        (types, messagedecls) = Backend.partitionTypesMessages decls
        messages = rpcs_to_msgs messagedecls
        rpcs = [m | m@(RPC _ _ _) <- messagedecls]


msg_handler :: String -> MessageDef -> [TypeDef] -> C.Unit
msg_handler ifname msg@(Message _ mn args _) types = C.FunctionDef C.Static (C.TypeName "void") name [C.Param (C.Ptr C.Void) "arg"] [
    localvar (C.Ptr $ C.Struct $ intf_bind_type ifname)
        intf_bind_var (Just $ C.Variable "arg"),
    localvar (C.TypeName "errval_t") "err" Nothing,
    if null args then C.SBlank else localvar (C.Struct $ msg_argstruct_name RX ifname mn) "arguments" (Just (bindvar `C.DerefField` "rx_union" `C.FieldOf` mn)),
    C.SBlank,

    C.Ex $ C.Assignment errvar $ C.CallInd receive_next [bindvar],
    C.Ex $ C.Call "assert" [C.Call "err_is_ok" [errvar]],
    C.StmtList $ call_message_handler_msgargs ifname mn types args
    ]
    where
        name = msg_handler_fn_name ifname msg
        receive_next = C.DerefField bindvar "receive_next"

msg_handler ifname msg@(RPC mn args a) types = C.FunctionDef C.Static (C.TypeName "void") name [C.Param (C.Ptr C.Void) "arg"] [
    localvar (C.Ptr $ C.Struct $ intf_bind_type ifname)
        intf_bind_var (Just $ C.Variable "arg"),
    localvar (C.TypeName "errval_t") "err" Nothing,
    if null in_args then C.SBlank else localvar (C.Struct $ msg_argstruct_name RX ifname (rpc_call_name mn)) "arguments" (Just (bindvar `C.DerefField` "rx_union" `C.FieldOf` (rpc_call_name mn))),
    localvar (C.TypeName "uint32_t") "token" (Just $ binding_incoming_token),
    C.SBlank,

    C.Ex $ C.Assignment errvar $ C.CallInd receive_next [bindvar],
    C.Ex $ C.Call "assert" [C.Call "err_is_ok" [errvar]],
    C.If (rpc_rx_handler) [
        if null out_args then C.SBlank else localvar (C.Struct $ msg_argstruct_name RX ifname (rpc_resp_name mn)) "result" Nothing,
        C.StmtList $ call_rpc_handler ifname mn types args,
        C.Ex $ C.Call "thread_set_outgoing_token" [C.Binary C.BitwiseAnd (C.Variable "token") (C.Variable "~1" )],
        C.StmtList $ send_response ifname mn types args
        ] [
        C.StmtList $ call_message_handler_rpcargs ifname mn types args
        ]
    ]
    where
        name = msg_handler_fn_name ifname (RPC (rpc_call_name mn) args a)
        receive_next = C.DerefField bindvar "receive_next"
        rpc_rx_handler = C.DerefField bindvar "rpc_rx_vtbl" `C.FieldOf` (rpc_call_name mn)
        in_args = [a | RPCArgIn tr a <- args]
        out_args = [a | RPCArgOut tr a <- args]
        tx_handler = C.DerefField bindvar "tx_vtbl" `C.FieldOf` (rpc_resp_name mn)
        binding_outgoing_token = C.DerefField bindvar "outgoing_token"
        binding_incoming_token = C.DerefField bindvar "incoming_token"

connect_handlers_fn_def :: String -> [MessageDef] -> C.Unit
connect_handlers_fn_def n messages =
    C.FunctionDef C.Static (C.TypeName "errval_t") (connect_handlers_fn_name n)
        [C.Param (C.Ptr $ C.Struct $ intf_bind_type n) intf_bind_var] [
        localvar (C.TypeName "errval_t") "err" Nothing,

        C.StmtList [connect_handler n m | m <- messages],
        C.Return $ C.Variable "SYS_ERR_OK"
    ]

connect_handler :: String -> MessageDef -> C.Stmt
connect_handler n msg@(Message _ mn _ _) = C.StmtList [
    C.Ex $ C.Call "flounder_support_waitset_chanstate_init_persistent" [message_chanstate],
    C.Ex $ C.Assignment errvar $ C.Call "flounder_support_register" [waitset, message_chanstate, closure, C.Variable "false"],
    C.Ex $ C.Assignment (C.DerefField message_chanstate "trigger") $ C.CallInd (bindvar `C.DerefField` "get_receiving_chanstate") [bindvar],
    C.Ex $ C.Call "assert" [C.Call "err_is_ok" [errvar]]
    ]
    where
        waitset = bindvar `C.DerefField` "waitset"
        message_chanstate = C.Binary C.Plus (C.DerefField bindvar "message_chanstate") (C.Variable $ msg_enum_elem_name n mn)
        closure = C.StructConstant "event_closure"
           [("handler", C.Variable $ msg_handler_fn_name n msg), ("arg", bindvar)]

disconnect_handlers_fn_def :: String -> [MessageDef] -> C.Unit
disconnect_handlers_fn_def n messages =
    C.FunctionDef C.Static (C.TypeName "errval_t") (disconnect_handlers_fn_name n)
        [C.Param (C.Ptr $ C.Struct $ intf_bind_type n) intf_bind_var] [
        C.StmtList [disconnect_handler n m | m <- messages],
        C.Return $ C.Variable "SYS_ERR_OK"
    ]

disconnect_handler :: String -> MessageDef -> C.Stmt
disconnect_handler n msg@(Message _ mn _ _) = C.StmtList [
    C.Ex $ C.Call "flounder_support_deregister_chan" [message_chanstate]
    ]
    where
        message_chanstate = C.Binary C.Plus (C.DerefField bindvar "message_chanstate") (C.Variable $ msg_enum_elem_name n mn)

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
        C.Ex $ C.Call (connect_handlers_fn_name ifn) [C.Variable intf_bind_var],
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

rpc_rx_union_elem :: String -> String -> C.Expr
rpc_rx_union_elem mn fn =
   C.FieldOf (C.FieldOf (C.DerefField bindvar "rx_union")
                    (rpc_resp_name mn)) fn

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
        params = [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var]
                 ++ concat [rpc_argdecl2 TX ifn typedefs a | a <- args]
        rpc_progress_var = C.Call "thread_get_rpc_in_progress" []
        async_err_var = C.Call "thread_get_async_error" []
        waitset_var = C.DerefField bindvar "waitset"
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
        errvar_name = "_err"
        errvar = C.Variable errvar_name



local_rpc_fn :: String -> [TypeDef] -> MessageDef -> C.Unit
local_rpc_fn ifn typedefs msg@(RPC n args _) =
    C.FunctionDef C.Static (C.TypeName "errval_t") (local_rpc_fn_name ifn n) params [
        C.Ex $ C.Call "assert" [C.Binary C.NotEquals tx_func (C.Variable "NULL")],
        C.Return $ C.CallInd tx_func (localbindvar:(map C.Variable $ concat $ map mkargs rpc_args))
    ]
    where
        params = [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var]
                 ++ concat [rpc_argdecl2 TX ifn typedefs a | a <- args]
        rpc_args = map rpc_arg args
        tx_func = C.DerefField localbindvar "rpc_rx_vtbl" `C.FieldOf` (rpc_call_name n)
        localbindvar = C.DerefField bindvar "local_binding"
        rpc_arg (RPCArgIn t v) = Arg t v
        rpc_arg (RPCArgOut t v) = Arg t v
        mkargs (Arg _ (Name an)) = [an]
        mkargs (Arg _ (StringArray an _)) = [an]
        mkargs (Arg _ (DynamicArray an al _)) = [an, al]
        (txargs, rxargs) = partition_rpc_args args

rpc_vtbl :: String -> [MessageDef] -> C.Unit
rpc_vtbl ifn ml =
    C.StructDef C.Static (rpc_tx_vtbl_type ifn) (rpc_vtbl_name ifn) fields
    where
        fields = [let mn = msg_name m in (mn, rpc_fn_name ifn mn) | m <- ml]

local_rpc_vtbl :: String -> [MessageDef] -> C.Unit
local_rpc_vtbl ifn ml =
    C.StructDef C.Static (rpc_tx_vtbl_type ifn) (local_rpc_vtbl_name ifn) fields
    where
        fields = [let mn = msg_name m in (mn, local_rpc_fn_name ifn mn) | m <- ml]


arg_names :: MessageArgument -> [String]
arg_names (Arg _ v) = var_names v
    where
        var_names (Name n) = [n]
        var_names (StringArray n _) = [n]
        var_names (DynamicArray n1 n2 _) = [n1, n2]

rpc_init_fn :: String -> [MessageDef] -> C.Unit
rpc_init_fn ifn ml = C.FunctionDef C.NoScope (C.Void)
                            (rpc_init_fn_name ifn) (rpc_init_fn_params ifn) $
    [
     C.SBlank,
     C.SComment "Setup state of RPC client object",
     C.If (C.DerefField bindvar "local_binding") [
        C.Ex $ C.Assignment (C.DerefField bindvar "rpc_tx_vtbl") (C.Variable $ local_rpc_vtbl_name ifn)
     ][
        C.Ex $ C.Assignment (C.DerefField bindvar "rpc_tx_vtbl") (C.Variable $ rpc_vtbl_name ifn)
     ],
     C.SBlank,
     C.SComment "Set RX handlers on binding object for RPCs",
     C.StmtList [C.Ex $ C.Assignment (C.FieldOf (C.DerefField bindvar "rx_vtbl")
                                        (rpc_resp_name mn))
         (C.Variable "NULL") | RPC mn _ _ <- ml],
     C.Ex $ C.Assignment (bindvar `C.DerefField` "error_handler") (C.Variable "NULL"),
     C.SBlank,
     C.ReturnVoid]
    where
        rpc_init_fn_params n = [C.Param (C.Ptr $ C.Struct (intf_bind_type n)) "_binding"]

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
                     local_bind_backend,
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
bind_iref = bindst `C.DerefField` "iref"
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
            C.Call (lmp_bind_fn_name ifn) [binding, bind_iref, cont, C.Variable "b", waitset,
                                           flags,
                                           C.Variable "DEFAULT_LMP_BUF_WORDS"]
    ],
    test_cb_success = C.Call "err_is_ok" [errvar],
    test_cb_try_next = C.Binary C.Or
                        (C.Binary C.Equals (C.Call "err_no" [errvar]) (C.Variable "MON_ERR_IDC_BIND_NOT_SAME_CORE"))
                        (C.Binary C.Equals (C.Call "err_no" [errvar]) (C.Variable "MON_ERR_IDC_BIND_LOCAL")),
    cleanup_bind = [ C.Ex $ C.Call "free" [binding] ]
    }

local_bind_backend ifn (C.Variable cont) =
  BindBackend {
    flounder_backend = "local",
    start_bind = [
        C.If (C.Binary C.Equals (C.Call "err_no" [errvar]) (C.Variable "MON_ERR_IDC_BIND_LOCAL"))
        [
            C.Ex $ C.Assignment binding $ C.Call "malloc" [C.SizeOfT $ C.Struct $ intf_bind_type ifn],
            C.Ex $ C.Call "assert" [C.Binary C.NotEquals binding (C.Variable "NULL")],
            localvar (C.Ptr $ C.Struct "idc_export") "e" $ Nothing,
            localvar (C.Ptr $ C.Void) "ret_binding" $ Nothing,
            C.Ex $ C.Assignment errvar $ C.Call "idc_get_service" [bind_iref, C.AddressOf $ C.Variable "e"],
            C.Ex $ C.CallInd (C.DerefField (C.Variable "e") "local_connect_callback") [C.Variable "e", binding, C.AddressOf $ C.Variable "ret_binding"],
            C.Ex $ C.Call (local_init_fn_name ifn) [binding, waitset, C.Variable "ret_binding"],
            C.Ex $ C.Call cont [C.Variable "b", C.Variable "SYS_ERR_OK", binding]
        ] [
            C.Ex $ C.Call cont [C.Variable "b", errvar, C.Variable "NULL"],
            C.Ex $ C.Assignment errvar (C.Variable "SYS_ERR_OK")
        ]
    ],
    test_cb_success = C.Call "err_is_ok" [errvar],
    test_cb_try_next = C.Variable "true",
    cleanup_bind = []
    }

ump_bind_backend ifn cont =
  BindBackend {
    flounder_backend = "ump",
    start_bind = [
        C.Ex $ C.Assignment binding $
            C.Call "malloc" [C.SizeOfT $ C.Struct $ UMP.bind_type ifn],
        C.Ex $ C.Call "assert" [C.Binary C.NotEquals binding (C.Variable "NULL")],
        C.Ex $ C.Assignment errvar $
            C.Call (UMP.bind_fn_name ifn) [binding, bind_iref, cont, C.Variable "b", waitset,
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
            C.Call (UMP_IPI.bind_fn_name ifn) [binding, bind_iref, cont, C.Variable "b", waitset,
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
                         C.Call (Multihop.m_bind_fn_name ifn) [binding, bind_iref, cont, C.Variable "b", waitset, flags]],
    test_cb_success = C.Call "err_is_ok" [errvar],
    test_cb_try_next = C.Variable "true",
    cleanup_bind = [ C.Ex $ C.Call "free" [binding] ]
    }
