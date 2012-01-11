{- 
   BMP.hs: Flounder stub generator for Beehive message passing.

  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module BMP where

import Data.Bits
import Maybe

import qualified CAbsSyntax as C
import qualified Backend
import Arch
import BackendCommon
import Syntax
import MsgFragments

drvname = "bmp"
arch = fromJust $ parse_arch "beehive"
frag_len_words = 58 - 1 -- determined by kernel transport, with a 1-word header

------------------------------------------------------------------------
-- Language mapping: C identifier names
------------------------------------------------------------------------

-- Name of the binding struct
bind_type :: String -> String
bind_type ifn = ifscope ifn "bmp_binding"

-- Name of the local variable used for the BMP-specific binding type
my_bindvar_name :: String
my_bindvar_name = "b"
my_bindvar = C.Variable my_bindvar_name

-- Name of the bind function
bind_fn_name n = ifscope n "bmp_bind"

-- Name of the bind continuation function
bind_cont_fn_name n = ifscope n "bmp_bind_continuation"

-- Name of the continuation for new monitor bindings
new_monitor_cont_fn_name n = ifscope n "bmp_new_monitor_binding_continuation"

-- Name of the init function
init_fn_name n = ifscope n "bmp_init"

-- Name of the destroy function
destroy_fn_name n = ifscope n "bmp_destroy"

-- Name of the transmit function
tx_fn_name ifn mn = idscope ifn mn "bmp_send"

-- Name of the transmit handler
tx_handler_name ifn = ifscope ifn "bmp_send_handler"

-- Name of the cap transmit handler
tx_cap_handler_name ifn = ifscope ifn "bmp_cap_send_handler"

-- Name of the transmit vtable
tx_vtbl_name ifn = ifscope ifn "bmp_tx_vtbl"

-- Name of the receive handler
rx_handler_name ifn = ifscope ifn "bmp_rx_handler"

-- Name of the cap send/recv handlers
cap_rx_handler_name ifn = ifscope ifn "bmp_cap_rx_handler"
cap_tx_reply_handler_name ifn = ifscope ifn "bmp_cap_tx_reply_handler"

-- Names of the control functions
change_waitset_fn_name ifn = ifscope ifn "bmp_change_waitset"
control_fn_name = generic_control_fn_name drvname

-- Name of the continuation that runs when we get the monitor mutex
monitor_mutex_cont_name ifn = ifscope ifn "bmp_monitor_mutex_cont"

------------------------------------------------------------------------
-- Language mapping: Create the header file for this interconnect driver
------------------------------------------------------------------------

header :: String -> String -> Interface -> String
header infile outfile intf = 
    unlines $ C.pp_unit $ header_file intf (header_body infile intf)
    where
        header_file :: Interface -> [C.Unit] -> C.Unit
        header_file interface@(Interface name _ _) body = 
            let sym = "__" ++ name ++ "_BMP_H"
            in C.IfNDef sym ([ C.Define sym [] "1"] ++ body) []

header_body :: String -> Interface -> [C.Unit]
header_body infile interface@(Interface name descr decls) = [
    intf_preamble infile name descr,
    C.Blank,
    C.MultiComment [ "BMP interconnect driver" ],
    C.Blank,
    C.Include C.Standard "barrelfish/bmp_chan.h",
    C.Include C.Standard "flounder/flounder_support_bmp.h",
    C.Blank,
    bmp_binding_struct name,
    C.Blank,
    init_function_proto name,
    destroy_function_proto name,
    bind_function_proto name,
    connect_handler_proto name,
    rx_handler_proto name,
    C.Blank
    ]

bmp_binding_struct :: String -> C.Unit
bmp_binding_struct ifn = C.StructDecl (bind_type ifn) fields
  where
    fields = [
        C.Param (C.Struct $ intf_bind_type ifn) "b",
        C.Param (C.Struct "flounder_bmp_state") "bmp_state",
        C.ParamBlank,
        -- these are needed for the new monitor continuation to know the bind parameters
        C.ParamComment "bind params for the new monitor continuation",
        C.Param (C.TypeName "iref_t") "iref",
        C.Param (C.TypeName "size_t") "ep_buflen"
        ]

init_function_proto :: String -> C.Unit
init_function_proto n = 
    C.GVarDecl C.Extern C.NonConst 
         (C.Function C.NoScope C.Void params) name Nothing
    where 
      name = init_fn_name n
      params = [C.Param (C.Ptr $ C.Struct (bind_type n)) "b",
                C.Param (C.Ptr $ C.Struct "waitset") "waitset"]

destroy_function_proto :: String -> C.Unit
destroy_function_proto n = 
    C.GVarDecl C.Extern C.NonConst 
         (C.Function C.NoScope C.Void params) name Nothing
    where 
      name = destroy_fn_name n
      params = [C.Param (C.Ptr $ C.Struct (bind_type n)) "b"]

bind_function_proto :: String -> C.Unit
bind_function_proto n = 
    C.GVarDecl C.Extern C.NonConst 
         (C.Function C.NoScope (C.TypeName "errval_t") params) name Nothing
    where 
      name = bind_fn_name n
      params = bind_params n

bind_params n = [ C.Param (C.Ptr $ C.Struct (bind_type n)) "b",
                 C.Param (C.TypeName "iref_t") "iref",
                 C.Param (C.Ptr $ C.TypeName $ intf_bind_cont_type n) intf_cont_var,
                 C.Param (C.Ptr $ C.TypeName "void") "st",
                 C.Param (C.Ptr $ C.Struct "waitset") "waitset",
                 C.Param (C.TypeName "idc_bind_flags_t") "flags",
                 C.Param (C.TypeName "size_t") "ep_buflen" ]

rx_handler_proto ifn = C.GVarDecl C.Extern C.NonConst
    (C.Function C.NoScope C.Void [C.Param (C.Ptr C.Void) "arg"])
    (rx_handler_name ifn) Nothing

connect_handler_proto :: String -> C.Unit
connect_handler_proto ifn = C.GVarDecl C.Extern C.NonConst
    (C.Function C.NoScope (C.TypeName "errval_t") connect_handler_params)
    (drv_connect_handler_name drvname ifn) Nothing

connect_handler_params :: [C.Param]
connect_handler_params
    = [C.Param (C.Ptr $ C.Void) "st",
       C.Param (C.Ptr $ C.Struct "monitor_binding") "monitor_binding",
       C.Param (C.TypeName "uintptr_t") "monitor_id",
       C.Param (C.Struct "capref") "bee_ep",
       C.Param (C.TypeName "size_t") "bee_ep_len"]


------------------------------------------------------------------------
-- Language mapping: Create the stub (implementation) for this interconnect driver
------------------------------------------------------------------------

stub :: String -> String -> Interface -> String
stub infile outfile intf =  unlines $ C.pp_unit $ stub_body infile intf

stub_body :: String -> Interface -> C.Unit
stub_body infile intf@(Interface ifn descr decls) = C.UnitList [
    intf_preamble infile ifn descr,
    C.Blank,
    C.MultiComment [ "Generated Stub for BMP" ],
    C.Blank,

    C.Include C.Standard "string.h",
    C.Include C.Standard "barrelfish/barrelfish.h",
    C.Include C.Standard "barrelfish/monitor_client.h",
    C.Include C.Standard "flounder/flounder_support.h",
    C.Include C.Standard "flounder/flounder_support_bmp.h",
    C.Include C.Standard ("if/" ++ ifn ++ "_defs.h"),
    C.Blank,

    C.MultiComment [ "Send handler function" ],
    tx_handler ifn msg_specs,
    C.Blank,

    C.MultiComment [ "Capability sender function" ],
    tx_cap_handler ifn msg_specs,
    C.Blank,
    
    C.MultiComment [ "Receive handler" ],
    rx_handler ifn types messages msg_specs,
    C.Blank,

    C.MultiComment [ "Cap send/receive handlers" ],
    cap_rx_handler ifn types messages msg_specs,
    cap_tx_reply_handler ifn,
    C.Blank,

    C.UnitList $ if has_caps then
        [C.MultiComment [ "Monitor mutex acquire continuation" ],
        monitor_mutex_cont ifn,
        C.Blank]
    else [],

    C.MultiComment [ "Message sender functions" ],
    C.UnitList [ tx_fn ifn types msg spec | (msg, spec) <- zip messages msg_specs ],
    C.Blank,

    C.MultiComment [ "Send vtable" ],
    tx_vtbl ifn messages,

    C.MultiComment [ "Control functions" ],
    can_send_fn_def drvname ifn,
    register_send_fn_def drvname ifn,
    default_error_handler_fn_def drvname ifn,
    change_waitset_fn_def ifn,
    generic_control_fn_def drvname ifn,
    C.Blank,

    C.MultiComment [ "Functions to initialise/destroy the binding state" ],
    init_fn ifn,
    destroy_fn ifn,
    C.Blank,

    C.MultiComment [ "Bind function" ],
    bind_cont_fn ifn,
    new_monitor_cont_fn ifn,
    bind_fn ifn,
    C.Blank,

    C.MultiComment [ "Connect callback for export" ],
    connect_handler_fn ifn
    ]
    where
        (types, messagedecls) = Backend.partitionTypesMessages decls
        messages = rpcs_to_msgs messagedecls
        msg_specs = [build_msg_spec arch frag_len_words False types m | m <- messages]
        has_caps = [1 | MsgSpec _ _ caps <- msg_specs, caps /= []] /= []

init_fn :: String -> C.Unit
init_fn ifn = C.FunctionDef C.NoScope C.Void (init_fn_name ifn) params [
    C.StmtList common_init,
    C.Ex $ C.Call "bmp_chan_init" [C.AddressOf $ statevar `C.FieldOf` "chan"],
    C.Ex $ C.Call "flounder_stub_bmp_state_init" [C.AddressOf statevar, my_bindvar],
    C.Ex $ C.Assignment (common_field "change_waitset") (C.Variable $ change_waitset_fn_name ifn),
    C.Ex $ C.Assignment (common_field "control") (C.Variable $ control_fn_name ifn) ]
    where
      statevar = C.DerefField my_bindvar "bmp_state"
      params = [C.Param (C.Ptr $ C.Struct (bind_type ifn)) my_bindvar_name,
                C.Param (C.Ptr $ C.Struct "waitset") "waitset"]
      common_field f = my_bindvar `C.DerefField` "b" `C.FieldOf` f
      common_init = binding_struct_init drvname ifn
        (C.DerefField my_bindvar "b")
        (C.Variable "waitset")
        (C.Variable $ tx_vtbl_name ifn)

destroy_fn :: String -> C.Unit
destroy_fn ifn = C.FunctionDef C.NoScope C.Void (destroy_fn_name ifn) params [
    C.StmtList common_destroy,
    C.Ex $ C.Call "bmp_chan_destroy" [C.AddressOf $ statevar `C.FieldOf` "chan"]]
    where
      statevar = C.DerefField my_bindvar "bmp_state"
      params = [C.Param (C.Ptr $ C.Struct (bind_type ifn)) my_bindvar_name]
      common_destroy = binding_struct_destroy ifn (C.DerefField my_bindvar "b")

bind_fn :: String -> C.Unit
bind_fn ifn =
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (bind_fn_name ifn) params [
        localvar (C.TypeName "errval_t") "err" Nothing,
        C.Ex $ C.Call (init_fn_name ifn) [my_bindvar, C.Variable "waitset"],
        C.Ex $ C.Assignment (intf_bind_field "st") (C.Variable "st"),
        C.Ex $ C.Assignment (intf_bind_field "bind_cont") (C.Variable intf_cont_var),
        C.Ex $ C.Assignment (my_bindvar `C.DerefField` "iref") (C.Variable "iref"),
        C.Ex $ C.Assignment (my_bindvar `C.DerefField` "ep_buflen") (C.Variable "ep_buflen"),
        C.SBlank,
        C.SComment "do we need a new monitor binding?",
        C.If (C.Binary C.BitwiseAnd (C.Variable "flags") (C.Variable "IDC_BIND_FLAG_RPC_CAP_TRANSFER"))
            [C.Ex $ C.Assignment errvar $ C.Call "monitor_client_new_binding"
                [C.Variable (new_monitor_cont_fn_name ifn),
                 my_bindvar, C.Variable "waitset",
                 C.Variable "DEFAULT_LMP_BUF_WORDS"],
             C.If (C.Call "err_is_fail" [errvar])
                [C.Ex $ C.Assignment errvar $
                    C.Call "err_push" [errvar, C.Variable "LIB_ERR_MONITOR_CLIENT_BIND"]] []
            ]
            -- no new monitor binding, just bind
            [C.Ex $ C.Assignment errvar $ C.Call "bmp_chan_bind"
                [C.AddressOf $ my_bindvar `C.DerefField` "bmp_state" `C.FieldOf` "chan",
                 C.StructConstant "bmp_bind_continuation"
                    [("handler", C.Variable (bind_cont_fn_name ifn)),
                     ("st", my_bindvar)],
                 C.AddressOf $ intf_bind_field "event_qnode",
                 C.Variable "iref",
                 C.Call "get_monitor_binding" [],
                 C.Variable "ep_buflen"]],
        C.If (C.Call "err_is_fail" [errvar])
            [C.Ex $ C.Call (destroy_fn_name ifn) [my_bindvar]] [],
        C.Return errvar
    ]
    where 
      params = bind_params ifn
      intf_bind_field = C.FieldOf (C.DerefField my_bindvar "b")


new_monitor_cont_fn :: String -> C.Unit
new_monitor_cont_fn ifn =
    C.FunctionDef C.Static C.Void (new_monitor_cont_fn_name ifn) params [
        localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
                intf_bind_var (Just $ C.Variable "st"),
        localvar (C.Ptr $ C.Struct $ bind_type ifn)
                my_bindvar_name (Just $ C.Variable "st"),
        C.SBlank,

        C.If (C.Call "err_is_fail" [errvar])
            [C.Ex $ C.Assignment errvar $
                C.Call "err_push" [errvar, C.Variable "LIB_ERR_MONITOR_CLIENT_BIND"],
             C.Goto "out"] [],
        C.SBlank,

        C.Ex $ C.Assignment (chanvar `C.FieldOf` "monitor_binding") (C.Variable "monitor_binding"),
        C.SComment "start the bind on the new monitor binding",
        C.Ex $ C.Assignment errvar $ C.Call "bmp_chan_bind"
           [C.AddressOf $ chanvar,
            C.StructConstant "bmp_bind_continuation"
               [("handler", C.Variable (bind_cont_fn_name ifn)),
                ("st", my_bindvar)],
            C.AddressOf $ bindvar `C.DerefField` "event_qnode",
            my_bindvar `C.DerefField` "iref",
            C.Variable "monitor_binding",
            my_bindvar `C.DerefField` "ep_buflen"],
        C.SBlank,

        C.Label "out",
        C.If (C.Call "err_is_fail" [errvar])
            [C.Ex $ C.CallInd (bindvar `C.DerefField` "bind_cont")
                [bindvar `C.DerefField` "st", errvar, bindvar],
            C.Ex $ C.Call (destroy_fn_name ifn) [my_bindvar]] []
    ]
    where
        params = [C.Param (C.Ptr C.Void) "st",
                  C.Param (C.TypeName "errval_t") "err",
                  C.Param (C.Ptr $ C.Struct "monitor_binding") "monitor_binding"]
        chanvar = my_bindvar `C.DerefField` "bmp_state" `C.FieldOf` "chan"


bind_cont_fn :: String -> C.Unit
bind_cont_fn ifn =
    C.FunctionDef C.Static C.Void (bind_cont_fn_name ifn) params [
        localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
            intf_bind_var (Just $ C.Variable "st"),
        localvar (C.Ptr $ C.Struct $ bind_type ifn)
            my_bindvar_name (Just $ C.Variable "st"),
        C.SBlank,

        C.If (C.Call "err_is_ok" [errvar])
            [C.StmtList $ setup_cap_handlers ifn,
             C.StmtList $ register_recv ifn]
            [C.Ex $ C.Call (destroy_fn_name ifn) [my_bindvar]],
        C.SBlank,

        C.Ex $ C.CallInd (intf_var `C.FieldOf` "bind_cont")
            [intf_var `C.FieldOf` "st", errvar, C.AddressOf intf_var]]
    where
      params = [C.Param (C.Ptr C.Void) "st",
                C.Param (C.TypeName "errval_t") "err",
                C.Param (C.Ptr $ C.Struct "bmp_chan") "chan"]
      intf_var = C.DerefField my_bindvar "b"
      errvar = C.Variable "err"
      chanaddr = C.Variable "chan"

connect_handler_fn :: String -> C.Unit
connect_handler_fn ifn = C.FunctionDef C.NoScope (C.TypeName "errval_t")
    (drv_connect_handler_name "bmp" ifn) connect_handler_params [
    localvar (C.Ptr $ C.Struct $ export_type ifn) "e" $ Just $ C.Variable "st",
    localvar (C.TypeName "errval_t") "err" Nothing,
    C.SBlank,
    C.SComment "allocate storage for binding",
    localvar (C.Ptr $ C.Struct $ bind_type ifn) my_bindvar_name
        $ Just $ C.Call "malloc" [C.SizeOfT $ C.Struct $ bind_type ifn],
    C.If (C.Binary C.Equals my_bindvar (C.Variable "NULL"))
        [C.Return $ C.Variable "LIB_ERR_MALLOC_FAIL"] [],
    C.SBlank,
    
    localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
         intf_bind_var (Just $ C.AddressOf $ my_bindvar `C.DerefField` "b"),
    C.Ex $ C.Call (init_fn_name ifn) [my_bindvar,
                                          exportvar `C.DerefField` "waitset"],
    C.SBlank,

    C.SComment "run user's connect handler",
    C.Ex $ C.Assignment errvar $ C.CallInd (C.DerefField exportvar "connect_cb")
                       [C.DerefField exportvar "st", bindvar],
    C.If (C.Call "err_is_fail" [errvar])
        [C.SComment "connection refused",
         C.Ex $ C.Call (destroy_fn_name ifn) [my_bindvar],
         C.Return $ errvar] [],
    C.SBlank,

    C.SComment "accept the connection and setup the channel",
    C.SComment "FIXME: user policy needed to decide on the size of the message buffer?",
    C.Ex $ C.Assignment errvar $ C.Call "bmp_chan_accept"
                                [chanaddr,
                                 C.Variable "monitor_id", C.Variable "bee_ep",
                                 C.Variable "bee_ep_len", C.Variable "bee_ep_len"],
    C.If (C.Call "err_is_fail" [errvar])
        [C.Ex $ C.Assignment errvar $ C.Call "err_push"
                    [errvar, C.Variable "LIB_ERR_BMP_CHAN_ACCEPT"],
         report_user_err errvar,
         C.Return $ errvar] [],
    C.SBlank,

    C.StmtList $ register_recv ifn,
    C.StmtList $ setup_cap_handlers ifn,
    C.SBlank,

    C.SComment "send back bind reply",
    C.Ex $ C.Call "bmp_chan_send_bind_reply"
         [C.Variable "monitor_binding", chanaddr, C.Variable "SYS_ERR_OK",
          C.Variable "monitor_id"],
    C.Return $ C.Variable "SYS_ERR_OK"]
    where
        exportvar = C.Variable "e"
        chanaddr = C.AddressOf $ chanvar
        chanvar = C.DerefField my_bindvar "bmp_state" `C.FieldOf` "chan"

change_waitset_fn_def :: String -> C.Unit
change_waitset_fn_def ifn = 
    C.FunctionDef C.Static (C.TypeName "errval_t") (change_waitset_fn_name ifn) params [
        localvar (C.Ptr $ C.Struct $ bind_type ifn)
            my_bindvar_name (Just $ C.Cast (C.Ptr C.Void) bindvar),
        localvar (C.TypeName "errval_t") "err" Nothing,
        C.SBlank,

        C.SComment "change waitset on private monitor binding if we have one",
        C.If (C.Binary C.NotEquals (chanvar `C.FieldOf` "monitor_binding") (C.Call "get_monitor_binding" []))
            [C.Ex $ C.Assignment errvar $
                C.Call "flounder_support_change_monitor_waitset"
                    [chanvar `C.FieldOf` "monitor_binding", C.Variable "ws"],
             C.If (C.Call "err_is_fail" [errvar])
                [C.Return $
                    C.Call "err_push" [errvar, C.Variable "FLOUNDER_ERR_CHANGE_MONITOR_WAITSET"]]
                []
            ] [],
        C.SBlank,

        C.SComment "change waitset on binding",
        C.Ex $ C.Assignment
            (bindvar `C.DerefField` "waitset")
            (C.Variable "ws"),
        C.SBlank,

        C.SComment "re-register for receive (if previously registered)",
        C.Ex $ C.Assignment errvar $ C.Call "bmp_chan_deregister_recv" [chanaddr],
        C.If (C.Binary C.And
                (C.Call "err_is_fail" [errvar])
                (C.Binary C.NotEquals (C.Call "err_no" [errvar])
                                    (C.Variable "LIB_ERR_CHAN_NOT_REGISTERED")))
            [C.Return $
               C.Call "err_push" [errvar, C.Variable "LIB_ERR_CHAN_DEREGISTER_RECV"]]
            [],
        C.If (C.Call "err_is_ok" [errvar]) [
            C.Ex $ C.Assignment errvar $ C.Call "bmp_chan_register_recv"
                [chanaddr, C.Variable "ws",
                 C.StructConstant "event_closure"
                    [("handler", C.Variable $ rx_handler_name ifn),
                     ("arg", bindvar)]],
            C.If (C.Call "err_is_fail" [errvar])
                [C.Return $
                    C.Call "err_push" [errvar, C.Variable "LIB_ERR_CHAN_REGISTER_RECV"]]
                []
            ] [],
        C.Return $ C.Variable "SYS_ERR_OK"
    ]
    where
        chanvar = my_bindvar `C.DerefField` "bmp_state" `C.FieldOf` "chan"
        chanaddr = C.AddressOf $ chanvar
        params = [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var,
                  C.Param (C.Ptr $ C.Struct "waitset") "ws"]


handler_preamble :: String -> C.Stmt
handler_preamble ifn = C.StmtList
    [C.SComment "Get the binding state from our argument pointer",
     localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
         intf_bind_var (Just $ C.Variable "arg"),
     localvar (C.Ptr $ C.Struct $ bind_type ifn)
         my_bindvar_name (Just $ C.Variable "arg"),
     localvar (C.TypeName "errval_t") "err" Nothing,
     C.Ex $ C.Assignment errvar (C.Variable "SYS_ERR_OK"),
     C.SBlank]

tx_cap_handler :: String -> [MsgSpec] -> C.Unit
tx_cap_handler ifn msgspecs = 
    C.FunctionDef C.Static C.Void (tx_cap_handler_name ifn) [C.Param (C.Ptr C.Void) "arg"] [
        handler_preamble ifn,

        C.Ex $ C.Call "assert" [capst `C.FieldOf` "rx_cap_ack"],
        C.Ex $ C.Call "assert" [capst `C.FieldOf` "monitor_mutex_held"],
        C.SBlank,

        C.SComment "Switch on current outgoing message",
        C.Switch (C.DerefField bindvar "tx_msgnum") cases
            [C.Ex $ C.Call "assert"
                    [C.Unary C.Not $ C.StringConstant "invalid message number"],
             report_user_err (C.Variable "FLOUNDER_ERR_INVALID_STATE")]
    ]
    where
        bmpst = C.DerefField my_bindvar "bmp_state"
        capst = bmpst `C.FieldOf` "capst"
        cases = [C.Case (C.Variable $ msg_enum_elem_name ifn mn)
                        (tx_cap_handler_case ifn mn (length frags) caps)
                 | MsgSpec mn frags caps <- msgspecs, caps /= []]

tx_cap_handler_case :: String -> String -> Int -> [CapFieldTransfer] -> [C.Stmt]
tx_cap_handler_case ifn mn nfrags caps = [
    C.SComment "Switch on current outgoing cap",
    C.Switch (capst `C.FieldOf` "tx_capnum") cases
            [C.Ex $ C.Call "assert"
                    [C.Unary C.Not $ C.StringConstant "invalid cap number"],
             report_user_err (C.Variable "FLOUNDER_ERR_INVALID_STATE")],
    C.Break]
    where
        give_away_val :: CapTransferMode -> C.Expr
        give_away_val Copied = C.Variable "false"
        give_away_val GiveAway = C.Variable "true"

        bmpst = C.DerefField my_bindvar "bmp_state"
        capst = bmpst `C.FieldOf` "capst"
        chan = bmpst `C.FieldOf` "chan"
        cases = [C.Case (C.NumConstant $ toInteger i) $ subcase cap i
                 | (cap, i) <- zip caps [0..]] ++ 
                [C.Case (C.NumConstant $ toInteger $ length caps) last_case]

        last_case = [
            -- release our lock on the monitor binding
            C.Ex $ C.Call "flounder_support_monitor_mutex_unlock"
                    [chan `C.FieldOf` "monitor_binding"],

            -- if we've sent the last cap, and we've sent all the other fragments, we're done
            C.If (C.Binary C.Equals tx_msgfrag_field
                                (C.NumConstant $ toInteger nfrags))
                    finished_send [],
            C.Break]
        tx_msgfrag_field = C.DerefField bindvar "tx_msg_fragment"

        subcase :: CapFieldTransfer -> Int -> [C.Stmt]
        subcase (CapFieldTransfer tm cap) ncap = [
            C.Ex $ C.Assignment errvar $ C.Call "flounder_stub_send_cap"
                [C.AddressOf $ capst, chan `C.FieldOf` "monitor_binding",
                 chan `C.FieldOf` "monitor_id", argfield_expr TX mn cap,
                 give_away_val tm, C.Variable $ tx_cap_handler_name ifn],
            C.If (C.Call "err_is_fail" [errvar])
                [report_user_tx_err errvar, C.Break] [],
            C.Break]

tx_handler :: String -> [MsgSpec] -> C.Unit
tx_handler ifn msgs =
    C.FunctionDef C.Static C.Void (tx_handler_name ifn) [C.Param (C.Ptr C.Void) "arg"] [
        handler_preamble ifn,

        C.SComment "do we need to (and can we) send a cap ack?",
        C.If (C.Binary C.And
                    (capst `C.FieldOf` "tx_cap_ack")
                    (C.Call "flounder_stub_bmp_can_send" [C.AddressOf bmpst,
                                                          C.NumConstant 1]))
            [C.Ex $ C.Assignment errvar $
                    C.Call "flounder_stub_bmp_send_cap_ack" [C.AddressOf bmpst],
             C.If (C.Call "err_is_ok" [errvar])
                [C.Ex $ C.Assignment (capst `C.FieldOf` "tx_cap_ack") (C.Variable "false")]
                [C.Goto "out"]] [],
        C.SBlank,

        C.SComment "Switch on current outgoing message number",
        C.Switch (C.DerefField bindvar "tx_msgnum") msgcases
            [C.Ex $ C.Call "assert" [C.Unary C.Not $ C.StringConstant "invalid msgnum"],
                report_user_err (C.Variable "FLOUNDER_ERR_INVALID_STATE")],
        C.SBlank,

        C.SComment "send a forced ack if the incoming channel is still full",
        C.If (C.Binary C.And (C.Call "err_is_ok" [errvar])
                                (C.Call "flounder_stub_bmp_needs_ack" [C.AddressOf $ bmpst]))
            [C.Ex $ C.Assignment errvar $ C.Call "flounder_stub_bmp_send_ack" [C.AddressOf $ bmpst]] [],
        C.SBlank,

        C.Label "out",
        C.If (C.Call "err_is_fail" [errvar])
            [C.If (C.Binary C.Equals (C.Call "err_no" [errvar]) (C.Variable "SYS_ERR_BMP_TX_BUSY"))
                -- transient error: re-register to send
                [C.Ex $ C.Assignment errvar
                        (C.Call "bmp_chan_register_send" [
                            C.AddressOf $ bmpst `C.FieldOf` "chan",
                            C.DerefField bindvar "waitset",
                            C.StructConstant "event_closure" [
                                ("handler", C.Variable $ tx_handler_name ifn),
                                ("arg", C.Variable "arg")]]),
                 C.Ex $ C.Call "assert" [C.Call "err_is_ok" [errvar]]]
                [C.If (C.Binary C.Equals (C.Call "err_no" [errvar]) (C.Variable "FLOUNDER_ERR_BUF_SEND_MORE"))
                    -- transient error: flow control, need to wait for rx before we can send more
                    [C.SComment "do nothing; waiting to receive an ack"]
                    -- permanent errors
                    [C.SComment "Report error to user",
                     report_user_tx_err errvar]]] []
    ]
    where
        inc_fragnum = C.Ex $ C.PostInc $ C.DerefField bindvar "tx_msg_fragment"
        tx_msgnum_field = C.DerefField bindvar "tx_msgnum"
        bmpst = C.DerefField my_bindvar "bmp_state"
        capst = bmpst `C.FieldOf` "capst"

        msgcases = (C.Case (C.NumConstant 0) [C.Break]):
                   [C.Case (C.Variable $ msg_enum_elem_name ifn mn)
                    $ gen_msgcase mn msgfrags caps
                    | MsgSpec mn msgfrags caps <- msgs]
    
        gen_msgcase mn msgfrags caps = [
            C.SComment "Switch on current outgoing message fragment",
            C.Switch (C.DerefField bindvar "tx_msg_fragment") fragcases
                [C.Ex $ C.Call "assert" [C.Unary C.Not $ C.StringConstant "invalid fragment"],
                    report_user_err (C.Variable "FLOUNDER_ERR_INVALID_STATE")],
            C.Break]

            where
            fragcases = [C.Case (C.NumConstant $ toInteger i)
                         $ (tx_handler_case ifn mn frag) ++ gen_epilog i
                         | (frag, i) <- zip msgfrags [0..]]
                     ++ [C.Case (C.NumConstant $ toInteger $ length msgfrags)
                         $ last_frag]

            last_frag = [
                C.SComment "we've sent all the fragments, we must just be waiting for caps",
                C.Ex $ C.Call "assert"
                    [C.Binary C.LessThanEq (capst `C.FieldOf` "tx_capnum")
                            (C.NumConstant $ toInteger $ length caps)],
                C.Break]

            -- generate the code that runs after the send succeeds
            gen_epilog i =
                [C.If (C.Call "err_is_fail" [errvar]) [C.Break] [], inc_fragnum] ++
                if (i + 1 == length msgfrags) then
                    -- if the last fragment succeeds, and we've sent all the caps, we're done
                    -- otherwise we'll need to wait to finish sending the caps
                    ((if caps /= [] then
                        [C.If (C.Binary C.Equals (capst `C.FieldOf` "tx_capnum")
                            (C.NumConstant $ toInteger $ length caps + 1))
                            finished_send []]
                     else finished_send)
                     ++ [C.ReturnVoid])
                else -- more fragments to go
                    [C.SComment "fall through to next fragment"]


tx_handler_case :: String -> String -> MsgFragment -> [C.Stmt]
tx_handler_case ifn mn (MsgFragment words) = [
    C.SComment "check if we can send another message",
    C.If (C.Unary C.Not $ C.Call "flounder_stub_bmp_can_send"
                                [stateaddr, C.NumConstant $ toInteger msglen])
        [C.Break] [],
    C.SBlank,

    C.SComment "send the next fragment",
    localvar (C.Array (toInteger msglen) (C.TypeName "uintptr_t")) "msg"
        (Just $ C.ArrayConstant $ header:payload),
     C.Ex $ C.Assignment errvar $ C.Call "flounder_stub_bmp_send"
        [stateaddr, C.Variable "msg", C.NumConstant $ toInteger msglen]
    ] where
        msglen = length words + 1 -- for header
        header = C.Call "flounder_stub_bmp_mkheader" [stateaddr, msgnum_arg]
        payload = map (fragment_word_to_expr arch ifn mn) words
        msgnum_arg = C.Variable $ msg_enum_elem_name ifn mn
        statevar = C.DerefField my_bindvar "bmp_state"
        stateaddr = C.AddressOf statevar
        chanaddr = C.AddressOf $ C.FieldOf statevar "chan"

tx_handler_case ifn mn (OverflowFragment (StringFragment af)) =
    [C.Ex $ C.Assignment errvar (C.Call "flounder_stub_bmp_send_string" args)]
    where
        args = [chan_arg, msgnum_arg, string_arg, pos_arg, len_arg]
        chan_arg = C.AddressOf $ C.DerefField my_bindvar "bmp_state"
        msgnum_arg = C.Variable $ msg_enum_elem_name ifn mn
        string_arg = argfield_expr TX mn af
        pos_arg = C.AddressOf $ C.DerefField bindvar "tx_str_pos"
        len_arg = C.AddressOf $ C.DerefField bindvar "tx_str_len"

tx_handler_case ifn mn (OverflowFragment (BufferFragment _ afn afl)) =
    [C.Ex $ C.Assignment errvar (C.Call "flounder_stub_bmp_send_buf" args)]
    where
        args = [chan_arg, msgnum_arg, buf_arg, len_arg, pos_arg]
        chan_arg = C.AddressOf $ C.DerefField my_bindvar "bmp_state"
        msgnum_arg = C.Variable $ msg_enum_elem_name ifn mn
        buf_arg = argfield_expr TX mn afn
        len_arg = argfield_expr TX mn afl
        pos_arg = C.AddressOf $ C.DerefField bindvar "tx_str_pos"


tx_fn :: String -> [TypeDef] -> MessageDef -> MsgSpec -> C.Unit
tx_fn ifn typedefs msg@(Message _ n args _) (MsgSpec _ _ caps) =
    C.FunctionDef C.Static (C.TypeName "errval_t") (tx_fn_name ifn n) params body
    where
        params = [binding_param ifn, cont_param] ++ (
                    concat [ msg_argdecl TX ifn a | a <- args ])
        cont_param = C.Param (C.Struct "event_closure") intf_cont_var
        body = [
            C.SComment "check that we can accept an outgoing message",
            C.If (C.Binary C.NotEquals tx_msgnum_field (C.NumConstant 0))
                [C.Return $ C.Variable "FLOUNDER_ERR_TX_BUSY"] [],
            C.SBlank,
            C.SComment "register send continuation",
            C.StmtList $ register_txcont (C.Variable intf_cont_var),
            C.SBlank,
            C.SComment "store message number and arguments",
            C.Ex $ C.Assignment tx_msgnum_field (C.Variable $ msg_enum_elem_name ifn n),
            C.Ex $ C.Assignment tx_msgfrag_field (C.NumConstant 0),
            C.StmtList [ tx_arg_assignment ifn typedefs n a | a <- args ],
            C.StmtList $ start_send drvname ifn n args,
            C.SBlank,
            -- if this message has caps, we need to acquire the monitor binding mutex
            C.StmtList $ if caps /= [] then
                [C.SComment "init cap send state",
                 C.Ex $ C.Assignment (capst `C.FieldOf` "tx_capnum") (C.NumConstant 0),
                 C.Ex $ C.Assignment (capst `C.FieldOf` "rx_cap_ack") (C.Variable "false"),
                 C.Ex $ C.Assignment (capst `C.FieldOf` "monitor_mutex_held") (C.Variable "false"),
                 C.SBlank,

                 C.SComment "wait to acquire the monitor binding mutex",
                 C.Ex $ C.Call "flounder_support_monitor_mutex_enqueue"
                    [bmpst `C.FieldOf` "chan" `C.FieldOf` "monitor_binding",
                     C.AddressOf $ bindvar `C.DerefField` "event_qnode",
                     C.StructConstant "event_closure" [
                        ("handler", C.Variable $ monitor_mutex_cont_name ifn),
                        ("arg", bindvar)]],
                 C.SBlank]
                else [],
            C.SComment "try to send!",
            C.Ex $ C.Call (tx_handler_name ifn) [bindvar],
            C.SBlank,
            C.Return $ C.Variable "SYS_ERR_OK"
            ]
        bmpvar = C.Cast (C.Ptr $ C.Struct $ bind_type ifn) bindvar
        bmpst = C.DerefField bmpvar "bmp_state"
        capst = bmpst `C.FieldOf` "capst"
        tx_msgnum_field = C.DerefField bindvar "tx_msgnum"
        tx_msgfrag_field = C.DerefField bindvar "tx_msg_fragment"

tx_vtbl :: String -> [MessageDef] -> C.Unit
tx_vtbl ifn ml =
    C.StructDef C.Static (intf_vtbl_type ifn TX) (tx_vtbl_name ifn) fields
    where
        fields = [let mn = msg_name m in (mn, tx_fn_name ifn mn) | m <- ml]

monitor_mutex_cont :: String -> C.Unit
monitor_mutex_cont ifn =
    C.FunctionDef C.Static C.Void (monitor_mutex_cont_name ifn) [C.Param (C.Ptr C.Void) "arg"] [
        localvar (C.Ptr $ C.Struct $ bind_type ifn) my_bindvar_name (Just $ C.Variable "arg"),
        C.Ex $ C.Call "assert" [C.Unary C.Not (capst `C.FieldOf` "monitor_mutex_held")],
        C.Ex $ C.Assignment (capst `C.FieldOf` "monitor_mutex_held") (C.Variable "true"),
        C.If (capst `C.FieldOf` "rx_cap_ack")
            [C.Ex $ C.Call (tx_cap_handler_name ifn) [my_bindvar]] []
    ]
    where
        statevar = C.DerefField my_bindvar "bmp_state"
        capst = statevar `C.FieldOf` "capst"

rx_handler :: String -> [TypeDef] -> [MessageDef] -> [MsgSpec] -> C.Unit
rx_handler ifn typedefs msgdefs msgs =
    C.FunctionDef C.NoScope C.Void (rx_handler_name ifn) [C.Param (C.Ptr C.Void) "arg"] [
        handler_preamble ifn,

        -- local variables
        localvar (C.Struct "bmp_recv_msg") "msg" (Just $ C.Variable "BMP_RECV_MSG_INIT"),
        localvar (C.TypeName "int") "msgnum" Nothing,
        C.SBlank,

        C.While (C.Variable "true") loopbody,
        C.SBlank,

        C.Label "out",
        C.StmtList $ register_recv ifn,
        C.SBlank,

        -- XXX: hack around the AST to get an attribute on this label, which may not be used
        C.Label "out_no_reregister",
        C.Ex $ C.Variable "__attribute__((unused))",

        C.SComment "run our send process, if we need to",
        C.If (C.Binary C.Or
                    (C.Binary C.Or
                        (capst `C.FieldOf` "tx_cap_ack")
                        (C.Binary C.NotEquals
                            (bindvar `C.DerefField` "tx_msgnum")
                            (C.NumConstant 0)))
                    (C.Call "flounder_stub_bmp_needs_ack" [stateaddr]))
            [C.Ex $ C.Call (tx_handler_name ifn) [my_bindvar]] []
    ] where
        loopbody = [
            C.SComment "try to retrieve a message from the channel",
            C.Ex $ C.Assignment errvar
                    $ C.Call "bmp_chan_recv" [chanaddr,
                                C.AddressOf $ C.Variable "msg"],

            C.SComment "check if we succeeded",
            C.If (C.Call "err_is_fail" [errvar])
                -- if err_is_fail, check err_no
                [C.If (C.Binary C.Equals (C.Call "err_no" [errvar]) (C.Variable "LIB_ERR_NO_LMP_MSG"))
                    [C.SComment "no message",
                     C.Break]
                    [C.SComment "real error",
                     report_user_err $ C.Call "err_push" [errvar, C.Variable "LIB_ERR_BMP_CHAN_RECV"],
                     C.ReturnVoid]
                ] [],
            C.SBlank,

            C.SComment "check message length",
            C.If (C.Binary C.Equals msglen (C.NumConstant 0)) [
                report_user_err $ C.Variable "FLOUNDER_ERR_RX_EMPTY_MSG",
                C.Break] [],
            C.SBlank,

            C.SComment "process control word",
            C.Ex $ C.Assignment (C.Variable "msgnum")
                 $ C.Call "flounder_stub_bmp_process_header"
                    [stateaddr, C.SubscriptOf msgwords $ C.NumConstant 0, msglen],
            C.SBlank,

            C.SComment "is this a dummy message (ACK)?",
            C.If (C.Binary C.Equals (C.Variable "msgnum") (C.Variable "FL_BMP_ACK"))
                [C.Continue] [],
            C.SBlank,

            C.SComment "is this a cap ack for a pending tx message",
            C.If (C.Binary C.Equals (C.Variable "msgnum") (C.Variable "FL_BMP_CAP_ACK"))
                [C.Ex $ C.Call "assert" [C.Unary C.Not (capst `C.FieldOf` "rx_cap_ack")],
                 C.Ex $ C.Assignment (capst `C.FieldOf` "rx_cap_ack") (C.Variable "true"),
                 C.If (capst `C.FieldOf` "monitor_mutex_held")
                    [C.Ex $ C.Call (tx_cap_handler_name ifn) [my_bindvar]] [],
                 C.Continue]
                [],
            C.SBlank,

            C.SComment "is this the start of a new message?",
            C.If (C.Binary C.Equals rx_msgnum_field (C.NumConstant 0)) [
                C.Ex $ C.Assignment rx_msgnum_field (C.Variable "msgnum"),
                C.Ex $ C.Assignment rx_msgfrag_field (C.NumConstant 0)
            ] [],
            C.SBlank,

            C.SComment "switch on message number and fragment number",
            C.Switch rx_msgnum_field msgnum_cases bad_msgnum]

        statevar = C.DerefField my_bindvar "bmp_state"
        capst = statevar `C.FieldOf` "capst"
        stateaddr = C.AddressOf statevar
        chanaddr = C.AddressOf $ statevar `C.FieldOf` "chan"
        msglen = C.Variable "msg" `C.FieldOf` "buf" `C.FieldOf` "msglen"
        msgwords = C.Variable "msg" `C.FieldOf` "words"
        rx_msgnum_field = C.DerefField bindvar "rx_msgnum"
        rx_msgfrag_field = C.DerefField bindvar "rx_msg_fragment"

        msgnum_cases = [C.Case (C.Variable $ msg_enum_elem_name ifn mn) (msgnum_case msgdef msg)
                            | (msgdef, msg@(MsgSpec mn _ _)) <- zip msgdefs msgs]

        msgnum_case msgdef@(Message _ _ msgargs _) (MsgSpec mn frags caps) = [
            C.Switch rx_msgfrag_field
                [C.Case (C.NumConstant $ toInteger i) $
                 (if i == 0 then
                    -- first fragment of a message
                    start_recv drvname ifn typedefs mn msgargs ++ 
                    (if caps /= [] then [
                        -- + with caps received
                        C.Ex $ C.Assignment
                            (capst `C.FieldOf` "tx_cap_ack") (C.Variable "true"),
                        C.Ex $ C.Assignment
                            (capst `C.FieldOf` "rx_capnum") (C.NumConstant 0)
                        ] else []) 
                       else []) ++
                    (msgfrag_case msgdef frag caps (i == length frags - 1))
                 | (frag, i) <- zip frags [0..] ]
                bad_msgfrag,
            C.Break]

        bad_msgnum = [report_user_err $ C.Variable "FLOUNDER_ERR_RX_INVALID_MSGNUM",
                      C.Goto "out"]

        bad_msgfrag = [report_user_err $ C.Variable "FLOUNDER_ERR_INVALID_STATE",
                      C.Goto "out"]

        msgfrag_case :: MessageDef -> MsgFragment -> [CapFieldTransfer] -> Bool -> [C.Stmt]
        msgfrag_case msg@(Message _ mn _ _) (MsgFragment wl) caps isLast = [
            C.StmtList $ concat [store_arg_frags arch ifn mn msgwords word 0 afl
                                 | (afl, word) <- zip wl [1..]],
            C.SBlank,
            C.StmtList $ msgfrag_case_prolog msg caps isLast,
            C.Break]

        msgfrag_case msg@(Message _ mn _ _) (OverflowFragment ofrag) caps isLast = [
            C.Ex $ C.Assignment errvar (C.Call func args),
            C.If (C.Call "err_is_ok" [errvar])
                (msgfrag_case_prolog msg caps isLast)
                -- error from receive code, check if it's permanent
                [C.If (C.Binary C.NotEquals
                        (C.Call "err_no" [errvar])
                        (C.Variable "FLOUNDER_ERR_BUF_RECV_MORE"))
                    [report_user_err errvar] -- real error
                    [] -- will receive more next time
                ],
            C.Break]
            where
                msg_arg = C.AddressOf $ C.Variable "msg"
                pos_arg = C.AddressOf $ C.DerefField bindvar "rx_str_pos"
                (func, args) = case ofrag of
                    (BufferFragment _ afn afl) -> ("flounder_stub_bmp_recv_buf",
                                                   [msg_arg, buf_arg, len_arg, pos_arg])
                      where 
                        buf_arg = C.Cast (C.Ptr $ C.Ptr C.Void)
                                        $ C.AddressOf $ argfield_expr RX mn afn
                        len_arg = C.AddressOf $ argfield_expr RX mn afl
                    (StringFragment af) -> ("flounder_stub_bmp_recv_string",
                                            [msg_arg, string_arg, pos_arg, len_arg])
                      where
                        string_arg = C.AddressOf $ argfield_expr RX mn af
                        len_arg = C.AddressOf $ C.DerefField bindvar "rx_str_len"

        msgfrag_case_prolog :: MessageDef -> [CapFieldTransfer] -> Bool -> [C.Stmt]
        -- intermediate fragment
        msgfrag_case_prolog _ _ False = [rx_fragment_increment]

        -- last fragment: call handler and zero message number
        -- if we're expecting any caps, only do so if we've received them all
        msgfrag_case_prolog (Message _ mn msgargs _) caps True
            | caps == [] = finished_recv drvname ifn typedefs mn msgargs
            | otherwise = [
                rx_fragment_increment,
                C.If (C.Binary C.Equals
                                    (capst `C.FieldOf` "rx_capnum")
                                    (C.NumConstant $ toInteger $ length caps))
                    (finished_recv drvname ifn typedefs mn msgargs)
                    [C.SComment "don't process anything else until we're done",
                     C.Goto "out_no_reregister"]]

        rx_fragment_increment
            = C.Ex $ C.PostInc $ C.DerefField bindvar "rx_msg_fragment"

cap_tx_reply_handler :: String -> C.Unit
cap_tx_reply_handler ifn
    = C.FunctionDef C.Static C.Void (cap_tx_reply_handler_name ifn)
        [C.Param (C.Ptr C.Void) "st",
         C.Param (C.TypeName "uint32_t") "capid",
         C.Param (C.TypeName "errval_t") "err"]
        [
        localvar (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var (Just $ C.Variable "st"),
        C.If (C.Call "err_is_fail" [errvar])
            [C.Ex $ C.Call "DEBUG_ERR" [errvar,
                        C.StringConstant "monitor refused to accept cap for BMP send"],
             report_user_tx_err $
                    C.Call "err_push" [errvar, C.Variable "LIB_ERR_MONITOR_CAP_SEND"]] []
        ]

cap_rx_handler :: String -> [TypeDef] -> [MessageDef] -> [MsgSpec] -> C.Unit
cap_rx_handler ifn typedefs msgdefs msgspecs
    = C.FunctionDef C.Static C.Void (cap_rx_handler_name ifn)
        [C.Param (C.Ptr C.Void) "arg",
         C.Param (C.Struct "capref") "cap",
         C.Param (C.TypeName "uint32_t") "capid"]
        [
        handler_preamble ifn,

        C.Ex $ C.Call "assert" [C.Binary C.Equals
                                       (C.Variable "capid")
                                       (capst `C.FieldOf` "rx_capnum")],
        C.SBlank,

        C.SComment "Switch on current incoming message",
        C.Switch (C.DerefField bindvar "rx_msgnum") cases
            [C.Ex $ C.Call "assert"
                    [C.Unary C.Not $ C.StringConstant "invalid message number"],
             report_user_err (C.Variable "FLOUNDER_ERR_INVALID_STATE")]
    ]
    where
        bmpst = C.DerefField my_bindvar "bmp_state"
        capst = bmpst `C.FieldOf` "capst"
        cases = [C.Case (C.Variable $ msg_enum_elem_name ifn mn)
                        (cap_rx_handler_case ifn typedefs mn msgdef (length frags) caps)
                 | (MsgSpec mn frags caps, msgdef) <- zip msgspecs msgdefs, caps /= []]

cap_rx_handler_case :: String -> [TypeDef] -> String -> MessageDef -> Int -> [CapFieldTransfer] -> [C.Stmt]
cap_rx_handler_case ifn typedefs mn (Message _ _ msgargs _) nfrags caps = [
    C.SComment "Switch on current incoming cap",
    C.Switch (C.PostInc $ capst `C.FieldOf` "rx_capnum") cases
            [C.Ex $ C.Call "assert"
                    [C.Unary C.Not $ C.StringConstant "invalid cap number"],
             report_user_err (C.Variable "FLOUNDER_ERR_INVALID_STATE")],
    C.Break]
    where
        bmpst = C.DerefField my_bindvar "bmp_state"
        capst = bmpst `C.FieldOf` "capst"
        cases = [C.Case (C.NumConstant $ toInteger i) $ subcase cap i
                 | (cap, i) <- zip caps [0..]]

        subcase :: CapFieldTransfer -> Int -> [C.Stmt]
        subcase (CapFieldTransfer _ cap) ncap = [
            C.Ex $ C.Assignment (argfield_expr RX mn cap) (C.Variable "cap"),
            if is_last then
                -- if this was the last cap, and we've received all the other fragments, we're done
                C.If (C.Binary C.Equals rx_msgfrag_field (C.NumConstant $ toInteger nfrags))
                    [
                        C.StmtList $ finished_recv drvname ifn typedefs mn msgargs,
                        C.StmtList $ register_recv ifn
                    ] []
                else C.StmtList [],
            C.Break]
            where
                rx_msgfrag_field = C.DerefField bindvar "rx_msg_fragment"
                is_last = (ncap + 1 == length caps)

-- generate the code to register for receive notification
register_recv :: String -> [C.Stmt]
register_recv ifn = [
    C.SComment "register for receive notification",
    C.Ex $ C.Assignment errvar $ C.Call "bmp_chan_register_recv"
        [C.AddressOf $ my_bindvar `C.DerefField` "bmp_state" `C.FieldOf` "chan",
         bindvar `C.DerefField` "waitset",
         C.StructConstant "event_closure"
            [("handler", C.Variable $ rx_handler_name ifn),
             ("arg", my_bindvar)]],
    C.If (C.Call "err_is_fail" [errvar])
        [report_user_err $ C.Call "err_push" [errvar, C.Variable "LIB_ERR_CHAN_REGISTER_RECV"]]
        [] ]

-- generate the code to set cap rx/tx handlers
setup_cap_handlers :: String -> [C.Stmt]
setup_cap_handlers ifn = [
    C.SComment "setup cap handlers",
    C.Ex $ C.Assignment (C.FieldOf handlers "st") my_bindvar,
    C.Ex $ C.Assignment (C.FieldOf handlers "cap_receive_handler")
                        (C.Variable $ cap_rx_handler_name ifn),
    C.Ex $ C.Assignment (C.FieldOf handlers "cap_send_reply_handler")
                        (C.Variable $ cap_tx_reply_handler_name ifn) ]
    where
        chanvar = my_bindvar `C.DerefField` "bmp_state" `C.FieldOf` "chan"
        handlers = chanvar `C.FieldOf` "cap_handlers"
