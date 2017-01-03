{-
  UMPCommon.hs: Flounder stub generator for cross-core shared memory message passing.

  Part of Flounder: a message passing IDL for Barrelfish

  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}

module UMPCommon where

import Data.Char
import Data.Maybe

import qualified CAbsSyntax as C
import qualified Backend
import Arch
import BackendCommon
import Syntax
import MsgFragments
import GHBackend (connect_handlers_fn_name, disconnect_handlers_fn_name)

-- parameters used to modify the behaviour of this backend
data UMPParams = UMPParams {
    ump_payload :: Int,    -- UMP payload size in bytes, excluding header
    ump_drv :: String,     -- name of underlying interconnect driver
    ump_arch :: Arch,

    ump_binding_extra_fields :: [C.Param], -- extra fields in binding struct
    ump_extra_includes :: [String], -- extra includes in header
    ump_extra_protos :: String -> [C.Unit], -- extra prototypes in header
    ump_extra_fns :: String -> [C.Unit], -- extra functions in stub

    ump_register_recv :: String -> [C.Stmt],    -- register for receive
    ump_deregister_recv ::  String -> [C.Stmt], -- deregister
    ump_accept_alloc_notify :: Maybe (String -> [C.Stmt]),  -- code to allocate notify state for accept
    ump_bind_alloc_notify :: Maybe (String -> [C.Stmt]),    -- code to allocate notify state for bind
    ump_store_notify_cap :: String -> C.Expr -> [C.Stmt],   -- code to store the remote notify cap
    ump_notify :: [C.Stmt],                     -- send notification
    ump_binding_extra_fields_init :: [C.Stmt],  -- initialize extra fields in binding structure upon bind
    ump_connect_extra_fields_init :: [C.Stmt]   -- initialize extra fields in binding structure upon connect
}

template_params = UMPParams {
    ump_payload = undefined,
    ump_drv = "ump",
    ump_arch = undefined,

    ump_binding_extra_fields = [],
    ump_extra_includes = [],

    ump_extra_protos = \ifn -> [],
    ump_extra_fns = \ifn -> [],

    ump_register_recv = undefined,
    ump_deregister_recv = undefined,
    ump_accept_alloc_notify = Nothing,
    ump_bind_alloc_notify = Nothing,
    ump_store_notify_cap = \ifn v -> [C.SComment "notify cap ignored"],
    ump_notify = [],
    ump_binding_extra_fields_init = [],
    ump_connect_extra_fields_init = []
}

------------------------------------------------------------------------
-- Language mapping: C identifier names
------------------------------------------------------------------------

ump_ifscope :: UMPParams -> String -> String -> String
ump_ifscope p ifn s = ifscope ifn ((ump_drv p) ++ "_" ++ s)

-- Name of the binding struct
my_bind_type :: UMPParams -> String -> String
my_bind_type p ifn = ump_ifscope p ifn "binding"

-- Name of the local variable used for the UMP-specific binding type
my_bind_var_name :: String
my_bind_var_name = "b"
my_bindvar = C.Variable my_bind_var_name

-- Name of the bind function
bind_fn_name p n = ump_ifscope p n "bind"

-- Name of the tx_bind_msg function
tx_bind_msg_fn_name p n = ump_ifscope p n "tx_bind_msg"

-- Name of the tx_bind_reply function
tx_bind_reply_fn_name p n = ump_ifscope p n "tx_bind_reply"

-- Name of the connect function
connect_fn_name p n = ump_ifscope p n "connect"

-- Name of the accept function
accept_fn_name p n = ump_ifscope p n "accept"

-- Name of the bind continuation function
bind_cont_fn_name p n = ump_ifscope p n "bind_continuation"

-- Name of the continuation for new monitor bindings
new_monitor_cont_fn_name p n = ump_ifscope p n "new_monitor_binding_continuation"

-- Name of the destroy function
destroy_fn_name p n = ump_ifscope p n "destroy"

-- Name of the transmit function
tx_fn_name p ifn mn = idscope ifn mn ((ump_drv p) ++ "_send")

-- Name of the transmit handler
tx_handler_name p ifn = ump_ifscope p ifn "send_handler"

-- Name of the cap transmit handler
tx_cap_handler_name p ifn = ump_ifscope p ifn "cap_send_handler"

-- Name of the transmit vtable
tx_vtbl_name p ifn = ump_ifscope p ifn "tx_vtbl"

-- Name of the receive handler
rx_handler_name p ifn = ump_ifscope p ifn "rx_handler"

-- Name of the cap send/recv handlers
cap_rx_handler_name p ifn = ump_ifscope p ifn "cap_rx_handler"

-- Names of the control functions
change_waitset_fn_name p ifn = ump_ifscope p ifn "change_waitset"

-- Name of the continuation that runs when we get the monitor mutex
monitor_mutex_cont_name p ifn = ump_ifscope p ifn "monitor_mutex_cont"

-- Name of the receive next function that should be called when a binding
-- can start receiving next message
receive_next_fn_name p ifn = ump_ifscope p ifn "receive_next"
get_receiving_chanstate_fn_name p ifn = ump_ifscope p ifn "get_receiving_chanstate"

------------------------------------------------------------------------
-- Language mapping: Create the header file for this interconnect driver
------------------------------------------------------------------------

header :: UMPParams -> String -> String -> Interface -> String
header p infile outfile intf =
    unlines $ C.pp_unit $ header_file intf (header_body p infile intf)
    where
        header_file :: Interface -> [C.Unit] -> C.Unit
        header_file interface@(Interface name _ _) body =
            let sym = "__" ++ name ++ "_" ++ (map toUpper (ump_drv p)) ++ "_H"
            in C.IfNDef sym ([ C.Define sym [] "1"] ++ body) []

header_body :: UMPParams -> String -> Interface -> [C.Unit]
header_body p infile interface@(Interface name descr decls) = [
    intf_preamble infile name descr,
    C.Blank,
    C.MultiComment [ (map toUpper (ump_drv p)) ++ " interconnect driver" ],
    C.Blank,
    C.Include C.Standard $ "barrelfish/ump_chan.h",
    C.Include C.Standard "flounder/flounder_support_ump.h",
    C.UnitList $ [C.Include C.Standard i | i <- ump_extra_includes p],
    C.Blank,
    binding_struct p name,
    C.Blank,
    destroy_function_proto p name,
    bind_function_proto p name,
    connect_handler_proto p name,
    rx_handler_proto p name,
    accept_function_proto p name,
    connect_function_proto p name,
    C.UnitList $ ump_extra_protos p name,
    C.Blank
    ]


connect_function_proto :: UMPParams -> String -> C.Unit
connect_function_proto p n =
    C.GVarDecl C.Extern C.NonConst
         (C.Function C.NoScope (C.TypeName "errval_t") params) name Nothing
    where
      name = connect_fn_name p n
      params = connect_params p n

connect_params p n = [ C.Param (C.Ptr $ C.Struct $ intf_frameinfo_type n) intf_frameinfo_var,
                       C.Param (C.Ptr $ C.TypeName $ intf_bind_cont_type n) intf_cont_var,
                       C.Param (C.Ptr $ C.TypeName "void") "st",
                       C.Param (C.Ptr $ C.Struct "waitset") "ws",
                       C.Param (C.TypeName "idc_bind_flags_t") "flags" ]

accept_function_proto :: UMPParams -> String -> C.Unit
accept_function_proto p n =
    C.GVarDecl C.Extern C.NonConst
         (C.Function C.NoScope (C.TypeName "errval_t") params) name Nothing
    where
      name = accept_fn_name p n
      params = accept_params p n

accept_params p n = [ C.Param (C.Ptr $ C.Struct $ intf_frameinfo_type n) intf_frameinfo_var,
                      C.Param (C.Ptr $ C.TypeName "void") "st",
                      C.Param (C.Ptr $ C.TypeName $ intf_bind_cont_type n) intf_cont_var,
                      C.Param (C.Ptr $ C.Struct "waitset") "ws",
                      C.Param (C.TypeName "idc_export_flags_t") "flags" ]


destroy_function_proto :: UMPParams -> String -> C.Unit
destroy_function_proto p n =
    C.GVarDecl C.Extern C.NonConst
         (C.Function C.NoScope C.Void params) name Nothing
    where
      name = destroy_fn_name p n
      params = [C.Param (C.Ptr $ C.Struct (my_bind_type p n)) "b"]

bind_function_proto :: UMPParams -> String -> C.Unit
bind_function_proto p n =
    C.GVarDecl C.Extern C.NonConst
         (C.Function C.NoScope (C.TypeName "errval_t") params) name Nothing
    where
      name = bind_fn_name p n
      params = bind_params p n

bind_params p n = [ C.Param (C.Ptr $ C.Struct (my_bind_type p n)) "b",
                 C.Param (C.TypeName "iref_t") "iref",
                 C.Param (C.Ptr $ C.TypeName $ intf_bind_cont_type n) intf_cont_var,
                 C.Param (C.Ptr $ C.TypeName "void") "st",
                 C.Param (C.Ptr $ C.Struct "waitset") "waitset",
                 C.Param (C.TypeName "idc_bind_flags_t") "flags",
                 C.Param (C.TypeName "size_t") "inchanlen",
                 C.Param (C.TypeName "size_t") "outchanlen" ]

connect_handler_proto :: UMPParams -> String -> C.Unit
connect_handler_proto p ifn = C.GVarDecl C.Extern C.NonConst
    (C.Function C.NoScope (C.TypeName "errval_t") (connect_handler_params p))
    (drv_connect_handler_name (ump_drv p) ifn) Nothing

connect_handler_params :: UMPParams -> [C.Param]
connect_handler_params p
    = [C.Param (C.Ptr $ C.Void) "st",
       C.Param (C.Ptr $ C.Struct "monitor_binding") "mb",
       C.Param (C.TypeName "uintptr_t") "mon_id",
       C.Param (C.Struct "capref") "frame",
       C.Param (C.TypeName "size_t") "inchanlen",
       C.Param (C.TypeName "size_t") "outchanlen",
       C.Param (C.Struct "capref") "notify_cap"]

binding_struct :: UMPParams -> String -> C.Unit
binding_struct p ifn = C.StructDecl (my_bind_type p ifn) fields
  where
    fields = [
        C.Param (C.Struct $ intf_bind_type ifn) "b",
        C.Param (C.Struct "flounder_ump_state") "ump_state",
        C.ParamBlank,
        -- these are needed for the new monitor continuation to know the bind parameters
        C.ParamComment "bind params for the new monitor continuation",
        C.Param (C.TypeName "iref_t") "iref",
        C.Param (C.TypeName "size_t") "inchanlen",
        C.Param (C.TypeName "size_t") "outchanlen",
        C.ParamBlank,
        C.ParamComment "flag indicating that transfers of caps are not supported",
        C.Param (C.TypeName "uint8_t") "no_cap_transfer",
        C.Param (C.TypeName "uint8_t") "is_client"
        ]
        ++ ump_binding_extra_fields p

rx_handler_proto p ifn = C.GVarDecl C.Extern C.NonConst
    (C.Function C.NoScope C.Void [C.Param (C.Ptr C.Void) "arg"])
    (rx_handler_name p ifn) Nothing

------------------------------------------------------------------------
-- Language mapping: Create the stub (implementation) for this interconnect driver
------------------------------------------------------------------------

stub :: UMPParams -> String -> String -> Interface -> String
stub p infile outfile intf = unlines $ C.pp_unit $ stub_body p infile intf

stub_body :: UMPParams -> String -> Interface -> C.Unit
stub_body p infile intf@(Interface ifn descr decls) = C.UnitList [
    intf_preamble infile ifn descr,
    C.Blank,
    C.IfDef ("CONFIG_FLOUNDER_BACKEND_" ++ (map toUpper (ump_drv p)))
    [ C.Blank,
      C.MultiComment [ "Generated Stub for " ++ (map toUpper (ump_drv p)) ],
      C.Blank,

      C.Include C.Standard "barrelfish/barrelfish.h",
      C.Include C.Standard "barrelfish/monitor_client.h",
      C.Include C.Standard "flounder/flounder_support.h",
      C.Include C.Standard "flounder/flounder_support_ump.h",
      C.Include C.Standard ("if/" ++ ifn ++ "_defs.h"),
      C.Blank,

      C.MultiComment [ "Send handler function" ],
      tx_handler p ifn msg_specs,
      C.UnitList $ if (drvname == "ump") then [ tx_bind_msg p ifn ] else [],
      tx_bind_reply p ifn,
      C.Blank,

      C.MultiComment [ "Capability sender function" ],
      tx_cap_handler p ifn msg_specs,
      C.Blank,


      C.MultiComment [ "Receive handler" ],
      rx_handler p ifn types messages msg_specs,
      C.Blank,

      C.MultiComment [ "Cap send/receive handlers" ],
      cap_rx_handler p ifn types messages msg_specs,
      C.Blank,

      C.UnitList $ if has_caps then
                       [C.MultiComment [ "Monitor mutex acquire continuation" ],
                        monitor_mutex_cont p ifn,
                        C.Blank]
                   else [],

      C.MultiComment [ "Message sender functions" ],

      C.UnitList [ tx_fn p ifn types msg spec | (msg, spec) <- zip messages msg_specs ],
      C.Blank,

      C.MultiComment [ "Send vtable" ],
      tx_vtbl p ifn messages,

      C.MultiComment [ "Control functions" ],
      can_send_fn_def drvname ifn,
      register_send_fn_def drvname ifn,
      default_error_handler_fn_def drvname ifn,
      change_waitset_fn_def p ifn,
      generic_control_fn_def drvname ifn,
      receive_next_fn_def p ifn,
      get_receiving_chanstate_fn_def p ifn,
      C.Blank,

      C.MultiComment [ "Function to destroy the binding state" ],
      destroy_fn p ifn,
      C.Blank,

      C.MultiComment [ "Bind function" ],
      bind_cont_fn p ifn,
      C.UnitList $ ump_extra_fns p ifn,
      new_monitor_cont_fn p ifn,
      bind_fn p ifn,
      C.Blank,

      C.MultiComment [ "Connect callback for export" ],
      connect_handler_fn p ifn,
      C.Blank,

      C.MultiComment [ "Functions to accept/connect over a already shared frame" ],
      C.UnitList $ if (drvname == "ump") then [ accept_fn p ifn, connect_fn p ifn ] else []
    ] [] ]
    where
        drvname = ump_drv p
        (types, messagedecls) = Backend.partitionTypesMessages decls
        messages = rpcs_to_msgs messagedecls
        msg_specs = map (build_msg_spec myarch words_per_frag False types) messages
        words_per_frag = (ump_payload p) `div` (wordsize (ump_arch p) `div` 8)

        has_caps = [1 | MsgSpec _ _ caps <- msg_specs, caps /= []] /= []

        -- hack: ensure that we raise an error if any types in the messages depend
        -- on the architecture-specific sizes (uintptr etc.)
        myarch = (ump_arch p) {
            ptrsize = error $ "cannot compile this interface for UMP;" ++
                        " it uses intptr/uintptr which are non-portable",
            sizesize = error $ "cannot compile this interface for UMP;" ++
                        " it uses the size type which is non-portable"
        }

destroy_fn :: UMPParams -> String -> C.Unit
destroy_fn p ifn =
    C.FunctionDef C.NoScope C.Void (destroy_fn_name p ifn) params
        [C.StmtList common_destroy,
         C.Ex $ C.Call "ump_chan_destroy"
            [C.AddressOf $ statevar `C.FieldOf` "chan"]]
    where
        statevar = C.DerefField my_bindvar "ump_state"
        common_destroy = binding_struct_destroy ifn (C.DerefField my_bindvar "b")
        params = [C.Param (C.Ptr $ C.Struct (my_bind_type p ifn)) "b"]


connect_fn :: UMPParams -> String -> C.Unit
connect_fn p ifn =
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (connect_fn_name p ifn) params [
      localvar (C.TypeName "errval_t") "err" Nothing,

      C.SComment "allocate storage for binding",
      localvar (C.Ptr $ C.Struct $ my_bind_type p ifn) my_bind_var_name
          $ Just $ C.Call "malloc" [C.SizeOfT $ C.Struct $ my_bind_type p ifn],
      C.If (C.Binary C.Equals my_bindvar (C.Variable "NULL"))
          [C.Return $ C.Variable "LIB_ERR_MALLOC_FAIL"] [],
      C.SBlank,

      localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
            intf_bind_var (Just $ C.AddressOf $ my_bindvar `C.DerefField` "b"),

      C.StmtList common_init,
      C.Ex $ C.Call "flounder_stub_ump_state_init" [C.AddressOf statevar, my_bindvar],
      C.Ex $ C.Assignment (common_field "change_waitset") (C.Variable $ change_waitset_fn_name p ifn),
      C.Ex $ C.Assignment (common_field "control") (C.Variable $ generic_control_fn_name (ump_drv p) ifn),
      C.Ex $ C.Assignment (common_field "receive_next") (C.Variable $ receive_next_fn_name p ifn),
      C.Ex $ C.Assignment (common_field "get_receiving_chanstate") (C.Variable $ get_receiving_chanstate_fn_name p ifn),
      C.Ex $ C.Assignment (common_field "st") (C.Variable "st"),
      C.Ex $ C.Assignment (intf_bind_v `C.FieldOf` "bind_cont") (C.Variable intf_cont_var),

      C.Ex $ C.Assignment errvar $ C.Call "ump_chan_init"
          [C.AddressOf $ statevar `C.FieldOf` "chan",
          (C.DerefField (C.Variable intf_frameinfo_var) "inbuf"),
          (C.DerefField (C.Variable intf_frameinfo_var) "inbufsize"),
          (C.DerefField (C.Variable intf_frameinfo_var) "outbuf"),
          (C.DerefField (C.Variable intf_frameinfo_var) "outbufsize")],
      C.If (C.Call "err_is_fail" [errvar])
          [C.Ex $ C.Call (destroy_fn_name p ifn) [my_bindvar],
           C.Return $
              C.Call "err_push" [errvar, C.Variable "LIB_ERR_UMP_CHAN_INIT"]]
          [],
      C.Ex $ C.Assignment (C.FieldOf (common_field "tx_cont_chanstate") "trigger") (C.AddressOf $ C.FieldOf chanvar "send_waitset"),
      C.SBlank,

      C.Ex $ C.Assignment (sendvar) (C.DerefField (C.Variable "_frameinfo") "sendbase"),
      C.Ex $ C.Assignment (my_bindvar `C.DerefField` "inchanlen") (C.DerefField (C.Variable intf_frameinfo_var) "inbufsize"),
      C.Ex $ C.Assignment (my_bindvar `C.DerefField` "outchanlen") (C.DerefField (C.Variable intf_frameinfo_var) "outbufsize"),
      C.Ex $ C.Assignment (my_bindvar `C.DerefField` "no_cap_transfer") (C.Variable "1"),
      C.Ex $ C.Assignment (my_bindvar `C.DerefField` "is_client") (C.Variable "1"),
      C.StmtList $ (ump_binding_extra_fields_init p),
      C.SBlank,

      C.StmtList $ ump_store_notify_cap p ifn (C.Variable "notify_cap"),
      C.StmtList $ setup_cap_handlers p ifn,
      C.SBlank,

      C.StmtList $ register_recv p ifn,
      C.SBlank,

      C.Return  $ C.Call (tx_bind_msg_fn_name p ifn) [my_bindvar]]

    where
        params = connect_params p ifn
        errvar = C.Variable "err"
        statevar = C.DerefField my_bindvar "ump_state"
        chanvar = statevar `C.FieldOf` "chan"
        sendvar = chanvar `C.FieldOf` "sendid"
        common_init = binding_struct_init (ump_drv p) ifn
          (C.DerefField my_bindvar "b")
          (C.Variable "ws")
          (C.Variable $ tx_vtbl_name p ifn)
        intf_bind_v = C.DerefField my_bindvar "b"
        common_field f = intf_bind_v `C.FieldOf` f
        receiving_chanstate = my_bindvar `C.DerefField` "b" `C.FieldOf` "receiving_chanstate"

accept_fn :: UMPParams -> String -> C.Unit
accept_fn p ifn =
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (accept_fn_name p ifn) params [
      localvar (C.TypeName "errval_t") "err" Nothing,
      C.SBlank,
      C.SComment "allocate storage for binding",
      localvar (C.Ptr $ C.Struct $ my_bind_type p ifn) my_bind_var_name
          $ Just $ C.Call "malloc" [C.SizeOfT $ C.Struct $ my_bind_type p ifn],
      C.If (C.Binary C.Equals my_bindvar (C.Variable "NULL"))
          [C.Return $ C.Variable "LIB_ERR_MALLOC_FAIL"] [],
      C.SBlank,

      localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
            intf_bind_var (Just $ C.AddressOf $ my_bindvar `C.DerefField` "b"),
      C.StmtList common_init,
      C.Ex $ C.Call "flounder_stub_ump_state_init" [C.AddressOf statevar, my_bindvar],
      C.Ex $ C.Assignment errvar $ C.Call "ump_chan_init"
          [C.AddressOf $ statevar `C.FieldOf` "chan",
          (C.DerefField (C.Variable intf_frameinfo_var) "inbuf"),
          (C.DerefField (C.Variable intf_frameinfo_var) "inbufsize"),
          (C.DerefField (C.Variable intf_frameinfo_var) "outbuf"),
          (C.DerefField (C.Variable intf_frameinfo_var) "outbufsize")],
      C.If (C.Call "err_is_fail" [errvar])
          [C.Ex $ C.Call (destroy_fn_name p ifn) [my_bindvar],
           C.Return $
              C.Call "err_push" [errvar, C.Variable "LIB_ERR_UMP_CHAN_INIT"]]
          [],
      C.Ex $ C.Assignment (C.FieldOf (common_field "tx_cont_chanstate") "trigger") (C.AddressOf $ C.FieldOf chanvar "send_waitset"),
      C.SBlank,

      C.Ex $ C.Assignment (sendvar) (C.DerefField (C.Variable "_frameinfo") "sendbase"),
      C.Ex $ C.Assignment (common_field "change_waitset") (C.Variable $ change_waitset_fn_name p ifn),
      C.Ex $ C.Assignment (common_field "control") (C.Variable $ generic_control_fn_name (ump_drv p) ifn),
      C.Ex $ C.Assignment (common_field "receive_next") (C.Variable $ receive_next_fn_name p ifn),
      C.Ex $ C.Assignment (common_field "get_receiving_chanstate") (C.Variable $ get_receiving_chanstate_fn_name p ifn),
      C.Ex $ C.Assignment (common_field "st") (C.Variable "st"),
      C.Ex $ C.Assignment (common_field "bind_cont") (C.Variable intf_cont_var),
      C.Ex $ C.Assignment (my_bindvar `C.DerefField` "inchanlen") (C.DerefField (C.Variable intf_frameinfo_var) "inbufsize"),
      C.Ex $ C.Assignment (my_bindvar `C.DerefField` "outchanlen") (C.DerefField (C.Variable intf_frameinfo_var) "outbufsize"),
      C.Ex $ C.Assignment (my_bindvar `C.DerefField` "no_cap_transfer") (C.Variable "1"),
      C.Ex $ C.Assignment (my_bindvar `C.DerefField` "is_client") (C.Variable "0"),
      C.StmtList $ register_recv p ifn,
      C.SBlank,

      C.StmtList $ ump_store_notify_cap p ifn (C.Variable "notify_cap"),
      C.StmtList $ setup_cap_handlers p ifn,
      C.SBlank,

      C.Return (C.Variable "SYS_ERR_OK")]
    where
        params = accept_params p ifn
        statevar = C.DerefField my_bindvar "ump_state"
        chanvar = statevar `C.FieldOf` "chan"
        sendvar = chanvar `C.FieldOf` "sendid"
        chanaddr = C.AddressOf $ chanvar
        common_field f = my_bindvar `C.DerefField` "b" `C.FieldOf` f
        common_init = binding_struct_init (ump_drv p) ifn
                        (C.DerefField my_bindvar "b")
                        (C.Variable "ws")
                        (C.Variable $ tx_vtbl_name p ifn)


bind_fn :: UMPParams -> String -> C.Unit
bind_fn p ifn =
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (bind_fn_name p ifn) params [
        localvar (C.TypeName "errval_t") "err" Nothing,
        C.StmtList common_init,
        C.Ex $ C.Call "flounder_stub_ump_state_init" [C.AddressOf statevar, my_bindvar],
        C.Ex $ C.Assignment (common_field "change_waitset") (C.Variable $ change_waitset_fn_name p ifn),
        C.Ex $ C.Assignment (common_field "control") (C.Variable $ generic_control_fn_name (ump_drv p) ifn),
        C.Ex $ C.Assignment (common_field "receive_next") (C.Variable $ receive_next_fn_name p ifn),
        C.Ex $ C.Assignment (common_field "get_receiving_chanstate") (C.Variable $ get_receiving_chanstate_fn_name p ifn),
        C.Ex $ C.Assignment (common_field "st") (C.Variable "st"),
        C.Ex $ C.Assignment (C.FieldOf (common_field "tx_cont_chanstate") "trigger") (C.AddressOf $ C.FieldOf chanvar "send_waitset"),
        C.Ex $ C.Assignment (intf_bind_var `C.FieldOf` "bind_cont") (C.Variable intf_cont_var),
        C.Ex $ C.Assignment (my_bindvar `C.DerefField` "iref") (C.Variable "iref"),
        C.Ex $ C.Assignment (my_bindvar `C.DerefField` "inchanlen") (C.Variable "inchanlen"),
        C.Ex $ C.Assignment (my_bindvar `C.DerefField` "outchanlen") (C.Variable "outchanlen"),
        C.Ex $ C.Assignment (my_bindvar `C.DerefField` "no_cap_transfer") (C.Variable "0"),
        C.Ex $ C.Assignment (C.FieldOf (common_field "tx_cont_chanstate") "trigger") (C.AddressOf $ C.FieldOf chanvar "send_waitset"),
        C.StmtList $ (ump_binding_extra_fields_init p),
        C.SBlank,
        C.SComment "do we need a new monitor binding?",
        C.If (C.Binary C.BitwiseAnd (C.Variable "flags") (C.Variable "IDC_BIND_FLAG_RPC_CAP_TRANSFER"))
            [C.Ex $ C.Assignment errvar $ C.Call "monitor_client_new_binding"
                [C.Variable (new_monitor_cont_fn_name p ifn),
                 my_bindvar, C.Variable "waitset",
                 C.Variable "DEFAULT_LMP_BUF_WORDS"]
            ]

            -- no monitor binding, but do we need to alloc notify state?
            (if isJust (ump_bind_alloc_notify p)
            then
                [C.Ex $ C.Assignment (chanvar `C.FieldOf` "monitor_binding")
                                     (C.Call "get_monitor_binding" []),
                 C.StmtList $ (fromJust $ ump_bind_alloc_notify p) ifn,
                 C.If (C.Call "err_is_fail" [errvar])
                    [C.Ex $ C.Assignment errvar $ C.Call "err_push"
                     [errvar, C.Variable "FLOUNDER_ERR_UMP_ALLOC_NOTIFY"]] [] ]
            else -- nothing special, just call bind
                [C.Ex $ C.Assignment errvar $ C.Call "ump_chan_bind"
                    [C.AddressOf $ statevar `C.FieldOf` "chan",
                     C.StructConstant "ump_bind_continuation"
                        [("handler", C.Variable (bind_cont_fn_name p ifn)),
                         ("st", my_bindvar)],
                     C.AddressOf $ intf_bind_var `C.FieldOf` "event_qnode",
                     C.Variable "iref", C.Call "get_monitor_binding" [],
                     C.Variable "inchanlen", C.Variable "outchanlen",
                     C.Variable "NULL_CAP"]]),
        C.SBlank,
        C.If (C.Call "err_is_fail" [errvar])
            [C.Ex $ C.Call (destroy_fn_name p ifn) [my_bindvar]] [],
        C.Return errvar
    ]
    where
      statevar = C.DerefField my_bindvar "ump_state"
      chanvar = statevar `C.FieldOf` "chan"
      common_init = binding_struct_init (ump_drv p) ifn
        (C.DerefField my_bindvar "b")
        (C.Variable "waitset")
        (C.Variable $ tx_vtbl_name p ifn)
      params = bind_params p ifn
      intf_bind_var = C.DerefField my_bindvar "b"
      common_field f = intf_bind_var `C.FieldOf` f
      receiving_chanstate = my_bindvar `C.DerefField` "b" `C.FieldOf` "receiving_chanstate"


new_monitor_cont_fn :: UMPParams -> String -> C.Unit
new_monitor_cont_fn p ifn =
    C.FunctionDef C.Static C.Void (new_monitor_cont_fn_name p ifn) params [
        localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
                intf_bind_var (Just $ C.Variable "st"),
        localvar (C.Ptr $ C.Struct $ my_bind_type p ifn)
                my_bind_var_name (Just $ C.Variable "st"),
        C.SBlank,

        C.If (C.Call "err_is_fail" [errvar])
            [C.Ex $ C.Assignment errvar $
                C.Call "err_push" [errvar, C.Variable "LIB_ERR_MONITOR_CLIENT_BIND"],
             C.Goto "out"] [],
        C.SBlank,

        C.Ex $ C.Assignment (chanvar `C.FieldOf` "monitor_binding") (C.Variable "monitor_binding"),
        C.StmtList $ if isJust (ump_bind_alloc_notify p)
        then
            [C.StmtList $ (fromJust $ ump_bind_alloc_notify p) ifn,
            C.If (C.Call "err_is_fail" [errvar])
                [C.Ex $ C.Assignment errvar $ C.Call "err_push"
                    [errvar, C.Variable "FLOUNDER_ERR_UMP_ALLOC_NOTIFY"],
                 C.Goto "out"] [] ]
        else
            [C.SComment "start the bind on the new monitor binding",
             C.Ex $ C.Assignment errvar $ C.Call "ump_chan_bind"
                [C.AddressOf $ my_bindvar `C.DerefField` "ump_state" `C.FieldOf` "chan",
                 C.StructConstant "ump_bind_continuation"
                    [("handler", C.Variable (bind_cont_fn_name p ifn)),
                     ("st", my_bindvar)],
                 C.AddressOf $ bindvar `C.DerefField` "event_qnode",
                 my_bindvar `C.DerefField` "iref",
                 C.Variable "monitor_binding",
                 my_bindvar `C.DerefField` "inchanlen",
                 my_bindvar `C.DerefField` "outchanlen",
                 C.Variable "NULL_CAP"]],
        C.SBlank,

        C.Label "out",
        C.If (C.Call "err_is_fail" [errvar])
            [C.Ex $ C.CallInd (bindvar `C.DerefField` "bind_cont")
                [bindvar `C.DerefField` "st", errvar, bindvar],
            C.Ex $ C.Call (destroy_fn_name p ifn) [my_bindvar]] []
    ]
    where
        params = [C.Param (C.Ptr C.Void) "st",
                  C.Param (C.TypeName "errval_t") "err",
                  C.Param (C.Ptr $ C.Struct "monitor_binding") "monitor_binding"]
        chanvar = my_bindvar `C.DerefField` "ump_state" `C.FieldOf` "chan"


bind_cont_fn :: UMPParams -> String -> C.Unit
bind_cont_fn p ifn =
    C.FunctionDef C.Static C.Void (bind_cont_fn_name p ifn) params [
        localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
            intf_bind_var (Just $ C.Variable "st"),
        localvar (C.Ptr $ C.Struct $ my_bind_type p ifn)
            my_bind_var_name (Just $ C.Variable "st"),
        C.SBlank,

        C.If (C.Call "err_is_ok" [errvar])
            [C.StmtList $ ump_store_notify_cap p ifn (C.Variable "notify_cap"),
             C.StmtList $ setup_cap_handlers p ifn,
             C.StmtList $ register_recv p ifn]
            [C.Ex $ C.Call (destroy_fn_name p ifn) [my_bindvar]],
        C.SBlank,

        C.Ex $ C.CallInd (bindvar `C.DerefField` "bind_cont")
            [bindvar `C.DerefField` "st", errvar, bindvar]]
    where
      params = [C.Param (C.Ptr C.Void) "st",
                C.Param (C.TypeName "errval_t") "err",
                C.Param (C.Ptr $ C.Struct "ump_chan") "chan",
                C.Param (C.Struct "capref") "notify_cap"]
      errvar = C.Variable "err"
      chanaddr = C.Variable "chan"

connect_handler_fn :: UMPParams -> String -> C.Unit
connect_handler_fn p ifn = C.FunctionDef C.NoScope (C.TypeName "errval_t")
    (drv_connect_handler_name (ump_drv p) ifn) (connect_handler_params p) [
    localvar (C.Ptr $ C.Struct $ export_type ifn) "e" $ Just $ C.Variable "st",
    localvar (C.TypeName "errval_t") "err" Nothing,
    C.SBlank,
    C.SComment "allocate storage for binding",
    localvar (C.Ptr $ C.Struct $ my_bind_type p ifn) my_bind_var_name
        $ Just $ C.Call "malloc" [C.SizeOfT $ C.Struct $ my_bind_type p ifn],
    C.If (C.Binary C.Equals my_bindvar (C.Variable "NULL"))
        [C.Return $ C.Variable "LIB_ERR_MALLOC_FAIL"] [],
    C.SBlank,

    localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
         intf_bind_var (Just $ C.AddressOf $ my_bindvar `C.DerefField` "b"),
    C.StmtList common_init,
    C.Ex $ C.Call "flounder_stub_ump_state_init" [C.AddressOf statevar, my_bindvar],
    C.Ex $ C.Assignment (common_field "change_waitset") (C.Variable $ change_waitset_fn_name p ifn),
    C.Ex $ C.Assignment (common_field "control") (C.Variable $ generic_control_fn_name (ump_drv p) ifn),
    C.Ex $ C.Assignment (common_field "receive_next") (C.Variable $ receive_next_fn_name p ifn),
    C.Ex $ C.Assignment (common_field "get_receiving_chanstate") (C.Variable $ get_receiving_chanstate_fn_name p ifn),
    C.Ex $ C.Assignment (my_bindvar `C.DerefField` "no_cap_transfer") (C.Variable "0"),
    C.StmtList $ (ump_connect_extra_fields_init p),
    C.Ex $ C.Assignment (C.FieldOf (common_field "tx_cont_chanstate") "trigger") (C.AddressOf $ C.FieldOf chanvar "send_waitset"),
    C.SBlank,

    C.SComment "run user's connect handler",
    C.Ex $ C.Assignment errvar $ C.CallInd (C.DerefField exportvar "connect_cb")
                       [C.DerefField exportvar "st", bindvar],
    C.If (C.Call "err_is_fail" [errvar])
        [C.SComment "connection refused",
         C.Ex $ C.Call (destroy_fn_name p ifn) [my_bindvar],
         C.Return $ errvar] [],
    C.SBlank,

    C.SComment "accept the connection and setup the channel",
    C.Ex $ C.Assignment errvar $ C.Call "ump_chan_accept"
                                [chanaddr, C.Variable "mon_id", C.Variable "frame",
                                 C.Variable "inchanlen", C.Variable "outchanlen"],
    C.If (C.Call "err_is_fail" [errvar])
        [C.Ex $ C.Assignment errvar $ C.Call "err_push"
                    [errvar, C.Variable "LIB_ERR_UMP_CHAN_ACCEPT"],
         report_user_err errvar,
         C.Return $ errvar] [],
    C.SBlank,

    C.StmtList $ ump_store_notify_cap p ifn (C.Variable "notify_cap"),
    C.StmtList $ setup_cap_handlers p ifn,
    C.SBlank,

    C.Ex $ C.Call (connect_handlers_fn_name ifn) [C.Variable intf_bind_var],

    C.StmtList $ if isJust (ump_accept_alloc_notify p)
        then
            [C.StmtList $ (fromJust $ ump_accept_alloc_notify p) ifn,
            C.If (C.Call "err_is_fail" [errvar])
                [C.Ex $ C.Assignment errvar $ C.Call "err_push"
                    [errvar, C.Variable "FLOUNDER_ERR_UMP_ALLOC_NOTIFY"],
                report_user_err errvar,
                C.Return $ errvar] [] ]
        else
            [C.StmtList $ register_recv p ifn,
             C.SBlank,
             C.SComment "send back bind reply",
             C.Ex $ C.Call "ump_chan_send_bind_reply"
                  [C.Variable "mb", chanaddr, C.Variable "SYS_ERR_OK",
                   C.Variable "mon_id", C.Variable "NULL_CAP"],
             C.SBlank],

    C.Return $ C.Variable "SYS_ERR_OK"]
    where
        exportvar = C.Variable "e"
        statevar = C.DerefField my_bindvar "ump_state"
        chanvar = statevar `C.FieldOf` "chan"
        chanaddr = C.AddressOf $ chanvar
        common_init = binding_struct_init (ump_drv p) ifn
                        (C.DerefField my_bindvar "b")
                        (exportvar `C.DerefField` "waitset")
                        (C.Variable $ tx_vtbl_name p ifn)
        common_field f = my_bindvar `C.DerefField` "b" `C.FieldOf` f
        receiving_chanstate = my_bindvar `C.DerefField` "b" `C.FieldOf` "receiving_chanstate"

change_waitset_fn_def :: UMPParams -> String -> C.Unit
change_waitset_fn_def p ifn =
    C.FunctionDef C.Static (C.TypeName "errval_t") (change_waitset_fn_name p ifn) params [
        localvar (C.Ptr $ C.Struct $ my_bind_type p ifn)
            my_bind_var_name (Just $ C.Cast (C.Ptr C.Void) bindvar),
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
        C.StmtList $ ump_deregister_recv p ifn,
        C.If (C.Binary C.And
                (C.Call "err_is_fail" [errvar])
                (C.Binary C.NotEquals (C.Call "err_no" [errvar])
                                    (C.Variable "LIB_ERR_CHAN_NOT_REGISTERED")))
            [C.Return $
               C.Call "err_push" [errvar, C.Variable "LIB_ERR_CHAN_DEREGISTER_RECV"]]
            [],
        C.Ex $ C.Call (disconnect_handlers_fn_name ifn) [bindvar],

        C.SComment "change waitset on binding",
        C.Ex $ C.Assignment
            (bindvar `C.DerefField` "waitset")
            (C.Variable "ws"),
        C.SBlank,

        C.Ex $ C.Call (connect_handlers_fn_name ifn) [bindvar],
        C.SComment "re-register for receive (if previously registered)",
        C.If (C.Call "err_is_ok" [errvar]) [
            C.StmtList $ ump_register_recv p ifn,
            C.If (C.Call "err_is_fail" [errvar])
                [C.Return $
                    C.Call "err_push" [errvar, C.Variable "LIB_ERR_CHAN_REGISTER_RECV"]]
                []
            ] [],
        C.Return $ C.Variable "SYS_ERR_OK"
    ]
    where
        chanvar = my_bindvar `C.DerefField` "ump_state" `C.FieldOf` "chan"
        chanaddr = C.AddressOf $ chanvar
        params = [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var,
                  C.Param (C.Ptr $ C.Struct "waitset") "ws"]

receive_next_fn_def :: UMPParams -> String -> C.Unit
receive_next_fn_def p ifn =
    C.FunctionDef C.Static (C.TypeName "errval_t") (receive_next_fn_name p ifn) params [
        localvar (C.TypeName "errval_t") "err" Nothing,
        localvar (C.Ptr $ C.Struct $ my_bind_type p ifn)
            my_bind_var_name (Just $ C.Cast (C.Ptr C.Void) $ C.Variable intf_bind_var),
        C.SBlank,
        C.StmtList $ register_recv p ifn,
        C.Return $ C.Variable "SYS_ERR_OK"
    ]
    where
        params = [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var]

get_receiving_chanstate_fn_def :: UMPParams -> String -> C.Unit
get_receiving_chanstate_fn_def p ifn =
    C.FunctionDef C.Static (C.Ptr $ C.Struct "waitset_chanstate") (get_receiving_chanstate_fn_name p ifn) params [
        localvar (C.Ptr $ C.Struct $ my_bind_type p ifn)
            my_bind_var_name (Just $ C.Cast (C.Ptr C.Void) bindvar),
        C.SBlank,
        C.Return $ C.Call "ump_chan_get_receiving_channel" [C.AddressOf $ C.FieldOf (C.DerefField my_bindvar "ump_state") "chan"]
    ]
    where
        params = [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var]

handler_preamble :: UMPParams -> String -> C.Stmt
handler_preamble p ifn = C.StmtList
    [C.SComment "Get the binding state from our argument pointer",
     localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
         intf_bind_var (Just $ C.Variable "arg"),
     localvar (C.Ptr $ C.Struct $ my_bind_type p ifn)
         my_bind_var_name (Just $ C.Variable "arg"),
     localvar (C.TypeName "errval_t") "err" Nothing,
     C.Ex $ C.Assignment errvar (C.Variable "SYS_ERR_OK"),
     C.SBlank]

tx_cap_handler :: UMPParams -> String -> [MsgSpec] -> C.Unit
tx_cap_handler p ifn msgspecs =
    C.FunctionDef C.Static C.Void (tx_cap_handler_name p ifn) [C.Param (C.Ptr C.Void) "arg"] [
        handler_preamble p ifn,

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
        umpst = C.DerefField my_bindvar "ump_state"
        capst = umpst `C.FieldOf` "capst"
        cases = [C.Case (C.Variable $ msg_enum_elem_name ifn mn)
                        (tx_cap_handler_case p ifn mn (length frags) caps)
                 | MsgSpec mn frags caps <- msgspecs, caps /= []]

tx_cap_handler_case :: UMPParams -> String -> String -> Int -> [CapFieldTransfer] -> [C.Stmt]
tx_cap_handler_case p ifn mn nfrags caps = [
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
        umpst = C.DerefField my_bindvar "ump_state"
        capst = umpst `C.FieldOf` "capst"
        chan = umpst `C.FieldOf` "chan"
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
                 give_away_val tm, C.Variable $ tx_cap_handler_name p ifn],
            C.If (C.Call "err_is_fail" [errvar])
                [report_user_tx_err errvar, C.Break] [],
            C.Break]


tx_bind_msg :: UMPParams -> String -> C.Unit
tx_bind_msg p ifn =
    C.FunctionDef C.Static (C.TypeName "errval_t") (tx_bind_msg_fn_name p ifn) params [
      handler_preamble p ifn,

      localvar (C.Volatile $ C.Ptr $ C.Struct "ump_message") "msg" Nothing,
      localvar (C.Struct "ump_control") "ctrl" Nothing,
      C.SBlank,


      C.SComment "send the next fragment",
      C.Ex $ C.Assignment ump_token (C.Variable "0"),
      C.Ex $ C.Assignment msgvar $ C.Call "ump_chan_get_next" [chanaddr, ctrladdr],
      C.SComment "check if we can send another message",
      C.If (C.Unary C.Not msgvar)
          [C.Return (C.Variable "FLOUNDER_ERR_TX_BUSY")] [],
      C.SBlank,
      C.Ex $ C.Call "flounder_stub_ump_control_fill"
                  [chanst, ctrladdr, C.Variable $ msg_enum_elem_name ifn "__bind"],
--      C.StmtList
--          [C.Ex $ C.Assignment (msgword n) (fragment_word_to_expr (ump_arch p) ifn "___bind" (words !! n))
--           | n <- [0 .. length(words) - 1], words !! n  /= []],
      C.Ex $ C.Assignment (msgword 0) (C.Variable "0xcafebabe"),
      C.Ex $ C.Call "flounder_stub_ump_barrier" [],
      C.Ex $ C.Assignment msgheader ctrlvar,
      C.StmtList finished_send,
      C.Return (C.Variable "SYS_ERR_OK")]
    where
      params = [C.Param (C.Ptr C.Void) "arg"]
      chanst = C.AddressOf umpst
      chanaddr = C.AddressOf (C.DerefField chanst "chan")
      ctrlvar = C.Variable "ctrl"
      ctrladdr = C.AddressOf ctrlvar
      umpst = C.DerefField my_bindvar "ump_state"
    --  stateaddr = C.AddressOf umpst
      msgvar = C.Variable "msg"
      msgword n = C.DerefField msgvar "data" `C.SubscriptOf` (C.NumConstant $ toInteger n)
      msgheader = C.DerefField msgvar "header" `C.FieldOf` "control"
      ump_token = C.DerefField chanst "token"



tx_bind_reply :: UMPParams -> String -> C.Unit
tx_bind_reply p ifn =
    C.FunctionDef C.Static (C.TypeName "errval_t")  (tx_bind_reply_fn_name p ifn) params [
      handler_preamble p ifn,

      localvar (C.Volatile $ C.Ptr $ C.Struct "ump_message") "msg" Nothing,
      localvar (C.Struct "ump_control") "ctrl" Nothing,
      C.SBlank,

      C.SComment "send the next fragment",
      C.Ex $ C.Assignment ump_token (C.Variable "0"),
      C.Ex $ C.Assignment msgvar $ C.Call "ump_chan_get_next" [chanaddr, ctrladdr],
      C.SComment "check if we can send another message",
      C.If (C.Unary C.Not msgvar)
          [C.Return (C.Variable "FLOUNDER_ERR_TX_BUSY")] [],
      C.Ex $ C.Call "flounder_stub_ump_control_fill"
                  [chanst, ctrladdr, C.Variable $ msg_enum_elem_name ifn "__bind_reply"],
--      C.StmtList
--          [C.Ex $ C.Assignment (msgword n) (fragment_word_to_expr (ump_arch p) ifn "___bind" (words !! n))
--           | n <- [0 .. length(words) - 1], words !! n  /= []],
      C.Ex $ C.Assignment (msgword 0) (C.Variable "0xcafebabe"),
      C.Ex $ C.Call "flounder_stub_ump_barrier" [],
      C.Ex $ C.Assignment msgheader ctrlvar,
      C.StmtList finished_send,
      C.Return (C.Variable "SYS_ERR_OK")]
    where
      params = [C.Param (C.Ptr C.Void) "arg"]
      chanst = C.AddressOf umpst
      chanaddr = C.AddressOf (C.DerefField chanst "chan")
      umpst = C.DerefField my_bindvar "ump_state"
      ctrlvar = C.Variable "ctrl"
      ctrladdr = C.AddressOf ctrlvar
 --     stateaddr = C.AddressOf umpst
      msgvar = C.Variable "msg"
      msgword n = C.DerefField msgvar "data" `C.SubscriptOf` (C.NumConstant $ toInteger n)
      msgheader = C.DerefField msgvar "header" `C.FieldOf` "control"
      ump_token = C.DerefField chanst "token"

tx_handler :: UMPParams -> String -> [MsgSpec] -> C.Unit
tx_handler p ifn msgs =
    C.FunctionDef C.Static C.Void (tx_handler_name p ifn) [C.Param (C.Ptr C.Void) "arg"] [
        handler_preamble p ifn,

        -- local variables (if needed)
        C.StmtList $ if msgvars_will_be_used then
          [localvar (C.Volatile $ C.Ptr $ C.Struct "ump_message") "msg" Nothing,
           localvar (C.Struct "ump_control") "ctrl" Nothing] else [],
        localvar (C.TypeName "bool") "tx_notify" (Just $ C.Variable "false"),
        C.SBlank,

        C.SComment "do we need to (and can we) send a cap ack?",
        C.If (capst `C.FieldOf` "tx_cap_ack")
            [C.Ex $ C.Call "flounder_stub_ump_send_cap_ack" [C.AddressOf umpst],
             C.Ex $ C.Assignment (capst `C.FieldOf` "tx_cap_ack") (C.Variable "false")] [],
        C.SBlank,

        C.SComment "Switch on current outgoing message number",
        C.Switch (C.DerefField bindvar "tx_msgnum") msgcases
            [C.Ex $ C.Call "assert" [C.Unary C.Not $ C.StringConstant "invalid msgnum"],
                report_user_err (C.Variable "FLOUNDER_ERR_INVALID_STATE")],
        C.SBlank,

        C.SComment "Retry send",
        C.If (C.Variable "tx_notify")
            [
            localvar (C.Struct "event_closure") "retry_closure"
                (Just $ C.StructConstant "event_closure" [
                ("handler", C.Variable $ tx_handler_name p ifn), ("arg", C.Variable "arg")]),
            C.Ex $ C.Assignment errvar (C.Call "ump_chan_register_send" [
                chanaddr, C.DerefField bindvar "waitset", C.Variable "retry_closure"]),
            C.Ex $ C.Call "assert" [C.Call "err_is_ok" [errvar]]
            ] []
    ]
    where
        inc_fragnum = C.Ex $ C.PostInc $ C.DerefField bindvar "tx_msg_fragment"
        tx_msgnum_field = C.DerefField bindvar "tx_msgnum"
        umpst = C.DerefField my_bindvar "ump_state"
        capst = umpst `C.FieldOf` "capst"

        -- variables will be needed only if there are non-string/buffer messages
        msgvars_will_be_used
            = or [or $ map isMsgFragment frags | MsgSpec _ frags _ <- msgs]
            where
                isMsgFragment (MsgFragment _) = True
                isMsgFragment _ = False

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
                         $ (tx_handler_case p ifn mn frag) ++ gen_epilog i
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
            gen_epilog i
                | i + 1 == length msgfrags =
                [-- send a notification, now we've done a complete message
                 C.StmtList $ ump_notify p,
                 inc_fragnum,
                 -- if the last fragment succeeds, and we've sent all the caps, we're done
                -- otherwise we'll need to wait to finish sending the caps
                 if caps /= [] then
                    C.If (C.Binary C.Equals (capst `C.FieldOf` "tx_capnum")
                            (C.NumConstant $ toInteger $ length caps + 1))
                        finished_send []
                    else C.StmtList finished_send,
                C.ReturnVoid]

                | otherwise = -- more fragments to go
                [inc_fragnum, C.SComment "fall through to next fragment"]
        statevar = C.DerefField my_bindvar "ump_state"
        chanaddr = C.AddressOf $ C.FieldOf statevar "chan"

tx_handler_case :: UMPParams -> String -> String -> MsgFragment -> [C.Stmt]
tx_handler_case p ifn mn (MsgFragment words) = [
    C.SComment "send the next fragment",
    C.Ex $ C.Assignment ump_token binding_outgoing_token,
    C.Ex $ C.Assignment msgvar $ C.Call "ump_chan_get_next" [chanaddr, ctrladdr],
    C.SComment "check if we can send another message",
    C.If (C.Unary C.Not msgvar)
      [C.Ex $ C.Assignment (C.Variable "tx_notify") (C.Variable "true"),
       C.Break] [],
    C.Ex $ C.Call "flounder_stub_ump_control_fill"
                [stateaddr, ctrladdr, C.Variable $ msg_enum_elem_name ifn mn],
    C.StmtList
        [C.Ex $ C.Assignment (msgword n) (fragment_word_to_expr (ump_arch p) ifn mn (words !! n))
         | n <- [0 .. length(words) - 1], words !! n  /= []],
    C.Ex $ C.Call "flounder_stub_ump_barrier" [],
    C.Ex $ C.Assignment msgheader ctrlvar]
    where
        ctrlvar = C.Variable "ctrl"
        ctrladdr = C.AddressOf ctrlvar
        statevar = C.DerefField my_bindvar "ump_state"
        stateaddr = C.AddressOf statevar
        msgvar = C.Variable "msg"
        msgword n = C.DerefField msgvar "data" `C.SubscriptOf` (C.NumConstant $ toInteger n)
        msgheader = C.DerefField msgvar "header" `C.FieldOf` "control"
        chanaddr = C.AddressOf $ C.FieldOf statevar "chan"
        ump_token = C.DerefField chanst "token"
        umpst = C.DerefField my_bindvar "ump_state"
        chanst = C.AddressOf umpst
        binding_outgoing_token = C.DerefField bindvar "outgoing_token"

tx_handler_case p ifn mn (OverflowFragment (StringFragment af)) =
    [C.Ex $ C.Assignment ump_token binding_outgoing_token,
     C.Ex $ C.Assignment errvar (C.Call "flounder_stub_ump_send_string" args),
     C.If (C.Call "err_is_fail" [errvar]) [
        -- have we run out of space in the buffer?
        C.If (C.Binary C.Equals (C.Call "err_no" [errvar])
                                (C.Variable "FLOUNDER_ERR_BUF_SEND_MORE"))
            -- yes, better send a notify
            [C.Ex $ C.Assignment (C.Variable "tx_notify") (C.Variable "true")]
            -- no, some other error happened
            [C.SComment "Permanent error, report to user",
             report_user_tx_err errvar],
        C.Break] []]
    where
        args = [chan_arg, msgnum_arg, string_arg, pos_arg, len_arg]
        chan_arg = C.AddressOf $ C.DerefField my_bindvar "ump_state"
        msgnum_arg = C.Variable $ msg_enum_elem_name ifn mn
        string_arg = argfield_expr TX mn af
        pos_arg = C.AddressOf $ C.DerefField bindvar "tx_str_pos"
        len_arg = C.AddressOf $ C.DerefField bindvar "tx_str_len"
        ump_token = C.DerefField chanst "token"
        umpst = C.DerefField my_bindvar "ump_state"
        chanst = C.AddressOf umpst
        binding_outgoing_token = C.DerefField bindvar "outgoing_token"

tx_handler_case p ifn mn (OverflowFragment (BufferFragment _ afn afl)) =
    [C.Ex $ C.Assignment ump_token binding_outgoing_token,
     C.Ex $ C.Assignment errvar (C.Call "flounder_stub_ump_send_buf" args),
     C.If (C.Call "err_is_fail" [errvar]) [
        -- have we run out of space in the buffer?
        C.If (C.Binary C.Equals (C.Call "err_no" [errvar])
                                (C.Variable "FLOUNDER_ERR_BUF_SEND_MORE"))
            -- yes, better send a notify
            [C.Ex $ C.Assignment (C.Variable "tx_notify") (C.Variable "true")]
            -- no, some other error happened
            [C.SComment "Permanent error, report to user",
             report_user_tx_err errvar],
        C.Break] []]
    where
        args = [chan_arg, msgnum_arg, buf_arg, len_arg, pos_arg]
        chan_arg = C.AddressOf $ C.DerefField my_bindvar "ump_state"
        msgnum_arg = C.Variable $ msg_enum_elem_name ifn mn
        buf_arg = argfield_expr TX mn afn
        len_arg = argfield_expr TX mn afl
        pos_arg = C.AddressOf $ C.DerefField bindvar "tx_str_pos"
        ump_token = C.DerefField chanst "token"
        umpst = C.DerefField my_bindvar "ump_state"
        chanst = C.AddressOf umpst
        binding_outgoing_token = C.DerefField bindvar "outgoing_token"

tx_fn :: UMPParams -> String -> [TypeDef] -> MessageDef -> MsgSpec -> C.Unit
tx_fn p ifn typedefs msg@(Message mtype n args _) (MsgSpec _ _ caps) =
    C.FunctionDef C.Static (C.TypeName "errval_t") (tx_fn_name p ifn n) params body
    where
        params = [binding_param2 ifn, cont_param] ++ (
                    concat [ msg_argdecl TX ifn a | a <- args ])
        cont_param = C.Param (C.Struct "event_closure") intf_cont_var
        body = [
            localvar (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var (Just $ C.Variable (intf_bind_var ++ "_")),
            -- check message size does not exceed receive buffer
            C.StmtList [ tx_fn_arg_check_size ifn typedefs n a | a <- args ],
            C.Ex $ C.Call "thread_mutex_lock" [C.AddressOf $ C.DerefField bindvar "send_mutex"],
            C.Ex $ C.Assignment binding_error (C.Variable "SYS_ERR_OK"),
            C.SComment "check that we can accept an outgoing message",
            C.If (C.Binary C.NotEquals tx_msgnum_field (C.NumConstant 0))
                [C.Ex $ C.Call "thread_mutex_unlock" [C.AddressOf $ C.DerefField bindvar "send_mutex"],
                 C.Return $ C.Variable "FLOUNDER_ERR_TX_BUSY"] [],
            C.SBlank,
            C.SComment "register send continuation",
            C.StmtList $ register_txcont (C.Variable intf_cont_var),
            C.SBlank,
            C.SComment "store message number and arguments",
            C.Ex $ C.Assignment binding_outgoing_token (C.Binary C.BitwiseAnd binding_incoming_token (C.Variable "~1" )),
            C.Ex $ C.Call "thread_get_outgoing_token" [C.AddressOf binding_outgoing_token],
            C.Ex $ C.Assignment tx_msgnum_field (C.Variable $ msg_enum_elem_name ifn n),
            C.Ex $ C.Assignment tx_msgfrag_field (C.NumConstant 0),
            C.StmtList [ tx_arg_assignment ifn typedefs n a | a <- args ],
            C.StmtList $ start_send (ump_drv p) ifn n args,
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
                    [umpst `C.FieldOf` "chan" `C.FieldOf` "monitor_binding",
                     C.AddressOf $ bindvar `C.DerefField` "event_qnode",
                     C.StructConstant "event_closure" [
                        ("handler", C.Variable $ monitor_mutex_cont_name p ifn),
                        ("arg", bindvar)]],
                 C.SBlank]
                else [],
            C.SComment "try to send!",
            C.Ex $ C.Call "thread_mutex_lock" [C.AddressOf $ C.DerefField bindvar "rxtx_mutex"],
            C.Ex $ C.Call (tx_handler_name p ifn) [bindvar],
            C.Ex $ C.Call "thread_mutex_unlock" [C.AddressOf $ C.DerefField bindvar "rxtx_mutex"],
            C.StmtList $ (if caps /= [] then block_sending_with_caps p ifn else block_sending) (C.Variable intf_cont_var),
            C.Ex $ C.Call "thread_mutex_unlock" [C.AddressOf $ C.DerefField bindvar "send_mutex"],
            C.SBlank,
            C.Return binding_error
            ]
        umpvar = C.Cast (C.Ptr $ C.Struct $ my_bind_type p ifn) bindvar
        umpst = C.DerefField umpvar "ump_state"
        capst = umpst `C.FieldOf` "capst"
        tx_msgnum_field = C.DerefField bindvar "tx_msgnum"
        tx_msgfrag_field = C.DerefField bindvar "tx_msg_fragment"
        binding_incoming_token = C.DerefField bindvar "incoming_token"
        binding_outgoing_token = C.DerefField bindvar "outgoing_token"

block_sending_with_caps :: UMPParams -> String -> C.Expr -> [C.Stmt]
block_sending_with_caps p ifn cont_ex = [
    C.If (C.Binary C.Equals (cont_ex `C.FieldOf` "handler") (C.Variable "blocking_cont"))
        [C.If (C.Binary C.Equals binding_error (C.Variable "SYS_ERR_OK")) [
            localvar (C.Ptr $ C.Struct $ my_bind_type p ifn)
                 my_bind_var_name (Just $ C.Cast (C.Ptr C.Void) $ bindvar),
            localvar (C.Ptr $ C.Struct "waitset") "ws" (Just $ C.Call "flounder_support_get_current_monitor_waitset" [monitor_binding]),

            C.Ex $ C.Assignment binding_error $ C.Call "flounder_support_change_monitor_waitset" [monitor_binding, C.DerefField bindvar "waitset"],
            C.If (C.Binary C.Equals binding_error (C.Variable "SYS_ERR_OK")) [
                C.Ex $ C.Assignment (C.DerefField tx_cont_chanstate "trigger") $ C.CallInd (bindvar `C.DerefField` "get_receiving_chanstate") [bindvar],
                C.Ex $ C.Assignment binding_error $ C.Call "wait_for_channel" [C.DerefField bindvar "waitset", tx_cont_chanstate, C.AddressOf binding_error]] [],
            C.If (C.Binary C.Equals binding_error (C.Variable "SYS_ERR_OK")) [
                C.Ex $ C.Assignment binding_error $ C.Call "flounder_support_change_monitor_waitset" [monitor_binding, C.Variable "ws"]] []
            ] [
            C.Ex $ C.Call "flounder_support_deregister_chan" [tx_cont_chanstate]
            ]
        ] []
    ] where
        errvar = C.Variable "_err"
        mask = C.CallInd (C.DerefField bindvar "get_receiving_chanstate") [bindvar]
        tx_cont_chanstate = C.AddressOf $ bindvar `C.DerefField` "tx_cont_chanstate"
        umpst = C.DerefField my_bindvar "ump_state"
        chan = umpst `C.FieldOf` "chan"
        monitor_binding = chan `C.FieldOf` "monitor_binding"

tx_vtbl :: UMPParams -> String -> [MessageDef] -> C.Unit
tx_vtbl p ifn ml =
    C.StructDef C.Static (intf_vtbl_type ifn TX) (tx_vtbl_name p ifn) fields
    where
        fields = [let mn = msg_name m in (mn, tx_fn_name p ifn mn) | m <- ml]

monitor_mutex_cont :: UMPParams -> String -> C.Unit
monitor_mutex_cont p ifn =
    C.FunctionDef C.Static C.Void (monitor_mutex_cont_name p ifn) [C.Param (C.Ptr C.Void) "arg"] [
        localvar (C.Ptr $ C.Struct $ my_bind_type p ifn) my_bind_var_name (Just $ C.Variable "arg"),
        C.Ex $ C.Call "assert" [C.Unary C.Not (capst `C.FieldOf` "monitor_mutex_held")],
        C.Ex $ C.Assignment (capst `C.FieldOf` "monitor_mutex_held") (C.Variable "true"),
        C.If (capst `C.FieldOf` "rx_cap_ack")
            [C.Ex $ C.Call (tx_cap_handler_name p ifn) [my_bindvar]] []
    ]
    where
        statevar = C.DerefField my_bindvar "ump_state"
        capst = statevar `C.FieldOf` "capst"

rx_handler :: UMPParams -> String -> [TypeDef] -> [MessageDef] -> [MsgSpec] -> C.Unit
rx_handler p ifn typedefs msgdefs msgs =
    C.FunctionDef C.NoScope C.Void (rx_handler_name p ifn) [C.Param (C.Ptr C.Void) "arg"] [
        handler_preamble p ifn,

        -- local variables
        localvar (C.Volatile $ C.Ptr $ C.Struct "ump_message") "msg" Nothing,
        localvar (C.TypeName "int") "msgnum" Nothing,
        localvar (C.TypeName "int") "__attribute__ ((unused)) no_register" (Just $ C.NumConstant 0),
        localvar (C.TypeName "int") "call_msgnum" $ Just $ C.NumConstant 0,

        C.Ex $ C.Call "thread_mutex_lock" [C.AddressOf $ C.DerefField bindvar "rxtx_mutex"],
        C.SBlank,

        C.While (C.Variable "true") loopbody,
        C.SBlank,

        C.Label "out",
        C.If (C.Unary C.Not (C.Variable "no_register"))
            [C.StmtList $ register_recv p ifn] [],
        C.SBlank,

        -- XXX: hack around the AST to get an attribute on this label, which may not be used
        C.Label "out_no_reregister",
        C.Ex $ C.Variable "__attribute__((unused))",

        C.If (C.Variable "call_msgnum") [C.Ex $ C.Assignment rx_msgnum_field (C.NumConstant 0)] [],
        C.Ex $ C.Call "thread_mutex_unlock" [C.AddressOf $ C.DerefField bindvar "rxtx_mutex"],
        C.Switch (C.Variable "call_msgnum") call_cases [C.Break]
        ]
    where
        loopbody = [
            C.SComment "try to retrieve a message from the channel",
            C.Ex $ C.Assignment errvar
                    $ C.Call "ump_chan_recv" [chanaddr,
                            C.AddressOf $ C.Variable "msg"],

            C.SComment "check if we succeeded",
            C.If (C.Call "err_is_fail" [errvar])
                -- if err_is_fail, check err_no
                [C.If (C.Binary C.Equals (C.Call "err_no" [errvar])
                                         (C.Variable "LIB_ERR_NO_UMP_MSG"))
                    [C.SComment "no message", C.Break]
                    [C.SComment "real error",
                     report_user_err $ C.Call "err_push"
                                   [errvar, C.Variable "LIB_ERR_UMP_CHAN_RECV"],
                     C.Ex $ C.Call "thread_mutex_unlock" [C.AddressOf $ C.DerefField bindvar "rxtx_mutex"],
                     C.ReturnVoid] ]
                [],
            C.SBlank,

            C.SComment "process control word",
            C.Ex $ C.Assignment (C.Variable "msgnum")
                 $ C.Call "flounder_stub_ump_control_process"
                    [stateaddr,
                     C.Variable "msg" `C.DerefField` "header" `C.FieldOf` "control"],
            C.SBlank,

            C.SComment "is this a binding message of connect/accept?",
            C.If (C.Binary C.Equals (C.Variable "msgnum") (C.Variable "FL_UMP_BIND")) [
              C.Ex $ C.Call "ump_chan_free_message" [C.Variable "msg"],
                 C.If ((C.Binary C.Equals (C.DerefField my_bindvar "is_client")) (C.Variable "1")) [
                  C.SComment "Client should not recv bind messages. Ignore.",
                  C.Continue] [],
              C.SComment "handle bind reply: calling bind callback",
              C.Ex $ C.CallInd (bindvar `C.DerefField` "bind_cont")
                  [bindvar `C.DerefField` "st", errvar, bindvar],
                  C.Ex $ C.Call (tx_bind_reply_fn_name p ifn) [my_bindvar],
              C.Continue] [],
            C.SBlank,

            C.SComment "is this a binding reply message of connect/accept?",
            C.If (C.Binary C.Equals (C.Variable "msgnum") (C.Variable "FL_UMP_BIND_REPLY")) [
               C.Ex $ C.Call "ump_chan_free_message" [C.Variable "msg"],
               C.If ((C.Binary C.Equals (C.DerefField my_bindvar "is_client")) (C.Variable "0")) [
                C.SComment "Server should not recv bind messages. Ignore.",
                C.Continue] [],
              C.SComment "handle bind: calling connect callback",
              C.Ex $ C.CallInd (bindvar `C.DerefField` "bind_cont")
                  [bindvar `C.DerefField` "st", errvar, bindvar],
              C.Continue] [],
            C.SBlank,

            C.SComment "is this a cap ack for a pending tx message",
            C.If (C.Binary C.Equals (C.Variable "msgnum") (C.Variable "FL_UMP_CAP_ACK"))
                [C.Ex $ C.Call "ump_chan_free_message" [C.Variable "msg"],
                 C.Ex $ C.Call "assert" [C.Unary C.Not (capst `C.FieldOf` "rx_cap_ack")],
                 C.Ex $ C.Assignment (capst `C.FieldOf` "rx_cap_ack") (C.Variable "true"),
                 C.If (capst `C.FieldOf` "monitor_mutex_held")
                    [C.Ex $ C.Call (tx_cap_handler_name p ifn) [my_bindvar]] [],
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
            C.Switch rx_msgnum_field msgnum_cases bad_msgnum
            ]

        tx_is_busy = C.Binary C.Or
                        (capst `C.FieldOf` "tx_cap_ack")
                        (C.Binary C.NotEquals
                            (bindvar `C.DerefField` "tx_msgnum")
                            (C.NumConstant 0))
        run_tx = C.Ex $ C.Call (tx_handler_name p ifn) [my_bindvar]

        statevar = C.DerefField my_bindvar "ump_state"
        stateaddr = C.AddressOf statevar
        capst = statevar `C.FieldOf` "capst"
        chanaddr = C.AddressOf $ statevar `C.FieldOf` "chan"
        msgdata = C.Variable "msg" `C.DerefField` "data"
        rx_msgnum_field = C.DerefField bindvar "rx_msgnum"
        rx_msgfrag_field = C.DerefField bindvar "rx_msg_fragment"

        call_cases = [C.Case (C.Variable $ msg_enum_elem_name ifn mn) (call_msgnum_case msgdef msg)
                            | (msgdef, msg@(MsgSpec mn _ caps)) <- zip msgdefs msgs, caps == []]

        call_msgnum_case msgdef@(Message mtype mn msgargs _) (MsgSpec _ frags caps) =
            [C.StmtList $ call_handler (ump_drv p) ifn typedefs mtype mn msgargs, C.Break]

        msgnum_cases = [C.Case (C.Variable $ msg_enum_elem_name ifn mn) (msgnum_case msgdef msg)
                            | (msgdef, msg@(MsgSpec mn _ _)) <- zip msgdefs msgs]

        msgnum_case msgdef@(Message _ _ msgargs _) (MsgSpec mn frags caps) = [
            C.Switch rx_msgfrag_field
                [C.Case (C.NumConstant $ toInteger i) $
                 (if i == 0 then
                    -- first fragment of a message
                    start_recv (ump_drv p) ifn typedefs mn msgargs ++
                    (if caps /= [] then [
                        -- + with caps received
                        C.Ex $ C.Call "flounder_stub_ump_send_cap_ack" [C.AddressOf umpst],
                        -- C.Ex $ C.Assignment
                        --     (capst `C.FieldOf` "tx_cap_ack") (C.Variable "true"),
                        C.Ex $ C.Assignment
                            (capst `C.FieldOf` "rx_capnum") (C.NumConstant 0)
                        ] else [])
                       else []) ++
                    (msgfrag_case msgdef frag caps (i == 0) (i == length frags - 1))
                 | (frag, i) <- zip frags [0..] ]
                bad_msgfrag,
            C.Break]
            where
                umpst = C.DerefField my_bindvar "ump_state"

        bad_msgnum = [report_user_err $ C.Variable "FLOUNDER_ERR_RX_INVALID_MSGNUM",
                      C.Goto "out"]

        bad_msgfrag = [report_user_err $ C.Variable "FLOUNDER_ERR_INVALID_STATE",
                      C.Goto "out"]

        msgfrag_case :: MessageDef -> MsgFragment -> [CapFieldTransfer] -> Bool -> Bool -> [C.Stmt]
        msgfrag_case msg@(Message _ mn _ _) (MsgFragment wl) caps isFirst isLast = [
            C.StmtList $ concat [store_arg_frags (ump_arch p) ifn mn msgdata word 0 afl
                                 | (afl, word) <- zip wl [0..]],
            (if isFirst then C.Ex $ C.Assignment binding_incoming_token ump_token else C.SBlank),
            C.SBlank,
            C.Ex $ C.Call "ump_chan_free_message" [C.Variable "msg"],
            C.StmtList $ msgfrag_case_prolog msg caps isLast,
            C.Goto "out"]
            where
                ump_token = C.Variable "msg" `C.DerefField` "header" `C.FieldOf` "control" `C.FieldOf` "token"
                umpst = C.DerefField my_bindvar "ump_state"
                chanst = C.AddressOf umpst
                binding_incoming_token = C.DerefField bindvar "incoming_token"

        msgfrag_case msg@(Message _ mn _ _) (OverflowFragment (StringFragment af)) caps isFirst isLast = [
            C.Ex $ C.Assignment errvar (C.Call "flounder_stub_ump_recv_string" args),
            (if isFirst then C.Ex $ C.Assignment binding_incoming_token ump_token else C.SBlank),
            C.Ex $ C.Call "ump_chan_free_message" [C.Variable "msg"],
            C.If (C.Call "err_is_ok" [errvar])
                (msgfrag_case_prolog msg caps isLast)
                -- error from string receive code, check if it's permanent
                [C.If (C.Binary C.NotEquals
                        (C.Call "err_no" [errvar])
                        (C.Variable "FLOUNDER_ERR_BUF_RECV_MORE"))
                    [report_user_err errvar] -- real error
                    [] -- will receive more next time
                ],
            C.Break]
            where
                args = [msg_arg, string_arg, pos_arg, len_arg, max_size]
                msg_arg = C.Variable "msg"
                string_arg = argfield_expr RX mn af
                pos_arg = C.AddressOf $ C.DerefField bindvar "rx_str_pos"
                len_arg = C.AddressOf $ C.DerefField bindvar "rx_str_len"
                ump_token = C.Variable "msg" `C.DerefField` "header" `C.FieldOf` "control" `C.FieldOf` "token"
                umpst = C.DerefField my_bindvar "ump_state"
                chanst = C.AddressOf umpst
                binding_incoming_token = C.DerefField bindvar "incoming_token"
                max_size = C.SizeOf $ string_arg

        msgfrag_case msg@(Message _ mn _ _) (OverflowFragment (BufferFragment _ afn afl)) caps isFirst isLast = [
            C.Ex $ C.Assignment errvar (C.Call "flounder_stub_ump_recv_buf" args),
            C.Ex $ C.Call "ump_chan_free_message" [C.Variable "msg"],
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
                args = [msg_arg, buf_arg, len_arg, pos_arg, max_size]
                msg_arg = C.Variable "msg"
                buf_arg = C.Cast (C.Ptr C.Void) $ argfield_expr RX mn afn
                len_arg = C.AddressOf $ argfield_expr RX mn afl
                pos_arg = C.AddressOf $ C.DerefField bindvar "rx_str_pos"
                ump_token = C.Variable "msg" `C.DerefField` "header" `C.FieldOf` "control" `C.FieldOf` "token"
                umpst = C.DerefField my_bindvar "ump_state"
                chanst = C.AddressOf umpst
                binding_incoming_token = C.DerefField bindvar "incoming_token"
                max_size = C.SizeOf $ argfield_expr RX mn afn


        msgfrag_case_prolog :: MessageDef -> [CapFieldTransfer] -> Bool -> [C.Stmt]
        -- intermediate fragment
        msgfrag_case_prolog _ _ False = [rx_fragment_increment]

        -- last fragment: call handler and zero message number
        -- if we're expecting any caps, only do so if we've received them all
        msgfrag_case_prolog (Message mtype mn msgargs _) caps True
            | caps == [] = call_callback
            | otherwise = [
                rx_fragment_increment,
                C.Ex $ C.Assignment (C.DerefField message_chanstate "trigger") $ C.Call "monitor_bind_get_receiving_chanstate" [ump_chan `C.DerefField` "monitor_binding"],
                C.Goto "out_no_reregister"]
             where
                call_callback = [C.StmtList $ finished_recv_nocall (ump_drv p) ifn typedefs mtype mn msgargs, C.Goto "out"]
                ump_chan = C.AddressOf $ statevar `C.FieldOf` "chan"
                message_chanstate = C.Binary C.Plus (C.DerefField bindvar "message_chanstate") (C.Variable $ msg_enum_elem_name ifn mn)

        rx_fragment_increment
            = C.Ex $ C.PostInc $ C.DerefField bindvar "rx_msg_fragment"

cap_rx_handler :: UMPParams -> String -> [TypeDef] -> [MessageDef] -> [MsgSpec] -> C.Unit
cap_rx_handler p ifn typedefs msgdefs msgspecs
    = C.FunctionDef C.Static C.Void (cap_rx_handler_name p ifn)
        [C.Param (C.Ptr C.Void) "arg",
         C.Param (C.TypeName "errval_t") "success",
         C.Param (C.Struct "capref") "cap",
         C.Param (C.TypeName "uint32_t") "capid"]
        [handler_preamble p ifn,
        localvar (C.TypeName "int") "call_msgnum" $ Just $ C.NumConstant 0,
        localvar (C.TypeName "int") "__attribute__ ((unused)) no_register" (Just $ C.NumConstant 0),

         C.Ex $ C.Call "assert" [C.Binary C.Equals
                                       (C.Variable "capid")
                                       (capst `C.FieldOf` "rx_capnum")],
         C.SBlank,

         C.Ex $ C.Call "thread_mutex_lock" [C.AddressOf $ C.DerefField bindvar "rxtx_mutex"],
         C.SComment "Check if there's an associated error",
         C.SComment "FIXME: how should we report this to the user? at present we just deliver a NULL capref",
         C.If (C.Call "err_is_fail" [C.Variable "success"])
              [C.Ex $ C.Call "DEBUG_ERR" [C.Variable "success",
                                          C.StringConstant "error in cap transfer"]]
              [],
         C.SBlank,

         C.SComment "Switch on current incoming message",
         C.Switch (C.DerefField bindvar "rx_msgnum") cases
            [C.Ex $ C.Call "assert"
                    [C.Unary C.Not $ C.StringConstant "invalid message number"],
             report_user_err (C.Variable "FLOUNDER_ERR_INVALID_STATE")],
        C.If (C.Variable "call_msgnum") [C.Ex $ C.Assignment rx_msgnum_field (C.NumConstant 0)] [],
        C.Ex $ C.Call "thread_mutex_unlock" [C.AddressOf $ C.DerefField bindvar "rxtx_mutex"],
        C.Switch (C.Variable "call_msgnum") call_cases [C.Break]
        ]
    where
        umpst = C.DerefField my_bindvar "ump_state"
        capst = umpst `C.FieldOf` "capst"
        cases = [C.Case (C.Variable $ msg_enum_elem_name ifn mn)
                        (cap_rx_handler_case p ifn typedefs mn msgdef (length frags) caps)
                 | (MsgSpec mn frags caps, msgdef) <- zip msgspecs msgdefs, caps /= []]
        rx_msgnum_field = C.DerefField bindvar "rx_msgnum"
        call_cases = [C.Case (C.Variable $ msg_enum_elem_name ifn mn) (call_msgnum_case msgdef msg)
                            | (msgdef, msg@(MsgSpec mn _ caps)) <- zip msgdefs msgspecs, caps /= []]

        call_msgnum_case msgdef@(Message mtype mn msgargs _) (MsgSpec _ frags caps) =
            [C.StmtList $ call_handler (ump_drv p) ifn typedefs mtype mn msgargs, C.Break]

cap_rx_handler_case :: UMPParams -> String -> [TypeDef] -> String -> MessageDef -> Int -> [CapFieldTransfer] -> [C.Stmt]
cap_rx_handler_case p ifn typedefs mn (Message mtype _ msgargs _) nfrags caps = [
    C.SComment "Switch on current incoming cap",
    C.Switch (C.PostInc $ capst `C.FieldOf` "rx_capnum") cases
            [C.Ex $ C.Call "assert"
                    [C.Unary C.Not $ C.StringConstant "invalid cap number"],
             report_user_err (C.Variable "FLOUNDER_ERR_INVALID_STATE")],
    C.Break]
    where
        umpst = C.DerefField my_bindvar "ump_state"
        capst = umpst `C.FieldOf` "capst"
        cases = [C.Case (C.NumConstant $ toInteger i) $ subcase cap i
                 | (cap, i) <- zip caps [0..]]

        subcase :: CapFieldTransfer -> Int -> [C.Stmt]
        subcase (CapFieldTransfer _ cap) ncap = [
            C.Ex $ C.Assignment (argfield_expr RX mn cap) (C.Variable "cap"),
            if is_last then
                -- if this was the last cap, and we've received all the other fragments, we're done
                C.If (C.Binary C.Equals rx_msgfrag_field (C.NumConstant $ toInteger nfrags))
                    [
                        C.StmtList $ finished_recv_nocall (ump_drv p) ifn typedefs mtype mn msgargs,
                        C.Ex $ C.Assignment (C.DerefField message_chanstate "trigger") $ C.CallInd (bindvar `C.DerefField` "get_receiving_chanstate") [bindvar],
                        C.If (C.Unary C.Not (C.Variable "no_register"))
                            [C.StmtList $ register_recv p ifn] [],
                        C.SBlank
                    ] []
                else C.StmtList [],
            C.Break]
            where
                rx_msgfrag_field = C.DerefField bindvar "rx_msg_fragment"
                is_last = (ncap + 1 == length caps)
                statevar = C.DerefField my_bindvar "ump_state"
                ump_chan = C.AddressOf $ statevar `C.FieldOf` "chan"
                message_chanstate = C.Binary C.Plus (C.DerefField bindvar "message_chanstate") (C.Variable $ msg_enum_elem_name ifn mn)

-- generate the code to register for receive notification
register_recv :: UMPParams -> String -> [C.Stmt]
register_recv p ifn = [
    C.SComment "register for receive notification",
    C.StmtList $ ump_register_recv p ifn,
    C.If (C.Call "err_is_fail" [errvar])
        [report_user_err $ C.Call "err_push" [errvar, C.Variable "LIB_ERR_CHAN_REGISTER_RECV"]]
        [] ]

-- generate the code to set cap rx/tx handlers
setup_cap_handlers :: UMPParams -> String -> [C.Stmt]
setup_cap_handlers p ifn = [
    C.SComment "setup cap handlers",
    C.Ex $ C.Assignment (C.FieldOf handlers "st") my_bindvar,
    C.Ex $ C.Assignment (C.FieldOf handlers "cap_receive_handler")
                        (C.Variable $ cap_rx_handler_name p ifn) ]
    where
        chanvar = my_bindvar `C.DerefField` "ump_state" `C.FieldOf` "chan"
        handlers = chanvar `C.FieldOf` "cap_handlers"
