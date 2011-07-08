%include polycode.fmt

%if false
  Flounder2: an even more simpler IDL for Barrelfish
   
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif


\section{The Code Back-End}

Here, we are provided a syntactically correct syntax tree and we want
to compile it into a (valid) C code.

> module CodeBackend where

%if false

> import Data.List
> import Data.Char

> import qualified CSyntax as C
> import Backend
> import Syntax

%endif

As for header compilation, there are two tasks. First, print out a
nice preamble, then do the real work.

> compile :: String -> String -> Side -> Interface -> String
> compile infile baseName side interface@(Interface interfaceName _ _) =
>     preamble interface infile
>  ++ unlines [
>          C.include "inttypes",
>          C.include "flounder/glue_internal",
>          C.include $ "if/" ++ baseName ++ "_types"
>         ]
>  ++ compileInterface side interface


Just as for the compilation of headers, the compilation to C code goes
into several steps. Actually, 2 steps. The first one consists of some
general definitions, used both on client and server side. Follows
server-side or client-side specific definitions. First, common
definitions are compiled in both cases. Then, we only compile the
server-specific code on the server side, and conversely for client
code.

> compileInterface :: Side -> Interface -> String
> compileInterface side interface@(Interface name description declaration) = 
>     let (_, messages) = partitionTypesMessages declaration in
>         unlines $ C.multi_comment1 "Common Definitions"
>                ++ compileCommonDefinitions side name messages
>                ++ case side of 
>                    ServerSide -> 
>                        C.multi_comment1 "Server-side Definitions"
>                        ++ compileServerSide name messages
>                    ClientSide ->
>                       C.multi_comment1 "Client-side Definitions"
>                       ++ compileClientSide name messages


\subsection{Compiling Common Definitions}


The common definitions to both client and server sides are the stubs
associated to messages. So, we will treat each message definition in
turn and generate a stub accordingly.

> compileCommonDefinitions :: Side -> String -> [MessageDef] -> [String] 
> compileCommonDefinitions side interfaceName messages =
>     (concat $ map (compileCommonDefinition side interfaceName) messages)
>     ++ compileDisconnectFunction side interfaceName

Now we are to compile a single message definition. For example, given
the following definition:
\begin{verbatim}
message initmsg(uint32 someint, cap somecap, iref some_if, 
	        string some_string);
\end{verbatim}

We have to generate the following code:
\begin{verbatim}
static errval_t ifname_initmsg_stub( void *_st,  
                                     uint32_t someint,  
                                     cap_t somecap,  
                                     iref_t some_if, 
                                     const char *some_string )
{
    struct chips_connection *_conn = (struct chips_connection *) _st;
    assert(_conn != NULL);
    // Create a Message buffer
    struct msgbuf *_msg = _conn->f->create_msg(_conn);
    assert(_msg != NULL);
    errval_t _err = FLOUNDER_ERR_CREATE_MSG;
    if (!_msg) {FL_FAILED_DEBUG("create_msg failed"); goto failure;}
    // Marshall the message code
    if (err_is_fail(_err = FL_MARSHAL_uint16(_msg, IFNAME_INITMSG_FC))) {FL_FAILED_DEBUG(""); goto failure;}
    // Marshall all the arguments (if any)
    if (err_is_fail(_err = FL_MARSHAL_uint32(_msg, someint))) {FL_FAILED_DEBUG(""); goto failure;}
    if (err_is_fail(_err = FL_MARSHAL_cap(_msg, somecap))) {FL_FAILED_DEBUG(""); goto failure;}
    if (err_is_fail(_err = FL_MARSHAL_iref(_msg, some_if))) {FL_FAILED_DEBUG(""); goto failure;}
    if (err_is_fail(_err = FL_MARSHAL_string(_msg, some_string))) {FL_FAILED_DEBUG(""); goto failure;}
    // Call send
    return _conn->f->send(_msg);
    failure:
    if (_msg != NULL) _msg->f->destroy(_msg);
    return _err;
}
\end{verbatim}

This is achieved by this function:

> compileCommonDefinition :: Side -> String -> MessageDef -> [String]

Firstly, we don't need to compile the marshaling stubs for a call on
server side, and similarly for a response on client side:

> compileCommonDefinition ServerSide _ (Message MCall _ _) = [""]
> compileCommonDefinition ClientSide _ (Message MResponse _ _) = [""]

> compileCommonDefinition ClientSide interfaceName message@(RPC messageName rpcArgs) =
>     C.function1 "static" "errval_t"
>      (interfaceName ++ "_" ++ messageName ++ "_stub")
>      (compileRPCDefinitionArgs interfaceName rpcArgs)
>      ([
>       "assert(st != NULL);",
>       "errval_t _err;",
>       "struct flounder_glue_binding *_chan = st->chan;",
>       "// Create a new Message buffer, store the old one",
>       "struct msgbuf *_msg = &st->sendmsg[st->nsendmsg];",
>       "struct msgbuf *rcvd_msg = NULL;",
>       "struct msgbuf *oldmsg = &st->sendmsg[!st->nsendmsg];",
>       "msgbuf_init_dynamic(_msg, 80, 1);",
>       "// Marshall the message code",
>       "if (err_is_fail(_err = FL_MARSHAL_uint16(_msg, " ++ map toUpper interfaceName 
>                                                         ++ "_" 
>                                                         ++ map toUpper messageName 
>                                                         ++ "_FC))) {FL_FAILED_DEBUG(\"\"); goto failure;}",
>       "// Marshall all the arguments (if any)" ]
>       ++ [ "if (err_is_fail(_err = FL_MARSHAL_" ++ qualifyName interfaceName typeArg 
>                                                 ++ "(_msg, " ++ listOfArgs "" arg ++ "))) {FL_FAILED_DEBUG(\"\"); goto failure;}"
>            | RPCArgIn typeArg arg <- rpcArgs ]
>       ++ [ "// Call send",
>            "st->reply_present = false;",
>            "_err = flounder_glue_binding_send_msgbuf(_chan, NOP_CLOSURE, _msg);",
>            "if (err_is_fail(_err)) { FL_FAILED_DEBUG(\"\"); goto failure; }",
>            "msgbuf_destroy(oldmsg);",
>            "st->nsendmsg = !st->nsendmsg;",
>            "// Wait for reply",
>            "assert(_chan->waitset == &st->rpc_waitset);",
>            "while (!st->reply_present) {",
>            "  _err = event_dispatch(&st->rpc_waitset);",
>            "  assert(err_is_ok(_err));",
>            "}",
>            "rcvd_msg = st->replymsg;",
>            "uint16_t _mc;",
>            "if (err_is_fail(_err = FL_UNMARSHAL_uint16(rcvd_msg, &_mc))) {FL_FAILED_DEBUG(\"\"); goto failure;}",
>            "FL_DEBUG_PRINT_TYPE(_mc);",
>            "if (_mc != " ++ map toUpper interfaceName 
>                          ++ "_" 
>                          ++ map toUpper messageName 
>                          ++ "_FC ) { _err = FLOUNDER_ERR_RPC_MISMATCH; FL_FAILED_DEBUG(\"\"); goto failure;}",
>            "// Declaration of overflowing reception variables" ]
>       ++ [ "if (err_is_fail(_err = FL_UNMARSHAL_" ++ qualifyName interfaceName typeArg 
>                    ++ "(rcvd_msg, " ++ listOfArgs "" arg ++ "))) {FL_FAILED_DEBUG(\"\"); goto failure;}" 
>                   | RPCArgOut typeArg arg <- rpcArgs ]
>       ++ [ "if (rcvd_msg != NULL) msgbuf_destroy(rcvd_msg);",
>            "return SYS_ERR_OK;",
>            "failure:",
>            "if (_msg != NULL) msgbuf_destroy(_msg);",
>            "return _err;"
>          ])


> compileCommonDefinition ServerSide interfaceName message@(RPC messageName rpcArgs) =
>     compileCommonDefinition ServerSide interfaceName (serverRpcOutToMessage message)

In the other cases, we generate the code shown above:

> compileCommonDefinition side interfaceName message@(Message _ messageName messageArgs) = 
>    C.function1 "static" "errval_t" 
>              (interfaceName ++ "_" ++ messageName ++ "_stub")
>              (compileCommonDefinitionArgs interfaceName side message)
>              ([
>               "assert(st != NULL);",
>               "struct flounder_glue_binding *_chan = st->chan;",
>               "// Create a new Message buffer, store the old one",
>               "struct msgbuf *_msg = &st->sendmsg[st->nsendmsg];",
>               "struct msgbuf *oldmsg = &st->sendmsg[!st->nsendmsg];",
>               "msgbuf_init_dynamic(_msg, 80, 1);",
>               "// Marshall the message code",
>               "errval_t _err;",
>               "if (err_is_fail(_err = FL_MARSHAL_uint16(_msg, (uint16_t)" ++ map toUpper interfaceName 
>                                              ++ "_" 
>                                              ++ map toUpper messageName 
>                                              ++ "_FC))) {FL_FAILED_DEBUG(\"\"); goto failure;}",
>               "// Marshall all the arguments (if any)" ]
>             ++ [ "if (err_is_fail(_err = FL_MARSHAL_" ++ qualifyName interfaceName typeArg 
>                                    ++ "(_msg, " ++ listOfArgs "" arg ++ "))) {FL_FAILED_DEBUG(\"\"); goto failure;}"
>                  | Arg typeArg arg <- messageArgs ]
>             ++ [ "// Call send",
>                  "_err = flounder_glue_binding_send_msgbuf(_chan, NOP_CLOSURE, _msg);",
>                  "if (err_is_ok(_err)) {",
>                  "  msgbuf_destroy(oldmsg);",
>                  "  st->nsendmsg = !st->nsendmsg;",
>                  "} else {",
>                  "  msgbuf_destroy(_msg);",
>                  "}",
>                  "failure:",
>                  "return _err;"]) 

> serverRpcInToMessage :: MessageDef -> MessageDef
> serverRpcInToMessage (RPC messageName rpcArgs) =
>     Message MResponse messageName args
>         where args = [ let RPCArgIn typeRef arg = rpcArg in
>                        Arg typeRef arg
>                        | rpcArg <- rpcArgs, 
>                          case rpcArg of 
>                            RPCArgIn _ _ -> True
>                            RPCArgOut _ _ -> False ]


> serverRpcOutToMessage :: MessageDef -> MessageDef
> serverRpcOutToMessage (RPC messageName rpcArgs) =
>     Message MResponse messageName args
>         where args = [ let RPCArgOut typeRef arg = rpcArg in
>                        Arg typeRef arg
>                        | rpcArg <- rpcArgs, 
>                          case rpcArg of 
>                            RPCArgIn _ _ -> False
>                            RPCArgOut _ _ -> True ]


Part of the common definitions is also the disconnection function. It
is a simple wrapper arround chips disconnect function. The generated
code looks like this:

\begin{verbatim}
static void ifname_disconnect( void * _st )
{
    struct chips_connection *_conn = (struct chips_connection *)_st;
    _conn->f->disconnect(_conn);
}
\end{verbatim}

As compiled by:

> compileDisconnectFunction :: Side -> String -> [String]
> compileDisconnectFunction side interfaceName =
>     C.static "void" 
>              (interfaceName ++ "_disconnect")
>              [("struct " ++ interfaceName ++ "_" 
>                ++ show side ++ "_response *", "_st")]
>              [
>               "assert(!\"disconnect not implemented\");"
>              ]



\subsection{Compiling Server-side Definitions}

The server-side definitions are composed of:
\begin{enumerate}
        \item The response vtable
        \item The @request@ handler
        \item The @disconnect@ handler
        \item The @connect@ handler
        \item The @listen@ function
\end{enumerate}

> compileServerSide :: String -> [MessageDef] -> [String]
> compileServerSide interfaceName messages =
>     compileServerVtable interfaceName messages
>  ++ compileServerRequestHandler interfaceName messages
>  -- ++ compileServerDisconnectHandler interfaceName
>  ++ compileServerConnectHandler interfaceName
>  ++ compileServerListen interfaceName

We will build these functions in turn, in the following sections.

\subsubsection{Compiling the response vtable}

On the server side, we can already compute the response vtable, with
the marshalling stubs and disconnect function compiled above. This is
something similar to:

\begin{verbatim}
struct ifname_server_response_vtbl the_ifname_server_response_vtbl =
{
    ifname_simplemsg_stub, 
    ifname_withstrmsg_stub, 
    ifname_initmsg_stub, 
    ifname_backwards_stub, 
    ifname_disconnect
};
\end{verbatim}

Generated by:

> compileServerVtable :: String -> [MessageDef] -> [String]
> compileServerVtable interfaceName messages = 
>     ("struct " ++ interfaceName ++ "_server_response_vtbl the_" 
>                      ++ interfaceName ++ "_server_response_vtbl =") :
>     C.block (
>              [ let messageName = case message of
>                                  Message _ messageName _ -> messageName
>                                  RPC name _ -> name
>                in
>                 interfaceName ++ "_" ++ messageName ++ "_stub" ++ ", "
>               | message <- messages,
>                 isBackward message ] 
>              ++ [ interfaceName ++ "_disconnect" ]
>             ) ++ [";"]


\subsubsection{Compiling the Request Handler}

The server request handler takes a service response closure and is
called by chips when receiving a message. Its purpose is to unmarshal
the message and call the appropriate event handler. The code looks
like this:

\begin{verbatim}
 struct msgbuf * ifname_server_request_handler( void * _cl, struct msgbuf * _msg )
{
    // Unmarshall the message code
    struct ifname_service_response *_clp = (struct ifname_service_response *) _cl;
    uintptr_t _mc;
    if (err_is_fail(FL_UNMARSHAL_uint16(_msg, &_mc))) {FL_FAILED_DEBUG(""); goto failure;}
    FL_DEBUG_PRINT_TYPE(_mc);
    // Switch on the message code
    switch (_mc) {
        // Dispatch code (...)
    }
    return NULL;
    failure:
    _msg->f->destroy(_msg);
    return NULL;
}
\end{verbatim}

As generated by:

> compileServerRequestHandler :: String -> [MessageDef] -> [String]
> compileServerRequestHandler interfaceName messages =
>     C.multi_comment1 "Server request handler: called for each request"
>  ++ C.function1 "" "void"
>              (interfaceName ++ "_server_request_handler")
>              [
>               ("void *", "_cl"),
>               ("struct msgbuf *", "_msg")
>              ]
>         ( 
>          [
>           "// Unmarshall the message code",
>           "struct " ++ interfaceName ++ "_service_response *_clp "
>             ++"= (struct " ++ interfaceName ++  "_service_response *) _cl;",
>           "uint16_t _mc;",
>           "if (err_is_fail(FL_UNMARSHAL_uint16(_msg, &_mc))) {FL_FAILED_DEBUG(\"\"); goto failure;}",
>           "FL_DEBUG_PRINT_TYPE(_mc);",
>           "// Switch on the message code"
>           ]
>          ++ (C.switch1 "_mc"
>               [ let messageName = case message of
>                                     Message _ messageName _ -> messageName
>                                     RPC name _ -> name
>                 in
>                 (map toUpper interfaceName ++ 
>                   "_" ++ 
>                  map toUpper messageName ++ 
>                  "_FC",
>                  compileServerDispatchCode interfaceName message)
>                 | message <- messages, 
>                   isForward message ]
>               (C.block [
>                         "{FL_FAILED_DEBUG(\"bad msg ID %\" PRIu16, _mc); goto failure;}"
>                         ]))
>            ++ [ "failure:",
>                 "msgbuf_destroy(_msg);"])

The dispatch code identifies a message and unmarshal it
correspondingly:

\begin{verbatim}
        case IFNAME_SIMPLEMSG_FC:
        {
            uint32_t someint;
            uint64_t tmp_someint;
            if (err_is_fail(FL_UNMARSHAL_uint32(_msg, &tmp_someint))) {FL_FAILED_DEBUG(""); goto failure;}
            someint = tmp_someint;
            _msg->f->destroy(_msg);
            assert(_clp->sclp->f->simplemsg != NULL);
            _clp->sclp->f->simplemsg(_clp,someint);
            break;
        }
        case IFNAME_WITHSTRMSG_FC:
        {
            ifname_alias_type_t at;
            if (err_is_fail(FL_UNMARSHAL_ifname_alias_type(_msg, &at))) {FL_FAILED_DEBUG(""); goto failure;}
            _msg->f->destroy(_msg);
            assert(_clp->sclp->f->withstrmsg != NULL);
            _clp->sclp->f->withstrmsg(_clp,at);
            break;
        }
        case IFNAME_INITMSG_FC:
        {
            uint32_t someint;
            cap_t somecap;
            iref_t some_if;
            char * some_string;
            uint64_t tmp_someint;
            if (err_is_fail(FL_UNMARSHAL_uint32(_msg, &tmp_someint))) {FL_FAILED_DEBUG(""); goto failure;}
            someint = tmp_someint;
            if (err_is_fail(FL_UNMARSHAL_cap(_msg, &somecap))) {FL_FAILED_DEBUG(""); goto failure;}
            if (err_is_fail(FL_UNMARSHAL_iref(_msg, &some_if))) {FL_FAILED_DEBUG(""); goto failure;}
            if (err_is_fail(FL_UNMARSHAL_string(_msg, &some_string))) {FL_FAILED_DEBUG(""); goto failure;}
            _msg->f->destroy(_msg);
            assert(_clp->sclp->f->initmsg != NULL);
            _clp->sclp->f->initmsg(_clp,someint,somecap,some_if,some_string);
            break;
        }
        case IFNAME_FORWARDS_FC:
        {
            _msg->f->destroy(_msg);
            assert(_clp->sclp->f->forwards != NULL);
            _clp->sclp->f->forwards(_clp);
            break;
        }
        default:
        {
            {FL_FAILED_DEBUG("bad msg ID %" PRIuPTR, _mc); goto failure;}
        }
\end{verbatim}

> compileServerDispatchCode :: String -> MessageDef -> [String]
> compileServerDispatchCode interfaceName message@(Message _ messageName messageArgs) = 
>     C.block $
>      [ qualifyType interfaceName typeArg ++ " " ++ (nameOf arg) ++ ";"
>        | Arg typeArg arg <- messageArgs ]
>   ++ [ "if (err_is_fail(FL_UNMARSHAL_" ++ qualifyName interfaceName typeArg 
>                   ++ "(_msg, " ++ listOfArgs "&" arg ++ "))) {FL_FAILED_DEBUG(\"\"); goto failure;}"
>        | Arg typeArg arg <- messageArgs ]
>   ++ [ "msgbuf_destroy(_msg);",
>        "assert(_clp->sclp->f->" ++ messageName ++ " != NULL);",
>        "_clp->sclp->f->" ++ messageName ++ "(_clp" ++ messageArgList ++");",
>        "break;"
>      ]
>   where messageArgList = concat [ "," ++ nameOf arg | Arg _ arg <- messageArgs ]

> compileServerDispatchCode interfaceName message@(RPC rpcName messageArgs) = 
>     compileServerDispatchCode interfaceName (serverRpcInToMessage message)

\subsubsection{Compiling the Disconnect Handler}

In case the client disconnect, we register the following disconnect
callback in chips. It calls, if possible, the disconnect handler
stored in the service response closure. Then, it frees this closure.

\begin{verbatim}
static void server_disconnect_handler( void * cl )
{
    struct ifname_service_response *clp = cl;
    if (clp->sclp->f->_disconnect != NULL) { clp->sclp->f->_disconnect(clp);}
    free(clp);
}
\end{verbatim}

> compileServerDisconnectHandler :: String -> [String]
> compileServerDisconnectHandler interfaceName =
>     C.multi_comment1 "Server disconnect handler" 
>  ++ C.static "void"
>              "server_disconnect_handler"
>              [("void *", "cl")]
>              [ 
>                 "struct " ++ interfaceName ++ "_service_response *clp = cl;",
>                 "if (clp->sclp->f->_disconnect != NULL) { clp->sclp->f->_disconnect(clp);}",
>                 "free(clp);"
>              ]

\subsubsection{Compiling the Connect Handler}

We register the following connect callback to chips. Therefore, when
the server receives a connection, we malloc a per-client closure and
initialize it with a response vtable, the chips connection, and the
shared closure. The per-client closure, request handler, and
disconnect handler are then registered in the chips connection
closure. If there is some service logic to apply at connection time,
as implemented by \verb!_connected!, it is run. If it returns an error
code, chips will abort the connection. This looks like this:

\begin{verbatim}
static errval_t server_connect_handler( void * cl, struct chips_connection * conn )
{
    struct ifname_service_response *concl  = malloc(sizeof(struct ifname_service_response));
    assert(concl != NULL);
    struct ifname_service *closure = cl;
    concl->f = &the_ifname_server_response_vtbl;
    concl->conn = conn;
    concl->sclp = cl;
    conn->closure = concl;
    conn->req_handler = ifname_server_request_handler;
    conn->disconn_handler = server_disconnect_handler;
    if (closure->f->_connected != NULL){
        return closure->f->_connected(cl, concl);
    } else {
        return SYS_ERR_OK;
    }
}
\end{verbatim}

As generated by:

> compileServerConnectHandler :: String -> [String]
> compileServerConnectHandler interfaceName =
>     C.multi_comment1 "Server connect handler: called for each incoming bind"
>  ++ C.static "errval_t"
>              "server_connect_handler"
>              [
>               ("void *", "st"),
>               ("struct flounder_glue_binding *", "chan")
>              ]
>              [
>               "struct " ++ interfaceName ++ "_service_response *concl "
>                 ++ " = malloc(sizeof(struct " ++ interfaceName ++ "_service_response));",
>               "assert(concl != NULL);",
>               "struct " ++ interfaceName ++ "_service *closure = st;",
>               "concl->f = &the_" ++ interfaceName ++ "_server_response_vtbl;",
>               "concl->chan = chan;",
>               "concl->nsendmsg = 0;",
>               "msgbuf_init_static(&concl->sendmsg[0], NULL, 0, NULL, 0);",
>               "msgbuf_init_static(&concl->sendmsg[1], NULL, 0, NULL, 0);",
>               "concl->sclp = closure;",
>               "chan->st = concl;",
>               "chan->receive_handler = " ++ (interfaceName ++ "_server_request_handler;"),
>               "// conn->disconn_handler = server_disconnect_handler;",
>               "if (closure->f->_connected != NULL){",
>               "    return closure->f->_connected(concl);",
>               "} else {",
>               "    return SYS_ERR_OK;",
>               "}"
>              ] 

\subsubsection{Compiling the Listen Function}

The \verb!listen! function is a simple wrapper arround chips listen
function. It looks like this:

\begin{verbatim}
errval_t ifname_listen(struct ifname_service *cl)
{
    return chips_listen((chips_listen_callback_t) cl->f->_listening,
                         server_connect_handler, cl, cl->flags);
}
\end{verbatim}

As generated by:

> compileServerListen :: String -> [String]
> compileServerListen interfaceName =
>     C.multi_comment1 "Server listen function: called to create a service"
>  ++ C.function1 "static" "void" (interfaceName ++ "_export_callback")
>                 [("void *", "st"), ("errval_t", "err"), ("iref_t", "iref")]
>                 [ "struct " ++ interfaceName ++ "_service *cl = st;",
>                   "cl->f->_listening(cl, err_is_ok(err) ? iref : 0);" ]
>  ++ C.function1 ""
>                 "errval_t"
>                 (interfaceName ++ "_listen")
>                 [("struct " ++ interfaceName ++ "_service *", "cl")]
>                 [ "struct flounder_glue_export *e = malloc(sizeof(struct flounder_glue_export));",
>                   "assert(e != NULL);",
>                   "return flounder_glue_export(e, " ++ interfaceName ++ "_export_callback, server_connect_handler, cl, get_default_waitset(), cl->flags);" ]


\subsection{Compiling Client-side Definitions}

On the client side, we have to generate the following code:
\begin{itemize}
    \item the client call vtable, filled with the common stubs and disconnect function
    \item the client request handler, called when chips receives a message
    \item the client connect handler, called when the connection to a server succeeds
    \item the connection function, to bind to a service
\end{itemize}

> compileClientSide :: String -> [MessageDef] -> [String]
> compileClientSide interfaceName messages =
>     compileClientVtable interfaceName messages
>  ++ compileClientRequestHandler interfaceName isRPC messages
>  ++ compileClientConnectHandler interfaceName isRPC
>  ++ compileClientConnect interfaceName
>  where
>    isRPC = any isMsgRpc messages
>    isMsgRpc (RPC _ _) = True
>    isMsgRpc _ = False

\subsubsection{Compiling the Call Vtable}

The client call vtable can already be filled by the marshaling stubs
as well as the disconnect handler. The code looks like this:

\begin{verbatim}
struct ifname_client_call_vtbl the_ifname_client_call_vtbl = 
{
    ifname_simplemsg_stub ,
    ifname_withstrmsg_stub ,
    ifname_initmsg_stub ,
    ifname_forwards_stub ,
    ifname_disconnect
};
\end{verbatim}

As generated by:

> compileClientVtable :: String -> [MessageDef] -> [String]
> compileClientVtable interfaceName messages =
>     ("struct " ++ interfaceName ++ "_client_call_vtbl "
>       ++ "the_" ++ interfaceName ++ "_client_call_vtbl = " )
>   : C.block (
>              [ interfaceName ++ "_" ++ messageName ++ "_stub ,"
>               | message@(Message _ messageName _) <- messages, isForward message ]
>           ++ [ interfaceName ++ "_" ++ messageName ++ "_stub ,"
>               | rpc@(RPC messageName _) <- messages,
>                 isForward rpc ]
>           ++ [ interfaceName ++ "_disconnect" ]
>             ) ++ [";"]

\subsubsection{Compiling the Request Handler}

As for the server request handler, the client request handler is
called by chips when a message is received. It is provided with a
client response closure, which contains the callbacks corresponding to
the possible events. We identify the message type and dispatch the
unmarshaling of message accordingly. The code corresponds to:

\begin{verbatim}
 struct msgbuf * ifname_client_request_handler( void * _cl, struct msgbuf * _msg )
{
    // unmarshall the message code
    //struct chips_connection *_conn = _msg->conn;
    struct ifname_client_response *_clp = _cl;
    assert(_clp != NULL);
    uintptr_t _mc;
    if (err_is_fail(FL_UNMARSHAL_uint16(_msg, &_mc))) {FL_FAILED_DEBUG(""); goto failure;}
    FL_DEBUG_PRINT_TYPE(_mc);
    // Switch on the message code
    switch (_mc) {
        // Dispatch code (...)
    }
    return NULL;
    failure:
    _msg->f->destroy(_msg);
    return NULL;
}\end{verbatim}

> compileClientRequestHandler :: String -> Bool -> [MessageDef] -> [String]
> compileClientRequestHandler interfaceName isRPC messages =
>     C.multi_comment1 "Client request handler: called for each request"
>  ++ C.function1 "" "void"
>              (interfaceName ++ "_client_request_handler")
>              [("void *", "_cl"),
>               ("struct msgbuf *", "_msg")]
>              (if isRPC then [
>               "struct " ++ interfaceName ++ "_client_response *_clp = _cl;",
>               "assert(_clp != NULL);",
>               "_clp->replymsg = _msg;",
>               "assert(!_clp->reply_present);",
>               "_clp->reply_present = true;"
>              ] else ([
>               "// unmarshall the message code",
>               "struct " ++ interfaceName ++ "_client_response *_clp = _cl;",
>               "assert(_clp != NULL);",
>               "uint16_t _mc;",
>               "if (err_is_fail(FL_UNMARSHAL_uint16(_msg, &_mc))) {FL_FAILED_DEBUG(\"\"); goto failure;}",
>               "FL_DEBUG_PRINT_TYPE(_mc);",
>               "// Switch on the message code"
>              ]
>               ++ (C.switch1 "_mc" 
>                        [ ( map toUpper interfaceName ++ 
>                            "_" ++ 
>                            map toUpper messageName ++ 
>                            "_FC",
>                            compileClientDispatchCode interfaceName message )
>                         | message@(Message _ messageName _) <- messages, isBackward message ]
>                        (C.block 
>                              [
>                               "{FL_FAILED_DEBUG(\"bad msg ID %\" PRIu16, _mc); goto failure;}"
>                              ])
>               ++ [
>                   "failure:",
>                   "msgbuf_destroy(_msg);"
>                  ]
>               )))

The dispatch code is standard by now: for each message type, it
unmarshals its arguments and call the relevant handler.

\begin{verbatim}
        case IFNAME_SIMPLEMSG_FC:
        {
            uint32_t someint;
            uint64_t tmp_someint;
            if (err_is_fail(FL_UNMARSHAL_uint32(_msg, &tmp_someint))) {FL_FAILED_DEBUG(""); goto failure;}
            someint = tmp_someint;
            _msg->f->destroy(_msg);
            assert(_clp->f->simplemsg != NULL);
            _clp->f->simplemsg(_clp ,someint);
            break;
        }
        case IFNAME_WITHSTRMSG_FC:
        {
            ifname_alias_type_t at;
            if (err_is_fail(FL_UNMARSHAL_ifname_alias_type(_msg, &at))) {FL_FAILED_DEBUG(""); goto failure;}
            _msg->f->destroy(_msg);
            assert(_clp->f->withstrmsg != NULL);
            _clp->f->withstrmsg(_clp ,at);
            break;
        }
        case IFNAME_INITMSG_FC:
        {
            uint32_t someint;
            cap_t somecap;
            iref_t some_if;
            char * some_string;
            uint64_t tmp_someint;
            if (err_is_fail(FL_UNMARSHAL_uint32(_msg, &tmp_someint))) {FL_FAILED_DEBUG(""); goto failure;}
            someint = tmp_someint;
            if (err_is_fail(FL_UNMARSHAL_cap(_msg, &somecap))) {FL_FAILED_DEBUG(""); goto failure;}
            if (err_is_fail(FL_UNMARSHAL_iref(_msg, &some_if))) {FL_FAILED_DEBUG(""); goto failure;}
            if (err_is_fail(FL_UNMARSHAL_string(_msg, &some_string))) {FL_FAILED_DEBUG(""); goto failure;}
            _msg->f->destroy(_msg);
            assert(_clp->f->initmsg != NULL);
            _clp->f->initmsg(_clp ,someint,somecap,some_if,some_string);
            break;
        }
        case IFNAME_BACKWARDS_FC:
        {
            _msg->f->destroy(_msg);
            assert(_clp->f->backwards != NULL);
            _clp->f->backwards(_clp );
            break;
        }
        default:
        {
            {FL_FAILED_DEBUG("bad msg ID %" PRIuPTR, _mc); goto failure;}
        }
\end{verbatim}

This is achieved by:

> compileClientDispatchCode :: String -> MessageDef -> [String]
> compileClientDispatchCode interfaceName message@(Message _ messageName messageArgs) = 
>     C.block 
>          (
>           [ qualifyType interfaceName typeArg ++ " " ++ nameOf arg ++ ";"
>            | Arg typeArg arg <- messageArgs ]
>        ++ [ "if (err_is_fail(FL_UNMARSHAL_" ++ qualifyName interfaceName typeArg 
>               ++ "(_msg, &" ++ nameOf arg ++ "))) {FL_FAILED_DEBUG(\"\"); goto failure;}"
>            | Arg typeArg arg <- messageArgs ]
>        ++ [ 
>            "msgbuf_destroy(_msg);",
>            "assert(_clp->f->" ++ messageName ++ " != NULL);",
>            "_clp->f->" ++ messageName ++"(_clp " 
>             ++ concat [ "," ++ nameOf arg 
>                       | Arg _ arg <- messageArgs ] ++ ");",
>            "break;"
>            ]
>          )


\subsubsection{Compiling the Connect Handler}

The client connect handler is called by chips when a connection
request succeeds, or fails. If the connection fals, the chips
\verb!conn! is null and we directly disconnect.

Otherwise, we register the request and disconnection handlers to chips
and set up the client closure with this connection and the pre-filled
call vtable. If some service logic has been defined to handle the
connection, it is called. The code looks like this:

\begin{verbatim}
static void client_connect_handler( void * cl, struct chips_connection * conn )
{
    struct ifname_client_response *clp = cl;
    if (conn == NULL) 
    {
        if (clp->f->_disconnect != NULL){
        clp->f->_disconnect(clp->st);
        }
    }
    else
    {
        conn->req_handler = ifname_client_request_handler;
        conn->disconn_handler = (chips_disconnect_handler_t) clp->f->_disconnect;
        clp->call_vtbl = &the_ifname_client_call_vtbl;
        clp->conn = conn;
        if (clp->f->_connected != NULL){
        clp->f->_connected(clp);
        }
    }
}
\end{verbatim}

As generated by:

> compileClientConnectHandler :: String -> Bool -> [String]
> compileClientConnectHandler interfaceName isRPC =
>     C.multi_comment1 "Client connect handler (callback)"
>  ++ C.static "void"
>              "client_connect_handler"
>              [
>               ("void *", "cl"),
>               ("errval_t", "err"),
>               ("struct flounder_glue_binding *", "chan")
>              ]
>         (
>          [
>           "struct " ++ interfaceName ++ "_client_response *clp = cl;"
>          ]
>       ++ C.if_else "chan == NULL || err_is_fail(err)"
>              [
>               "assert(!\"connection failed\");"
>              ]
>              [
>               "chan->st = clp;",
>               "chan->receive_handler = " ++ interfaceName ++ "_client_request_handler;",
>               "chan->error_handler = flounder_glue_error_handler;",
>               "//conn->disconn_handler = (chips_disconnect_handler_t) clp->f->_disconnect;",
>               "clp->call_vtbl = &the_" ++ interfaceName ++ "_client_call_vtbl;",
>               "clp->chan = chan;",
>               "clp->nsendmsg = 0;",
>               "waitset_init(&clp->rpc_waitset);",
>               if isRPC then "chan->waitset = &clp->rpc_waitset;" else "",
>               "if (clp->f->_connected != NULL){",
>               "clp->f->_connected(clp);",
>               "}"
>              ]
>         )


\subsubsection{Compiling the Connect Function}

To bind to a service, the developer will call the following
\verb!connect! function. This is a simple wrapper arround chips
connect function:

\begin{verbatim}
errval_t ifname_connect(iref_t iref, struct ifname_client_response * cl, size_t buflen)
{
    return chips_connect(iref, client_connect_handler, cl, buflen);
}
\end{verbatim}

As generated by:

> compileClientConnect :: String -> [String]
> compileClientConnect interfaceName =
>     C.multi_comment1 "Client connect function: called to bind to a service"
>  ++ C.function1 ""
>                 "errval_t"
>                 (interfaceName ++ "_connect")
>                 [
>                  ("iref_t", "iref"),
>                  ("struct " ++ interfaceName ++ "_client_response *", "cl"),
>                  ("size_t", "buflen")
>                 ]
>                 [
>                  "assert(buflen == 0); // non-default buflen is not implemented here",
>                  "struct flounder_glue_bind_continuation cont = { .handler = client_connect_handler, .st = cl };",
>                  "return flounder_glue_bind(iref, get_default_waitset(), cont);"
>                 ]

