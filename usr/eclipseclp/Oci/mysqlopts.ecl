% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 1997 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Kish Shen, CrossCore Optimization
% 
% END LICENSE BLOCK


:- lib(module_options).

:- local struct(options(/*session*/ session_dbname,session_storage,
                        /*cursor*/  cursor_buffering,cursor_type)
               ).

default_options(options{cursor_buffering:"client",cursor_type:"read_only"}).


valid_option_field(session_dbname, session_dbname of options).
valid_option_field(session_storage, session_storage of options).

valid_option_field(cursor_buffering, cursor_buffering of options).
valid_option_field(cursor_type, cursor_type of options).


valid_option_value(session_dbname, Value) :-  
        (string(Value) ; atom(Value)), !.
valid_option_value(session_storage, Value) :- 
        (string(Value) ; atom(Value)), !.

valid_option_value(cursor_buffering, Value) :-
        (string(Value) ; atom(Value)), 
        concat_string([Value], ValueS),
        (ValueS == "server" ; ValueS = "client"), !.
valid_option_value(cursor_type, Value) :-
        (string(Value) ; atom(Value)), 
        concat_string([Value], ValueS),
        (ValueS == "no_cursor" ; ValueS = "read_only"), !.


:- local struct(session_opts(dbname,storage)).
:- local struct(cursor_opts(buffering,type)).

valid_opts_field(Type, Name, Field) :-
        concat_atom([Type, '_', Name], OptName),
        valid_option_field(OptName, Field).

valid_opt_value(Type, Name, Value) :-
        concat_atom([Type, '_', Name], OptName),
        valid_option_value(OptName, Value).

get_opts(Type, OptList, OptStruct) :-
        (foreach(F:V, OptList), param(Type), 
         foreach(TF:V, TypedOptList) do
            concat_atom([Type,'_',F], TF)
        ),
        get_options(TypedOptList, OptStruct).

