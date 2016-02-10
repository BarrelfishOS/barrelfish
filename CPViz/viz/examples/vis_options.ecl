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
% The Original Code is  CPViz Constraint Visualization System
% The Initial Developer of the Original Code is  Helmut Simonis
% Portions created by the Initial Developer are
% Copyright (C) 2009-2010 Helmut Simonis
% 
% Contributor(s): 	Helmut Simonis, 4C, Univerity College Cork, Cork
%			
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
:-module(vis_options).

:-export(add_options/3).
:-export(default_options/2).

:-use_module(vis_structures).

add_options(Options,ObjectType,Handle):-
        (foreach(Attr:Value,Options),
         param(ObjectType,Handle) do
            (argument_number(ObjectType,Attr,Arg,_) ->
                arg(Arg,Handle,Slot),
                (var(Slot) ->
                    Slot = Value
                ;
                    (Slot == Value ->
                        true
                    ;
                        writeln(already_fixed(ObjectType,Attr,Slot,Value))
                    )
                )            
            ;
                writeln(unknown_attr(ObjectType,Attr,Value))
            )
        ).

default_options(Handle,ObjectType):-
        (foreacharg(X,Handle),
         count(J,1,_),
         param(Handle,ObjectType) do
            default_option(X,J,Handle,ObjectType)
        ).

default_option(X,_,_,_):-
        nonvar(X),
        !.
default_option(X,J,Handle,ObjectType):-
        (argument_number(ObjectType,Attr,J,Default) ->
            (Default = required(_) ->
                writeln(required_attr_missing(ObjectType,Attr,J))
            ; Default = system(_) ->
                true
            ;
                X = Default
            )
        ;
            writeln(missing_default_attr(ObjectType,J,Handle))
        ).

