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
% Copyright (C) 1995 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Mark Wallace, IC-Parc
% 
% END LICENSE BLOCK
% bin_info.pl
% 
%

:- module(bin_info).

:- export get_bin_info/2.
:- export add_bin_info/2.

:- meta_attribute(bin_info, [unify:unify_bin_info/2,print:print_bin_info/2]).

add_bin_info(Var,Bin_info) :-
	add_attribute(Var,Bin_info).

unify_bin_info(_,Attr) :-
	var(Attr).
unify_bin_info(Term, Attr) :-
	nonvar(Attr),
	unify_term_bin_info(Term,Attr).

unify_term_bin_info(Value,_Attr) :-
	nonvar(Value).
unify_term_bin_info(Y{AttrY},AttrX) :-
	-?->
	unify_bin_info_bin_info(Y,AttrX,AttrY).

unify_bin_info_bin_info(_Y,AttrX,AttrY) :-
	var(AttrY), AttrX=AttrY.
unify_bin_info_bin_info(_Y,_AttrX,AttrY) :-
	nonvar(AttrY).

%print_bin_info(Bin_info,Bin_info).
print_bin_info(_{bin_info:Bin_info},OutBin_info) :-
	-?->
	OutBin_info=Bin_info.

%get_bin_info(Val,Bin_info) :-
%	nonvar(Val), !,
%	Bin_info=Val.
get_bin_info(_Var{Bin_info},OutBin_info) :-
	-?->
	nonvar(Bin_info),
	Bin_info=OutBin_info.

