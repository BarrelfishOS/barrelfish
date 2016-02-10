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
% Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: ilog.pl,v 1.1 2006/09/23 01:54:04 snovello Exp $
% ----------------------------------------------------------------------

% $Id: ilog.pl,v 1.1 2006/09/23 01:54:04 snovello Exp $

:- module_interface(ilog).

:- lib(structures).

:- export
      dvar_domain/2,
      is_domain/1,
      dom_range/3,
      dom_size/2,
      dom_member/2,
      dom_check_in/2,
      dom_compare/3,
      dom_intersection/4,
      dom_difference/4,
      dom_union/4,
      dom_copy/4,
      domain_msg/3,
      var_fd/2,
      list_to_dom/2,
      sorted_list_to_dom/2,
      integer_list_to_dom/2,
      ilog_init/0,
      ilog_intvar/3,
      ilog_intvar/2,
      ilog_add/1,
      ilog_info/0,
      dvar_attribute/2,
      default_domain/1,
      setmin/1,
      removemin/1.

:- export ilog_var_print/2.

% Must be COHERENT with ilog.cc (VARIABLE_INDEX, ...)
:- define_struct(fd(domain, variable, min, max, any)).
:- meta_attribute(fd, [
        print:ilog_var_print/2,
        copy_term: copy_term_domain/2,
	unify:unify_ilog/2]).

:- begin_module(ilog).


:- op(600, xfx, [..]).

:- import
	call_priority/3,
	setarg/3,
	suspensions/1,
	symbol_address/2
    from sepia_kernel.

:-
   ( symbol_address(c_ilog_init, _) ->
       true
   ;
       write("loading ILOG ... "), flush(output),
       get_flag(hostarch, Arch),
       get_flag(object_suffix, O),
       ( O = "o" ->
	   concat_string([Arch,'/ilog.',O,' -lm'], Load)
       ;
	   concat_string([Arch,'/ilog.',O], Load)
       ),
       load(Load),
       writeln(done)
   ),
   external(ext_ilog_init/1, c_ilog_init),
   external(ilog_info/0, c_ilog_print_info),
   external(ilog_range_var/4, c_ilog_intvar),
   external(ilog_enum_var/3, c_ilog_enum_var),
   external(ilog_copy_var/3, c_ilog_copy_var),
   external(ilog_get_range/3, c_ilog_get_range),
   external(ilog_get_size/2, c_ilog_get_size),
   external(ilog_get_domain/2, c_ilog_get_domain),
   external(ext_ilog_set_value/3, c_ilog_set_value),
   external(ilog_eq_vars/3, c_ilog_eq_vars),
   external(ilog_add/2, c_ilog_add),
   external(ilog_setmin/2, c_ilog_setmin),
   external(ilog_removemin/2, c_ilog_removemin),
   external(ilog_is_intvar/1, c_ilog_is_intvar),
   make_array(ilog_handle, global_reference).

ilog_init :-
  ext_ilog_init(H),
  setval(ilog_handle, H).

unify_ilog(_Term, Attr) :-
  var(Attr).		% Ignore if no attribute for this extension
unify_ilog(Term, Attr) :-
  compound(Attr),
  unify_term_ilog(Term, Attr).

:- mode unify_term_ilog(?, +).
unify_term_ilog(X, fd with [domain:Id]) :-
  integer(X), !,		% The variable was instantiated
  ilog_id_set_value(Id, X).
unify_term_ilog(Y, AttrX) :-
  get_ilog_attr(Y, AttrY),
  unify_ilog_ilog(Y, AttrX, AttrY).

unify_ilog_ilog(Y, AttrX, AttrY) :-
  var(AttrY), 		% No attribute for this extension
  AttrX = AttrY,		% Transfer the attribute
  notify_constrained(Y).
unify_ilog_ilog(_Y, fd with [domain:IdX], AttrY) :-
  nonvar(AttrY),
  AttrY = fd with [domain:IdY],
  getval(ilog_handle, H),
  ilog_eq_vars(H, IdX, IdY).



%----------------------------------------------------------------
% print
%----------------------------------------------------------------


ilog_var_print(Var, IlogDomain) :-
  get_ilog_attr(Var, fd with [domain:Id]),
  ilog_get_domain(Id, IlogDomain).



%----------------------------------------------------------------
% copy_term
%----------------------------------------------------------------

copy_term_domain(X{fd:AttrX}, Copy) :-
    -?->
    copy_term_domain(X, Copy, AttrX).


copy_term_domain(_, _, AttrX) :-
    /*** VAR ***/
    var(AttrX).
copy_term_domain(_, Copy, fd with domain:IlogId1) :-
    -?->
    /*** META ***/
    set_ilog_attr(Copy, IlogId2, Attr),
    ilog_copy_var(Attr, IlogId2, IlogId1).

empty_domain(D, fd with [domain:D, any:[], min:[], max:[]]).


ilog_id_set_value(Id, Val) :-
  getval(ilog_handle, H),
  ext_ilog_set_value(H, Id, Val).

ilog_intvar(Var, Min, Max) :-
  set_ilog_attr(V, Id, Attr),
  ilog_range_var(Attr, Id, Min, Max),
  V = Var. % To get correct behaviour if Var is instantiated or constrained

ilog_intvar(Var, Values) :-
  set_ilog_attr(V, Id, Attr),
  ilog_enum_var(Attr, Id, Values),
  V = Var. % To get correct behaviour if Var is instantiated or constrained

get_ilog_attr(_{fd:Attr}, A) :-
  -?->
  A = Attr.


dvar_attribute(I, A) :-
  integer(I), !,
  A = fd with [domain:I, min:[], max:[], any:[]].
dvar_attribute(I, A) :-
  get_ilog_attr(I, A).

is_domain(X) :-
  get_ilog_attr(X, A),
  nonvar(A).

set_ilog_attr(Var, IlogHandle, Attr) :-
  -?->
  Attr = fd with [domain:IlogHandle,variable:Var],
  init_suspension_list(min of fd, Attr),
  init_suspension_list(max of fd, Attr),
  init_suspension_list(any of fd, Attr),
  add_attribute(Var, Attr, fd).

default_domain(V) :-
  ilog_intvar(V, -10000000, 10000000).


ec_vars2ilog_vars(VE, VI) :-
  var(VE), !,
  ( get_ilog_attr(VE, fd with domain:VI) -> true
  ; 
    default_domain(VE),
    get_ilog_attr(VE, fd with domain:VI)
  ).
ec_vars2ilog_vars(TermE, TermI) :-
  functor(TermE, F, N), functor(TermI, F, N),
  ( foreacharg(VE, TermE), foreacharg(VI, TermI)
  do
    ec_vars2ilog_vars(VE, VI)
  ).

ilog_add(C) :-
  getval(ilog_handle, H),
  ec_vars2ilog_vars(C, CI),
  ilog_add(H, CI),
  wake.

dvar_domain(X, I) :-
  integer(X), !,
  I = X. 
dvar_domain(X, I) :- 
  get_ilog_attr(X, fd with domain:I).

dom_range(I, Min, Max) :-
  integer(I), !,
  Min = I, Max = I.
dom_range(I, Min, Max) :-
  ilog_get_range(I, Min, Max).

dom_size(I, Size) :-
  integer(I), !,
  Size=1.
dom_size(I, Size) :-
  ilog_get_size(I, Size).



setmin(X) :-
  integer(X), !.
setmin(X) :-
  get_ilog_attr(X, fd with [domain:I]),
  getval(ilog_handle, H),
  ilog_setmin(H, I).
  % wake not necessary because an instanciation occurs ?

removemin(X) :-
  integer(X), !, fail.
removemin(X) :-
  get_ilog_attr(X, fd with [domain:I]),
  getval(ilog_handle, H),
  ilog_removemin(H, I),
  wake.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unefficient domain operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% mode(+Integer or Handle, ? List of intervals)
get_domain(Dom, Domain) :-
  integer(Dom), !,
  Domain = [Dom].
get_domain(Dom, Domain) :-
  ilog_get_domain(Dom, Domain).



dom_check_in(X, Dom) :-
  dom_member(X, Dom), !.


dom_member(Element, Dom) :-
  get_domain(Dom, IlogDomain),
  member(X, IlogDomain),
  ( integer(X), Element = X
  ; X = Min..Max,
    between(Min, Max, 1, Element) 
  ).

dom_compare(Res, Dom1, Dom2) :-
  get_domain(Dom1, IlogDomain1),
  get_domain(Dom2, IlogDomain2),
  ( IlogDomain1 = IlogDomain2 -> Res = (=)
  ; domain_intersection(IlogDomain1, IlogDomain2, Intersection),
    ( IlogDomain1 = Intersection -> Res = (<)
    ; IlogDomain2 = Intersection -> Res = (>)
    ; fail
    )
  ).

dom_intersection(Dom1, Dom2, Intersection, Size) :-
  get_domain(Dom1, IlogDomain1),
  get_domain(Dom2, IlogDomain2),
  domain_intersection(IlogDomain1, IlogDomain2, IlogDomain),
  flat_domain(IlogDomain, Values),
  ilog_intvar(Dummy, Values),
  dvar_domain(Dummy, Intersection),
  dom_size(Intersection, Size).

dom_union(Dom1, Dom2, Intersection, Size) :-
  get_domain(Dom1, IlogDomain1),
  get_domain(Dom2, IlogDomain2),
  domain_union(IlogDomain1, IlogDomain2, IlogDomain),
  flat_domain(IlogDomain, Values),
  ilog_intvar(Dummy, Values),
  dvar_domain(Dummy, Intersection),
  dom_size(Intersection, Size).


dom_difference(Dom1, Dom2, Intersection, Size) :-
  get_domain(Dom1, IlogDomain1),
  get_domain(Dom2, IlogDomain2),
  domain_difference(IlogDomain1, IlogDomain2, IlogDomain),
  flat_domain(IlogDomain, Values),
  ilog_intvar(Dummy, Values),
  dvar_domain(Dummy, Intersection),
  dom_size(Intersection, Size).

:- mode(dom_copy(+, -)).
dom_copy(Dom1, Dom2) :-
  var_fd(Dummy, Dom1),
  dvar_domain(Dummy, Dom2).

list_to_dom(Values, Dom) :-
  ilog_intvar(Dummy, Values),
  dvar_domain(Dummy, Dom).


integer_list_to_dom(Values, Dom) :- list_to_dom(Values, Dom).
sorted_list_to_dom(Values, Dom) :- list_to_dom(Values, Dom).

var_fd(Var, Dom) :-
  var(Var),
  set_ilog_attr(New, IlogId, Attr),
  ilog_copy_var(Attr, IlogId, Dom),
  Var = New.

domain_msg(V1, V2, Msg) :-
  dvar_domain(V1, Dom1), get_domain(Dom1, L1),
  dvar_domain(V2, Dom2), get_domain(Dom2, L2),
  domain_union(L1, L2, L, _),
  flat_domain(L, Values),
  ilog_intvar(Msg, Values).


domain_intersection([], _, []).
domain_intersection(_, [], []) :- !.
domain_intersection([X1|Xs1], [X2|Xs2], I) :-
  ( integer(X1) ->
      ( integer(X2) ->
	  ( X1 = X2 -> I = [X1 | I0], domain_intersection(Xs1,Xs2,I0)
          ; X1 < X2 -> domain_intersection(Xs1, [X2|Xs2], I)
          ; /* X1 > X2 */ domain_intersection([X1|Xs1], Xs2, I)
          )
      ; X2 = Min2..Max2,
          ( Min2 > Max2 -> domain_intersection([X1|Xs1], Xs2, I)
          ; X1 < Min2 -> domain_intersection(Xs1, [X2|Xs2], I)
          ; X1 > Max2 -> domain_intersection([X1|Xs1], Xs2, I)
          ; X1 = Max2 -> I = [X1 | I0], domain_intersection(Xs1,Xs2,I0)
          ; /* X1 < Max2, X1>Min2*/
              I=[X1|I0], domain_intersection(Xs1,[X2|Xs2],I0)
          )
      )
  ; X1 = Min1..Max1,
    ( Min1 > Max1 -> domain_intersection(Xs1, [X2|Xs2], I)
    ; integer(X2) ->
	domain_intersection([X2|Xs2], [X1|Xs1], I)
    ;
	X2=Min2..Max2,
        ( Min2 > Max2 -> domain_intersection([X1|Xs1], Xs2, I)
        ; Max1 < Min2 -> domain_intersection(Xs1, [X2|Xs2], I)
        ; Max2 < Min1 -> domain_intersection([X1|Xs1], Xs2, I)
        ; Min is max(Min1, Min2), Max is min(Max1, Max2),
          ( Min = Max -> I = [Min | I0]
          ; Min < Max -> I = [Min..Max| I0]
          ; I = I0
          ),
          Max_1 is Max + 1,
          domain_intersection([Max_1..Max1|Xs1], [Max_1..Max2|Xs2], I)
        )
    )
  ).


domain_union([], Xs2, Xs2).
domain_union(Xs1, [], Xs1) :- !.
domain_union([X1|Xs1], [X2|Xs2], U) :-
  ( integer(X1) ->
      ( integer(X2) ->
	  ( X1 = X2 -> I = [X1 | U0], domain_union(Xs1,Xs2,U0)
          ; X1 < X2 -> I = [X1 | U0], domain_union(Xs1, [X2|Xs2], U0)
          ; /* X1 > X2 */ I = [X2 | U0], domain_union([X1|Xs1], Xs2, U0)
          )
      ; X2 = Min2..Max2,
          ( X1 < Min2 -> I = [X1 | U0], domain_union(Xs1, [X2|Xs2], U0)
          ; X1 > Max2 -> I = [X2 | U0], domain_union([X1|Xs1], Xs2, U0)
          ; /* X1 <= Max2, X1>/Min2*/ domain_union(Xs1,[X2|Xs2],U)
          )
      )
  ; X1 = Min1..Max1,
    ( integer(X2) ->
	domain_union([X2|Xs2], [X1|Xs1], U)
    ;
	X2=Min2..Max2,
        ( Max1 < Min2 -> U = [X1|U0], domain_union(Xs1, [X2|Xs2], U0)
        ; Max2 < Min1 -> U = [X2|U0], domain_union([X1|Xs1], Xs2, U0)
        ; Min is min(Min1, Min2), Max is max(Max1, Max2),
          ( Max = Max1 -> domain_union([Min..Max|Xs1], Xs2, U)
          ; /* Max=Max2 */ domain_union(Xs1, [Min..Max|Xs2], U)
          )
        )
    )
  ).


    
      
         
  


flat_domain([], []).
flat_domain([X | Xs], Values) :-
  ( integer(X) ->
      Values = [X | OtherValues]
  ; X = Min..Max,
    ( for(I, Min, Max), fromto(Values, [I|R], R, OtherValues) do true )
  ),
  flat_domain(Xs, OtherValues).


:- ilog_init.
