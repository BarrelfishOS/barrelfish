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
% Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Pascal Brisset and Thom Fruehwirth, ECRC. 
% 
% END LICENSE BLOCK

% This library contains the predicates used in the code produced by
% the chr2pl compiler


:- module(chr).

:- pragma(deprecated_warnings(off)).

:- comment(categories, ["Constraints","Techniques"]).
:- comment(summary, "Constraint Handling Rules Library - obsolescent, use library(ech) instead").
:- comment(author, "Pascal Brisset and Thom Fruehwirth, ECRC").
:- comment(copyright, "1994-2006 Cisco Systems, Inc").
:- comment(date, "$Date: 2009/07/16 09:11:25 $").
:- comment(status, deprecated).
:- comment(include, "chr_doc.pl").

% This predicate called for the labeling must be dynamic as soon as
% several handlers are loaded in the same session: the predicate is the
% scatterred in several files.
:- export initialization(dynamic('CHRlabel_with' /3)).

%%%%%% Check of a guard %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The guard (Goal) is simply called and it's checked that there are no
% delayed goals left after
chr_macro(no_delayed_goals(Goal),
  ( sepia_kernel:last_suspension(LD),
    Goal,
    true,                   % force all wakings
    sepia_kernel:new_suspensions(LD, []))).

% Before the guard (Goal) is called, a 'fail' is attached to every variable
% of the Goal. Then, as soon as one of these variables is touched
% (unified), the call will fail.
chr_macro(no_global_bindings(Goal, Globals),
  ( make_suspension('CHRfail', 1, Susp),
    IS,
    sepia_kernel:last_suspension(LD),
    Goal,
    true,                   % force all wakings
    sepia_kernel:new_suspensions(LD, []),
    kill_suspension(Susp))) :-
  IS = insert_suspension(Globals, Susp, constrained of suspend, suspend).
    

:- export no_delayed_goals/1.
:- inline(no_delayed_goals/1, chr_macro/2).
no_delayed_goals(Goal) :-
	no_delayed_goals(Goal).

:- export no_global_bindings/2.
:- inline(no_global_bindings/2, chr_macro/2).
no_global_bindings(Goal, Globals) :-
	no_global_bindings(Goal, Globals).

:- export portray('CHRhead_not_kept'/1, tr_chr/2, []).

:- reexport(chr2pl).

:- import sepia_kernel.

%%%%%%%% Exported predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- export
	'CHRget_delayed_goals' /2,
	'CHRkill' /1,
	'CHRalready_in' /1,
	'CHRkeep_heads_checking' /6,
	'CHRkeep_heads_checking' /4,
	'CHRcheck_and_mark_applied' /5,
	'CHRcheck_and_mark_applied' /2,
	'CHRgen_num' /1,
	'CHRvar' /1,
	'CHRhead_not_kept' /1,
	'CHRfail' /0,
	'CHRnonvar' /1,
	'CHRdelay' /2,
	'CHR=' /2,
	coca /1,
	chr_trace /0,
	chr_notrace /0,
%	chr_opium /0,
	chr_get_constraint /1,
	chr_get_constraint /2,
	chr_label_with/1,
	chr_delayed_goals_handler/3,
	chr_start_handler/3,
	tr_chr/2,
	chr_resolve/1,
	chr_labeling/0.


:- tool('CHRdelay'/2, 'CHRdelay'/3),
   tool(chr_labeling/0, chr_labeling/1),
   tool(chr_resolve/1, chr_resolve/2),
   tool(chr_label_with/1, chr_label_with/2).

:- coroutine.


% CHR attribute: added for ECLiPSe 5.8 and later.
% This was necessary because of a change in semantics of the constrained-
% waking list (var-var unifications now only wake suspensions which are
% in the constrained-lists of both variables). This unify_chr handler forces
% _all_ constrained-suspensions to be woken when two chr-variables get
% unified, which is wasteful, but corresponds to the semantics before 5.8.
% Note that we arbitrarily wake only one variable's constrained-list
% because the constraints should be able to find their partner either way.

:- meta_attribute(chr, [unify:unify_chr/2]).

unify_chr(_Y, AttrX) :-
	var(AttrX).
unify_chr(Y, AttrX) :-
	nonvar(AttrX),
	unify_any_chr(Y, AttrX).

    unify_any_chr(Y{AttrY}, AttrX) ?- !,
	unify_chr_chr(Y, AttrX, AttrY).
    unify_any_chr(_Y, _AttrX).

    unify_chr_chr(_Y, AttrX, AttrY) :-
	var(AttrY),
	AttrY = AttrX.			% transfer the attribute
    unify_chr_chr(Y, _AttrX, AttrY) :-
	nonvar(AttrY),
	notify_constrained(Y).		% wake either X's or Y's list


mark_as_chr_variables(Term) :-
	term_variables(Term, Vars),
	( foreach(Var,Vars) do
	    mark_as_chr_variable(Var)
	).

    mark_as_chr_variable(_{Attr}) ?-
	Attr = chr.
    mark_as_chr_variable(X) :-
	free(X),
	add_attribute(X, chr).


%%%%%%%% CHR primitives used by the code produced by chr2pl %%%%%%%%%%%%%%%


%%%%% Return the delayed goals (which may contain a partner for a double-headed
% rule). If there is a share variable, goals delayed
'CHRget_delayed_goals'(X, DG) :-
	nonground(X, Var), !,
	delayed_goals(Var, DG).
'CHRget_delayed_goals'(_, DG) :-
	delayed_goals(DG).


%%%%% Set the kill flag to 'true'
'CHRkill'(true).



%%%% Checks that a constraint is already in the constraint store (it's usually
% called each time a constraint is about to be added
% The check is done only if the goal is not ground
'CHRalready_in'(Goal) :-
	arg(1, Goal, Constraint),	
	( nonground(Constraint, OneVar) ->
	    delayed_goals(OneVar, Goals)
	 ;
	    delayed_goals(Goals)
	),
	constraint_member(Goals, Constraint).

% The list of suspensions is erminated with a variable
constraint_member([Goal | _], Constraint1)
 ?-
	functor(Goal, _, 4),     %%% The goal is a CHR constraint
	arg(1, Goal, Constraint2),
	arg(2, Goal, KF),
	var(KF), %%% Because of waking order, a delayed constraint may have
	         %%% its Flag set
	Constraint2 == Constraint1,
	!.
constraint_member([_ | Goals], Goal)
 ?-
	constraint_member(Goals, Goal).


% When a constraint C is about to be added, in the case that this constraint
% may be equal to one head H of the rule (recognised statically), a test is
% done to know if C is really equal to H.
% The following predicates do the check, possibly kill the constraint H
% and return a flag set to true if the constraint should be added

% Comparaison with one head
'CHRkeep_heads_checking'(H, KF, G, CallG) :-
	( H == G -> true
         ; 'CHRkill'(KF), CallG = true
        ).

% Comparaison with two heads
'CHRkeep_heads_checking'(H1, KF1, H2, KF2, G, CallG) :-
	( H1 == G -> KF2 = true
         ; H2 == G -> KF1 = true
         ; 'CHRkill'(KF1), 'CHRkill'(KF2), CallG = true
        ).



%%%%% An augmentation rule should not be applied twice on the same constraints.
% To avoid such a redundance, a list of pairs (rule, partner) is associated
% to every constraint. When a augmentation rule is about to be tried
% this list is checked
'CHRcheck_and_mark_applied'(RuleName, _KF1, KF2, [[RuleName | KF2] | _PA1], _PA2)
 ?-
	!, %%% Rule already used with this partner
	fail.
'CHRcheck_and_mark_applied'(RuleName, KF1, KF2, [_ | PA1], PA2)
 ?-
	!,
	'CHRcheck_and_mark_applied'(RuleName, KF1, KF2, PA1, PA2).
'CHRcheck_and_mark_applied'(RuleName, KF1, KF2, PA1, PA2) :-
	PA1 = [[RuleName | KF2] | _New_PA1],
	name_for_the_partner(RuleName, PartnerRuleName),
	mark_applied([PartnerRuleName | KF1], PA2).

% The rule    r @ h(X), h(Y) ==> Body
% should fire with H1, H2  and   H2, H1
% so the name of the rule is not enough, the position of the head must also
% be stored.
name_for_the_partner('12'(Name), '21'(Name)) :- !.
name_for_the_partner('21'(Name), '12'(Name)) :- !.
name_for_the_partner(Name, Name).

% Goes to the tail (variable) of the list and add a new element.
mark_applied(X, [_ | PA])
 ?-
	!,
	mark_applied(X, PA).
mark_applied(X, [X | _New_PA]).


%%%%% Same check for simple headed rules
'CHRcheck_and_mark_applied'(RuleName, [RuleName | _PA])
 ?-
	!,
	fail.
'CHRcheck_and_mark_applied'(RuleName, [_ | PA])
 ?-
	!,
	'CHRcheck_and_mark_applied'(RuleName, PA).
'CHRcheck_and_mark_applied'(RuleName, PA) :-
	PA = [RuleName | _New_PA].


%%%%% gensym for numbering contraints, the number is used in the debugger
:- local variable(gen_num).
'CHRgen_num'(X) :-
	getval(gen_num, X),
	incval(gen_num).


%%%%% These predicates are renamed in order to be hidden in the debugger
'CHRnonvar'(X) :- nonvar(X).
'CHRvar'(X) :- var(X).
'CHRhead_not_kept'(true)
 ?-
	true.
'CHR='(X, X).
'CHRtrue'.
'CHRfail' :- fail.


%%%%% Delays on all the variables in inst, bound suspended lists
'CHRdelay'(Vars, Goal, Module) :-
	make_suspension(Goal, 3, Susp, Module),
	mark_as_chr_variables(Vars),
	insert_suspension(Vars, Susp, constrained of suspend, suspend).

%%%%%%%% User primitives %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- setval(chr_trace, off).

%%% Starts the debug mode using Opium
%chr_opium :-
%	get_flag(installation_directory, ID),
%	concat_string(["make(chr_op,chr_op,[active,untraceable,global],'", ID, "/lib/chr/','", ID, "/lib/chr/')"], Init),
%	opium(init(Init)),
%	set_flag((;)/2, leash, notrace),
%	set_flag(coca/1, leash, stop),
%	setval(chr_trace, opium).

chr_trace :-
	set_flag(coca/1, leash, notrace),
	setval(chr_trace, dbg),
	set_flag(debugging, creep).

%%% Stops the debugger
chr_notrace :-
	set_flag(coca/1, leash, notrace),
	setval(chr_trace, off),
	set_flag(debugging, nodebug).



%%% labeling
% While there are sone contraints in the store, one is picked and tried to
% be solved using the 'label_with' rules and the Prolog clauses
chr_labeling(Module):-
        ( get_one_constraint(Constraint, Nb, KF),
	  call('CHRlabel_with'(Constraint, Goal, Nb), Module),
	  !,
	  KF = true,
	  call(Goal, Module),
	  chr_labeling(Module)
	;
	  'CHRtrue'
	).

get_one_constraint(Constraint, Nb, KF) :-
	delayed_goals(DG),                     
	member(C, DG),
	C =.. [_, Constraint, KF, _PA, Nb].


%%% Does a check using a label_with declaration
chr_label_with(Constraint, Module) :-
	call('CHRlabel_with'(Constraint, _Goal, 0), Module).



%%% Returns (by backtrack) the current constraints
chr_get_constraint(Constraint) :-
	delayed_goals(DGs),
	member(DG, DGs),
	DG =.. [_, Constraint, true, _PA, _Nb].


chr_get_constraint(Var, Constraint) :-
	delayed_goals(Var, DGs),
	member(DG, DGs),
	DG =.. [_, Constraint, true, _PA, _Nb].


%%% Solve a constraint using the Prolog clauses
chr_resolve(Constraint, Module) :-
	functor(Constraint, Functor, Arity),
	concat_atom(['clause_', Functor], Goal_Functor),
	( call(is_predicate(Goal_Functor/Arity), Module)
         ->
	  Constraint =.. [Functor | Args],
	  Goal =.. [Goal_Functor | Args],
	  call(Goal, Module)
         ;
	  error(6, chr_resolve(Constraint))
        ).


%%%%%%%%%% Debugger %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% coca/1 goals are put in the produced code if the flag 'debug_compile' is
% set. These goals are the ones recognised by Opium
% Note about the name: just because it's related to Opium
coca(try_double(Nb1, H1, Nb2, H2, H1C, H2C, Kind, GC, BC, Rule_Name)) :-
	chr_dbg(try_double(Nb1,H1,Nb2, H2, H1C, H2C, Kind, GC, BC, Rule_Name)),
	'CHRtrue'
        ;
	coca(delayed_rule(Rule_Name)), !, 'CHRfail'.
coca(try_rule(Nb, H, Rule_Name, HC, Kind, GuardC, BodyC)) :-
	chr_dbg(try_rule(Nb, H, Rule_Name, HC, Kind, GuardC, BodyC)),
	'CHRtrue'
        ;
	coca(delayed_rule(Rule_Name)), !, 'CHRfail'.
coca(try_clause(Nb, Head, HeadC, GuardC)) :-
	chr_dbg(try_clause(Nb, Head, HeadC, GuardC)),
	'CHRtrue'
        ;
	coca(call_delayed), !, 'CHRfail'.
coca(Event) :-
	chr_dbg(Event).



chr_dbg(Event) :-
	getval(chr_trace, dbg), !,
	treat_chr_dbg(Event).
chr_dbg(_).


treat_chr_dbg(add_one_constraint(Nb, Constraint)) :-
	printf(debug_output, "ADD (%d) %p\n", [Nb, Constraint]).
treat_chr_dbg(already_in) :-
	printf(debug_output, "CONSTRAINT ALREADY THERE\n", []).
treat_chr_dbg(try_clause(Nb, Constraint, Head, Guard)) :-
	printf(debug_output, "TRY LABEL (%d) %p\nwith\nlabel_with %p if %p\n", [Nb, Constraint, Head, Guard]).
treat_chr_dbg(clause_fired(CstNb)) :-
	printf(debug_output, "LABEL FIRED with %d\n", [CstNb]).
treat_chr_dbg(call_delayed) :-
	printf(debug_output, "LABEL DELAYED\n", []).
treat_chr_dbg(try_rule(Nb, Goal, Rule_Name, Head, Kind, Guard, Body)) :-
	printf(debug_output, "TRY (%d) %p\nwith\n", [Nb, Goal]),
	( Kind = replacement
	 ->
	  Connector = "<=>"
	;
	  Connector = "==>"
	),
	( nonvar(Rule_Name), Rule_Name = anonymous(Name)
	 ->
	  printf(debug_output, "%s: %p %s %p | %p\n", [Name, Head, Connector, Guard, Body])
	;
	  printf(debug_output, "%p\n", [Rule_Name])
	).
treat_chr_dbg(try_double(Nb1, Goal1, Nb2, Goal2, Head1, Head2, Kind, Guard, Body, Rule_Name)) :-
	printf(debug_output, "TRY (%d) %p (%d) %p\nwith\n", [Nb1, Goal1, Nb2, Goal2]),
	double_rule(Head1, Head2, Kind, Guard, Body, Rule),
	( nonvar(Rule_Name), Rule_Name = anonymous(Name)
	 ->
	  printf(debug_output, "%s: %p\n", [Name, Rule])
	;
	  printf(debug_output, "%p\n", [Rule_Name])
	).
treat_chr_dbg(fired_rule(Rule_Name)) :-
	( nonvar(Rule_Name), Rule_Name = anonymous(Name)
	 ->
	  printf(debug_output, "RULE '%s' FIRED\n", [Name])
	 ;
	  printf(debug_output, "RULE '%p' FIRED\n", [Rule_Name])
	).
treat_chr_dbg(delayed_rule(Rule_Name)) :-
        ( nonvar(Rule_Name), Rule_Name = anonymous(Name)
	 ->
	  printf(debug_output, "RULE '%s' DELAYED\n", [Name])
	 ;
	  printf(debug_output, "RULE '%p' DELAYED\n", [Rule_Name])
	).


%%% Special case for rules which have been translated for keeping heads
double_rule(Head1, Head2, Kind, Guard, Body, Rule) :-
	(Kind = keep_first ; Kind = keep_second),
	remove_keep_heads_checking(Guard, G),
	!,
	remove_head_not_kept(Body, B),
	Rule = (Head1, Head2 <=> G | B).
double_rule(Head1, Head2, Kind, Guard, Body, Rule) :-
	( Kind = augmentation
	 ->
	  Rule = (Head1, Head2 ==> Guard | Body)
	; Kind = replacement
	 ->
	  Rule = (Head1, Head2 <=> Guard | Body)
	; Kind = keep_first
	 ->
	  Rule = (Head1 \ Head2 <=> Guard | Body)
	; Kind = keep_second
	 ->
	  Rule = (Head1 \ Head2 <=> Guard | Body)
	).


remove_keep_heads_checking((G1, G2), G) :-
	remove_keep_heads_checking(G2, G3),
	( G3 = true -> G = G1 ; G = (G1, G3)).
remove_keep_heads_checking('CHRkeep_heads_checking'(_,_,_,_,_,_), true).
remove_keep_heads_checking('CHRkeep_heads_checking'(_,_,_,_), true).


remove_head_not_kept((G1, G2), (G11, G21)) :- !,
	remove_head_not_kept(G1, G11),
	remove_head_not_kept(G2, G21).
remove_head_not_kept(('CHRhead_not_kept'(_) -> Constraint ; true), Constraint) :- !.
remove_head_not_kept(G, G).


%%%%%%%%%%%% HANDLERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Numbering of constraints
%%% The counter is initialized statically and dynamically before each
%%% query
:- setval(gen_num, 1).

:- define_error("Original handler 154", N),
   setval(orig_handler_154, N),
   get_error_handler(154, Handler, Module154),
   (import Handler from Module154),
   set_error_handler(N, Handler).

chr_start_handler(_, Goal, Module) :-
	setval(gen_num, 1),
	getval(orig_handler_154, N),
	error(N, Goal, Module).

:- set_error_handler(154, chr_start_handler/3).



%%% Display of the delayed goals after a computation
%%% A special printer must be used for constraints
%%% The original handler for printing delayed goals is replaced by a new one
%%% which, after displaying the constraints, calls the original handler.
%%% The link to the original handler is done by a user defined exception.

:- define_error("Original handler 273", N),
   setval(orig_handler_273, N),
   get_error_handler(273, Handler, Module273),
   (import Handler from Module273),
   set_error_handler(N, Handler).

chr_delayed_goals_handler(_, DG, Module) :-
	( DG \= []
         ->
	  write(answer_output, "\n\nConstraints:\n"),
	  print_constraints(DG, Rest)
	; Rest = DG
        ),
	( Rest = [_H | _T]
         -> 
	  getval(orig_handler_273, N),
	  error(N, Rest, Module)
        ; true
        ).

:- set_error_handler(273, chr_delayed_goals_handler/3).

%%% Print the current constraints on the current output, kills them and
% returns the other delayed goals.
% The user predicate chr_portray/2, if it exists, is called to do a
% 'pretty' printing
print_constraints([], []).
print_constraints([Susp | DG], Rest) :-
	suspension_to_goal(Susp, Delayed, _),
	Delayed =.. [PredName, _Constraint, KF, _PA, Nb],
	atom_string(PredName, PredNameS),
	append_strings("CHR", _, PredNameS),
	!,
	( integer(Nb)
         ->
	  call(printf("(%d) %QVw\n", [Nb, Delayed]), eclipse)
	 ; %%% constraint not labelled (nodbgcomp)
	  call(printf("%QVw\n", [Delayed]), eclipse)
	),
        KF = true,
        print_constraints(DG, Rest).
print_constraints([D | DG], [D | Rest]) :-
        print_constraints(DG, Rest).



tr_chr(Clause_Goal, Goal) :-
	Clause_Goal =.. [Clause_Name | Args],
	atom_string(Clause_Name, Clause_String),
	append_strings("clause_", S, Clause_String),
	!,
	atom_string(N, S),
	Goal =.. [N | Args].
tr_chr('CHRhead_not_kept'(_), 'HEAD REMOVED') :- !.
% For macros defined in the produced code
tr_chr(CHR, Co) :-
	arg(1, CHR, Co).

%%%%%%%%%% Setting flags fo the debugger %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_flag(coca/1, leash, notrace).

:- (untraceable chr_dbg/1), skipped chr_dbg/1.

:- untraceable chr_labeling/1.

:- set_flag('CHR='/2, leash, notrace).

:- set_flag('CHRget_delayed_goals'/2, skip, on), set_flag('CHRget_delayed_goals'/2, leash, notrace).

:- set_flag('CHRalready_in'/1, skip, on), set_flag('CHRalready_in'/1, leash, notrace).

:- set_flag(subcall/2, leash, notrace), set_flag(call/2, leash, notrace).

:- set_flag('CHRcheck_and_mark_applied'/5, leash, notrace), set_flag('CHRcheck_and_mark_applied'/5, skip, on).

:- set_flag('CHRcheck_and_mark_applied'/2, leash, notrace), set_flag('CHRcheck_and_mark_applied'/2, skip, on).

:- set_flag('CHRgen_num'/1, skip, on), set_flag('CHRgen_num'/1, leash, notrace).

:- skipped(('CHRkeep_heads_checking'/6, 'CHRkeep_heads_checking'/4)),
   untraceable(('CHRkeep_heads_checking'/6, 'CHRkeep_heads_checking'/4)).

:- set_flag('CHRnonvar'/1, leash, notrace), set_flag('CHRnonvar'/1, skip, on).
:- set_flag('CHRvar'/1, leash, notrace), set_flag('CHRvar'/1, skip, on).

:- set_flag('CHRfail'/0, leash, notrace), set_flag('CHRfail'/0, skip, on).

:- set_flag('CHRkill'/1, leash, notrace).

:- set_flag('CHRtrue'/0, leash, notrace).

:- set_flag('true'/0, leash, notrace).

:- set_flag(new_suspensions/2, leash, notrace).

:- set_flag(!/0, leash, notrace).


:- set_flag('CHRdelay'/2, leash, notrace), set_flag('CHRdelay'/3, leash, notrace).


:- (import subcall/3 from sepia_kernel), set_flag(subcall/3, leash, notrace).


:- set_flag(chr_trace/0, skip, on), set_flag(chr_trace/0, leash, notrace).
:- set_flag(chr_notrace/0, skip, on), set_flag(chr_notrace/0, leash, notrace).
:- set_flag(get_one_constraint/3, leash, notrace), set_flag(get_one_constraint/3, skip, on).

:- set_flag(kill_suspension/1, leash, notrace).
:- skipped (chr_get_constraint/2, chr_get_constraint/1).
:- skipped('CHRhead_not_kept'/1).
:- skipped chr_resolve/1.
:- untraceable(chr_label_with/2).
%:- untraceable(insert_suspension/4).
