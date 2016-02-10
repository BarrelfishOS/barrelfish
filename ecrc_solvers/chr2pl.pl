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
% Contributor(s): Kish Shen, IC-Parc.
% 
% END LICENSE BLOCK

%%%% Compiler for CHR
%% Modified by Kish Shen, March 98, to print more readable compiled code

:- module(chr2pl).

:- pragma(deprecated_warnings(off)).

:- export
	op(1200, fy, handler),
	op(1200, fy, constraints),
	op(1200, fy, [label_with]),
	op(1190, xfx, [==>, <=>]),
	op(1200, xfx, :--),
	op(1200, xfx, @),
	op(1100, xfx, |),
	op(1100, xfx, \ ),
	op(700, xfx, flag).


%%%%%%%%%%%% Exported predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- export chr2pl/1, chr/1.

:- tool(chr/1, chr/2).

:- local variable(gen_num).

:- skipped((chr2pl/1, chr/2)).

:- local portray('CHRhead_not_kept'/1, id/2, []).

id(X,X).

%%%%%%% Predicates used to store what have been read
:- dynamic (handler)/1, (constraints)/1, (label_with)/1, rule/5, (:--)/2.


%%% Because, calls to macros are generated:
:- set_flag(macro_expansion, off).


chr(File, Module) :-
	chr2pl(File),
	basename_chr(File, _, File_pl),
	call(compile(File_pl), Module).

:- mode chr2pl(++).
%%% File is an atom or a string
chr2pl(File) :-
        set_flag(syntax_option, '$VAR'),
	cputime(StartTime),

	init,
	basename_chr(File, FileChr, File_pl),
	open(File_pl, write, file_pl),
	header,
	read_all(FileChr),
	!,
	translate,
	close(file_pl),
	end,

	CompileTime is cputime - StartTime,
	get_file_info(File_pl, size, Size),
	atom_string(FileChrAtom, FileChr),
	error(139, (FileChrAtom, Size, CompileTime), chr2pl).


:- mode read_all(++).
%%% File is a string
% read the .chr file,
% store the CHR part (assert)
% Execute the '?-' queries
% copy the rest (Prolog) in the .pl
read_all(File) :-
    ( File == "user" ->
	ChrIn = input
    ;
	open(File, read, ChrIn)
    ),
    read_and_write(ChrIn),
    close(ChrIn).

read_and_write(Stream) :-
	call(read(Stream, Term), eclipse),
	( Term == end_of_file
         -> true
        ;
	  ( treat(Term)
           ->
	    true
	   ;
	    error(110, Term)
	  ),
	  read_and_write(Stream)
        ).


%%%%%%%%% Global Initialization %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init :-
	setval(gen_num, 0),
	get_flag(print_depth, PrintDepth),
	setval(print_depth, PrintDepth),
	set_flag(print_depth, 100000), % The printer is used to produce code
	setval(check_guard_bindings, on),
	setval(already_in_store, on),
	setval(already_in_heads, on),
	retract_all(handler _),
	retract_all(constraints _),
	retract_all(label_with _),
	retract_all(rule(_, _, _, _, _)),
	retract_all( _ :-- _).


end :-
	getval(print_depth, PrintDepth),
	set_flag(print_depth, PrintDepth).
	
%%%%%%%%% Syntax %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

treat(operator(A, B, C)) :-
	!, %%% operator declaration
	call(op(A, B, C), eclipse),
	printf(file_pl, "%q.\n", [:- op(A, B, C)]).
treat(option(Flag, Value)) :-
	!,
	setval(Flag, Value).
treat((?- Goal)) :-
	!,
	Goal.
treat(Rule) :-
	is_rule(Rule),
	!,
	syn_rule(Rule, Syn),
	preprocess_rule(Syn, Syn1),
	assert(Syn1).
treat(Term) :-
	chr_item(Term),
	!,
	assert(Term).
treat(Head :- Body) :-
	is_a_constraint(Head),
	!,   %%% Clause for a constraint
	assert(Head :-- Body).
treat(Head) :-
	is_a_constraint(Head),
	!,   %%% Clause for a constraint
	assert(Head :-- true).
treat(Prolog) :-%%% Prolog stuff
	printf(file_pl, "%q.\n", [Prolog]).


chr_item(handler(Name)) :- atomic(Name).
chr_item(constraints(SL)) :-
	( speclist(SL)
         ->
	  true
         ;
	  error(110, constraints(SL))
        ).
chr_item(label_with(Head if Condition)) :-
	( is_a_constraint(Head)
         ->
	  true
         ;
	  error(110, label_with(Head if Condition))
        ).


is_a_constraint(Goal) :-
	functor(Goal, Functor, Arity),
	constraint_list(List),
	memberchk(Functor/Arity, List).


is_rule(_ <=> _).
is_rule(_ ==> _).
is_rule(_ @ _).


speclist((SL1, SL2)) :- -?->
	!,
	speclist(SL1),
	speclist(SL2).
speclist(Functor/Arity) :-
	atom(Functor),
	integer(Arity).


syn_rule((Name @ Rule), rule(Kind, Name, Heads, Guard, Body)) :-
	!,
	syn_anonym_rule(Rule, Kind, Heads, Guard, Body).
syn_rule(Rule, rule(Kind, Name, Heads, Guard, Body)) :-
	gen_sym(S),
	Name = anonymous(S),
	syn_anonym_rule(Rule, Kind, Heads, Guard, Body).

syn_anonym_rule((Heads <=> Rule_Body), replacement, Syn_Heads, Guard, Body) :-
	syn_heads(Heads, Syn_Heads),
	syn_rule_body(Rule_Body, Guard, Body).
syn_anonym_rule((Heads ==> Rule_Body), augmentation, Syn_Heads, Guard, Body) :-
	syn_heads(Heads, Syn_Heads),
	syn_rule_body(Rule_Body, Guard, Body).


syn_heads((HeadKf1 \ HeadKf2), (kf(Head1, KF1) \ kf(Head2, KF2))) :-
	!,
	syn_head(HeadKf1, Head1, KF1),
	syn_head(HeadKf2, Head2, KF2).
syn_heads((HeadKf1, HeadKf2), [kf(Head1, KF1), kf(Head2, KF2)]) :-
	!,
	syn_head(HeadKf1, Head1, KF1),
	syn_head(HeadKf2, Head2, KF2).
syn_heads(HeadKf, [kf(Head, KF)]) :-
	syn_head(HeadKf, Head, KF).


syn_head(Head flag KF, Head1, KF1) :- -?->
	!,
	Head1 = Head, KF1 = KF.
syn_head(Head, Head, _KF) :-
	is_a_constraint(Head),
	!.
syn_head(_Head, _, _) :-
	printf(stderr, "Wrong head of rule\n", []),
	fail.
	


syn_rule_body((Guard | Body), Guard, Body) :- !.
syn_rule_body( Body, true, Body).


preprocess_rule(rule(replacement, Name, [Head1, Head2], Guard, B), Rule) :-
	getval(already_in_heads, on), !,
	keep_heads_double_repl(B, G, Body, Head1, Head2, Kind),
	( Kind == augmentation
         -> %%% Both heads may be kept
	  Rule = rule(augmentation, Name, [Head1, Head2], (Guard, G), Body)
        ; Kind == keep_first
         -> %%% First head may be kept
	  Rule = rule(replacement, Name, (Head1 \ Head2), (Guard, G), Body)
        ; Kind == keep_second
         -> %%% Second head may be kept
	  Rule = rule(replacement, Name, (Head2 \ Head1), (Guard, G), Body)
        ; Rule = rule(replacement, Name, [Head1, Head2], Guard, B)
        ).
preprocess_rule(Rule, Rule).


keep_heads_double_repl(Body, _Guard, Body, _, _, Kind) :-
	nonvar(Kind), !. %%% Job already done for a previous goal
keep_heads_double_repl((B1, B2), Guard, (B11, B21), H1, H2, Kind) :-
	!,
	keep_heads_double_repl(B1, Guard, B11, H1, H2, Kind),
	keep_heads_double_repl(B2, Guard, B21, H1, H2, Kind).
keep_heads_double_repl(G, Guard, Goal, kf(H1, KF1), kf(H2, KF2), Kind) :-
	( \+ \+ H1 = G
         ->  %%% First does match
	  ( \+ \+ H2 = G
           -> %%% Second head does match
	    Kind = augmentation,
	    Guard = 'CHRkeep_heads_checking'(H1, KF1, H2, KF2, G, CallG),
	    Goal = ('CHRhead_not_kept'(CallG) -> G ; true)
	   ; %%% Second head does not match
	    Kind = keep_first,
	    Guard = 'CHRkeep_heads_checking'(H1, KF1, G, CallG),
	    Goal = ('CHRhead_not_kept'(CallG) -> G ; true)
	  )
         ; %%%  First head does not match
	  \+ \+ H2 = G
         ->  %%% Second head matches
	  Kind = keep_second,
	  Guard = 'CHRkeep_heads_checking'(H2, KF2, G, CallG),
	  Goal = ('CHRhead_not_kept'(CallG) -> G ; true)
	 ; %%% Nothing match
	  Goal = G
        ).

%%%%%%% Translation from .chr to .pl, topelevel %%%%%%%%%%%%%%%%%%%
translate :-
	get_constraints(Constraints),
	l_tr_callable(Constraints),
	l_tr_constraint(Constraints),
	tailer.

:- mode get_constraints(-).
%%% Returns the list of declared constraints
get_constraints(Constraints) :-
	findall(C, constraint_list(C), List_Of_Lists),
	flatten(List_Of_Lists, Constraints).


constraint_list(List) :-
	constraints(Conjunction),
	conjunction2list(Conjunction, List).


conjunction2list((A , B), [A | List]) :-
	!,
	conjunction2list(B, List).
conjunction2list(A, [A]).



%%%%%%% Translation of label_with declarations %%%%%%%%%%%%%%%%%%%%
:- mode l_tr_callable(++).
%%% l_tr_callable(Constraint's spec List)
l_tr_callable([]).
l_tr_callable([Constraint | Constraints]) :-
	tr_callable(Constraint),
	l_tr_callable(Constraints).


tr_callable(Name/Arity) :-
	functor(Head, Name, Arity),
	findall(callable2(Head, Guard), (label_with Head if Guard), L_Callable),
	translate_callable(Name, L_Callable, L_Translated_Callable),
	( L_Translated_Callable = [] -> true
        ; printf(file_pl, "\n\n\n%%%%%% Callables for %p\n\n", [Name/Arity]),
	  l_out_clause(L_Translated_Callable)
        ).


translate_callable(_, [], []).
translate_callable(Name, [callable2(Head, Guard) | L_C], [R_C | L_R_C]) :-
	pred_name_rename(Name, Prolog_Goal_Name),
	Head =.. [Name | Args],
	Prolog_Goal =.. [Prolog_Goal_Name | Args],
	copy_term([Head | Guard], [HeadC | GuardC]),
	build_guard_checking(Guard, Head, Check_The_Guard),
	R_C = ('CHRlabel_with'(Head, Goal, Nb) ?-
	       coca(try_clause(Nb, Head, HeadC, GuardC)),
	       Check_The_Guard,
	       coca(clause_fired(Nb)),
	       'CHR='(Goal, Prolog_Goal)),
	translate_callable(Name, L_C, L_R_C).



%%%%%%%%% Translation of rules and clauses %%%%%%%%%%%%%%%%%%%%%%%%%

:- mode l_tr_constraint(++).
%%% l_tr_constraint(constraint's spec List)
l_tr_constraint([]).
l_tr_constraint([Constraint | Constraints]) :-
	tr_constraint(Constraint),
	l_tr_constraint(Constraints).


:- mode tr_constraint(++).
%%% tr_constraint(A constraint's spec)
tr_constraint(Constraint_Spec) :-
	tr_clauses(Constraint_Spec),
	gen_constraint(Constraint_Spec).


:- mode gen_constraint(++).
gen_constraint(Name/Arity) :-
	constraint2pred(Name/Arity, Renamed),

	gen_call_to_pred(Name/Arity, Renamed),
	
	printf(file_pl, "\n\n\n%%%%%% Rules handling for %p\n\n", [Name/Arity]),

        functor(Constraint, Name, Arity),
	First_Head =.. [Renamed, Constraint, KF, _PA, _Nb],
	( getval(already_in_store, on)
         ->
	  l_out_clause([(First_Head :- ('CHRnonvar'(KF) ; 'CHRalready_in'(First_Head), coca(already_in)), !)])
         ;
	  l_out_clause([(First_Head :- 'CHRnonvar'(KF), !)])
        ),

	tr_single_replacements(Name/Arity, Renamed),

	add_suffix(Renamed, Single_Augm_Name),
	tr_double_replacements_and_mixed_right(Name/Arity, Renamed, Single_Augm_Name),

/*** treated by opium. Not compatible with DELAY-RESUME ports
	gen_notrace_set_flag(Renamed/4, Directive),
	out_query(Directive),
***/
        gen_notrace_set_flag(Renamed/4, SetFlagDirective),
	out_query(SetFlagDirective),
	gen_printing_macro(Renamed/4, MacroDirective),
	out_query(MacroDirective),

	add_suffix(Renamed, Mixed_Left_Name),
	tr_single_augmentations(Name/Arity, Single_Augm_Name, Mixed_Left_Name),

	tr_mixed_left_and_double_augmentations(Name/Arity, Mixed_Left_Name, Renamed).



gen_call_to_pred(Constraint_Name/Arity, Pred_Name) :-
	functor(Head, Constraint_Name, Arity),
	Body =.. [Pred_Name, Head, _KF, _PA, Nb],
	l_out_clause([(Head :- 'CHRgen_num'(Nb), coca(add_one_constraint(Nb, Head)), Body)]).




%%%%%%%% Translation of clauses %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- mode tr_clauses(++).
tr_clauses(Name/Arity) :-
	functor(Head, Name, Arity),
	pred_name_rename(Name, ReName),
	findall(Clause, get_clause_and_rename(Head, ReName, Clause), L_Clause),
	( L_Clause = [] -> true
        ; printf(file_pl, "\n\n\n%%%%%% Prolog clauses for %p\n\n", [Name/Arity]),
	  l_out_clause(L_Clause),
	  gen_printing_macro(ReName/Arity, Directive),
	  out_query(Directive)
        ).


get_clause_and_rename(Head, ReName, (New_Head :- Body)) :-
	(Head :-- Body),
	Head =.. [_ | Args],
	New_Head =.. [ReName | Args].


%%%%%%%% Head <=> Body %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode tr_single_replacements(++, ++).
tr_single_replacements(Name/Arity, Pred_Name) :-
	functor(Head, Name, Arity),
	findall(single(H, G, B, replacement, Rule_Name), get_single(Head, H, G, B, replacement, Rule_Name), L_Clause),
	l_compile_single(Pred_Name, L_Clause, L_Renamed_Clause),
	( L_Renamed_Clause = [] -> true
        ; 
	  l_out_clause(L_Renamed_Clause)
        ).


get_single(Head1, Head, Guard, Body, Kind, Name) :-
	Head = kf(Head1, _KF),
	rule(Kind, Name, [Head], Guard, Body).


:- mode l_compile_single(++, ++, -).
l_compile_single(_, [], []).
l_compile_single(Pred_Name, [single(HKf, Guard, Body, _Kind, Rule_Name) | L_C], [R_C | L_R_C]) :-
	HKf = kf(H, KF),
	Head =.. [Pred_Name, H, KF, _PA, Nb],
	copy_term((H :- Guard | Body), (HC :- GuardC | BodyC)),
	build_guard_checking(Guard, H, Check_The_Guard),
	R_C = (Head ?-
	       coca(try_rule(Nb, H, Rule_Name, HC, replacement, GuardC, BodyC)),
	       Check_The_Guard, !, 'CHRkill'(KF),
	       coca(fired_rule(Rule_Name)),
	       Body),
	l_compile_single(Pred_Name, L_C, L_R_C).



%%%%%%%% HEAD ==> BODY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode tr_single_augmentations(++, ++, ++).
tr_single_augmentations(Name/Arity, Pred_Name, Cont_Name) :-
	functor(Head, Name, Arity),
	findall(single(H, G, B, augmentation, Rule_Name), get_single(Head, H, G, B, augmentation, Rule_Name), L_Clause),
	
	l_compile_single_augm(Pred_Name, Cont_Name, L_Clause).

l_compile_single_augm(PredName, ContName, []) :-
	Head =.. [PredName, Constraint, KF, PA, Nb],
	Body =.. [ContName, Constraint, KF, PA, Nb],
	l_out_clause([Head :- Body]),
	gen_notrace_set_flag(PredName/4, Directive),
	out_query(Directive).
l_compile_single_augm(PredName, ContName, [single(HKf, Guard, Body, _Kind, Rule_Name) | L_C]) :-
	HKf = kf(H, KF),
	Head1 =.. [PredName, H, KF, PA, Nb],
	add_suffix(PredName, NextName),
	Cont1 =.. [NextName, H, KF, PA, Nb],
	copy_term((H :- Guard | Body), (HC :- GuardC | BodyC)),
	build_guard_checking(Guard, H, Check_The_Guard),
	Clause1 = (Head1 ?-
	           'CHRvar'(KF),
	           'CHRcheck_and_mark_applied'(PredName, PA),
	           coca(try_rule(Nb, H, Rule_Name, HC, augmentation, GuardC, BodyC)),
		   Check_The_Guard, !, Cont1,
		   coca(fired_rule(Rule_Name)),
		   Body),
	Head2 =.. [PredName, Constraint, KF, PA, Nb],
	Cont2 =.. [NextName, Constraint, KF, PA, Nb],
	Clause2 = (Head2 ?- Cont2),
	l_out_clause([Clause1, Clause2]),
	gen_notrace_set_flag(PredName/4, Directive),
	out_query(Directive),
	l_compile_single_augm(NextName, ContName, L_C).



%%%%%%%% HEAD1, HEAD2 <=> BODY ; HEAD1 \ HEAD2 <=> Body %%%%%%%%%%%%

:- mode tr_double_replacements_and_mixed_right(++, ++, ++).
tr_double_replacements_and_mixed_right(Name/Arity, Pred_Name, Cont_Name) :-
	functor(Head, Name, Arity),
	findall(double(H1, H2, G, B, K, Order, Rule_Name),
	        ( (K = replacement; K = keep_second),
		  get_double(Head, H1, H2, G, B, K, Order, Rule_Name)
		  ),
		L_Clause),
	l_compile_double_repls(Head, Pred_Name, L_Clause, L_Renamed_Clause, Printed_After),
	( L_Renamed_Clause = [] -> true
        ; 
	  l_out_clause(L_Renamed_Clause)
        ),
	Head_Last =.. [Pred_Name, Head, KF, PA, Nb],
	Body_Last =.. [Cont_Name, Head, KF, PA, Nb],
	l_out_clause([Head_Last :- Body_Last]),
	l_out_clause(Printed_After).


%%% Order is returned only for augmentation rules ('12' if heads are
%%% returned like in the source, '21' if not)
get_double(Head, Head1, Head2, Guard, Body, Kind, Order, Name) :-
	Head1 = kf(Head, _),
	( Kind = augmentation,
	  rule(augmentation, Name, [Head1_a, Head2_a], Guard, Body),
          ( [Head1_a, Head2_a] = [Head1, Head2], Order = '12'
          ; [Head1_a, Head2_a] = [Head2, Head1], Order = '21'
          )
        ; Kind = replacement,
          rule(replacement, Name, [Head1_r, Head2_r], Guard, Body),
          ( [Head1_r, Head2_r] = [Head1, Head2]
          ; [Head1_r, Head2_r] = [Head2, Head1]
          )
        ; Kind = keep_first,
	  rule(replacement, Name, (Head1 \ Head2), Guard, Body)
        ; Kind = keep_second,
	  rule(replacement, Name, (Head2 \ Head1), Guard, Body)
        ).


l_compile_double_repls(_, _, [], [], []).
l_compile_double_repls(Constraint, Pred_Name, [double(H1Kf, H2Kf, G, B, Kind, _Order, Rule_Name) | L_C], [R_C | L_R_C], [P_H, P_L, SF_C | Printed_After]) :-
	H1Kf = kf(H1, KF1),
	H2Kf = kf(H2, KF2),
	Head =.. [Pred_Name, H1, KF1, _PA, Nb1],
	shared_variable(H1, H2, Shared_Var),
	common_vars(H1, H2, Shared_Vars),
	common_vars(H2Kf, (G, B), Common_Vars),
	delta_vars(Common_Vars, Shared_Vars, Other_Vars),
	copy_term(Other_Vars, Other_Vars_Copy),
	copy_term((H1, H2 :- G | B), (H1C, H2C :- GC | BC)),

	add_suffix(Pred_Name, Partner_Name),
	Call_To_Partner =.. [Partner_Name, Cs, Shared_Vars, Other_Vars, Nb2],
	build_guard_checking(G, (H1, H2), Check_The_Guard),
	R_C = (Head ?-
	       'CHRget_delayed_goals'(Shared_Var, Cs),
	       Call_To_Partner,
	       coca(try_double(Nb1, H1, Nb2, H2, H1C, H2C, Kind, GC, BC, Rule_Name)),
	       Check_The_Guard,
	       !,
	       'CHRkill'(KF1), %%% KF2 is set by the partner
	       coca(fired_rule(Rule_Name)),
	       B),

	functor(H2, Head2_Name, Head2Arity),
	constraint2pred(Head2_Name/Head2Arity, Pred2_Name),
	Partner_Head =.. [Pred2_Name, H2, KF2, _PA, Nb2],
	Head_Partner =.. [Partner_Name, [Partner_Head |_Cs], Shared_Vars, Other_Vars_Copy, Nb2C],
	(Kind = replacement -> Kill = ('CHRkill'(KF2)) ; Kill = true),
	P_H = (Head_Partner ?-
	                      'CHRvar'(KF2),
	                      Kill,
			      'CHR='(Other_Vars, Other_Vars_Copy),
			      'CHR='(Nb2, Nb2C)),
	partner_loop(Partner_Name, P_L),
	gen_notrace_set_flag(Partner_Name/4, SF_C),
	l_compile_double_repls(Constraint, Pred_Name, L_C, L_R_C, Printed_After).


partner_loop(Name, (Head :- Body)) :-
	Head =.. [Name, [_ | Cs], Shared_Vars, Other_Vars, Nb],
	Body =.. [Name, Cs, Shared_Vars, Other_Vars, Nb].


%%%%%%%%% HEAD1 \ HEAD2 <=> BODY, HEAD1, HEAD2 ==> BODY %%%%%%%%%%%%%%%

:- mode tr_mixed_left_and_double_augmentations(++, ++, ++).
tr_mixed_left_and_double_augmentations(Name/Arity, Pred_Name, Delay_Name) :-
	functor(Head, Name, Arity),
	findall(double(H1, H2, G, B, K, Order, Rule_Name),
	        ( (K = keep_first; K = augmentation),
		  get_double(Head, H1, H2, G, B, K, Order, Rule_Name)
		  ),
		L_Clause),
	l_compile_double_augms(Head, Pred_Name, Delay_Name, L_Clause).




l_compile_double_augms(Constraint, Pred_Name, Delay_Name, []) :-
	Head_Last =.. [Pred_Name, Constraint, KF, PA, Nb],
	To_Be_Delayed =.. [Delay_Name, Constraint, KF, PA, Nb],
	l_out_clause([Head_Last :- 'CHRvar'(KF) -> 'CHRdelay'([KF, Constraint], To_Be_Delayed) ; true]),
	gen_notrace_set_flag(Pred_Name/4, Directive),
	out_query(Directive).	
l_compile_double_augms(Constraint, Pred_Name, Delay_Name, [double(H1Kf, H2Kf, G, B, Kind, Order, Rule_Name) | L_C]) :-
	H1Kf = kf(H1, KF1),
	H2Kf = kf(H2, KF2),
	Head =.. [Pred_Name, H1, KF1, PA, Nb1],

	add_suffix(Pred_Name, Partner_Name),
	Call_To_Partner =.. [Partner_Name, Cs, KF1, H1, PA, Nb1],

	shared_variable(H1, H2, Shared_Var),
	Clause1 = (Head ?-
	       'CHRvar'(KF1), !,
	       'CHRget_delayed_goals'(Shared_Var, Cs),
	       Call_To_Partner),

	Clause2_Head =.. [Pred_Name, Constraint, KF1, PA, Nb1],

	add_suffix(Pred_Name, Next_Name),
	Continuation =.. [Next_Name, Constraint, KF1, PA, Nb1],
	Clause2 = (Clause2_Head :- Continuation),

	functor(H2, Head2_Name, Head2Arity),
	constraint2pred(Head2_Name/Head2Arity, Pred2_Name),
	Partner_Constraint =.. [Pred2_Name, H2, KF2, PA2, Nb2],
	Head_Partner =.. [Partner_Name, [Partner_Constraint |Cs], KF1, H1, PA1, Nb1],
	( \+ \+ (H1 = H2), Kind = augmentation
         -> %%% the rule (c(X), c(Y) ==> c1(X, Y)) can be applied twice on
	    %%% the same constraints
	  Ident =.. [Order, Rule_Name]
        ;
	  Ident = Rule_Name
        ),
	( Kind = keep_first
         ->
	  Check = true,
	  Kill = 'CHRkill'(KF2)
        ; %%% augmentation
	  Check = 'CHRcheck_and_mark_applied'(Ident, KF1, KF2, PA1, PA2),
	  Kill = true
        ),

	Loop =.. [Partner_Name, Cs, KF1, H1, PA1, Nb1],

	copy_term((H1, H2 :- G | B), (H1C, H2C :- GC | BC)),
	build_guard_checking(G, (H1, H2), Check_The_Guard),
	Partner_Handling = (Head_Partner ?-
	                     'CHRvar'(KF2), Check,
			     coca(try_double(Nb1, H1, Nb2, H2, H1C, H2C, Kind, GC, BC, Rule_Name)),
			     Check_The_Guard, !,
			     Kill,
			     coca(fired_rule(Rule_Name)),
			     Loop,
			     B),
        %%% The body B is after the Loop because the head may be needed
        %%% as partner while solving this body: Loop ends with the delay
        %%% of this head

	l_out_clause([Clause1, Clause2]),
	gen_notrace_set_flag(Pred_Name/4, Directive1),
	out_query(Directive1),

	l_out_clause([Partner_Handling]),
	out_partner_loop_and_continuation(Partner_Name, Next_Name),
	gen_notrace_set_flag(Partner_Name/5, Directive2),
	out_query(Directive2),
	l_compile_double_augms(Constraint, Next_Name, Delay_Name, L_C).



out_partner_loop_and_continuation(Name, Next_Name) :-
	Head1 =.. [Name, [_ | Cs], KF1, First_Head, PA, Nb1],
	Body1 =.. [Name, Cs, KF1, First_Head, PA, Nb1],
	l_out_clause([Head1 :- Body1]),
	Head2 =.. [Name, [], KF1, Constraint, PA, Nb1],
	Body2 =.. [Next_Name, Constraint, KF1, PA, Nb1],
	l_out_clause([Head2 :- Body2]).

%%%%%%%% Misc %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_notrace_set_flag(Name, (:- set_flag(Name, leash, notrace))).

gen_printing_macro(Spec, (:- current_macro(Spec, _, _, _) -> true ; define_macro(Spec, tr_chr/2, [write]))).


build_guard_checking(Guard, _, Check_The_Guard) :-
	getval(check_guard_bindings, off),
	!,
	Check_The_Guard = no_delayed_goals(Guard).
build_guard_checking(Guard, Heads, Check_The_Guard) :-
	getval(check_guard_bindings, on),
	!,
	Check_The_Guard = no_global_bindings(Guard, Heads).
build_guard_checking(_, _, _) :-
	printf(error, "Error: check_guard_bindings value not correct\n", []),
	abort.


:- mode pred_name_rename(++, -).
pred_name_rename(Name, Clause_Name) :-
	concat_atom(['clause_', Name], Clause_Name).


:- mode constraint2pred(++, -).
constraint2pred(Name/Arity, Pred_Name) :-
	concat_atom(['CHR', Name, '_', Arity], Pred_Name).


add_suffix(Name, Renamed) :-
	gen_sym(Suffix),
	atom_string(Name, Name_S),
	concat_string([Name_S, "__", Suffix], Renamed_Name),
	atom_string(Renamed, Renamed_Name).
	

gen_sym(S) :-
	getval(gen_num, X),
	number_string(X, S),
	incval(gen_num).


shared_variable(T1, T2, V) :-
	term_variables(T1, V1),
	( l_occurs(V1, T2, V)
        ->
	  true
        ;
	  V = true
        ).


l_occurs([X | _L], T, X) :-
	occurs(X, T),
	!.
l_occurs([_ | L], T, X) :-
	l_occurs(L, T, X).


% basename_chr(File, ChrFile, PlFile)
%   resolves the input file, ChrFile is the file to read from or user
%   PlFile is the name of the .pl file
basename_chr(lib(File), ChrFile, PlFile) :-
	-?->
	(atom(File); string(File)),
	!,
	get_flag(installation_directory, Inst),
	concat_string([Inst, "/lib/chr/", File, ".chr"], ChrFile),
	concat_string([File, ".pl"], PlFile).
basename_chr(user, "user", "user.pl") :- !.
basename_chr(File, ChrFile, PlFile) :-
	(atom(File); string(File)),
	!,
	concat_string([File, ".chr"], ChrFile),
	concat_string([File, ".pl"], PlFile). 
basename_chr(File, _, _) :-
	nonvar(File),
	error(5, chr2pl(File)).
basename_chr(File, _, _) :-
	var(File),
	error(4, chr2pl(File)).
       


%%%%%%%%%%%%%%%%%%%%%%%%%% PRETTY PRINTING %%%%%%%%%%%%%%%%%%%%%%%%%
:- mode l_out_clause(++).
l_out_clause([]).
l_out_clause([Clause | L]) :-
	simpl(Clause, SClause),
	out_clause(SClause),
	l_out_clause(L).

out_clause(C) :- numbervars(C,0,_), writeclause(file_pl, C), fail.
% clean up after numbervars
out_clause(_).


numbervars('$VAR'(N), N, M) :- !, M is N + 1.
numbervars(Term, N, M) :- 
	functor(Term, _, Arity),
        numbervars(0, Arity, Term, N, M).

numbervars(Arity, Arity, _, N, N) :- !.
numbervars(I, Arity, Term, N0, N) :-
	I1 is I + 1,
	arg(I1, Term, Arg),
	numbervars(Arg, N0, N1),
        numbervars(I1, Arity, Term, N1, N).

/*
out_clause(Head :- Body) :-
	set_flag(gc, off),
	printf(file_pl, "%OQVw :-\n", Head),
        out_goal(Body),
	printf(file_pl, "\n", []),
	set_flag(gc, on).
out_clause(Head ?- Body) :-
	set_flag(gc, off),
	printf(file_pl, "%OQVw ?-\n", Head),
        out_goal(Body),
	printf(file_pl, "\n", []),
	set_flag(gc, on).
out_clause(:- Goal) :-
	set_flag(gc, off),
	printf(file_pl, ":-\n", []),
        out_goal(Goal),
	set_flag(gc, on).
*/

out_query(:- Goal) :- 
	writeclause(file_pl, :- Goal).

/*out_query(:- Goal) :-
	set_flag(gc, off),
	printf(file_pl, ":-\n", []),
        out_goal(Goal),
	printf(file_pl, "\n", []),
        set_flag(gc, on).
*/

:- setval(left, off).

out_goal((Goal1, Goal2)) :-
	!,
	setval(left, on),
	out_goal(Goal1),
	setval(left, off),
	out_goal(Goal2).
out_goal(Goal) :-
	getval(left, on), !,
	( Goal =.. [Op | _],
	  current_op(Priority, _, Op),
	  Priority > 1000
         ->
	  printf(file_pl, "%t(%OQVw),\n", Goal)
        ;
	  printf(file_pl, "%t%OQVw,\n", Goal)
        ).
out_goal(Goal) :-
	( Goal =.. [Op | _],
	  current_op(Priority, _, Op),
	  Priority > 1000
         ->
	  printf(file_pl, "%t(%OQVw).\n", Goal)
        ;
	  printf(file_pl, "%t%OQVw.\n", Goal)
        ).


header :-
	printf(file_pl, "\n%%%%%% The following code has been produced by the CHR compiler\n\n", []),
        printf(file_pl, "\n:- ( current_module(chr) -> true ; use_module(library(chr)) ).\n", []),
	printf(file_pl, "\n:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).\n", []).

tailer :-
	printf(file_pl, "\n:- getval(variable_names_flag, Val), set_flag(variable_names, Val).\n", []).


%%%%%%% Simplification fo the produced code %%%%%%%%%%%%%%%%%%%%%%%%%%%%


simpl((Head :- Body), (Head :- SBody)) :- !,
	simpl(Body, SBody).
simpl((Head ?- Body), (Head ?- SBody)) :- !,
	simpl(Body, SBody).
simpl((:- Goal), (:- SGoal)) :- !,
	simpl(Goal, SGoal).
simpl((Goal1, Goal2), SGoal) :-
	!,
	simpl(Goal1, SGoal1),
	simpl(Goal2, SGoal2),
	compose_goals(SGoal1, SGoal2, SGoal).
simpl(Goal, SSGoal) :-
	simpl_goal(Goal, SGoal),
	( get_flag(debug_compile, off)
         ->
	  nodbg_simpl_goal(SGoal, SSGoal)
        ; SSGoal = SGoal
        ).


compose_goals(true, Goal, Goal) :- !.
compose_goals(Goal, true, Goal) :- !.
compose_goals(X = Y, Goals, Comp) :-
	first_goal(Goals, Goal1, Rest),
	Goal1 = (X1 = Y1),
        !,
	compose_goals([X, X1] = [Y, Y1], Rest, Comp).
compose_goals(Goal1, Goal2, (Goal1, Goal2)).


first_goal((Goal1, Rest), Goal1, Rest) :- !.
first_goal(Goal1, Goal1, true).


nodbg_simpl_goal(coca(_), true) :- !.
nodbg_simpl_goal('CHRgen_num'(_), true) :- !.
nodbg_simpl_goal(X = X, Goal)
 :- -?->
	Goal = true,
	!.
nodbg_simpl_goal([X] = [Y], Goal)
 :- -?->
	!,
	nodbg_simpl_goal(X = Y, Goal).
nodbg_simpl_goal('CHR='(X, Y), Goal) :-
	!,
	simpl(X = Y, Goal).
nodbg_simpl_goal('CHRkill'(X), X = true) :- !.
nodbg_simpl_goal('CHRvar'(X), var(X)) :- !.
nodbg_simpl_goal('CHRnonvar'(X), nonvar(X)) :- !.
nodbg_simpl_goal(Goal, Goal).

simpl_goal(no_global_bindings(true, _), true) :- !.
simpl_goal(no_delayed_goals(true), true) :- !.
simpl_goal(Goal, Goal).


%%%%%%%%% Manipulations of list of variables %%%%%%%%%%%%%%%%%%%

%%% From the lists.pl library
intersection_identicals([], _, []).
intersection_identicals([Head|L1tail], L2, L3) :-
	memberchk_identical(Head, L2),
	!,
	L3 = [Head|L3tail],
	intersection_identicals(L1tail, L2, L3tail).
intersection_identicals([_|L1tail], L2, L3) :-
	intersection_identicals(L1tail, L2, L3).


memberchk_identical(X,[X|_]) :- -?-> !.
memberchk_identical(X,[_|T]):- memberchk_identical(X,T).


delta_identicals([], _, []).
delta_identicals([Head|L1tail], L2, L3) :-
	memberchk_identical(Head, L2),
	!,
	delta_identicals(L1tail, L2, L3).
delta_identicals([Head|L1tail], L2, [Head|L3tail]) :-
	delta_identicals(L1tail, L2, L3tail).

common_vars(T1, T2, Vars) :-
	term_variables(T1, V1),
	term_variables(T2, V2),
	intersection_identicals(V1, V2, Vars).

/*** T1 and T2 are lists of variables ***/
delta_vars(T1, T2, Vars) :-
	delta_identicals(T1, T2, Vars).



:- set_flag(macro_expansion, on).


