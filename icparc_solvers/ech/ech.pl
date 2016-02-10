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
% Contributor(s): Kish Shen, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: ech.pl,v 1.6 2013/02/16 02:55:20 kish_shen Exp $
% ----------------------------------------------------------------------

%  New CHR implementation
%  Kish Shen, March - June 1998, version 1
%             March - April, 1999, some fixes
%             Sept 1999,  further fixes
%             Sept 2001, merged into one file
%  Partial support for multi-head (>2) rules: no multi-head propagation rules
%  Syntax changes and minor semantic changes from old CHR
%  Faster execution

:- module(ech).

:- meta_attribute(ech, [
			  unify:   unify_ech/2
                          %print:   print_ech/2
   ]).

:- export op(1100, fy, handler).
:- export op(1000, fy, constraints).
%:- export op(1200, fy, [label_with]).
:- export op(1190, xfx, [==>, <=>]).
%:- export op(1200, xfx, :--).
:- export op(1200, xfx, ::=). % replaces @ in old syntax
:- export op(1100, xfx, |).
:- export op(1100, xfx, \ ).
:- export op(700, xfx, flag).

:- local struct(chrcinfo(spec,count,prio)).
:- local variable(chr_priority, 9).

:- export record_chrrule/3, record_chrprule/3, new_constraints/3, 
          ignore_handler/2, record_namedrule/3, appliedpos/2,
          suspendindexpos/2, constraintnumpos/2,
          wrapperinpos/2, cdeletethreshold/2.

:- tool(record_namedrule/2, record_namedrule/3).
:- tool(record_chrrule/2, record_chrrule/3).
:- tool(record_chrprule/2, record_chrprule/3).
:- tool(new_constraints/2, new_constraints/3).


:- export suspend_constraint/4, get_global_constraint/2, 
          get_constraint_list/3, kill_constraint/2, check_pairapplied/6,
          check_pairapplied_direct/6, insert_pairapplied/6, is_in_store/2,
          create_applied/2, check_samepairapplied/6, insert_samepairapplied/6, 
          check_samepairapplied_direct/6, check_samepairapplied_directmarked/8,
          check_pairapplied_directmarked/8, find_partner/4, in_chrstore/2, 
	  chr_get_gconstraint/2, chr_get_vconstraint/3.


:- export chr/1, chr/2, in_chrstore/1, option/2, option/3, chr_get_constraint/1, 
       chr_get_constraint/2, (constraints)/1, (constraints)/2, (handler)/1.


:- tool(suspend_constraint/4, suspend_constraint/5).
:- tool(get_global_constraint/2, get_global_constraint/3).
:- tool(get_constraint_list/3, get_constraint_list/4).
:- tool(kill_constraint/2, kill_constraint/3).
:- tool(check_pairapplied/6, check_pairapplied/7).
:- tool(insert_pairapplied/6, insert_pairapplied/7).
:- tool(check_samepairapplied/6, check_samepairapplied/7).
:- tool(insert_samepairapplied/6, insert_samepairapplied/7).
:- tool(check_pairapplied_directmarked/8, check_pairapplied_directmarked/9).
:- tool(check_samepairapplied_directmarked/8, 
        check_samepairapplied_directmarked/9).
:- tool((chr)/1, (chr)/2).
:- tool(in_chrstore/1, in_chrstore/2).
:- tool(option/2, option/3).
:- tool((constraints)/1, (constraints)/2).
:- tool(chr_get_constraint/1, chr_get_gconstraint/2).
:- tool(chr_get_constraint/2, chr_get_vconstraint/3).



% avoid magic number in code
realconstraintpos(constraint_in_wrapper_pos, 2). 
% arg. pos for const. in wrapper
suspendindexpos(suspendid_pos, 3). % arg. pos for Index in wrapper
% arg. pos for In arg. in wrapper
wrapperinpos(wrapper_inpos, 4).
% arg. pos for global const. no. in wrapper
constraintnumpos(constraintnum_pos, 1). 
% arg. pos for applied-list in wrapper
appliedpos(applied_pos, 5).
% threshold for general clean-up of var's constraint list
%varslistthreshold(varslist_threshold, 100).
% threshold of number of kill_constraints before general cleanup
cdeletethreshold(cdelete_threshold, 15).

% macros for use when CHR syntax code is being read in

ignore_handler(_, []). 

record_chrrule(Rule, [], Module) :-
      % put in front as later we add to Processed at the front of the list
      check_if_new_or_update(Module),
      recorda('CHRcode', Rule)@Module. 

record_chrprule(Rule0, [], Module) :-
      check_if_new_or_update(Module),
      erase('CHRprule_count', count(Module,Np)),
      Np1 is Np + 1,
      (Rule0 = (Name ::= Rule1) -> 
         Rule = (Name ::= Np1-(Rule1)) ; Rule = Np1-(Rule0)
      ),
      recorda('CHRcode', Rule)@Module,
      recorda('CHRprule_count',count(Module,Np1)).

record_namedrule(NamedRule, [], Module) :-
     NamedRule = (_Name ::= Rule),
     ( Rule = (Head ==> Body) -> record_chrprule(NamedRule, [], Module)
    ;( Rule = (Head <=> Body) -> record_chrrule(NamedRule, [], Module)
    ;  writeln(error, "Syntax error, ::= is not followed by a valid rule in"),
       pretty_write(NamedRule), nl
     )).

new_constraints(constraints ConstDec, [], Module) :- % still support old syntax for now
   check_if_new_or_update(Module),
   erase('CHRconst_count', count(Module,NConst0)),
   count_and_record_constraints(ConstDec, NConst0, Count0, Module),
   recorda('CHRconst_count', count(Module,Count0)).

constraints(ConstDec, Module) :-
   check_if_new_or_update(Module),
   erase('CHRconst_count', count(Module,NConst0)),
   count_and_record_constraints(ConstDec, NConst0, Count0, Module),
   recorda('CHRconst_count', count(Module,Count0)).

handler _. % do nothing; for compatibility only



:- export macro((handler)/2, ignore_handler/3, [clause]).
:- export macro((==>)/2, record_chrprule/3, [clause]).
:- export macro((<=>)/2, record_chrrule/3, [clause]).
:- export macro((::=)/2, record_namedrule/3, [clause]).
:- export macro((constraints)/1, new_constraints/3, [clause]).
:- export macro(no_macro_expansion(constraint_in_wrapper_pos/0), realconstraintpos/2, []).
:- export macro(no_macro_expansion(suspendid_pos/0), suspendindexpos/2, []).
:- export macro(no_macro_expansion(constraintnum_pos/0), constraintnumpos/2, []).
:- export macro(no_macro_expansion(applied_pos/0), appliedpos/2, []).
:- export macro(no_macro_expansion(wrapper_inpos/0), wrapperinpos/2, []).
%:- export macro(varslist_threshold/0, varslistthreshold/2, []).
:- export macro(no_macro_expansion(cdelete_threshold/0), cdeletethreshold/2, []).


:- pragma(expand).



% global constraint count
:- local variable(constraint_number, 0).

:- import sepia_kernel.

:- set_flag(coroutine, on).


:- lib(lists).
:- lib(numbervars).


:- local struct(
         ech(
           slists,      % indexed store for suspension lists of constraint
           count
         )
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% `compiler'

chrcompile(Error, Culprit, Module) :- Culprit = (term,_,_), !,
   error(default(Error), Culprit, Module).
chrcompile(Error, Culprit, Module) :- Culprit = (_, _, dumped), !,
   error(default(Error), Culprit, Module).
chrcompile(Error, Culprit, Module) :- 
   recorded_list('CHRadding_code', Modules),
   erase_all('CHRadding_code'),
   (Modules \== [] -> % have read in some constraints 
        chrcompile_by_module(Modules) ; true
   ),
   error(default(Error), Culprit, Module).

chrcompile_by_module([]) :- !.
chrcompile_by_module([Module|Modules]) :-
   recorded('CHRconst_count', count(Module,N)),
   % N should be > 0
   recorded_list('CHRconstraints', Constraints)@Module,
   recorded_list('CHRcode', Rules)@Module,
   compile_term([(:- import sepia_kernel), (:- set_flag(coroutine, on)),
          number_of_constraints(N)])@Module,
   (transform(Rules,Constraints,N, Module) -> 
        true 
   ;
        printf(error, "***Compiling of CHRs failed in module %w. Please"
               " report problem.",
               [Module]),
        abort
   ),
   ArraySize is N + 1, % need +1 to avoid using 0.
   (current_array('CHRcdelete_count'(OldSize),_)@Module ->
      (OldSize \== ArraySize ->
         erase_array('CHRcdelete_count'/1)@Module,
         local(array('CHRcdelete_count'(ArraySize), integer))@Module
       ; true
      )
     ;local(array('CHRcdelete_count'(ArraySize), integer))@Module

   ), 
   chrcompile_by_module(Modules).

check_if_new_or_update(Module) :-
   (recorded('CHRadding_code', Module) ->
        true  
           
      ; % indicates that CHR code is being added since last (if any) compile
        recorda('CHRadding_code', Module)
   ), 
   (current_array('CHRcstore',_)@Module -> 
    % CHRcstore will be defined in Module if CHR encountered for Module
    % must use something from Module as the module may have been erased
        true ; initialise_module_for_chr(Module)
   ).


:- local initialization(set_event_handler(139, chrcompile/3)).

/* transform(+Rules, +Constraints, +N, +Module)
   transforms the list of CHR in rules into Prolog code. Constraints is the 
   list of declared constraints for these rules. N is the number of
   constraints, and Nprule is the number of propagation rules in the CHR
*/ 
transform(Rules, Constraints, N, Module) :-
    initialise_processed(N, ProcessedRules), 
    syntax_check(Rules, Constraints, ProcessedRules, Module),
    recorded('CHRprule_count', count(Module,Nprule)),
    compile_term(number_of_propagations(Nprule))@Module,
    translate(ProcessedRules, Constraints, Nprule, Module).


indexing_argsize(_N, 0).% :- writeln(_N). % for now

  
/* translate(+Processed, +Constraints, +Nprule, +Module)
   translate the processed rules in Processed into Prolog code. Constraints is
   the list of constraints, and Nprule is the number of propagation rules --
   if this is zero, the suspension can be simplified.
*/
translate(Processed,Constraints,Nprule, Module) :- 
    functor(Processed,_,Size),
    translate_each_constraint(Size, Processed, Constraints, Nprule, Module).

/* translate_each_constraint(+Nth, +Processed, +Constraints, +Nprule, +Module)
   translate the rules associated with the Nth constraint, i.e. those rules
   where the Nth constraint occurs in the head. Note that seperate code needs to 
   be generated for each occurance of a head constraint, except where the
   situation is symmetric and can be optimised
*/
translate_each_constraint(0,_,_,_,_) :- !.
translate_each_constraint(N, Processed, Constraints, Nprule, Module) :-
   arg(N,Processed,HRuleList),
   length(HRuleList, NRule),
   indexing_argsize(NRule, NRule1),
   nth_member(N, Constraints, F/A),
   printf(log_output, "Found CHR %a/%w in module %w, generating transformed"
          " code\n", [F,A, Module]),
   gen_code_for_constraint(HRuleList, [], F, A, 1, NRule1, Nprule, Code0, Module),
   optimise(Code0, Code), 
%   printcode(Code), 
   compile_term([(:- pragma(expand))|Code])@Module,
   N1 is N - 1, 
   translate_each_constraint(N1, Processed, Constraints, Nprule, Module).

/* construct_exec_constraint(+WrappedF, +WrappedA, +F, +A, +ConstNum, +
   Priority, +NRule, +Nprule, -Code, +Module)
    constructs the various codes for managing a particular constraint. Two 
    clauses are needed: one (WrappedF/WrappedA) that is suspended, and when 
    woken, calls the rules (with possible optimisations); and the other, 
    called from the plane constraint (F/A), which is used to initiate a
    new call to the rules. A demon directive for WrappedF/WrappedA is also
    needed so that it is not killed when woken. ConstNum is the index number
    into the constraint store for this constraint (F/A)
*/
construct_exec_constraint(WF, WA, CF, CA, ConstNo, Prio, NRule, Nprule, Code, Module) :-
    functor(WHead, WF, WA), % wrapped constraint 
    make_current_rule_name(CF, CA, 1, NRule, Nprule, FirstRule0, A),
    WHead =.. [_F,_ConsNum,Constraint|MetaArgs], 
    % WHead - suspended constraint, for wake up calls
    functor(Constraint,CF,CA),
    Constraint =.. [CF|ConsArgs], % original constraint form
    length(Index,NRule),
    instantiate_list(Index, 1),  % make all 1 for now 
    append(Index, ConsArgs, L1),
    append(L1, MetaArgs, ArgList),
    functor(FirstRule0, FirstRuleF, A),
    Body =.. [FirstRuleF|ArgList],
    add_check_in_store(Constraint, Body, ConstNo, Prio, Call, Module),
    % to add optimising code before Body in WHead
    Code = [(:- demon(WF/WA)), ( WHead :- Body), (Constraint :- Call)].


add_check_in_store(Constraint, Call, No, Prio, Check, Module) :-
   PriCall = call_priority(Call, Prio),
   (recorded('CHRdont_in_store', Module) ->
         Check = PriCall
       ; Check = (ech:get_global_constraint(No, SuspL), 
                   (ech:is_in_store(SuspL, Constraint) -> true ; PriCall)
                 )
   ).

/* extract the first Ath argument from the structure NewHead as a list, ConsArgs */
get_constraint_args(NewHead, A, ConsArgs) :-
 NewHead =.. [_|Args],
 split_list(A, Args, ConsArgs, _).


gen_code_for_constraint([], PreviousTryNexts, F, A, Nth, NRule, Nprule, CodeTail, Module) :- !,
  % need to generate termination rule
    (PreviousTryNexts \== none -> % have processed rules for this constraint
    make_rulefunctor(F, A, Nth, Functor),
    nmetaargs(Nprule, N),
    NewArity is A + N, % no indexing for this last clause.
    functor(NewHead, Functor, NewArity),
    get_constraint_args(NewHead, A, ConsArgs),
    Head =.. [F|ConsArgs], % set the constraint arguments
    AddBase is A + 1, % start of extra args
    arg(AddBase, NewHead, Index),
    %%% MAGIC NUMBER
    InPos is AddBase + 1,
    arg(InPos, NewHead, In),
    (Nprule =:= 0 -> ExtraArgs = [Index,In] 
          ; AppPos is AddBase + 2, arg(AppPos, NewHead, Applied),
            ExtraArgs = [Index,In,Applied]
    ),

    get_constraintnumber_fa(F,A, ConstNo, Prio, Module),
    construct_wrapper_constraint(Head, ExtraArgs, Constraint, _),
    construct_writeconstraint_code(Constraint, WriteDefine, WriteRule),
    CodeTail = [( NewHead :-
      (var(Index) -> 
          ech:suspend_constraint(Constraint, Head, ConstNo, Prio)
        ; get_suspension_data(Index, goal, Goal),
          setarg(wrapper_inpos, Goal, In)
      )
    ),WriteDefine, WriteRule| ExecConstraint],
    functor(Constraint, WH, WA),

    construct_exec_constraint(WH,WA,F,A, ConstNo, Prio, NRule, Nprule, 
         ExecConstraint, Module),
    construct_previous_try_next(PreviousTryNexts, A, NRule, NRule, NewHead)
  ; /* no code for this constraint at all */
    printf(warning_output, "Warning: No rule found for constraint %w/%w.\n",[F,A]),
    CodeTail = []).
gen_code_for_constraint([rule(Rule,Status,Head,KeepHeads,DeleteHeads,Body,
    PInfo, ComInfo, _Name)|Rest], 
    PreviousTryNexts, F, A, Nth, NRule, Nprule, Transformed1, Module) ?-
  Head =.. [F|ArgList],
  make_current_rule_name(F,A,Nth,NRule,Nprule,  CHead, _Arity),
  % CHead is clause head here (i.e. with meta args); Head is constrtraint
  construct_previous_try_next(PreviousTryNexts, A, NRule, 0, CHead),
  fill_current_head(CHead, A, Nth,NRule, ArgList, IndexHeadArgs,MetaArgs), 
  % fill in the arguments for the head
  (Nprule \== 0 -> 
     MetaArgs = [CIndex,In,_Applied] 
   ; MetaArgs = [CIndex,In]
  ),
  append(DeleteHeads, KeepHeads, Partners),
  %DeleteHeads first so that these can be deleted in body
  Clause = (CHead ?- Code0), % Code0 to be filled in
  rule_type(Rule, RuleType),
  construct_main_clause(RuleType, Status, Partners, DeleteHeads, KeepHeads, Head, CHead, Body, Nth, Nprule, PInfo, ComInfo, IndexHeadArgs, MetaArgs, Code0, Vars, Cans, InsChecks, TryNexts0, PreFiredCodes0, PostFiredCodes0, AllRest, Module),
  construct_2ndclause(CHead, Nth, NRule, A, SecondClause, TryNexts1),

  (Partners \== [] -> % not needed if no partners
     construct_prefired_otherpartners(RuleType, Status, Partners, DeleteHeads, KeepHeads, CHead, Head, Body, A, AllRest,
        Nth, NRule, Nprule, Cans, InsChecks, PInfo, ComInfo, PreFiredName, PreFiredCodes1, PostFiredCodes1, TryNexts2, PrePRule, Module),
     construct_postfired_otherpartners(RuleType, Status, Partners, DeleteHeads, KeepHeads, CHead, Head, Body, A, AllRest,
        Nth, NRule, Nprule, Cans, InsChecks, PInfo, ComInfo, PostFiredName, PostFiredCodes2, TryNexts3, PostPRule, Module),
     % fill in various bits of code
     functor(CHead, CFunc, _),
     construct_instancematches(Partners, Cans, Vars, InsChecks, CFunc, 1, Module),

     fill_in_try_otherrest(PreFiredCodes0, PreFiredName),
     fill_in_try_otherrest(PreFiredCodes1, PreFiredName),
     fill_in_try_otherrest(PostFiredCodes0, PostFiredName),
     fill_in_try_otherrest(PostFiredCodes1, PostFiredName),
     fill_in_try_otherrest(PostFiredCodes2, PostFiredName),
     append(TryNexts0,TryNexts1, TryNexts4),
     append(TryNexts4, TryNexts2, TryNexts5),
     append(TryNexts5, TryNexts3, TryNexts),
     PrePRule = [Pre1,Pre2], PostPRule = [Post1,Post2],
     Transformed1 = [Clause,SecondClause,Pre1,Pre2,Post1,Post2|Transformed2]

   ; /* should not have prefirecodes etc. if no partners */
     append(TryNexts0, TryNexts1, TryNexts),
     Transformed1 = [Clause,SecondClause|Transformed2]
  ),


  Nth1 is Nth + 1,
  gen_code_for_constraint(Rest, TryNexts, F, A, Nth1, NRule, Nprule, Transformed2, Module).


construct_instancematches([], [], [], [], _, _, _) :- !.
construct_instancematches([Partner|Ps], [Candidate|Cans], [Common-Remain|Cs], [ICheck|ICs], CFunc, N, Module) :-
   copy_term(Remain,CRemain),
   concat_atom([CFunc,match,N], NewFunc),
   ICheck =.. [NewFunc,Candidate,Common, Remain],
   GoalHead =.. [NewFunc,Partner,Common, CRemain],
   compile_term((GoalHead ?- CRemain = Remain))@Module,
   N1 is N + 1, 
   construct_instancematches(Ps, Cans, Cs, ICs, CFunc, N1, Module).


fill_in_try_otherrest([], _Name) :- !.
fill_in_try_otherrest([f(ArgList,TobeFilled)|Others], Name) :-
   (var(TobeFilled) -> 
        TobeFilledGoal =.. [Name|ArgList], 
	% add a cut before the call to prevent spurious backtracking
	TobeFilled = (!, TobeFilledGoal)
   ;    true),
   % TobeFilled is not var if no code needs to be filled in
   fill_in_try_otherrest(Others, Name).


rule_type((_H==>_B), Type) ?-  !, Type = propagation.
rule_type((H<=>_B), Type) ?-
  H = (_\_) ->
     Type = simpogation ; Type = simplification.


construct_main_clause(RuleType, Status, Partners, DeleteHeads, KeepHeads, Head, CHead, Body, Nth, Nprule, PropInfo, CompInfo, 
     IndexHeadArgs, MetaArgs, Code0, Vars, Candidates, 
     InstanceChecks, TryNexts, PreFiredCodes, PostFiredCode, AllRest, Module) :-

  get_constraintnumber(Head, ConstNo, Module),
  decompose_body(Body, Guard, BGoals),

  % construct the clause
  CommonArgs = a(Head,CHead,MetaArgs,Nth,AllRest,_PIndecies,_GConst,
     _Add_Applied,AppTail),
  ExtendArgs = a(Status,ConstNo,BGoals,DeleteHeads,KeepHeads,PostFired0,NTryNext),

  construct_find_partners(Partners, CommonArgs, 1, [], [], 
                          Vars, Candidates, InstanceChecks, FindPartners,
                          Module),
  MetaArgs = [_,In|_],
  (FindPartners \== true ->
       TryNexts0 = [f(Try_Next_Rule,[Nth-np|In],CHead)], % don't know how Cth
                                                     % is used, omitted
       Code0 = (FindPartners ->
                    Code1
               ;
                    !, Try_Next_Rule
               )
  ;
       TryNexts0 = [],
       Code0 = Code1
  ),
  construct_applied(RuleType, Partners, CommonArgs, PropInfo, CompInfo, Code1, 
     PreFired0, TryNext0, Code2),
  construct_guard(Guard, (Head,Partners), CommonArgs, CompInfo, ExtendArgs, Nprule, Code2, PreFired1, TryNext, Code3, Module),
  construct_body(CommonArgs, ExtendArgs, Nprule, Code3, PostFired, TryNext1, Module),

  % construct the various arg list
  multi_append([TryNext0,TryNext,TryNext1,TryNexts0,NTryNext], TryNexts),
  append(IndexHeadArgs, [AllRest], L0),
  append(L0, MetaArgs, Post1),
  append(L0, [applied|MetaArgs], Pre0), append(L0, [gf|MetaArgs], Pre1),
  (Nprule == 0 ->
      Post1 = PostFiredArgs,
      Pre1 = PreArgs1,
      Pre0 = PreArgs0 
    ; append(Post1, [AppTail], PostFiredArgs),
      append(Pre0, [AppTail], PreArgs0),
      append(Pre1, [AppTail], PreArgs1)
  ),
  PostFiredCode = [f(PostFiredArgs,PostFired),f(PostFiredArgs,PostFired0)],
  PreFiredCodes = [f(PreArgs0,PreFired0),f(PreArgs1,PreFired1)].


construct_2ndclause(OldHead, Nth, NRule, ConsArity, Code, TryNexts) :-
   functor(OldHead, F,A), 
   functor(NewHead, F,A), % construct new head
   InPos is ConsArity + NRule + 2, % second meta-arg is In
   arg(InPos, NewHead, In),
   (NRule > 0 ->
      arg(Nth, NewHead, I), % the indexing var. for this clause
      %% MAGIC NUMBER
      TryNexts = [f(TryNext1,[Nth-hf|In],NewHead),f(TryNext0,In,NewHead)],
      Code = (NewHead :-
         I == 1 -> TryNext0 
	      ; TryNext1
      )
    ; Code = (NewHead :- TryNext0),
      TryNexts = [f(TryNext0,[Nth-hf|In],NewHead)]
   ).


fill_previous_infoargs(PInfoPos, NRule, TryOtherHead, [FailType,SuspL,AppTail]) :-
   arg(PInfoPos, TryOtherHead, FailType),
   SuspPos is NRule + 2,
   arg(SuspPos, TryOtherHead, SuspL),
   AppTailPos is NRule + 3,
   arg(AppTailPos, TryOtherHead, AppTail).


construct_previous_try_next([], _, _, _, _) :- !.
construct_previous_try_next([f(TryNext, NewIn, PrevHead)|TryNexts], OArity, NRule, RemoveArg, NewHead) :-
% RemoveArg normally set to zero, so that indexing args are not removed, except for last clause for this constraint
   (TryNext \== true ->
     functor(NewHead, Name, _A), % NewHead used as template to construct calling goal
     add_inarg(PrevHead, OArity, NRule, NewIn, NewArgs0),
     remove_indexing(RemoveArg, NewArgs0, NewArgs),
     TryNext =.. [Name|NewArgs] % code for calling goal
    ;true
   ),
   construct_previous_try_next(TryNexts, OArity, NRule, RemoveArg, NewHead).

/* updates MetaArgs from OldIn to NewIn */
add_inarg(PrevHead, OArity, NRule, NewIn, NewArgs) :-
   divide_vars(PrevHead, OArity, NRule, OIndexHeadArgs, OMetaArgs),
   update_inarg(OMetaArgs,NewIn, NMetaArgs),
   append(OIndexHeadArgs, NMetaArgs, NewArgs).

/* updates In when given Metaargs */
update_inarg([Index,_OldIn|Rest], NewIn, [Index,NewIn|Rest]).


/* removes the first Nth arguments from rule clause, which are for indexing */
remove_indexing(0, Args0, Args1) :- !, Args0 = Args1. % nothing to remove
remove_indexing(1, [_|Args0], Args1) :- !, Args0 = Args1.
remove_indexing(N, [_|Args0], Args1) :- N > 1,
   N1 is N - 1,
   remove_indexing(N1, Args0, Args1).
 

/* connect_orig_args(+OrigHead, +NRule, +IndexHeadArgs)
   connect the original args. (i.e. "real" arguments from the constraint 
   OrigHead) with their corresponding args. in the current clause being
   constructed -- these are in the IndexHeadArgs, which includes the 
   indexing args at the start
*/
connect_orig_args(OHead, NRule, IndexHeadArgs) :-
   OHead =.. [_|RealArgs],
   remove_indexing(NRule, IndexHeadArgs, RealArgs).



construct_prefired_otherpartners(RuleType, Status, Partners, DeleteHeads,KeepHeads, 
  RuleF, Head, Body, OrigArity, AllRests, Nth, NRule, Nprule, Candidates, CheckInsts, PInfo, ComInfo, PreFiredName, 
  PreFiredCodes, PostFiredCode, TryNexts, Code0, Module) :- 
   decompose_body(Body, Guard, BGoals),
   get_constraintnumber(Head, ConstNo, Module),
   make_try_other_name(RuleF, prefired, PreFiredName),
   nmetaargs(Nprule, BasicSize), % number of "basic" meta-args
   (Nprule == 0 -> MetaSize is BasicSize  + 2; MetaSize is BasicSize + 3),
   % 4 fixed meta-args (AllRest,Reason,Index,In) + 2 w/propagation (Applied, AppTail)
   Arity is NRule + OrigArity + MetaSize,
   functor(CHead, PreFiredName, Arity),
   divide_vars(CHead, OrigArity, NRule, IndexHeadArgs, PreMetaArgs),
   connect_orig_args(Head, NRule, IndexHeadArgs),
   PreMetaArgs = [AllRests,_Reason|MetaArgs],
   get_appliedtail(Nprule, MetaArgs, AppTail),
   Code0 = [(CHead ?- Code1), (CHead :- NextRule)],
   MetaArgs = [_Index,In|_],
true,
   construct_basic_head(CHead, Head, NRule, Nprule, MetaArgs, ConsHead),
   TryNext0 = [f(NextRule, In, ConsHead)],
   CommonArgs = a(Head,CHead,MetaArgs,Nth,AllRests1,_PIndecies,_GConst,
     _Add_Applied, NAppTail),
   % we no longer create TryNexts branches in fguards, so no need for TryNext
   ExtendArgs = a(Status,ConstNo,BGoals,DeleteHeads,KeepHeads,PostFired0,[]),

   construct_other_findpartners(AllRests, Partners, CommonArgs, NRule, 
      1, [], Nprule, Candidates, CheckInsts, Code1, Code2),
   construct_fapplied(RuleType, AppTail, CommonArgs, PInfo, ComInfo, Code2, 
      PreFired0, Code3),
   construct_fguard(Guard, (Head,Partners), CommonArgs, ExtendArgs, ComInfo, Nprule, Code3, Code4, Module),
   construct_body(CommonArgs, ExtendArgs, Nprule, Code4, PostFired, TryNext1, Module),

   % same args as head, except for AllRest, Reason and AppTail
   % no need to append TryNext0, should not have pre/post fired if no 
   % partner
   new_metaargs(Nprule, MetaArgs, NAppTail, NewMetaArgs),
   append(TryNext0, TryNext1, TryNexts),
   append(IndexHeadArgs, [AllRests1,applied|NewMetaArgs], PreArgs0),
   append(IndexHeadArgs, [AllRests1|NewMetaArgs], PostArgs),
   PreFiredCodes = [f(PreArgs0,PreFired0)],
   PostFiredCode =  [f(PostArgs,PostFired),f(PostArgs,PostFired0)].



construct_postfired_otherpartners(RuleType, Status, Partners, DeleteHeads, KeepHeads, RuleF, Head, Body, OrigArity, AllRests, Nth, NRule, Nprule, Candidates, CheckInsts, PInfo, ComInfo, PostFiredName, PostFiredCodes, TryNexts, Code0, Module) :-
   decompose_body(Body, Guard, BGoals),
   get_constraintnumber(Head, ConstNo, Module),
   make_try_other_name(RuleF, postfired, PostFiredName),
   nmetaargs(Nprule, BasicSize),
   (Nprule ==0 -> MetaSize is BasicSize +1 ; MetaSize is BasicSize + 2),
   % should try to remove these magic numbers
   Arity is NRule + OrigArity + MetaSize,
   functor(CHead, PostFiredName, Arity),
   divide_vars(CHead, OrigArity, NRule, IndexHeadArgs, PostMetaArgs),
   connect_orig_args(Head, NRule, IndexHeadArgs),
   PostMetaArgs = [AllRests|MetaArgs],
   get_appliedtail(Nprule, MetaArgs, AppTail),
   Code0 = [( CHead :- Code1), (CHead :- NextRule)],
   MetaArgs = [_Index,In|_],
true,
   construct_basic_head(CHead, Head, NRule, Nprule, MetaArgs, ConsHead),
   TryNext0 = [f(NextRule, In, ConsHead)],
   CommonArgs = a(Head,CHead,MetaArgs,Nth,AllRests1,_PIndecies,_GConst,
     _Add_Applied,AppTail1),
   ExtendArgs = a(Status,ConstNo,BGoals,DeleteHeads,KeepHeads,PostFired3,[]),


   construct_other_findpartners(AllRests, Partners, CommonArgs, NRule,  
     1, [], Nprule, Candidates, CheckInsts, Code1, Code2),
   construct_fapplied(RuleType, AppTail, CommonArgs, PInfo, ComInfo, Code2, PostFired0, Code3),
   construct_fguard(Guard, (Head,Partners), CommonArgs, ExtendArgs, ComInfo, Nprule, Code3, Code4, Module),
   construct_body(CommonArgs, ExtendArgs, Nprule, Code4, PostFired2, TryNext1, Module),
   % no need to delete post-fired

   append(TryNext0,TryNext1, TryNexts),
   
   (Status == keep -> true; fill_in_with_true(TryNexts)),
   new_metaargs(Nprule, MetaArgs, AppTail1, NewMetaArgs),
   append(IndexHeadArgs, [AllRests1|NewMetaArgs], PostArgs),
   PostFiredCodes = [f(PostArgs,PostFired0),f(PostArgs,PostFired2),f(PostArgs,PostFired3)].


/* new_metaargs(+Nprule, +OldArg, -NewAppTail, NewArgs)
   creates NewArgs, the new meta-args, for the recursive calls to pre- and post-
   fired clauses, from within a pre- and post- fire clause. OldArgs are the
   original meta-args upon entry into the pre-/post-fired clause that is 
   being created, and the only value that needs to be changed is the AppTail
   argument, if it exist
*/
new_metaargs(Nprule, OldArgs, NewAppTail, NewArgs):-
    (Nprule =:= 0 -> NewArgs = OldArgs ; 
         OldArgs = [Index,In,Applied,_],
         NewArgs = [Index,In,Applied,NewAppTail]
    ).


fill_in_with_true([]) :- !.
fill_in_with_true([f(ToBeTrue, _, _)|TryNexts]) :- 
   ToBeTrue = true,
   fill_in_with_true(TryNexts).
      


/* fill_current_head(+CurrentHead, +OriginalArity, +Nth, +NRule, +ArgList,
      -IndexHeadArgs, -MetaArgs)
   CurrentHead is the head of the translated Prolog goal for the current
   rule being compiled. OriginalArity is the
   original arity of the head constraints that triggered the trying of the
   current rule, Nth indicates that the current rule is the Nth rule for this 
   constraint, and is used to construct the indexing, NRule is the number of 
   rules for this constraint, which determines how many arguments will be 
   needed for indexing. ArgList is a list of the original arguments in the 
   constraint. IndexHeadArgs will be returned with the indexing and head 
   (original arguments of the constraint) arguments of CurrentHead, and
   MetaArgs are the "meta" arguments of CurrentHead, i.e. those needed to
   allow for the suspension and rewakening of the constraint.
*/
fill_current_head(CHead, A, Nth, NRule, ArgList, IndexHeadArgs, MetaArgs) ?-
/* current scheme for head args are 
   (Indexing..., Original Args..., ConstraintIndex, FailureInfo, Applied) */
   (NRule \== 0 -> arg(Nth, CHead, 1) ; true),% indexing
   fill_original_args(0, A, NRule, ArgList, CHead),
   divide_vars(CHead, A, NRule, IndexHeadArgs, MetaArgs).


/* divide_vars(+CurrentHead, +OrigArity, +NRule, -IndexHeadArgs, -MetaArgs)
   splits the arguments of CurrentHead into two lists: IndexHeadArgs, which
   are the Indexing and head args, and MetaArgs, which are the meta args
*/
divide_vars(CHead, OArity, NRule, IndexHeadArgs, MetaArgs) :-
   CHead =.. [_|AllArgs],
   IndexHeadSize is OArity + NRule,
   split_list(IndexHeadSize, AllArgs, IndexHeadArgs, MetaArgs).

fill_original_args(N, N, _, [], _) :- !.
fill_original_args(N0, Max, NRule, [Arg|ArgList], CHead) :-
   N1 is N0 + 1,
   Pos is  NRule + N1,
   arg(Pos, CHead, Arg), % put Arg in
   fill_original_args(N1, Max, NRule, ArgList, CHead).


/* get_appliedtail(+Nprule, +MetaArgs, -AppTail)
   returns the AppTail meta-argument from MetaArgs if it exists (if there are 
   no propagation rules, i.e. Nprule = 0). This is for the pre and post fired
   clauses, where the two propagation args would be at the end if they exist
   The clause simply does not do anything if AppTail does not exist -- this 
   means that following clauses should not make use of AppTail
*/
get_appliedtail(Nprule, [_,_|Pos], AppTail) :-
   (Nprule =\= 0 -> Pos = [_Applied,AppTail] ; true). 

/* get_constraintnumber(+Constraint, -ListNumber, +Module)
   returns the ListNumber for constraint, i.e. which constraint list a 
   particular constraint (in the form of a structure) is stored in
*/
get_constraintnumber(Cons, ConstNo, Module) :-
   functor(Cons,F,A),
   get_constraintnumber_fa(F,A, ConstNo, _, Module).

get_constraintnumber_fa(F,A, ConstNo, Prio, Module) :-
   recorded_list('CHRconstraints', Constraints)@Module,
   chr_constraint_info(Constraints, F/A, ConstNo, Prio).


construct_check_identical(Partner, Head, PartnerIndex, CIndex, CheckIdentical) :-
   /*get_constraintnumber(Partner, PartnerIndex),*/
   (\+ \+(Partner = Head) -> 
   % if unifiable, then need to generate code to check Partner is not current
       CheckIdentical = (PartnerIndex \== CIndex) ; CheckIdentical = true
   ).

% construct code that checks if the currently found partner has already
% been previously matched in this rule
construct_not_already_matched([], _, CheckCode) :- !, 
        CheckCode = true.
construct_not_already_matched([MatchedIndex], PartnerIndex, CheckCode) :- !,
        CheckCode = (PartnerIndex \== MatchedIndex).
construct_not_already_matched(MatchedIdxs, PartnerIndex, CheckCode) :-
% more than one matched head already...
        CheckCode = (\+memberchk(PartnerIndex, MatchedIdxs)).

construct_find_partners([],a(_,_,_,_,AllRest,PIndex,_,_,_),_,_,_,
           Com,Cans,InsChk,Code,_M) :- !, 
   Code = true, AllRest = [], PIndex = [], Com = [], 
   InsChk = [], Cans = [].
construct_find_partners([Partner|Partners],a(Head,CHead,MetaArgs,Nth,AllRest,
   [PartnerIndex|PIndecies],_,_,_), Cth, MatchedPIdxs, MatchedPartners,
   [Common-Remain|Cs], [Candidate|Cans], [InstanceCheck|ICs], Code, Module) :-
% Nth rule for this constraint, Cth partner, In for failure info
% Head is the raw constraint head (i.e. without meta args)
% MatchedPIdxs are indecies of the suspended goals for already matched 
% partners
   MetaArgs = [CIndex,_In|_],
   shared_vars(Partner,  [Head|MatchedPartners], Common, Remain),
   % non-shared variables in Candidate and Partner are the same 
   get_constraintnumber(Partner,ConstNo, Module),
   (Common \== []  ->
        GetSList = ech:get_constraint_list(Common, ConstNo, SuspL)
   ; 
        GetSList = ech:get_global_constraint(ConstNo, SuspL)
   ),
   construct_check_identical(Partner, Head, PartnerIndex, CIndex, CheckIdentical),
   construct_not_already_matched(MatchedPIdxs, PartnerIndex, CheckNotAlreadyMatched),
   Code = 
      (GetSList,
       ech:find_partner(SuspL, PartnerIndex, Candidate, Rest),
       InstanceCheck,
       CheckIdentical,
       CheckNotAlreadyMatched,
       RestCode0
      ),
   % If this is not the first partner, use the original suspension list
   % instead of Rest (the remaining list after finding he current partner)
   % This is because there might be dependencies from the earlier partners 
   % that means subsequent refiring of the (simpogation/propagation) rule
   % needs to check partners that did not match with the current.
   % A possible optimisation is to determine the dependencies to see if
   % Rest can be used.
   (Cth > 1 ->
       AllRest = [SuspL|AllRest0]
   ;
       AllRest = [Rest|AllRest0]
   ),
   Cth1 is Cth + 1,
   construct_find_partners(Partners,a(Head,CHead,MetaArgs,Nth,AllRest0,
      PIndecies,_,_,_), Cth1, [PartnerIndex|MatchedPIdxs],
      [Partner|MatchedPartners], Cs, Cans, ICs, RestCode0, Module).


/* construct_basic_head(+CurrentClauseHead, +OrigConstraintHead, +NRule,
       +Nprule, +BasicMetaArgs, -ConstructedHead)
   constructs a "basic" head with just the indexing, real arguments, and
   the basic meta-args. CurrentClauseHead is the head of the clause that
   is being constructed. This can contain additional meta-args to the basic
   one. These need to be removed. OrigConstraintHead is the constraint,
   without any indexing or meta-args. NRule is the number of rules for this
   constraint (i.e. number of indexing args.). BasicMetaArgs is the basic
   meta-args for the CurrentClauseHead
*/
construct_basic_head(CHead, Head, NRule, Nprule, MetaArgs, NewHead) :- 
   functor(Head,F,A),
   divide_vars(CHead, A, NRule, IndexHeadArgs, _),
   nmetaargs(Nprule, N), length(MetaArgs1, N),
   append(MetaArgs1, _, MetaArgs), 
   % get rid of any extra non-basic arguments at end of MetaArgs
   append(IndexHeadArgs, MetaArgs1, Args),
   NewHead =.. [F|Args].


construct_other_findpartners([], [], a(_,_,_,_,AllRest,PIndex,_,_,_), _, _, _,
      _, _, _, Code0, Code) :- !,
   Code0 = Code, AllRest = [], PIndex = [].
construct_other_findpartners([Rest|Rests], [Partner|Partners], a(Head,CHead, 
    MetaArgs,Nth,AllNRest,[PartnerIndex|PIndecies],_,_,_), NRule, Cth,
    MatchedPIdxs, Nprule, [Candidate|Cans], [CheckIns|CIs], Code, RestCode) :-
   MetaArgs = [CIndex, _In|_],

   % construct ConsHead which contains only the basic meta-args
   %construct_basic_head(CHead, Head, NRule, Nprule, MetaArgs, ConsHead),

   construct_check_identical(Partner, Head, PartnerIndex, CIndex, CheckIdentical),
   construct_not_already_matched(MatchedPIdxs, PartnerIndex, CheckNotAlreadyMatched),
   Code = ( 
      ech:find_partner(Rest, PartnerIndex, Candidate, NewRest),
      CheckIns,
      CheckIdentical,
      CheckNotAlreadyMatched,
      
      RestCode0
   ),
   % use full original list if Cth > 1
   (Cth > 1 ->
       AllNRest = [Rest|NewRests]
   ;
       AllNRest = [NewRest|NewRests]
   ),
   Cth1 is Cth + 1,
   construct_other_findpartners(Rests, Partners, a(Head,CHead,MetaArgs,Nth,NewRests,PIndecies,_,_,_), NRule,  
     Cth1, [PartnerIndex|MatchedPIdxs], Nprule, Cans, CIs, RestCode0, RestCode). 


/* extract_applied(+MetaArgs, -Applied) extracts the argument for storing
   Applied information from the meta arguments of a clause
*/
extract_applied([_,_,Applied|_], Applied).

/* construct_applied_check 
 
 this constructs the appropriate direct applied (i.e. applied list has
 already been extracted) check for propagation rules. This occurs in the
 pre-fired and post-fired clauses. Currently it only deals with two headed
 propagation rules (no check needed for single headed propagation
 rules). In such cases, there are two factors: if the head constraints have
 the same functor/arity, and if the one initiating the rule has already
 been added to the constraint store or not. These generate four different
 situations 
*/

construct_applied_check(1, double(H1,_H2), Head, RuleNo, PList, [PIndex], Index, Add_Applied, Not_AppliedCheck, AppTail, GConsNo, Status) :- !,
	get_currentheadpos(H1, Head, Pos, OtherPos),
	Not_AppliedCheck = (
	    get_suspension_data(PIndex, goal, PGoal),
            arg(constraintnum_pos, PGoal, PConsNumber), % get the ConsNo for partner
            PList = [_|PListRest],
	    (var(Index) -> /* if var, then initiating head is not in constraint store */
	        /* can only check in that case */
	        ech:check_samepairapplied_direct(PList, PListRest, Pos-PConsNumber, AppTail, AppRest, Status)

	      ; /* can check and mark if initiating head already in constraint store */
	        get_suspension_data(Index, goal, Cons0),
                arg(constraintnum_pos, Cons0, GConsNo),
              
	        ech:check_samepairapplied_directmarked(PList, PListRest, Pos-PConsNumber, AppTail, OtherPos-GConsNo, PGoal, RuleNo, Status)
            )

        ),
	Add_Applied = samepairapplied(Pos,PConsNumber,AppTail,AppRest,OtherPos,PGoal,RuleNo).
construct_applied_check(1, _, _, RuleNo, PList, [PIndex], Index, Add_Applied, 
    Code, AppTail, GConsNo, Status) :- !,
	Code = (get_suspension_data(PIndex, goal, PGoal),
          arg(constraintnum_pos, PGoal, PConsNumber),
          PList = [_|PListRest],
	  (var(Index) ->
                ech:check_pairapplied_direct(PList, PListRest, PConsNumber, AppTail, AppRest, Status)

	       ;get_suspension_data(Index, goal, Cons0),
                arg(constraintnum_pos, Cons0, GConsNo),

	        ech:check_pairapplied_directmarked(PList, PListRest, PConsNumber, AppTail, GConsNo, PGoal, RuleNo, Status)
          )
        ),
        Add_Applied = pairapplied(PConsNumber,AppTail,AppRest,PGoal,RuleNo).
construct_applied_check(N, _, _, _RuleNo, _PList, _, _Index, Add_Applied, 
    Code, _AppTail, _, _Status) :- !,
   N > 1,
   writeln(error, "Propagation rule with more than two head constraints not yet supported. No applied testing done.\n"), flush(error),
   Add_Applied = true,
   Code = true.





appropriate_applied(0, _, _, RuleNo, [], Applied, Code, Add_Applied, 
  AppTail, _ConsNo, Status) :- !,
    Code = ( nonvar(Applied) ->
                arg(RuleNo, Applied, App),
	        (App == * -> Status = found ; Status = notfound)
	     ; % var(Applied)
               number_of_propagations(Size), 
               ech:create_applied(Applied, Size),
               Status = notfound
           ), AppTail = [],
    Add_Applied = arg(RuleNo, Applied, *).   % * mark as read
appropriate_applied(1, double(H1, _H2), Head, RuleNo, [PIndex], Applied, Code, 
  Add_Applied, AppTail, ConsNo, Status) :- !,
    % propagation rule, with two heads that has the same functor and arity
    % needs special check and insert applies
    get_currentheadpos(H1, Head, Pos, OtherPos),
    Code = ( get_suspension_data(PIndex, goal, PGoal),
              arg(constraintnum_pos, PGoal, PConsNumber),
              ech:check_samepairapplied(RuleNo, Applied, Pos-PConsNumber, AppTail, AppRest, Status)
     ),
     Add_Applied = ech:insert_samepairapplied(Pos-PConsNumber, AppTail, AppRest, OtherPos-ConsNo, PGoal, RuleNo).
appropriate_applied(1, _, _, RuleNo, [PIndex], Applied, Code, Add_Applied, AppTail, ConsNo,
   Status) :- !,
     Code = ( get_suspension_data(PIndex, goal, PGoal),
              arg(constraintnum_pos, PGoal, PConsNumber),
              ech:check_pairapplied(RuleNo, Applied, PConsNumber, AppTail, AppRest, Status)
     ),
     Add_Applied = ech:insert_pairapplied(PConsNumber, AppTail, AppRest, ConsNo, PGoal, RuleNo).
appropriate_applied(N, _, _, _RuleNo, _, _, Code, Add_Applied, _, _, _) :- 
   N > 1,
   writeln(error, "Propagation rule with more than two head constraints not yet supported. Not applied testing done.\n"), flush(error),
   Code = true,
   Add_Applied = true.



/* get_currentheadpos(+Head1, +Partner, -Pos, -OtherPos) 
   returns in Pos the position (1 or 2) of the current head that is being
   transformed. OtherPos is the other position. Head1 is the first (leftmost)
   head as occur in the rule, and Head is the current active head
*/
get_currentheadpos(Head1, Head, Pos, OPos) :-
   Head1 \== Head -> Pos = 2, OPos = 1; Pos = 1, OPos = 2.




construct_applied(Rule_Type, Partners, a(Head,CHead,MetaArgs,Nth,_,PIndecies,
   GConsNo,Add_Applied,AppTail), RuleNo, ComInfo, Code, PreFiredAlt, TryNext, 
   RestCode) :-
   Rule_Type == propagation ->
     length(PIndecies, NPartners),
     extract_applied(MetaArgs, Applied),
     appropriate_applied(NPartners, ComInfo, Head, RuleNo, PIndecies, Applied, Not_Applied, Add_Applied, AppTail, GConsNo, Status),

     Code = (
        Not_Applied,
        (Status == notfound -> 
            RestCode
	  ; !, AltAction
        )
     ),
     (Partners == [] ->
         MetaArgs = [_Index,In|_], % get In
         TryNext = [f(AltAction, [Nth-notapp|In], CHead)],
         PreFiredAlt = true
       ; TryNext = [], PreFiredAlt = AltAction
     )
   ; Add_Applied = true, PreFiredAlt = true, TryNext = [],
     RestCode = Code.
 
construct_fapplied(Rule_Type, Partners, a(Head,_,MetaArgs,_,_,PIndecies,GConsNo,Add_Applied,AppTail), RuleNo, ComInfo, Code, PreFiredAlt, RestCode) :-
   Rule_Type == propagation ->
     MetaArgs = [Index|_],
     length(PIndecies, NPartners),
     construct_applied_check(NPartners, ComInfo, Head, RuleNo, Partners, PIndecies, Index, Add_Applied, Not_AppliedCheck, AppTail, GConsNo, Status),

     Code = (
        Not_AppliedCheck,
        (Status == notfound ->
            RestCode
           ; !, PreFiredAlt
        )
     )
   ; 
     Add_Applied = true,
     RestCode = Code.

/* metaguard_code(+Guard, -Code)
   construct the code for "meta" Guards -- i.e. none of the guards can 
   constrain the variables in them
*/
metaguard_code(Guard, Code) :-
    Code = (last_suspension(Mark),
            Guard, ttrue, % need to mark differently so not optimised away
            new_suspensions(Mark,[]) % no goals in guard has been delayed
           ).

/* guard_code(+Guard, +Global, -Code)
   construct the code for Guards, where they contain goals which are capable
   of constraining (global)variables. Code taken from old chr.pl:

% Before the guard (Goal) is called, a 'fail' is attached to every variable
% of the Goal. Then, as soon as one of these variables is touched
% (unified), the call will fail.

*/
guard_code(Guard, Globals, Code) :-
    Code = (
      make_suspension(fail, 1, Susp),
      insert_suspension(Globals, Susp, constrained of suspend, suspend),
      last_suspension(Mark),
      Guard, ttrue,
      new_suspensions(Mark, []),
      kill_suspension(Susp)
    ).

construct_invertable_rule(Globals, CommonArgs, InvBody, ExtendArgs, Nprule, Code, Try_Other_Partners, TryNext, Module) :- 
    decompose_body(InvBody, InvG, InvB),
    construct_guard(InvG, Globals, CommonArgs, nil, ExtendArgs, Nprule, Code, Try_Other_Partners, TryNext, RestCode, Module),
    ExtendArgs = a(delete,ConstNo,_,DeleteHead,KeepHead,PostFired,TryNext1),
    % Delete and Keep Heads swapped
    construct_body(CommonArgs, a(keep,ConstNo,InvB,KeepHead,DeleteHead,
        PostFired,TryNext1), Nprule, RestCode, PostFired, TryNext1, Module).

construct_guard(Guard, Globals, CommonArgs, ComInfo, ExtendArgs, Nprule, Code, Try_Other_Partners, TryNext, Rest_Code, Module) :-

   CommonArgs = a(_,CHead,MetaArgs,Nth,AllRest,_,_,_,_),  
   (Guard \== true ->
       ((recorded('CHRdont_guard_bindings', Module) ; nonconstrainable(Guard)) ->
       % check if guard can possibly cause the constraining of any variable
           metaguard_code(Guard, Execute_Guard)
         ; guard_code(Guard, Globals, Execute_Guard)
       ),

       (AllRest == [] -> /* no need to try other partners */
           Code = ( 
              Execute_Guard -> !,
                  Rest_Code
	        ; !, AltCode

           ),
           MetaArgs = [_Index,In|_],
           TryNext = [f(Try_Next_Rule, [Nth-gf|In], CHead)],
           Try_Other_Partners = true

         ; %AllRest \== []
           Code = 
             (Execute_Guard -> !,
                 Rest_Code
               ; !, AltCode
           ), 
           TryNext = []
       ),

       (ComInfo = twobodies(InvBody) -> 
           construct_invertable_rule(Globals, CommonArgs, InvBody, ExtendArgs, Nprule, AltCode, Try_Other_Partners, TryNext, Module)
 
         ; (AllRest == [] -> AltCode = Try_Next_Rule ; AltCode = Try_Other_Partners),
           ExtendArgs = a(_,_,_,_,_,true,[])
       )
       
     ; /* Guard == true */
       ExtendArgs = a(_,_,_,_,_,true,[]), % bind the output args
       Code = (!, Rest_Code), Try_Other_Partners = true,
       TryNext = []
   ).


construct_fguard(Guard, Globals, CommonArgs, ExtendArgs, ComInfo, Nprule, Code, Rest_Code, Module) :-
   (Guard \== true ->
       ((recorded('CHRdont_guard_bindings', Module) ; nonconstrainable(Guard)) ->
       % check if guard can possibly cause the constraining of any variable
           metaguard_code(Guard, Execute_Guard)
         ; guard_code(Guard, Globals, Execute_Guard)
       ),
       (ComInfo = twobodies(Inv) ->
          Code = (Execute_Guard ->
                   !, Rest_Code ; AltCode
                 ),
          construct_finvertable_rule(Globals, Inv, CommonArgs, ExtendArgs, Nprule, AltCode, Module)

        ; Code = ( 
             Execute_Guard, !,
             Rest_Code
          )


       )
     ; /* Guard == true, no difference in twobodies case, as head will be
          removed */
       Code = (!, Rest_Code)
   ).



construct_finvertable_rule(Globals, Inv, CommonArgs, ExtendArgs, Nprule, Code, Module) :-
     decompose_body(Inv, InvG, InvB),
     construct_fguard(InvG, Globals, CommonArgs, ExtendArgs, nil, Nprule, Code, RestCode, Module),
     ExtendArgs = a(delete,ConstNo,_,DeleteHead,KeepHead,PostFired,TryNext1),
     % Delete and Keep Heads swapped
     construct_body(CommonArgs, a(keep,ConstNo,InvB,KeepHead,DeleteHead,
        PostFired,TryNext1), Nprule, RestCode, PostFired, TryNext1, Module).



construct_deletion([], _, _, true) :- !.
construct_deletion([DHead|DHs], [Index|Indecies], Constraints, Code) :-
   functor(DHead,F,A),
   chr_constraint_info(Constraints, F/A, ConstNo, _), 
   Code = (ech:kill_constraint(ConstNo, Index), Code1), 
   construct_deletion(DHs, Indecies, Constraints, Code1).


construct_body(a(Head,CHead,MetaArgs,_,AllRest,PIndecies,GConst,Add_Applied,_),
  ExtendArgs,  Nprule, Code, PostFired, TryNext, Module) :-

   ExtendArgs = a(HeadAction,ConstNo,BodyGoals,DeleteHeads,_,_,_),
   recorded_list('CHRconstraints', Constraints)@Module,
   construct_deletion(DeleteHeads, PIndecies, Constraints, DeleteConstraints),
   MetaArgs = [Index,In|_], 
   nmetaargs(Nprule, BasicSize),
   length(BasicMetaArgs, BasicSize),  % only basic MetaArgs are used in suspended constraint
   append(BasicMetaArgs, _, MetaArgs),
   (HeadAction == keep ->
       construct_wrapper_constraint(Head, BasicMetaArgs, Constraint, GConst),
       functor(Head,F,A),
       get_constraintnumber_fa(F,A, _, Prio, Module),
       generate_constraint_suspend(Add_Applied, Index, Constraint, Head,
                                   ConstNo, Prio, GConst, SCode),
       Code = (
          SCode,
          DeleteConstraints,
          BodyGoals, 
          May_Continue
       ),
       (AllRest == [] ->
           PostFired = true,
           TryNext = [f(Continue1,In,CHead)],
	   add_may_continue(BodyGoals, Index, May_Continue, Continue1)
       ;   
	   TryNext = [],
           add_may_continue(BodyGoals, Index, May_Continue, PostFired)
       )

     ; /* HeadAction \= keep */
       Code = (
         ech:kill_constraint(ConstNo, Index),
         DeleteConstraints,
         BodyGoals
       ), PostFired = true, TryNext = []
   ).


/* add check for if Index is a live suspension or not if the body goals
   could possibly cause the constraint to be killed
*/
add_may_continue(BodyGoals, Index, May_Continue, ContinueCode) :-
	(nonconstrainable(BodyGoals) -> 
         % if body goals could not possibly cause constraint to be killed,
         % then no need to test if suspension is valid or not.
	    May_Continue = ContinueCode
         ;  May_Continue =
                (is_suspension(Index) ->
	            ContinueCode ; true
                )
	 ).

/* generate the code for dealing with the possible addition of a constraint
   to the constraint store after a rule is fired. If the rule is a two
   headed propagation rule, then special code may need to be generated to
   deal with checking if the heads have been applied already. There is also
   different code in this case for the initial call and the pre- and post-
   fired calls
*/
generate_constraint_suspend(samepairapplied(Pos,PConsNumber,AppTail,AppRest,
    OtherPos,PGoal,RuleNo), _Index, Constraint, Head, ConstNo, Prio, GConst, Code) ?- !,
% double headed propagation rule, with same (same functor) heads. Here 
% Add_Applied (first arg.) is used to pass extra args for constructing code
	Code = 
          (var(GConst) -> % check GConst instead of Index. Previous check of var(Index) means GConst would be instatiated if Index non-var
	      ech:suspend_constraint(Constraint, Head, ConstNo, Prio),
	      ech:insert_samepairapplied(Pos-PConsNumber, AppTail, AppRest, OtherPos-GConst, PGoal, RuleNo)

            ; true
          ).
generate_constraint_suspend(pairapplied(PConsNumber,AppTail,AppRest,PGoal,RuleNo),
    _Index, Constraint, Head, ConstNo, Prio, GConst, Code) ?- !,
% double headed propagation rule, with different heads
	Code = 
          (var(GConst) ->
	      ech:suspend_constraint(Constraint, Head, ConstNo, Prio),
	      ech:insert_pairapplied(PConsNumber, AppTail, AppRest, GConst, PGoal, RuleNo)

            ; true
          ).
generate_constraint_suspend(Add_Applied, Index, Constraint, Head, ConstNo,
                            Prio, GConst, Code) :- 
% generic case
       Code = ( 
          (var(Index) ->
	      ech:suspend_constraint(Constraint, Head, ConstNo, Prio)
            ; get_suspension_data(Index, goal, Cons0),
              arg(constraintnum_pos, Cons0, GConst)
              %Cons0 is Constraint, but use new var as no need to deconstruct
          ),
          Add_Applied
      ).


/* make_current_rule_name(+CFunctor, +CArity, +Nth, +NRule, +Nprule, -NewRuleHead, -NewArity)
   creates a new clause head (NewRuleHead) for a translated rule called with a 
   constraint CFunctor/CArity. This clause is the Nth clause for this constraint.
   There are a total of NRule for this constraint. 
   Nprule is the number of propagation rule in the program, if this is zero, the
   clause head is simplier as it omits the Applied argument
*/
make_current_rule_name(CF, CA, Nth, NRule, Nprule, RuleHead, Arity) :-
    (Nprule =:= 0 -> Extra = 0 ; Extra = 1),
    Arity is CA + Extra + NRule + 2, % NRule args for indexing; 2 extra meta arguments
    make_rulefunctor(CF, CA, Nth, Functor),
    functor(RuleHead, Functor, Arity).

make_rulefunctor(CF, CA, Nth, Functor) :- % creates a (hopefully) unique new name
    concat_atom(['CHR',CF,CA,'_',Nth], Functor).

/* make_try_other_name(+MainRuleHead, +Position, -Name)
   creates the name for the auxillary predicates for the rule with MainRuleHead
   as head of the rule. Position is either prefired or postfired, depending
   on if the goal is to be called before or after the firing of the rule
*/
make_try_other_name(MainRuleHead, Position, Name) :-
   functor(MainRuleHead, F, _),
   concat_atom([F,Position], Name).


/* construct_wrapper_constraint(+CHead, +ExtraArgs, -Constraint, -GlobalNumber)
  constructs the actual suspension (Constraint) that is used to represent 
  the suspended constraint CHead. ExtraArgs is a list of the meta arguments
  carried by the actual suspension. GlobalNumber is the global constraint
  number used to index CHR constraints for this Constraint
*/
construct_wrapper_constraint(CHead, ExtraArgs, Constraint, GConsNo) :-
   functor(CHead, CF, CA),
   concat_atom(['CHRsusp',CF,CA], WFunctor), 
   Constraint =.. [WFunctor, GConsNo, CHead|ExtraArgs].
   
nmetaargs(Nprule, N) :-
% Currently MetaArgs are Index,In,Applied
    (Nprule =:= 0 -> N = 2 ; N = 3).


optimise([], []) :- !.
optimise([Clause|Code], [OClause|OCode]) :-
   optimise_clause(Clause, OClause),
   optimise(Code, OCode).

optimise_clause((:- Goal), (:- Goal)) :- !.
optimise_clause((Head :- Body), (Head :- OBody)) :- !,
   optimise_body(Body, OBody0), optimise_further(OBody0, OBody).
optimise_clause((Head ?- Body), (Head ?- OBody)) :- !,
   optimise_body(Body, OBody0), optimise_further(OBody0, OBody).
optimise_clause(Fact, Fact).


% convert ttrue to true and remove singleton trues that may be left.
optimise_further(ttrue, Out) ?- !, Out = true.
optimise_further((ttrue, Goals), Out) ?- !, Out = (true,OGoals),
   optimise_further(Goals, OGoals).
optimise_further((true, Goals), OGoals) ?- !,
   optimise_further(Goals, OGoals).
optimise_further((Goal, true), OGoal) ?- !,
   optimise_further(Goal, OGoal).
optimise_further((Goal,Goals), Out) ?- !, Out = (OGoal,OGoals),
   optimise_further(Goal, OGoal), optimise_further(Goals, OGoals).
optimise_further((Goal1;Goal2), Out) ?- !, Out = (OGoal1;OGoal2),
   optimise_further(Goal1, OGoal1), optimise_further(Goal2, OGoal2).
optimise_further((If -> Then), Out) ?- !, Out = (OIf -> OThen),
   optimise_further(If, OIf), optimise_further(Then, OThen).
optimise_further(Goal, Goal).


optimise_body((true, Goals), OGoals) ?- !,
   optimise_body(Goals, OGoals).
optimise_body((Goal,true), OGoal) ?- !,
   optimise_body(Goal, OGoal).
optimise_body((Goal,Goals), Out) ?- !, Out = (OGoal,OGoals),
   optimise_body(Goal, OGoal),
   optimise_body(Goals, OGoals).
optimise_body((Goals1;Goals2), Out) ?- !, Out = (OGoals1;OGoals2),
   optimise_body(Goals1, OGoals1),
   optimise_body(Goals2, OGoals2).
optimise_body((If -> Then), Out) ?- !, Out = (OIf -> OThen),
   optimise_body(If, OIf),
   optimise_body(Then, OThen).
optimise_body(Goal, Goal).


printcode([]) :- !.
printcode([Clause|Clauses]) :-
   writeclause(log_output,Clause), nl,
   printcode(Clauses).


/* nonconstrainable(+Goals) succeeds if all goals in Goals
   cannot possibly constrain the value of their arguments. 
*/
nonconstrainable((Guard1, Guard2)) ?- 
      nonvar(Guard1), !,
      nonconstrainable(Guard1), nonconstrainable(Guard2).
nonconstrainable(Guard) :- % single goal
      nonconstraining_goal(Guard).

nonconstraining_goal(_ > _) :- !.
nonconstraining_goal(_ < _) :- !.
nonconstraining_goal(_ >= _) :- !.
nonconstraining_goal(_ =< _) :- !.
nonconstraining_goal(var(_)) :- !.
nonconstraining_goal(nonvar(_)) :- !.
nonconstraining_goal(_ == _) :- !.
nonconstraining_goal(_ =:= _) :- !.
nonconstraining_goal(_ =\= _) :- !.
nonconstraining_goal(_ \== _) :- !.
nonconstraining_goal(ground(_)) :- !.
nonconstraining_goal(nonground(_)) :- !.
nonconstraining_goal(free(_)) :- !.
nonconstraining_goal(integer(_)) :- !.
nonconstraining_goal(number(_)) :- !.
nonconstraining_goal(float(_)) :- !.
nonconstraining_goal(real(_)) :- !.
nonconstraining_goal(rational(_)) :- !.
nonconstraining_goal(breal(_)) :- !.
nonconstraining_goal(_@>_) :- !.   
nonconstraining_goal(_@<_) :- !.   
nonconstraining_goal(_@>=_) :- !.   
nonconstraining_goal(_@=<_) :- !.   
nonconstraining_goal(_@>_) :- !.


/* shared_vars(?T1, ?T2, -Shared, -Remain1) 
     returns in Shared the shared variables of Terms T1 and T2, Remain1 
     will contain the non-shared variables in T1
*/
shared_vars(T1, T2, Shared, Remain1) :-
   term_variables(T1, Vars1),
   term_variables(T2, Vars2),
   intersect(Vars1, Vars2, Shared, Remain1).

intersect([], _, L, R) :- !, L = [], R =[].
intersect([V1|L1], L2, Intersect, Remain) :-
   (membervar(L2, V1) ->
      Intersect = [V1|Intersect0], Remain = Remain0
    ; Intersect = Intersect0, Remain = [V1|Remain0]
   ), intersect(L1, L2, Intersect0, Remain0).


membervar([V1|L], V) :- 
    V == V1 -> true ; membervar(L,V).


initialise_processed(N, Processed) :- 
    functor(Processed, rules, N),
    make_empty_lists(N, Processed).

/* make_list(+CommaList, -List)
   changes CommaList (in the form (A1,A2,....An) to a normal List)
*/
make_list((A1,A2), [A1|L]) :-
    !,
    make_list(A2, L).
make_list(A, [A]).


/* decompose_body(+Body, -GuardGoals, -BodyGoals)
    breaks down the incoming Body into goals for the guard and body
*/
decompose_body((Guards|Goals), Guards, Goals) :- !.
decompose_body(Goals, true, Goals).



syntax_check([], _, _, _).
syntax_check([Rule|Rules], Constraints, PRules, Module) :-
    check_one_rule0(Rule, Constraints, PRules, Module),
    syntax_check(Rules, Constraints, PRules, Module).


check_one_rule0(FRule, Constraints, Processed, Module) :-
    (FRule = (Name ::= Rule) ->
        check_one_rule(Rule, Constraints, Processed, Name, Module)
      ; check_one_rule(FRule, Constraints, Processed, [], Module)
    ).
 
check_one_rule((KeepHeads\DeleteHeads <=> Body), Constraints, Processed, Name,
      Module) ?- !,
    Rule = (KeepHeads\DeleteHeads<=>Body),
    make_list(KeepHeads, KHeadsL0),
    make_list(DeleteHeads, DHeadsL0),
    remove_symmetric(DHeadsL0, DHeadsL), 
    %may_remove_symmetric(KHeadsL0, KHeadsL),
    KHeadsL0 = KHeadsL,
    single_simpogation(KHeadsL, DHeadsL, Body, InvBody, Type, Module),
    simpogation_action(Type, Rule, KHeadsL0, KHeadsL, DHeadsL0, DHeadsL, Body, 
       InvBody, Constraints, Processed, Name).
check_one_rule((Heads <=> Body), Constraints, Processed, Name, _Module) ?- !,
    Rule = (Heads <=> Body),
    make_list(Heads, HeadList0), 
    remove_symmetric(HeadList0, HeadList), 
    definedheads(HeadList, Constraints, delete, [], HeadList0, Body, 0, Rule, 
      Name, not_prop, nil, Processed).
check_one_rule(PNo-(Heads==>Body), Constraints, Processed, Name, _Module) ?-
    Rule = (Heads==>Body),
    make_list(Heads, HeadList0),
    may_remove_symmetric(Body, HeadList0, HeadList, Info), 
    definedheads(HeadList, Constraints, keep, HeadList0, [], Body, 0, Rule, 
      Name, PNo, Info, Processed).


simpogation_action(not_reducible, Rule, KHeadsL0, KHeadsL, DHeadsL0, DHeadsL, 
  Body, _InvBody, Constraints, Processed, Name) ?- !,

    definedheads(KHeadsL, Constraints, keep, KHeadsL0, DHeadsL0, Body, 0, Rule, 
      Name, not_prop, nil, Processed),
    definedheads(DHeadsL, Constraints, delete, KHeadsL0, DHeadsL0, Body, 0, Rule,
      Name, not_prop, nil, Processed).
simpogation_action(nobody, Rule, KHeadsL0, _KHeadsL, DHeadsL0, DHeadsL, 
  Body, _InvBody, Constraints, Processed, Name) ?- !,
    definedheads(DHeadsL, Constraints, delete, KHeadsL0, DHeadsL0, Body, 0, 
      Rule, Name, not_prop, nil, Processed).
simpogation_action(twobodies, Rule, KHeadsL0, _KHeadsL, DHeadsL0, DHeadsL, 
  Body, InvBody, Constraints, Processed, Name) ?- 
    definedheads(DHeadsL, Constraints, delete, KHeadsL0, DHeadsL0, Body, 0, 
      Rule, Name, not_prop, twobodies(InvBody), Processed).



get_prop_nclausearg(V0,V1) :- var(V0), !, V0 = V1.
get_prop_nclausearg([N-_Head|Rest], [N|Ns]) :-
    get_prop_nclausearg(Rest, Ns).

/* definedheads(+HeadList, +Constraints, +Status, +KeepHeads, +DeleteHeads, 
        +Body, +N, +Rule, +Name, +PropNum, +Info, +Processed)
    definedheads checks if the head constraints in HeadList of rule Rule are 
    defined or not. It also does some initial processing on the rule to ease
    the later translation: the constraints that are kept (KeepHeads) and those
    that are deleted (DeleteHeads) are seperated when called. The HeadList 
    passed is either those constraints that are kept (i.e. same as KeepHeads) 
    or those that are deleted (DeleteHeads), as indicated by Status. Body is
    the body of the rule (guard and body goals). N is used to indicate the Nth
    Head in HeadList is being considered. Processed is used to store the 
    processed rule. For a particular rule, an entry is made in Processed for 
    each head constraint in the rule. Name is the name the user gave to the
    rule, would be [] if not named. PropNum is the propagation rule number for
    this propagation rule (not_prop if not propagation rule). 
    Additional information deduced obtained during the processing which 
    have affected the way processed rules are generated - this information
    is passed along in Info for correct behaviour during transformation

*/
definedheads([H|Hs0], Constraints, Status, AllKHeads, AllDHeads, Body, N0, Rule,  Name, NProp, Info, Processed) ?- 
    functor(H, F, A),
    (chr_constraint_info(Constraints, F/A, ConsNum, _) ->
       N1 is N0 + 1,
       (Status == keep ->
	  remove_head(N1, AllKHeads, OtherHeads),
          addto_processed(ConsNum, Rule, keep, H, OtherHeads, AllDHeads, Body, 
             Name, NProp, Info, Processed)
        ; remove_head(N1, AllDHeads, OtherHeads),
          addto_processed(ConsNum, Rule, delete, H, AllKHeads, OtherHeads, 
             Body, Name, NProp, Info, Processed)
       ),
       definedheads(Hs0, Constraints, Status, AllKHeads, AllDHeads, Body, N1, Rule, Name, NProp, Info, Processed)
     ; printf(error, "Syntax error: undefined constraint %a/%w (%w) found in:\n", [F,A,H]),
       flush(error),
       pretty_write(Rule)
       % some error recovery routine
    ). 
definedheads([], _, _, _, _, _, _, _, _, _, _, _).


% for propagation rule, can only remove symmetric heads if body goals cannot 
% affect computation (only catch "true" as such a body goal for now)

may_remove_symmetric(Body, Hs0, Hs, Info) :-
   simple_body(Body) ->
       remove_symmetric(Hs0, Hs), 
       (Hs0 == Hs -> % no removal
          check_double_samehead(Hs, Info)
        ; Info = nil
       ) 
     ; check_double_samehead(Hs0, Info),
       Hs0 = Hs.

simple_body(true) :- !.
simple_body((_G|Body)) :-
   simple_body(Body).

check_double_samehead([H1,H2], Info) :-
   functor(H1, F, A), functor(H2, F, A), !,
   Info = double(H1, H2).
check_double_samehead(_, nil).

 
remove_symmetric([H1,H2], Hs) :- !,
   (is_symmetric(H1, H2) -> Hs = [H1] ; Hs = [H1,H2]).
remove_symmetric(Hs, Hs).


is_symmetric(H1, H2) :-
   \+ \+(H1 = H2),
   \+ \+is_pairsymmetric(H1,H2).

is_pairsymmetric(H1,H2) :-
   copy_term((H2,H1), NPair),
   numbervars((H1,H2), 0, N),
   numbervars(NPair, 0, N),
   NPair == (H1,H2).

no_body((true|true)) :- !.
no_body(true).

% if the simpogation rule has only two heads, and the keep and deleted
% heads are symmetric, then generate only one rule for the two heads
single_simpogation([KH], [DH], Body, NBody, Type, Module) :-
   \+recorded('CHRdont_simpa_symmetric', Module),
   \+ \+(KH = DH),
   \+ \+is_pairsymmetric(KH, DH), !,
   (no_body(Body)  ->
      Type = nobody

    ; Type = twobodies,
      % the following assumes term_variables/2 will extract variables in
      % exactly the same order for two symmetric terms
      term_variables((KH,Body),Vars1),
      term_variables((DH,Body),Vars2),
      copy_term((Body,Vars1,Vars2),(NBody,Vars2,Vars1))
   ).
single_simpogation(_, _, _, _, Type, _M) :- Type = not_reducible.



/*   shared_vars(H1, H2, Shared, _Rest),
   is_symmetric_with_shared(Shared, H1, H2).

is_symmetric_with_shared([], _, _) :- !.
is_symmetric_with_shared([Var|Vars], H1, H2) :-
   % need to check that all shared vars are in same position
   \+ \+(check_one_var(Var, H1, H2)),       
   is_symmetric_with_shared(Vars, H1, H2).
*/

check_one_var(*, H1, H2) :-
   copy_term(H1, H11), 
   variant(H11, H2).  % need copy_term to get rid of shared var

/* addto_processed(+ConsNo, +Rule, +Status, +Head, +KeepHeads, +DeleteHeads, 
       +Body, +Name, +PropagationNo, +Info, +Processed)
   adds information about rule Rule when the trying of the rule is initiated 
   by Head. ConsNo is the constraint number for Head. Status is either keep or 
   delete, indicating if Head is to be kept or deleted from the constraint 
   store if the rule is fired. KeepHeads and DeleteHeads are lists of the 
   other heads in rule (i.e. excluding Head) that are to be kept or deleted 
   after firing of rule. Body is the guard and body goals of the rule.
   Processed is the structure into which this information is to be added.
   PropagationNo is the propagation rule number for this propagation rule.
   Info is additional information that may be needed in transformation stage
*/
addto_processed(ConsNo, Rule, Status, Head, KeepHeads, DeleteHeads, Body, Name,
  NProp, Info, Processed) :-
     arg(ConsNo, Processed, RulesList),
     setarg(ConsNo, Processed, [rule(Rule,Status,Head,KeepHeads,DeleteHeads,Body, NProp, Info, Name)|RulesList]).



remove_head(1, [_Head|Heads], Heads) :- !.
remove_head(N, [Head|Heads0], [Head|Heads1]) :-
    N1 is N - 1,
    remove_head(N1, Heads0, Heads1).

construct_writeconstraint_code(SCons, Define, NNameRule) :-
    functor(SCons, F, A),
    atom_string(F, FS),
    append_strings(FS, "print", NNameS),
    atom_string(NName, NNameS), 

    arg(2, SCons, Constraint),
    NNameRule =.. [NName, SCons, Constraint],
    Define = (:- erase_macro(F/A), export(macro(F/A, NName/2, [write,goal]))).

chr(File, Module) :-
  chr_clear,
  compile(File)@Module.














%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Runtime stuff

count_and_record_constraints((ConstDec1,ConstDec2), Count0, Count, Module) :-
   !,
   count_and_record_constraints(ConstDec1, Count0, Count1, Module),
   count_and_record_constraints(ConstDec2, Count1, Count, Module).
count_and_record_constraints(SingleConstDec, Count0, Count, Module) :-
   Count1 is Count0 + 1,
   % Count is also used as id for constraint
   recorded_list('CHRconstraints', ConList)@Module,
   (extract_constraint_info(SingleConstDec, ConstSpec, Prio) -> 
        true 
   ;
        printf(error, "invalid chr constraint declaration: %w;"
                       " aborting..%n", [SingleConstDec]),
        abort
   ),
   (chr_constraint_info(ConList, ConstSpec, _,_) ->
       % WARNING -- need to be integrated into system errors?
       printf(warning_output, "Warning -- constraint %w already declared\n",ConstSpec),
       Count = Count0
     ; Count = Count1,
       
       recordz('CHRconstraints', chrcinfo with [spec:ConstSpec,count:Count,prio:Prio])@Module
   ).

extract_constraint_info(F/A:PrioSpec, ConstSpec, Prio) ?-
       ConstSpec = F/A,
       priospec_to_priority(PrioSpec, F/A, Prio).
extract_constraint_info(F/A, ConsSpec, Prio) ?-
        ConsSpec = F/A,
        getval(chr_priority, Prio).

priospec_to_priority(at_higher(N), F/A, Prio) ?-
        Prio0 is getval(chr_priority) - N, 
        (Prio0 < 1 -> 
             printf(error, "Relative priority specified for %w:higher(%d)"
                    " too high; using 1 instead%n", [F/A,N]),
             Prio = 1
        ;
             Prio = Prio0
        ).
priospec_to_priority(at_lower(N), F/A, Prio) ?-
        Prio0 is getval(chr_priority) + N, 
        (Prio0 > 11 -> 
             printf(error, "Relative priority specified for %w:lower(%d)"
                    " too low; using 11 instead%n", [F/A,N]),
             Prio = 11
        ;
             Prio = Prio0
        ).
priospec_to_priority(at_absolute_priority(N), F/A, Prio) ?-
        (N > 0, N < 12 -> 
             Prio = N 
        ; 
             printf(error, "Absolute priority out of range for"
                    " %w:absolute_priority(%d); using default priority"
                    " instead%n", [F/A, N]),
             getval(chr_priority, Prio)
        ).


second_member([Nth-I1|Is], I, N) ?- 
    I == I1 -> N = Nth ; second_member(Is, I, N).

chr_constraint_info([chrcinfo with [spec:CSpec0,prio:P0,count:N0]|_],
                    CSpec, N, P) :-
        CSpec0 == CSpec, !,
        N0 = N, P0 = P.
chr_constraint_info([_|CInfos], CSpec, N, P) :-
        chr_constraint_info(CInfos, CSpec, N, P).


nth_member(Nth, [chrcinfo with [count:N, spec:F0/A0]|Constraints], F/A) :-
    (Nth == N -> F = F0, A0 = A ; nth_member(Nth, Constraints, F/A)).


initialise_module_for_chr(Module) :-
   % "global" (to chr in Module) chr constraint list. Note needs to 
   % be initialised to empty list when used.
   local(reference('CHRcstore', 0))@Module,


   % the following probably should not use the index database and should be 
   % module-based; but this update minimise code changes Kish 2002-11-20

   % count of number of chr constraints
   % remove any existing count (may be there as Module erased
   (erase('CHRconst_count', count(Module, _)) -> true ; true), 
   recorda('CHRconst_count', count(Module,0)),

   % count of number of propagation rules
   (erase('CHRprule_count', count(Module, _)) -> true ; true),
   recorda('CHRprule_count', count(Module,0)),
   
   local(record('CHRcode'))@Module, local(record('CHRconstraints'))@Module,
   
   % define a finalization goal for Module that make sure any stray record
   % for Module in the ech module is properly removed.
   local(finalization((
                          ( current_module(ech) -> % may be erased!
                              (erase('CHRconst_count', count(Module,_))@ech->true;true)
                          ;
                              true   % nothing to be done if ech erased
                          )
                      ))
        )@Module.

redefine_cdelete_count(Error, Culprit, Module, LM) :-
    (Culprit = local(array('CHRcdelete_count'(_N), integer)) -> 
           true % allow silent update 
         ; error(default(Error), Culprit, Module)@LM
    ). 



% allow cdelete_count to be redefined silently.
:- set_event_handler(42,  redefine_cdelete_count/4). 


instantiate_list([], _) :- !.
instantiate_list([E|L], E) :-
   instantiate_list(L, E).


/* split_list(+N, +List, -Front, -Back)
   splits the list List such that Front will contain the first Nth elements,
   and Back the rest
*/
split_list(0, List, [], List) :- !.
split_list(N, [E|L], [E|Front], Back) :-
   N1 is N - 1,
   split_list(N1, L, Front, Back).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* find_partner(+SuspL, -PartnerIndex, +Partner, -RestL, -Condition)
   checks the list of suspended constraint SuspL to try to find one that
   matches Partner. PartnerIndex is the internal suspension index 
   associated with the constraint. RestL is the remaining items on the
   SuspL after Partner is found. 

   In the old find_partner/5, Condition returns either yes or no 
   depending on if Partner is found or not. The predicate does not simply
   fail to allow the CHR program to collect information. This information
   is not currently used, so the code is commented out

find_partner([SIndex|Rest], SIndex, Constraint, Rest, yes) :- 
    get_suspension_data(SIndex, goal, Suspended),
    % need to extract constraint from the meta-info surrounding it
    arg(constraint_in_wrapper_pos,Suspended,Constraint).
find_partner([_|Rest], SIndex, Constraint, RestL, Cond) :-
    find_partner(Rest, SIndex, Constraint, RestL, Cond).
find_partner([], _, _, _, no).

*/
/* find_parnter(+,-,-,-) */
find_partner([SIndex|Rest], SIndex, Constraint, Rest) :- 
    get_suspension_data(SIndex, goal, Suspended),
    % need to extract constraint from the meta-info surrounding it
    arg(constraint_in_wrapper_pos,Suspended,Constraint).
find_partner([_|Rest], SIndex, Constraint, RestL) :-
    find_partner(Rest, SIndex, Constraint, RestL).


/* get_global_constraint(+Const, -SuspL, +Module)
   get the global constraint list SuspL for constraint number Const 
*/
get_global_constraint(Const, SuspL, Module) :- 
    getval_body('CHRcstore', Store, Module),
    (Store \== 0 -> arg(Const, Store, SuspL)  ; SuspL = []).



% list is terminated by a variable. Find it
find_tail(Tail, Tail0) :-
   var(Tail), !, Tail0 = Tail.
find_tail([_|Rest], Tail) :-
   find_tail(Rest, Tail).

pretty_write(Term) :- writeln(Term).
       
count_dead([], L,L, D,D) :- !.
count_dead([S|Ss], L0, L, D0, D) :-
    (is_suspension(S) -> D1 = D0 ; D1 is D0 + 1),
    L1 is L0 + 1,
    count_dead(Ss, L1, L, D1, D).

/* get_constraint_list(?Vars, +ConstNo, -SuspL, +Module)
    returns the list of suspended constraints for constraint ConstNo in SuspL.
    The list is either those that are suspended on Vars, or the global list,
    if none of the variables in Vars remain free.
*/ 
get_constraint_list(Vars, ConstNo, SuspL, Module) :-
      nonground(Vars, Var) ->
        get_varconstraint(Var, ConstNo, SuspL, Module)

      ; get_globalconstraint(ConstNo, SuspL, Module).

get_constraint_listi(Vars, ConstNo, SuspL, Cond, Module) :-
      nonground(Vars, Var) ->
        Cond = var,
        get_varconstraint(Var, ConstNo, SuspL, Module)

      ; get_globalconstraint(ConstNo, SuspL, Module).

/*set_varconstraint(_X{ech:Attr}, ConstNo, SuspL) ?-
    setarg(ConstNo, Attr, SuspL).
*/

set_varconstraint(_X{ech:Attr}, ConstNo, SuspL) ?-
    Attr = ech with [slists: Ss],
    setarg(ConstNo, Ss, SuspL).

get_wholevarconstraint(X{ech:Attr0}, Attr, Module) ?-
    nonvar(Attr0), !,
    Attr = Attr0,
    get_wholevarconstraint1(X, Attr, Module).
get_wholevarconstraint(X, Attr, Module) :-
    var(X),
    new_chr_attr(X, Attr, Module).
/*NOCOUNT
get_wholevarconstraint1(_X, SuspSt, Module) :-
    var(SuspSt) -> new_cstore(SuspSt, Module); true.
*/
get_wholevarconstraint1(_X, ech with [slists: SuspSt,count:C], Module) :-
    var(SuspSt) -> new_cstore(SuspSt, Module), C = 0; true.


get_varconstraint(X{ech:Attr}, ConstNo, SuspL, Module) ?-
     nonvar(Attr), !,
     get_varconstraint1(X, ConstNo, SuspL, Attr, Module).     
get_varconstraint(X, _ConstNo, SuspL, Module) :-
     var(X),
     SuspL = [],
     new_chr_attr(X, _Attr, Module).

get_varconstraint1(X, _, SuspL, Attr, Module) :-
     var(Attr), 
     SuspL = [],
     new_chr_attr(X, Attr, Module).
/*get_varconstraint1(_X, ConstNo, SuspL, Attr, _Module) :-
     arg(ConstNo, Attr, SuspL).
NOCOUNT*/
get_varconstraint1(_X, ConstNo, SuspL, Attr, Module) :-
     nonvar(Attr),
     Attr = ech with [slists: Ss,count:C],
     (var(Ss) -> new_cstore(Ss, Module), C = 0, SuspL = [] ;
          arg(ConstNo, Ss, SuspL0),
          (C == 10 -> remove_dead_suspensions(SuspL0, SuspL),
                setarg(ConstNo, Ss, SuspL), setarg(count of ech, Attr, 0)
              ; SuspL = SuspL0, C1 is C + 1,
                setarg(count of ech, Attr, C1)
          )
     ).

get_globalconstraint(ConstNo, SuspL, Module) :-
     getval_body('CHRcstore', Store, Module),
     % Store == 0 if uninitialised. 
     (Store \== 0 -> arg(ConstNo, Store, SuspL) ; SuspL = []).


new_chr_attr(X, Attr, Module) :-  % make a new chr-variable
     Attr = ech with [slists: Ss,count: 0],
     new_cstore(Ss, Module), add_attribute(X, Attr).
/*NOCOUNT
new_chr_attr(X, Attr, Module) :-
     new_cstore(Attr, Module), add_attribute(X, Attr).
*/

new_cstore(Ss, Module) :-
     call(number_of_constraints(Size))@Module, % dependent on chr prog. Need to be supplied
     functor(Ss, s, Size),
     make_empty_lists(Size, Ss).


make_empty_lists(N, Ss) :-
    N == 0 -> 
       true 
     ; N1 is N - 1,
       arg(N, Ss, List),
       (var(List) -> List = [] ; true),  
       make_empty_lists(N1, Ss).

/* insert(+Constraint, +Susp, +ConstNo)
   insert the suspension Susp into variables in the constraint Constraint.
   Optimisation should be possible which variables need to be inserted,
   and into what list
*/
insert(Constraint, Susp, ConstNo, Module) :- 
      term_variables(Constraint, VarL),
      varsuspending(VarL, Susp, ConstNo, Module).


varsuspending([], _, _, _).
/*varsuspending([V|Vs], Susp, ConstNo, Module) :-
   insert_suspension(V, Susp, constrained of suspend, suspend),
   %get_wholevarconstraint(V, Attr, Module),
   %Attr = ech with [slists: SuspSt],
   get_wholevarconstraint(V, SuspSt, Module),
   arg(ConstNo, SuspSt, SuspL0),
   cleanup(SuspL0, SuspL1),
   setarg(ConstNo, SuspSt, [Susp|SuspL1]),
   varsuspending(Vs, Susp, ConstNo, Module).
COUNT*/
varsuspending([V|Vs], Susp, ConstNo, Module) :-
   insert_suspension(V, Susp, constrained of suspend, suspend),
   get_wholevarconstraint(V, Attr, Module),
   Attr = ech with [slists: SuspSt],
   arg(ConstNo, SuspSt, SuspL0),
   cleanup(SuspL0, SuspL1),
   setarg(ConstNo, SuspSt, [Susp|SuspL1]),
   varsuspending(Vs, Susp, ConstNo, Module).


/* cleanup(+SuspLIn, -SuspLOut)
   performs some clean up (removal of dead suspensions) from SuspLIn. 
*/
cleanup(SuspL0, SuspL) :-
   SuspL0 = [Susp|SuspL1] ->
       (is_suspension(Susp) -> SuspL = SuspL0 
           ; cleanup(SuspL1, SuspL)
       )
     ; SuspL = SuspL0.



/* suspended constraints looks like:
    Name(Index,ConstraintGoal,SuspIndex,ExecInfo,Applied)
*/
suspend_constraint(ConstGoal, Constraint, ConstNo, Prio, Module) :-
    getval(constraint_number, N),
    arg(constraintnum_pos,ConstGoal,N),
    incval(constraint_number),
    arg(suspendid_pos,ConstGoal,Susp), % to be filled by suspended goal's index
    make_suspension(ConstGoal, Prio, Susp, Module), 
    getval_body('CHRcstore',Store,Module), % need to add it to the global constraint store
    insert(Constraint, Susp, ConstNo, Module), 
    % insert into variables' constraint store 

    % insert into global constraint store
    (Store == 0 -> /* constraint store not yet initialised, initialise it */
        new_cstore(Store0, Module),
        setval_body('CHRcstore', Store0,Module),
        setarg(ConstNo, Store0, [Susp])
    ;   arg(ConstNo, Store,  SuspL), setarg(ConstNo, Store, [Susp|SuspL])
    ).

kill_constraint(ListIndex, Susp, Module) :-
    var(Susp) -> true ;
    kill_suspension(Susp),
    getval_body('CHRcdelete_count'(ListIndex), Count, Module),
    (Count == cdelete_threshold ->
       setval_body('CHRcdelete_count'(ListIndex), 0, Module), 
       cleanup_conlist(ListIndex, Module)
     ; %Count1 is Count + 1,
       %setval('CHRcdelete_count'(ListIndex), Count1)@Module
       incval_body('CHRcdelete_count'(ListIndex), Module)
    ).

cleanup_conlist(ListIndex, Module) :-
    getval_body('CHRcstore', ConStore, Module),
    arg(ListIndex, ConStore, ConList), % must be already initialised
    remove_dead_suspensions(ConList, NewConList),
    setarg(ListIndex, ConStore,  NewConList).


remove_dead_suspensions_count([], [], 0) :- !.
remove_dead_suspensions_count([Susp|ConsList0], ConsList, N) :-
   is_suspension(Susp) -> 
           ConsList = [Susp|ConsList1],
           remove_dead_suspensions_count(ConsList0, ConsList1, N)
         ; remove_dead_suspensions_count(ConsList0, ConsList, N1),
           N is N1 + 1.


remove_dead_suspensions([], []) :- !.
%remove_dead_suspensions(ConsList, 0, ConsList) :- !.
remove_dead_suspensions([Susp|ConsList0], ConsList) :-
    is_suspension(Susp) -> ConsList = [Susp|ConsList1],
          remove_dead_suspensions(ConsList0, ConsList1)
        ; %N1 is N - 1, 
          remove_dead_suspensions(ConsList0, ConsList).


    


/* check_pairapplied(+RuleNo, ?Applied, +PartnerIndex, -Tail, -Rest, -Status,
        +Module)
   check that propagation rule RuleNo, which has two heads, has not been 
   applied with partner with PartnerIndex. As the applied lists are ordered,
   checking only needs to be done to where PartnerIndex should be in the
   list. Rest is the rest of the list after this position, and Tail is
   where the position is. This is to allow for insertion of PartnerIndex
   in place if rule is fired. Status returns found or notfound.
*/
check_pairapplied(RuleNo, Applied, PartnerIndex, Tail, Rest, Status, Module) :-
     (nonvar(Applied) ->
          get_applied_list(RuleNo, Applied, AppL),
          AppL = [_|Rest0],
          check_pairapplied_direct(AppL, Rest0, PartnerIndex, Tail, Rest, Status)
        ; call(number_of_propagations(Size))@Module, % Need to be supplied by chr program
          create_applied(Applied, Size),
          get_applied_list(RuleNo, Applied, Tail),
	  Rest = [],
          Status = notfound
     ).

check_samepairapplied(RuleNo, Applied, PartnerIndex, Tail, Rest, Status, Module) :-
     (nonvar(Applied) ->
          get_applied_list(RuleNo, Applied, AppL),
          AppL = [_|Rest0],
          check_samepairapplied_direct(AppL, Rest0, PartnerIndex, Tail, Rest, Status)
        ; call(number_of_propagations(Size))@Module, % Need to be supplied by chr program
          create_applied(Applied, Size),
          get_applied_list(RuleNo, Applied, Tail),
          /* in samepairapplied, so Rest is empty list */
          Rest = [],
          Status = notfound
     ).


check_pairapplied_direct(AppL, Rest0, PartnerIndex, Tail, Rest, Status) :-
    (Rest0 = [Index|Rest1] ->
       (Index \== PartnerIndex -> 
         (Index > PartnerIndex -> /* still need to search list */
            check_pairapplied_direct(Rest0, Rest1, PartnerIndex, Tail, Rest, Status) 
          ; Tail = AppL, Rest = Rest0, Status = notfound
         )
        ; Rest = Rest0, Tail = AppL, Status = found
       ) 
     ; /* reached end of list - not found */
       Tail = AppL, Rest = [], Status = notfound
    ).

check_samepairapplied_direct(AppL, Rest0, PosPartnerIndex, Tail, Rest, Status) :-
    (Rest0 = [PosIndex|Rest1] ->
       (PosIndex \== PosPartnerIndex -> 
         PosIndex = Pos-Index, PosPartnerIndex = _PPos-PartnerIndex,
         (Index > PartnerIndex -> /* still need to search list */
            check_samepairapplied_direct(Rest0, Rest1, PosPartnerIndex, Tail, Rest, Status) 
          ; (Index \== PartnerIndex -> Tail = AppL, Rest = Rest0, Status = notfound
            ; % Index == PartnerIndex, so Poses must be different
              % 3 --> both pos tried
              (Pos == 3 -> Rest = Rest0, Tail = AppL, Status = found 
                ; % Pos = 1 or 2, not tried in PPos' position
                  Tail = AppL, Rest = Rest0, Status = notfound
              )
            )
         )
       ; % PosIndex == PosPartnerIndex
         Tail = AppL, Rest = Rest0, Status = found
       ) 
     ; /* reached end of list - not found */ 
       Tail = AppL, Rest = [], Status = notfound
    ).

check_pairapplied_directmarked(AppL, Rest0, PartnerIndex, Tail, ConsNumber, PGoal, RuleNo, Status, Module) :-
    (Rest0 = [Index|Rest1] ->
       (Index \== PartnerIndex -> 
         (Index > PartnerIndex -> /* still need to search list */
            check_pairapplied_directmarked(Rest0, Rest1, PartnerIndex, Tail, ConsNumber, PGoal, RuleNo, Status, Module) 
            ; % first Index < PartnerIndex, so not found and insert it
              Status = notfound,  
              Tail = [PartnerIndex|Rest0],
              setarg(2, AppL, Tail),
              mark_partnerapplied(ConsNumber, PGoal, RuleNo, Module)
         )
        ; %Index == PartnerIndex
          Status = found,
          Tail = Rest0
       ) 
     ; % reached end of list - not found  
       Status = notfound,
       Tail = [PartnerIndex],       
       setarg(2, AppL, Tail),
       mark_partnerapplied(ConsNumber, PGoal, RuleNo, Module)
    ).

mark_partnerapplied(ConsNumber, PGoal, RuleNo, Module) :-
   arg(applied_pos, PGoal, PApplied),
   (var(PApplied) -> 
      call(number_of_propagations(Size))@Module,
      create_applied(PApplied, Size)
    ;true
   ),
   get_applied_list(RuleNo, PApplied, PartnerAppL),
   insert_into_partnerappliedlist(PartnerAppL, ConsNumber).

check_samepairapplied_directmarked(AppL, Rest0, PosPartnerIndex, Tail, PosConsNumber, PGoal, RuleNo, Status, Module) :-
    (Rest0 = [PosIndex|Rest1] ->
       (PosIndex \== PosPartnerIndex -> 
         PosIndex = Pos-Index, PosPartnerIndex = _PPos-PartnerIndex,
         (Index > PartnerIndex -> /* still need to search list */
            check_samepairapplied_directmarked(Rest0, Rest1, PosPartnerIndex, Tail, PosConsNumber, PGoal, RuleNo, Status, Module) 
          ; (Index \== PartnerIndex -> 
                mark_samepartnerapplied(PosConsNumber, PGoal, RuleNo, Module),
                setarg(2, AppL, [PosPartnerIndex|Rest0]), 
                Tail = Rest0, Status = notfound
            ; % Index == PartnerIndex, so Poses must be different
              % 3 --> both pos tried
              (Pos == 3 -> Tail = Rest0, Status = found 
                ; % Pos = 1 or 2, not tried in PPos' position, now both tried
                  setarg(1,PosIndex, 3), Tail = Rest0, 

                  mark_samepartnerapplied(PosConsNumber, PGoal, RuleNo, Module), 

                  Status = notfound
              )
            )
         )
       ; % PosIndex == PosPartnerIndex
         Tail = Rest0, Status = found
       ) 
     ; /* reached end of list - not found */
       mark_samepartnerapplied(PosConsNumber, PGoal, RuleNo, Module),  
       Tail = [PosPartnerIndex],
       setarg(2, AppL, Tail), Status = notfound
    ).

mark_samepartnerapplied(PosConsNumber, PGoal, RuleNo, Module) :-
    arg(applied_pos, PGoal, PApplied),
    (var(PApplied) -> 
          call(number_of_propagations(Size))@Module,
          create_applied(PApplied, Size)
         ;true
    ),
    get_applied_list(RuleNo, PApplied, PartnerAppL),
    insert_into_samepartnerappliedlist(PartnerAppL, PosConsNumber).


% like checkpairapplied_direct, except that Index known not to be in list,
% just need to find where it should be inserted.
find_pairappliedinsertpos(AppL, Rest0, Index, Tail, Rest) :-
    (Rest0 = [Index0|Rest1] ->
         (Index0 > Index -> /* still need to search list */
            check_pairapplied_direct(Rest0, Rest1, Index, Tail, Rest, _) 
          ; Tail = AppL, Rest = Rest0
         )
     ; /* reached end of list - not found */ 
       Tail = AppL, Rest = []
    ).

/*  For multiple heads not yet ready
check_multiapplied(RuleNo, Applied, Partners, Tail, Rest, Status, Module) :-
    (nonvar(Applied) ->
         get_applied_list(RuleNo, Applied, AppL),
         check_applied_direct(AppL, Partners, Tail, Rest, Status)
       ; call(number_of_propagations(Size))@Module, % Need to be supplied by chr program
         create_applied(Applied, Size),
         arg(RuleNo, Applied, Tail),
         Rest = [],
         Status = notfound
     ).
%%%%%%%%%%%% write rest of code later.
*/


insert_pairapplied(PConsNumber, Tail, Rest, ConsNumber, PGoal, RuleNo, Module) :-
   setarg(2, Tail, [PConsNumber|Rest]),
   arg(applied_pos, PGoal, PApplied),
   (var(PApplied) -> 
      call(number_of_propagations(Size))@Module,
      create_applied(PApplied, Size)
    ;true
   ),
   get_applied_list(RuleNo, PApplied, PartnerAppL),
   insert_into_partnerappliedlist(PartnerAppL, ConsNumber).

insert_samepairapplied(PosPConsNumber, Tail, Rest, PosConsNumber, PGoal, RuleNo, Module) :-
   insert_samepair_atpos(PosPConsNumber, Tail, Rest),
   arg(applied_pos, PGoal, PApplied),
   (var(PApplied) -> 
      call(number_of_propagations(Size))@Module,
      create_applied(PApplied, Size)
    ;true
   ),
   get_applied_list(RuleNo, PApplied, PartnerAppL),
   insert_into_samepartnerappliedlist(PartnerAppL, PosConsNumber).

insert_samepair_atpos(PosPConsNumber, Tail, Rest) :-
   (Rest = [PosNum|_] ->
      PosPConsNumber = _Pos-PConsNumber,
      (PosNum = _-PConsNumber ->
         % same PConsNumber, must have tried both positions, so set Pos to 3
         setarg(1, PosNum, 3)
       ; % not same PConsNumber...
         setarg(2, Tail, [PosPConsNumber|Rest])
      )
     ; % Rest is empty list
       setarg(2, Tail, [PosPConsNumber])
   ).

insert_into_partnerappliedlist(PAppL, ConsNum) :-
   PAppL = [_|Rest0],
   find_pairappliedinsertpos(PAppL, Rest0, ConsNum, Tail, Rest),
   setarg(2, Tail, [ConsNum|Rest]).

insert_into_samepartnerappliedlist(PAppL, PosPConsNum) :-
   PAppL = [_|Rest0],
   check_samepairapplied_direct(PAppL, Rest0, PosPConsNum, Tail, Rest, _S),
   % defensive check, comment out for max. speed
   %(_S == found -> writeln('ECKKKKKK....');true),
   insert_samepair_atpos(PosPConsNum, Tail, Rest).


add_applied(PartnersNo, Tail) :-
     (nonvar(Tail) ->
        Tail = [_|Ns],
        NewTail = [PartnersNo|Ns],
        setarg(2,Tail,NewTail)
      ; Tail = [PartnersNo]
    ).

create_applied(Applied, Size) :-
   functor(Applied, a, Size).

get_applied_list(N, Applied, List) :-
    arg(N, Applied, List),
    (var(List) -> List = [1.0Inf] ; true).


% Fix for bug 491: when unifying two ech-variables, we wake one (arbitary)
% variable's slists.  This will cause multi-head rules to fire which, as a
% result of the unification, now have the necessary shared head variables.
% This is necessary because since release 5.8 not all suspensions in the
% constrained-lists are woken on var-var unifications (only those that the
% vars have in common).  Waking a single variable's lists should be
% sufficient because either head-partner constraint can find the other.

% unify_ech(+Term, Attribute)
unify_ech(_, Attr) :- 
   var(Attr), !.
% Fix for bug#745, Kish 2013-02-14 - the ech suspension lists needs to be
% inherited by any variables in the compound term.
unify_ech(Term, Attr) :-
   compound(Term), !,
   arg(slists of ech, Attr, CStore),
   term_variables(Term, Vars),
   ( Vars = [] -> 
       true
   ;
       arity(CStore, Size),
       (foreach(V, Vars), param(CStore, Size) do
           add_chrstore_to_var(V, CStore, Size)
       )
   ).   
unify_ech(Term, _) :-
   atomic(Term), !.
unify_ech(Term{ech:Attr0}, Attr1) ?- 
   (nonvar(Attr0) ->
       /* Term is var, there are chr attributes for both variables */
       arg(slists of ech, Attr0, CStore0),
       arg(slists of ech, Attr1, CStore1),
       chrstore_merge_and_schedule(CStore0, CStore1)
   ;
       /* Term does not have  chr attribute, just add it in */
       add_attribute(Term, Attr)
   ).


chrstore_merge_and_schedule(CS0, CS1) :-
   functor(CS0, F, A),
   functor(CS1, F, A),
   % merge the lists from Attr1 into Attr0
   chrstore_merge_and_schedule(A, CS0, CS1).

chrstore_merge_and_schedule(0, _, _) :- !.
chrstore_merge_and_schedule(N, CS0, CS1) :-
   schedule_suspensions(N, CS0),	% schedule either CS0 or CS1 here
   merge_one_slist(N, CS0, CS1),
   N1 is N - 1,
   chrstore_merge_and_schedule(N1, CS0, CS1).

chrstore_merge(0, _, _) :- !.
chrstore_merge(N, CS0, CS1) :-
   merge_one_slist(N, CS0, CS1),
   N1 is N - 1,
   chrstore_merge(N1, CS0, CS1).

merge_one_slist(N, CS0, CS1) :-
   arg(N, CS0, List0),
   arg(N, CS1, List1),
   ordered_merge(List0, List1, List),
   setarg(N, CS0, List).

ordered_merge([], L, L) :- !.
ordered_merge(L, [], L) :- !.
ordered_merge(L0, L1, L) :-
   L0 = [SIndex0|L00],
   L1 = [SIndex1|L11],
   (get_suspension_data(SIndex0, goal, G0) ->
       (get_suspension_data(SIndex1, goal, G1) ->
           arg(constraintnum_pos, G0, CN0),
           arg(constraintnum_pos, G1, CN1),
           (CN0 =:= CN1 ->
               L = [SIndex0|L2], % identical, throw one away
               ordered_merge(L00, L11, L2)
	   ;CN0 > CN1 ->
               L = [SIndex0|L2],
               ordered_merge(L00, L1, L2)
	   ; % CN1 > CN0
               L = [SIndex1|L2],
               ordered_merge(L0, L11, L2)
           )
	 ; ordered_merge(L0, L11, L) % throw away dead suspension
       )
     ; ordered_merge(L00, L1, L) % throw away dead suspension

   ).


/* add an existing chrstore from one variable to another variable */
add_chrstore_to_var(V{Attr1}, Cs, Size) ?- !,
        (nonvar(Attr1) ->
            % already a CHR module, merge stores
            arg(slists of ech, Attr1, Cs1),
            arity(Cs1, Size),
            chrstore_merge(Size, Cs1, Cs)
        ;
            % new CHR variable, copy store
            duplicate_cstore(Attr, Cs, Size)
        ).
add_chrstore_to_var(V, Cs, Size) :-
        free(V),
        duplicate_cstore(Attr, Cs, Size),
        add_attribute(V, Attr).

duplicate_cstore(NewAttr, OldCs, Size) :-
        NewAttr = ech{slists: NewCs, count:0},
        functor(OldCs, Name, Size),
        functor(NewCs, Name, Size),
        (foreacharg(OldL, OldCs), foreacharg(NewL, NewCs) do
            (foreach(S, OldL), fromto(NewL, NL1,NL2, []) do
                % copy and also clean up lists
                (is_suspension(S) -> NL1 = [S|NL2] ; NL1 = NL2)
            )
        ).

chr_clear :-
   recorded_list('CHRconst_count', CCountL),
   erase_all('CHRconst_count'),
   erase_all('CHRprule_count'),
   erase_all('CHRadding_code'),
   setval(constraint_number, 0),
   clean_each_module(CCountL).

clean_each_module([]) :- !.
clean_each_module([count(Module,_)|L]) :-
   erase_all('CHRcode')@Module,
   erase_all('CHRconstraints')@Module,
   % get around bug b91: reset the store so a new store will not inherit 
   % incorrect value 
   setval_body('CHRcstore', 0, Module), 
   erase_array('CHRcstore')@Module,
   clean_each_module(L).


is_in_store([CIndex|L], NewCon) :-
   (get_suspension_data(CIndex, goal, CGoal) ->
       arg(constraint_in_wrapper_pos, CGoal, Con),
       (Con == NewCon -> true ; is_in_store(L, NewCon) )
     ; is_in_store(L, NewCon)
   ).


multi_append(Lists, Appended) :-
  multi_append1(Lists, [], Appended).

multi_append1([], List0, List1) ?- !, List0 = List1.
multi_append1([List|Ls], App1, Appended) ?- 
   nonvar(List),
   append(List, App1, App2),
   multi_append1(Ls, App2, Appended).


chr_get_gconstraint(Constraint, Module) :-
    (nonvar(Constraint) ->
	recorded_list('CHRconstraints', ConList)@Module,
	functor(Constraint, F, A),
	chr_constraint_info(ConList,  F/A, ConsNo, _),
	get_global_constraint(ConsNo, SuspL, Module),
	matching_constraint(SuspL, Constraint, CIndex)
    ;   getval_body('CHRcstore', Store, Module),
	functor(Store, _, Size),
	get_all_sconstraints(Size, Store, Constraint, ConsNo, CIndex)
    ),
    kill_constraint(ConsNo, CIndex, Module).

get_all_sconstraints(0, _, _, _, _) :- !, fail.
get_all_sconstraints(N, Store, Constraint, N, CIndex) :-
	arg(N, Store, SuspL),
	matching_constraint(SuspL, Constraint, CIndex).
get_all_sconstraints(N, Store, Constraint, CurrentN, CIndex) :-
	N1 is N - 1,
	get_all_sconstraints(N1, Store, Constraint, CurrentN, CIndex).

matching_constraint([CIndex0|_], Constraint, CIndex) :-
	get_suspension_data(CIndex0, goal, CGoal),
	arg(constraint_in_wrapper_pos, CGoal, Constraint),
	CIndex = CIndex0.
matching_constraint([_|L], Constraint, CIndex) :-
	matching_constraint(L, Constraint, CIndex).


chr_get_vconstraint(_V{ech:Attr}, Constraint, Module) ?-
	nonvar(Attr),
	Attr = ech with [slists:Ss],
	nonvar(Ss),
	(nonvar(Constraint) ->
	    recorded_list('CHRconstraints', ConList)@Module,
	    functor(Constraint, F, A),
	    chr_constraint_info(ConList,  F/A, ConsNo, _),
	    arg(ConsNo, Ss, SuspL),
	    matching_constraint(SuspL, Constraint, CIndex)
	; functor(Ss, _, Size),
	  get_all_sconstraints(Size, Ss, Constraint, ConsNo, CIndex)
        ), 
	kill_constraint(ConsNo, CIndex, Module).


in_chrstore(Constraint, Module) :-
    recorded_list('CHRconstraints', ConList)@Module,
    functor(Constraint, F, A),
    chr_constraint_info(ConList,  F/A, ConsNo, _),
    get_global_constraint(ConsNo, SuspL, Module),
    is_in_store(SuspL, Constraint).

option(default_chr_priority, Prio, _) ?- !,
        (integer(Prio),
         Prio > 0,
         Prio < 12 ->
             setval(chr_priority, Prio),
             printf(log_output, "Changed default chr priority to %d%n",
                    [Prio])
        ;
             printf(error, "Invalid chr priority: %d. Priority is uncahnged."
                    "%n", [Prio])
        ).
option(Option, State, Module) :- 
    is_valid_option(Option, OptionName) ->
       may_erase(OptionName, Module),
       update_option(State, OptionName, Module)
     ; printf(error, "'%w' is not a valid option.\n", [Option]).

update_option(on, _OptionName, _Module) ?- !.
update_option(off, OptionName, Module) ?- !,
    recorda(OptionName, Module).
update_option(State, _, _) :-
    printf(error, "'%w' is an invalid state for options.\n",[State]).

is_valid_option(check_guard_bindings, 'CHRdont_guard_bindings').
is_valid_option(already_in_store, 'CHRdont_in_store').
is_valid_option(single_symmetric_simpagation, 'CHRdont_simpa_symmetric').

may_erase(Key, Value) :-
   erase(Key, Value), !.
may_erase(_, _).

%-----------------------------------------------------------------------

:- comment(categories, ["Constraints","Techniques"]).
:- comment(summary, "Extended constraint handling rules library").

:- comment(desc, html("\
   This library allows the user to write constraint handling rules (CHR) in 
   their ECLiPSe programs. CHR is a high-level language extension for writing
   user-defined constraints, allowing for rapid prototyping of constraints.
<P>
   This library provides some extensions over the older chr library: 
<UL>
    <LI> support for multi (>2) headed simplification and simpagation rules

    <LI> cleanup of semantics and syntax of CHRs

    <LI> faster execution

    <LI> more convenient compilation and mixing with ECLiPSe code
</UL>
   CHRs are compiled by source-to-source transformation into ECLiPSe code that
   make calls to many ech library predicates that implements the CHR 
   functionality. Thus, most exported predicates predicates  are not intended
   to be used by the user directly, and are not documented.
")).
 
:- comment((constraints)/1, [
	summary: "Directive for declaring SpecList to be CHR constraints",
	amode: constraints(+),
	args: ["SpecList": "Sequence of the form Atom/Integer, or Atom/Integer:PrioSpec"],
	eg: "\
   :- constraints leq/2.
   :- op(700, xfx, leq).

   X leq Y <=> \\+nonground(X), \\+nonground(Y) | X @=< Y.
   X leq X <=> true.
",
	desc: html("\
   Declares the predicates specified in SpecList as CHR constraints. This 
   allows the predicate to appear in the head of a CHR rule. A constraint
   can be follwed by a priority specification PrioSpec, which can be one of:
<P>
      1. at_lower(++N) 2. at_higher(++N) 3. at_absolute_priority(++N)
<P>
   where N is an integer. This specifies the priority the CHR rules will be
   executed at if the specified constraint is the active constraint.
   at_lower and at_higher specifies that the priority is N lower or higher
   than the default CHR priority, and for at_absolute_priority, it is the
   actual priority. 
<P>
   Note that a predicate declared as a CHR constraint should not appear as
   a normal ECLiPSe predicate. Any such definition of the predicate in the
   user's program would be replaced by the CHR definition.
")]
).

:- comment(in_chrstore/1, [
	summary: "Test if CHRConst is in the CHR constraint store or not",
	amode: in_chrstore(+),
	args: ["CHRConst": "A CHR constraint"],
	eg: "\
    X leq Y, Y leq Z ==> \\+in_chrstore(X leq Z)| X leq Z.
",
	desc: html("\
  This predicate is used to test if a particular CHR constraint is in the
  CHR constraint store or not. It can be used to prevent the addition of
  redundant constraints. This only useful if the 'already_in_store' 
  option is off. 
")]
).

:- comment(option/2, [
	summary: "Specify options for controlling ech compilation",
	amode: option(++,++),
	args: ["Option": "Option Name (Atom)",
               "On_or_Off":  "If Option should be on ('on') or off ('off'),"
               " or an integer between 1 and 11"
	      ],
	desc: html("\
   Allows the user to set options that affect the way the CHRs are compiled.
   These options can be turned on or off, with the default state being on.
   The options controls if certain run-time safety checks are performed or
   not. With the option off, the associated safety check will not be generated
   for the CHR code when compiled. Such code can run more efficiently, but
   can lead to incorrect behaviour that the checks would have been able to
   prevent.
   
<DL>
     <DT>check_guard_bindings
          <DD> When executing a guard in a CHR rule, checks are performed
          so that if a guard goal attemps to touch a global variable (i.e. a
          variable which occurs in the rule head), the guard will fail. With
          this option set to `off', then the checks are not performed.

     <DT>already_in_store
          <DD> Before adding a user-defined CHR constraint to the constraint
          store, a check is performed to see if the particular constraint
          (i.e. with exactly the same variables) is already in the store.
          If this option is set to `off', then the check is not performed.
          The user can explicitly check if a constraint is in store by the
          in_chrstore/1 predicate.

     <DT>chr_priority
          <DD> On_or_Off is an integer between 1 and 11, specifying the
          default priority for CHR constraints generated by the compiler.
          It also specifies the priority that at_lower/at_higher
          declarations will be calculated from in the constraints/1
          declarations. Note that all priorities are determined at the
          point of the constraint declaration and is not affected by any
          subsequent changes in chr_priority.

</DL>
")
]).

