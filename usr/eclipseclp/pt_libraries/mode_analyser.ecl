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
% Copyright (C) 2003 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Andrew Cheadle, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
%
% Description:  Instrumentation based mode analyser
%
% System:       ECLiPSe Constraint Logic Programming System
% Author/s:     Andrew Cheadle, IC-Parc
% Version:      $Id: mode_analyser.ecl,v 1.3 2009/07/16 09:11:27 jschimpf Exp $
%
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
% Module definition, library dependencies and predicate import / export
% ----------------------------------------------------------------------

:- module(mode_analyser).

:- comment(categories, ["Development Tools"]).
:- comment(summary, "Instrumentation based mode analyser").
:- comment(author,"Andrew Cheadle").
:- comment(date, "$Date: 2009/07/16 09:11:27 $").
:- comment(copyright, "Cisco Systems, Inc.").
:- comment(status, prototype).

:- comment(desc, html("<P>
    The mode_analyser library is a tool that assists in the determination of
    the mode/1 directive for predicate definitions. This directive informs
    the compiler that the arguments of the specified predicate will always 
    have the corresponding form when the predicate is called. The compiler 
    utilises this information during compilation of the predicate in order
    to generate more compact and/or faster code. Specifying the mode of a 
    predicate that has already been compiled has no effect, unless it is 
    recompiled. If the specified procedure does not exist, a local
    undefined procedure is created.
</P><P>
    The mode analyser inserts instrumentation into the clause definitions of
    predicates during compilation in order to record mode usage of each 
    predicate argument. The code should then be run (as many times as is
    necessary to capture the most common invocations of each predicate 
    undergoing analysis). Finally, the results of the analysis are requested
    and the suggested mode annotations for each predicate are displayed.
</P><P>
    NOTE: It is imperative to understand that the results of mode analysis 
    are merely suggestions for the invocation modes of a predicate based on 
    runtime information. If there are potential predicate invocation modes 
    that were not exercised during runtime, the tool is unable to account 
    for them in its analysis. For the mode specifier '-' the mode analyser
    does not determine whether the variable occurs in any other argument
    (i.e. is aliased), this must be manually verified. 
    In summary, the programmer must verify that the suggested modes are correct 
    before using the directive in the code.  If the instantiation of the 
    predicate call violates its mode declaration, no exception is raised and 
    its behaviour is undefined.
</P><P>
	The usage is as follows:
	<OL>
	<LI>Load the mode_analyser library
	<PRE>
	?- lib(mode_analyser).
	</PRE>
	<LI>Compile your program with the mode analyser compiler
    <PRE>
	?- mode_analyser:analyse(my_program).
	</PRE>
	<LI>Run the query for which you wish to generate mode analysis data
	<PRE>
	?- my_query(X,Y,Z).
	</PRE>
	<LI>Generate the results for the module into which the program
        was compiled.
	<PRE>
	?- mode_analyser:result(my_program_module).
	</PRE>
</P><P>
	Limitations to be resolved shortly: 
	<OL>
    <LI>A future enhancement will enable the insertion of the suggested 
    (or programmer ammended) mode/1 directives for each predicate
    definition into the source code.
    </OL>
</P>")).

:- lib(instrument).

:- export analyse/1, weave_analyse_mode_code/6, 
          result/0, result/1, reset/0.


% ----------------------------------------------------------------------
% Global variables
% ----------------------------------------------------------------------

:- local store(mode_analysis_store).

:- local reference(headArguments).


% ----------------------------------------------------------------------
% Top-level 'analyse' predicate
% ----------------------------------------------------------------------

:- comment(analyse/1,
        [ summary:"Compile a file, inserting invocation mode analysis 
   instrumentation for each predicate definition.",
          args:["File":"Atom or string"],
          amode:analyse(+),
          resat:no,
          exceptions:[5 : "File is not an atom or string"],
          see_also:[result/0, result/1, reset/0],
          desc:html("<P>    
   The mode analyser is a variant of the ECLiPSe that inserts 
   instrumentation into the clause definitions of predicates
   (defined in the named file) during compilation in order to record mode 
   usage of each predicate argument. The code is then run as many times 
   as is necessary to capture the most common invocations of each 
   predicate undergoing analysis. Finally, the results are analysed, 
   using <TT>mode_analyser:result/0</TT>, to produce suggested mode/1 
   directives for each predicate.
   </P><P>
   The mode/1 directive informs the compiler that the arguments of the 
   specified predicate will always have the corresponding form when the 
   predicate is called.  The compiler utilises this information during 
   compilation of the predicate in order to generate more compact and/or 
   faster code. Specifying the mode of a predicate that has already been 
   compiled has no effect, unless it is recompiled. If the specified 
   procedure does not exist, a local undefined procedure is created.
   </P><P>
   NOTE: It is imperative to understand that the results of mode analysis 
   are merely suggestions for the invocation modes of a predicate based on 
   runtime information. If there are potential predicate invocation modes 
   that were not exercised during runtime, the tool is unable to account 
   for them in its analysis. For the mode specifier '-' the mode analyser
   does not determine whether the variable occurs in any other argument
   (i.e. is aliased), this must be manually verified. 
   In summary, the programmer must verify that the suggested modes are correct 
   before using the directive in the code.  If the instantiation of the 
   predicate call violates its mode declaration, no exception is raised and 
   its behaviour is undefined."),
          eg: "
   [eclipse 1]: mode_analyser:analyse('queens_gfc.pl').
   queens_gfc.pl compiled traceable 13920 bytes in 0.10 seconds

   Yes (0.10s cpu)"
        ]).

:- tool(analyse/1, analyse_body/2).

analyse_body(File, Module) :-
        instrument(File, itemplate with [code_weaver:(mode_analyser:weave_analyse_mode_code/6),
                                         module_scope:every_module], 
                   [instrument_recursive:on])@Module.


valid_on_off_option(Option) :-
        var(Option),
        !,
        error(4, valid_on_off_option(Option)).
valid_on_off_option(on) :- !.
valid_on_off_option(off) :- !.
valid_on_off_option(Error) :-
        error(5, valid_on_off_option(Error)).


% ----------------------------------------------------------------------
% Mode checking code weaver predicate
% ----------------------------------------------------------------------

:- comment(weave_analyse_mode_code/6, hidden).
   
:- mode weave_analyse_mode_code(++, ?, ++, ?, ++, ++).
weave_analyse_mode_code(_FileKey, Head, head, Head, compile, _Module) :-
        !,
        setval(headArguments, Head).
weave_analyse_mode_code(_FileKey, Body, body, WeavedBody, compile, Module) :-
        !,
        getval(headArguments, Head),
        functor(Head, F, A),
        % Create the stores for the mode analysis counters
        create_mode_count_stores(F, A, Module),
        WeavedBody = (
                      store_get(mode_analysis_store, Module, PredStore)@mode_analyser,
                      store_get(PredStore, F/A, ArgStore),
                      WeavedArgCheckBody
                     ),
        ( foreacharg(Arg, Head),
          fromto(Body, Body0, Body1, WeavedArgCheckBody),
          for(ArgNum, 1, A),
          param(ArgStore)
        do
            Body1 = 
            (
                store_get(ArgStore, ArgNum, ArgModeCountStore),
		( free(Arg) ->
                    store_inc(ArgModeCountStore, -)
                ; var(Arg) -> 
                    store_inc(ArgModeCountStore, *)
                ; ground(Arg) -> 
                    store_inc(ArgModeCountStore, ++)
                ; nonvar(Arg) ->
                    store_inc(ArgModeCountStore, +)
                ;
                    store_inc(ArgModeCountStore, ?)
                ),
                Body0
            )
        ).
weave_analyse_mode_code(_FileKey, Code, _Type, Code, _Mode, _Module).


:- mode create_mode_count_stores(++, ++, ++).
create_mode_count_stores(Functor, Arity, Module) :-
        ( store_get(mode_analysis_store, Module, PredStore) ->
            (store_get(PredStore, Functor/Arity, _) ->
                % Weaving new clause for a previously encountered
                % predicate definition
                true
            ;
                % First clause of predicate encountered
                CreateArgStore = true
            )
        ;
            % First clause of first predicate in module encountered
            store_create(PredStore),
            store_set(mode_analysis_store, Module, PredStore),
            CreateArgStore = true
        ),
        ( CreateArgStore == true ->
            store_create(ArgStore),
            ( for(ArgNum, 1, Arity),
              param(ArgStore)
            do
              store_create(ArgModeCountStore),
              store_set(ArgStore, ArgNum, ArgModeCountStore)
            ),
            store_set(PredStore, Functor/Arity, ArgStore)
        ;
            true
        ).


% ----------------------------------------------------------------------
% Mode count analysis and results display
% ----------------------------------------------------------------------

:- comment(result/0,
        [ summary:"Produce the mode analysis results for predicates 
   compiled into the calling context module.",
          args:[],
          fail_if:"No predicates have been compiled with the mode analyser 
   into the calling context module.",
          exceptions:[4 : "Calling context module is not instantiated"],
          resat:no,
          see_also:[analyse/1, result/1, reset/0],
          desc:html("<P>
   This predicate displays the suggested invocation modes for each 
   predicate compiled using the mode analyser into the calling context module. 
   The suggested mode annotations can be passed to the mode/1 directive for 
   the associated predicate. The predicate is a tool and as such can be 
   invoked for an arbitrary module using <TT>mode_analyser:result@Module</TT>.
   </P><P>
   The possible argument modes produced for each argument of each 
   predicate are: 
   </P><P><PRE>
   +   The argument is instantiated, i.e. it is not a variable.
   </PRE></P><P><PRE>
   ++  The argument is ground.
   </PRE></P><P><PRE>
   -   The argument is not instantiated, it must be a free variable without 
       any constraints, especially it must not occur in any other argument 
       and it cannot be a suspending variable. 
   </PRE></P><P><PRE>
   ?   The mode is not known or it is neither of the above ones.
   </PRE></P><P>
   NOTE: It is imperative to understand that the results of mode analysis 
   are merely suggestions for the invocation modes of a predicate based on 
   runtime information. If there are potential predicate invocation modes 
   that were not exercised during runtime, the tool is unable to account 
   for them in its analysis. For the mode specifier '-' the mode analyser
   does not determine whether the variable occurs in any other argument
   (i.e. is aliased), this must be manually verified. 
   In summary, the programmer must verify that the suggested modes are correct 
   before using the directive in the code.  If the instantiation of the 
   predicate call violates its mode declaration, no exception is raised and 
   its behaviour is undefined."),
          eg: "
   [eclipse 2]: nqueens(8, Qs).
   L = [1, 5, 8, 6, 3, 7, 2, 4]
   Yes (0.00s cpu, solution 1, maybe more) ?

   ...
   ...

   Yes (0.10s cpu)

   [eclipse 5]: mode_analyser:result.
           nqueens(++, -)
           noattack(?, ?)
           safe(+)
           noattack(?, +, ++)

   Yes (0.00s cpu)"
        ]).

:- tool(result/0, result_body/1).

result_body(Module) :-
        result_body([verbose:off], Module).

:- comment(result/1,
        [ summary:"Produce the mode analysis results for predicates
   compiled into the calling context module.",
          args:["OptionList" : "List of Name:Value pairs"],
          amode:result(+),
          fail_if:"No predicates have been compiled with the mode analyser 
   into the calling context module.",
          resat:no,
          exceptions:[4 : "Calling context module is not instantiated"],
          see_also:[analyse/1, result/0, reset/0],
          desc:html("<P>
   This predicate displays the suggested invocation modes for each predicate 
   compiled using the mode analyser into the calling context module. 
   The suggested mode annotations can be passed to the mode/1 directive for 
   the associated predicate. The predicate is a tool and as such can be 
   invoked for an arbitrary module using <TT>mode_analyser:result@Module</TT>.
   </P><P>
   OptionList may contain the following options:
   <DL><DT>
   <PRE>verbose (default:off)</PRE>
   <DD>If set to 'on', the mode analyser will print details of argument 
   invocation modes for each argument of every predicate compiled into 
   the module with the mode analyser.
   </DL></P></P><P>
   The possible argument modes produced for each argument of each 
   predicate are: 
   </P><P><PRE>
   +   The argument is instantiated, i.e. it is not a variable.
   </PRE></P><P><PRE>
   ++  The argument is ground.
   </PRE></P><P><PRE>
   -   The argument is not instantiated, it must be a free variable without 
       any constraints, especially it must not occur in any other argument 
       and it cannot be a suspending variable. 
   </PRE></P><P><PRE>
   *   The argument is an attributed variable that may have constraints 
       or is a suspending variable. This detail is not a valid argument mode
       to the mode/1 directive and is printed merely for interest. As a result
       the actual argument mode will appear as '?'.
   </PRE></P><P><PRE>
   ?   The mode is not known or it is neither of the above ones.
   </PRE></P><P>
   NOTE: It is imperative to understand that the results of mode analysis 
   are merely suggestions for the invocation modes of a predicate based on 
   runtime information. If there are potential predicate invocation modes 
   that were not exercised during runtime, the tool is unable to account 
   for them in its analysis. For the mode specifier '-' the mode analyser
   does not determine whether the variable occurs in any other argument
   (i.e. is aliased), this must be manually verified. 
   In summary, the programmer must verify that the suggested modes are correct 
   before using the directive in the code.  If the instantiation of the 
   predicate call violates its mode declaration, no exception is raised and 
   its behaviour is undefined."),
          eg: "
   [eclipse 2]: nqueens(8, Qs).
   L = [1, 5, 8, 6, 3, 7, 2, 4]
   Yes (0.00s cpu, solution 1, maybe more) ?
   ...
   ...
   Yes (0.10s cpu)
   [eclipse 5]: mode_analyser:result([verbose:on]).

   Mode analysis for eclipse : nqueens / 2:
        Results for argument 1:
                -: 0    *: 0    +: 0    ++: 1
        Results for argument 2:
                -: 1    *: 0    +: 0    ++: 0

        nqueens(++, -)

   Mode analysis for eclipse : noattack / 2:
        Results for argument 1:
                -: 0    *: 8    +: 0    ++: 0
        Results for argument 2:
                -: 0    *: 0    +: 7    ++: 1

        noattack(?, ?)

   Mode analysis for eclipse : safe / 1:
        Results for argument 1:
                -: 0    *: 0    +: 8    ++: 0

        safe(+)

   Mode analysis for eclipse : noattack / 3:
        Results for argument 1:
                -: 0    *: 28   +: 0    ++: 0
        Results for argument 2:
                -: 0    *: 0    +: 28   ++: 0
        Results for argument 3:
                -: 0    *: 0    +: 0    ++: 28

        noattack(?, +, ++)

   Yes (0.00s cpu)"
        ]).

:- tool(result/1, result_body/2).

result_body(OptionList, Module) :-
        ( memberchk(verbose:Verbose, OptionList) ->
            valid_on_off_option(Verbose)
        ;
            Verbose = off
        ),
        store_get(mode_analysis_store, Module, PredStore),
        stored_keys(PredStore, PredSpecKeys),
        ( foreach(Functor/Arity, PredSpecKeys),
          param(Module, PredStore, Verbose)
        do
            ( Verbose == on ->
                printf("%nMode analysis for %w:%n", [Module:Functor/Arity])
            ;
                true
            ),
            store_get(PredStore, Functor/Arity, ArgStore),
            functor(SuggestedModes, Functor, Arity),
            ( fromto(begin, _, Continue, Arity),
              count(ArgNum, 1, _),
              param(ArgStore, Module, Functor, Arity, SuggestedModes, Verbose)
            do
                store_get(ArgStore, ArgNum, ArgModeCountStore),
	        (store_get(ArgModeCountStore, -, FreeVar) -> true; FreeVar = 0),
	        (store_get(ArgModeCountStore, *, AttrVar) -> true; AttrVar = 0),
	        (store_get(ArgModeCountStore, +, NonVar) -> true; NonVar = 0),
	        (store_get(ArgModeCountStore, ++, Ground) -> true; Ground = 0),
	        (store_get(ArgModeCountStore, ?, Unknown) -> true; Unknown = 0),
                ( (FreeVar = 0, AttrVar = 0, NonVar = 0, Ground = 0, Unknown = 0) ->
                    ( Verbose == on ->
                        printf("%tPredicate never invoked, analysis unavailable!%n", [])
                    ;
                        printf("%t%w never invoked, analysis unavailable!%n", 
                               [Module:Functor/Arity])
                    ),
                    Continue = Arity
                ;
                    ( Verbose == on ->
                        printf("%tResults for argument %w:%n", [ArgNum])
                    ;
                        true
                    ),
                    ( not Unknown = 0 ->
                        ( Verbose == on ->
                            printf("%t%t*** Warning: %w invocations with "
                                   "unsupported mode! ***%n", [Unknown])
                        ;
                            printf("%t*** Warning: %w invocations of %w with "
                                   "unsupported mode! ***%n", 
                                   [Unknown, Module:Functor/Arity])
                        ),
                        abort
                    ;
                        ( Verbose == on ->
                            printf("%t%t-: %w%t*: %w%t+: %w%t++: %w%n", 
                                   [FreeVar, AttrVar, NonVar, Ground])
                        ;
                            true
                        ),
                        suggest_argument_mode(FreeVar, AttrVar, 
                                              NonVar, Ground, SuggestedMode),
                        setarg(ArgNum, SuggestedModes, SuggestedMode)
                    ),
                    ( ArgNum = Arity ->
                        ( Verbose == on ->
                            printf("%n%t%w%n", [SuggestedModes])
                        ;
                            printf("%t%w%n", [SuggestedModes])
                        )
                    ;
                        true
                    ),
                    Continue = ArgNum
                )
            )
        ).


:- mode suggest_argument_mode(++, ++, ++, ++, -).
suggest_argument_mode(FreeVarCount, AttrVarCount, 
                      NonVarCount, GroundCount, SuggestedMode) :-
        ( ((FreeVarCount > 0, NonVarCount > 0) ;
           (FreeVarCount > 0, GroundCount > 0) ;
           (NonVarCount > 0, GroundCount > 0)  ;
           AttrVarCount > 0) -> 
            SuggestedMode = (?)
        ; FreeVarCount > 0 ->
            SuggestedMode = (-)
        ; NonVarCount > 0 ->
            SuggestedMode = (+)
        ; GroundCount > 0 ->
            SuggestedMode = (++)
        ;
            SuggestedMode = error
        ).

:- comment(reset/0,
        [ summary:"Reset the mode analysis results for predicates compiled 
   into the calling context module.",
          args:[],
          fail_if:no,
          resat:no,
          exceptions:[4 : "Calling context module is not instantiated"],
          desc: html("<P>
   This predicate erases all mode analysis data for every predicate that 
   has been compiled into the calling context module using the mode 
   analysis compiler.  The predicate is a tool and as such can be 
   invoked for an arbitrary module using <TT>mode_analyser:reset@Module</TT>."),
          eg: "
   [eclipse 6]: mode_analyser:reset@eclipse.

   Yes (0.00s cpu)",
          see_also:[analyse/1, result/0, result/1]
        ]).

:- tool(reset/0, reset/1).

reset(Module) :-
        store_delete(mode_analysis_store, Module).
