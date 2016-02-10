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
% Description:  Instrumentation / sampling based profiling library
%
% System:       ECLiPSe Constraint Logic Programming System
% Author/s:     Andrew Cheadle, IC-Parc
% Version:      $Id: instprofile.ecl,v 1.2 2009/07/16 09:11:27 jschimpf Exp $
%
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
% Module definition, library dependencies and predicate import / export
% ----------------------------------------------------------------------

:- module(instprofile).

:- comment(categories, ["Development Tools"]).
:- comment(summary, "Instrumentation / sampling based statistics profiler").
:- comment(author,"Andrew Cheadle").
:- comment(date, "$Date: 2009/07/16 09:11:27 $").
:- comment(status, prototype).

:- comment(desc, html("<P>
   The instprofile library provides two complimentary mechanisms for
   the profiling of ECLiPSe code with the metrics provided by the
   statistics/2 predicate:
   <OL>
   <LI>Instrumentation based, where metric measuring code is
   inserted directly into the user program in order to accurately
   determine how its constituent pieces of code perform. The
   mechanism for the insertion of instrumentation code is based upon 
   the facilities of the instrument library. As such, the statistic 
   measurements may be inserted at the beginning and end of clause or block 
   definitions, and around subgoals and/or predicate calls.  
   <LI>Sample driven, where metric measuring code is executed at a user
   defined interval as the user program runs. The sample based profiler is 
   implemented using after events and is therefore less intrusive in its
   effect on the performance of the executing code. 
   </OL>
   </P><P>
   While the performance overhead of the sampling profiler is lower, it 
   cannot be used to determine the exact cost of a piece of code for a 
   particular statistic. It merely provides snapshots with respect to time 
   not code location. The instrumentation based profiler does however
   provide the means for the measurement of both the aggregated cost for 
   all executions of a piece of code and also the cost for each individual 
   execution.
   </P><P>
   The usage of the instrumentation based profiler is as follows:
   <OL>
   <LI>Load the instprofile library
   <PRE>
   ?- lib(instprofile).
   </PRE>
   <LI>Compile your program with instrumentation profiling:
   <PRE>
   ?- instprofile:statprofile(my_program, [global_stack_used,
                              trail_stack_used]).
   </PRE>
   <LI>Run the query for which you wish to generate mode analysis data
   <PRE>
   ?- my_query(X,Y,Z).
   </PRE>
   <LI>Generate the results for the module into which the program
       was compiled.
   <PRE>
   ?- instprofile:result@my_program_module.
   </PRE>
   </OL>
   The usage of the sampling profiler is as follows:
   <OL>
   <LI>Load the instprofile library
   <PRE>
   ?- lib(instprofile).
   </PRE>
   <LI>Define a sampling profile
   <PRE>
   ?- instprofile:statsample(memory, 5, [global_stack_used,
                             trail_stack_used], 'memory_sample.dat').
   </PRE>
   <LI>Prior to running the queries for which you wish to gather 
   sampling data, enable the sampling profiler for the named profile
   <PRE>
   ?- instprofile:statsample_control(memory, on)
   </PRE>
   <LI>Run the query for which you wish to generate sampling data
   <PRE>
   ?- my_query(X,Y,Z).
   </PRE>
   <LI>Disable the sampling profiler for the named profile. The results
   file specified in statsample/4 contains the sampling data.    
   <PRE>
   ?- instprofile:statsample_control(memory, off)
   </PRE>
   </OL>
   </P><P>
   Limitations to be resolved shortly: 
   <OL>
   <LI><P>
   Results are currently produced per module, not for individual files
   within a module (i.e. results must be retrieved for all files within 
   a module).
   </P>
   <LI><P>When multiple statistics are specified, the order in which the
   statistics will appear in the results (pretty-printed source and results
   file) is displayed during the invocation of the statsample predicate,
   not the result predicate.
   </P>
   <LI><P>Results are produced to a file or annotated on pretty-printed
   source and these results to be viewed offline. Additional facilities 
   are to be provided that allow for the real time visualisation of results.
   </P>
   <LI><P>The model that accounts for the cost of inserted instrumentation is 
   fairly rudimentary (especially for the 'global_stack_used' statistic.
   </P>
   <LI>A future enhancement will enable the insertion of user defined metrics
   not just those available via statistics/2.
   </OL>
   </P>")).

:- lib(instrument).

:- export statprofile/2, statprofile/3, result/0, aggregate_result/1,
          reset/0, statsample/4, statsample_control/2, open_delta_file/1,
          close_delta_file/0, profile_result/5.


% ----------------------------------------------------------------------
% Global variables and data structures
% ----------------------------------------------------------------------

% Value appended to instrumentation profiling predicate to 
% produce a unique predicate name
:- local variable(predCnt), 
         initialization((PredCnt = 0, setval(predCnt, PredCnt))).

% Store for defined sampling profiles
:- local store(sample_profiles).

% Delta value file stream used by the instrument profiler
:- local variable(delta_file).


% ----------------------------------------------------------------------
% Top-level 'statprofile' predicate
% ----------------------------------------------------------------------

:- comment(statprofile/2,
        [ summary:"Instrument a file with statistics profiling code.",
          args:["File":"Atom or string", "Statistics":"List of statistic keywords"],
          amode:statprofile(+, +),
          resat:no,
          fail_if:"Statistics is empty or contains an invalid statistic/2 keyword.",
          exceptions:[5 : "File is not an atom or string"],
          see_also:[statprofile/3, statistics/2, result/0,
                    aggregate_result/1, reset/0, open_delta_file/1, 
                    close_delta_file/0, library(instprofile), library(instrument)],
          desc:html("<P>    
   The predicate inserts into the user code of File, instrumentation 
   for the collection of the statistics (statistics/2 metric keywords) 
   specified in Statistics. The instrumentation is inserted at clause 
   entry and exit points (i.e. the start, end, fail and redo ports of 
   the conceptual predicate box model) and aggregates the cost for each 
   of the metrics as the program executes. The source code can be annotated 
   with these aggregated results using the result/0 predicate. Additionally, 
   the results may be dumped to a file in a format suitable for offline 
   analysis and graphing.
   </P><P>
   statprofile/3 should be used for instrumentation profiling of code at a
   finer granularity than the clause definition. statprofile/3 provides 
   options to prevent the profiling of recursive predicates. It also, 
   provides an option to produce the results for a single execution of a 
   profiled piece of code, in addition to the aggregated results.
   </P><P>
   The instrumentation profiler places an overhead on the execution of the 
   code undergoing profiling due to the insertion of the profiling code.
   The sample based profiler is implemented using after events and is
   therefore less intrusive in its effect on the performance of the 
   executing code than the instrumentation profiler. However, it is used to
   indicate the trend of resource usage over time of a running program not to
   attribute specific costs of a statistic to a specific piece of code. The
   instrumentation based profiler is used to do this.
   </P>"),
          eg: "
   [eclipse 1]: instprofile:statprofile(queen, [global_stack_used, trail_stack_used]).

   Yes (0.06s cpu)
   "]).

:- tool(statprofile/2, statprofile_body/3).
statprofile_body(File, Statistics, Module) :-
        statprofile_body(File, Statistics, [], Module).

:- comment(statprofile/3,
        [ summary:"Instrument a file with statistics profiling code.",
          args:["File":"Atom or string", 
                "Statistics":"List of statistic keywords", 
                "OptionsList":"List of Name:Value pairs"
                ],
          amode:statprofile(+, +, +),
          resat:no,
          fail_if:"Statistics is empty or contains an invalid statistic/2 keyword.",
          exceptions:[5 : "File is not an atom or string."],
          see_also:[open_delta_file/1, close_delta_file/0, result/0, 
                    aggregate_result/1, reset/0, statprofile/2, statistics/2, 
                    library(instprofile), library(instrument), instrument/3],
          desc:html("<P>    
   The predicate inserts into the user code of File, instrumentation 
   for the collection of the statistics (statistics/2 metric keywords) 
   specified in Statistics. The instrumentation is inserted by default 
   at clause entry and exit points (i.e. the start, end, fail and redo 
   ports of the conceptual predicate box model) and aggregates the cost 
   for each of the metrics as the program executes. The source code can 
   be annotated with these aggregated results using the result/0 predicate. 
   Additionally, the results may be dumped to a file in a format suitable 
   for offline analysis and graphing.
   </P><P>
   OptionList may contain the following options:
   <DL><DT>
   <TT>instrument_style</TT>  (default:<TT>clause</TT>)
   <DD>
   Valid values for this option are <TT>clause</TT>, <TT>block</TT>, 
   <TT>subgoal</TT> and <TT>call</TT>. These values correspond to the
   instrument insertion points of instrument library. Each provides a
   different level of granularity for the profiling of code fragments.
   <DT>
   <TT>delta_results</TT> (default:<TT>off</TT>)
   <DD>
   Specifying this option as <TT>on</TT> indicates that the instrumentation
   results for execution of individual profiled code fragments (associated by
   instrument's callsite identifier) should be written to the file specified 
   using open_delta_file/1. More specifically, these are the delta results,
   the difference for each of the statistics between the start and end of the
   profiled code fragment's execution.
   <DT>
   Instrument library options
   <DD>
   In addition to the above options, options valid to the instrument
   library can also be passed in OptionsList. The most useful of which are 
   <TT>verbose</TT> and <TT>instrument_recursive</TT>. 
   <TT>instrument_recursive</TT> is used to prevent the incorrect aggregation
   of results accumulated during the execution of recursive predicates, by
   default it is off, i.e. recursive predicates will not be instrumented.
   </DL>
   </P><P>
   The instrumentation profiler places an overhead on the execution of the 
   code undergoing profiling due to the insertion of the profiling code.
   The sample based profiler is implemented using after events and is
   therefore less intrusive in its effect on the performance of the 
   executing code than the instrumentation profiler. However, it is used to
   indicate the trend of resource usage over time of a running program not to
   attribute specific costs of a statistic to a specific piece of code. The
   instrumentation based profiler is used to do this.
   </P>"),
          eg: "
   [eclipse 1]: instprofile:statprofile(queen,[global_stack_used, trail_stack_used], 
                                        [instrument_style:block, delta_results:on]).

   Yes (0.06s cpu)
   "]).

:- tool(statprofile/3, statprofile_body/4).
statprofile_body(File, [], OptionsList, Module) :- 
        !,
        parse_options(OptionsList, 0, _, InstrumentOptions, _),
        instrument(File, InstrumentOptions)@Module.
statprofile_body(File, Statistics, OptionsList, Module) :-
        getval(predCnt, ProfilePredCnt),
        NewProfilePredCnt is ProfilePredCnt + 1,
        setval(predCnt, NewProfilePredCnt),
        parse_options(OptionsList, ProfilePredCnt, 
                      DeltaResults, InstrumentOptions, Template),
        % Can't just generate the code in a single pass,
        % want to do the mem stats first and emit the garbage_collect
        % in the right place... So build up a list of mem and remaining
        % stats.
        validate_statistics(Statistics, MemStatList, RemStatList),
        append(MemStatList, RemStatList, OrderedStatList),
        printf("%n*** Results will appear in the following order ***"
               "%n%t%w%n%n",
               [OrderedStatList]),
        open(string("profile_start"), update, StatStartCompileStream),
        open(string("profile_end"), update, StatEndCompileStream),
        NumMemStats is length(MemStatList),
        NumStats is NumMemStats + length(RemStatList),
        % If we're doing mem profiling can't have the dynamic policy
        % kicking in!
        printf(StatStartCompileStream, ":- export profile_start%d/1.\n"
               ":- tool(profile_start%d/1, profile_start_body%d/2).\n",
               [ProfilePredCnt, ProfilePredCnt, ProfilePredCnt]),
        printf(StatEndCompileStream, ":- export profile_end%d/1.\n"
               ":- tool(profile_end%d/1, profile_end_body%d/2).\n",
               [ProfilePredCnt, ProfilePredCnt, ProfilePredCnt]),
        printf(StatStartCompileStream, 
               "profile_start_body%d(SiteId, Module) :- ", [ProfilePredCnt]),
        printf(StatEndCompileStream, 
               "profile_end_body%d(SiteId, Module) :- ", [ProfilePredCnt]),
        ( NumMemStats > 0 ->
            set_flag(gc_policy, fixed),
            set_flag(gc_interval, 999999999),
            write(StatStartCompileStream, "garbage_collect, "),
            write(StatEndCompileStream, "garbage_collect, ")
        ;
            true
        ),
        (DeltaResults == on ->
            getval(delta_file, DeltaStream),
            write(StatEndCompileStream, "getval(delta_file, RDeltaStream),")
        ;
            true
        ),
        write(StatEndCompileStream, "instrument:get_callsite_data(SiteId,"
              "(Delta, Aggregate))@Module,"),
        ( Statistics = [SingleStat] ->
            write(StatStartCompileStream, "instrument:get_callsite_data(SiteId,"
                  "(_, Aggregate))@Module,"),
            printf(StatStartCompileStream, 
                   "statistics(%q, StatVar), "
                   "instrument:set_callsite_data(SiteId, "
                   "(StatVar, Aggregate))@Module, fail.\n", [SingleStat]),
            (  NumMemStats > 0 ->
                ( nonvar(DeltaStream) ->
                    printf(DeltaStream, "Callsite%t%t%t%w%n", SingleStat),
                    printf(StatEndCompileStream, 
                           "TDeltaVal is Delta - statistics(%q),"
                           "abs(TDeltaVal, DeltaVal),"
                           "printf(RDeltaStream, \"%%q%%t%%t%%t%%q%%n\", [SiteId, DeltaVal]),"
                           "NewAggregate is Aggregate + DeltaVal/1000, "
                           "instrument:set_callsite_data(SiteId, "
                           "(0.0, NewAggregate))@Module, fail.\n", [SingleStat])
                ;
                    printf(StatEndCompileStream, 
                           "TDeltaVal is Delta - statistics(%q),"
                           "abs(TDeltaVal, DeltaVal),"
                           "NewAggregate is Aggregate + DeltaVal/1000,"
                           "instrument:set_callsite_data(SiteId, "
                           "(0.0, NewAggregate))@Module, fail.\n", [SingleStat])
                )
            ;
                ( nonvar(DeltaStream) ->
                    printf(StatEndCompileStream, 
                           "TDeltaVal is Delta - statistics(%q),"
                           "abs(TDeltaVal, DeltaVal),"
                           "printf(RDeltaStream, \"%%q%%t%%t%%t%%q%%n\", [SiteId, DeltaVal]),"
                           "NewAggregate is Aggregate + DeltaVal,"
                           "instrument:set_callsite_data(SiteId, "
                           "(0.0, NewAggregate))@Module, fail.\n", [SingleStat])
                ;
                    printf(StatEndCompileStream, 
                           "TDeltaVal is Delta - statistics(%q),"
                           "abs(TDeltaVal, DeltaVal),"
                           "NewAggregate is Aggregate + DeltaVal,"
                           "instrument:set_callsite_data(SiteId, "
                           "(0.0, NewAggregate))@Module, fail.\n", [SingleStat])
                )
            )
        ;
            write(StatStartCompileStream, "instrument:get_callsite_data(SiteId,"
                  "(Delta, Aggregate))@Module,"),
            ( nonvar(DeltaStream) ->
                printf(DeltaStream, "%Callsite%t", []),
                printf(StatEndCompileStream, 
                       "printf(RDeltaStream, \"%%q%%t\", [SiteId]),", [])
            ;
                true
            ),
            % Do memory stats, then remainder!
            ( foreach(MemStat, MemStatList),
              count(MemArgNum, 1, NumMemStats),
              param(StatStartCompileStream, 
                    StatEndCompileStream, DeltaStream, NumStats)
            do
                printf(StatStartCompileStream, "%q, setarg(%w, Delta, %q), ",
                       [statistics(MemStat,StatVar), MemArgNum, StatVar]),
                % If we're collecting delta values
                ( nonvar(DeltaStream) ->
                    ( MemArgNum = NumStats ->
                        printf(DeltaStream, "%w%n", MemStat),
                        printf(StatEndCompileStream, 
                               "arg(%w, Delta, %q), arg(%w, Aggregate, %q),"
                               "%q, %q, %q, setarg(%w, Aggregate, %q),"
                               "printf(RDeltaStream, \"%%q%%n\", [%q]),",
                               [MemArgNum, StartVal, MemArgNum, AggrVal,
                                TDeltaVal is StartVal - statistics(MemStat),
                                abs(TDeltaVal, DeltaVal),
                                NewAggrVal is AggrVal + (DeltaVal / 1000), MemArgNum,
                                NewAggrVal, DeltaVal])
                    ;
                        printf(DeltaStream, "%w%t", MemStat),
                        printf(StatEndCompileStream, 
                               "arg(%w, Delta, %q), arg(%w, Aggregate, %q),"
                               "%q, %q, %q, setarg(%w, Aggregate, %q),"
                               "printf(RDeltaStream, \"%%q%%t\", [%q]),",
                               [MemArgNum, StartVal, MemArgNum, AggrVal,
                                TDeltaVal is StartVal - statistics(MemStat),
                                abs(TDeltaVal, DeltaVal),
                                NewAggrVal is AggrVal + (DeltaVal / 1000), MemArgNum,
                                NewAggrVal, DeltaVal])
                    )
                ;
                    printf(StatEndCompileStream, 
                           "arg(%w, Delta, %q), arg(%w, Aggregate, %q),"
                           "%q, %q, %q, setarg(%w, Aggregate, %q),",
                           [MemArgNum, StartVal, MemArgNum, AggrVal,
                            TDeltaVal is StartVal - statistics(MemStat),
                            abs(TDeltaVal, DeltaVal),
                            NewAggrVal is AggrVal + (DeltaVal / 1000), MemArgNum,
                            NewAggrVal])
                )
            ),
            ( foreach(RemStat, RemStatList),
              count(RemArgNum, NumMemStats + 1, NumStats),
              param(StatStartCompileStream, 
                    StatEndCompileStream, DeltaStream, NumStats)
            do
                printf(StatStartCompileStream, "%q, setarg(%w, Delta, %q), ",
                       [statistics(RemStat,StatVar), RemArgNum, StatVar]),
                % If we're collecting delta values
                ( nonvar(DeltaStream) ->
                    ( RemArgNum = NumStats ->
                        printf(DeltaStream, "%w%n", RemStat),
                        printf(StatEndCompileStream, 
                               "arg(%w, Delta, %q), arg(%w, Aggregate, %q),"
                               "%q, %q, %q, setarg(%w, Aggregate, %q),"
                               "printf(RDeltaStream, \"%%q%%n\", [%q]),",
                               [RemArgNum, StartVal, RemArgNum, AggrVal,
                                TDeltaVal is StartVal - statistics(RemStat), 
                                abs(TDeltaVal, DeltaVal),
                                NewAggrVal is AggrVal + DeltaVal, RemArgNum,
                                NewAggrVal, DeltaVal])
                    ;
                        printf(DeltaStream, "%w%t", RemStat),
                        printf(StatEndCompileStream, 
                               "arg(%w, Delta, %q), arg(%w, Aggregate, %q),"
                               "%q, %q, %q, setarg(%w, Aggregate, %q),"
                               "printf(RDeltaStream, \"%%q%%t\", [%q]),",
                               [RemArgNum, StartVal, RemArgNum, AggrVal,
                                TDeltaVal is StartVal - statistics(RemStat), 
                                abs(TDeltaVal, DeltaVal),
                                NewAggrVal is AggrVal + DeltaVal, RemArgNum,
                                NewAggrVal, DeltaVal])
                    )
                ;
                    printf(StatEndCompileStream, 
                           "arg(%w, Delta, %q), arg(%w, Aggregate, %q),"
                           "%q, %q, %q, setarg(%w, Aggregate, %q),",
                           [RemArgNum, StartVal, RemArgNum, AggrVal,
                            TDeltaVal is StartVal - statistics(RemStat), 
                            abs(TDeltaVal, DeltaVal),
                            NewAggrVal is AggrVal + DeltaVal, RemArgNum,
                            NewAggrVal])
                )
            ),
            SetStmt = "instrument:set_callsite_data(SiteId, "
                      "(Delta, Aggregate))@Module, fail.\n",
            write(StatStartCompileStream, SetStmt),
            write(StatEndCompileStream, SetStmt)
        ),
        printf(StatStartCompileStream, "profile_start_body%w(_, _).\n",
               [ProfilePredCnt]),
        printf(StatEndCompileStream, "profile_end_body%w(_, _).\n",
               [ProfilePredCnt]),
/* Debugging!
        seek(StatStartCompileStream,0),
        ( fromto(none, _, Term, end_of_file),
          param(StatStartCompileStream)
        do
            read(StatStartCompileStream, Term),
            writeclause(Term)
        ),
        seek(StatEndCompileStream,0),
        ( fromto(none, _, Term, end_of_file),
          param(StatEndCompileStream)
        do
            read(StatEndCompileStream, Term),
            writeclause(Term)
        ),
*/
        seek(StatStartCompileStream,0),
        seek(StatEndCompileStream,0),
        compile_stream(StatStartCompileStream),
        compile_stream(StatEndCompileStream),
        instrument(File, Template, InstrumentOptions)@Module,
        close(StatStartCompileStream),
        close(StatEndCompileStream),
        defined_modules(File, DefinedModules),
        ( foreach(DefinedModule, DefinedModules),
          param(File, NumStats)
        do
            file_callsites(File, StartId, EndId)@DefinedModule,
            % Initialise callsite data
            ( for(SiteId, StartId, EndId), 
              param(DefinedModule, NumStats) do
                ( NumStats = 1 ->
                    set_callsite_data(SiteId, (0.0, 0.0))@DefinedModule
                ;
                    integer_atom(SiteId, SiteIdAtom),
                    functor(SiteValStruct, SiteIdAtom, NumStats),
                    ( foreacharg(ArgA, SiteValStruct)
                    do
                        ArgA = 0.0
                    ),
                    set_callsite_data(SiteId, (SiteValStruct, SiteValStruct))@DefinedModule
                )
            )
        ).


parse_options([], ProfilePredCnt, _, [], Template) :-
        !,
        concat_atom([profile_start, ProfilePredCnt], ProfileStartPred),
        concat_atom([profile_end, ProfilePredCnt], ProfileEndPred),
        Template = itemplate with [clause_start:(instprofile:ProfileStartPred/1),
                                   clause_redo:(instprofile:ProfileStartPred/1),
                                   clause_end:(instprofile:ProfileEndPred/1),
                                   clause_fail:(instprofile:ProfileEndPred/1),
                                   result:(instprofile:profile_result/5),
                                   module_scope:every_module].
parse_options(OptionsList, ProfilePredCnt, 
              DeltaResults, InstrumentOptions, Template) :-
        ( foreach(Option, OptionsList), 
          fromto(InstrumentOptions, InstOut, InstIn, []),
          param(InstFields, DeltaResults)
        do
            ( Option = instrument_style:Style ->
                instrument_style_option(Style, InstFields),
                InstOut = InstIn
            ; Option = delta_results:DeltaResults ->
                valid_on_off_option(DeltaResults),
                InstOut = InstIn
            ;
                InstOut = [Option | InstIn]
            )
        ),
        ( var(InstFields) -> instrument_style_option(clause, InstFields) ; true ),
        ( var(DeltaResults) -> DeltaResults = off ; true ),
        concat_atom([profile_start, ProfilePredCnt], ProfileStartPred),
        concat_atom([profile_end, ProfilePredCnt], ProfileEndPred),
        Template = itemplate with [result:(instprofile:profile_result/5),
                                      module_scope:every_module],
        ( foreach(Field, InstFields), 
          foreach(Var, [Start, End, Fail, Redo])
        do
            Var = Field
        ),
        setarg(Start, Template, (instprofile:ProfileStartPred/1)),
        setarg(End, Template, (instprofile:ProfileEndPred/1)),
        setarg(Fail, Template, (instprofile:ProfileEndPred/1)),
        setarg(Redo, Template, (instprofile:ProfileStartPred/1)).


instrument_style_option(clause, Fields) :- !,
        Fields = [clause_start of itemplate, clause_end of itemplate,
                  clause_fail of itemplate, clause_redo of itemplate].
instrument_style_option(block, Fields) :- !,
        Fields = [block_start of itemplate, block_end of itemplate,
                  block_fail of itemplate, block_redo of itemplate].
instrument_style_option(subgoal, Fields) :- !,
        Fields = [subgoal_start of itemplate, subgoal_end of itemplate,
                  subgoal_fail of itemplate, subgoal_redo of itemplate].
instrument_style_option(call, Fields) :- !,
        Fields = [call_start of itemplate, call_end of itemplate,
                  call_fail of itemplate, call_redo of itemplate].


valid_on_off_option(Error) :-
        var(Error), !,
        error(4, valid_on_off_option(Error)).
valid_on_off_option(on)  :- !.
valid_on_off_option(off) :- !.
valid_on_off_option(Error) :-
        error(5, valid_on_off_option(Error)).


% ----------------------------------------------------------------------
% Delta value file handling predicates
% ----------------------------------------------------------------------

:- comment(open_delta_file/1,
        [ summary:"Open the instrumentation profiler's delta results file.",
          args:["DeltaFile":"Atom or string"],
          amode:open_delta_file(+),
          resat:no,
          fail_if:no,
          exceptions:[4 : "File is not instantiated",
                      5 : "File is not an atom or string"],
          see_also:[close_delta_file/0, statprofile/3, 
                    statistics/2, library(instprofile)],
          desc:html("<P>    
   Open the file that the instrumentation profiler writes delta values to.
   If a file has been previously opened using open_delta_file/1 it is shut
   before the specified file is opened. 
   </P><P>
   Delta values are the instrumentation profiling results for the single
   execution (as opposed to aggregated) of a profiled code fragment.
   </P><P>
   NOTE: Memory related statistics are displayed in bytes.
   </P><P>
   For performance reasons, the results are written to the file using 
   buffered I/O - each result is not flushed to the file as it is emitted. 
   As a result to ensure all results are flushed to the disk file,
   <TT>close_delta_file</TT> must be executed.
   </P>")]).

open_delta_file(DeltaFile) :-
        getval(delta_file, PrevDeltaStream),
        ( var(PrevDeltaStream) -> 
            true 
        ; 
            % Just in case we fail before setting a valid stream
            setval(delta_file, _),
            close(PrevDeltaStream)
        ),
        canonical_path_name(DeltaFile, CanonicalDeltaFile),
        open(CanonicalDeltaFile, write, DeltaStream),
        setval(delta_file, DeltaStream).


:- comment(close_delta_file/0,
        [ summary:"Close the instrumentation profiler's delta results file.",
          args:[],
          resat:no,
          fail_if:no,
          see_also:[open_delta_file/1, statprofile/3, 
                    statistics/2, library(instprofile)],
          desc:html("<P>    
   Close the file that the instrumentation profiler writes delta values to.
   The file must previously have been opened using open_delta_file/1. 
   </P><P>
   Delta values are the instrumentation profiling results for the single
   execution (as opposed to aggregated) of a profiled code fragment.
   </P><P>
   For performance reasons, the results are written to the file using 
   buffered I/O - each result is not flushed to the file as it is emitted. 
   As a result to ensure all results are flushed to the disk file,
   <TT>close_delta_file</TT> must be executed.
   </P>")]).

close_delta_file :-
        getval(delta_file, DeltaStream),
        ( var(DeltaStream) -> 
            true 
        ; 
            % Just in case we fail before setting a valid stream
            setval(delta_file, _),
            close(DeltaStream)
        ).


% ----------------------------------------------------------------------
% Top-level 'result' predicate
% ----------------------------------------------------------------------

:- comment(result/0,
        [ summary:"Pretty-print the instrument profiled source with
   aggregated results.",
          args:[],
          resat:no,
          fail_if:"The calling context module has not been profiled.",
          see_also:[statprofile/2, statprofile/3, aggregate_result/1,
                    open_delta_file/1, close_delta_file/0, reset/0, 
                    statistics/2, library(instprofile)],
          desc:html("<P>    
   This will pretty-print all source files in the calling module context,
   annotated with the aggregated results of instrumentation profiling.  
   The resulting .html files are placed in a sub-directory called
   'instprofile', relative to the files that were compiled into the file.
   </P><P>
   NOTE: Memory related statistics are displayed in kilobytes not bytes.
   </P><P>
   The use of this predicate is only meaningful if the module has
   previously been compiled for instrumentation profiling using 
   <TT>instprofile:statprofile/2,3</TT>, and the code has been
   run in order to obtain profiling results.
   </P><P>
   The predicate is a tool and the profiling results of a
   module other than the current calling module context can be obtained
   by invoking <TT>result@Module</TT>.
   </P>")]).

:- tool(result/0, result_body/1).
result_body(Module) :-
        module_result([outdir:instprofile])@Module,
        reset_body(Module).


% ----------------------------------------------------------------------
% 'aggregate_result' predicate
% ----------------------------------------------------------------------

:- comment(aggregate_result/1,
        [ summary:"Write the aggregated instrument profiling results to
   a named file.",
          args:["File":"Atom or string"],
          amode:aggregate_result(+),
          resat:no,
          fail_if:"The calling context module has not been profiled.",
          exceptions:[4 : "File is not instantiated",
                      5 : "File is not an atom or string"],
          see_also:[statprofile/2, statprofile/3, aggregate_result/1,
                    open_delta_file/1, close_delta_file/0, reset/0, 
                    statistics/2, library(instprofile)],
          desc:html("<P>    
   This will write the aggregated results of instrumentation profiling 
   for all source files that have been compiled into the calling module 
   context and profiled with the instrument profiler.
   </P><P>
   NOTE: Memory related statistics are displayed in kilobytes not bytes.
   </P><P>
   This predicate should be executed before result/0 predicate, since
   result/0 also invokes reset/0 which resets the statistics.
   </P><P>
   The use of this predicate is only meaningful if the module has
   previously been compiled for instrumentation profiling using 
   <TT>instprofile:statprofile/2,3</TT>, and the code has been
   run in order to obtain profiling results.
   </P><P>
   The predicate is a tool and the profiling results of a
   module other than the current calling module context can be obtained
   by invoking <TT>aggregate_result@Module</TT>.
   </P>")]).

:- tool(aggregate_result/1, aggregate_result/2).

aggregate_result(File, Module) :-
        canonical_path_name(File, CanonicalFile),
        open(CanonicalFile, write, Stream),
        module_callsites(StartId, EndId)@Module,
        ( for(CallSite, StartId, EndId), 
          param(Module, Stream) do
            ( instrument:get_callsite_data(CallSite, (_, End))@Module ->
                printf(Stream, "%w%t", [CallSite]),
                % Single statistics isn't a structure with the callsite as
                % the functor, however, multiple stats are
                ( float(End) ->
                    printf(Stream, "%w%t", [End])
                ;
                    ( foreacharg(Arg, End),
                      param(Stream)
                    do
                        printf(Stream, "%w%t", [Arg])
                    )
                ),
                printf(Stream, "%n", [])
            ;
                % Should never get here!
                printf(Stream, "%w%t0%n", [CallSite])
            )
        ),
        close(Stream).


% ----------------------------------------------------------------------
% 'reset' predicate
% ----------------------------------------------------------------------

:- comment(reset/0,
        [ summary:"Reset the profiling statistics for the calling context
   module.",
          args:[],
          resat:no,
          fail_if:"The calling context module has not been profiled.",
          see_also:[statprofile/2, statprofile/3, open_delta_file/1,
                    close_delta_file/0, result/0,  aggregate_result/1,
                    statistics/2, library(instprofile)],
          desc:html("<P>    
   This will reset the currently stored aggregated profiling results for 
   the calling context module.
   </P><P>
   The use of this predicate is only meaningful if the module has
   previously been compiled for instrumentation profiling using 
   <TT>instprofile:statprofile/2,3</TT>, and the code has been
   run in order to obtain profiling results.
   </P><P>
   The predicate is a tool and the profiling results of a
   module other than the current calling module context can be obtained
   by invoking <TT>reset@Module</TT>.
   </P>")]).

:- tool(reset/0, reset_body/1).

reset_body(Module) :-
        % Probably got to do on a per file basis!
        module_callsites(StartId, EndId)@Module,
        ( for(SiteId, StartId, EndId), 
          param(Module) do
            get_callsite_data(SiteId, (_, Aggregate))@Module,
            ( float(Aggregate) ->
                set_callsite_data(SiteId, (0.0, 0.0))@Module
            ;
                functor(Aggregate, _, Arity),
                integer_atom(SiteId, SiteIdAtom),
                functor(SiteValStruct, SiteIdAtom, Arity),
                ( foreacharg(ArgA, SiteValStruct)
                do
                    ArgA = 0.0
                ),
                set_callsite_data(SiteId, (SiteValStruct, SiteValStruct))@Module
            )
        ).


% ----------------------------------------------------------------------
% The predicate used for determining the pretty printing of the results
% ----------------------------------------------------------------------

:- comment(profile_result/5, hidden).

% This is all a quick hack i'm afraid
profile_result(CallSite, InstLocation, Module, Goal, NewGoal) :-
        atom_string(InstLocation, InstLocationStr),
        ( substring(InstLocationStr, "start", _) ->
            ( instrument:get_callsite_data(CallSite, (_, End))@Module ->
                % Single statistics isn't a structure with the callsite as
                % the functor, however, multiple stats are
                ( float(End) ->
                    integer_atom(CallSite, CallSiteAtom),
                    Result =.. [CallSiteAtom, End],
                    NewGoal = 'coverage:exit'(Result, Goal)
                ;
                    NewGoal = 'coverage:exit'(End, Goal)
                )
            ;
                % Should never get here!
                integer_atom(CallSite, CallSiteAtom),
                Result =.. [CallSiteAtom, 0],
                NewGoal = 'coverage:exit'(Result, Goal)
            )
        ;
            NewGoal = Goal
        ).


% ----------------------------------------------------------------------
% Top-level 'statsample' predicate
% ----------------------------------------------------------------------

:- comment(statsample/4,
        [ summary:"Create a statistics sampling profile.",
          args:["ProfileName":"Atom or string", "SamplePeriod":"Number",
                "Statistics":"List of statistic keywords", "File":"Atom or string"],
          amode:statsample(+, +, +, +),
          resat:no,
          fail_if:"Statistics is empty or contains an invalid statistic/2 keyword.",
          exceptions:[5 : "File is not an atom or string"],
          see_also:[statsample_control/2, statistics/2, library(instprofile)],
          desc:html("<P>    
   The predicate defines a profile, ProfileName, for use by the sampling
   profiler. Samples of the statistics (statistics/2 metric keywords)
   specified in Statistics are collected every SamplePeriod seconds and 
   stored in File. The format of file is such that it can easily be
   analysed or graphed offline.
   </P><P>
   The sample based profiler is implemented using after events and is
   therefore less intrusive in its effect on the performance of the 
   executing code than the instrumentation profiler. However, it is used to
   indicate the trend of resource usage over time of a running program not to
   attribute specific costs of a statistic to a specific piece of code. The
   instrumentation based profiler is used to do this.
   </P>"),
          eg: "
   [eclipse 1]: instprofile:statsample(memory, 5, [global_stack_used,
                                       trail_stack_used], 'memory_sample.dat').
   string     compiled traceable 476 bytes in 0.00 seconds

   Yes (0.01s cpu)
   "]).

:- tool(statsample/4, statsample/5).
statsample(_ProfileName, _SamplePeriod, [], _File, _Module) :-
        !,
        printf(error, "At least one statistic should be specified!%n", []),
        fail.
statsample(ProfileName, SamplePeriod, Statistics, File, Module) :-
        validate_statistics(Statistics, MemStatList, RemStatList),
        % Do memory stats, then remainder!
        append(MemStatList, RemStatList, OrderedStatList),
        length(OrderedStatList, NumStats),
        canonical_path_name(File, CanonicalFile),
        open(CanonicalFile, update, Stream),
        concat_atom([Module, ProfileName, '_stat'], ProfilePred),
        open(string("profile_sample"), update, CompileStream),
        printf(CompileStream, ":- export %w/0.\n", [ProfilePred]),
        printf(CompileStream, ":- untraceable %w/0.\n", [ProfilePred]),
        printf(CompileStream, "%w :- ", [ProfilePred]),
        printf(CompileStream, "store_get(sample_profiles, %w, (_, ResultStream)),%n", 
               [ProfileName]),
        ( foreach(Stat, OrderedStatList),
          for(StatNum, 1, NumStats),
          param(CompileStream, Stream, NumStats)
        do
            ( StatNum = NumStats ->
                printf(Stream, "%w%n", Stat),
                printf(CompileStream, "%q,%nwriteln(ResultStream, %q)%n,fail.%n",
                       [statistics(Stat, StatVar), StatVar])
            ;
                printf(Stream, "%w%t", Stat),
                printf(CompileStream, "%q,%nwrite(ResultStream, %q),%n"
                       "write(ResultStream, \"%t\"),%n", 
                       [statistics(Stat, StatVar), StatVar])
            )
        ),
        printf(CompileStream, "%w.\n", [ProfilePred]),
/* Debugging
        seek(CompileStream,0),
        ( fromto(none, _, Term, end_of_file),
          param(CompileStream)
        do
            read(CompileStream, Term),
            writeclause(Term)
        ),
*/
        seek(CompileStream,0),
        compile_stream(CompileStream),
        close(CompileStream),
        set_event_handler(ProfileName, ProfilePred/0),
        store_set(sample_profiles, ProfileName, (SamplePeriod, Stream)).


:- comment(statsample_control/2,
        [ summary:"Enable / disable the sampling profiler for a named profile.",
          args:["ProfileName":"Atom or string", "State":"Atoms 'on' or 'off'"],
          amode:statsample_control(+, +),
          resat:no,
          fail_if:"Profilename is not a valid profile defined by statsample/4."
   "State is neither the atom 'on' nor the atom 'off'.",
          see_also:[statsample/4, statistics/2, library(instprofile)],
          desc:html("<P>    
   For the named profile, ProfileName, enable or disable the sampling
   profiler based on the value of State. Setting State to 'on' immediately
   enables sample collection at the inteval and to to the file specified in
   statsample/4.
   </P><P>
   For performance reasons, the results are written to File (specified by
   statsample/4) using buffered I/O - each result is not flushed to disk as
   it is written. As a result to ensure all results are flushed to the file,
   <TT>statsample_control(ProfileName, off)</TT> must be executed before
   the results file can be analysed.
   </P>"),
          eg: "
   [eclipse 2]: instprofile:statsample_control(memory, on).

   Yes (0.00s cpu)
   [eclipse 3]: ...

   Yes (10.24s cpu)
   [eclipse 4]: instprofile:statsample_control(memory, off).

   Yes (0.00s cpu)
        "]).

statsample_control(ProfileName, on) :-
        !,
        store_get(sample_profiles, ProfileName, (SamplePeriod, _)),
        event_after_every(ProfileName, SamplePeriod).
statsample_control(ProfileName, off) :-
        !,
        store_get(sample_profiles, ProfileName, (_, Stream)),
        cancel_after_event(ProfileName, _),
        close(Stream).


validate_statistics(StatList, MemStatList, RemStatList) :-
        findall(Statistic, statistics(Statistic, _), ValidStatistics),
        ( foreach(Stat, StatList),
          fromto(MemStatList, MemStatOut, MemStatIn, []),
          fromto(RemStatList, RemStatOut, RemStatIn, []),
          param(ValidStatistics)
        do 
            ( memberchk(Stat, ValidStatistics) ->
                true
            ;
                printf(error, "'%w' is not a valid statistic!%n", [Stat]),
                fail
            ),
            atom_string(Stat, StatString),
            ( (substring(StatString, "stack", _); substring(StatString, "heap", _)) ->
                MemStatOut = [Stat|MemStatIn],
                RemStatOut = RemStatIn
            ;
                MemStatOut = MemStatIn,
                RemStatOut = [Stat|RemStatIn]
            )
        ).
