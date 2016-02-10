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
% Copyright (C) 2002 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK

% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Joachim Schimpf, IC-Parc, Imperial College
% Version:	$Id: coverage.ecl,v 1.5 2013/02/18 00:43:24 jschimpf Exp $
%
%
% NOTES
%	- (read) macro expansion should be on during preprocessing
%	  because the result is compile_term'd subsequently.
%	  During printing, the settings should be the same, but normally
%	  leaving macro expansion off will not cause a problem and lead
%	  to nicer looking output.
%
%	- DCGs are preprocessed after expansion. They could be done
%	  before when the preprocessor knows about dcg format.
%	  The printing must print whatever is being preprocessed.
%
% ----------------------------------------------------------------------

:- module(coverage).

:- comment(author,"Joachim Schimpf, based on ideas by Helmut Simonis").
:- comment(copyright,"Cisco Systems, Inc.").
:- comment(date,"$Date: 2013/02/18 00:43:24 $").
:- comment(categories, ["Development Tools"]).
:- comment(summary, "Tool for obtaining code coverage information").
:- comment(desc, html("<P>
	This is a tool for obtaining code coverage information, i.e.
	information about which points in the code were executed how
	often during a particular run of the program.
</P><P>
	The usage is as follows:
	<OL>
	<LI>Load the coverage library
	<PRE>
	?- lib(coverage).
	</PRE>
	<LI>Compile your program with the coverage compiler
	<PRE>
	?- coverage:ccompile(my_program).
	</PRE>
	<LI>Run the query which you want to examine
	<PRE>
	?- my_query(X,Y,Z).
	</PRE>
	<LI>Generate an html file containing the results. E.g. the following
	will create the result file coverage/my_program.html:
	<PRE>
	?- coverage:result(my_program).
	</PRE>
	<LI>View the result file using any browser. The result file
	contains a pretty-printed form of the source, annotated with
	the values of the code coverage counters.
	</OL>
</P><P>
	By default, code coverage counters are inserted before and after
	every subgoal in the code. For instance, in the clause
<PRE>
	p :- q, r, s.
</PRE>
	four counters would be inserted: before the call to q, between
	q and r, between r and s, and after s:
<PRE>
	p :- point(1), q, point(2), r, point(3), s, point(4).
</PRE>
	This is the most precise form provided. The counter values do not
	only show whether all code points were reached, they also show whether
	subgoals ever failed or aborted (in that case the counter before
	a subgoal will have a higher value than the counter after it).
	For example, the result of running the above code may look like:
<PRE>
	p :- <span style=\"color:black; font-weight:bold; background-color:limegreen\"> 43 </span> q, <span style=\"color:black; font-weight:bold; background-color:limegreen\"> 25 </span> r, <span style=\"color:black; font-weight:bold; background-color:limegreen\"> 25 </span> s <span style=\"color:black; font-weight:bold; background-color:red\"> 0 </span>.
</PRE>
	which would indicate that q was called 43 times, but succeeded
	only 25 times, r was called 25 times and succeeded always, and
	s was called 25 times and never succeeded.  Coverage counts of
	zero are displayed in red because they indicate unreached code.
</P><P>
	If one is only interested in knowing whether all code was covered,
	it is not necessary to have all these counters. point(1) and point(4)
	are enough to know whether q, r and s were successfully executed.
	The option <B>blocks_only</B> implements this: counters only
	get inserted at the beginning and at the end of conjunctions
	(comma-separated goal sequences), i.e. in the example:
<PRE>
	p :- point(1), q, r, s, point(4).
</PRE>
	For big programs, the presence of counters at the end of
	clauses can cause problems because they prevent tail-recursion
	optimization and may lead to stack overflows.  If that should
	be the case, exit-counters can be disabled by setting the
	<B>exit_counters</B> option to <B>off</B>, leading to the following
	incomplete instrumentation:
<PRE>
	p :- point(1), q, point(2), r, point(3), s.
</PRE>
</P><P>
	Note on the analysis of large, structured applications: 
	Larger applications often consist of several modules which get
	compiled implicitly through use_module/1 directives.  In this
	case, the module(s) that one wants to compile in coverage mode
	can be either compiled with ccompile/1 in advance (before
	loading the main application module), or afterwards (in which
	case ccompile/1 will replace the previously compiled normal
	code with coverage-code).
</P><P>
	Limitation: The current implementation cannot deal with multiple
	(non-module) files that are ccompiled into the same module.
</P>")).


:- lib(source_processor).
:- lib(module_options).
:- use_module(pretty_printer).


%----------------------------------------------------------------------
% Option handling
%----------------------------------------------------------------------

:- local struct(options(
    	exit_counters,
	blocks_only,
	macro_expansion,
	goal_expansion,
	verbose
	)).


% Skeleton option structure with defaults for the user-settable fields
default_options(options with [
    	exit_counters:on,
	blocks_only:off,
	macro_expansion:off,
	goal_expansion:on,
	verbose:off
    ]).

% User-settable option names and their structure index
valid_option_field(exit_counters,	exit_counters of options).
valid_option_field(blocks_only,		blocks_only of options).
valid_option_field(macro_expansion,	macro_expansion of options).
valid_option_field(goal_expansion,	goal_expansion of options).
valid_option_field(verbose,		verbose of options).

% Type checks for user-settable options
valid_option_value(exit_counters, OnOff) :- onoff(OnOff).
valid_option_value(blocks_only, OnOff) :- onoff(OnOff).
valid_option_value(macro_expansion, OnOff) :- onoff(OnOff).
valid_option_value(goal_expansion, OnOff) :- onoff(OnOff).
valid_option_value(verbose, OnOff) :- onoff(OnOff).

onoff(on).
onoff(off).


% We remember File and Module of every call to ccompile/1,2.
% We also remember the options that were used in ccompile
% because we have to use the same ones when printing the results.
:- local record(options_for_file).

remember_ccompile(CanonicalFile, Module, Options) :-
	( erase(options_for_file, file_options(CanonicalFile,_,_)) ->
	    true
	;
	    true
	),
	record(options_for_file, file_options(CanonicalFile,Module,Options)).
	
remembered_options(CanonicalFile, Options) :-
	recorded(options_for_file, file_options(CanonicalFile,_,Options)),
	!.

current_remembered_file(CanonicalFile, Module) :-
	recorded(options_for_file, file_options(CanonicalFile,Module,_)).

% Use an atomic, full pathname, with suffix, to make sure we don't get any
% unexpected failures when matching the filenames in the above predicates
robust_file_name(File, CanonicalFile) :-
	get_flag(prolog_suffix, Suffixes),
	once existing_file(File, Suffixes, [readable], File1),
	canonical_path_name(File1, File2),
	concat_atom([File2], CanonicalFile).


%----------------------------------------------------------------------
% Coverage counters
%
% For each source module, we have use a nonlogical 'store' to store
% the counters, using the hash table as a flexible array.
% Its name is the same as the module name.
% To prevent name clashes, no other named stores
% must be used inside this library module!
%----------------------------------------------------------------------

:- comment(reset_counters/0, [
    summary:"Reset all the coverage counters to zero",
    args:[],
    see_also:[ccompile/1,result/1],
    desc:html("
    The system maintains code coverage counters for all code compiled
    with coverage:ccompile/1. These counters get incremented when this
    code is run, and get reset implicitly when the results are output
    using coverage:result/1.
    ")
]).

:- export reset_counters/0.
reset_counters :-
	(
	    current_coverage_module(Name),
	    reset_counters(Name),
	    fail
	;
	    true
	).
	

% test or enumerate all modules with coverage counters
current_coverage_module(Name) :-
	current_store(Name).


% reset counters for module Name
reset_counters(Name) :-
	( current_store(Name) ->
	    ( for(I,0,store_count(Name)-1), param(Name) do
		store_set(Name, I, 0)
	    )
	;
	    true	% succeed silently
	).


:- export print_counters/0.
print_counters :-
	(
	    current_coverage_module(Name),
	    stored_keys_and_values(Name, KeysVals),
	    member(I-Count, KeysVals),
	    functor(Point, Name, 1),
	    arg(1, Point, I),
	    writeln(Point = Count),
	    fail
	;
	    true
	).


% make or recreate a counter array
make_counter_array(Name) :-
	(
	    current_coverage_module(Name)
	->
	    store_erase(Name)
	;
	    local(store(Name))
	).


% check whether things got out of sync between ccompile and result printing
check_counter_array(Name, Size) :-
	( current_store(Name) ->
	    store_count(Name, RealSize),
	    ( Size == RealSize ->
		true
	    ;
		printf(warning_output,
		    "Coverage counters out of sync - recompile module '%w'!%n",
		    Name)
	    )
	;
	    true	% error message was done earlier in get_counter
	).


% The actual counting predicate that gets inserted by the preprocessor
:- export point/1.
:- tool(point/1, point/2).
point(I, Module) :-
	store_inc(Module, I).


% initialise a new point counter
new_point(PointCount0, PointCount, Module) :-
	PointCount is PointCount0 + 1,
	store_set(Module, PointCount0, 0).


/*
:- export portray(point/1, portray_point/3, []).
:- export portray_point/3.
portray_point(point(I), {Count}, Module) :-
	get_counter(Module, I, Count).
*/


% Retrieve a counter value
get_counter(Module, I, Count) :-
	( current_store(Module) ->
	    ( store_get(Module, I, Count) ->
		true
	    ;
		% error will be printed in check_counter_array later
	    	Count = 0
	    )
	; I = 0 ->
	    printf(warning_output,
	    	"No coverage counters for module '%w' - ccompile and run first!%n",
		Module)
	;
	    Count = 0	% be silent, we already printed the error earlier
	).


%----------------------------------------------------------------------
% File-to-memory compiler with coverage preprocessing
%----------------------------------------------------------------------

:- export ccompile/1.
:- comment(ccompile/1, [
    summary:"Compile a file, inserting code coverage counters",
    args:["File":"Atom or string"],
    amode:ccompile(+),
    see_also:[ccompile/2,library(coverage), result/1, result/2],
    desc:html("
    This is a variant of the ECLiPSe compiler that inserts code coverage
    counters into the compiled code. This code can then be run, and the
    results analysed by printing them using coverage:result/1.
    ")
]).

:- tool(ccompile/1, ccompile_body/2).
ccompile_body(File, Module) :-
	process_file(File, Module, compile, []).


:- export ccompile/2.
:- comment(ccompile/2, [
    summary:"Compile a file, inserting code coverage counters",
    args:["File":"Atom or string", "OptionList":"List of Name:Value pairs"],
    amode:ccompile(+,+),
    see_also:[ccompile/1,library(coverage), result/1, result/2],
    desc:html("<P>
    This is a variant of the ECLiPSe compiler that inserts code coverage
    counters into the compiled code. This code can then be run, and the
    results analysed by printing them using coverage:result/1.
</P><P>
    Options to modify the behaviour of the code coverage library are:
<DL>
    <DT>exit_counters (default:on)<DD>
    	Setting this to 'off' will suppress coverage counters at the end of
	conjunctions (comma-sequences of subgoals).
    <DT>blocks_only (default:off)<DD>
    	Setting this to 'on' will leave only coverage counters at the
	beginning and at the end of conjunctions (comma-sequences of
	subgoals) and suppress all others.
    <DT>macro_expansion (default:off)<DD>
	This options affects only the printing of the result (html) file.
	By default, read-macros are not expanded in this output.  In rare
	cases, where macro expansion would affect the placement of coverage
	counter positions, it may be necessary to set this option to 'on'
	in order to display the counter values at the correct positions
	in the code.
    <DT>goal_expansion (default:on)<DD>
    	Setting this to 'off' will suppress goal expansion (inlining)
	during compilation. This may be necessary when the processed
	code contains predicates that get executed at compile time.
    <DT>verbose (default:off)<DD>
    	If set to 'on', the coverage preprocessor will print predicate
	names as they are processed (to log_output).
</DL>
</P>
    ")
]).

:- tool(ccompile/2, ccompile_body/3).
ccompile_body(File, OptionList, Module) :-
	process_file(File, Module, compile, OptionList).


%----------------------------------------------------------------------
% Result pretty-printer
%----------------------------------------------------------------------

:- export result/1.
:- comment(result/1, [
    summary:"Pretty-print a file, including any code coverage results",
    args:["File":"Atom or string"],
    amode:result(+),
    see_also:[result/2,library(coverage), library(pretty_printer), ccompile/1],
    desc:html("<P>
    This will pretty-print the given source file, annotated with the
    values of code coverage counters. The resulting .html file will be
    placed in a sub-directory called 'coverage', relative to File.
    See result/2 for options to modify the output.
</P><P>
    This predicate only makes sense if File has previously been compiled
    in coverage mode (using coverage:ccompile/1,2), and the code has been
    run in order to obtain values for the coverage counters.
</P>
    ")
]).

:- tool(result/1, result_body/2).
result_body(File, Module) :-
	process_file(File, Module, print, []).


:- export result/2.
:- comment(result/2, [
    summary:"Pretty-print a file, including any code coverage results",
    args:["File":"Atom or string", "OptionList":"List of Name:Value pairs"],
    amode:result(+,+),
    see_also:[result/1, pretty_printer:pretty_print/2, ccompile/1,
    	library(coverage), library(pretty_printer)],
    desc:html("<P>
    This will pretty-print the given source file, annotated with the
    values of code coverage counters. By default, the resulting .html
    file will be placed in a sub-directory called 'coverage', relative
    to File.
</P><P>
    OptionList is a list of options identical to the one accepted by
    pretty_print/2 in the library(pretty_printer), and can be used to
    modify the output style and the location of the output file.
</P><P>
    This predicate only makes sense if File has previously been compiled
    in coverage mode (using coverage:ccompile/1,2), and the code has been
    run in order to obtain values for the coverage counters.
    ")
]).

:- tool(result/2, result_body/3).
result_body(File, OptionList, Module) :-
	process_file(File, Module, print, OptionList).


:- export result/0.
:- comment(result/0, [
    summary:"Pretty-print all files with code coverage results",
    args:[],
    see_also:[result/1,result/2,library(coverage),library(pretty_printer),ccompile/1],
    desc:html("<P>
    This will pretty-print all source files that have been previously
    compiled with ccompile/1,2. For details see result/1.
</P>
    ")
]).

result :-
	current_remembered_file(File, Module),
	result_body(File, Module),
	fail.
result.


%----------------------------------------------------------------------
% Common code, runs in 'compile' or 'print' mode
% In compile mode, OptionList is coverage options.
% In print mode, OptionList is pretty_printer options.
%----------------------------------------------------------------------

process_file(File, Module, Mode, OptionList) :-
	robust_file_name(File, FileKey),
	( Mode = compile ->
	    get_options(OptionList, Options),		% coverage options
	    remember_ccompile(FileKey, Module, Options),
	    ReadOptions = [recreate_modules]
	; % Mode = print ->
	    remembered_options(FileKey, Options),	% coverage options
	    ( Options = options with macro_expansion:on ->
		% for rare cases of macro_expansion affecting counters
		ReadOptions = [keep_comments]
	    ;
		% default, macros not expanded for nicer printing
		ReadOptions = [keep_comments,no_macro_expansion]
	    )
	),
	cputime(StartTime),
	statistics(shared_heap_used, StartMem),
	error(146, File, Module),	% start of compilation event
	source_open(File, ReadOptions, SourcePos0)@Module,
	( Mode = compile ->
	    true
	;
	    % interpret OptionList as PrettyPrintOptions
	    ( memberchk(outdir:_, OptionList) ->
	    	PrettyPrintOptionList = OptionList
	    ;
	    	PrettyPrintOptionList = [outdir:coverage|OptionList]
	    ),
	    pretty_print_open(File, PrettyPrintOptionList, Listing0)
	),
	start_of_module(Mode, Module, PointCountInit),
	(
	    fromto(begin, _, Class, end),
	    fromto(SourcePos0, SourcePos1, SourcePos2, SourcePosEnd),
	    fromto(Listing0, Listing1, Listing2, Listing),
	    fromto(ClauseTail, Clauses0, Clauses1, []),
	    fromto(ClauseTail, ClauseTail0, ClauseTail1, []),
	    fromto(PointCountInit, PointCount0, PointCount2, _),
	    fromto(none, Pred0, Pred1, none),
	    param(Mode,Options)
	do
	    source_read(SourcePos1, SourcePos2, Class, SourceTerm),
	    arg(module of source_position, SourcePos1, PosModule),
	    arg(term of source_term, SourceTerm, Term),

	    ( Class = clause ->

		extract_pred(Term, N, A),
		Pred1 = PosModule:N/A,
		preprocess_clause(Term, TermPP, PointCount0, PointCount2, Mode, Options, PosModule),
%		writeclause(TermPP),
		( Mode = compile ->
		    ( Pred1 = Pred0 ->		% new clause for same pred
			ClauseTail0 = [TermPP|ClauseTail1],
			Clauses1 = Clauses0
		    ;
			ClauseTail0 = [],		% new pred, compile previous
			compile_predicate(Pred0, Clauses0, Options),
			Clauses1 = [TermPP|ClauseTail1]
		    )
		; % Mode = print ->
		    update_struct(source_term, [term:TermPP], SourceTerm, SourceTermPP),
		    pretty_print_term(Class, SourceTermPP, SourcePos1, Listing1, Listing2)
		)

	    ; Class = comment ->		% comment, ignore
		( Mode = compile ->
		    true
		;
		    pretty_print_term(Class, SourceTerm, SourcePos1, Listing1, Listing2)
		),
		Pred1 = Pred0,
		ClauseTail1 = ClauseTail0,
		PointCount2 = PointCount0,
		Clauses1 = Clauses0

	    ; % other classes are taken as predicate separator
		ClauseTail0 = [],		% compile previous predicate
		Clauses1 = ClauseTail1,
		Pred1 = none,

		( Mode = compile ->
		    compile_predicate(Pred0, Clauses0, Options)
		;
		    pretty_print_term(Class, SourceTerm, SourcePos1, Listing1, Listing2)
		),

		( Class = directive ->
		    PointCount2 = PointCount0,
		    call_directive(Mode, SourcePos1, Term, PosModule)
		; Class = query ->
		    PointCount2 = PointCount0,
		    call_directive(Mode, SourcePos1, Term, PosModule)
		; Class = handled_directive ->
		    arg(module of source_position, SourcePos2, PosModule2),
		    ( PosModule2 == PosModule ->
			PointCount2 = PointCount0
		    ;
			/* module-changing directive encountered */
			end_of_module(Mode, PosModule, PointCount0),
			start_of_module(Mode, PosModule2, PointCount2)
		    )
		; Class = var ->
		    PointCount2 = PointCount0,
		    compiler_error(4, SourcePos1, SourceTerm)
		; Class = end ->
		    PointCount2 = 0,
		    end_of_module(Mode, PosModule, PointCount0)
		;
		    PointCount2 = PointCount0
		)
	    )
	),
	( Mode = compile ->
	    Time is cputime-StartTime,
	    Size is statistics(shared_heap_used) - StartMem,
	    arg(file of source_position, SourcePos0, CanonicalFile),
	    concat_atom([CanonicalFile], CanonicalFileA),
	    error(166, CanonicalFileA-(coverage:ccompile(CanonicalFileA)), Module),
	    error(139, (CanonicalFileA,Size,Time), Module)
	;
	    pretty_print_close(Listing)
	),
	source_close(SourcePosEnd, [keep_modules]).


    compile_predicate(_, [], _) :- !.
    compile_predicate(Pred, Clauses,
	    options with [verbose:Verbose,goal_expansion:GoalExpansion]) :-
	( Verbose = on ->
	    writeln(log_output, ccompiling(Pred))
	;
	    true
	),
	Pred = M:_,
	( GoalExpansion=on, get_flag(goal_expansion, on) ->
	    (
		foreach(Clause,Clauses),
		foreach(ExpandedClause,ExpandedClauses),
		param(M)
	    do
		( Clause = (Head:-Body) ->
		    expand_goal(Body, ExpandedBody)@M,
		    ExpandedClause = (Head:-ExpandedBody)
		;
		    ExpandedClause = Clause
		)
	    )
	;
	    ExpandedClauses = Clauses
	),
    	compile_term(ExpandedClauses)@M.


    extract_pred((Head :- _), N, A) :- !,
    	functor(Head, N, A).
    extract_pred((Head ?- _), N, A) :- !,	% shouldn't occur
    	functor(Head, N, A).
    extract_pred((Head if _), N, A) :- !,	% shouldn't occur
    	functor(Head, N, A).
    extract_pred(Fact, N, A) :-
    	functor(Fact, N, A).


    call_directive(compile, source_position with [file:F,line:L], Dir, Module) :-
	arg(1, Dir, Goal),
    	block(
	    ( call(Goal)@Module ->
	    	true
	    ;
		printf(error, "Compiler: query failed in file %w, line %d.%n", [F,L])
	    ),
	    Tag,
	    printf(error, "Compiler: query exited (%w) in file %w, line %d.%n", [Tag, F,L])
	).
    call_directive(print, _, _, _).


    compiler_error(N, source_position with [file:F,line:L],
    		source_term with [term:Term]) :-
	error_id(N, Message),
	printf(error, "Compiler: %w in file %w, line %d:%n%Qw%n",
		[Message,F,L,Term]).


    end_of_module(_Mode, _Name, 0) :- !.
    end_of_module(compile, Name, Size) :- !,
	error(149, end_of_file, Name),	% end-of-compilation-unit event
	printf(log_output,
	    "coverage: inserted %d coverage counters into module %w%n",
	    [Size, Name]).
    end_of_module(print, Name, Size) :-
	check_counter_array(Name, Size),
	reset_counters(Name).

    start_of_module(compile, Module, 0) :-
    	make_counter_array(Module).
    start_of_module(print, _Module, 0).


% ----------------------------------------------------------------------
% preprocess_clause
%	Mode is either 'compile' or 'print'
%	Position is call/exit/both/none (used to suppress some counters)
%	PointCount is the numbering of the coverage points
% ----------------------------------------------------------------------


% -?-> is treated specially because nothing must be inserted before the -?->

preprocess_clause((Head :- -?-> Body), NewClause, PointCount0, PointCount, Mode, Options, Module) ?- !,
	NewClause = (Head :- -?-> NewBody),
	preprocess_body(Body, NewBody, PointCount0, PointCount, Mode, Options, both, Module).
preprocess_clause((Head :- Body), NewClause, PointCount0, PointCount, Mode, Options, Module) ?- !,
	NewClause = (Head :- NewBody),
	preprocess_body(Body, NewBody, PointCount0, PointCount, Mode, Options, both, Module).
preprocess_clause(Fact, NewClause, PointCount0, PointCount, Mode, _Options, Module) :-
	( Mode = compile ->
	    new_point(PointCount0, PointCount, Module),
	    NewClause = (Fact :- coverage:point(PointCount0))
	;
	    PointCount is PointCount0 + 1,
	    get_counter(Module, PointCount0, Count),
	    NewClause = 'coverage:exit'(Count,Fact)
	).


% (G1->G2;G3) is treated specially because nothing must be wrapped aound the ->

:- mode preprocess_body(?,-,+,-,+,+,+,+).
preprocess_body(Goal, NewGoal, PointCount0, PointCount, Mode, Options, Position, Module) :-
	var(Goal),
	!,
	profiler_point(Mode, Options, Position, PointCount0, PointCount, Goal, NewGoal, Module).
preprocess_body((G1,G2), (NewG1,NewG2), PointCount0, PointCount, Mode, Options, Position, Module) :- !,
	sibling_position(Options, Position, Position1, Position2),
	preprocess_body(G1, NewG1, PointCount0, PointCount1, Mode, Options, Position1, Module),
	preprocess_body(G2, NewG2, PointCount1, PointCount, Mode, Options, Position2, Module).
preprocess_body((G1->G2;G3), NewGoal, PointCount0, PointCount, Mode, Options, Position, Module) :- !,
	profiler_point(Mode, Options, Position, PointCount0, PointCount1, (NewG1->NewG2;NewG3), NewGoal, Module),
	preprocess_body(G1, NewG1, PointCount1, PointCount2, Mode, Options, none, Module),
	preprocess_body(G2, NewG2, PointCount2, PointCount3, Mode, Options, both, Module),
	preprocess_body(G3, NewG3, PointCount3, PointCount, Mode, Options, both, Module).
preprocess_body(Goal, NewGoal, PointCount0, PointCount, Mode, Options, Position, Module) :-
	profiler_point(Mode, Options, Position, PointCount0, PointCount1, ProcessedGoal, NewGoal, Module),
	functor(Goal, F, N),
	functor(Pattern, F, N),
	( meta_predicate_pattern(Pattern, Module) ->
	    % some of Goal's arguments may need to be preprocessed themselves
	    functor(ProcessedGoal, F, N),
	    (
	        for(I,1,N),
		fromto(PointCount1, PointCount2, PointCount3, PointCount),
		param(Goal,Pattern,ProcessedGoal,Mode,Options,Module)
	    do
		arg(I, Goal, Arg),
	        arg(I, Pattern, ArgSpec),
		arg(I, ProcessedGoal, NewArg),
		( meta_arg_position(ArgSpec, ArgPosition) ->
		    preprocess_body(Arg, NewArg, PointCount2, PointCount3, Mode, Options, ArgPosition, Module)
		;
		    NewArg = Arg,
		    PointCount3 = PointCount2
		)
	    )
	;
	    % simple goal, just inserting PP in front is enough
	    ProcessedGoal = Goal,
	    PointCount = PointCount1
	).


% replace the Goal, dependent on compile/print mode and goal position
profiler_point(Mode, Options, Position, PointCount0, PointCount, Goal, NewGoal, Module) :-
	( Options = options with [exit_counters:off] ->
	    no_exit_counters(Position, SimplifiedPosition)
	;
	    Position = SimplifiedPosition
	),
	profiler_point1(Mode, SimplifiedPosition, PointCount0, PointCount, Goal, NewGoal, Module).


profiler_point1(_Mode, none, PointCount, PointCount, Goal, Goal, _Module) :- !.
profiler_point1(compile, call, PointCount0, PointCount, Goal,
		(coverage:point(PointCount0),Goal), Module) :- !,
	new_point(PointCount0, PointCount, Module).
profiler_point1(print, call, PointCount0, PointCount, Goal,
		'coverage:call'(Count,Goal), Module) :- !,
	PointCount is PointCount0+1,
	get_counter(Module, PointCount0, Count).
profiler_point1(compile, exit, PointCount0, PointCount, Goal,
		(Goal,coverage:point(PointCount0)), Module) :- !,
	new_point(PointCount0, PointCount, Module).
profiler_point1(print, exit, PointCount0, PointCount, Goal,
		'coverage:exit'(Count,Goal), Module) :- !,
	PointCount is PointCount0+1,
	get_counter(Module, PointCount0, Count).
profiler_point1(compile, both, PointCount0, PointCount, Goal,
		(coverage:point(PointCount0),Goal,coverage:point(PointCount1)), Module) :- !,
	new_point(PointCount0, PointCount1, Module),
	new_point(PointCount1, PointCount, Module).
profiler_point1(print, both, PointCount0, PointCount, Goal,
		'coverage:call_exit'(CallCount,Goal,ExitCount), Module) :- !,
	PointCount1 is PointCount0+1,
	get_counter(Module, PointCount0, CallCount),
	PointCount is PointCount1+1,
	get_counter(Module, PointCount1, ExitCount).


% change the position code to supress exit counters
:- mode no_exit_counters(+,-).
no_exit_counters(both, call).
no_exit_counters(call, call).
no_exit_counters(exit, none).
no_exit_counters(none, none).


% given the position code for (A,B), return position codes for A and B
sibling_position(options with [blocks_only:off], PosAB, PosA, PosB) :- !,
	sibling_position(PosAB, PosA, PosB).
sibling_position(_, PosAB, PosA, PosB) :-
	sibling_position_simple(PosAB, PosA, PosB).

    % given the position code for (A,B), return position codes for A and B
    :- mode sibling_position(+,-,-).
    sibling_position(both, call, both).
    sibling_position(call, call, call).
    sibling_position(exit, none, both).
    sibling_position(none, none, call).

    % same as above, but call counters for B are suppressed
    :- mode sibling_position_simple(+,-,-).
    sibling_position_simple(both, call, exit).
    sibling_position_simple(call, call, none).
    sibling_position_simple(exit, none, exit).
    sibling_position_simple(none, none, none).


% map the character used in the meta_predicate spec to a "position"
:- mode meta_arg_position(+,-).
meta_arg_position(0, both).
meta_arg_position(u, exit).
meta_arg_position(e, call).
meta_arg_position(s, none).


% Wrapper around meta_predicate/2 for more precise information,
% enables us to suppress unnecessary counter insertions.
% u = unconditional call, e = subgoal exit is goal exit, s = u+e
meta_predicate_pattern( ^(*,s)		, _) :- !.
meta_predicate_pattern( -?->(s)		, _) :- !.
meta_predicate_pattern( @(s,*)		, _) :- !.
meta_predicate_pattern( :(*,s)		, _) :- !.
%meta_predicate_pattern( ','(u,u)	, _) :- !. % handled directly
meta_predicate_pattern( ;(u,0)		, _) :- !.
meta_predicate_pattern( ->(s,0)		, _) :- !. % when standalone
meta_predicate_pattern( *->(s,0)	, _) :- !.
meta_predicate_pattern( \+(u)		, _) :- !.
meta_predicate_pattern( ~(u)		, _) :- !.
meta_predicate_pattern( block(u,*,0)	, _) :- !.
meta_predicate_pattern( call(s)		, _) :- !.
meta_predicate_pattern( call(s,*)	, _) :- !.
meta_predicate_pattern( call_priority(s,*), _) :- !.
meta_predicate_pattern( catch(u,*,0)	, _) :- !.
meta_predicate_pattern( mutex(*,s)	, _) :- !.
meta_predicate_pattern( not(u)		, _) :- !.
meta_predicate_pattern( once(s)		, _) :- !.
meta_predicate_pattern( subcall(s,*)	, _) :- !.
meta_predicate_pattern(Pattern, Module) :-
	functor(Pattern, F, N),
	get_flag(F/N, meta_predicate, Pattern)@Module.

