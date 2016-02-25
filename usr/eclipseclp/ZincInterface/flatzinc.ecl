%----------------------------------------------------------------------
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
% The Original Code is  The Zinc Modelling Tools for ECLiPSe
% The Initial Developer of the Original Code is  Joachim Schimpf
% with support from Cisco Systems and NICTA Victoria.
% Portions created by the Initial Developer are
% Copyright (C) 2007 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf
% 
% END LICENSE BLOCK
%----------------------------------------------------------------------

:- module(flatzinc).

:- comment(date, "$Date: 2015/01/14 01:31:09 $").
:- comment(categories, ["Interfacing","Constraints"]).
:- comment(summary, "Interpreter for FlatZinc").
:- comment(author, "Joachim Schimpf, supported by Cisco Systems and NICTA Victoria").
:- comment(copyright, "Cisco Systems Inc, licensed under CMPL").
:- comment(see_also, [
	library(minizinc),
	library(flatzinc_parser),
	library(fzn_ic),
	library(fzn_fd),
	library(fzn_eplex)
    ]).
:- comment(status, prototype).
:- comment(desc, html("
<H3>
Overview
</H3>
<P>
The core of this module is an interpreter for FlatZinc models, based
on 'Specification of FlatZinc 1.0' (May 2009).  It uses
lib(flatzinc_parser) to read a FlatZinc model one item at a time, and
immediately interprets it.  The mapping from FlatZinc built-in
operations to actual ECLiPSe solver operations is in separate modules
called fzn_ic, fzn_fd, fzn_eplex, etc.
</P>

<H3>
Running FlatZinc Models
</H3>
<P>
If you have a file containing a FlatZinc model, it can be loaded and
executed from ECLiPSE by calling
<PRE>
    ?- fzn_run(\"model.fzn\", fzn_ic).
</PRE>
where model.fzn is the file name (the .fzn extension can be omitted)
and fzn_ic is the name of the chosen solver mapping.  It is also
possible to read a model from the standard input using fzn_run/1, or
from an arbitrary ECLiPSe input stream using fzn_run_stream/2.
</P>
If finer control is needed, the processing of a FlatZinc model can be
split up into initialization, loading and constraint-set-up, search,
and output.  The primitives that perform these steps are exported
and can be invoked separately, e.g.
<PRE>
my_fzn_run_stream(ModelStream, Options) :-

	% initialize the solver state
	fzn_init(Options, State),

	% load the model and set up the constraints
	fzn_load_stream(ModelStream, State),

	% perform the search
	fzn_search(State),

	% output solution, if found
	fzn_output(State).
</PRE>
</P>

<H3>
Creating FlatZinc Models
</H3>
<P>
Note that FlatZinc is not intended to be written by humans, but
created by translating models written in Zinc or MiniZinc.  A
translator for MiniZinc to FlatZinc called mzn2fzn is available at
<A HREF=\"http://www.g12.csse.unimelb.edu.au/minizinc\">
http://www.g12.csse.unimelb.edu.au/minizinc</A>
</P>
<P>
The use of an intermediate FlatZinc file can be avoided by
piping the result of the MiniZinc to FlatZinc converter directly
into the ECLiPSe-FlatZinc interpreter, e.g. via
<PRE>
% mzn2fzn --output-to-stdout model.mzn | eclipse -e \"flatzinc:fzn_run(fzn_ic)\"
</PRE>
The file lib/fzn_ic/globals.mzn contains specialised global constraint
definitinions for the use of fzn_ic.
For alternative ways to run MiniZinc models, see library(minizinc).
</P>

<H3>
How to write a new solver mapping
</H3>
<P>
The mapping from FlatZinc built-in operations to actual ECLiPSe solver
operations is defined in separate modules called fzn_ic, fzn_eplex, etc. 
To add a new mapping, create a new module file called fzn_xxx.ecl, and
place it in your library_path.
These modules should export predicates corresponding to the \"built-in\"
operations defined by FlatZinc, i.e.
<CODE>
int_lin_le/3, float_times/3,
</CODE>
etc.  See the FlatZinc specification for a complete list.
</P><P>
In addition to those, we require the following interface predicates:
</P><P>
For initialising variables:
<CODE>
bool_declare(-var),
int_declare(-var),
int_declare(-var, +list),
int_declare(-var, +min, +max),
float_declare(-var),
float_declare(-var, +min, +max),
set_declare(-var, +min, +max),
set_declare(-var, +list)
</CODE>
</P><P>
For initialising arrays:
<CODE>
bool_declare_array(-array),
int_declare_array(-array),
int_declare_array(-array, +list),
int_declare_array(-array, +min, +max),
float_declare_array(-array),
float_declare_array(-array, +min, +max),
set_declare_array(-array, +min, +max),
set_declare_array(-array, +list)
</CODE>
</P><P>
For invoking search:
<CODE>
satisfy(+annotations),
minimize(+objective, +annotations, -cost),
maximize(+objective, +annotations, -cost)
</CODE>
</P><P>
For converting constants in the Zinc model to the appropriate solver
type in ECLiPSe (e.g. floats to breals when using lib(ic)):
<CODE>
bool_fzn_to_solver(+atom, -bool),
bool_solver_to_fzn(+bool, -atom),
float_fzn_to_solver(+float, -real),
float_solver_to_fzn(+real, -float),
set_fzn_to_solver(+list, -set),
set_solver_to_fzn(+set, -list),
range_fzn_to_solver(+min, +max, -set).
</CODE>
</P>
<P>
<H3>
TODO
</H3>
<UL>
<LI>interpret more variable annotations</LI>
<LI>constraint annotations (currently ignored)</LI>
<LI>stricter checking of the FlatZinc input?</LI>
</UL>
</P>
")).

:- lib(hash).

% The following are lazily loaded
%:- use_module(flatzinc_parser).
%:- use_module(flatzinc_syntax).

:- local initialization(
    	set_stream_property(warning_output, flush, end_of_line)).

% Used to find the mapping libraries relative to here
%:- getcwd(ZincLib),
%   get_flag(library_path, P),
%   set_flag(library_path, [ZincLib|P]).


%----------------------------------------------------------------------
% Data structures
% CAUTION: in this file we have two uses of the functor of/2:
% - standard ECLiPSe structure field access (e.g. "dict of state")
% - normal functor in FlatZinc array declaration terms ("array(1..5) of int")
% The latter must always be quoted using no_macro_expansion/1
%----------------------------------------------------------------------

:- export struct(zn_options(
	solver,			% name of mapping library/module, e.g. fzn_ic
	parser,			% atom: [strict,fast], default strict
	var_names,		% on/off: use lib(var_name) to attach MiniZinc names
	solutions,		% int: max number of solutions (0=all)
	fzn_tmp,		% file/pipe: how to handle intermediate .fzn
	setup_prio,		% 0..12 setup priority (0=current)
	output   		% output_stream
    )).

:- comment(struct(zn_options), [
    summary:"Options for Mini/FlatZinc solving",
    fields:[
    "solver":"
    	Determines which ECLiPSe solver(s) the FlatZinc variables and
	constraints get mapped to. The name is the name of a library
	implementing the mapping, e.g. fzn_ic, fzn_fd or fzn_eplex.
	The default is 'fzn_ic'.",
    "fzn_tmp":"
	One of the atoms 'file' or 'pipe', determining if intermediate
	FlatZinc code is piped from the generator to the interpreter,
	or passed through an intermediate file. Default is 'file'.",
    "output":"
        A stream (name or handle) to which the results are printed.
        The default is 'output'."
    "parser":"
	One of the atoms 'strict' or 'fast', giving the choice between
	a stricter, but slower dedicated FlatZinc parser (in
	lib(flatzinc_parser)), or a faster parsing method (which
	detects less errors because it uses the normal, but
	reconfigured ECLiPSe parser).  Since FlatZinc input is
	normally automatically generated, the default is to use the
	faster and more permissive method 'fast'.",
    "setup_prio":"
	The priority under which the constraint setup will be executed
	(see call_priority/2 and get_priority/1). Possible values are
	the ECLiPSe priorities 1 to 12, or 0 (the default) which stands
	for the current priority of the calling code.  A sensible value
	for this option is 2, which means that the setup code is executed
	under high priority (still allowing debug/visualisation goals).
	The effect of such a setting is that no propagation occurs until
	all constraints are fully set up, possibly leading to time savings.",
    "solutions":"
	The maximum number of solutions computed (and possibly output).
	Only effective if no minimization/maximization is done.
	Only effective for toplevel predicates like fzn_run/mzn_run
	that are deterministic and do not succeed once per solution.
	Default is 1, 'all' (or 0) means no limit.",
    "var_names":"
	If this option is set to 'on', the ECLiPSe variables representing
	Zinc variables will be marked with their Zinc names, using the
	facilities of lib(var_name).  This is mainly useful for debugging.
	The default is 'off' to save space and time."
    ],
    see_also:[fzn_run/1, fzn_run/2, fzn_run_stream/2, fzn_init/2],
    desc:html("<P>
	Used to specify options for Mini/FlatZinc solving.
    </P><P>
    	All predicates that accept the zn_options structure also accept
	the name of a solver mapping library alone. In this case, all
	other options take their default values.
    </P>"),
    eg:"
    ?- fzn_run(\"queens.fzn\", fzn_ic).
    ?- fzn_run(\"queens.fzn\", zn_options{solver:fzn_ic,solutions:3}).
    "
]).

:- export struct(zn_var(
	id,			% FZ name of variable/parameter
	ann,			% list of FZ annotations
	type,			% FZ type descriptor
	group,			% simplified type (int, arr_float, par, etc)
	eclvar,			% corresponding ECLiPSe variable/array
	num			% var id number in input order (0 for pars)
    )).

:- comment(struct(zn_var), [
    summary:"Descriptor for a Mini/FlatZinc variable",
    fields:[
	"id":"(atom) Mini/FlatZinc name",
	"ann":"List of variable annotations",
	"type":"FlatZinc type descriptor (structure)",
	"group":"simplified type descriptor (atom)",
	"eclvar":"Corresponding ECLiPSe variable",
	"num":"Variable number in input order"],
    see_also:[fzn_var_lookup/3]
]).

:- local struct(state(
	solver,			% name of mapping library/module, e.g. fzn_ic
	options,
	dict,			% hash table of all identifiers in the model
	id_count,		% identifier counter (to track input order)
	solve_goal,		% goal resulting from solve item
	output_elems,		% argument of output item (obsolete)
        output_stream,          % output stream
	init_time,		% time we started
	start_setup_time,	% time we started loading the model
	end_setup_time,		% time we finished loading the model
	start_search_time,	% time we started search
	end_search_time,	% time we finished search
	sol_cnt,		% solution countdown (shelf)
        sol_cnt_init,           % starting value for sol_cnt (from options)
	cost			% result of min/maximize, or 'none'
    )).


%----------------------------------------------------------------------
% Options
%----------------------------------------------------------------------

default_options(zn_options{solver:Solver,parser:Parser,var_names:VNames,
                    fzn_tmp:FznTmp,solutions:N,setup_prio:Prio,output:Out}) :-
	set_default(Solver, fzn_ic),
	set_default(N, 1),
	get_flag(version_as_list, Version),
	( Version @>= [5,10,110] ->
	    set_default(Parser, fast)
	;
	    writeln(warning_output,
	    	"WARNING: Use ECLiPSe >= 5.10_110 with lib(flatzinc) for faster parsing"),
	    set_default(Parser, strict)
	),
	set_default(Prio, 0),
	set_default(VNames, off),
	set_default(Out, output),
	% Use 'file' default because it is difficult to get both stdout
	% and stderr back from exec(mzn2fzn) simultaneously without blocking
	set_default(FznTmp, file).

    set_default(X, X) :- !.
    set_default(_, _).

    onoff(on) ?- true.
    onoff(off) ?- true.

valid_options(zn_options{solver:Solver,parser:Parser,var_names:VNames,
			fzn_tmp:FznTmp,solutions:N,setup_prio:Prio}) :-
	atom(Solver),
	(integer(N), N>=0 ; N==all),
	integer(Prio), Prio>=0, Prio=<12,
	(Parser==strict;Parser==fast),
	(FznTmp==pipe;FznTmp==file),
	onoff(VNames),
	!.
valid_options(Options) :-
	fzn_error("Invalid option: %w", [Options]).


%----------------------------------------------------------------------
% Top level predicates
%----------------------------------------------------------------------

:- export fzn_run/1.
:- comment(fzn_run/1, [
    summary:"Run a FlatZinc model from standard input",
    amode:(fzn_run(+) is det),
    args:[
	"SolverOrOptions":"Name of solver mapping module, or zn_options-structure"],
    see_also:[fzn_run/2, fzn_run_stream/2],
    desc:html("<P>
	Reads a FlatZinc model from the input stream, and interprets it using
	the solver mapping defined in SolverOrOptions.  At the end of
	solving, results are printed to the output stream, timing and
	progress messages are printed to the log_output stream, warnings
	to the warning_output stream, and error messages the error stream.
	This predicate always succeeds. It is equivalent to:
    <PRE>
    fzn_run(SolverOrOptions) :-
	fzn_run_stream(input, SolverOrOptions).
    </PRE>
    </P>"),
    eg:"
    % generate_model | eclipse -e \"flatzinc:fzn_run(fzn_ic)\"
"]).

fzn_run(SolverOrOptions) :-
	fzn_run_stream(input, SolverOrOptions).


:- export fzn_run/2.
:- comment(fzn_run/2, [
    summary:"Run a FlatZinc model from a given file",
    amode:(fzn_run(+,++) is det),
    args:["File":"File name (extension defaults to .fzn)",
	"SolverOrOptions":"Name of solver mapping module, or zn_options-structure"],
    see_also:[fzn_run/1, fzn_run_stream/2],
    desc:html("<P>
	Reads a FlatZinc model from a file, and interprets it using
	the solver mapping defined in SolverOrOptions.  At the end of
	solving, results are printed to the output stream, timing and
	progress messages are printed to the log_output stream, warnings
	to the warning_output stream, and error messages the error stream.
	This predicate always succeeds.
    </P>"),
    eg:"
    ?- fzn_run(\"mymodel.fzn\", fzn_ic).
	Found a solution with cost 10
	Found no solution with cost 7.0 .. 9.0
	end = 10
	b1 = 1
	b2 = 0
	b3 = 1
	b4 = 0
	Objective value = 10
	Total time 0.031s cpu (0.016 setup + 0.000 search)
"]).

fzn_run(File, SolverOrOptions) :-
	( existing_file(File, ["",".fzn"], [readable], FullFile) ->
	    printf(log_output, "%% Loading model %w%n", [FullFile]),
	    open(FullFile, read, Stream)
	;
	    fzn_error("No such file: %w", [File])
	),
	fzn_run_stream(Stream, SolverOrOptions).


:- export fzn_run_stream/2.
:- comment(fzn_run_stream/2, [
    summary:"Run a FlatZinc model from a given open input stream",
    amode:(fzn_run_stream(+,++) is det),
    args:["Stream":"ECLiPSe stream name or handle",
	"SolverOrOptions":"Name of solver mapping module, or zn_options-structure"],
    see_also:[fzn_run/1, fzn_run/2],
    desc:html("<P>
	Reads a FlatZinc model from a stream, and interprets it using
	the solver mapping defined in SolverOrOptions.  At the end of
	solving, results are printed to the output stream, timing and
	progress messages are printed to the log_output stream, warnings
	to the warning_output stream, and error messages the error stream.
	This predicate always succeeds and closes the stream.
    </P>"),
    eg:"
    ?- open(\"mymodel.fzn\",read,S), fzn_run_stream(S, fzn_ic).
	Found a solution with cost 10
	Found no solution with cost 7.0 .. 9.0
	end = 10
	b1 = 1
	b2 = 0
	b3 = 1
	b4 = 0
	Objective value = 10
	Total time 0.031s cpu (0.016 setup + 0.000 search)
"]).
fzn_run_stream(Stream, SolverOrOptions) :-
	fzn_init(SolverOrOptions, State),
        State = state{output_stream:Out},
	(
	    fzn_load_stream(Stream, State),
	    fzn_search(State),
	    fzn_output(State),
	    writeln(Out, ----------),
	    fzn_last(State),
	    !
	;
            ( fzn_unsat(State) ->
                writeln(Out, "=====UNSATISFIABLE=====")
            ;
                writeln(Out, ==========)
            )
	).


:- export fzn_search/1.
:- comment(fzn_search/1, [
    summary:"Run the search part of a FlatZinc model",
    amode:(fzn_search(+) is nondet),
    fail_if:"Fails if no (further) solution is found",
    args:["FznState":"FlatZinc state descriptor"],
    see_also:[fzn_init/2, fzn_load_stream/2, fzn_output/1],
    desc:html("<P>
	Perform the search part of a FlatZinc model, as specified in
	the model's solve-item.  The model must previously have been
	loaded successfully using fzn_load_xxx or mzn_load_xxx.
	If the solve item was 'satisfy', multiple solutions may be
	returned on backtracking.
    </P>"),
    eg:"
    my_fzn_run_stream(ModelStream, Options) :-
	fzn_init(Options, State),
	fzn_load_stream(ModelStream, State),
	fzn_search(State),
	fzn_output(State).
"]).
fzn_search(State) :-
	State = state{solver:Solver,solve_goal:SearchGoal},
	register_start_search(State),
	Solver:SearchGoal,
	register_end_search(State).


:- export zn_options/2.
zn_options(SolverOrOptions, Options) :-
	( atom(SolverOrOptions) ->
	    Solver = SolverOrOptions,
	    Options = zn_options{solver:Solver},
	    default_options(Options)
	; SolverOrOptions = zn_options{} ->
	    Options = SolverOrOptions,
	    default_options(Options),
	    valid_options(Options)
	;
	    fzn_error("Solver name or option structure expected: %w",
	    	[SolverOrOptions])
	).


:- export fzn_init/2.
:- comment(fzn_init/2, [
    summary:"Initialize a FlatZinc solver",
    amode:(fzn_init(++,-) is det),
    args:["SolverOrOptions":"Name of solver mapping module, or zn_options-structure",
	"FznState":"Output: a FlatZinc state descriptor"],
    see_also:[fzn_load_stream/2, fzn_search/1, fzn_output/1, struct(zn_options)],
    desc:html("<P>
	Initialize a FlatZinc solver and return a descriptor, ready
	for loading the model.  The given solver options are taken
	into account and remembered in the descriptor.
    </P>"),
    eg:"
    my_fzn_run_stream(ModelStream, Options) :-
	fzn_init(Options, State),
	fzn_load_stream(ModelStream, State),
	fzn_search(State),
	fzn_output(State).
"]).
fzn_init(SolverOrOptions,
    		state{solver:Solver,options:Options,dict:Dict,id_count:0,
		sol_cnt:SolCnt,sol_cnt_init:InitCnt,init_time:T0,
                output_stream:Out}) :-
	zn_options(SolverOrOptions, Options),
	Options = zn_options{solver:Solver,solutions:SolMax,output:Out},

	% solution counter
        ( SolMax == 0 -> InitCnt is all-1 ; InitCnt is eval(SolMax)-1 ),
	shelf_create(c(InitCnt), SolCnt),

	% load solver here, so it doesn't affect the timings
	ensure_loaded(library(Solver)),

	% init dictionary, make entries for false and true
	hash_create(Dict),
	Solver:bool_fzn_to_solver(false, False),
	hash_insert_id(Dict, false, zn_var{id:false,ann:[],type:bool,group:par,eclvar:False,num:0}),
	Solver:bool_fzn_to_solver(true, True),
	hash_insert_id(Dict, true, zn_var{id:true,ann:[],type:bool,group:par,eclvar:True,num:0}),

	cputime(T0).

    all(1000000000).	% default solution counter

    register_start_search(state{start_search_time:T,dict:_Dict}) :-
	writeln(log_output, "% Starting search"),
	cputime(T).

    register_end_search(state{end_search_time:T}) :-
	cputime(T).

    report_statistics(state{init_time:T0,
    		start_setup_time:T1,end_setup_time:T2,
		start_search_time:T3,end_search_time:T4}) :-
	cputime(T5),
	Tall is T5-T0,
	printf(log_output, "%% Total time %.3fs cpu ", [Tall]),
	( var(T2) ->
	    printf(log_output, "(failure)%n", [])
	;
	    Tsetup is T2-T1,
	    printf(log_output, "(%.3f setup", [Tsetup]),
	    ( var(T4) ->
		printf(log_output, ")%n", [])
	    ;
		Tsearch is T4-T3,
		printf(log_output, " + %.3f search)%n", [Tsearch])
	    )
	).


:- export fzn_load_stream/2.
:- comment(fzn_load_stream/2, [
    summary:"Load a FlatZinc model and set up its constraints",
    amode:(fzn_load_stream(+,-) is semidet),
    fail_if:"Fails if any constraint fails during setup",
    args:["ModelStream":"An ECLiPSe stream name or handle",
	"FznState":"a FlatZinc state descriptor"],
    see_also:[fzn_init/2, fzn_search/1, fzn_output/1, fzn_var_lookup/3, struct(zn_options)],
    desc:html("<P>
	Load a FlatZinc model and set up its constraints.  The options
	that were used in fzn_init/2 will be taken into account.
	If fzn_load_stream/2 succeeds, all constraints have been
	successfully set up, and the search phase can begin.
    </P><P>
    	This predicate will always close ModelStream on success,
	failure or abort.
    </P>"),
    eg:"
    my_fzn_run_stream(ModelStream, Options) :-
	fzn_init(Options, State),
	fzn_load_stream(ModelStream, State),
	fzn_search(State),
	fzn_output(State).
"]).
fzn_load_stream(Stream, State) :-
	State = state{options:zn_options{parser:Parser,setup_prio:Prio},start_setup_time:T0,end_setup_time:T1},
	cputime(T0),
	( block(fzn_load_stream1(Stream, State, Parser, Prio),
		Tag,
		report_abort(Stream, Tag))
	->
	    add_var_names(State),
	    cputime(T1),
	    close(Stream)
	;
	    get_stream_info(Stream, line, Line),
	    printf(log_output, "%% Failure during constraint setup, FlatZinc model line %w%n", [Line]),
	    close(Stream),
	    fail
	).

    fzn_load_stream1(Stream, State, Parser, 0) :- !,
	fzn_load_loop(Stream, State, Parser).
    fzn_load_stream1(Stream, State, Parser, Prio) :-
    	call_priority(fzn_load_loop(Stream, State, Parser), Prio).

    fzn_load_loop(Stream, State, Parser) :-
	( read_item(Stream, Item, Parser) ->

%	    get_stream_info(Stream, line, Line), printf("%d: ", [Line]), write_canonical(Item), nl,
	    interpret(Item, State),
	    fzn_load_loop(Stream, State, Parser)
	;
	    State = state{solve_goal:SolveGoal},
	    ( var(SolveGoal) ->
		fzn_error("Unexpected end of FlatZinc input", [])
	    ;
		true
	    )
	).

    read_item(Stream, State, strict) :-
    	flatzinc_parser:read_item(Stream, State).
    read_item(Stream, State, fast) :-
    	flatzinc_syntax:read_item(Stream, State).

    report_abort(Stream, Tag) :-
	get_stream_info(Stream, line, Line),
	printf(error, "Aborted in line %w%n", [Line]),
	close(Stream),
	exit_block(Tag).



%----------------------------------------------------------------------
% Item interpreter
%----------------------------------------------------------------------

interpret(annotation(Ann) , _State) :- !,
	printf(warning_output, "Annotation declaration ignored: %w%n", [Ann]).

interpret(Type:IdentAnns, State) :- !,
	detach_annotations(IdentAnns, Ident, Anns),
	check_annotations(Anns),
	( Type = no_macro_expansion(array([1..Max]) of ElemInstType) ->
	    % an uninitialised array
	    declare_array(Type, Max, ElemInstType, Ident, Anns, _Init, State)
	; Type = var(VarType) ->
	    State = state{dict:Dict},
	    new_varnum(State, N),
	    hash_insert_id(Dict, Ident,
		zn_var{id:Ident,ann:Anns,type:Type,group:Group,eclvar:EclVar,num:N}),
	    declare_var(VarType, EclVar, Ident, Anns, State, Group)
	;
	    fzn_error("Uninitialized parameter: %w", [Ident])
	).

interpret(Type:IdentAnns=Init, State) :- !,
	detach_annotations(IdentAnns, Ident, Anns),
	check_annotations(Anns),
	( Type = no_macro_expansion(array([1..Max]) of ElemInstType) ->
	   % initialised array-of-par, or partially initialised array-of-var
	   declare_array(Type, Max, ElemInstType, Ident, Anns, Init, State)
	; Type = var(VarType) ->
	    eval_expr(Init, State, EclVar),
	    State = state{dict:Dict},
	    new_varnum(State, N),
	    hash_insert_id(Dict, Ident,
		zn_var{id:Ident,ann:Anns,type:Type,group:Group,eclvar:EclVar,num:N}),
	    declare_var(VarType, EclVar, Ident, Anns, State, Group)
	;
	    % a simple parameter
	    eval_expr(Init, State, EclVar),
	    State = state{dict:Dict},
	    hash_insert_id(Dict, Ident,
		zn_var{id:Ident,ann:Anns,type:Type,group:par,eclvar:EclVar,num:0})
	).

interpret(constraint(ElemAnns), State) :- !,
	detach_annotations(ElemAnns, Constraint, Anns),
	check_annotations(Anns),
	eval_expr(Constraint, State, True),
	eval_expr(true, State, True).

interpret(satisfy(SolveAnns), State) :- !,
	State = state{solve_goal:satisfy(Anns),cost:none},
	detach_annotations(SolveAnns, _solve, UserAnns),
	add_default_anns(State, UserAnns, Anns).

interpret(minimize(SolveAnns,Expr), State) :- !,
	State = state{solve_goal:minimize(Obj, Anns, Cost),cost:Cost},
	detach_annotations(SolveAnns, _solve, UserAnns),
	add_default_anns(State, UserAnns, Anns),
	eval_lin_expr(Expr, State, Obj).

interpret(maximize(SolveAnns,Expr), State) :- !,
	State = state{solve_goal:maximize(Obj, Anns, Cost),cost:Cost},
	detach_annotations(SolveAnns, _solve, UserAnns),
	add_default_anns(State, UserAnns, Anns),
	eval_lin_expr(Expr, State, Obj).

interpret(output(Elems), State) :- !,
	State = state{output_elems:Elems}.

interpret(predicate(_Elems), _State) :- !.	% ignore for now


% Declarations ---------------------------------

declare_array(Type, Max, ElemInstType, Ident, Anns, Init, State) :-
	dim(EclVar, [Max]),
	State = state{dict:Dict},
	( ElemInstType = var(ElemType) ->
	    Group = InitGroup,
	    new_varnum(State, N)
	;
	    N = 0,
	    Group = par,
	    ElemType = ElemInstType
	),
	( var(Init) ->
	    declare_vars(ElemType, EclVar, Ident, Anns, State, InitGroup)
	;
	    (
		eval_expr(Init, State, EclVar),
		declare_vars(ElemType, EclVar, Ident, Anns, State, InitGroup)
	    ->
		true
	    ;
		fzn_error("Array initialization failed: %w", [Ident])
	    )
	),
	hash_insert_id(Dict, Ident,
		zn_var{id:Ident,ann:Anns,type:Type,group:Group,eclvar:EclVar,num:N}).


:- mode declare_var(++,?,+,+,+,-).
declare_var(bool, EclVar, _, _Anns, state{solver:Solver}, bool) :-
	Solver:bool_declare(EclVar).
declare_var(int, EclVar, _, _Anns, state{solver:Solver}, int) :-
	Solver:int_declare(EclVar).
declare_var(float, EclVar, _, _Anns, state{solver:Solver}, float) :-
	Solver:float_declare(EclVar).
declare_var(Min..Max, EclVar, _, _Anns, state{solver:Solver}, int) :-
	integer(Min), !,
	Solver:int_declare(EclVar, Min, Max).
declare_var(Min..Max, EclVar, _, _Anns, state{solver:Solver}, float) :-
	%float(Min),
	Solver:float_declare(EclVar, Min, Max).
declare_var({}(Elems), EclVar, _, _Anns, state{solver:Solver}, int) :-
	%is_list(Elems),
	Solver:int_declare(EclVar, Elems).
declare_var(no_macro_expansion(of(set,int)), EclVar, Ident, _Anns, _State, set) :- !,
	( var(EclVar) ->
	    fzn_error("Set of int not allowed: %w", [Ident])
	;
	    true
	).
declare_var(no_macro_expansion(of(set,Min..Max)), EclVar, _, _Anns, state{solver:Solver}, set) :- !,
	Solver:set_declare(EclVar, Min, Max).
declare_var(no_macro_expansion(of(set,Elems)), EclVar, _, _Anns, state{solver:Solver}, set) :-
	%is_list(Elems),
	Solver:set_declare(EclVar, Elems).


:- mode declare_vars(++,?,+,+,+,-).
declare_vars(bool, EclVars, _, _Anns, state{solver:Solver}, arr_bool) :-
	Solver:bool_declare_array(EclVars).
declare_vars(int, EclVars, _, _Anns, state{solver:Solver}, arr_int) :-
	Solver:int_declare_array(EclVars).
declare_vars(float, EclVars, _, _Anns, state{solver:Solver}, arr_float) :-
	Solver:float_declare_array(EclVars).
declare_vars(Min..Max, EclVars, _, _Anns, state{solver:Solver}, arr_int) :-
	integer(Min), !,
	Solver:int_declare_array(EclVars, Min, Max).
declare_vars(Min..Max, EclVars, _, _Anns, state{solver:Solver}, arr_float) :-
	%float(Min),
	Solver:float_declare_array(EclVars, Min, Max).
declare_vars({}(Elems), EclVars, _, _Anns, state{solver:Solver}, arr_int) :-
	%is_list(Elems),
	Solver:int_declare_array(EclVars, Elems).
declare_vars(no_macro_expansion(of(set,int)), EclVar, Ident, _Anns, _State, arr_set) :- !,
	( var(EclVar) ->
	    fzn_error("Set of int not allowed: %w", [Ident])
	;
	    true
	).
declare_vars(no_macro_expansion(of(set,Min..Max)), EclVars, _, _Anns, state{solver:Solver}, arr_set) :- !,
	Solver:set_declare_array(EclVars, Min, Max).
declare_vars(no_macro_expansion(of(set,Elems)), EclVars, _, _Anns, state{solver:Solver}, arr_set) :-
	%is_list(Elems),
	Solver:set_declare_array(EclVars, Elems).


% Convert FlatZinc term to ECLiPSe term --------------------------
% Arrays are represented as ECLiPSe arrays
% Bools, Floats and Sets are represented depending on solver
% CAUTION: Empty arrays are mapped to [], which may be the same
% as the representation of the empty set for some set solvers.

:- mode eval_expr(+,+,-).
eval_expr([], _State, EmptyArray) :- !,
	EmptyArray = [].
eval_expr({}, State, Set) :- !,
	State = state{solver:Solver},
	Solver:set_fzn_to_solver([], Set).
eval_expr(Ident, State, Result) :-
	atom(Ident), !,
	fzn_var_lookup(State, Ident, Result).	% takes care of bools
eval_expr(X, _State, Result) :-
	integer(X), !,
	Result = X.
eval_expr(X, State, Real) :-
	float(X), !,
	State = state{solver:Solver},
	Solver:float_fzn_to_solver(X, Real).
eval_expr(X, _State, Result) :-
	string(X), !,
	Result = X.
eval_expr(FZElems, State, Array) :-
	FZElems = [_|_], !,
	length(FZElems, N),
	dim(Array, [N]),
	( foreach(FZElem,FZElems), foreacharg(Elem,Array), param(State) do
	    eval_expr(FZElem, State, Elem)
	).
eval_expr({}(List), State, Set) :- !,
	State = state{solver:Solver},
	Solver:set_fzn_to_solver(List, Set).
eval_expr(Min..Max, State, Set) :- !,
	State = state{solver:Solver},
	Solver:range_fzn_to_solver(Min, Max, Set).
eval_expr(Ident[I0], State, Elem) :- !,
	eval_expr(I0, State, I),
	( integer(I) -> true ; fzn_error("Non-integer subscript %w", [I])),
	fzn_var_lookup(State, Ident, Array),
	arg(I, Array, Elem).
eval_expr(Expr, State, _Elem) :-
	compound(Expr), !,
	State = state{solver:Solver},
	eval_args(Expr, State, Goal),
	Solver:Goal.	% must be bool - succeed or fail
eval_expr(Expr, _State, _Elem) :-
	fzn_error("Illegal expression: %w", [Expr]).

eval_args(FZGoal, State, Goal) :-
	functor(FZGoal, Name, Arity),
	functor(Goal, Name, Arity),
	( for(I,1,Arity), param(FZGoal,Goal,State) do
	    arg(I, FZGoal, FZArg),
	    arg(I, Goal, Arg),
	    eval_expr(FZArg, State, Arg)
	).


% Version 0.8 extension: allow special int_float_lin() function.
% Convert to standard ECLiPSe linear expression.
eval_lin_expr(int_float_lin(Ints,Floats,IntVars,FloatVars), State, Result) ?- !,
	(
	    ( Ints==[], IntVars==[] ->
		nonempty_lists_to_linex(Floats, FloatVars, State, Result)
	    ;
		nonempty_lists_to_linex(Ints, IntVars, State, LinInts),
		lists_to_linex(Ints, IntVars, State, LinInts, Result)
	    )
	->
	    true
	;
	    fzn_error("Illegal arguments: %w", [int_float_lin(Ints,Floats,IntVars,FloatVars)])
	).
eval_lin_expr(Expr, State, Result) :-
	eval_expr(Expr, State, Result).

    nonempty_lists_to_linex([C|Cs], [V|Vs], State, Expr) ?-
    	eval_expr(C, State, Ceval),
    	eval_expr(V, State, Veval),
	lists_to_linex(Cs, Vs, State, (Ceval*Veval), Expr).

    lists_to_linex([], [], _State, Expr0, Expr) ?- Expr=Expr0.
    lists_to_linex([C|Cs], [V|Vs], State, Expr0, Expr) ?-
    	eval_expr(C, State, Ceval),
    	eval_expr(V, State, Veval),
	lists_to_linex(Cs, Vs, State, Expr0+(Ceval*Veval), Expr).


% Generate new variable number
new_varnum(State, N) :-
	State = state{id_count:N0},
	N is succ(N0),
	setarg(id_count of state, State, N).


% Annotations ---------------------------------------

% Split ident and annotations and make a proper annotation list
detach_annotations(Ident0::Anns, Ident, AnnList) ?- !,
	Ident = Ident0,
	anns_to_list(Anns, AnnList).
detach_annotations(IdentAnns, Ident, AnnList) :-
	Ident = IdentAnns, AnnList = [].

    anns_to_list(Ann::Anns, AnnList) ?- !,
	AnnList = [Ann|AnnList1],
	anns_to_list(Anns, AnnList1).
    anns_to_list(Ann, [Ann]).


check_annotations([]).
check_annotations([A|As]) :-
	( silent(A) ->
	    true
	;
	    printf(warning_output, "Annotation ignored: %w%n", [A])
	),
	check_annotations(As).

    silent(var_is_introduced).
    silent(output_var).
    silent(output_array(_)).
    silent(is_defined_var).
    silent(defines_var(_)).


% Evaluate annotation terms. This is similar to evaluating expressions,
% but we do not invoke any constraints/functions, we just do name lookups
% and Zinc->ECLiPSe type conversions.

:- mode eval_ann(+,+,-).
eval_ann([], _State, EmptyArray) :- !,
	EmptyArray = [].
eval_ann({}, State, Set) :- !,
	State = state{solver:Solver},
	Solver:set_fzn_to_solver([], Set).
eval_ann(Ident, State, Result) :-
	atom(Ident), !,
	( fzn_var_lookup(State, Ident, Result) ->
	    true		% takes care of variables, pars, true/false
	;
	    Result = Ident	% leave the atom unevaluated
	).
eval_ann(X, _State, Result) :-
	integer(X), !,
	Result = X.
eval_ann(X, State, Real) :-
	float(X), !,
	State = state{solver:Solver},
	Solver:float_fzn_to_solver(X, Real).
eval_ann(X, _State, Result) :-
	string(X), !,
	Result = X.
eval_ann(FZElems, State, Array) :-
	FZElems = [_|_], !,
	length(FZElems, N),
	dim(Array, [N]),
	( foreach(FZElem,FZElems), foreacharg(Elem,Array), param(State) do
	    eval_ann(FZElem, State, Elem)
	).
eval_ann({}(List), State, Set) :- !,
	State = state{solver:Solver},
	Solver:set_fzn_to_solver(List, Set).
eval_ann(Min..Max, State, Set) :- !,
	State = state{solver:Solver},
	Solver:range_fzn_to_solver(Min, Max, Set).
eval_ann(Ident[I0], State, Elem) :- !,
	eval_ann(I0, State, I),
	( integer(I) -> true ; fzn_error("Non-integer subscript %w", [I])),
	fzn_var_lookup(State, Ident, Array),	% array should exist
	arg(I, Array, Elem).
eval_ann(Expr, State, Result) :-
	compound(Expr), !,
	functor(Expr, Name, Arity),
	functor(Result, Name, Arity),
	( for(I,1,Arity), param(Expr,Result,State) do
	    arg(I, Expr, FZArg),
	    arg(I, Result, Arg),
	    eval_ann(FZArg, State, Arg)
	).
eval_ann(Expr, _State, _Elem) :-
	fzn_error("Illegal annotation expression: %w", [Expr]).


% Search ---------------------------------------

% Collect all problem variables in input order, grouped by types
extract_problem_vars(state{dict:Dict}, Ints, Floats, Bools, Sets) :-
	hash_list(Dict, _Keys, Entries),
	sort(num of zn_var, =<, Entries, Sorted),
	(
	    foreach(zn_var{eclvar:X,group:Group,ann:_Ann}, Sorted),
	    fromto(Ints,Is1,Is2,[]),
	    fromto(Floats,Fs1,Fs2,[]),
	    fromto(Bools,Bs1,Bs2,[]),
	    fromto(Sets,Ss1,Ss2,[])
	do
%	    ( memberchk(var_is_introduced, Ann) ->
%		Is1=Is2, Fs1=Fs2, Bs1=Bs2, Ss1=Ss2
%	    ;
		classify_var(Group, X, Is1, Is2, Fs1, Fs2, Bs1, Bs2, Ss1, Ss2)
%	    )
	).

    :- mode classify_var(++,?,-,+,-,+,-,+,-,+).
    classify_var(int, X, [X|Is], Is, Fs, Fs, Bs, Bs, Ss, Ss).
    classify_var(bool, X, Is, Is, Fs, Fs, [X|Bs], Bs, Ss, Ss).
    classify_var(float, X, Is, Is, [X|Fs], Fs, Bs, Bs, Ss, Ss).
    classify_var(set, X, Is, Is, Fs, Fs, Bs, Bs, [X|Ss], Ss).
    classify_var(arr_int, Xs, Is, Is0, Fs, Fs, Bs, Bs, Ss, Ss) :-
	( foreacharg(X,Xs), fromto(Is,[X|Is1],Is1,Is0) do true ).
    classify_var(arr_bool, Xs, Is, Is, Fs, Fs, Bs, Bs0, Ss, Ss) :-
	( foreacharg(X,Xs), fromto(Bs,[X|Bs1],Bs1,Bs0) do true ).
    classify_var(arr_float, Xs, Is, Is, Fs, Fs0, Bs, Bs, Ss, Ss) :-
	( foreacharg(X,Xs), fromto(Fs,[X|Fs1],Fs1,Fs0) do true ).
    classify_var(arr_set, Xs, Is, Is, Fs, Fs, Bs, Bs, Ss, Ss0) :-
	( foreacharg(X,Xs), fromto(Ss,[X|Ss1],Ss1,Ss0) do true ).
    classify_var(par, _X, Is, Is, Fs, Fs, Bs, Bs, Ss, Ss).


add_default_anns(State, UserAnns, Anns) :-
	% evaluate arguments of user annotations
	(
	    foreach(UserAnn,UserAnns),
	    fromto(Anns,[EvalUserAnn|Anns1],Anns1,DefaultAnns),
	    param(State)
	do
	    eval_ann(UserAnn, State, EvalUserAnn)
	),
	% add default search annotations
	extract_problem_vars(State, Ints, Floats, Bools, Sets),
	DefaultAnns = [
	    bool_search(Bools, input_order, indomain, complete),
	    int_search(Ints, first_fail, indomain, complete),
	    set_search(Sets, input_order, indomain_min, complete),
	    float_search(Floats, 0.0001, input_order, indomain_split, complete)
	].


% Output ---------------------------------------

:- export fzn_output/1.
:- comment(fzn_output/1, [
    summary:"Perform a FlatZinc model's output actions",
    amode:(fzn_output(+) is det),
    args:["FznState":"a FlatZinc state descriptor"],
    see_also:[fzn_init/2, fzn_load_stream/2, fzn_search/1],
    desc:html("<P>
	Assuming that a FlatZinc model has previously been set up
	and solved, this predicate will perform the output actions
	specified by the model's output annotations.  If no output
	annotations were given, no variable bindings will be printed.
	In addition, statistics information will be printed to the
	log_output stream.
    </P>"),
    eg:"
    my_fzn_run_stream(ModelStream, Options) :-
	fzn_init(Options, State),
	fzn_load_stream(ModelStream, State),
	fzn_search(State),
	fzn_output(State).
"]).
fzn_output(State) :-
	State = state{output_elems:Elems,output_stream:Out},
	( var(Elems) ->
	    default_output(State)
	;
	    % obsolete - there should be no output items
	    ( foreach(Elem,Elems), param(State,Out) do
		( Elem = show(Expr) ->
		    eval_expr(Expr, State, Value),
		    fzn_write(Out, Value)
		; Elem = show_cond(Cond,Then,Else) ->
		    eval_expr(Cond, State, Bool),
		    ( eval_expr(true, State, Bool) ->
			eval_expr(Then, State, Value)
		    ;
			eval_expr(Else, State, Value)
		    ),
		    fzn_write(Out, Value)
		;
		    write(Out, Elem)
		)
	    )
	),
	report_statistics(State).


default_output(state{solver:Solver,dict:Dict,solve_goal:Solve,cost:Cost,output_stream:Out}) :-
	hash_list(Dict, _Keys, Entries),
/*
	% Old code (pre output annotations)
	sort(num of zn_var, =<, Entries, Sorted),
	( foreach(zn_var{id:Ident,eclvar:X,type:Type,ann:Ann}, Sorted), param(Solver,Out) do
	    ( memberchk(var_is_introduced, Ann) ->
		true
	    ; Type = var(_) ->
		printf(Out, "%w = ", [Ident]),
                fzn_write(Out, X, Type, Solver),
                nl(Out)
	    ; Type = no_macro_expansion(array(_) of var(_)) ->
		printf(Out, "%w = ", [Ident]),
                fzn_write(Out, X, Type, Solver),
                nl(Out)
	    ;
		true
	    )
	),
*/
	% sort alphabetically
	sort(id of zn_var, =<, Entries, Sorted),
	( foreach(zn_var{id:Ident,eclvar:X,type:Type,ann:Ann,group:Group}, Sorted), param(Solver,Out) do
	    ( memberchk(output_var, Ann) ->
		( Type = var(_) ->
		    printf(Out, "%w = ", [Ident]),
		    fzn_write(Out, X, Type, Solver),
		    writeln(Out, ";")
		;
		    fzn_error("output_var cannot output %w", [Group])
		)
	    ; memberchk(output_array(Ranges), Ann) ->
		( Type = no_macro_expansion(array(_) of var(_)) ->
		    length(Ranges, Dim),
		    printf(Out, "%w = array%dd(", [Ident,Dim]),
		    ( foreach(Range,Ranges),param(Out) do printf(Out, "%Kw, ", [Range])),
		    fzn_write(Out, X, Type, Solver),
		    writeln(Out, ");")
		;
		    fzn_error("output_array cannot output %w", [Group])
		)
	    ;
		true
	    )
	),
	( number(Cost) ->
	    ( Solve = minimize(_,_,_) ->
		printf(log_output, "%% Minimum objective value = %q%n", [Cost])
	    ;
		printf(log_output, "%% Maximum objective value = %q%n", [Cost])
	    )
	;
	    true
	).


% Write ECLiPSe data according to MiniZinc type information
% We do not check for strict type compatibility here
:- export fzn_write/4.
fzn_write(Stream, X, Type, Solver) :-
	( nonground(X) ->
	    fzn_error("fzn_write requires ground data: %w", [X])
	; fzn_write1(Stream, X, Type, Solver) ->
	    true
	;
	    fzn_error("Solver data / Zinc type mismatch (%w / %w)", [X,Type])
	).

    fzn_write1(Stream, X, var(VType), Solver) :- !,
	fzn_write1(Stream, X, VType, Solver).
    fzn_write1(Stream, X, bool, Solver) :- !,
	Solver:bool_solver_to_fzn(X, Z), writeq(Stream, Z).
    fzn_write1(Stream, X, int, _Solver) :- !,
	writeq(Stream, X).
    fzn_write1(Stream, X, _.._, _Solver) :- !,
	writeq(Stream, X).
    fzn_write1(Stream, X, {}(_), _Solver) :- !,
	writeq(Stream, X).
    fzn_write1(Stream, X, float, Solver) :- !,
	Solver:float_solver_to_fzn(X, Z), writeq(Stream, Z).
    fzn_write1(Stream, Array, no_macro_expansion(array([_]) of EType), Solver) :- !,
	functor(Array, [], N),
	write(Stream, '['),
	( N==0 -> true ;
	    ( for(I,1,N-1), param(Array,Stream,EType,Solver) do
		arg(I, Array, X),
		fzn_write1(Stream, X, EType, Solver),
		write(Stream, ',')
	    ),
	    arg(N, Array, Xn),
	    fzn_write1(Stream, Xn, EType,Solver)
	),
	write(Stream, ']').
    fzn_write1(Stream, Set, no_macro_expansion(set of EType), Solver) ?-
	Solver:set_solver_to_fzn(Set, Xs),	% may fail
	write(Stream, '{'),
        ( Xs = [X1|Xs1] ->
            fzn_write1(Stream, X1, EType, Solver),
            ( foreach(X,Xs1), param(Stream,EType,Solver) do
                write(Stream, ','),
                fzn_write1(Stream, X, EType, Solver)
            )
        ;
            true
        ),
	write(Stream, '}').


% Write ECLiPSe data in MiniZinc format, making a guess at the type
:- export fzn_write/2.
fzn_write(Stream, X) :- string(X), !, write(Stream, X).
fzn_write(Stream, X) :- number(X), !, writeq(Stream, X).
fzn_write(Stream, Xs) :- is_list(Xs), !,	% lists (incl []) are Zinc sets
	write(Stream, '{'),
        ( Xs = [X1|Xs1] ->
            fzn_write(Stream, X1),
            ( foreach(X,Xs1), param(Stream) do
                write(Stream, ','),
                fzn_write(Stream, X)
            )
        ;
            true
        ),
	write(Stream, '}').
fzn_write(Stream, X) :- atom(X), !, write(Stream, X).	% excluding []
fzn_write(Stream, L..H) ?- integer(L), integer(H), !, write(Stream, L..H).
fzn_write(Stream, Array) :- functor(Array, [], N), !,
        % For 2D arrays, write Minizinc 2D syntax, e.g. [|1,2|3,4|].
        % We can then use it in a Minizinc data file as a parameter.
        ( arg(1, Array, X1), compound(X1), functor(X1, [], _) ->
            write(Stream, '[|')
        ;
            write(Stream, '[')
        ),
	( for(I,1,N-1), param(Array,Stream) do
	    arg(I, Array, X),
            fzn_write_row(Stream, X)
	),
	arg(N, Array, Xn),
	fzn_write_row(Stream, Xn),
	write(Stream, ']').
fzn_write(_Stream, X) :-
	fzn_error("Illegal data in fzn_write(%w)", [X]).

    fzn_write_row(Stream, Row) :-
        ( compound(Row), functor(Row, [], M) ->
            ( for(J,1,M-1), param(Row,Stream) do
                arg(J, Row, X),
                fzn_write(Stream, X),
                write(Stream, ',')
            ),
            arg(M, Row, X),
            fzn_write(Stream, X),
            write(Stream, '|')
        ;
            fzn_write(Stream, Row),
            write(Stream, ',')
        ).


% Attach ECLiPSe var_names ---------------------------------------

add_var_names(state{options:zn_options{var_names:on},dict:Dict}) ?- !,
	hash_list(Dict, _, Decls),
	( foreach(zn_var{id:Id,eclvar:X,type:Type,ann:Ann},Decls) do
	    ( memberchk(var_is_introduced, Ann) ->
	    	true

	    ; Type = var(_) ->
		mzn_to_eclipse_name(Id, Name),
		( var_name:set_var_name(X, Name) -> true ; true )

	    ; Type = no_macro_expansion(array(_) of var(_)) ->
		mzn_to_eclipse_name(Id, Name),
		( foreacharg(Xi,X), param(Name) do
		    ( var(Xi), var_name:set_var_name(Xi, Name) ->
		    	true
		    ;
			% Xi cannot be named: name a dummy variable
			% to keep the index in sync with FlatZinc index
			var_name:set_var_name(_, Name)
		    )
		)
	    ;
		true
	    )
	).
add_var_names(_).

mzn_to_eclipse_name(Id, Name) :-
	atom_string(Id, IdS),
	string_code(IdS, 1, FirstC),
	( get_chtab(FirstC, upper_case) ->
	    Name = Id
	; get_chtab(FirstC, lower_case) ->
	    concat_strings("_", IdS, Name)
	;
	    fzn_error("Unexpected varname %w", [Id])
	).


% Auxiliary ---------------------------------------

hash_insert_id(Hash, Key, Value) :-
	(atom(Key) -> true ;
	    fzn_error("Illegal Identifier %w",[Key])
	),
	( hash_insert(Hash, Key, Value) -> true ;
	    fzn_error("Variable defined twice: %w", [Key])
	).


:- export fzn_var_lookup/3.
:- comment(fzn_var_lookup/3, [
    summary:"Find ECLiPSe term corresponding to Mini/FlatZinc identifier",
    amode:(fzn_var_lookup(+,+,-) is semidet),
    fail_if:"Fails if there is no such identifier",
    args:["FznState":"a FlatZinc state descriptor",
	"Id":"Mini/FlatZinc identifier (atom)",
	"Value":"Output: ECLiPSe constant, array or domain variable"],
    see_also:[fzn_load_stream/2,fzn_obj_lookup/2,library(minizinc)],
    desc:html("<P>
	Assuming a model has previously been loaded successfully
	using fzn_load_xxx or mzn_load_xxx, this primitive provides
	the mapping from Mini/FlatZinc identifier to the ECLiPSe
	term that it is mapped to.
    </P>")
]).
fzn_var_lookup(state{dict:Dict}, Key, Value) :-
	hash_get(Dict, Key, Entry),
	Entry = zn_var{eclvar:Value}.


:- export fzn_obj_lookup/2.
:- comment(fzn_obj_lookup/2, [
    summary:"Find ECLiPSe term representing the Mini/FlatZinc model's objective",
    amode:(fzn_obj_lookup(+,-) is semidet),
    fail_if:"Fails if the model has no objective",
    args:["FznState":"a FlatZinc state descriptor",
	"Obj":"Output: ECLiPSe constant or domain variable"],
    see_also:[fzn_load_stream/2,fzn_var_lookup/3,library(minizinc)],
    desc:html("<P>
	Assuming a model has previously been loaded successfully
	using fzn_load_xxx or mzn_load_xxx, this primitive returns
	the ECLiPSe term that represents the model's objective.
	This can be used for instance by a branch-and-bound primitive.
    </P>")
]).
fzn_obj_lookup(state{solve_goal:minimize(Obj,_,_)}, Obj) :- !.
fzn_obj_lookup(state{solve_goal:maximize(Obj,_,_)}, Obj) :- !.


:- export fzn_error/2.
fzn_error(Message, Culprit) :-
	write(error, "Error: "),
	printf(error, Message, Culprit),
	nl(error),
	abort.


:- export fzn_last/1.
:- comment(fzn_last/1, [
    summary:"Increments solutions count, and succeeds if last one reached",
    amode:(fzn_last(+) is semidet),
    fail_if:"Fails if not enough solutions found yet",
    args:["FznState":"a FlatZinc state descriptor"],
    see_also:[struct(zn_options),fzn_init/2,fzn_search/1],
    desc:html("<P>
	Assuming a model has previously been loaded successfully
	using fzn_load_xxx or mzn_load_xxx, this primitive can be used
	to limit the number of solutions produced.  It increments a
	nonlogical counter and fails as long as the number of solutions
	that were requested in the zn_options has not yet been reached.
    </P>"),
    eg:"
    my_fzn_run_stream(ModelStream, Options) :-
	fzn_init(Options, State),
	fzn_load_stream(ModelStream, State),
	fzn_search(State),
	fzn_output(State),
	fzn_last(State),
	!.
    "
]).
:- export fzn_last/1.
fzn_last(state{sol_cnt:N}) :-
	\+ shelf_dec(N, 1).

:- export fzn_unsat/1.
fzn_unsat(state{sol_cnt:N,sol_cnt_init:InitCnt}) :-
	shelf_get(N, 1, InitCnt).
