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
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf.
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Component:	ECLiPSe III compiler
% Version:	$Id: compiler_common.ecl,v 1.24 2010/03/12 10:22:46 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(compiler_common).

:- comment(summary, "ECLiPSe III compiler - common data structures and auxiliaries").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2010/03/12 10:22:46 $").

%----------------------------------------------------------------------
% Common options-structure
%----------------------------------------------------------------------

:- comment(struct(options), [
    summary:"Compiler Options",
    desc:html("
    Options to control the operation of the compiler.
<P>
<P>
    The abstract machine code which is the result of the compilation can
    be output in various forms.  Values for 'output' option (default: none):
    <DL>
    <DT>none</DT><DD>
	no output (but code may be loaded, see load option),
    </DD><DT>asm</DT><DD>
	output compiled code in asm format to input file wth .asm suffix,
        This format represents the code as WAM code that can be loaded back
        into ECLiPSe using the assembler (lib(asm)).  
    </DD><DT>eco(File)</DT><DD>
	output compiled code in eco format,
    </DD><DT>eco</DT><DD>
	output compiled code in eco format to input file wth .eco suffix,
    </DD><DT>print</DT><DD>
	print resulting WAM code to the output stream,
    </DD><DT>print(Stream)</DT><DD>
	print WAM code to Stream,
    </DD><DT>listing(File)</DT><DD>
	print WAM code to File,
    </DD><DT>listing</DT><DD>
	print WAM code to input file with .lst suffix,
    </DD></DL>
    The destination directory for output files is determined by the 'outdir'
    option. The default is to place output files in the same directory as
    the corresponding input file.
    <P>
    When loading is requested, the abstract machine code produced by the
    compiler gets assembled and loaded into memory as executable code.
    Values for the 'load' option (default: all):
    <DL>
    <DT>none</DT><DD>
	Do not load any code into memory, do not execute queries 
	nor directives, but interpret pragmas.
    </DD><DT>new</DT><DD>
	Do not overwrite any code in memory, but load new predicates.
	Do not re-create modules, but create new ones and erase them
	again after compilation. For existing modules, erase pragmas.
	Do not execute queries, but call directives and interpret pragmas.
    </DD><DT>all</DT><DD>
	Load and replace code in memory, create/re-create all modules, 
	interpret pragmas, call directives, execute queries.
    </DD></DL>
    <P>
    The 'debug' option (off/on) determines whether the resulting code is
    traceable.
    <P>
    The 'system' and 'skip' option set predicate properties.
    <P>
    The 'verbose' option controls whether the compiler produces any log
    messages to the log_output stream. Level 1 produces information about
    predicates as they are compiled.
    <P>
    The 'srcroot' option is used to make the compilation result (e.g. .eco
    files) independent of absolute source file names: when set, and it is
    a parent directory is a source file, the source file property of the
    compiled predicates will be set as a pathname relative to the given
    root directory.
    <P>
    Various print_xxx options allow the output of internal compiler data
    structures for debugging purposes.
    "),
    fields:[
	"output":"Output format and destination (none, print, eco, asm, listing - default none)",
	"outdir":"directory in which to place output files (default \"\", same as input)",
	"load":"load code into memory (none/new/all, default:all).",
	"debug":"generate code with debug instructions (on/off, default: same as global flag debug_compile).",
	"system":"mark compiled predicates as type:built_in (on/off, default:off).",
	"skip":"set the skip-flag in all compiled predciates (on/off, default:off).",
	"expand_clauses":"expand clause macros, such as DCGs (on/off, default:on).",
	"expand_goals":"expand goal macros, i.e. do inlining (on/off, default: same as global flag goal_expansion).",
	"opt_level":"optimization level (0 or 1)",
	"print_normalised":"print result of the normalisation pass (on/off, default:off).",
	"print_indexes":"print result of indexing  analysis (on/off, default:off).",
	"print_lifetimes":"print result of the variable lifetime analysis (on/off, default:off).",
	"print_raw_code":"print annotated WAM code before register allocation (on/off, default:off).",
	"print_final_code":"print annotated WAM code after register allocation (on/off, default:off).",
	"srcroot":"directory prefix that will be stripped from source file paths (default:\"\")",
	"verbose":"print messages to log_output, according to level (integer (0=silent,1=quiet,2=verbose), default:0).",
	"warnings":"print warning messages to warning_output (on/off, default:on)."
    ],
    see_also:[get_flag/2,set_flag/2]
]).

:- export struct(options(
	output,
	outdir,
	load,
	debug,
	system,
	skip,
	expand_clauses,
	expand_goals,
	opt_level,
	print_normalised,
	print_indexes,
	print_lifetimes,
	print_raw_code,
	print_final_code,
	srcroot,
	verbose,
	warnings
    )).


valid_option_field(debug, debug of options).
valid_option_field(system, system of options).
valid_option_field(skip, skip of options).
valid_option_field(expand_clauses, expand_clauses of options).
valid_option_field(expand_goals, expand_goals of options).
valid_option_field(opt_level, opt_level of options).
valid_option_field(print_normalised, print_normalised of options).
valid_option_field(print_lifetimes, print_lifetimes of options).
valid_option_field(print_raw_code, print_raw_code of options).
valid_option_field(print_final_code, print_final_code of options).
valid_option_field(print_indexes, print_indexes of options).
valid_option_field(verbose, verbose of options).
valid_option_field(warnings, warnings of options).
valid_option_field(srcroot, srcroot of options).
valid_option_field(outdir, outdir of options).
valid_option_field(output, output of options).
valid_option_field(load, load of options).

valid_option_value(debug, Value) :- onoff(Value).
valid_option_value(system, Value) :- onoff(Value).
valid_option_value(skip, Value) :- onoff(Value).
valid_option_value(expand_clauses, Value) :- onoff(Value).
valid_option_value(expand_goals, Value) :- onoff(Value).
valid_option_value(load, none).
valid_option_value(load, new).
valid_option_value(load, all).
valid_option_value(opt_level, Value) :- integer(Value).
valid_option_value(print_normalised, Value) :- onoff(Value).
valid_option_value(print_lifetimes, Value) :- onoff(Value).
valid_option_value(print_raw_code, Value) :- onoff(Value).
valid_option_value(print_final_code, Value) :- onoff(Value).
valid_option_value(print_indexes, Value) :- onoff(Value).
valid_option_value(verbose, Value) :- integer(Value).
valid_option_value(warnings, Value) :- onoff(Value).
valid_option_value(srcroot, Value) :- (string(Value);atom(Value)),!.
valid_option_value(outdir, Value) :- (string(Value);atom(Value)),!.
valid_option_value(output, listing(_File)).
valid_option_value(output, listing).
valid_option_value(output, print(_Stream)).
valid_option_value(output, print).
valid_option_value(output, none).
valid_option_value(output, eco).
valid_option_value(output, eco(_)).
valid_option_value(output, eco_to_stream(_)).
valid_option_value(output, asm).
valid_option_value(output, asm(_)).
valid_option_value(output, asm_to_stream(_)).

onoff(off).
onoff(on).

default_options(options{
	debug:Debug,
	system:off,
	skip:off,
	expand_clauses:on,
	expand_goals:GoalExp,
	load:all,
	opt_level:1,
	print_normalised:off,
	print_lifetimes:off,
	print_raw_code:off,
	print_final_code:off,
	print_indexes:off,
	verbose:0,
	warnings:on,
	srcroot:"",
	outdir:"",
	output:none
}) :-
	get_flag(debug_compile, Debug),
	get_flag(goal_expansion, GoalExp).


%----------------------------------------------------------------------
% Normalised source data structures
%----------------------------------------------------------------------

% Descriptor for a subgoal other than a conjunction or disjunction
% Also used for the clause head

:- comment(struct(goal), [
    summary:"Descriptor for a subgoal other than a conjunction or disjunction",
    desc:ascii("
	Descriptor for a subgoal other than a conjunction or disjunction.
	These goals come in three flavours:

	    - simple (inline)
	    - regular
	    - head (treated as pseudo-goal)
    "),
    fields:[
	kind:		"Kind of goal (simple|regular|head)",
	callpos:	"identifier for the chunk it occurs in",
	functor:	"the goal's functor (Name/Arity)",
	args:		"list of normalised terms",
	envmap:		"environment activity bitmap (at call time)",
	envsize:	"environment size (at call time)",
	state:		"execution state at call time (struct(state)),"
			" the result of the analysis phase",
        path:           "full file path to file in which goal occurs",
	line:		"source line of goal",
        from:           "start position (from start of file) of goal",
        to:             "end position (from start of file) of goal",
        lookup_module:	"module where to look up the predicate definition ([] if context module)",
	definition_module: "module where the predicate is defined ([] if unknown)"
    ]
]).

:- export struct(goal(
    	kind,
	callpos,
	functor,
	args,
	envmap,
	envsize,
	state,
        path,
        line,
        from,
        to,
        lookup_module,
    	definition_module	% cached
    )).


:- comment(struct(disjunction), [
    summary:"Descriptor for a disjunction",
    desc:ascii("
	Descriptor for a disjunction, resulting either from a disjunction
	in the source (;/2) or from multiple clauses.
    "),
    fields:[
	determinism:	"array[NBranches] of (det|try|retry|trust|failure)",
	callpos:	"identifier for the chunk it occurs in",
	arity:		"pseudo-arity (valid arguments at retry time)",
	args:		"the disjunction's pseudo-arguments",
	branchheadargs:	"the pseudo-head arguments for each branch",
	indexvars:	"list of variable{}s to which the indexes apply",
	indexes:	"list of index{} corresponding to indexvars",
	branches:	"list of list of normalised goals (at least 2 elements)",
	branchlabels:	"array[NBranches] of Labels",
	branchentrymaps:"list of envmaps for each branch start (used in retry/trust instructions)",
	branchinitmaps:	"list of envmaps for end-of-branch inits",
	entrymap:	"environment activity bitmap (just before disjunction)",
	entrysize:	"environment size (just before disjunction)",
	exitmap:	"environment activity bitmap (just after disjunction)",
	exitsize:	"environment size (just after disjunction)",
	state:		"execution state on entry (struct(state)),"
			" the result of the analysis phase"
    ]
]).

:- export struct(disjunction(
	determinism,	% determinism information for each branch
	callpos,
	arity,		% arity for try instructions (number of pseudo-arguments)
	args,		% the disjunction's pseudo-arguments
	branchheadargs,	% the pseudo-head arguments for each branch
	indexvars,	% list of variable{}s to which the indexes apply
	indexes,	% list of index{} corresponding to indexvars
    	branches,	% list of list of goals
	branchlabels,	% array[NBranches] of Labels
	branchentrymaps, % list of envmaps for start of each branch
	branchinitmaps, % list of envmaps for end-of-branch-inits
	entrymap,	
	entrysize,
	exitmap,
	exitsize,
	state
    )).


:- comment(struct(index), [
    summary:"Descriptor for one index",
    desc:ascii("
	Descriptor for one index, i.e. a branching decision
	on a single variable.
    "),
    fields:[
	quality:	"positive float: index quality, the smaller the better",
	variable:	"variable{} that this index works on",
	partition:	"dt{} decision tree"
    ]
]).

:- export struct(index(
	quality,	% positive float, 0.0=best
	variable,	% variable{}
	partition	% decision tree
    )).



:- comment(struct(structure), [
    summary:"Descriptor for a compound term",
    desc:ascii("
	Descriptor for a compound term (other than a list) that occurred in
	the source code. Lists in the source code are simply represented as
	lists in the normalised code. Atomic constants in the source code
	are represented as themselves in the normalised form.
    "),
    fields:[
	name:		"functor name (atom)",
	arity:		"functor arity (integer)",
	args:		"list of arguments (normalised terms)"
    ]
]).

:- export struct(structure(	% maybe this should be simpler
	name,
	arity,
	args
    )).


% We have one struct(variable) for every variable occurrence.
%
% isfirst/islast are set for _all_ occurrences in the chunk where the
% variable occurs first/last in order to still allow for reordering later.
%
% The class field is shared between all occurrences which belong to the
% same instance of the variable. For temporary variables it is nonvoid(t(T)).
% For permanent variables it is nonvoid(y(I)) where I is filled in once known.
%
% All information apart from varid could in principle be held in separate
% tables, indexed on varid.

:- comment(struct(variable), [
    summary:"Descriptor for a variable occurrence in the normalised source",
    desc:ascii("
	We have one struct(variable) for every variable occurrence.
	isfirst/islast are set for _all_ occurrences in the chunk where
	the variable occurs first/last in order to still allow for later
	reordering.

	The class field is shared between all occurrences which belong to
	the same instance of the variable.  Its possible values are:
	
	- void           for void variables (only one occurrence)
	- nonvoid(temp)  for temporary variables (occurs only in one chunk)
	- nonvoid(y(Y))  for permanent variables (Y filled in once known)

	Possible alternative implementation: All information apart from varid
	could in principle be held in separate tables, indexed on varid.
    "),
    fields:[
	varid:		"unique source variable id (integer), created by normalize_clauses",
	class:		"variable class and permanent location (filled in by assign_env_slots)",
	source_info:	"struct(annotated_term), name/1 or 'none'"
    ]
]).

:- export struct(variable(
	varid,			% unique source variable id (integer)
	class,			% variable class and location:
				%	void
				%	nonvoid(temp)
				%	nonvoid(y(I))
	source_info		% struct(annotated_term) or uninstantiated
    )).



:- comment(struct(attrvar), [
    summary:"Descriptor for an attributed variable in the normalised form",
    desc:ascii("
	Descriptor for an attributed variable that occurred in the source
	code. It is represented as a pair of a plain variable, and a
	structure. The structure arguments are the normalised attributes.
	Note that the mapping of attribute slots to indices in the
	structure depends on the meta_attribute/2 declarations that are
	in place at compile time.
    "),
    fields:[
	"variable":"A standard variable (struct(variable))",
	"meta":"An attribute struture (struct(structure))"
    ]
]).

:- export struct(attrvar(
	variable,		% struct(variable)
	meta			% struct(structure)
    )).


% Get a descriptor for an additional occurrence of existing variable
% This can only be used _before_ compute_lifetimes!
:- export new_vardesc/2.
new_vardesc(VarId, variable{varid:VarId,source_info:none}).

/*
% Introduce a new, auxiliary source variable into normalised code
:- export new_aux_variable_norm/3.
new_aux_variable_norm(VarDesc, VId0, VId) :-
	VarDesc = variable{varid:VId,source_name:''},
	VId is VId0+1.
*/


%----------------------------------------------------------------------
% Annotated WAM code
% This is generated by the code generation phase and understood
% by register allocation.
%----------------------------------------------------------------------

:- comment(struct(code), [
    summary:"Annotated abstract machine instruction",
    desc:ascii("
	This is the format that the code generator produces (list of
	struct(code)), and which is fed into the register allocator and
	the peephole optimizer.  Finally, the annotations are stripped
	and the instr-fields alone are use as input to the assembler.

	The comment field can be used for debugging, e.g. it can contain
	information about the purpose of an instruction, or about the
	source construct it relates to.
    "),
    see_also:[generate_code/5,print_annotated_code/1],
    fields:[
	instr:	"WAM instruction (assembler input format)",
	regs:	"Register usage descriptor",
	comment:"Arbitrary ground term"
    ]
]).

:- export struct(code(
	instr,			% AM instruction (assembler input format)
	regs,			% list of register descriptors
	comment			% string
    )).


%----------------------------------------------------------------------
% Analysis phase data structures
%----------------------------------------------------------------------

:- comment(struct(state), [
    summary:"Descriptor for dataflow analysis results",
    desc:ascii("
	This describes what is known about the state of computation at the
	end of a subgoal (or at predicate entry, for heads).
	This is where the results of the flow and binding analysis get
	collected. It contains determinism and binding information.
	The information is stored in struct(goal)

	Possible determinism values:

	    det
	    failure

	Possible binding states:

	    alias(VarId)  aliased to another variable

	    --type	uninitialised (not aliased, not in a delayed goal)
	    (-type)	uninstantiated - this is currently unused
	     ?type	unknown instantiation state
	     +type	instantiated
	    ++type	ground

	Possible types:

	    univ
	      +----------------------------------------------+
	    atomic                                        compound
	      +--------+--------+-------------+-------+      |
	    number   handle   cutpoint      string  atom    N/A
	      |                               |       |
	      +-------+-------+--------+    value() value()
	    integer float   rational breal
	      |       |       |        |
	    value() value() value()  value()


    "),
    see_also:[binding_analysis/1,struct(goal)],
    fields:[
	determinism:	"an atom (det|failure)",
	bindings:	"a map varid->binding"
    ]
]).

:- export struct(state(
    	determinism,	% an atom (det|failure|...)
	bindings	% a map varid->binding
    )).


% Determinism of branch entries are: (det|try|retry|trust|failure)

:- export first_alternative/1.
first_alternative(det).
first_alternative(try).
first_alternative(failure).

:- export last_alternative/1.
last_alternative(det).
last_alternative(trust).
last_alternative(failure).


%----------------------------------------------------------------------
% Debugging
%----------------------------------------------------------------------

:- comment((verify)/1, [
    summary:"Debugging aid: verify a condition",
    args:["Goal":"A condition to check"],
    see_also:[(certainly_once)/1],
    desc:ascii("
	Checks at runtime whether a given condition is true, and prints
    	an error and aborts if false.
	Goal is not supposed to contribute to the semantics of the program,
	i.e. it should not bind anything or cause side effects.
	When checks are disabled, verify(Goal) is replaced by true.
    ")
]).

:- comment((certainly_once)/1, [
    summary:"Debugging aid: make sure a deterministic goal does not fail",
    args:["Goal":"A goal to execute"],
    see_also:[(verify)/1],
    desc:ascii("
	Executes once(Goal) and checks that it doesn't fail. If it fails,
	prints an error and aborts.
	Unlike verify/1, the goal Goal is part of the program semantics.
	When checks are disabled, certainly_once(Goal) is replaced by once(Goal).
    ")
]).

:- export (verify)/1.
:- export (certainly_once)/1.
:- export op(900, fy, verify).
:- export op(900, fy, certainly_once).
expand_check((verify Goal), (Goal->true;printf(error, "Check failed: %w%n", [Goal]),abort) ).
expand_check((certainly_once Goal), (Goal->true;printf(error, "Goal failed unexpectedly: %w%n", [Goal]),abort) ).
expand_nocheck(verify _, true).
expand_nocheck(certainly_once Goal, once Goal).

% Uncomment alternative lines to enable/disable checks
:- inline((verify)/1, expand_check/2).
%:- inline((verify)/1, expand_nocheck/2).
:- inline((certainly_once)/1, expand_check/2).
%:- inline((certainly_once)/1, expand_nocheck/2).

verify _Goal :-
	verify _Goal.

certainly_once _Goal :-
	certainly_once _Goal.


:- export unreachable/1.
unreachable(Message) :-
	printf(warning_output, "WARNING: Unreachable code reached: %w%n",
		[Message]).


:- export indent/2.
indent(Stream, Indent) :-
	I is 4*Indent,
	printf(Stream, "%*c", [I,0' ]).


:- export message/2.
message(Message, options{verbose:Level}) :-
	( Level > 0 ->
	    writeln(log_output, Message)
	;
	    true
	).

:- export message/3.
message(Message, MsgLevel, options{verbose:Level}) :-
	( Level >= MsgLevel ->
	    writeln(log_output, Message)
	;
	    true
	).


:- export warning/1.
warning(Message) :-
	printf(warning_output, "WARNING: %w%n", [Message]).


:- export singleton_warning/2.
%singleton_warning(+VarSourceInfo, +Options).
singleton_warning(annotated_term{type:var(Name),file:Path,line:Line}, options{warnings:on}) ?-
	atom_string(Name, NameS), \+ substring(NameS,"_",1),
	get_flag(variable_names, check_singletons),	% preliminary
	!,
	pathname(Path, _, File),
	printf(warning_output, "File %w, line %d: Singleton variable %w%n", [File,Line,Name]).
singleton_warning(_, _).


%----------------------------------------------------------------------
% Errors and warnings
%----------------------------------------------------------------------

:- export compiler_warning/5.
compiler_warning(Ann, SourcePos, String, Params, options{warnings:on}) :- !,
	compiler_message('WARNING', Ann, SourcePos, String, Params).
compiler_warning(_, _, _, _, _).


:- export compiler_error/4.
compiler_error(_Ann, SourcePos, String, Params) :-
	compiler_message('ERROR', _Ann, SourcePos, String, Params),
	exit_block(abort_compile_predicate).


:- export compiler_event/5.
compiler_event(EventNr, SourcePos, Ann, Term, Module) :-
	get_error_location(Ann, SourcePos, Location),
	error(EventNr, Term@Location, Module).


    compiler_message(Severity, Ann, SourcePos, String, Params) :-
	severity_stream(Severity, Stream),
	printf(Stream, "%w: ", [Severity]),
	print_error_location(Stream, Ann, SourcePos),
	printf(Stream, String, Params),
	nl(Stream),
	flush(Stream).

    severity_stream('WARNING', warning_output).
    severity_stream('ERROR', error).


:- export print_error_location/3.
print_error_location(Stream, Ann, SourcePos) :-
	get_error_location(Ann, SourcePos, Location),
	print_location(Stream, Location).


:- export print_location/2.
print_location(Stream, File:Line) ?- !,
	local_file_name(File, LocalFile),
	printf(Stream, "%w:%d:%n  ", [LocalFile,Line]).
print_location(Stream, Location) :-
	printf(Stream, "In compiling %w:%n  ", [Location]).


:- use_module(source_processor).	% for source_position{}

:- export get_error_location/3.
get_error_location(Ann, SourcePos, Location) :-
	( nonvar(Ann), Ann = annotated_term{file:File,line:Line} ->
	    local_file_name(File, LocalFile),
	    Location = LocalFile:Line
	; SourcePos = source_position{stream:Stream,file:File,line:Line} ->
	    ( current_stream(Stream) ->
		get_stream_info(Stream, device, Device),
		( Device == file ->
		    local_file_name(File, LocalFile),
		    Location = LocalFile:Line
		;
		    concat_string([Device," stream ",Stream], PseudoFile),
		    Location = PseudoFile:Line
		)
	    ;
		concat_string(["Stream ",Stream], PseudoFile),
		Location = PseudoFile:Line
	    )
	; SourcePos = term ->
	    Location = SourcePos
	; compiled_stream(Stream) ->
	    Location = LocalFile:Line,
	    get_stream_info(Stream, name, File),
	    local_file_name(File, LocalFile),
	    get_stream_info(Stream, line, Line)
	;
	    Location = unknown_location
	).


:- export local_file_name/2.
local_file_name(File, LocalF) :-
	getcwd(Cwd),
	concat_string([File], FileS),
	( substring(FileS, "/", 1), append_strings(Cwd, LocalF, FileS) ->
	    true
	;
	    LocalF = File
	).


%----------------------------------------------------------------------
% Parameters
%----------------------------------------------------------------------

:- export macro((#)/1,'tr_#'/2,[]), op(100, fx, #).
:- export 'tr_#'/2.
'tr_#'(no_macro_expansion(#Name), Value) :- constant(Name, Value).

% WAM
    constant(wam_registers,		265).		% NARGREGS-1
    constant(wam_max_global_push,	200).
% Tracer ports
    constant(no_port,			0).		% NO_PORT
    constant(call_port,			1).		% CALL_PORT
    constant(next_port,			9).		% NEXT_PORT
    constant(else_port,			16'20D).	% INLINE_PORT|ELSE_PORT
% Events
    constant(inst_fault,		4).		% INSTANTIATION_FAULT
    constant(type_error,		5).		% TYPE_ERROR
    constant(tool_redef,		61).		% TOOL_REDEF
    constant(illegal_head,		130).		% ILLEGAL_HEAD
    constant(illegal_goal,		131).		% ILLEGAL_GOAL
    constant(consecutive,		134).		% CONSECUTIVE
    constant(compiled_file,		139).		% COMPILED_FILE
    constant(multifile,			145).		% MULTIFILE
    constant(start_compiler,		146).		%
    constant(bad_pragma,		148).		% BAD_PRAGMA
    constant(code_unit_loaded,		149).		% CODE_UNIT_LOADED
    constant(record_compiled_file,	166).


:- export smallint/1.
smallint(X) :- 
	integer(X),
	X =< 16'7fffffff,
	X+1 >= -16'7fffffff.


:- export words_to_bytes/2.
words_to_bytes(Words, Bytes) :-
	Bytes is Words*32//(sepia_kernel:decode_code(w(32))).


:- export machine_bits/1.
machine_bits(Bits) :-
	Bits is 256//(sepia_kernel:decode_code(w(32))).


%----------------------------------------------------------------------
% General auxiliaries
%----------------------------------------------------------------------

:- export
	strip_keys/2,
	merge_sorted_lists/3,
	group_same_key_values/2,
	group_same_key_values/3,
	merge_same_key_values/2,
	concat_same_key_values_unstable/2,
	concat_same_key_values_stable/2.
	

% merge_sorted_lists(+Lists, ?ListsTail, -MergedList)
merge_sorted_lists([], [], []).
merge_sorted_lists([L], [], L) :- !.
merge_sorted_lists([L1,L2|Ls], [L12|Tail], L) :-
    	merge(0, =<, L1, L2, L12),
	merge_sorted_lists(Ls, Tail, L).


group_same_key_values([], []).
group_same_key_values([K-V|List], [K-[V|KVs]|GroupedList]) :-
        group_same_key_values1(List, K, KVs, GroupedList).

    group_same_key_values1([], _, [], []).
    group_same_key_values1([K-V|List], K, [V|KVs], GroupedList) :- !,
        group_same_key_values1(List, K, KVs, GroupedList).
    group_same_key_values1([K-V|List], _K, [], [K-[V|KVs]|GroupedList]) :-
        group_same_key_values1(List, K, KVs, GroupedList).


group_same_key_values(_Pos, [], []).
group_same_key_values(Pos, [KV|List], [[KV|KVs]|GroupedList]) :-
	arg(Pos, KV, K),
        group_same_key_values2(Pos, List, K, KVs, GroupedList).

    group_same_key_values2(_Pos, [], _, [], []).
    group_same_key_values2(Pos, [KV|List], K, [KV|KVs], GroupedList) :-
	arg(Pos, KV, K1), K == K1, !,
        group_same_key_values2(Pos, List, K, KVs, GroupedList).
    group_same_key_values2(Pos, [KV|List], _K, [], [[KV|KVs]|GroupedList]) :-
	arg(Pos, KV, K),
        group_same_key_values2(Pos, List, K, KVs, GroupedList).


% the values are assumed to be lists.
% unstable version: concats the lists in reverse order,
% but makes less copies (no copy for groups of size one)
concat_same_key_values_unstable([], []).
concat_same_key_values_unstable([K-V|List], [K-Vs|GroupedList]) :-
        concat_same_key_values_unstable(List, V, K, Vs, GroupedList).

    concat_same_key_values_unstable([], Vs, _K, Vs, []).
    concat_same_key_values_unstable([K-V|List], Vs, K, KVs, GroupedList) :- !,
	append(V, Vs, VVs),
        concat_same_key_values_unstable(List, VVs, K, KVs, GroupedList).
    concat_same_key_values_unstable([K-V|List], Vs0, _K, Vs0, [K-Vs|GroupedList]) :-
        concat_same_key_values_unstable(List, V, K, Vs, GroupedList).


% the values are assumed to be lists.
% stable version: concats the lists in their original order
concat_same_key_values_stable([], []).
concat_same_key_values_stable([K-V|List], [K-VKVs|GroupedList]) :-
	append(V, KVs, VKVs),
        concat_same_key_values_stable(List, K, KVs, GroupedList).

    concat_same_key_values_stable([], _, [], []).
    concat_same_key_values_stable([K-V|List], K, VKVs, GroupedList) :- !,
	append(V, KVs, VKVs),
        concat_same_key_values_stable(List, K, KVs, GroupedList).
    concat_same_key_values_stable([K-V|List], _K, [], [K-VKVs|GroupedList]) :-
	append(V, KVs, VKVs),
        concat_same_key_values_stable(List, K, KVs, GroupedList).


% the values are assumed to be sorted lists.
merge_same_key_values(MultiKeyValues, KeyMergedValues) :-
    	group_same_key_values(MultiKeyValues, KeyMultiValues),
	( foreach(K-MultiValues,KeyMultiValues),
	  foreach(K-MergedValues,KeyMergedValues)
	do
	    merge_sorted_lists(MultiValues, [], MergedValues)
	).


:- export strip_keys/2.
strip_keys([], []).
strip_keys([_-X|KXs], [X|Xs]) :-
	strip_keys(KXs, Xs).


:- export select/4.
select(X, [X|Xs], Y, [Y|Xs]).
select(X, [Z|Xs], Y, [Z|Ys]) :-
	select(X, Xs, Y, Ys).


:- export selectchk/4.
selectchk(X, Xs, Y, Ys) :-
	select(X, Xs, Y, Ys), !.


:- export project_arg/3.
project_arg(_I, [], []).
project_arg(I, [X|Xs], [Y|Ys]) :-
	arg(I, X, Y),
	project_arg(I, Xs, Ys).


%----------------------------------------------------------------------
% top_sort(+AdjArray, -Ordered, -UpEdges)
% AdjArray is array of lists of adjacent nodes.
% This returns a topological order in Ordered.
% If the graph was cyclic, the order is computed under the assumption
% that the nodes in UpEdges have been removed from the graph.
% Note that for unordered items, the algorithm will reverse the
% previously existing order!
%----------------------------------------------------------------------

:- export top_sort/4.
top_sort(Adj, PreOrdered, Ordered, UpEdges) :-
	functor(Adj, F, MaxNode),
	functor(Seen, F, MaxNode),
	(
%	    for(StartNode, 1, MaxNode),
	    foreach(StartNode, PreOrdered),
	    fromto([], Ordered0, Ordered1, Ordered),
	    fromto([], UpEdges0, UpEdges1, UpEdges),
	    param(Adj,Seen)
	do
	    arg(StartNode, Seen, NodeSeen),
	    ( var(NodeSeen) ->
		topsort_visit(_, Adj, Seen, [StartNode], [],
				Ordered0, Ordered1, UpEdges0, UpEdges1)
	    ;
	    	Ordered0 = Ordered1,
	    	UpEdges0 = UpEdges1
	    )
	).

    topsort_visit(_From, _Adj, _Seen, [], [], Ordered0, Ordered, UpEdges0, UpEdges) :- !,
    	Ordered = Ordered0,
    	UpEdges = UpEdges0.
    topsort_visit(From, Adj, Seen, [], [[Node|Edges]|Stack], Ordered0, Ordered, UpEdges0, UpEdges) :- !,
	arg(Node, Seen, seen(done)),	% mark done
	topsort_visit(From, Adj, Seen, Edges, Stack, [Node|Ordered0], Ordered, UpEdges0, UpEdges).
    topsort_visit(From, Adj, Seen, EdgeEdges, Stack, Ordered0, Ordered, UpEdges0, UpEdges) :-
	EdgeEdges = [Node|Edges],
	arg(Node, Seen, NodeSeen),
	( var(NodeSeen) ->
	    NodeSeen = seen(_),		% mark visited
	    arg(Node, Adj, Successors),
	    topsort_visit(Node, Adj, Seen, Successors, [EdgeEdges|Stack], Ordered0, Ordered, UpEdges0, UpEdges)
	;
	    NodeSeen = seen(Done),
	    ( nonvar(Done) ->
		topsort_visit(From, Adj, Seen, Edges, Stack, Ordered0, Ordered, UpEdges0, UpEdges)
	    ;
		% we have an upward edge, i.e. a cycle: record and ignore it
		topsort_visit(From, Adj, Seen, Edges, Stack, Ordered0, Ordered, [From->Node|UpEdges0], UpEdges)
	    )
	).


%----------------------------------------------------------------------
% Chunks and Call Positions
% A chunk is a sequence of simple goals that ends with a regular goal
% or the end of the clause. All goals in a chunk have the same call
% position.  A call position is a list of positive integers (odd length):
%	[Pos]
%	[Pos,Branch1,Pos1,...,BranchN,PosN]
% In a flat clause, the call positions are [1] .. [N].
% If there are disjunctions, the parallel branches are distinguished by
% a branch number appended to the disjunction's call position. The chunks
% inside the branch again have a chunk number appended to the branch id.
% So [5,2,3] is the 3rd chunk in the second alternative of the disjunction
% in toplevel chunk 5.
%----------------------------------------------------------------------

:- export
	compare_pos/3,
	common_pos/3,
	init_branch/1,
	in_following_branch_guard/2,
	parallel_branch/2,
	same_call_pos/4,
	new_call_pos/4,
	new_branch/4,
	subsumes_pos/2,
	pos_branch/2,
	prev_call_pos/2,
	print_call_pos/2.

init_branch([]).

new_branch(CallPos, BranchNr0, BranchNr, Branch) :-
	BranchNr is BranchNr0+1,
	append(CallPos, [BranchNr0], Branch).


new_call_pos(Branch, CallNr0, CallNr, CallPos) :-
	CallNr is CallNr0+1,
	append(Branch, [CallNr0], CallPos).


same_call_pos(Branch, CallNr, CallNr, CallPos) :-
	append(Branch, [CallNr], CallPos).

prev_call_pos(CallPos, PrevCallPos) :-
	once append(Branch, [Pos], CallPos),
	Pos1 is Pos-1,
	append(Branch, [Pos1], PrevCallPos).

compare_pos([L|Ls], [R|Rs], Result) :-	% fails if not ordered
	( L < R ->
	    Result = (<)
	; L > R ->
	    Result = (>)
	;
	    compare_branches(Ls, Rs, Result)
	).

    compare_branches([], [], =).
    compare_branches([Branch|Ls], [Branch|Rs], Res) :-
    	compare_pos(Ls, Rs, Res).

% Test if CutPos is in a first chunk of a disjunction following SaveCutPos
in_following_branch_guard(SaveCutPos, CutPos) :-
	once append(CommonBranch, [Pos], SaveCutPos),
	DisjPos is Pos+1,
	append(CommonBranch, [DisjPos,_,1], CutPos).


% The positions are definitely in parallel branches
% Fails if comparable (<,>,=) or insufficient information
parallel_branch([P,B1|Ls], [P,B2|Rs]) :-
	( B1 == B2 ->
	    parallel_branch(Ls, Rs)
	;
	    true
	).


common_pos([L|Ls], [R|Rs], CCs) :-	% error if comparable
	( L == R ->
	    CCs = [L|Cs],
	    common_branch(Ls, Rs, Cs)
	;
	    writeln(error,"Error: comparable positions in common_pos/3"),
	    abort
	).

    common_branch([], _, CCs) :- !, CCs=[].
    common_branch(_, [], CCs) :- !, CCs=[].
    common_branch([LBranch|Ls], [RBranch|Rs], CCs) :-
    	( LBranch == RBranch ->
	    CCs = [LBranch|Cs],
	    common_pos(Ls, Rs, Cs)
	;
	    CCs = []
	).


subsumes_pos(L, R) :-
	append(L, _, R).


pos_branch(P, B) :-
	append(B, [_], P), !.


print_call_pos(_Stream, []).
print_call_pos(Stream, [Pos|Branch]) :-
	write(Pos),
	( Branch = [B|More] ->
	    write([B]),
	    print_call_pos(Stream, More)
	;
	    true
	).


%----------------------------------------------------------------------
% Environment activity maps 
% These are bitmaps indicating which environment slots are uninitialised
% If slot y(Y) is active, bit 2^Y-1 is set.
%----------------------------------------------------------------------

:- export decode_activity_map/2.
decode_activity_map(Map, _Unknown) :-
	var(Map), !.
decode_activity_map(Map, List) :-
	(
	    fromto(Map, Map1, Map2, 0),
	    fromto(List, List1, List2, []),
	    count(I,1,_)
	do
	    Map2 is Map1 >> 1,
	    ( Map1 /\ 1 =:= 0 -> List1=List2 ; List1=[I|List2] )
	).


%----------------------------------------------------------------------
% Decision trees
%
% Decision trees store key-value pairs.
% Keys are hierarchical lists of the form [class,subclass,subsubclass,...]
%   where the list elements can be any ground term. Values are arbitrary.
% A decision tree will have a node for every prefix of every key.
% Every tree node stores a set of values, which are the default values
%   for the key prefix that the node represents.  Exceptions are stored
%   in the child nodes, which share the prefix but are more specific.
% For lookup, we get the values from the node which represents the
%   longest prefix of the given lookup key.
%----------------------------------------------------------------------

:- lib(hash).
:- local struct(dt(class,values,except,closed)).


:- export dt_init/1.
dt_init(dt{class:univ,values:[],except:Hash,closed:no}) :-
	hash_create(Hash).


dt_init(Class, Values, dt{class:Class,values:Values,except:Hash,closed:no}) :-
	hash_create(Hash).


:- export dt_add/4.
:- comment(dt_add/4, [
    summary:"Add value for a key",
    amode:(dt_add(+,++,?,+) is det),
    fail_if:"No entry for Key in the tree",
    args:[
	"DT":"A decision tree (destructive update)",
	"Key":"A key (list of class names)",
	"Value":"Arbitrary term",
	"Final":"Atom 'yes' or 'no'"
    ],
    desc:"Value will be added as an additional value for Key,
    including all its exceptions, except for those (sub-)keys
    that have previously been marked as final.
    
    If Final is 'yes', then Key will be marked as final and not
    accept any future value additions."
]).
dt_add(Dt, [], Value, Final) :- !,
	% the value is for all nodes in this subtree (except closed ones)
	Dt = dt{values:Values,except:ExceptTable,closed:Closed},
	( Closed == no ->
	    % add the value to this node
	    setarg(values of dt, Dt, [Value|Values]),
	    setarg(closed of dt, Dt, Final)
	;
	    % Subtree already closed, nothing more can be added
	    true
	),
	% add the value to all subnodes (exceptions)
	hash_list(ExceptTable, _SubClasses, Excepts),
	(
	    foreach(Except,Excepts),
	    param(Value,Final)
	do
	    dt_add(Except, [], Value, Final)
	).
dt_add(Dt, [Class|SubClass], Value, Final) :-
	% [create and] descend into the subtree according to key (unless closed)
	Dt = dt{values:DefaultValues,except:ExceptTable,closed:Closed},
	( Closed == no ->
	    ( hash_get(ExceptTable, Class, ClassDt) ->
		true
	    ;
		dt_init(Class, DefaultValues, ClassDt),
		hash_set(ExceptTable, Class, ClassDt)
	    ),
	    dt_add(ClassDt, SubClass, Value, Final)
	;
	    true
	).


% add Value everywhere except Key
:- export dt_addnot/4.
dt_addnot(_Dt, [], _Value, _Final).
dt_addnot(Dt, [Class|SubClass], Value, Final) :-
	% [create and] descend into the subtree according to key (unless closed)
	Dt = dt{values:DefaultValues,except:ExceptTable,closed:Closed},
	( Closed == no ->
	    setarg(values of dt, Dt, [Value|DefaultValues]),
	    % add the value to all subnodes (except the one for Class)
	    hash_list(ExceptTable, ExceptClasses, Excepts),
	    (
		foreach(Except,Excepts),
		foreach(ExceptClass,ExceptClasses),
		param(Class,SubClass,Value,Final)
	    do
		( Class == ExceptClass ->
		    true
		;
		    dt_addnot(Except, SubClass, Value, Final)
		)
	    ),
	    ( hash_get(ExceptTable, Class, ClassDt) ->
		true
	    ;
		dt_init(Class, DefaultValues, ClassDt),
		hash_set(ExceptTable, Class, ClassDt)
	    ),
	    dt_addnot(ClassDt, SubClass, Value, Final)
	;
	    true
	).


dt_lookup(dt{values:DefaultValues,except:ExceptTable}, Key, Values, ExceptCount) :-
	( Key = [Class|SubClasses] ->
	    hash_get(ExceptTable, Class, ClassDt),	% may fail
	    dt_lookup(ClassDt, SubClasses, Values, ExceptCount)
	;
	    Values = DefaultValues,
	    hash_count(ExceptTable, ExceptCount)
	).


:- export dt_lookup2/4.
:- comment(dt_lookup2/4, [
    summary:"Lookup 2 levels: default values for key and immediate exceptions",
    amode:(dt_lookup2(+,++,-,-) is semidet),
    fail_if:"No entry for Key in the tree",
    args:[
	"DT":"A decision tree",
	"Key":"A key (list of class names)",
	"Values":"Output: list of default values for key",
	"Exceptions":"Output: list of SubKey-DefaultValues pairs"
    ]
]).

dt_lookup2(dt{values:RevValues,except:ExceptTable}, Key, Values, Exceptions) :-
	( Key = [Class|SubClasses] ->
	    hash_get(ExceptTable, Class, ClassDt),	% may fail
	    dt_lookup2(ClassDt, SubClasses, Values, Exceptions)
	;
	    % the value lists are in reverse order of insertion, reverse them
	    reverse(RevValues, Values),
	    hash_list(ExceptTable, SubKeys, SubDts),
	    (
		foreach(SubKey,SubKeys),
		foreach(dt{values:RevSubValues},SubDts),
		foreach(SubKey-SubValues,Exceptions)
	    do
		reverse(RevSubValues, SubValues)
	    )
	).


:- export dt_values/2.
dt_values(Dt, ValueGroups) :-
	dt_values(Dt, ValueGroups, []).

dt_values(dt{values:RevValues,except:ExceptTable}, ValueGroups, ValueGroups0) :-
	( RevValues = [] ->
	    ValueGroups = ValueGroups1
	;
	    reverse(RevValues, Values),
	    ValueGroups = [Values|ValueGroups1]
	),
	hash_list(ExceptTable, _SubKeys, SubDts),
	(
	    foreach(SubDt,SubDts),
	    fromto(ValueGroups1,ValueGroups2,ValueGroups3,ValueGroups0)
	do
	    dt_values(SubDt, ValueGroups2, ValueGroups3)
	).


:- export dt_list/2.
dt_list(Dt, KeysValues) :-
	dt_list(Dt, KeysValues, [], []).

dt_list(dt{values:RevValues,except:ExceptTable}, KeysValues, KeysValues0, Key) :-
	reverse(RevValues, Values),
	KeysValues = [Key-Values|KeysValues1],
	hash_list(ExceptTable, _SubKeys, SubDts),
	(
	    foreach(SubDt,SubDts),
	    fromto(KeysValues1,KeysValues2,KeysValues3,KeysValues0),
	    param(Key)
	do
	    SubDt = dt{class:SubClass},
	    append(Key, [SubClass], SubKey),
	    dt_list(SubDt, KeysValues2, KeysValues3, SubKey)
	).