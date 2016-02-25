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
% Copyright (C) 2006,2007 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf.
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Component:	ECLiPSe III compiler
% Version:	$Id: ecl_compiler.ecl,v 1.24 2013/02/26 02:10:06 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(ecl_compiler).

:- comment(summary,	"ECLiPSe III compiler - toplevel predicates").
:- comment(copyright,	"Cisco Technology Inc").
:- comment(author,	"Joachim Schimpf").
:- comment(date,	"$Date: 2013/02/26 02:10:06 $").

:- comment(desc, html("
    This module contains the toplevel predicates for invoking the
    compiler. This is where the different compiler passes are chained
    together, and where the compiler options are defined.  It also
    contains the code to process source files, and to interpret
    directives and queries.
    <P>
    The top-level interfaces to the compiler are: compile/1,2 for
    compilation from files, compile_stream/1,2 for compiling from an
    arbitrary stream, and compile_term/1,2 for compiling data.
    <P>
    The predicates themselves are documented in the kernel/database
    section of the reference manual.
")).


:- use_module(compiler_common).
:- use_module(compiler_normalise).
:- use_module(compiler_analysis).
:- use_module(compiler_peephole).
:- use_module(compiler_codegen).
:- use_module(compiler_varclass).
:- use_module(compiler_indexing).
:- use_module(compiler_regassign).
:- use_module(source_processor).

:- lib(asm).
:- lib(hash).
:- lib(module_options).

:- import
	collect_discontiguous_predicates/2,
	deregister_compiler/0,
	expand_clause_annotated/4,
	implicit_local/2,
	bip_error/1,
	record_discontiguous_predicate/4,
	record_inline_source/4,
	register_compiler/1,
	set_default_error_handler/2
   from sepia_kernel.

:- pragma(system).


%----------------------------------------------------------------------
% Compiler Options
%----------------------------------------------------------------------

compiler_options_setup(File, OptionList, Options) :-
	( atom(File) -> atom_string(File, FileS)
	; string(File) -> FileS = File
	; term_string(File, FileS)
	),

	( OptionList = options{} ->
	    Options00 = OptionList
	; get_options(OptionList, Options00)@compiler_common ->
	    true
	;
	    printf(error, "Invalid option list: %w%n", [OptionList]),
	    print_default_options(error)@compiler_common,
	    abort
	),

	Options00 = options{outdir:OutDir,srcroot:SrcRoot},
	( SrcRoot == "" ->
	    Options0 = Options00
	;
	    canonical_path_name(SrcRoot, CanSrcRoot),
	    concat_string([CanSrcRoot], CanSrcRootString),
	    update_struct(options, [srcroot:CanSrcRootString], Options00, Options0)
	),
	( Options0 = options{output:listing(LstFile)} ->
	    open(LstFile,write,Stream),
	    update_struct(options, [output:print(Stream)], Options0, Options)
	; Options0 = options{output:listing} ->
	    default_output_file(FileS, OutDir, '.lst', LstFile),
	    open(LstFile,write,Stream),
	    update_struct(options, [output:print(Stream)], Options0, Options)
	; Options0 = options{output:eco(EcoFile)} ->
	    open(EcoFile,write,Stream,[end_of_line(lf)]),
	    update_struct(options, [output:eco_to_stream(Stream)], Options0, Options)
	; Options0 = options{output:eco} ->
	    get_flag(eclipse_object_suffix, ECO),
	    default_output_file(FileS, OutDir, ECO, EcoFile),
	    open(EcoFile,write,Stream,[end_of_line(lf)]),
	    update_struct(options, [output:eco_to_stream(Stream)], Options0, Options)
	; Options0 = options{output:asm(AsmFile)} ->
	    open(AsmFile,write,Stream),
	    update_struct(options, [output:asm_to_stream(Stream)], Options0, Options)
	; Options0 = options{output:asm} ->
	    default_output_file(FileS, OutDir, '.asm', AsmFile),
	    open(AsmFile,write,Stream),
	    update_struct(options, [output:asm_to_stream(Stream)], Options0, Options)
	;
	    Options = Options0
	).

    default_output_file(InFile, OutDir, Suffix, OutFile) :-
	pathname(InFile, Dir, Base, _Suffix),
	( concat_string([OutDir], "") -> 
	    concat_string([Dir,Base,Suffix], OutFile)
	;
	    concat_string([OutDir,/,Base,Suffix], OutFile)
	).


compiler_options_cleanup(Options) :-
    	( Options = options{output:print(Stream)} ->
	    close(Stream)
    	; Options = options{output:eco_to_stream(Stream)} ->
	    close(Stream)
    	; Options = options{output:asm_to_stream(Stream)} ->
	    close(Stream)
	;
	    true
	).


% ----------------------------------------------------------------------
% Compile a single predicate.
% 
% Takes a list of clauses (which must all be for the same predicate),
% In case of error, succeed with Size = -1.0Inf.
% ----------------------------------------------------------------------

compile_predicate(ModulePred, Clauses, AnnClauses, SourcePos, PredsSeen, Options, Size) :-
	block(
	    compile_predicate1(ModulePred, Clauses, AnnClauses, SourcePos,
				PredsSeen, Options, Size),
	    abort_compile_predicate,
	    Size = -1.0Inf),
	( var(Size) -> Size=0 ; true ).


compile_predicate1(_, [], _, _, _, _, CodeSize) :- !, CodeSize = 0.
compile_predicate1(ModulePred, Clauses0, AnnClauses0, SourcePos, PredsSeen, Options, CodeSize) :-
	message(compiling(ModulePred), Options),
	ModulePred = Module:Pred,
	Pred = N/A,
	( atom(N), integer(A) -> true
	; compiler_event(#illegal_head, SourcePos, _Ann, N, Module)
	),
	verify (Clauses0 = [Clause|_], extract_pred(Clause, Pred)),
	legal_pred_definition(Pred, SourcePos, Module, Options),
	% Do inlining/goal expansion. This is done here rather than in the
	% source_processor to make it controllable via pragmas.
	( Options = options{expand_goals:on} ->
	    expand_clause_goals(Clauses0, AnnClauses0, Clauses, AnnClauses, Module)
	;
	    Clauses = Clauses0, AnnClauses = AnnClauses0
	),
	% Distinguish dynamic/discontiguous/static
	( local_get_flag(Pred, stability, dynamic, Module) ->
	    CodeSize = 0,
	    ( foreach(Clause, Clauses), param(SourcePos,Options,Module) do
		process_query(SourcePos, (?-assertz(Clause)), Options, Module)
	    )

	; record_discontiguous_predicate(Pred, Clauses, AnnClauses, Module) ->
	    % will be compiled later via compile_discontiguous_preds/5
	    CodeSize = 0

	; check_redefinition(ModulePred, PredsSeen, SourcePos, Options) ->
	    compile_static_predicate(Pred, Clauses, AnnClauses, SourcePos, Options, CodeSize, Module)
	;
	    CodeSize = 0
	).


compile_static_predicate(_Pred, [], _AnnClauses, _SourcePos, _Options, CodeSize, _Module) ?- !,
	CodeSize = 0.
compile_static_predicate(Pred, Clauses, AnnClauses, SourcePos, Options, CodeSize, Module) :-
	compile_pred_to_wam(Pred, Clauses, AnnClauses, WAM, Options, Module),
	pred_flags(Options, Flags),
	load_compiled_code(Pred, WAM, CodeSize, Flags, SourcePos, Options, Module),
	output_compiled_code(Pred, WAM, Clauses, CodeSize, Flags, SourcePos, Options, Module),
	( var(CodeSize) -> CodeSize=0 ; true ).


compile_discontiguous_preds(Module, SourcePos, Options, Size0, Size) :-
	collect_discontiguous_predicates(Module, NonContigPreds),
	(
	    foreach(Pred-ClausePairs,NonContigPreds),
	    fromto(Size0,Size1,Size2,Size),
	    param(Module, SourcePos, Options)
	do
	    (
		local_get_flag(Pred, source_file, OldFile, Module),
		SourcePos = source_position{file:NewFile0,line:Line},
		normalised_source_file(NewFile0, Options, NewFileAtom),
		OldFile \== NewFileAtom,
		\+ error(#multifile, (Pred,OldFile,NewFileAtom:Line), Module)
	    ->
		% Seen in other file: if handler fails, don't redefine
		Size2 = Size1
	    ;
		( foreach(Clause-AnnClause,ClausePairs),
		  foreach(Clause,Clauses), foreach(AnnClause,AnnClauses)
		do
		    true
		),
		block(compile_static_predicate(Pred, Clauses, AnnClauses, SourcePos, Options, CodeSize, Module),
			abort_compile_predicate,
			CodeSize = -1.0Inf),
		Size2 is Size1 + CodeSize
	    )
	).


    load_compiled_code(Pred, WAM, CodeSize, Flags, SourcePos, Options, Module) :-
	( ( Options = options{load:all} ; Options = options{load:new}, \+ is_predicate(Pred)@Module) ->
	    % double negation, because asm binds the labels
	    message("Asm and load", 2, Options),
	    \+ \+ asm(Pred, WAM, Flags)@Module,
	    get_flag(Pred, code_size, CodeSize)@Module,
	    set_pred_pos(Pred, SourcePos, Options, Module)
	;
	    true % don't clobber existing code if not loading
	).


    output_compiled_code(Pred, WAM, Clauses, CodeSize, Flags, SourcePos, Options, Module) :-
	( Options = options{output:print} ->
	    printf("%w:%n", [Pred]),
	    print_wam(WAM)

	; Options = options{output:print(Stream)} ->
	    writeclauses(Stream, Clauses),
	    get_stream(output, OldOut),
	    set_stream(output, Stream),
	    print_wam(WAM),
	    set_stream(output, OldOut),
	    writeln(Stream, --------------------)

	; Options = options{output:eco_to_stream(Stream)} ->
	    message("Asm", 2, Options),
	    pasm(WAM, CodeSize, BTPos, Codes),
	    ( portable_object_code(Codes) ->
		true
	    ;
		get_flag(eclipse_object_suffix, ECO),
		machine_bits(BPW),
		printf(warning_output,
		    "WARNING: the generated %w file will only work reliably on %w bit machines!%n",
		    [ECO,BPW])
	    ),
	    CodeArr =.. [[]|Codes],
	    get_pred_pos(SourcePos, Options, File, Line, Offset),
	    StorePred = store_pred(Pred,CodeArr,CodeSize,BTPos,Flags,File,Line,Offset),
	    ( Module == sepia_kernel ->
		% call locally, because :/2 may not be defined yet
		QStorePred = StorePred
	    ;
		QStorePred = sepia_kernel:StorePred
	    ),
	    message("Output", 2, Options),
	    printf(Stream, "%ODQKw.%n", [:-QStorePred])@Module
    

	; Options = options{output:asm_to_stream(Stream)} ->
	    message("Output", 2, Options),
	    pretty_print_asm(WAM, Stream, Pred, Flags, Module)

	; Options = options{output:none} ->
	    true
	;
	    Options = options{output:Junk},
	    printf(error, "Invalid output option: %w%n", [Junk]),
	    abort
	).


    writeclauses(Stream, Clauses) :-
	get_stream_info(Stream, output_options, Opt),
	( delete(numbervars(NV), Opt, Opt0) -> true ; NV=false,Opt0=Opt ),
	set_stream_property(Stream, output_options, [numbervars(true)|Opt0]),
	( foreach(Clause,Clauses),param(Stream) do
	    \+ \+ (
		numbervars(Clause, 0, _),
		writeclause(Stream, Clause)
	    )
	),
	nl(Stream),
	set_stream_property(Stream, output_options, [numbervars(NV)|Opt0]).

    numbervars('$VAR'(N), N, N1) :- !,
	N1 is N + 1.
    numbervars(Term, N0, N) :-
	( foreacharg(Arg,Term), fromto(N0,N1,N2,N) do
	    numbervars(Arg, N1, N2)
	).


    pretty_print_asm(WAM, Stream, Pred, Flags, Module) :-
        printf(Stream, ":- asm:asm(%DQKw, [%n", [Pred])@Module,
        ( fromto(WAM, [Instr|Rest],Rest, []), param(Stream, Module) do
            ( Instr = label(_) ->
                printf(Stream, "%DQKw", [Instr])@Module % no indent for labels
            ;
                printf(Stream, "	%DQKw", [Instr])@Module
            ),
            (Rest \== [] -> writeln(Stream, ",") ; nl(Stream))
        ),
        printf(Stream, "], %DQKw).%n%n", [Flags]).


            
    pred_flags(options{debug:Debug,system:System,skip:Skip}, Flags) ?-
	( Debug==on -> Flags0 = 16'00080000 ; Flags0 = 0 ),			%'% DEBUG_DB
	( System==on -> Flags1 is Flags0 \/ 16'40000000 ; Flags1 = Flags0 ),	%'% SYSTEM
	( Skip==on -> Flags is Flags1 \/ 16'00040000 ; Flags = Flags1 ).	%'% DEBUG_SK


    set_pred_flags(options{debug:Debug,system:System,skip:Skip}, Pred, Module) ?-
	set_flag(Pred, debugged, Debug)@Module,
	set_flag(Pred, skip, Skip)@Module,
	( System==on -> Type = built_in ; Type = user ),
	set_flag(Pred, type, Type)@Module.


    set_pred_pos(Pred, source_position{file:File,line:Line,offset:Offset}, Options, Module) :- !,
	normalised_source_file(File, Options, SrcFile),
    	set_flag(Pred, source_file, SrcFile)@Module,
	( Options = options{debug:on} ->
	    set_flag(Pred, source_line, Line)@Module,
	    set_flag(Pred, source_offset, Offset)@Module
	;
	    set_flag(Pred, source_line, 0)@Module,
	    set_flag(Pred, source_offset, 0)@Module
	).
    set_pred_pos(_Pred, _Pos, _Options, _Module).


    get_pred_pos(source_position{file:File,line:Line0,offset:Offset0}, Options, SrcFile, Line, Offset) ?- !,
	( Options = options{debug:on} ->
	    Line = Line0, Offset = Offset0
	;
	    % hide position in file to avoid irrelevant diffs in .eco files
	    Line = 0, Offset = 0
	),
	normalised_source_file(File, Options, SrcFile).
    get_pred_pos(_, _, 0, 0, 0).


    normalised_source_file(File, options{srcroot:Root,debug:Debug}, NormFileAtom) ?-
	% File is canonical, and either atom or string
	concat_string([File], FileS),
	( Debug==off, substring(FileS, 0, PrefixLen, _, Root) ->
	    substring(FileS, PrefixLen, _, 0, RelFileS)
	;
	    RelFileS = FileS
	),
	concat_atom([RelFileS], NormFileAtom).


    % Fail if this is a redefinition that we want to ignore
    check_redefinition(ModulePred, PredsSeen, SourcePos, Options) :-
	ModulePred = Module:Pred,
    	( hash_contains(PredsSeen, ModulePred) ->
	    % Non-consecutive clauses: if handler fails, don't redefine
	    compiler_event(#consecutive, SourcePos, _Ann, Pred, Module)
	; 	
	    local_get_flag(Pred, source_file, OldFile, Module),
	    SourcePos = source_position{file:NewFile0,line:Line},
	    normalised_source_file(NewFile0, Options, NewFileAtom),
	    OldFile \== NewFileAtom
	->
	    % Seen in other file: if handler fails, don't redefine
	    error(#multifile, (Pred,OldFile,NewFileAtom:Line), Module)
	;
	    true
	),
	hash_set(PredsSeen, ModulePred, []).


    % Make sure we can define this predicate
    legal_pred_definition(Pred, SourcePos, Module, Options) :-
	( Options = options{load:all} ->
	    % does all checks, hiding imports, etc
	    ( implicit_local(Pred, Module) ->
		true
	    ;
		print_error_location(error, _Ann, SourcePos),
		block(bip_error(Pred)@Module, _Any, exit_block(abort_compile_predicate))
	    )
	;
	    % If not loading, don't create the local predicate.  Problem:
	    % subsequent calls to get_flag/3 may trigger lazy imports for
	    % preds that would be hidden when loading, and so give wrong
	    % flags. That's why we have to use local_get_flag/3 instead.
	    true
	),
	( local_get_flag(Pred, tool, on, Module) ->
	    block(error(#tool_redef, Pred, Module), _Any, exit_block(abort_compile_predicate))
	; true ),
	( local_get_flag(Pred, parallel, on, Module),  Options = options{warnings:on} ->
	    printf(warning_output, "Parallel-declaration ignored for %w%n", [Module:Pred])
	; true ).


    % Use this for looking up properties of the predicate being compiled
    local_get_flag(Pred, Property, Value, Module) :-
    	( get_flag(Pred, definition_module, Module)@Module ->
	    get_flag(Pred, Property, Value)@Module
	; 
	    get_flag_default(Property, Value)
	).

	get_flag_default(tool, off).
	get_flag_default(parallel, off).
	get_flag_default(stability, static).
	%get_flag_default(source_file, _) :- fail.


% Compile a predicate (list of clauses) to WAM code list
% This chains together the various stages of the comiler

compile_pred_to_wam(Pred, Clauses, AnnCs, FinalCode, Options, Module) :-

	% Create our normal form
	message("Normalize", 2, Options),
	normalize_clauses_annotated(Clauses, AnnCs, NormPred0, NVars, Options, Module),
%	print_normalized_clause(output, NormPred0),

	% If this predicate is to be unfolded, record its (de)normalised source
	( get_flag(Pred, inline, unfold/_)@Module ->
	    denormalize_pred(NormPred0, NVars, Head, SingleClause, AnnSingleClause),
	    record_inline_source(Head, SingleClause, AnnSingleClause, Module)
	;
	    true
	),

	% Do some intra-predicate flow analysis
	message("Analysis", 2, Options),
	binding_analysis(NormPred0),

	% Here we could have a simplification pass, exploiting
	% information from the analysis phase.

	% Indexing_transformation (needs info from binding_analysis)
	message("Indexing", 2, Options),
	indexing_transformation(NormPred0, NormPred, Options),

	% Variable classification
	% classify_variables must be after indexing transformation, because
	% indexing_transformation introduces extra variable occurrences.
	% Classifies void, temp and permanent vaiables, and assigns environment
	% slots to the permanent ones. Also adds disjunction pseudo-args.
	message("Varclass", 2, Options),
	classify_variables(NormPred, 0, Options),

	( Options = options{print_normalised:on} ->
	    print_normalized_clause(output, NormPred)
	;
	    true
	),

	% Code generation
	message("Codegen", 2, Options),
	generate_code(NormPred, RawCode, AuxCode, Options, Module:Pred),
	( Options = options{print_raw_code:on} ->
	    print_annotated_code(RawCode)
	;
	    true
	),

	% Register allocation
	message("Regassign", 2, Options),
	assign_am_registers(RawCode, RegCode, AuxCode),
	( Options = options{print_raw_code:on} ->
	    print_annotated_code(RegCode)
	;
	    true
	),

	% WAM level postprocessing
	message("Simplify", 2, Options),
	simplify_code(RegCode, FinalCode, Options),
	( Options = options{print_final_code:on} ->
	    print_annotated_code(FinalCode)
	;
	    true
	).


%----------------------------------------------------------------------
% Error handling
%----------------------------------------------------------------------

?- set_default_error_handler(#consecutive, compiler_err_fail_handler/2).
?- reset_event_handler(#consecutive).
?- set_default_error_handler(#illegal_head, compiler_err_abort_handler/2).
?- reset_event_handler(#illegal_head).
?- set_default_error_handler(#illegal_goal, compiler_err_abort_handler/2).
?- reset_event_handler(#illegal_goal).

compiler_err_abort_handler(Error, Culprit) :-
	print_compiler_message('ERROR', Error, Culprit),
	exit_block(abort_compile_predicate).

compiler_err_fail_handler(Error, Culprit) :-
	print_compiler_message('ERROR', Error, Culprit),
	fail.

compiler_warn_cont_handler(Error, Culprit) :-
	print_compiler_message('WARNING', Error, Culprit).

    print_compiler_message(Severity, Error, Term@Location) ?-
	severity_stream(Severity, Stream),
	printf(Stream, "%w: ", [Severity]),
	print_location(Stream, Location),
	error_id(Error, Message), 
	printf(Stream, "%w: ", [Message]),
	get_flag(output_options, OutputOptions),
	write_term(Stream, Term, OutputOptions),
	nl(Stream),
	flush(Stream).

    severity_stream('WARNING', warning_output).
    severity_stream('ERROR', error).


:- set_default_error_handler(#multifile, redef_other_file_handler/2).
?- reset_event_handler(#multifile).

redef_other_file_handler(_, (Pred, OldFile0, Location)) :-
	print_location(warning_output, Location),
	local_file_name(OldFile0, OldFile),
	printf(warning_output, "WARNING: %Kw replaces previous definition in file %w%n",
		 [Pred,OldFile]).


%----------------------------------------------------------------------
% From-file compiler
%----------------------------------------------------------------------

:- export
	compile/1, compile_/2,
	compile/2, compile_/3,
	compile_stream/1, compile_stream_/2,
	compile_stream/2, compile_stream_/3.

:- tool(compile/1, compile_/2).
:- set_flag(compile/1, type, built_in).
compile_(File, Module) :-
    compile_(File, [], Module).


:- tool(compile_stream/1, compile_stream_/2).
:- set_flag(compile_stream/1, type, built_in).
compile_stream_(Stream, Module) :-
    compile_stream_(Stream, [], Module).


:- tool(compile_stream/2, compile_stream_/3).
:- set_flag(compile_stream/2, type, built_in).
compile_stream_(Stream, Options, Module) :-
    compile_source(stream(Stream), Options, Module).


:- tool(compile/2, compile_/3).
:- set_flag(compile/2, type, built_in).

compile_(Sources, OptionListOrModule, CM) :- Sources = [_|_], !,
	( foreach(Source,Sources), param(OptionListOrModule, CM) do
	    compile_source(Source, OptionListOrModule, CM)
	).
compile_(Source, OptionListOrModule, CM) :-
	compile_source(Source, OptionListOrModule, CM).


compile_source(Source, OptionListOrModule, CM) :-
	% The subcall is needed to make coroutining in the compiler work,
	% and to give compiled queries a standard environment to run in.
	subcall(compile_source1(Source, OptionListOrModule, CM), _Delayed).

compile_source1(Source, OptionListOrModule, CM) :-
	valid_source(Source),
	!,
	% for backward compatibility, allow compile(Source, Module)
	% with the module being created if it does not exist
	( atom(OptionListOrModule), OptionListOrModule \== [] ->
	    Module = OptionListOrModule, OptionList = [],
	    ( current_module(Module) -> true ; create_module(Module) )
	;
	    Module = CM, OptionList = OptionListOrModule
	
	),
	compiler_options_setup(Source, OptionList, Options),
	source_processor_options_setup(Options, OpenOptions, CloseOptions),
	error(#start_compiler, Source, CM),
	cputime(Tstart),
	( source_open(Source, [with_annotations|OpenOptions], SourcePos0)@Module ->
	    SourcePos0 = source_position{stream:Stream,file:CanonicalFile},
	    get_stream_info(Stream, device, Device),
	    Options = options{load:Loading},
	    register_compiler(args(Term,Ann,Loading)-(ecl_compiler:compile_term_annotated(Term,Ann,Options))),
	    hash_create(PredsSeen),
	    (
		fromto(begin, _, Class, end),
		fromto(SourcePos0, SourcePos1, SourcePos2, SourcePosEnd),
		fromto(SourcePos0, PredPos1, PredPos2, _),
		fromto(ClauseTail, Clauses0, Clauses1, []),
		fromto(ClauseTail, ClauseTail0, ClauseTail1, []),
		fromto(AnnClauseTail, AnnClauses0, AnnClauses1, []),
		fromto(AnnClauseTail, AnnClauseTail0, AnnClauseTail1, []),
                fromto(0, Size0, Size2, Size3), 
		fromto(none, Pred0, Pred1, none),
		param(PredsSeen,Options,Module)
	    do
		source_read(SourcePos1, SourcePos2, Class, SourceTerm),
		SourcePos1 = source_position{module:PosModule},
		SourceTerm = source_term{term:Term,annotated:Ann},

		( Class = clause ->
		    accumulate_clauses(Term, Ann, PosModule, Options, SourcePos1, PredsSeen,
			Size0, Pred0, PredPos1, Clauses0, ClauseTail0, AnnClauses0, AnnClauseTail0,
			Size2, Pred1, PredPos2, Clauses1, ClauseTail1, AnnClauses1, AnnClauseTail1)

		; Class = comment ->		% comment, ignore
                    Size0 = Size2,
                    Pred1 = Pred0,
		    ClauseTail1 = ClauseTail0,
		    Clauses1 = Clauses0,
		    AnnClauseTail1 = AnnClauseTail0,
		    AnnClauses1 = AnnClauses0,
		    PredPos2 = PredPos1

		; % other classes are taken as predicate separator
		    ClauseTail0 = [],		% compile previous predicate
		    AnnClauseTail0 = [],
		    compile_predicate(Pred0, Clauses0, AnnClauses0, PredPos1, PredsSeen, Options, CSize),
                    Size1 is Size0 + CSize,
                    Clauses1 = ClauseTail1,
		    AnnClauses1 = AnnClauseTail1,
		    Pred1 = none,
		    PredPos2 = none,

		    block(handle_nonclause(Class, Term, Ann, SourcePos1, Size1, Size2, Options, PosModule, Module),
		    	abort_compile_predicate, Size2 = -1.0Inf)
		)
	    ),

	    % Deal with discontiguous clauses collected above
	    SourcePosEnd = source_position{module:EndModule},
	    compile_discontiguous_preds(EndModule, SourcePosEnd, Options, Size3, Size),

	    % If the compilation was successful, raise various events
	    ( Size >= 0 ->
		% Raise event 149, which executes initialization goals, etc.
		% This must be done before cd-ing back in source_close below.
		% This event is also raised when the module changes mid-file!
		( Options = options{load:none} ->
		    true
		; EndModule == Module ->
		    error(#code_unit_loaded, [], EndModule)
		;
		    error(#code_unit_loaded, [check], EndModule)
		),

		% Raise event 139, which prints the compilation statistics
		Tcompile is cputime-Tstart,
		words_to_bytes(Size, SizeInBytes),
		( Device == file ->
		    concat_atom([CanonicalFile], CanonicalSource)
		;
		    CanonicalSource = source(Device)
		),
		error(#compiled_file, (CanonicalSource, SizeInBytes, Tcompile), EndModule),

		% Raise event 166, which records the compiled_file information
		% (used for recompilation, make/0 etc)
		( Options = options{load:none} ->
		    true
		; atom(CanonicalSource) ->
		    error(#record_compiled_file, CanonicalSource-(ecl_compiler:compile(CanonicalSource, OptionList)), Module)
		;
		    true
		)
	    ;
		true
	    ),
	    deregister_compiler,
	    source_close(SourcePosEnd, CloseOptions),
            compiler_options_cleanup(Options),
	    ( Size >= 0 -> true ;
		printf(error, "Error(s) occurred while compiling %w%n", [Source]),
		abort
	    )
	;
	    compiler_options_cleanup(Options),
	    printf(error, "No such file in %Qw%n", [compile(Source)]),
	    abort
	).
compile_source1(Source, OptionListOrModule, CM) :- var(Source), !,
	error(#inst_fault, compile(Source, OptionListOrModule), CM).
compile_source1(Source, OptionListOrModule, CM) :-
	error(#type_error, compile(Source, OptionListOrModule), CM).

    valid_source(Source) :- atom(Source).
    valid_source(Source) :- string(Source).
    valid_source(library(_)) ?- true.
    valid_source(stream(_)) ?- true.


source_processor_options_setup(options{load:Load,expand_clauses:ClauseExp}, OpenOptions, CloseOptions) :-
	( Load == all ->
	    OpenOptions = [recreate_modules|OO], CloseOptions = [keep_modules]
	;
	    OpenOptions = OO, CloseOptions = []
	),
	( ClauseExp == on ->
	    OO = []
	;
	    OO = [no_clause_expansion]
	).


% Add a single clause or a list of clauses to what we already have.
% If a predicate is finished, compile it.
:- mode accumulate_clauses(+,+,+,+,+,+,+,+,+,?,-,?,-,-,-,-,-,-,-,-).
accumulate_clauses([], [], _Module, _Options, _ClausePos, _PredsSeen,
		Size0, Pred0, PredPos0, PredCl0, PredClTl0, PredClAnn0, PredClAnnTl0,
		Size0, Pred0, PredPos0, PredCl0, PredClTl0, PredClAnn0, PredClAnnTl0) :-
	!.
accumulate_clauses([Term|Terms], [AnnTerm|AnnTerms], Module, Options, ClausePos, PredsSeen,
		Size0, Pred0, PredPos0, PredCl0, PredClTl0, PredClAnn0, PredClAnnTl0,
		Size, Pred, PredPos, PredCl, PredClTl, PredClAnn, PredClAnnTl) :-
	!,
	extract_pred(Term, NA),
	Pred1 = Module:NA,
	( Pred0 == Pred1 ->
	    % another clause for Pred0
	    PredClTl0 = [Term|PredClTl1],
	    PredClAnnTl0 = [AnnTerm|PredClAnnTl1],
	    accumulate_clauses(Terms, AnnTerms, Module, Options, ClausePos, PredsSeen,
	    	Size0, Pred0, PredPos0, PredCl0, PredClTl1, PredClAnn0, PredClAnnTl1,
		Size, Pred, PredPos, PredCl, PredClTl, PredClAnn, PredClAnnTl)
	;
	    % first clause for next predicate Pred1, compile Pred0
	    PredClTl0 = [], PredClAnnTl0 = [],
	    compile_predicate(Pred0, PredCl0, PredClAnn0, PredPos0, PredsSeen, Options, CSize),
            Size1 is Size0 + CSize,
            PredCl1 = [Term|PredClTl1],
	    PredClAnn1 = [AnnTerm|PredClAnnTl1],
	    accumulate_clauses(Terms, AnnTerms, Module, Options, ClausePos, PredsSeen,
	    	Size1, Pred1, ClausePos, PredCl1, PredClTl1, PredClAnn1, PredClAnnTl1,
		Size, Pred, PredPos, PredCl, PredClTl, PredClAnn, PredClAnnTl)
	).
accumulate_clauses(Term, AnnTerm, Module, Options, ClausePos, PredsSeen,
		Size0, Pred0, PredPos0, PredCl0, PredClTl0, PredClAnn0, PredClAnnTl0,
		Size, Pred, PredPos, PredCl, PredClTl, PredClAnn, PredClAnnTl) :-
	accumulate_clauses([Term], [AnnTerm], Module, Options, ClausePos, PredsSeen,
		Size0, Pred0, PredPos0, PredCl0, PredClTl0, PredClAnn0, PredClAnnTl0,
		Size, Pred, PredPos, PredCl, PredClTl, PredClAnn, PredClAnnTl).

    extract_pred((Head :- _), N/A) :- !,
    	( var(Head) -> A=0 ; functor(Head, N, A) ).
    extract_pred((Head ?- _), NA) :- !,
	extract_pred((Head :- _), NA).
    extract_pred(Fact, N/A) :-
    	functor(Fact, N, A).



%----------------------------------------------------------------------
% Queries, directives and pragmas
%----------------------------------------------------------------------

handle_nonclause(Class, Term, Ann, SourcePos1, Size0, Size, Options, PosModule, Module) :-
	( Class = directive ->
	    Size = Size0,
	    ( old_compiler_directive(Term, Options) ->
		true
	    ;
		process_directive(SourcePos1, Term, Options, PosModule)
	    )

	; Class = query ->
	    Size = Size0,
	    process_query(SourcePos1, Term, Options, PosModule)

	; Class = handled_directive ->
	    ( consider_pragmas(Term, Options, PosModule) ->
		Size = Size0,
		emit_directive_or_query(Term, Options, PosModule)
	    ; handle_module_boundary(Term, Options, PosModule, SourcePos1, Module, Size0, Size) ->
		emit_directive_or_query(Term, Options, PosModule)
	    ; Term = (:-meta_attribute(Name,Decls)) ->
		% This is tricky and needs to be split in two:
		% - syntax-relevant part: handled in source_processor, and also emitted as directive
		% - handler part: turned into initialization directive to be executed after loading
		Size = Size0,
		meta_attribute_now_later(Decls, UrgentDecls, HandlerDecls),
		emit_directive_or_query((:-meta_attribute(Name,UrgentDecls)), Options, PosModule),
		process_directive(SourcePos1, (:-local initialization(meta_attribute(Name,HandlerDecls))), Options, PosModule)
	    ;
		Size = Size0,
		emit_directive_or_query(Term, Options, PosModule)
	    )

	; (Class = var ; Class = other) ->
	    compiler_event(#illegal_head, SourcePos1, Ann, Term, Module),
	    Size = -1.0Inf

	; % Class = end_include,end
	    Size = Size0
	).


process_directive(SourcePos, Term, Options, Module) :-
	( current_pragma(iso(strict))@Module, Term=(:-Dir) ->
	    % ISO directives may not be directly callable
	    ( iso_directive(Dir, QDir) ->
		call_directive(SourcePos, (:-QDir), Options, Module),
		emit_directive_or_query((:-QDir), Options, Module)
	    ;
		compiler_error(_Ann, SourcePos, "Non-ISO directive (ignored) %w", [Term])
	    )
	;
	    call_directive(SourcePos, Term, Options, Module),
	    emit_directive_or_query(Term, Options, Module)
	).


process_query(SourcePos, Term, Options, Module) :-
	( Options = options{load:all} ->
	    call_directive(SourcePos, Term, Options, Module)
	;
	    % new/none
	    true
	),
	emit_directive_or_query(Term, Options, Module).


call_directive(SourcePos, Dir, Options, Module) :-
	arg(1, Dir, Goal),
    	block(
	    % negate the Goal - don't bind variables!
	    ( \+ call(Goal)@Module ->
		compiler_warning(_Ann, SourcePos, "Query failed: %w", Dir, Options)
	    ;
	    	true
	    ),
	    Tag,
	    compiler_error(_Ann, SourcePos, "Query exited (%w): %w", [Tag,Dir])
	).


% If we see the beginning of a new module, then finalize OldModule
% (unless it is the compilation's context module, in which case this
% is the first module directive we encounter)
handle_module_boundary((:-module(Module,_,_)), Options, OldModule, SourcePos, TopModule, Size0, Size) ?- !,
	handle_module_boundary((:-module(Module)), Options, OldModule, SourcePos, TopModule, Size0, Size).
handle_module_boundary((:-module(_Module)), Options, OldModule, SourcePos, TopModule, Size0, Size) ?- !,
	( Options = options{load:none} ->
	    Size = Size0
	; OldModule == TopModule ->
	    Size = Size0
	;
	    compile_discontiguous_preds(OldModule, SourcePos, Options, Size0, Size),
	    error(#code_unit_loaded, [check], OldModule)
	).


% Adjust compiler options according to pragmas

consider_pragmas((:-pragma(Pragma)), Options, M) ?-
	consider_pragma(Pragma, Options, M).

consider_pragma(debug, Options, _) :- !,
	setarg(debug of options, Options, on).
consider_pragma(nodebug, Options, _) :- !,
	setarg(debug of options, Options, off).
consider_pragma(system, Options, _) :- !,
	setarg(system of options, Options, on).
consider_pragma(skip, Options, _) :- !,
	setarg(skip of options, Options, on).
consider_pragma(noskip, Options, _) :- !,
	setarg(skip of options, Options, off).
consider_pragma(warnings, Options, _) :- !,
	setarg(warnings of options, Options, on).
consider_pragma(nowarnings, Options, _) :- !,
	setarg(warnings of options, Options, off).
consider_pragma(expand, Options, _) :- !,
	setarg(expand_goals of options, Options, on).
consider_pragma(noexpand, Options, _) :- !,
	setarg(expand_goals of options, Options, off).
consider_pragma(opt_level(Level), Options, _) :- integer(Level), !,
	setarg(opt_level of options, Options, Level).
consider_pragma(Pragma, _, M) :-
	error(#bad_pragma, pragma(Pragma), M).	% make accessible via current_pragma/1


% For compatibility with old compiler
old_compiler_directive((:-system), Options) ?- !,
	setarg(system of options, Options, on),
	setarg(debug of options, Options, off),
	setarg(skip of options, Options, on),
	setarg(expand_goals of options, Options, on).
old_compiler_directive((:-system_debug), Options) ?- !,
	setarg(system of options, Options, on),
	setarg(debug of options, Options, on),
	setarg(skip of options, Options, off).
old_compiler_directive((:-dbgcomp), Options) ?- !,
	set_flag(debug_compile, on),
	setarg(debug of options, Options, on).
old_compiler_directive((:-nodbgcomp), Options) ?- !,
	set_flag(debug_compile, off),
	setarg(expand_goals of options, Options, on),
	setarg(debug of options, Options, off).


% Valid ISO-Prolog directives
% We qualify those that are not built-ins
iso_directive(dynamic(P),	eclipse_language:dynamic(P)).
iso_directive(multifile(P),	multifile:multifile(P)).
iso_directive(discontiguous(P),	eclipse_language:discontiguous(P)).
iso_directive(op(P,A,O),	op(P,A,O)).
iso_directive(char_conversion(X,Y), char_conversion(X,Y)).
iso_directive(initialization(G), iso:initialization(G)).
iso_directive(include(_),	true).	% already handled
iso_directive(ensure_loaded(F),	eclipse_language:ensure_loaded(F)).
iso_directive(set_prolog_flag(F,V), set_prolog_flag(F,V)).


% copy directives and queries to the eco file
% omit comments and includes
% do copy pragmas, since some of them have load-time effect
% (e.g. suppress deprecation warnings)
emit_directive_or_query((:-comment(_,_)), _Options, _Module) ?- !.
emit_directive_or_query((:-include(_)), _Options, _Module) ?- !.
emit_directive_or_query((:-[_|_]), _Options, _Module) ?- !.
emit_directive_or_query(Dir, Options, Module) :-
	numbervars(Dir, 0, _),
	( Options = options{output:print} ->
	    printf("%Iw.%n", [Dir])
	; Options = options{output:print(Stream)} ->
	    printf(Stream, "%Iw.%n", [Dir])
	; Options = options{output:eco_to_stream(Stream)} ->
	    printf(Stream, "%IODQKw.%n", [Dir])@Module
	; Options = options{output:asm_to_stream(Stream)} ->
	    printf(Stream, "%IDQKw.%n", [Dir])@Module
	; Options = options{output:none} ->
	    true
	;
	    Options = options{output:Junk},
	    printf(error, "Invalid output option: %w%n", [Junk]),
	    abort
	),
	fail.	% to undo numbervars
emit_directive_or_query(_, _, _).


%----------------------------------------------------------------------
% Compile term/list
%----------------------------------------------------------------------

:- export
	compile_term/1, compile_term_/2,
	compile_term/2, compile_term_/3,
	compile_term_annotated/3, compile_term_annotated_/4.

:- tool(compile_term/1, compile_term_/2).
:- set_flag(compile_term/1, type, built_in).
compile_term_(Clauses, Module) :-
	compile_term_(Clauses, [], Module).


:- tool(compile_term/2, compile_term_/3).
:- set_flag(compile_term/2, type, built_in).

compile_term_(List, OptionList, Module) :-
        compile_term_annotated_(List, _, OptionList, Module).

:- tool(compile_term_annotated/3, compile_term_annotated_/4).
:- set_flag(compile_term_annotated/3, type, built_in).

compile_term_annotated_(List, AnnList, OptionList, Module) :-
	compiler_options_setup('_term', OptionList, Options),
	hash_create(PredsSeen),
	% The subcall is needed to make coroutining in the compiler work,
	% and to give compiled queries a standard environment to run in.
	subcall(compile_list(List, AnnList, first, Clauses, Clauses, AnnC, AnnC,
                     0, Size, PredsSeen, Options, Module), _Delays),
%	compiler_options_cleanup(Options).	% don't close files
	( Size < 0 ->
	    exit_block(abort)	% because of errors during compile
	;
	    true
	).


compile_list(Term, _, _, _, _, _, _, _, _, _PredsSeen, Options, Module) :- var(Term), !,
	error(#inst_fault, compile_term(Term, Options), Module).
compile_list([], _, Pred, Clauses, Tail, AnnC, AnnCTail, Size0, Size, PredsSeen, Options, _Module) :- !,
	Tail = [],
        AnnCTail = [],
	compile_predicate(Pred, Clauses, AnnC, term, PredsSeen, Options, Size1),
	Size is Size0+Size1.
compile_list([Term|Terms], AnnTermList, Pred, Clauses, Tail, AnnC, AnnCTail, Size0, Size, PredsSeen, Options, Module) :- !,
        (nonvar(AnnTermList) -> 
            AnnTermList = annotated_term{term:[AnnTerm|AnnTerms]}
        ;
            true
        ),
        ( var(Term) ->
	    error(#inst_fault, compile_term([Term|Terms], Options), Module)

	; Term = (:-_) ->
	    % separator, compile the preceding predicate
	    Tail = [],
            AnnCTail = [],
	    compile_predicate(Pred, Clauses, AnnC, term, PredsSeen, Options, Size1),
	    Size2 is Size0+Size1,
	    % unlike compile(file), interpret only pragmas,
	    % not directives like module/1, include/1, etc
	    ( consider_pragmas(Term, Options, Module) ->
		true
	    ;
		process_directive(no_source, Term, Options, Module)
	    ),
	    compile_list(Terms, AnnTerms, none, Clauses1, Clauses1,
                         AnnC1, AnnC1, Size2, Size, PredsSeen, Options, Module)

        ; Term = (?-_) ->
	    % separator, compile the preceding predicate
	    Tail = [],
            AnnCTail = [],
	    compile_predicate(Pred, Clauses, AnnC, term, PredsSeen, Options, Size1),
	    Size2 is Size0+Size1,
	    process_query(no_source, Term, Options, Module),
	    compile_list(Terms, AnnTerms, none, Clauses1, Clauses1,
                         AnnC1, AnnC1, Size2, Size, PredsSeen, Options, Module)
	; callable(Term) ->
	    optional_clause_expansion(Term, AnnTerm, TransTerm, AnnTrans, Options, Module),
	    % TransTerm may be a list of clauses!
	    accumulate_clauses(TransTerm, AnnTrans, Module, Options, term, PredsSeen,
		    Size0, Pred, term, Clauses, Tail, AnnC, AnnCTail,
		    Size1, Pred1, _Pos, Clauses1, Tail1, AnnC1, AnnCTail1),
	    compile_list(Terms, AnnTerms, Pred1, Clauses1, Tail1, 
                    AnnC1, AnnCTail1, Size1, Size, PredsSeen, Options, Module)
	;
	    ( block(compiler_event(#illegal_head, term, AnnTerm, Term, Module), abort_compile_predicate, true) -> true ; true ),
	    Size = -1.0Inf
	).
compile_list(Term, AnnTerm, Pred, Clauses, Tail, AnnC, AnnCTail, Size0, Size, PredsSeen, Options, Module) :-
	( Pred == first ->
	    % allow to omit list brackets for single term
            (nonvar(AnnTerm) ->
                AnnTermList = annotated_term{term:[annotated_term{term:AnnTerm}|annotated_term{term:[]}]}
            ;
                true
            ),
	    compile_list([Term], AnnTermList, none, Clauses, Tail, AnnC, AnnCTail, Size0, Size, PredsSeen, Options, Module)
	;
	    error(#type_error, compile_term(Term, Options), Module)
	).


    optional_clause_expansion(Term, AnnTerm, TransTerm, AnnTransTerm, options{expand_clauses:CFlag}, Module) :-
	( CFlag == on ->
	    expand_clause_annotated(Term, AnnTerm, TransTerm, AnnTransTerm)@Module
	;
	    TransTerm=Term, AnnTransTerm=AnnTerm
	).

