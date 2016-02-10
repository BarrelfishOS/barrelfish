% ----------------------------------------------------------------------
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
% Copyright (C) 2000-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: source_processor.ecl,v 1.16 2013/02/14 02:55:20 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(source_processor).

:- comment(categories, ["Development Tools"]).
:- comment(summary, "Tools for processing ECLiPSe sources").
:- comment(date, "$Date: 2013/02/14 02:55:20 $").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(author, "Joachim Schimpf, IC-Parc").

:- comment(eg, "
    % This can be used as a template for source processing code:
    % a source file is opened,
    % every term is read and printed,
    % then the file is closed

    test(File) :-
	source_open(File, [], SP0),
	(
	    fromto(begin, _, Class, end),
	    fromto(SP0, SP1, SP2, SPend)
	do
	    source_read(SP1, SP2, Class, SourceTerm),
	    SP1 = source_position{file:F,line:L,module:M},
	    printf(\"%w %w:%d %w%n\", [M,F,L,Class]),
	    arg(term of source_term, SourceTerm, Term),
	    writeclause(Term)
	),
	source_close(SPend, []).
    ").


:- export
	source_open/3,
	source_read/4,
	source_close/2.

:- export struct(source_position(
	filespec,		% Original source location argument
	stream,			% Eclipse stream
	file,			% canonical file name, 'user', or device name (tty,string,etc)
	line,			% integer
	offset,			% integer
        remaining_files,	% list of file names
	included_from,		% source_position or []
	options,		% struct options
	created_modules,	% list of modules
	oldcwd,			% current directory before opening
	module,			% module
	ifdefs,			% list of nested ifdefs (then/else atoms)
	old_compiled_stream	% the previous value of compiled_stream/1
				% (only used when included_from==[])
    )).

:- export struct(source_term(
	term,
	vars,
        annotated
    )).


:- import expand_clause_annotated/4,
          expand_goal_annotated/4,
          expand_macros_annotated/4,
	  erase_module_pragmas/1,
	  register_compiled_stream/1
   from sepia_kernel.


:- local struct(options(
    	keep_comments,		% true if set, else variable
    	ignore_conditionals,	% true if set, else variable
    	include_comment_files,	% true if set, else variable
	recreate_modules,	% erase old module before creating
	no_macro_expansion,	% true if set, else variable
	no_clause_expansion,	% true if set, else variable
	goal_expansion,		% true if set, else variable
	with_annotations	% true if set, else variable
    )).

:- comment(struct(source_term), [
    summary:"A source term with additional information",
    fields:[
	term:"A term read from a source file (a clause, fact, etc)",
	vars:"A list of the term's variables and their names information (or [] if the with_annotations option is in effect)",
        annotated:"An annotated version of term with source and type information (only if with_annotations option given, otherwise uninstantiated)"
    ],
    see_also:[readvar/3,read_annotated/2]
]).

:- comment(struct(source_position), [
    summary:"Current source position",
    desc:html("This structure describes a particular position that has been
    	reached during processing of an ECLiPSe source file. It also describes
	what has to be done when this file is finished. The source_read/4
	predicate reads a term from a given source position and returns
	the new source position after the read."),
    fields:[
	filespec:"the original source file argument used in source_open/3",
	stream:"the Eclipse stream being read",
	file:"this file's canonical file name",
	line:"this position's line number (integer)",
	offset:"this position's byte offset (integer)",
	remaining_files:"list of files still to be included",
	included_from:"the source_position from which this file is included, or [] if not included",
	options:"structure describing option settings",
	created_modules:"list of modules created so far",
	oldcwd:"current directory before opening this file",
	module:"read-module at this source position",
	ifdefs:"list of atoms (then|else) describing nesting of if-directives",
	old_compiled_stream:"value of compiled_stream/1 when toplevel file was opened"
    ],
    see_also:[source_open/3,source_close/2,source_read/4]
]).


:- comment(source_open/3, [
    summary: "Open an ECLiPSe source code file for subsequent processing",
    args: ["File":"Name of source file (Atom or string), or term of the form stream(Stream)",
    	"OptionList":"List of options, possibly empty",
	"SourcePos":"Source position handle"],
    desc:html("This predicates opens an ECLiPSe source file (or prepares and
    already opened stream) for subsequent reading with source_read/4.
    Compared to the standard primitives for reading from a file, this
    takes care of
    <UL>
    <LI>nesting of included files
    <LI>creating and keeping track of modules
    <LI>syntax settings
    <LI>comments (optional)
    <LI>changing the current directory to the opened file's directory
    <LI>handling of if-elif-else-endif directives
    </UL>
    OptionList can contain the following:
    <DL>
    <DT>keep_comments</DT>
	<DD>treat comments and spacing between source terms as data
	rather than ignoring it</DD>
    <DT>include_comment_files</DT>
	<DD>interpret the comment(include,Files) directive and include
	the contents of the given files, identical to an include(Files)
	directive. By default, these directives are ignored.</DD>
    <DT>ignore_conditionals</DT>
	<DD>Ignore any special meaning of conditional directives (if/1,
	elif/1, else/0, endif/0) and just return them as a source term.
	The default is to interpret these directives, including or
	excluding corresponding source parts accordingly, while removing
	the directives themselves</DD>
    <DT>with_annotations</DT>
	<DD>return an annotated source term with every source term
	(and do not return a separate variable list)</DD>
    <DT>no_macro_expansion</DT>
	<DD>do not expand term macros (e.g. with/2 and of/2)</DD>
    <DT>minimal_macro_expansion</DT>
	<DD>do not expand term macros except in :- directives</DD>
    <DT>no_clause_expansion</DT>
	<DD>do not expand clause macros (e.g. DCGs)</DD>
    <DT>goal_expansion</DT>
	<DD>do inline expansion of goals (only works if clause expansion
	is not disabled)</DD>
    <DT>recreate_modules</DT>
	<DD>erase and re-create module when encountering a module directive</DD>
    </DL>
    source_open/3 and source_read/4 maintain a 'current source position',
    which is a structure containing (among others) the following fields:
    <PRE>
    :- export struct(source_position(
	stream,			% Eclipse stream
	file,			% canonical file name
	line,			% integer
	offset,			% integer
	included_from,		% source_position or []
	module,			% current source module
	...
    )).
    </PRE>
    i.e. information about the module context and the precise location
    of a source term (e.g. for error messages).
    <P>
    If File is of the form stream(Stream), then the predicate expects
    Stream to be an already opened input stream. Correspondingly, the
    processed stream will not be closed at the end of source processing
    (unlike files).
    <P>
    "),
    see_also:[source_close/2,source_read/4],
    amode: source_open(+,+,-)
    ]).

:- tool(source_open/3,source_open/4).
source_open(File, OptionList, SourcePos, Module) :-
	( 
	    var(SourcePos),
	    ( foreach(Option,OptionList), param(OptFlags) do
		set_option(Option, OptFlags)
	    )
	->
	    ( source_open(File, [], [], OptFlags, SourcePos, Module) ->
		true
	    ;
		error(171, source_open(File, OptionList, SourcePos), Module)
	    )
	;
	    error(6, source_open(File, OptionList, SourcePos), Module)
	).

    % fails if file doesn't exist
    source_open(File, RF, IF, OptFlags, SourcePos, Module) :-
	nonvar(File),						% may fail
	getcwd(OldCwd),
	( File = stream(In) ->
	    get_stream_info(In, device, Device),
	    ( Device == file ->
		get_stream_info(In, name, FullFile0),
		concat_atom([FullFile0], FullFile)
	    ;
		FullFile = Device
	    )
	; File == user ->
	    In = input, FullFile = File
	; (atom(File) ; string(File) ; File = library(_)) ->			% may fail
	    get_flag(prolog_suffix, Suffixes),
	    once existing_file(File, Suffixes, [readable], GoodFile), % may fail
	    canonical_path_name(GoodFile, FullFile),
	    pathname(FullFile, Dir, _, _),
	    cd(Dir),
	    open(FullFile, read, In)
	),
	( IF==[], compiled_stream(SavedStream) -> true ; true ),
	register_compiled_stream(In),
	( skip_utf8_bom(In) -> true ; true ),
	OptFlags = options{no_macro_expansion:NoMacroExp},
	( var(NoMacroExp) ->
	    true
	;
	    set_stream_property(In, macro_expansion, off)
	),
	get_stream_info(In, offset, Offset),
	get_stream_info(In, line, Line),
	SourcePos = source_position{filespec:File,
		stream:In, module:Module, offset:Offset,options:OptFlags,
		created_modules:[], oldcwd:OldCwd, ifdefs:[],
		line:Line,file:FullFile,remaining_files:RF,included_from:IF,
		old_compiled_stream:SavedStream}.

    set_option(Var, _ ) :- var(Var), !, fail.
    set_option(keep_comments, options{keep_comments:true}).
    set_option(ignore_conditionals, options{ignore_conditionals:true}).
    set_option(recreate_modules, options{recreate_modules:true}).
    set_option(no_macro_expansion, options{no_macro_expansion:true}).
    set_option(minimal_macro_expansion, options{no_macro_expansion:minimal}).
    set_option(no_clause_expansion, options{no_clause_expansion:true}).
    set_option(goal_expansion, options{goal_expansion:true}).
    set_option(with_annotations, options{with_annotations:true}).
    set_option(include_comment_files, options{include_comment_files:true}).

    peek(In, C) :- get(In, C).
    peek(In, _) :- unget(In), fail.

    skip_utf8_bom(In) :-
	peek(In, 16'ef), peek(In, 16'bb), peek(In, 16'bf), !.

:- comment(source_close/2, [
    summary: "Close an open ECLiPSe source file.",
    args: ["SourcePos":"Source position handle",
    	"OptionList":"List of options, possibly empty"],
    desc:html("This is used to close an ECLiPSe source file that was
    previously opened with source_open/3.  It is possible to close
    before the end of the source is reached. Nesting of included
    files is properly handled.
    <P>
    OptionList can contain the following:
    <DL>
    <DT>keep_modules</DT>
    	<DD>keep the modules that have been created implicitly during
	source processing (by default they are erased to restore the
	original state)</DD>
    </DL>
    <P>
    Note that if source_open/3 had been called on an already open stream
    with a stream(Stream) argument, then Stream will not be closed by
    source_close/2.
    "),
    see_also:[source_open/3],
    amode: source_close(+,+)
    ]).


    % this is optional at the end but it can be used to close prematurely
    % will close the whole include-hierarchy
source_close(SourcePos, Options) :-
	close_streams(SourcePos),
	( memberchk(keep_modules, Options) ->
	    true
	;
	    % erase the modules that we have created
	    arg(created_modules of source_position, SourcePos, Modules),
	    ( foreach(Module,Modules) do
		( current_compiled_file(_, _, Module) ->
		    true	% the module was also loaded, keep it
		;
		    erase_module(Module)
		)
	    )
	).

    close_streams(source_position{filespec:File,stream:Stream,included_from:IF,
    		oldcwd:OldCwd,old_compiled_stream:SavedStream}) :-
	close_input(File, Stream),
	( IF == [] ->
	    cd(OldCwd),
	    register_compiled_stream(SavedStream)
	;
	    close_streams(IF)
	).

    close_input(File, Stream) :-
	( current_stream(Stream), File \= stream(_) ->
	    block(close(Stream), abort, true)
	;
	    true
	).


:- comment(source_read/4, [
    summary: "Read the next term from an open ECLiPSe source file",
    args: ["SourcePos":"Source position handle",
	"NextPos":"Source position handle",
	"Kind":"kind of source term (atom)",
	"SourceTerm":"a source_term structure"],
    desc:html("This reads the next source term from a source file previously
    opened with source_open/3. The term at the current source position
    SourcePos is read, and the next source position is returned for use
    in subsequent source_read/4 invocations (it is not possible to read
    twice from the same source position!).
    <P>
    The term that has been read is classified into one of the following
    categories (Kind):
    <DL>
    <DT>handled_directive</DT>
    	<DD>A directive (a term with functor :-/1) which has already
	been handled (interpreted by source_read/4). Such directives are:
	module/1,3, local/1, export/1, reexport/1, use_module/1, lib/1,
	pragma/1, include/1, ./2, op/3, meta_attribute/2 and
	comment(include,...)</DD>
    <DT>directive</DT>
    	<DD>A directive (a term with functor :-/1) which has not
	been handled (ignored by source_read/4)</DD>
    <DT>query</DT>
    	<DD>A query (a term with functor ?-/1)</DD>
    <DT>clause</DT>
    	<DD>Any other structure, list or atom (usually a clause)</DD>
    <DT>var</DT>
    	<DD>A term consisting of only a variable (very likely an error)</DD>
    <DT>other</DT>
    	<DD>A number, string, etc (very likely an error)</DD>
    <DT>comment</DT>
    	<DD>Spacing, layout and comments between source terms
    	(only when keep_comments option is in effect)</DD>
    <DT>end_include</DT>
    	<DD>The end of an included source file</DD>
    <DT>end</DT>
    	<DD>The end of the (top-level) source file</DD>
    </DL>
    The information about the source term itself is returned as a structure
    <PRE>
    :- export struct(source_term(
	term,		% the read term itself
	vars,		% list of [VarName|Var] pairs (as in readvar/3)
	annotated       % the read term with type and source annotations 
	...
    )).
    </PRE>
    For category 'comment', the term is a string containing the comment.
    For category 'end' and 'end_include', the term is the atom end_of_file.
    In all these cases, vars is the empty list, and annotated is
    uninstantiated.
    <P>
    Note that either the vars-field or the annotated field is valid,
    depending on the setting of the with_annotations-option.  If the option
    is set, the vars field contains an empty list, and the annotated term
    is in the same format as that returned by read_annotated/2, i.e. a
    recursive wrapper structure around the read term of the following format:
<PRE>
    :- export struct(annotated_term(
        term,                   % var, atomic or compound
                                % args of compound terms are annotated
        type,                   % term type (see below)
        file,                   % file name (atom)
        line,                   % line number (integer >= 1)
        from, to                % source position (integers >= 0)
        ...
    )).
</PRE>
    If the with_annotations-option is not set, the annotated-field remains
    uninstantiated, and the vars-field is a list as detailed in readvar/3.
    <P>
    Notes on module handling:  When source_read/4 encounters a
    module-directive (which is a handled_directive), the corresponding
    module is implicitly created (unless it exists already, in which
    case it is either reused or erased and re-created, depending on
    the setting of the recreate_modules option), and that
    module becomes the context module for any subsequently read
    clauses or directives.  By default, source_close/2 removes these
    modules again in order to restore the original state.
    "),
    see_also:[source_open/3,source_close/2,readvar/3,read_annotated/2],
    amode: source_read(+,-,-,-)
    ]).

    % Term classes:
    %   directive, handled_directive, query, clause, comment, var, end_include, end
source_read(OldPos, NextPos, Kind, SourceTerm) :-
	OldPos = source_position{stream:In,module:Module,oldcwd:OldCwd,created_modules:CMs1,
		options:OptFlags,remaining_files:RF,included_from:IF,filespec:File},

	read_next_term(In, TermOrEof, AnnTermOrEof, Vars, Error, Comment, OptFlags, Module),
        get_stream_info(In, line, Line),
	get_stream_info(In, offset, Offset),

	( nonvar(Error) ->
	    update_struct(source_position, [offset:Offset,line:Line], OldPos, Pos1),
	    source_read(Pos1, NextPos, Kind, SourceTerm)
                 
	; nonvar(Comment) ->
	    SourceTerm = source_term{term:Comment,vars:[]},
	    update_struct(source_position, [offset:Offset,line:Line], OldPos, NextPos),
	    Kind = comment

	; var(TermOrEof) ->
	    SourceTerm = source_term{term:TermOrEof,vars:Vars,
                                     annotated:AnnTermOrEof},
	    update_struct(source_position, [offset:Offset,line:Line], OldPos, NextPos),
	    Kind = var

	; TermOrEof = end_of_file ->
	    close_input(File, In),
	    cd(OldCwd),
	    (
		% open next include file that can be opened
		append(_, [RF0|RFs], RF),
	        ( source_open(RF0, RFs, IF, OptFlags, NextPos0, Module) -> true ;
		    printf(warning_output, "WARNING: Could not open include file \"%w\"%n", [RF0]),
		    fail
		)
	    ->
		source_read(NextPos0, NextPos, Kind, SourceTerm)

	    ; IF = [] ->
		warn_ifdef(OldPos),
		OldPos = source_position{old_compiled_stream:OldStream},
		register_compiled_stream(OldStream),
		SourceTerm = source_term{term:TermOrEof,vars:Vars},
		update_struct(source_position, [offset:Offset,line:Line], OldPos, NextPos),
		Kind = end
	    ;
		IF = source_position{stream:OldStream,created_modules:CMs0},
		register_compiled_stream(OldStream),
		SourceTerm = source_term{term:TermOrEof,vars:Vars},
		% go back to source position before include, but update module information
		append(CMs0, CMs1, CMs),
		update_struct(source_position, [created_modules:CMs,module:Module], IF, NextPos),
		Kind = end_include
	    )

	; TermOrEof = (:- _) ->
	    OptFlags = options{no_macro_expansion:NoMacEx},
	    ( NoMacEx \== minimal ->
	    	TermOrEofX=TermOrEof, AnnTermOrEofX=AnnTermOrEof
	    ; var(AnnTermOrEof) ->
		expand_macros(TermOrEof, TermOrEofX)@Module, AnnTermOrEofX=AnnTermOrEof
	    ;
		expand_macros_annotated(TermOrEof, AnnTermOrEof, TermOrEofX, AnnTermOrEofX)@Module
	    ),
	    SourceTerm0 = source_term{term:TermOrEofX,vars:Vars, annotated:AnnTermOrEofX},
	    update_struct(source_position, [offset:Offset,line:Line], OldPos, ThisPos),
	    TermOrEofX = (:- Directive),
	    handle_ifdef_and_directives(Directive, ThisPos, NextPos, Kind, SourceTerm0, SourceTerm, OptFlags)

	; TermOrEof = (?- _), \+current_pragma(iso(strict))@Module ->
	    SourceTerm = source_term{term:TermOrEof,vars:Vars, annotated:AnnTermOrEof},
	    update_struct(source_position, [offset:Offset,line:Line], OldPos, NextPos),
	    Kind = query

	; callable(TermOrEof) ->
	    apply_clause_expansion(TermOrEof, AnnTermOrEof, TransTerm, 
                                   AnnTransTerm, OptFlags, Module),
	    % TransTerm may be the empty list - skip unless it was in the source
	    ( TransTerm == [], TermOrEof \== [] ->
		update_struct(source_position, [offset:Offset,line:Line], OldPos, NextPos0),
		source_read(NextPos0, NextPos, Kind, SourceTerm)
	    ;
		SourceTerm = source_term{term:TransTerm,vars:Vars, annotated:AnnTransTerm},
		update_struct(source_position, [offset:Offset,line:Line], OldPos, NextPos),
		Kind = clause
	    )
	;
	    SourceTerm = source_term{term:TermOrEof,vars:Vars,
                                     annotated:AnnTermOrEof},
	    update_struct(source_position, [offset:Offset,line:Line], OldPos, NextPos),
	    Kind = other
	).


    read_next_term(In, _Term, _AnnTermOrEof, _Vars, _Error, Comment, options{keep_comments:true}, _Module) ?-
	read_comment(In, Comment),	% may fail
	!.
    read_next_term(In, TermOrEof, AnnTermOrEof, Vars, _Error, _Comment, OptFlags, Module) :-
	read_special(In, TermOrEof, AnnTermOrEof, Vars, OptFlags, Module),
	!.
    read_next_term(In, _TermOrEof, _AnnTermOrEof, [], error, _Comment, _OptFlags, _Module) :-
	skip_to_fullstop(In).		% syntax error


    % read one term, fail if syntax error
    read_special(In, TermOrEof, AnnTermOrEof, Vars, options{with_annotations:WithAnn}, Module) ?- !,
	( WithAnn == true ->
	    Vars = [],
	    read_annotated(In, TermOrEof, AnnTermOrEof)@Module
	;
	    readvar(In, TermOrEof, Vars)@Module
	),
	% If the read consumed a layout char as part of the fullstop,
	% put it back. Different cases (| indicates stream position):
	%	term|<eof>	=> don't unget
	%	term.|<eof>	=> don't unget
	%	term. |<eof>	=> unget
	%	term. |other	=> unget
	%	term.|%comment	=> don't unget
	% The point of doing this is so that pretty-printers don't lose
	% any linefeeds etc that were present in the source.
	( TermOrEof == end_of_file ->
	    true
	;
	    unget(In), 
	    get_cc(In, _C, Class),
	    ( Class = blank_space -> unget(In)
	    ; Class = end_of_line -> unget(In)
	    ; true
	    )
	).


apply_clause_expansion(Term, AnnTerm, TransTerm, AnnTransTerm,
              options{no_clause_expansion:NoCFlag,goal_expansion:GFlag0},
	      Module) :-
	( NoCFlag == true -> CFlag=off ; CFlag=on ),
	( GFlag0 == true -> GFlag=on ; GFlag=off ),
	expand_clause_and_goals_(Term, AnnTerm,
		TransTerm, AnnTransTerm, CFlag, GFlag, Module).


:- export expand_clause_and_goals/6.
:- tool(expand_clause_and_goals/6, expand_clause_and_goals_/7).
expand_clause_and_goals_(Term, AnnTerm, TransTerm, AnnTransTerm, CFlag, GFlag, Module) :-
	( CFlag == on, get_flag(macro_expansion, on) ->
	    expand_clause_annotated(Term, AnnTerm, TransTerm1, AnnTransTerm1)@Module,
            ( GFlag == on, get_flag(goal_expansion, on) ->
		expand_clause_goals(TransTerm1, AnnTransTerm1, TransTerm,
                                    AnnTransTerm, Module)
	    ;
		TransTerm = TransTerm1,
                AnnTransTerm = AnnTransTerm1
	    )
	;
	    TransTerm = Term,
            AnnTransTerm = AnnTerm
	).

:- export expand_clause_goals/5.
expand_clause_goals(C, AC, TC, ATC, _M) :- var(C), !,
	TC = C, ATC = AC.
expand_clause_goals(H :- B, Ann, H :- BC, AnnExp, M) :-
	!,
        Ann = annotated_term{term:(AH:-AB),type:Type,from:From,to:To},
        AnnExp = annotated_term{term:(AH:-ABC),type:Type,from:From,to:To},
	expand_goal_annotated(B, AB, BC, ABC)@M.
expand_clause_goals([H|T], [AH|AT], [HC|TC], [AHC|ATC], M) :-
	!,
	expand_clause_goals(H, AH, HC, AHC, M),
	expand_clause_goals(T, AT, TC, ATC, M).
expand_clause_goals(C, AC, C, AC, _).



%----------------------------------------------------------------------
% Directives
%----------------------------------------------------------------------

handle_ifdef_and_directives(Directive, ThisPos, NextPos, Kind, SourceTerm0, SourceTerm, _Options) :-
	ThisPos = source_position{module:Module},
	current_pragma(iso(strict))@Module,
	!,
	SourceTerm = SourceTerm0,
	iso_handle_directives(Directive, ThisPos, NextPos, Kind).
handle_ifdef_and_directives(Directive, ThisPos, NextPos, Kind, SourceTerm0, SourceTerm, options{ignore_conditionals:true}) ?- !,
	SourceTerm = SourceTerm0,
	handle_directives(Directive, ThisPos, NextPos, Kind).
handle_ifdef_and_directives(Directive, ThisPos, NextPos, Kind, SourceTerm0, SourceTerm, _OptFlags) :-
	handle_ifdef(Directive, ThisPos, NextPos, Kind, SourceTerm0, SourceTerm).


handle_ifdef(if(Cond), ThisPos, NextPos, Kind, SourceTerm0, SourceTerm) ?- !,
	ThisPos = source_position{module:Module,ifdefs:Nesting0},
	( block(call(Cond)@Module, _, fail) ->
	    Nesting1 = [then|Nesting0],		% go into then-reading mode
	    update_struct(source_position, [ifdefs:Nesting1], ThisPos, ThisPos1),
	    source_read(ThisPos1, NextPos, Kind, SourceTerm)
	;
	    skip_to_conditional(ThisPos, NextDir, Module),
	    ( NextDir == endif ->
		source_read(ThisPos, NextPos, Kind, SourceTerm)
	    ; NextDir == else ->
		Nesting1 = [else|Nesting0],	% go into else-reading mode
		update_struct(source_position, [ifdefs:Nesting1], ThisPos, ThisPos1),
		source_read(ThisPos1, NextPos, Kind, SourceTerm)
	    ; NextDir = elif(Cond1) ->
		% failed if() followed by elif: treat like if()
		handle_ifdef(if(Cond1), ThisPos, NextPos, Kind, SourceTerm0, SourceTerm)
	    ;	% end_of_file
		directive_warning("Unmatched conditional directive", if(Cond), ThisPos),
		source_read(ThisPos, NextPos, Kind, SourceTerm)
	    )
	).
handle_ifdef(Else, ThisPos, NextPos, Kind, _, SourceTerm) :-
	nonvar(Else), (Else=else;Else=elif(_)),
	!,
	( ThisPos = source_position{ifdefs:[then|Nesting],module:Module} ->
	    % we were reading a then/elif-part and found else/elif
	    skip_to_endif(ThisPos, NextDir, Module),
	    ( NextDir == endif ->
		update_struct(source_position, [ifdefs:Nesting], ThisPos, ThisPos1),
		source_read(ThisPos1, NextPos, Kind, SourceTerm)
	    ;
		% warning will be raised later
		source_read(ThisPos, NextPos, Kind, SourceTerm)
	    )
	;
	    % we found else/elif and were not in an ifdef, or in an else part
	    directive_warning("Unexpected conditional directive", Else, ThisPos),
	    source_read(ThisPos, NextPos, Kind, SourceTerm)
	).
handle_ifdef(endif, ThisPos, NextPos, Kind, _, SourceTerm) ?- !,
	( ThisPos = source_position{ifdefs:[_Any|Nesting]} ->
	    % properly terminate this ifdef
	    update_struct(source_position, [ifdefs:Nesting], ThisPos, ThisPos1),
	    source_read(ThisPos1, NextPos, Kind, SourceTerm)
	;
	    % we found endif without being in ifdef
	    directive_warning("Unmatched conditional directive", endif, ThisPos),
	    source_read(ThisPos, NextPos, Kind, SourceTerm)
	).
handle_ifdef(Directive, ThisPos, NextPos, Kind, SourceTerm, SourceTerm) :-
	handle_directives(Directive, ThisPos, NextPos, Kind).


    warn_ifdef(source_position{ifdefs:[]}) :- !.
    warn_ifdef(source_position{file:Path}) :-
	pathname(Path, _Dir, File),
	printf(warning_output, "WARNING: Missing endif at end of file %w%n", [File]).


handle_directives(Directive, NextPos, NextPos, directive) :-
	var(Directive), !.
handle_directives((D1,D2), ThisPos, NextPos, Kind) :- !,
	handle_directives(D1, ThisPos, ThatPos, Kind1),
	handle_directives(D2, ThatPos, NextPos,  Kind2),
	( Kind1 == Kind2 ->
	    Kind = Kind1
	;
	    ThisPos = source_position{line:Line,file:Path},
	    pathname(Path, _Dir, File),
	    printf(warning_output, "WARNING: Confusing compound directive"
	    	" in file %w, line %d:%n:- %w.%n", [File,Line,(D1,D2)]),
	    printf(warning_output, "  Advice: Either break up, or use ?- query%n", []),
	    Kind = directive
	).

    % include directives
handle_directives(Files, ThisPos, NextPos, Kind) :- Files = [_|_], !,
	handle_include(Files, ThisPos, NextPos, Kind).
handle_directives(include(Files), ThisPos, NextPos, Kind) :- !,
	handle_include(Files, ThisPos, NextPos, Kind).
handle_directives(comment(include,Files), ThisPos, NextPos, Kind) :- !,
	ThisPos = source_position{options:options{include_comment_files:WantInclude}},
	( WantInclude == true ->
	    handle_directives(include(Files), ThisPos, NextPos, Kind)
	;
	    Kind = handled_directive, ThisPos = NextPos
	).

    % module directives
handle_directives(module_interface(NewModule), ThisPos, NextPos, Kind) :- !,
	directive_warning("Obsolete directive", module_interface(NewModule), ThisPos),
	handle_directives(module(NewModule), ThisPos, NextPos, Kind).
handle_directives(module(NewModule), ThisPos, NextPos, Kind) :- !,
	handle_directives(module(NewModule,[],eclipse_language), ThisPos, NextPos, Kind).
handle_directives(begin_module(NewModule), ThisPos, NextPos, handled_directive) :- !,
	( current_module(NewModule) ->
	    erase_module_pragmas(NewModule)
	;
	    error(80, begin_module(NewModule))
	),
	update_struct(source_position, [module:NewModule], ThisPos, NextPos).
handle_directives(module(NewModule,Exports,Imports), ThisPos, NextPos, handled_directive) :- !,
	ThisPos = source_position{options:OptFlags,created_modules:CM0},
	( current_module(NewModule) ->
	    OptFlags = options{recreate_modules:ReCreate},
	    ( ReCreate == true ->
	    	erase_module(NewModule),
		create_module(NewModule, Exports, Imports)
	    ;
		erase_module_pragmas(NewModule)
	    ),
	    CM = CM0
	;
	    create_module(NewModule, Exports, Imports),
	    CM = [NewModule|CM0]
	),
	% update created_modules and module field
	update_struct(source_position, [created_modules:CM,module:NewModule], ThisPos, NextPos).

    % pragmas
handle_directives(pragma(Pragma), NextPos, NextPos, handled_directive) :- !,
	NextPos = source_position{module:Module},
	error(148, pragma(Pragma), Module).	% record the pragma

    % meta_attribute/2 - partially handled: call without handlers to make
    % sure the attribute syntax is accepted.  Leave it to the caller to
    % re-call with handler list after module is loaded and the handlers
    % are actually available to be called.
handle_directives(Directive, NextPos, NextPos, handled_directive) :-
	Directive = meta_attribute(Name, Decls),
	!,
	NextPos = source_position{module:Module},
	meta_attribute_now_later(Decls, UrgentDecls, _Others),
	( block(meta_attribute(Name, UrgentDecls)@Module, _, fail) ->
	    true
	;
	    directive_warning("Directive failed or aborted", Directive, NextPos)
	).

    % other directives
handle_directives(Directive, NextPos, NextPos, Kind) :-
	NextPos = source_position{module:Module,file:File},
	( handled_directive(Directive, ChangeDir) ->
	    ( ChangeDir = yes, pathname(File, Dir, _, _), Dir \== "" ->
		getcwd(Cwd),
		cd(Dir),
		Back = cd(Cwd)
	    ;
		Back = true
	    ),
	    ( block(call(Directive)@Module, _, fail) ->
		Back
	    ;
		Back,
		directive_warning("Directive failed or aborted", Directive, NextPos)
	    ),
	    Kind = handled_directive
	; obsolete_directive(Directive), Module \= sepia_kernel ->
	    directive_warning("Obsolete directive", Directive, NextPos),
	    Kind = directive
	;
	    Kind = directive
	).

    directive_warning(Msg, Directive, source_position{line:Line,file:Path}) :-
	pathname(Path, _Dir, File),
	printf(warning_output, "WARNING: %w in file %w, line %d:%n:- %w.%n",
		[Msg,File,Line,Directive]).


% Restricted directive handling for iso(strict)
:- mode iso_handle_directives(?,+,-,-).
iso_handle_directives(Directive, NextPos, NextPos, directive) :-
	var(Directive), !.
iso_handle_directives(include(Files), ThisPos, NextPos, Kind) :- !,
	handle_include(Files, ThisPos, NextPos, Kind).
iso_handle_directives(Directive, NextPos, NextPos, handled_directive) :-
	Directive = op(_,_,_),
	!,
	NextPos = source_position{module:Module},
	( block(call(Directive)@Module, _, fail) -> true
	; directive_warning("Directive failed or aborted", Directive, NextPos)
	).
iso_handle_directives(_Directive, NextPos, NextPos, directive).


handle_include([File|Files], ThisPos, NextPos, Kind) ?- !,
	Kind = handled_directive,
	ThisPos = source_position{module:Module,options:OptFlags,
		file:TopFile,line:Line},
	( source_open(File, Files, ThisPos, OptFlags, NextPos, Module) ->
	    true
	;
	    printf(warning_output, "WARNING: Could not open include file \"%w\""
		"%nin file %w, line %d%n", [File,TopFile,Line]),
	    ( Files == [] ->
		NextPos = ThisPos
	    ;
		handle_directives(include(Files), ThisPos, NextPos, _)
	    )
	).
handle_include(File, ThisPos, NextPos, Kind) :-
	handle_include([File], ThisPos, NextPos, Kind).


% handled_directive(+Directive, -NeedToChangeDir)
:- mode handled_directive(+, -).
handled_directive(local(_), no).
handled_directive(export(_), no).
handled_directive(reexport(_), no).
handled_directive(use_module(_), yes).
handled_directive(lib(_), yes).
handled_directive(op(_, _, _), no).
handled_directive(meta_attribute(_, _), no).

obsolete_directive(define_struct(_)).	% library(structures)
obsolete_directive(global_op(_, _, _)).
obsolete_directive(local_op(_, _, _)).
obsolete_directive(define_global_macro(_, _, _)).
obsolete_directive(define_local_macro(_, _, _)).
obsolete_directive(define_macro(_, _, _)).
obsolete_directive(set_chtab(_, _)).
obsolete_directive(cprolog).
obsolete_directive(quintus).
obsolete_directive(bsi).
obsolete_directive(sicstus).
obsolete_directive(autoload(_,_)).
obsolete_directive(autoload_tool(_,_)).
%obsolete_directive(coroutine).
obsolete_directive(local_record(_)).
obsolete_directive(make_array(_)).
obsolete_directive(make_array(_,_)).
obsolete_directive(make_local_array(_)).
obsolete_directive(make_local_array(_,_)).
obsolete_directive(tool(_)).

% Filter out the part of the meta_attribute declaration that must be
% considered immediately. All others are delayed until initialization.
:- export meta_attribute_now_later/3.
:- mode meta_attribute_now_later(?,-,-).
meta_attribute_now_later([Decl|Ds], Ns, Ls) ?- !,
	( nonvar(Decl), Decl=Name:_, Name == suspension_lists ->
	    Ns = [Decl|Ns1], Ls = Ls1
	;
	    Ns = Ns1, Ls = [Decl|Ls1]
	),
	meta_attribute_now_later(Ds, Ns1, Ls1).
meta_attribute_now_later(Ds, Ds, []).
	

%----------------------------------------------------------------------
% Auxiliary read predicates
%----------------------------------------------------------------------

read_comment(Stream, Comment) :-
	get_cc(Stream, C, Class),
	skip_to_comment(Stream, C, Class, Cs),
	Cs = [_|_],	% fails if emtpy
	string_list(Comment, Cs).

    skip_to_comment(Stream, C0, line_comment, [C0|Cs]) :- !,
	get_cc(Stream, C, Class),
    	skip_line_comment(Stream, C, Class, Cs).
    skip_to_comment(Stream, C0, first_comment, CCs) :- !,
	get_cc(Stream, C1, Class1),
	( Class1 == second_comment ->
	    CCs = [C0,C1|Cs],
	    get_cc(Stream, C, Class),
	    skip_bracketed_comment(Stream, C, Class, Cs)
	;
	    unget(Stream),
	    skip_to_comment(Stream, C0, symbol, CCs)
	).
    skip_to_comment(Stream, C0, blank_space, [C0|Cs]) :- !,
	get_cc(Stream, C, Class),
	skip_to_comment(Stream, C, Class, Cs).
    skip_to_comment(Stream, C0, end_of_line, [C0|Cs]) :- !,
	get_cc(Stream, C, Class),
	skip_to_comment(Stream, C, Class, Cs).
    skip_to_comment(Stream, _, _Other, []) :-
    	unget(Stream).

    skip_line_comment(Stream, _C0, eof, []) :- !,
	unget(Stream).
    skip_line_comment(Stream, C0, end_of_line, [C0|Cs]) :- !,
	get_cc(Stream, C, Class),
	skip_to_comment(Stream, C, Class, Cs).
    skip_line_comment(Stream, C0, _Other, [C0|Cs]) :-
	get_cc(Stream, C, Class),
	skip_line_comment(Stream, C, Class, Cs).

    skip_bracketed_comment(Stream, C0, second_comment, [C0|CCs]) :- !,
	get_cc(Stream, C1, Class1),
	( Class1 == first_comment ->
	    CCs = [C1|Cs],
	    get_cc(Stream, C, Class),
	    skip_to_comment(Stream, C, Class, Cs)
	;
	    skip_bracketed_comment(Stream, C1, Class1, CCs)
	).
    skip_bracketed_comment(Stream, C0, _Other, [C0|Cs]) :-
	get_cc(Stream, C, Class),
	skip_bracketed_comment(Stream, C, Class, Cs).

    get_cc(Stream, C, Class) :-
	get(Stream, C),
	( C >= 0 -> get_chtab(C, Class) ; Class = eof ).


% simple attempt at recovery: skip to fullstop at end of line
skip_to_fullstop(Stream) :-
    	get(Stream, C1),
	( C1 = 0'. ->
	    get(Stream, C2),
	    ( C2 \= -1, get_chtab(C2, end_of_line) ->
	    	true
	    ;
		unget(Stream),
		skip_to_fullstop(Stream)
	    )
	; C1 = -1 ->
	    unget(Stream)
	;
	    skip_to_fullstop(Stream)
	).


skip_to_endif(ThisPos, NextDir, Module) :-
	skip_to_conditional(ThisPos, NextDir0, Module),
	( is_else_directive(:-NextDir0) ->
	    skip_to_endif(ThisPos, NextDir, Module)
	;
	    NextDir = NextDir0
	).

% Skip to elif/1, else/0, endif/0 or end_of_file
skip_to_conditional(source_position{stream:Stream}, NextDir, Module) :-
	skip_to_conditional(Stream, 0, NextDir, Module).

skip_to_conditional(Stream, Nesting, NextDir, Module) :-
	% suppress syntax errors in the skipped code
	get_stream(error, OldError),
	set_stream(error, null),
	skip_to_conditional1(Stream, Nesting, NextDir, Module),
	set_stream(error, OldError).

    skip_to_conditional1(Stream, Nesting, NextDir, Module) :-
	( read(Stream, Term)@Module ->
	    ( Term == end_of_file ->
		unget(Stream),
		NextDir = end_of_file
	    ; Term == (:-endif) ->
		( Nesting > 0 ->
		    Nesting1 is Nesting-1,
		    skip_to_conditional1(Stream, Nesting1, NextDir, Module)
		;
		    NextDir = endif
		)
	    ; is_if_directive(Term) ->
		Nesting1 is Nesting+1,
		skip_to_conditional1(Stream, Nesting1, NextDir, Module)
	    ; is_else_directive(Term) ->
		( Nesting > 0 ->
		    skip_to_conditional1(Stream, Nesting, NextDir, Module)
		;
		    Term = (:-NextDir)
		)
	    ;
		skip_to_conditional1(Stream, Nesting, NextDir, Module)
	    )
	;
	    skip_to_conditional1(Stream, Nesting, NextDir, Module)
	).

    is_if_directive(:-if(_)) ?- true.

    is_else_directive(:-elif(_)) ?- true.
    is_else_directive(:-else) ?- true.


end_of_file.

%----------------------------------------------------------------------
% Sample code
%----------------------------------------------------------------------

test(File) :-
	source_open(File, [], SP0),
	(
	    fromto(begin, _, Class, end),
	    fromto(SP0, SP1, SP2, SPend)
	do
	    source_read(SP1, SP2, Class, SourceTerm),
	    SP1 = source_position{file:F,line:L,module:M},
	    printf("%w %w:%d %w%n", [M,F,L,Class]),
	    arg(term of source_term, SourceTerm, Term),
	    writeclause(Term)
	),
	source_close(SPend, []).

%----------------------------------------------------------------------

echo(File) :-
	source_open(File, [filter_conditionals], SP0),
	(
	    fromto(begin, _, Class, end),
	    fromto(SP0, SP1, SP2, SPend)
	do
	    source_read(SP1, SP2, Class, SourceTerm),
	    arg(term of source_term, SourceTerm, Term),
	    ( Class = comment ->
	    	printf("%Nw", [Term])
	    ;
		writeclause(Term)
	    )
	),
	source_close(SPend, []).

%----------------------------------------------------------------------

icom(File) :-
	source_open(File, [], SP0),
	(
	    fromto(begin, _, Class, end),
	    fromto(SP0, SP1, SP2, SPend),
	    fromto(Comments, C1, C0, [])
	do
	    source_read(SP1, SP2, Class, SourceTerm),
	    ( Class = directive ->
		arg(term of source_term, SourceTerm, (:-Directive)),
		( Directive = comment(_,_) ->
		    C1 = [Directive|C0]
		;
		    C1 = C0
		)
	    ;
		C1 = C0
	    )
	),
	( foreach(Comment, Comments) do
	    printf("%NQw%n", [:-Comment])
	),
	source_close(SPend, []).


%----------------------------------------------------------------------
% To-memory compiler
%----------------------------------------------------------------------

:- tool(com/1, com/2).

com(File, Module) :-
	source_open(File, [], SourcePos0)@Module,
	(
	    fromto(begin, _, Class, end),
	    fromto(SourcePos0, SourcePos1, SourcePos2, SourcePosEnd),
	    fromto(ClauseTail, Clauses0, Clauses1, []),
	    fromto(ClauseTail, ClauseTail0, ClauseTail1, []),
	    fromto(none, Pred0, Pred1, none)
	do
	    source_read(SourcePos1, SourcePos2, Class, SourceTerm),
	    arg(module of source_position, SourcePos1, PosModule),
	    arg(term of source_term, SourceTerm, Term),

	    ( Class = clause ->
		extract_pred(Term, N, A),
		Pred1 = PosModule:N/A,
		( Pred1 = Pred0 ->		% new clause for same pred
		    ClauseTail0 = [Term|ClauseTail1],
		    Clauses1 = Clauses0
		;
		    ClauseTail0 = [],		% new pred, compile previous
		    compile_predicate(Pred0, Clauses0),
		    Clauses1 = [Term|ClauseTail1]
		)

	    ; Class = comment ->		% comment, ignore
		Pred1 = Pred0,
		ClauseTail1 = ClauseTail0,
		Clauses1 = Clauses0

	    ; % other classes are taken as predicate separator
		ClauseTail0 = [],		% compile previous predicate
		compile_predicate(Pred0, Clauses0),
		Clauses1 = ClauseTail1,
		Pred1 = none,

		( Class = directive ->
		    call_directive(SourcePos1, Term, PosModule)
		; Class = query ->
		    call_directive(SourcePos1, Term, PosModule)
		; Class = var ->
		    compiler_error(4, SourcePos1, SourceTerm)
		;
		   true
		)
	    )
	),
	source_close(SourcePosEnd, [keep_modules]).

    compile_predicate(_, []) :- !.
    compile_predicate(M:NA, Clauses) :-
	writeln(compiling:NA@M),
    	compile_term(Clauses)@M.

    extract_pred(Head :- _, N, A) :- !,
    	functor(Head, N, A).
    extract_pred(Head ?- _, N, A) :- !,
    	functor(Head, N, A).
    extract_pred((Head if _), N, A) :- !,
    	functor(Head, N, A).
    extract_pred(Fact, N, A) :-
    	functor(Fact, N, A).

    call_directive(source_position{file:F,line:L}, Dir, Module) :-
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

    compiler_error(N, source_position{file:F,line:L},
    		source_term{term:Term}) :-
	error_id(N, Message),
	printf(error, "Compiler: %w in file %w, line %d:%n%Qw%n",
		[Message,F,L,Term]).

