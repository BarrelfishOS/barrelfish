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
% Version:	$Id: xref.ecl,v 1.4 2013/02/18 00:43:24 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(xref).

:- lib(module_options).
:- lib(source_processor).
:- lib(graph_algorithms).

:- comment(categories, ["Development Tools"]).
:- comment(summary, "Cross-referencing tool").
:- comment(author, "Joachim Schimpf, IC-Parc").
:- comment(copyright, "Cisco Systems, Inc.").
:- comment(date, "$Date: 2013/02/18 00:43:24 $").
:- comment(desc, html("
    This library analyses an ECLiPSe source module or file and build
    a call graph. The graph can either be returned in the format of
    lib(graph_algorithms), or output in different ways.
    ")).


:- comment(xref/2, [
    summary:"Display a call graph for a source module file",
    args:[
	"File":"Name of the source file (atom or string)",
	"Options":"A list of Option:Value structures"
    ],
    amode:xref(+,+),
    desc:html("
    Computes a call graph for the given source file and displays it.
    The possible options are:
<DL>
    <DT>builtins ('off' or 'on')</DT>
	<DD>selects whether to show built-in predicates or not. Default: off.</DD>
    <DT>output ('text', 'graphviz', graphviz(Options), 'daVinci' or file(File))</DT>
	<DD>selects how to show the results. 'text' is a for a simple textual
	listing on the output stream, file(File) writes text output into
	the specified file, 'daVinci' uses the daVinci graph drawing library,
	'graphviz' uses the graphviz graph drawing library. Default: text.</DD>
    <DT>relation ('calls' or 'called_by')</DT>
	<DD>selects whether to print the 'calls' relation or it inverse,
	the 'called_by' relation. Default: calls.</DD>
</DL>
    "),
    see_also:[call_graph/3]]).


:- comment(call_graph/3, [
    summary:"Compute a call graph for a source module file",
    args:[
	"File":"Name of the source file (atom or string)",
	"Graph":"Variable, returns a graph structure",
	"Options":"A list of Option:Value structures"
    ],
    amode:call_graph(+,-,+),
    desc:html("
    Computes a call graph for the given source file. The graph is in the
    format defined by lib(graph_algorithms).  The relevant options are:
<DL>
    <DT>builtins ('off' or 'on')</DT>
	<DD>selects whether to include built-in predicates or not. Default: off.</DD>
</DL>
    "),
    see_also:[xref/2,library(graph_algorithms)]]).


%----------------------------------------------------------------------
% Option handling
%----------------------------------------------------------------------

:- local struct(options(
    	builtins,
    	output,
    	relation,
        module_qualified
	)).


% Skeleton option structure with defaults for the user-settable fields
default_options(options with [
    	builtins:off,
    	output:text,
    	relation:calls,
        module_qualified:off
    ]).

% User-settable option names and their structure index
valid_option_field(builtins,		builtins of options).
valid_option_field(output,		output of options).
valid_option_field(relation,		relation of options).
valid_option_field(module_qualified,	module_qualified of options).

% Type checks for user-settable options
valid_option_value(builtins, off).
valid_option_value(builtins, on).
valid_option_value(output, text).
valid_option_value(output, file(File)) :- (atom(File);string(File)), !.
valid_option_value(output, daVinci).
valid_option_value(output, graphviz).
valid_option_value(output, graphviz(_Options)).
valid_option_value(relation, calls).
valid_option_value(relation, called_by).
valid_option_value(module_qualified, off).
valid_option_value(module_qualified, on).


%----------------------------------------------------------------------
% Graph output
%----------------------------------------------------------------------

xref_output(graphviz, Graph, _Relation) :- !,
	xref_output(graphviz([]), Graph, _Relation).
xref_output(graphviz(Options), Graph, _Relation) :- !,
	( memberchk(layout:_, Options) ->
	    Options1 = Options
	;
	    Options1 = [layout:left_to_right|Options]
	),
	call(graphviz:view_graph(Graph, Options1)).
xref_output(daVinci, Graph, _Relation) :- !,
	daVinci_draw_graph(Graph).
xref_output(daVinci, Graph, _Relation) :- !,
	daVinci_draw_graph(Graph).
xref_output(text, Graph, Relation) :- !,
	print_graph(output, Graph, Relation).
xref_output(file(OutFile), Graph, Relation) :- !,
	open(OutFile, write, Stream),
	print_graph(Stream, Graph, Relation),
	close(Stream).

print_graph(Stream, Graph, Relation) :-
	graph_get_maxnode(Graph, MaxNode),
	(
	    for(I,1,MaxNode),
	    fromto(CallerIds,CallerIds1,CallerIds0,[]),
	    param(Graph)
	do
	    ( graph_get_adjacent_edges(Graph, I, []) ->
		( graph_get_incoming_edges(Graph, I, []) ->
		    CallerIds1 = [I|CallerIds0]	% won't show up otherwise
		;
		    CallerIds1 = CallerIds0	% will show up as callee
		)
	    ;
		CallerIds1 = [I|CallerIds0]	% a proper caller
	    )
	),
	nodes_to_nodenames(Graph, CallerIds, Callers),
	sort(Callers, SortedCallers),
	( foreach(Caller,SortedCallers), param(Stream,Graph,Relation) do
	    printf(Stream, "%n%w %w:%n", [Caller,Relation]),
	    nodename_to_node(Graph, Caller, I),
	    graph_get_adjacent_edges(Graph, I, Callees),
	    ( foreach(e(_,J,_),Callees), param(Stream,Graph) do
		node_to_nodename(Graph, J, Callee),
		printf(Stream, "%t%w%n", [Callee])
	    )
	).


daVinci_draw_graph(Graph) :-
	graph_get_maxnode(Graph, NNodes),
	graph_get_all_edges(Graph, EdgeList),

	daVinci:daVinci_begin,
	( for(N,1,NNodes), param(Graph) do
	    ( node_to_nodename(Graph, N, NodeName) ->
		daVinci:daVinci_node(N, NodeName)
	    ;
		daVinci:daVinci_node(N)
	    )
	),
	( foreach(e(S,T,_D),EdgeList), count(E,1,_) do
	    daVinci:daVinci_edge(E, S, T)
	),
	daVinci:daVinci_end.


%----------------------------------------------------------------------

:- export xref/2.
:- tool(xref/2, xref_/3).
xref_(File, OptionList, Module) :-
	( get_options(OptionList, Options) ->
	    call_graph1(File, Graph0, Options, Module),
	    ( Options = options with relation:calls ->
		Relation = calls,
	    	Graph = Graph0
	    ;
		Relation = 'called by',
		graph_reverse_edges(Graph0, Graph)
	    ),
	    Options = options with output:OutputOption,
	    xref_output(OutputOption, Graph, Relation)
	;
	    printf(error, "Invalid option list: %w%n", [OptionList]),
	    print_default_options(error),
	    abort
	).

:- export saros_xref/2.
saros_xref(OSFile, Options) :-
        os_file_name(File, OSFile),
        xref(File, Options).

:- export call_graph/3.
:- tool(call_graph/3, call_graph/4).
call_graph(File, Graph, OptionList, Module) :-
	( get_options(OptionList, Options) ->
	    call_graph1(File, Graph, Options, Module)
	;
	    printf(error, "Invalid option list: %w%n", [OptionList]),
	    print_default_options(error),
	    abort
	).


call_graph1(File, Graph, Options, Module) :-
	file_to_graph_data(File, Edges, Preds, Options, Module),
	(
	    foreach(edge(From,To,_),Edges),
	    fromto(Nodes, [From,To|Nodes0], Nodes0, Preds)
	do
	    true
	),
	sort(Edges, UniqueEdges),
	sort(Nodes, UniqueNodes),
	NodeArr =.. [[]|UniqueNodes],
	make_graph_symbolic(NodeArr, UniqueEdges, Graph).


file_to_graph_data(File, Edges, Preds, Options, Module) :-
	source_open(File, [], SourcePos0)@Module,
	(
	    fromto(begin, _, Class, end),
	    fromto(SourcePos0, SourcePos1, SourcePos2, SourcePosEnd),
	    fromto(ClauseTail, Clauses0, Clauses1, []),
	    fromto(ClauseTail, ClauseTail0, ClauseTail1, []),
	    fromto(Edges, Edges1, Edges0, []),
	    fromto(Preds, Preds1, Preds0, []),
	    fromto(none, Pred0, Pred1, none),
	    param(Options)
	do
	    source_read(SourcePos1, SourcePos2, Class, SourceTerm),
	    arg(module of source_position, SourcePos1, PosModule),
	    arg(term of source_term, SourceTerm, Term),

	    ( Class = clause ->
		extract_pred(Term, N, A),
		Pred1 = PosModule:N/A,
		( Pred1 = Pred0 ->		% new clause for same pred
		    ClauseTail0 = [Term|ClauseTail1],
		    Edges1 = Edges0,
		    Preds1 = Preds0,
		    Clauses1 = Clauses0
		;
		    ClauseTail0 = [],		% new pred, compile previous
		    process_predicate(Pred0, Clauses0, Edges1, Edges0, Preds1, Preds0, Options),
		    Clauses1 = [Term|ClauseTail1]
		)

	    ; Class = comment ->		% comment, ignore
		Pred1 = Pred0,
		ClauseTail1 = ClauseTail0,
		Edges1 = Edges0,
		Preds1 = Preds0,
		Clauses1 = Clauses0

	    ; % other classes are taken as predicate separator
		ClauseTail0 = [],		% compile previous predicate
		process_predicate(Pred0, Clauses0, Edges2, Edges0, Preds1, Preds0, Options),
		Clauses1 = ClauseTail1,
		Pred1 = none,

		( Class = directive ->
		    call_directive(SourcePos1, Term, Edges1, Edges2, Options, PosModule)
		; Class = query ->
		    call_directive(SourcePos1, Term, Edges1, Edges2, Options, PosModule)
		; Class = var ->
		    Edges1 = Edges2,
		    compiler_error(4, SourcePos1, SourceTerm)
		;
		    Edges1 = Edges2
		)
	    )
	),
	source_close(SourcePosEnd, []).

    process_predicate(none, [], Edges, Edges, Preds, Preds, _Options) :- !.
    process_predicate(QualCaller, Clauses, Edges, Edges0, Preds, Preds0, Options) :-
	QualCaller = Module:Caller,
	( Options = options with module_qualified:off ->
	    Preds = [Caller|Preds0]
	;
	    Preds = [QualCaller|Preds0]
        ),
	local(Caller)@Module,	% create the predicate to track visibility
	(
	    foreach(Clause,Clauses),
	    fromto(Edges, Edges2, Edges1, Edges0),
	    param(Caller,Options,Module)
	do
	    process_clause(Clause, Caller, Edges2, Edges1, Options, Module)
	).

    extract_pred(Head :- _, N, A) :- !,
    	functor(Head, N, A).
    extract_pred(Head ?- _, N, A) :- !,
    	functor(Head, N, A).
    extract_pred((Head if _), N, A) :- !,
    	functor(Head, N, A).
    extract_pred(Fact, N, A) :-
    	functor(Fact, N, A).

    call_directive(_SP, (:- system), E, E, _Options, _Module) :- !.
    call_directive(_SP, (:- pragma(_)), E, E, _Options, _Module) :- !.
    call_directive(_SP, (:- tool(F/N,F1/N1)), Edges, Edges0, Options, Module) ?- !,
	insert_callee(F/N, F1/N1, Edges, Edges0, Options, Module).
    call_directive(source_position with [file:F,line:L], Dir, Edges, Edges0, Options, Module) :-
	arg(1, Dir, Goal),
	functor(Dir, CallerF, CallerN),
	process_body(Goal, CallerF/CallerN, Edges, Edges0, Options, Module),
    	block(
	    ( call(Goal)@Module ->
	    	true
	    ;
		printf(error, "xref: query failed in file %w, line %d.%n", [F,L])
	    ),
	    Tag,
	    printf(error, "xref: query exited (%w) in file %w, line %d.%n", [Tag, F,L])
	).

    compiler_error(N, source_position with [file:F,line:L],
    		source_term with [term:Term]) :-
	error_id(N, Message),
	printf(error, "xref: %w in file %w, line %d:%n%Qw%n",
		[Message,F,L,Term]).


process_clause((_Head :- -?-> Body), Caller, Edges, Edges0, Options, Module) ?- !,
	process_body(Body, Caller, Edges, Edges0, Options, Module).
process_clause((_Head ?- Body), Caller, Edges, Edges0, Options, Module) ?- !,
	process_body(Body, Caller, Edges, Edges0, Options, Module).
process_clause((_Head :- Body), Caller, Edges, Edges0, Options, Module) ?- !,
	process_body(Body, Caller, Edges, Edges0, Options, Module).
process_clause(_Fact, _Caller, Edges, Edges, _Options, _Module).



:- mode process_body(?,+,-,+,+,+).
process_body(Goal, _Caller, Edges, Edges0, _Options, _Module) :-
	var(Goal),
	!,
	Edges = Edges0.
process_body(M:Goal, Caller, Edges, Edges0, Options, Module) :- !,
	( var(Goal) ->
	    Edges = Edges0
	;
	    functor(Goal, F, N),
	    ( M == sepia_kernel, Options = options with builtins:off ->
		Edges = Edges0
	    ;
		( var(M) ->
		    ( get_var_info(M, name, VarName) -> 
			true
		    ; 
			VarName = 'Anon' 
		    ),
		    ( var_name:set_var_name(M, VarName) -> true ; true ),
		    var_name:get_var_name(M, ResolvedVarName)
		;
		    ResolvedVarName = M
		),
		( Options = options with module_qualified:off ->
		    Edges = [edge(Caller,ResolvedVarName:F/N,1)|Edges0]
		;
		    Edges = [edge(Module:Caller,ResolvedVarName:F/N,1)|Edges0]
		)
	    )
	).
process_body((G1,G2), Caller, Edges, Edges0, Options, Module) :- !,
	process_body(G1, Caller, Edges1, Edges0, Options, Module),
	process_body(G2, Caller, Edges, Edges1, Options, Module).
process_body((G1;G2), Caller, Edges, Edges0, Options, Module) :- !,
	process_body(G1, Caller, Edges1, Edges0, Options, Module),
	process_body(G2, Caller, Edges, Edges1, Options, Module).
process_body((G1->G2), Caller, Edges, Edges0, Options, Module) :- !,
	process_body(G1, Caller, Edges1, Edges0, Options, Module),
	process_body(G2, Caller, Edges, Edges1, Options, Module).
process_body(Goal, Caller, Edges, Edges0, Options, Module) :-
	functor(Goal, F, N),
	insert_callee(Caller, F/N, Edges1, Edges0, Options, Module),
	( get_flag(F/N, meta_predicate, Pattern)@Module ->
	    % some of Goal's arguments may need to be processed themselves
	    (
	        for(I,1,N),
		fromto(Edges, Edges3, Edges2, Edges1),
		param(Goal,Caller,Pattern,Options,Module)
	    do
		arg(I, Goal, Arg),
	        arg(I, Pattern, ArgSpec),
		( integer(ArgSpec), (compound(Arg);atom(Arg)) ->
		    Arg =.. [ArgF|ArgArgs],
		    length(XArgs, ArgSpec),
		    append(ArgArgs, XArgs, CallArgArgs),
		    Call =.. [ArgF|CallArgArgs],
		    process_body(Call, Caller, Edges3, Edges2, Options, Module)
		; ArgSpec == (/), nonvar(Arg), Arg = ArgF/ArgN, atom(ArgF), integer(ArgN) ->
		    insert_callee(Caller, Arg, Edges3, Edges2, Options, Module)
		; ArgSpec == (:-), nonvar(Arg), extract_pred(Arg, ArgF, ArgN) ->
		    insert_callee(Caller, ArgF/ArgN, Edges3, Edges2, Options, Module)
		;
		    Edges3 = Edges2
		)
	    )
	;
	    Edges = Edges1
	).

    insert_callee(Caller, FN, Edges1, Edges0, Options, Module) :-
	Options = options with module_qualified:ModuleQualified,
	( get_flag(FN, definition_module, DM)@Module, DM \= Module ->
	    ( ModuleQualified == off ->
		Caller1 = Caller
	    ;
		Caller1 = Module:Caller
	    ),
	    ( DM \== sepia_kernel ->
		Edges1 = [edge(Caller1,DM:FN,1)|Edges0]
	    ; Options = options with builtins:on ->
		Edges1 = [edge(Caller1,DM:FN,1)|Edges0]
	    ;
		Edges1 = Edges0
	    )
	;
	    ( ModuleQualified == off ->
		Edges1 = [edge(Caller,FN,1)|Edges0]
	    ;
		Edges1 = [edge(Module:Caller,Module:FN,1)|Edges0]
	    )
	).

