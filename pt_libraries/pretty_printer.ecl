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
% Copyright (C) 2000 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Helmut Simonis, Parc Technologies
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% Print ECLiPSe source in different formats
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Helmut Simonis, Parc Technologies Ltd
% Author/s:	Joachim Schimpf, IC-Parc, Imperial College
% Version:	$Id: pretty_printer.ecl,v 1.3 2009/07/16 09:11:27 jschimpf Exp $
% ----------------------------------------------------------------------

:-module(pretty_printer).

:-lib(source_processor).
:-lib(module_options).

:-ensure_loaded(library(document)).
:-import
	comment_to_html/2,
	htmlify_string/2,
	object_spec_to_filename/2
    from document.

:-export(pretty_print/1).
:-export(pretty_print/2).
:-export(pretty_print/3).
:-export(pretty_print/4).


%:-pragma(nodebug). % to avoid variable names being used in the compiled code


:- comment(categories, ["Development Tools"]).
:-comment(summary,"This library pretty-prints a file in different formats.").

:-comment(desc,html(
"This library prints a file in different formats. It can either produce 
<ul>
<li><b>txt</b>
	text only format, reformatting the input</li>
<li><b>html</b>
	html format with syntax coloring and navigation</li>
</ul>
")).

:-comment(author,"H. Simonis").
:-comment(copyright,"Cisco Systems, Inc.").
:-comment(date,"$Date: 2009/07/16 09:11:27 $").


%----------------------------------------------------------------------
% Option handling
%----------------------------------------------------------------------

:- local struct(options(
    % internal parameters
	stream,		% output stream
    % user-settable options
	format,		% html, txt
	outdir,		% output directory
	style,		% none, pretty, coverage, instrument
	inline_css,	% on, off
	css_span,       % span, font
	link_back,	% html string to be inserted in index
	indent_first,	% integer
	indent,		% integer
    % computation state
	last_pred,
	vars		% variable names
    )).


% Skeleton option structure with defaults for the user-settable fields
default_options(options with [
	    format:html,
	    outdir:pretty,
	    style:pretty,
	    inline_css:off,
	    css_span:span,
	    link_back:"",
	    indent_first:8,
	    indent:4
    ]).

% User-settable option names and their structure index
valid_option_field(format,		format of options).
valid_option_field(outdir,		outdir of options).
valid_option_field(style,		style of options).
valid_option_field(inline_css,		inline_css of options).
valid_option_field(css_span,		css_span of options).
valid_option_field(link_back,		link_back of options).
valid_option_field(indent_first,	indent_first of options).
valid_option_field(indent,		indent of options).

% Type checks for user-settable options
valid_option_value(format, html).
valid_option_value(format, txt).
valid_option_value(style, pretty).
valid_option_value(style, coverage).
valid_option_value(style, instrument).
valid_option_value(inline_css, off).
valid_option_value(inline_css, on).
valid_option_value(css_span, font).
valid_option_value(css_span, span).
valid_option_value(outdir, Dir) :- (atom(Dir);string(Dir)).
valid_option_value(link_back, Html) :- (atom(Html);string(Html)).
valid_option_value(indent_first, N) :- integer(N).
valid_option_value(indent, N) :- integer(N).



%----------------------------------------------------------------------
% main entry point
%----------------------------------------------------------------------

:-comment(pretty_print/1,[
summary:"Print a file in html format",
amode:pretty_print(++),
args:[
     "Files":"a filename (atom or string), or a list of filenames"
],
desc:html("
The system prints the contents of the file given as the argument as an
output file in html format.  This is one of the possible formats
supported by the pretty printer (see <b>pretty_print/2</b>).  The system
automatically adds the extension <i>.html</i> and places the result in a
subdirectory called 'pretty' relative to the source file.  Syntax coloring
is used to highlight different language features, and a uniform formatting
of the source code is used.
<p>
As the clauses are read with the normal Prolog read routine, comments
within clauses are lost, but comments and spacing between clauses is
preserved.
<p>
Hyperlinks are used for documented builtin predicates, as well as
predicates defined in the same file.  For a built-in, the hyperlink
will lead to the manual page of the predicate, for other predicates
the link points to the first definition of the predicate.  The
hyperlinks to the builtins are based on the installation directory of
Eclipse, so that the resulting output will normally not work on a
machine where eclipse is installed in a different directory.
<p>
If a list of several files is given, an additional index page index.html
with links to the individual pretty-printed source files will be generated.
<p>
The pretty printer does not check for undefined predicates or
predicates that are defined in included files.
<p>
The colours used are similar to those used by the EMACS Eclipse mode.
Colours are used more consistently than in the EMACS editor, as the
source text is parsed completely.
<p>
You can modify the colour scheme by modifying the style sheet
style.css which is located in the directory where the .html files get
generated (by default the subdirectory 'pretty').  The pretty-printer
will generate a default style sheet file if none exists, but not touch
any existing one.
<p>
"),
eg:"
?- pretty_print(simple').
",
see_also:[pretty_print/2]
]).

:-mode pretty_print(++).
pretty_print(File):-
	pretty_print(File,html).


:-comment(pretty_print/2,[
summary:"Print a file in one of different formats",
amode:pretty_print(++,++),
args:[
     "Files":"a filename (atom or string), or a list of filenames",
     "Options":"A list of Name:Value pairs"
%     "Option":"an atom, one of html, txt"
],
desc:html("<P>
    The system prints the contents of the file given as the argument as an
    output file in different formats. See pretty_print/1 for the general
    description.
</P><P>
    This predicate allows the following options to modify the results:
<DL>
    <DT>format (default:html)<DD>
	Possible values are 'html' or 'txt'.  The 'html' format
	produces html output with syntax coloring and navigation
	links.  The output file goes into a directory 'pretty' and has
	the suffix .html.  The 'txt' format produces pure text output. 
	The output file goes into a directory 'pretty' and has the
	suffix .txt.
    <DT>outdir (default:pretty)<DD>
    	The location of the generated files. This can be an absolute or
	a relative pathname (in Eclipse's generic pathname syntax, atom
	or string). If it is a relative pathname, it is interpreted as
	reative to the location of the input source file.
    <DT>style (default:pretty)<DD>
    	Possible values are 'pretty' or 'coverage'. Selects one of two
	predefined styles for html output. The latter is a less colourful
	form intended for code coverage output.
    <DT>inline_css (default:off)<DD>
    	Possible values are 'off' or 'on'. If 'on', the html output will
	use an inlined style sheet instead of an external one. This is
	provided for browsers that have problems with including style sheets.
    <DT>css_span (default:span)<DD>
    	Possible values are 'span' or 'font'. If 'font', the html output will
	not emit span tags. This is provided for browsers that do not yet
	support HTML 4.x.
    <DT>link_back (default:\"\")<DD>
    	A string in html format that gets inserted as a link to a parent
	page into the generated pretty-printed source index.html page.
</DL>
</P>
"),
see_also:[pretty_print/1]
]).

:-mode pretty_print(++,++).
pretty_print(Files, html) ?- !,			% backward compatibility
	pretty_print(Files, html, pretty).
pretty_print(Files, txt) ?- !,			% backward compatibility
	pretty_print(Files, txt, pretty).
pretty_print(Files, OptionList) :-
	get_options(OptionList, Options),
	!,
	pretty_print_with_options(Files, Options).
pretty_print(Files, Format) :-
	print_default_options(error),
	error(6, pretty_print(Files, Format)).



/*
:-comment(pretty_print/3,[
summary:"Print a file in one of different formats",
amode:pretty_print(++,++,++),
args:[
     "Files":"a filename (atom or string), or a list of filenames",
     "Format":"an atom, one of html, txt",
     "Directory":"directory where files are placed"
],
desc:html("
    The system prints the contents of the file given as the argument as an
    output file in one of four possible formats.  The options are
    <ul>
    <li><b>html</b> html output with syntax coloring and navigation links.
    	The output file has the suffix .html</li>
    <li><b>txt</b> pure text output. The output file has the suffix .txt</li>
    </ul>
    Output files are placed in Directory. Unless Directory starts with a
    '.' or '/', it is interpreted as relative to the input file's directory.
"),
fail_if:"no",
resat:no,
eg:"
?- pretty_print(simple,html,pretty).
?- pretty_print(simple,txt,txt).

",

see_also:[pretty_print/1, pretty_print/2, pretty_print/4]

]).
*/

% backward compatibility
:-mode pretty_print(++,++,++).
pretty_print(Files, Format, Directory) :-
	pretty_print(Files, Format, Directory, "").

/*
:-comment(pretty_print/4,[
summary:"Print a list of files file in one of different formats",
amode:pretty_print(++,++,++,++),
args:[
     "Files":"a filename (atom or string), or a list of filenames",
     "Format":"an atom, one of html, txt",
     "Directory":"directory where files are placed",
     "LinkBack": "HTML string (may be empty)"
],
desc:html("
    The system prints the contents of the file given as the argument as an
    output file in one of four possible formats.  The options are
    <ul>
    <li><b>html</b> html output with syntax coloring and navigation links.
    	The output file has the suffix .html</li>
    <li><b>txt</b> pure text output. The output file has the suffix .txt</li>
    </ul>
    Output files are placed in Directory. Unless Directory starts with a
    '.' or '/', it is interpreted as relative to the input file's directory.
    <P>
    If several input files are given, and the output is in html format,
    an index file (index.html) is generated as well.
"),
fail_if:"no",
resat:no,
eg:"
?- pretty_print([file1,file2],html,pretty,\"../index.html\").
?- pretty_print([file1,file2],txt,txt, \"../index.html\").

",

see_also:[pretty_print/1, pretty_print/2, pretty_print/3]

]).
*/

:-mode pretty_print(++,++,++,++).

% backward compatibility
pretty_print(FileOrFiles, Format, Directory, LinkBack) :- 
	get_options([format:Format,outdir:Directory,link_back:LinkBack], Options),
	!,
	pretty_print_with_options(FileOrFiles, Options).
pretty_print(File, Format, Directory, LinkBack) :-
	print_default_options(error),
	error(6, pretty_print(File, Format, Directory, LinkBack)).


pretty_print_with_options(FileOrFiles, Options) :- 
	( nonvar(FileOrFiles), FileOrFiles = [_|_] ->
	    Files = FileOrFiles
	;
	    Files = [FileOrFiles]
	),
	(
	    foreach(File,Files),
	    foreach(OutputFile,OutputFiles),
	    param(Options)
	do
	    start(File, Options, OutputFile),
	    process_file(File, Options),
	    pretty_print_close(Options)
	),
	make_source_index(Files, OutputFiles, Options).


:- mode option_info(+,+,-,-).
option_info(html, _, '.html', pretty).
option_info(txt, _, '.txt', pretty).


%----------------------------------------------------------------------
% Interface used by coverage library:
%	pretty_print_open(File, OptionList, State)
%	pretty_print_close(State)
%	pretty_print_term(Class, SourceTerm, SourcePos, StateIn, StateOut)
%----------------------------------------------------------------------

:- export pretty_print_open/3.
pretty_print_open(File, OptionList, Options) :-
	( get_options(OptionList, Options) ->
	    start(File, Options, _OutputFile)
	;
	    print_default_options(error),
	    error(6, pretty_print_open(File, OptionList))
	).


:- export pretty_print_close/1.
pretty_print_close(Options) :-
	Options = options with stream:S,
	off(pre,Options),
	hline(Options),
	off(body,Options),
	off(html,Options),
	close(S).


:- export pretty_print_term/5.
:- mode pretty_print_term(+,+,+,+,-).
pretty_print_term(Class, source_term with [term:Term],
	    source_position with [module:Module],
	    State, State) :-
%	arg(line of source_position, SP1, Line), writeln(Line:Class),
	process_term(Class, Term, State, Module).
		

%----------------------------------------------------------------------


start(File,Options,OutputFile):-
	Options = options with [
	    format:Format,		% get
	    outdir:OutputDir		% get
	],
	setarg(last_pred of options, Options, none/ -1),

	% the search for key is used to remember library pages looked up before
	erase_all(search_for),

	get_flag(prolog_suffix, Suffixes),
	once existing_file(File, Suffixes, [readable], GoodFile),
	canonical_path_name(GoodFile, FullFile),
	pathname(FullFile, FileDir, FileBase, FileSuffix),
	concat_string([FileDir,FileBase], FullBase),
	output_file(FullBase,Format,OutputDir,FileSuffix,OutputFile),
	open(OutputFile,write,S),
	setarg(stream of options, Options, S),
	printf(log_output, "Writing %s%n", OutputFile),

	on(html,Options),
	on(head,Options),
	onoff(title,GoodFile,Options),
	style_sheet(Options, OutputFile),
	off(head,Options),
	on(body,Options),
	on(h1,Options),
	extra("File: %w%n", [GoodFile], Options),
	off(h1,Options),
	hline(Options),
        on(pre,Options).


style_sheet(options with [format:html,stream:S,style:Style,inline_css:on], _) ?-
	!,
	default_style_sheet(Style, CSS),
	printf(S, "
	<style type=\"text/css\">
	<!--
	%s
	-->
	</style>", [CSS]).
style_sheet(options with [format:html,stream:S,style:Style,inline_css:off], OutputFile) ?-
	!,
	pathname(OutputFile, FileDir, _FileBase, _FileSuffix),
	concat_string([FileDir,"style.css"], CssFile),
	( existing_file(CssFile, [""], [readable], _CssFile) ->
	    true
	;	% create a style file if none exists
	    default_style_sheet(Style, CSS),
	    open(CssFile, write, CssStream),
	    write(CssStream, CSS),
	    close(CssStream)
	),
	printf(S, "
	    <link type=\"text/css\" rel=\"stylesheet\" href=\"style.css\"> ", []).
	% alternative method of including the style file
	% printf(S, "
	%	<style type=\"text/css\" media=\"screen, projection\">
	%	<!--
	%	@import url(style.css)
	%	-->
	%	</style>", []).
style_sheet(_, _).


% Some properties for style sheets:
%
% color
% font-family
% font-style	normal | italic | oblique
% font-variant	normal | small-caps
% font-weight	normal | bold | bolder | lighter | 100 | 200 | 300 | 400 | 500 | 600 | 700 | 800 | 900
% font-size	xx-small | x-small | small | medium | large | x-large | xx-large
%		larger | smaller
%		12pt
%		150%

default_style_sheet(_pretty, "/* default style: pretty */
	.comment {color:#B22222}
	.normal {color:#000000}
	.head {color:#0000FF}
	.call {color:#00008B}
	.builtin {color:#DA70D6}
	.control {color:#800080}
	.atom {color:#000000}
	.functor {color:#000000}
	.number {color:#000000}
	.string {color:#BC8F8F}
	.variable {color:#B8860B}
	.type {color:#228B22}
	.warning {color:#FF0000}
	.coverage {color:#000000; font-weight:bold; background-color:#32CD32}
	.nocoverage {color:#000000; font-weight:bold; background-color:#FF0000}").

%default_style_sheet(_pretty, "/* default style: pretty */
%	.comment {color:firebrick}
%	.normal {color:black}
%	.head {color:blue}
%	.call {color:darkblue}
%	.builtin {color:orchid}
%	.control {color:purple}
%	.atom {color:black}
%	.functor {color:black}
%	.number {color:black}
%	.string {color:rosybrown}
%	.variable {color:darkgoldenrod}
%	.type {color:forestgreen}
%	.warning {color:red}
%	.coverage {color:black; font-weight:bold; background-color:limegreen}
%	.nocoverage {color:black; font-weight:bold; background-color:red}").

%default_style_sheet(coverage, "/* default style: coverage */
%	.comment {color:firebrick}
%	.normal {color:black}
%	.head {color:darkblue}
%	.call {color:darkblue}
%	.builtin {color:black}
%	.control {color:black}
%	.atom {color:black}
%	.functor {color:black}
%	.number {color:black}
%	.string {color:darkgoldenrod}
%	.variable {color:darkgoldenrod}
%	.type {color:darkgoldenrod}
%	.warning {color:black}
%	.coverage {color:black; font-weight:bold; background-color:limegreen}
%	.nocoverage {color:black; font-weight:bold; background-color:red}").


make_source_index(Files, OutputFiles,
		options with [format:html,link_back:LinkBack]) :-
	!,
	OutputFiles = [FirstOut|_],
	pathname(FirstOut, HtmlDir, _, _),
	makedir(HtmlDir),
	concat_string([HtmlDir,'/index.html'], Index),
	open(Index, write, Stream),
	chmod644(Index),
	printf(Stream, "<HTML><HEAD><TITLE>Source Modules</TITLE></HEAD><BODY>%n", []),
	printf(Stream, "[ <A HREF=\"%w\">Documentation</A> ]", [LinkBack]),
	printf(Stream, "<H1>Source Modules</H1>%n", []),
	printf(Stream, "<OL>%n", []),
	( foreach(F,Files), foreach(OF,OutputFiles), foreach(F-OF,Pairs) do
	    true
	),
	keysort(Pairs, SortedPairs),
	( foreach(File-HtmlFile,SortedPairs), param(Stream,HtmlDir) do
	    pathname(File, _, Base, _),
	    make_relative(HtmlFile, HtmlDir, RelFile),
	    printf(Stream, "<LI><A HREF=\"%w\">%w</A>%n", [RelFile,Base])
	),
	printf(Stream, "</OL>%n", []),
	get_flag(unix_time, Now),
	local_time_string(Now, "%c", GenDate),
	printf(Stream, "<HR>Generated on %w%n", [GenDate]),
	writeln(Stream, "</BODY></HTML>"),
	close(Stream).
make_source_index(_Files, _OutputFiles, _Options).

    make_relative(File, Dir, RelFile) :-
    	( append_strings(Dir, RelFile, File) ->
	    true
	;
	    RelFile = File
	).

output_file(InFile, Format, OutDir, OrigSuffix, OutFile):-
	option_info(Format, OrigSuffix, Suffix, _),
	pathname(InFile, InDir, Base, _),
	% if OutDir starts with / or ., take it absolute or relative,
	% otherwise take it relative to input file directory
	( nonrelative(OutDir) ->
	    Dir = OutDir
	;
	    concat_string([InDir,OutDir], Dir)
	),
	makedir(Dir),
	concat_string([Dir,/,Base,Suffix], OutFile).

    nonrelative(F0) :- concat_string([F0], F), substring(F, "/", 1), !.
    nonrelative(F0) :- concat_string([F0], F), substring(F, ".", 1).


makedir(Dir) :-
	( exists(Dir) ->
	    true
	;
	    mkdir(Dir),
	    ( get_flag(hostarch, "i386_nt") -> true
	    ; exec([chmod,755,Dir], []))
	).

chmod644(File) :-
	( get_flag(hostarch, "i386_nt") -> true
	; exec([chmod,644,File], [])).



process_file(File, Options) :-
	source_open(File, [keep_comments,no_macro_expansion,
						no_clause_expansion], SP0),
	(
	    fromto(begin, _, Class, end),
	    fromto(SP0, SP1, SP2, SPend),
	    param(Options)
	do
	    source_read(SP1, SP2, Class, SourceTerm),
	    pretty_print_term(Class, SourceTerm, SP1, Options, _)
	),
	source_close(SPend, []).



%----------------------------------------------------------------------
% Actual pretty-printing of source terms
%----------------------------------------------------------------------

process_term(directive, Term, Options, Module) :-
	process_term(query, Term, Options, Module).
process_term(handled_directive, Term, Options, Module) :-
	process_term(query, Term, Options, Module).
process_term(query, Term, Options, Module) :-
	functor(Term, F, _),
	arg(1, Term, Query),
	process_query(Query, F, Options, Module).
process_term(clause, Term, Options, Module) :-
	clause_statement(Term, Options, Module).
process_term(comment, Term, Options, _Module) :-
	colored(Term, comment, Options).
%	setval(last_comment, Term).
process_term(var, Term, Options, _Module) :-
	colored(Term, warning, Options).
process_term(end_include, _Term, _Options, _Module).
process_term(end, _Term, _Options, _Module).



process_query(comment(A,B),Fct,Options, Module):-
	!,
	( Options = options with format:html ->
	    off(pre,Options),
	    hline(Options),
	    ( A = F/N ->
		%% Comment refers to pred
		arg(last_pred of options, Options, F1/N1),
		(F1/N1 \= F/N ->
		    object_spec_to_filename(F/N, Anchor),
		    anchor(Anchor, Options),
		    setarg(last_pred of options, Options, F/N)
		;
		    true
		)
	    ;
		%% Comment does not refer to pred
		true
	    ),
	    Options = options with stream:S,
	    ( comment_to_html(S, comment(A,B)) ->
	    	true
	    ;
	    	Query =.. [Fct,comment(A,B)],
	    	writeln(S, Query)
	    ),
	    hline(Options),
	    on(pre,Options)
	;
	    handle_comment(A,B,Options,Module)
	).
process_query(Goal, Fct, Options, Module):-
	colored_simple(Fct, control, Options),
	( complex_condition(Goal) ->
	    Indent = 1,		% multi-line query
	    o_nl(Options)
	;
	    Indent = 0,		% single-line query
	    o_write(Options, ' ')
	),
	body(Goal, Indent, none, 1200, Options, Module),
	o_write(Options, '.').



% We assume the following fixed precedences:
%	:- 1200
%	?- 1200
%	1200 :- 1200
%	1200 --> 1200
%	-?-> 1180
%	1100 ; 1101
%	1100 do 1101
%	1050 -> 1051
%	1000 , 1001
% if the subterm's top functor is an operator with Prec >= Number
% then the whole subterm must be parenthesised

clause_statement((Head :- -?-> Body), Options, Module) ?- !,
	head(Head,Options, Module),
	blank(Options),
	colored((:- -?->), control, Options),
	o_nl(Options),
	body(Body,2,none, 1180,Options, Module),
	o_write(Options,'.').
clause_statement((Head :- Body), Options, Module) ?- !,
	head(Head,Options, Module),
	blank(Options),
	colored_simple((:-), control, Options),
	o_nl(Options),
	body(Body,2,none, 1200,Options, Module),
	o_write(Options,'.').
clause_statement((Head ?- Body), Options, Module) ?- !,
	head(Head,Options, Module),
	blank(Options),
	colored_simple((?-), control, Options),
	o_nl(Options),
	body(Body,2,none, 1200,Options, Module),
	o_write(Options,'.').
clause_statement((Head --> Body), Options, Module) ?- !,
	head(Head,Options, Module),
	blank(Options),
	colored((-->), control, Options),
	dcg_body(Body,2,1200,Options, Module),
	o_write(Options,'.').
clause_statement('coverage:exit'(Count,Fact), Options, Module) ?- !,
	head(Fact, Options, Module),		% Fact
	coverage_count(Count, Options),
	o_write(Options,'.').
clause_statement('instrument:point_after'(Count,Fact), Options, Module) ?- !,
	head(Fact, Options, Module),		% Fact
	coverage_count(Count, Options),
	o_write(Options,'.').
clause_statement(Fact, Options, Module) :-
	head(Fact, Options, Module),		% Fact
	o_write(Options,'.').


/*
print the comment statement in a nice form
*/
handle_comment(A,B,Options,Module):-
%	o_nl(Options),
	on(h3,Options),
	colored(':- comment(',builtin,Options),
	colored(A,type,Options),
	colored(', ',normal,Options),
	off(h3,Options),
	comment_list(B,Options,Module),
	o_write(Options,').').

comment_list([H|T],Options,Module):-
	!,
	colored('[',builtin,Options),
	comment_list_pairs([H|T],Options,Module),
	colored(']',builtin,Options).
comment_list(X,Options,Module):-
	comment_term(X,Options,Module).

comment_list_pairs([],_Options,_Module).
comment_list_pairs([X],Options,Module):- !,
	comment_list_pair(X,Options,Module).
comment_list_pairs([A,B|R],Options,Module):-
	comment_list_pair(A,Options,Module),
	colored(', ',normal,Options),
	comment_list_pairs([B|R],Options,Module).

comment_list_pair(A:A1,Options,Module):-
	!,
	o_nl(Options),
	coloredq(A,type,Options),
	colored(':',builtin,Options),
	comment_list(A1,Options,Module).
comment_list_pair(A,Options,Module):-
	comment_term(A,Options,Module).

comment_term(html(X),Options,_Module):-
	!,
	o_write(Options,'html('),
	off(pre,Options),
	o_printf(Options,"%QDvNw", [X]),
	on(pre,Options),
	o_write(Options,')').
comment_term(X,Options,_Module):-
	string(X),
	!,
	on(p,Options),
	o_printf(Options,"%QDvNw", [X]),
	off(p,Options).
comment_term(X,Options,Module):-
	term(X,none,1000,Options,Module).


/*
print the head of a clause or a fact
*/
head(X, Options, Module):-
	functor(X,F,N),
	arg(last_pred of options, Options, F1/N1),
	(F1/N1 \= F/N ->
	    object_spec_to_filename(F/N, Anchor),
	    anchor(Anchor, Options),
	    setarg(last_pred of options, Options, F/N)
	;
	    true
	),
	term(X,head,1200,Options, Module).



/*
Body of definite clause grammar rule
*/
dcg_body((A,B), Indent, _Prec, Options, Module) ?- !,
	dcg_body(A, Indent, 1000,  Options, Module),
	o_write(Options,','),
	dcg_body(B, Indent, 1001,  Options, Module).
dcg_body((A->B;C), Indent, _Prec,  Options, Module) ?- !,
	Indent1 is Indent+1,
	o_nl(Options), indent(Options,Indent),
	colored_simple('(', control,Options),
	dcg_body(A, Indent1, 1050,  Options, Module),
	o_nl(Options), indent(Options,Indent),
	colored('->', control,Options),
	dcg_body(B, Indent1, 1051,  Options, Module),
	o_nl(Options), indent(Options,Indent),
	colored_simple(';', control,Options),
	dcg_body(C, Indent1, 1101,  Options, Module),
	o_nl(Options), indent(Options,Indent),
	colored_simple(')', control,Options).
dcg_body((A;B), Indent, _Prec,  Options, Module) ?- !,
	Indent1 is Indent+1,
	o_nl(Options), indent(Options,Indent),
	colored_simple('(', control,Options),
	dcg_body(A, Indent1, 1100,  Options, Module),
	o_nl(Options), indent(Options,Indent),
	colored_simple(';', control,Options),
	dcg_body(B, Indent1, 1101,  Options, Module),
	o_nl(Options), indent(Options,Indent),
	colored_simple(')', control,Options).
dcg_body({Goals}, Indent, _Prec,  Options, Module) ?- !,
	o_nl(Options), indent(Options,Indent),
	colored_simple('{', control,Options),
	Indent1 is Indent+1,
	o_nl(Options), body(Goals, Indent1, none, 1200,  Options, Module),
	o_nl(Options), indent(Options,Indent),
	colored_simple('}', control,Options).
dcg_body(Term, Indent, Prec,  Options, Module) :-
	o_nl(Options),
	body(Term, Indent, none, Prec,  Options, Module).

	

/*
print the body of a clause
leaves you at the end of a line
*/

body(X,Indent,CallCov,_Prec, Options, Module):-
	var(X),
	!,
	indent(Options,Indent),
	coverage_count(CallCov, Options),
	term(X,none,1000, Options, Module).
body('coverage:call'(Count,Term),Indent,CallCov,Prec,Options,Module):-
	!,
	coverage_count(CallCov, Options),
	body(Term,Indent,Count,Prec,Options,Module).
body('coverage:exit'(Count,Term),Indent,CallCov,Prec,Options,Module):-
	!,
	body(Term,Indent,CallCov,Prec,Options,Module),
	coverage_count(Count, Options).
body('coverage:call_exit'(CallCount,Term,ExitCount),Indent,CallCov,Prec,Options,Module):-
	!,
	coverage_count(CallCov, Options),
	body(Term,Indent,CallCount,Prec,Options,Module),
	coverage_count(ExitCount, Options).
body('instrument:point_before'(Count,Term),Indent,CallCov,Prec,Options,Module):-
	!,
	coverage_count(CallCov, Options),
	body(Term,Indent,Count,Prec,Options,Module).
body('instrument:point_after'(Count,Term),Indent,CallCov,Prec,Options,Module):-
	!,
	body(Term,Indent,CallCov,Prec,Options,Module),
	coverage_count(Count, Options).
body((X , Y), Indent,CallCov, _Prec, Options, Module):-
	!,
	body(X, Indent,CallCov, 1000, Options, Module),
	o_writeln(Options,','),
	body(Y, Indent,none, 1001, Options, Module).
body((X -> Y ; Z),Indent,CallCov,_Prec, Options, Module):-
	!,
	%% Code to cover multiple if then else
	(
	    fromto(Z, In, Out, []),
	    fromto(Bodies, Body2, Body1, []),
	    fromto(Conditions, Conditions2, Conditions1, [])
	do
	    ( var(In) ->
		  Conditions2 = Conditions1,
		  Body2 = [In|Body1],
		  Out = []
	    ; In = (X -> Y ; Z) ->
		  Conditions2 = [X|Conditions1],
		  Body2 = [Y|Body1],
		  Out = Z
	    ; In = (X -> Y) ->
		  Conditions2 = [X|Conditions1],
		  Body2 = [Y|Body1],
		  Out = []
	    ;
		  Out = [],
		  Body2 = [In|Body1],
		  Conditions2 = Conditions1
	    )
	),
	AllConditions = [X|Conditions],
	AllBodies = [Y|Bodies],
	( 
	    foreach(Con, AllConditions),
	    fromto(no,In,Out,Complex)
	do
	    ( complex_condition(Con) ->
		  Out = yes
	    ;
		  Out = In
	    )
	),
	Indent1 is Indent+1,
	indent(Options,Indent),
	coverage_count(CallCov, Options),
	colored_simple('(', control,Options),
	(
	    foreach(Condition, AllConditions),
	    fromto(AllBodies, [Body|Bodies], Bodies, FinalBody),
	    param(Complex, Indent, Indent1,  Options, Module)
	do
	    ( Complex == no ->
		  %% Single line condition
		  CondIndent = 0,
		  blank(Options),
		  body(Condition,CondIndent,none, 1000, Options, Module), 
		  blank(Options)
	    ;
		  %% Use new line
		  CondIndent is Indent + 1,
		  o_nl(Options),
		  body(Condition,CondIndent,none,1000, Options, Module), 
		  o_nl(Options),
		  indent(Options,Indent)
	    ),
	    colored('->', control,Options),
	    o_nl(Options),
	    body(Body,Indent1,none,1051, Options, Module),
	    o_nl(Options),
	    ( Bodies == [] ->
		  true
	    ;
		  indent(Options,Indent),
		  colored_simple(';', control,Options)
	    )
	),
	( FinalBody = [Body] ->
	      o_nl(Options),
	      body(Body,Indent1,none,1000, Options, Module),
	      o_nl(Options),
	      indent(Options,Indent),
	      colored_simple(')', control,Options)
	;
	      indent(Options,Indent),
	      colored_simple(')', control,Options)
	).
body((X -> Y),Indent,CallCov,_Prec, Options, Module):-
	!,
	Indent1 is Indent+1,
	indent(Options,Indent),
	coverage_count(CallCov, Options),
	colored_simple('(', control,Options),
	( complex_condition(X) ->
	      %% Use new line
	      CondIndent is Indent + 1,
	      o_nl(Options),
	      body(X,CondIndent,none, 1000, Options, Module), 
	      o_nl(Options),
	      indent(Options,Indent)
	;
	      %% Single line condition
	      CondIndent = 0,
	      blank(Options),
	      body(X,CondIndent,none, 1000, Options, Module), 
	      blank(Options)
	),
	colored('->', control,Options),
	o_nl(Options),
	body(Y,Indent1,none, 1051, Options, Module),
	o_nl(Options),
	indent(Options,Indent),
	colored_simple(')', control,Options).
body((X;Y),Indent,CallCov,_Prec, Options, Module):-
	!,
	Indent1 is Indent+1,
	indent(Options,Indent),
	coverage_count(CallCov, Options),
	colored_simple('(', control,Options),
	o_nl(Options),
	body(X,Indent1,none, 1100, Options, Module),
	o_nl(Options),
	indent(Options,Indent),
	colored_simple(';', control,Options),o_nl(Options),
	body(Y,Indent1,none, 1101, Options, Module),
	o_nl(Options),
	indent(Options,Indent),
	colored_simple(')', control,Options).
body((X do Y),Indent,CallCov,_Prec, Options, Module):-
	!,
	Indent1 is Indent+1,
	indent(Options,Indent),
	coverage_count(CallCov, Options),
	colored_simple('(', control,Options),
	o_nl(Options),
	body(X,Indent1,none, 1100, Options, Module),o_nl(Options),
	indent(Options,Indent),
	colored_simple(do, control,Options),o_nl(Options),
	body(Y,Indent1,none, 1101, Options, Module),
	o_nl(Options),
	indent(Options,Indent),
	colored_simple(')', control,Options).
body(X,Indent,CallCov,Prec, Options, Module):-
	indent(Options,Indent),
	coverage_count(CallCov, Options),
	term(X, +, Prec, Options, Module).


%% more than one line
complex_condition((_,_)) ?- true.
complex_condition((_ ; _)) ?- true.
complex_condition((_->_)) ?- true.


/*
get the color of a predicate, if possible also get a reference to its 
definition or manual
*/
:- mode function_color(+,+,-,+,+).

function_color(F,N,warning-refer(Path), Options, _Module):-
	dangerous(F/N),
	search_for(F/N,Path,Options),
	!.
function_color(F,N,builtin-refer(Path), Options, Module):-
	current_built_in(F/N)@Module,
	search_for(F/N,Path,Options),
	!.
function_color(F,N,warning, _Options, _Module):-
	dangerous(F/N),
	!.
function_color(F,N,builtin, _Options, Module):-
	current_built_in(F/N)@Module,
	!.
function_color(F,N,call-refer(Anchor), _Options, Module) :-
	object_spec_to_filename(F/N, SimpleFN),
	(
	    get_flag(F/N, definition_module, DefMod)@Module,
	    DefMod \= Module
	->
	    % attempt a cross-file link:
	    ( get_flag(F/N, source_file, File)@Module ->
		pathname(File, _, B, _),
		concat_string([B,'.html',#,SimpleFN], Anchor)
	    ;
		% For tools etc which don't have a source_file:
		% guess the file name is the same as the module name
		% Do not use source_file but definition_module to create a
		% relative link assuming all files are placed under the
		% same directory possibly different from source directory.
		% Also works with tools etc which don't have a source_file.
		concat_string([DefMod,'.html',#,SimpleFN], Anchor)
	    )
	;
	    % within this file
	    concat_string([#,SimpleFN], Anchor)
	).

/*
indent N*4 spaces
*/
indent(Options,N):-
	( N > 0 ->
	    o_write(Options, "    "),
	    N1 is N-1,
	    indent(Options,N1)
	;
	    true
	).

/*

print out a term with the main functor in Color
possibly with a meta calls inside and inside a term of precedence Prec
different cases depending on its type

Meta is '+' when X is a goal
Meta is 'head' when X is a head

*/


term(X,Meta,_Prec,Options,_Module):-
	var(X),
	!,
	( Meta == head ->
	    coloredmeta(X,variable,Options)
	;
	    coloredq(X,variable,Options)
	).
term(X,_Meta,_Prec,Options,_Module):-
	string(X),
	!,
	coloredq(X,string,Options). % quoted printout
term(X,_Meta,_Prec,Options,_Module):-
	number(X),
	!,
	coloredq(X,number,Options).
term('coverage:call'(Count,Term),Meta,Prec,Options,Module):-
	!,
	coverage_count(Count, Options),
	term(Term,Meta,Prec,Options,Module).
term('coverage:exit'(Count,Term),Meta,Prec,Options,Module):-
	!,
	term(Term,Meta,Prec,Options,Module),
	coverage_count(Count, Options).
term('coverage:call_exit'(CallCount,Term,ExitCount),Meta,Prec,Options,Module):-
	!,
	coverage_count(CallCount, Options),
	term(Term,Meta,Prec,Options,Module),
	coverage_count(ExitCount, Options).
term('instrument:point_before'(Count,Term),Meta,Prec,Options,Module):-
	!,
	coverage_count(Count, Options),
	term(Term,Meta,Prec,Options,Module).
term('instrument:point_after'(Count,Term),Meta,Prec,Options,Module):-
	!,
	term(Term,Meta,Prec,Options,Module),
	coverage_count(Count, Options).
term(X,_Meta,Prec,Options,Module):-
	X = subscript(A,I),
	(compound(A);var(A)),
	nonvar(I), I = [_|_],
	not get_flag(syntax_option, no_array_subscripts)@Module,
	!,
	term_arg(A,none,1,Prec,Options,Module),
	term_arg(I,none,2,Prec,Options,Module).
term(X,_Meta,_Prec,Options,Module):-
	X = no_macro_expansion(with(S,Args)),
	atom(S),
	proper_list(Args),
	not get_flag(syntax_option, no_curly_arguments)@Module,
	!,
	coloredq(S,none,Options),
	o_write(Options,'{'),
	( Args = [] -> true ; term_args(Args,none,1,Options,Module) ),
	o_write(Options,'}').
term(X,Meta,Prec,Options,Module):-
	X = no_macro_expansion('with attributes'(Var,Attrs)),
	var(Var),
	proper_list(Attrs), Attrs = [_|_],
	not get_flag(syntax_option, no_attributes)@Module,
	!,
	term(Var,Meta,Prec,Options,Module),
	o_write(Options,'{'),
	term_args(Attrs,none,1,Options,Module),
	o_write(Options,'}').
term(X,Meta,Prec,Options,Module):-
	X = no_macro_expansion(apply(Var,Args)),
	var(Var),
	proper_list(Args), Args = [_|_],
	get_flag(syntax_option, var_functor_is_apply)@Module,
	!,
	term(Var,Meta,Prec,Options,Module),
	o_write(Options,'('),
	term_args(Args,none,1,Options,Module),
	o_write(Options,')').
term(X,Meta,Prec,Options,Module):-
	atom(X),
	!,
	meta_color(X,Meta,Color,Options,Module),
	atom_is_also_operator(X,Prec1,Module),
	parenthesis_open(Options,Prec,Prec1),
	coloredq(X,Color,Options), % quoted printout
	parenthesis_close(Options,Prec,Prec1).
term([H|T],_Meta,_Prec,Options,Module):-
	!,
	list([H|T],Options,Module).
term(X,Meta,Prec,Options,Module):-
	infix_operator(X,F,OpPrec,LPrec,RPrec,Module),
	!,
	meta_color(X,Meta,Color,Options,Module),
	meta_args(Meta, X, Pattern),
	arg(1,X,A1),
	arg(2,X,A2),
	parenthesis_open(Options,Prec,OpPrec),
	term_arg(A1,Pattern,1,LPrec,Options,Module),
	blank(Options),
	colored(F,Color,Options),
	blank(Options),
	term_arg(A2,Pattern,2,RPrec,Options,Module),
	parenthesis_close(Options,Prec,OpPrec).
term(X,Meta,Prec,Options,Module):-
	prefix2_operator(X,F,OpPrec,LPrec,RPrec,Module),
	!,
	meta_color(X,Meta,Color,Options,Module),
	meta_args(Meta, X, Pattern),
	arg(1,X,A1),
	arg(2,X,A2),
	parenthesis_open(Options,Prec,OpPrec),
	colored(F,Color,Options),
	blank(Options),
	term_arg(A1,Pattern,1,LPrec,Options,Module),
	blank(Options),
	term_arg(A2,Pattern,2,RPrec,Options,Module),
	parenthesis_close(Options,Prec,OpPrec).
term(X,Meta,Prec,Options,Module):-
	prefix_operator(X,F,OpPrec,Prec1,Module),
	!,
	meta_color(X,Meta,Color,Options,Module),
	meta_args(Meta, X, Pattern),
	arg(1,X,A1),
	parenthesis_open(Options,Prec,OpPrec),
	colored(F,Color,Options),
	blank(Options),
	term_arg(A1,Pattern,1,Prec1,Options,Module),
	parenthesis_close(Options,Prec,OpPrec).
term(X,Meta,Prec,Options,Module):-
	postfix_operator(X,F,OpPrec,Prec1,Module),
	!,
	meta_color(X,Meta,Color,Options,Module),
	meta_args(Meta, X, Pattern),
	arg(1,X,A1),
	parenthesis_open(Options,Prec,OpPrec),
	term_arg(A1,Pattern,1,Prec1,Options,Module),
	blank(Options),
	colored(F,Color,Options),
	parenthesis_close(Options,Prec,OpPrec).
term(X,Meta,_Prec,Options,Module):-
	functor(X,F,N),
	N > 0,
	!,
	meta_color(X,Meta,Color,Options,Module),
	meta_args(Meta, X, Pattern),
	coloredq(F,Color,Options),
	o_write(Options,'('),
	X =.. [_|L],
	term_args(L,Pattern,1,Options,Module),
	o_write(Options,')').
term(X,_Meta,_Prec,Options,_Module):- % default case
	o_writeq(Options,X).

    proper_list([]) ?- true.
    proper_list([_|T]) ?- proper_list(T).


/*
decide if we need parenthesis around a term of precdence Prec inside a term 
of Precedence Prec1, where the operator definition gives either x or y 
preference
*/
parenthesis_open(Options,Prec,Prec1):-
	( Prec =< Prec1 -> o_write(Options,'(') ; true ).

parenthesis_close(Options,Prec,Prec1):-
	( Prec =< Prec1 -> o_write(Options,')') ; true ).


/*
special printout for lists
*/

list([H|T],Options,Module):-
	o_write(Options,'['),
	list_args([H|T],Options,Module),
	o_write(Options,']').

list_args([X,Y|R],Options,Module) ?- !,
	term(X,none,1000,Options,Module),
	o_write(Options,', '),
	list_args([Y|R],Options,Module).
list_args([X],Options,Module) ?- !,
	term(X,none,1000,Options,Module).
list_args([X|R],Options,Module) :-
	term(X,none,1000,Options,Module),
	o_write(Options,'|'),
	term(R,none,1000,Options,Module).

/*
print the arguments of a term comma-separated
*/
term_args([X],Meta,N,Options,Module):- !,
	term_arg(X,Meta,N,1000,Options,Module).
term_args([X,Y|R],Meta,N,Options,Module):-
	term_arg(X,Meta,N,1000,Options,Module),
	o_write(Options,', '),
	N1 is N+1,
	term_args([Y|R],Meta,N1,Options,Module).

/*
print one argument of a term
find out if it is meta-called
*/
term_arg(X,Meta,N,Prec,Options,Module):-
	meta_arg(N,Meta,MetaN),
	term(X,MetaN,Prec,Options,Module).

meta_arg(_,head,A) :- !, A = head.
meta_arg(K,T,A):-
	functor(T,_F,N),
	K =< N,
	!,
	arg(K,T,A).
meta_arg(_,_,none).


meta_args(head, _, Pattern) :- !, Pattern = head.
meta_args(+, Goal, Pattern) :-
	functor(Goal, F, N),
	functor(Pattern, F, N),
	metacall_table(Pattern,_,_),
	!.
meta_args(M, Goal, Pattern) :- integer(M),
	functor(Goal, F, N),
	Arity is N+M,
	functor(Pattern, F, Arity),
	metacall_table(Pattern,_,_),
	!.
meta_args(_, _NoGoal, none).


meta_color(Term, Context, Color, Options, Module) :-
	%( atom(Term) ; compound(Term) ),
	functor(Term, F, N),
	!,
	( Context = (+) ->
	    function_color(F,N,Color,Options,Module)
	; Context = p ->
	    ( Term = F1/N1 ->
		function_color(F1,N1,Color,Options,Module)
	    ; atom(Term) ->
		Color = atom
	    ;
		Color = functor
	    )
	; Context = head ->
	    Color = head
	; integer(Context) ->
	    N1 is N+Context,
	    function_color(F,N1,Color,Options,Module)
	; atom(Term) ->
	    Color = atom
	;
	    Color = functor
	).


/*

utility stuff

*/


blank(options with stream:S):-
	put(S,32).

prefix_operator(X,F,Prec,RPrec,Module):-
	functor(X,F,1),
	current_op(Prec,Type,F)@Module,
	( Type = fx -> RPrec = Prec
	; Type = fy, RPrec is Prec+1
	),
	!.

postfix_operator(X,F,Prec,LPrec,Module):-
	functor(X,F,1),
	current_op(Prec,Type,F)@Module,
	( Type = xf -> LPrec = Prec
	; Type = yf, LPrec is Prec+1
	),
	!.

infix_operator(X,F,Prec,LPrec,RPrec,Module):-
	functor(X,F,2),
	current_op(Prec,Type,F)@Module,
	( Type = xfx -> LPrec = Prec, RPrec = Prec
	; Type = xfy -> LPrec = Prec, RPrec is Prec+1
	; Type = yfx, LPrec is Prec+1, RPrec = Prec
	),
	!.

prefix2_operator(X,F,Prec,LPrec,RPrec,Module):-
	functor(X,F,2),
	current_op(Prec,Type,F)@Module,
	( Type = fxx -> LPrec = Prec, RPrec = Prec
	; Type = fxy, LPrec = Prec, RPrec is Prec+1
	),
	!.


atom_is_also_operator(X,Prec,Module):-
	atom(X),
	current_op(Prec,_Type,X)@Module,
	!,
	findall(P, current_op(P,_Type,X)@Module, Ops),
	sort(0, >, Ops, [Prec|_]).	% get highest precedence
atom_is_also_operator(_X,0,_Module).


/*

find the html file for the given predspec
this uses a positive and negative lemma generator to remember previous searches
you may wnat to erase all keys at the start of the program
note that the file name found is specific to a Eclipse installation
this can not be used on another machine with a different intallation directory

*/
:-mode search_for(++,-,+).
search_for(F/N,_Path,_Options):-
	recorded(search_for,search_for(F,N,none)),
	!,
	fail.
search_for(F/N,Path,_Options):-
	recorded(search_for,search_for(F,N,Path)),
	!.
search_for(F/N,Path,_Options):-
	lookup_builtin_help(F/N,Path),
	!.
search_for(F/N,Path, options with outdir:D1):-
	htm_file(F,N,File),
	concat_string([D1,/],D2),
	search_for_html(D2,File,Path),
	record(search_for,search_for(F,N,Path)),
	!.
search_for(F/N,_Path,_Options):-
	record(search_for,search_for(F,N,none)),
	!,
	fail.

search_for_html(D,File,Path):-
	read_directory(D,*,Dirs,Files),
	search_for_html(D,File,Dirs,Files,Path).

search_for_html(D,File,_Dirs,Files,Path):-
	member(File,Files),
	!,
	concat_string([D,File],Path_int),
	file_to_url(Path_int, Path).
search_for_html(D,File,Dirs,_Files,Path):-
	member(Sub,Dirs),
	concat_string([D,Sub,'/'],D1),
	search_for_html(D1,File,Path).
	

htm_file(F,N,File):-
	object_spec_to_filename(F/N, SimpleFN),
	concat_string([SimpleFN,'.html'],File).


lookup_builtin_help(F/N, Url) :-
	get_flag(installation_directory,D),
	( current_predicate(bip/5) ->
	    true
	;
	    concat_string([D,'/doc/bips/index.pl'],IndexFile),
	    exists(IndexFile),
	    compile(IndexFile)
	),
	once call(bip(F, N, Group, Lib, Base)),
	concat_string([D,'/doc/bips/',Group,/,Lib,/,Base,'.html'],File),
	file_to_url(File, Url).

file_to_url(File, Url) :-
    	( substring(File, "/", 1) ->				% absolute path
	    ( string_list(File, [0'/,0'/,Drive,0'/|Rest]) ->
		string_list(File1, [0'/,Drive,0':,0'/|Rest])	% Windows
	    ;
		File1 = File					% Unix
	    ),
	    concat_string(["file://",File1], Url)
	;
	    Url = File						% relative path
	).



/*************************************************************************

the low level output routines, 
print different things according to the options

*************************************************************************/

% print some text X known not to contain tricky characters
colored_simple(X, StyleLink, options with [format:html,stream:S,css_span:C]) :-
	!,
	( StyleLink = Style-refer(Ref) ->
	    printf(S, "<a class=\"%s\" href=\"%w\">%w</a>", [Style,Ref,X])
	;
	    printf(S, "<%w class=\"%s\">%w</%w>", [C,StyleLink,X,C])
	).
colored_simple(X, _Style, options with [format:txt,stream:S]) :-
	write(S, X).


% print some text X
colored(X, StyleLink, options with [format:html,stream:S,css_span:C]) :-
	!,
	htmlify_thing(X, HtmlX),
	( StyleLink = Style-refer(Ref) ->
	    printf(S, "<a class=\"%s\" href=\"%w\">%w</a>", [Style,Ref,HtmlX])
	;
	    printf(S, "<%w class=\"%s\">%w</%w>", [C,StyleLink,HtmlX,C])
	).
colored(X, _Style, options with [format:txt,stream:S]) :-
	write(S, X).


% print some text in quoted form
coloredq(X, StyleLink, options with [format:html,stream:S,css_span:C]) :-
	!,
	htmlify_thing(X, HtmlX),
	( StyleLink = Style-refer(Ref) ->
	    printf(S, "<a class=\"%s\" href=\"%w\">%Qw</a>", [Style,Ref,HtmlX])
	;
	    printf(S, "<%w class=\"%s\">%Qw</%w>", [C,StyleLink,HtmlX,C])
	).
coloredq(X, _Style, options with [format:txt,stream:S]) :-
	printf(S, "%Qw", [X]).


% print attributed variable
% note that this is likely to print more attributes than needed, but we
% have no easy way of knowing which ones actually occurred in the source.
coloredmeta(X, StyleLink, options with [format:html,stream:S,css_span:C]) :-
	!,
	printf(S, "<%w class=\"%s\">%MQw</%w>", [C,StyleLink,X,C]).
coloredmeta(X, _Style, options with [format:txt,stream:S]) :-
	printf(S, "%MQw", [X]).


    htmlify_thing(X, HtmlX) :-
    	string(X), !,
	% the following is a bit faster than calling htmlify_string directly
	( split_string(X, "<>&", "", [HtmlX]) ->
	    true
	;
	    htmlify_string(X, HtmlX)
	).
    htmlify_thing(X, HtmlX) :-
    	atom(X), !,
	atom_string(X, XS),
	htmlify_thing(XS, HtmlXS),
	atom_string(HtmlX, HtmlXS).
    htmlify_thing(X, X).


% emit a coverage count
coverage_count(0, Options) :-
	Options = options with [css_span:font, stream:S],
	!,
	printf(S, 
	 "<span style=\"background: #FF0000\" class=\"nocoverage\"> 0 </span>", 
	 []).
coverage_count(0, Options) :- !,
	colored_simple(' 0 ', nocoverage, Options).
coverage_count(none, _) :- !.
coverage_count(Count, Options) :-
	Options = options with [css_span:font, stream:S],
	number(Count),
	!,
	printf(S, 
   	  "<span style=\"background: #32CD32\" class=\"coverage\"> %w </span>",
	  [Count]).
coverage_count(Count, Options) :-
	number(Count), !,
	concat_string([' ',Count,' '], Text),
	colored_simple(Text, coverage, Options).
coverage_count(Count, Options) :-
	Options = options with [css_span:font, stream:S],
	!,
	term_string(Count, Text),
	printf(S, 
   	  "<span style=\"background: #32CD32\" class=\"coverage\"> %w </span>",
	  [Text]).
coverage_count(Count, Options) :-
	term_string(Count, Text),
	colored_simple(Text, coverage, Options).
%coverage_count(_none, _).


% create an anchor to text X
anchor(X, options with [format:html,stream:S]) :- !,
	printf(S,"<a name=\"%w\"></a>",[X]).
anchor(_X, _Options).


% turn on tag X
on(X, options with [format:html,stream:S]) :- !,
	printf(S,"<%w>",[X]).
on(_X, _Options).


% turn off tag X
off(X, options with [format:html,stream:S]) :- !,
	printf(S,"</%w>",[X]).
off(_X, _Options).


% print some text Y with a tag X
onoff(X,Y, options with [format:html,stream:S]) :- !,
	!,
	printf(S,"<%w>%w</%w>",[X,Y,X]).
onoff(_X,_Y, _Options).


% print horizontal line
hline(options with [format:html,stream:S]) :- !,
	writeln(S,"<HR>").
hline(_).


% print only in html format
extra(Format, Text, options with [format:html,stream:S]) :- !,
	!,
	printf(S, Format, Text).
extra(_, _, _).


o_nl(options with stream:S) :-
	nl(S).

o_write(options with stream:S, Term) :-
	write(S, Term).

o_writeln(options with stream:S, Term) :-
	writeln(S, Term).

o_writeq(options with stream:S, Term) :-
	writeq(S, Term).

o_printf(options with stream:S, Format, Term) :-
	printf(S, Format, Term).



/*
 * description of all meta called predicates
 */

metacall_table((+ , +),T,T).
metacall_table((+ ; +),T,T).
metacall_table((+ -> +),T,T).
metacall_table(once(+),T,T).
metacall_table(once(+),T,T).
metacall_table(call(+),T,T).
metacall_table(not(+),T,T).
metacall_table(fail_if(+),T,T).
metacall_table(\+(+),T,T).
metacall_table(~(+),T,T).
metacall_table(call_priority(+,-),T,T).
metacall_table(call_priority(+,-,-),T,T).
metacall_table(maplist(2,-,-),T,T).
metacall_table(block(+,-,+),_T,block).
metacall_table(do(-,+),_T,block).
metacall_table(assert(+),_T,assert).
metacall_table(asserta(+),_T,assert).
metacall_table(assertz(+),_T,assert).
metacall_table(retract(+),_T,retract).
metacall_table(retract_all(+),_T,retract).
metacall_table(retractall(+),_T,retract).
metacall_table(make_suspension(+,-,-),_T,suspend).
metacall_table(suspend(+,-,-),_T,suspend).
metacall_table(suspend(+,-,-,-),_T,suspend).
metacall_table(minimize(+,-),_T,optimize).
metacall_table(minimize(+,-,-,-),_T,optimize).
metacall_table(minimize(+,-,-,-,-),_T,optimize).
metacall_table(minimize(+,-,-,-,-,-),_T,optimize).
metacall_table(minimize(+,-,-,-,-,-,-,-),_T,optimize).
metacall_table(min_max(+,-),_T,optimize).
metacall_table(min_max(+,-,-,-),_T,optimize).
metacall_table(min_max(+,-,-,-,-),_T,optimize).
metacall_table(min_max(+,-,-,-,-,-),_T,optimize).
metacall_table(min_max(+,-,-,-,-,-,-,-),_T,optimize).
metacall_table(findall(-,+,-),_T,all).
metacall_table(setof(-,+,-),_T,all).
metacall_table(bagof(-,+,-),_T,all).
metacall_table(coverof(-,+,-),_T,all).
metacall_table(^(-,+),_T,all).
metacall_table(-?->(+),_T,all).
metacall_table(@(+,-),_T,all).
metacall_table(set_error_handler(-,p),_T,error).
metacall_table(set_default_error_handler(-,p),_T,error).
metacall_table(set_event_handler(-,p),_T,error).
metacall_table(export(p),T,T).
metacall_table(local(p),T,T).

% special user-defined metacalls for the retimer source code
metacall_table(annotated_constr(+,-),_T,user).
metacall_table(option_init_trace(+),_T,user).
metacall_table(option_trace(+),_T,user).
metacall_table(local_minimize(+,-,-,-,-,-,-,-),_T,user).
metacall_table(wrap_call(+),_T,user).
metacall_table(wrap_bottom_call(+,-,-,-,-,-),_T,user).
metacall_table(au_dichotomic(+,-,-,-),_T,user).
metacall_table(au_all_solns(+,-,-),_T,user).
metacall_table(timeout(+,-,1),_T,user).
metacall_table(verbose(+,-),_T,user).
metacall_table(display_call(-,-,-,-,+,-,-,-),_T,user).
metacall_table(display_manage_backtrack(+,+),_T,user).
metacall_table(conditional_call(+,+,-),_T,user).
metacall_table(condition_var(+,-),_T,user).
metacall_table(foldlbasecase(3,-,-,-),_T,user).
metacall_table(abstract_constraint(+),_T,user).
metacall_table(filter(1,-,-),_T,user).
metacall_table(filter(1,-,-,-),_T,user).


/*
:-comment(dangerous/1,[
summary:"see if a predicate is considered dangerous",
amode:dangerous(?),

args:[
      "PredSpec" : "term of the form Pred/Arity "
],
desc:html("
The table lists some predicates which are considered dangerous, i.e. they should be used with care in an application program. Many of these predicates perform some side-effect, like changing the program database, or control the backtracking of the application. <p>
Dangerous does not mean that you should avoid using them, but that you should be aware of their effect.<p>
"),
fail_if:"fails if PredSpec is not the name of a dangeruous predicate",
resat:yes,
eg:"
dangerous(assert/1).
yes

dangerous(toto/0).
no
",

see_also:[]

]).
*/

dangerous(foreach/2).
dangerous(foreach/2).
dangerous(param/1).
dangerous(param/2).
dangerous(param/3).
dangerous(param/4).
dangerous(param/5).
dangerous(param/6).
dangerous(param/7).
dangerous(foreacharg/2).
dangerous(fromto/4).
dangerous(for/3).
dangerous(for/4).
dangerous(loop_name/1).
dangerous(count/3).
dangerous(assert/1).
dangerous(asserta/1).
dangerous(assertz/1).
dangerous(retract/1).
dangerous(retract_all/1).
dangerous(retractall/1).
dangerous((abolish)/1).
dangerous(record/2).
dangerous(recorded/2).
dangerous(erase_all/1).
dangerous(xset/2).
dangerous(xget/2).
dangerous(getval/2).
dangerous(setval/2).
dangerous(incval/1).
dangerous(decval/1).
dangerous(exit/1).
dangerous(arr_create/4).
dangerous(suspend/3).
dangerous(suspend/4).
dangerous(exec/2).
dangerous(exec/3).
dangerous(define_macro/3).
dangerous(set_error_handler/2).
dangerous(set_event_handler/2).
dangerous(set_default_error_handler/2).
dangerous(setarg/3).
dangerous(error/2).
dangerous(include/1).
dangerous(meta_attribute/2).
dangerous(abort/0).
dangerous(halt/0).
dangerous((not)/1).
dangerous(repeat/0).
dangerous(wake/0).
dangerous(!/0).
dangerous((-->)/2).
dangerous((\+)/1).

