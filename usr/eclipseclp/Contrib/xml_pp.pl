/* xml_pp: "pretty print" an XML Document on the current output stream.
 *
 * Copyright (C) 2001-2005 Binding Time Limited
 * Copyright (C) 2005, 2006 John Fletcher
 *
 * Current Release: $Revision: 1.2 $
 * 
 * TERMS AND CONDITIONS:
 *
 * This program is offered free of charge, as unsupported source code. You may
 * use it, copy it, distribute it, modify it or sell it without restriction,
 * but entirely at your own risk.
 */

:- ensure_loaded( xml_utilities ).

/* xml_pp( +XMLDocument ) "pretty prints" XMLDocument on the current
 * output stream.
 */
xml_pp( xml(Attributes, Document) ) :-
	write( 'xml( ' ), pp_attributes( Attributes, "" ), put( 0', ), nl,
	xml_pp_list( Document, "	" ),
	format( ' ).~n', [] ).
xml_pp( malformed(Attributes, Document) ) :-
	write( 'malformed( ' ), pp_attributes( Attributes, "" ), put( 0', ), nl,
	xml_pp_list( Document, "	" ),
	format( ' ).~n', [] ).

xml_pp_indented( [], Indent ) :-
	format( '~s[]', [Indent] ).
xml_pp_indented( List, Indent ) :-
	List = [_|_],
	format( '~s', [Indent] ),
	xml_pp_list( List, Indent ).
xml_pp_indented( comment(Text), Indent ) :-
	format( '~scomment(', [Indent] ), pp_string(Text), put( 0') ). %'
xml_pp_indented( namespace(URI,Prefix,Element), Indent ) :-
	format( '~snamespace( ~q, "~s",~n', [Indent,URI,Prefix] ),
	xml_pp_indented( Element, [0'	|Indent] ),
	format( '~n~s)', [[0'	|Indent]] ).
xml_pp_indented( element(Tag,Attributes,Contents), Indent ) :-
	format( '~selement( ~q,~n', [Indent,Tag] ),
	pp_attributes( Attributes, [0'	|Indent] ), put(0',), nl,
	xml_pp_list( Contents, [0'	|Indent] ), write( ' )' ). %'
xml_pp_indented( instructions(Target, Processing), Indent ) :-
	format( '~sinstructions( ~q, ', [Indent,Target] ),
	pp_string(Processing), put( 0') ). %'
xml_pp_indented( doctype(Name, DoctypeId), Indent ) :-
	format( '~sdoctype( ~q, ', [Indent,Name] ),
	xml_pp_indented( DoctypeId, [0'	|Indent] ), %'
	write( ' )' ).
xml_pp_indented( cdata(CData), Indent ) :-
	format( '~scdata(', [Indent] ), pp_string(CData), put( 0') ). %'
xml_pp_indented( pcdata(PCData), Indent ) :-
	format( '~spcdata(', [Indent] ), pp_string(PCData), put( 0') ). %'
xml_pp_indented( public(URN,URL), _Indent ) :-
	format( 'public( "~s", "~s" )', [URN,URL] ).
xml_pp_indented( public(URN,URL,Literals), Indent ) :-
	format( 'public( "~s", "~s",~n', [URN,URL] ),
	xml_pp_list( Literals, [0'	|Indent] ), write( ' )' ). %'
xml_pp_indented( system(URL), _Indent ) :-
	format( 'system( "~s" )', [URL] ).
xml_pp_indented( system(URL,Literals), Indent ) :-
	format( 'system( "~s",~n', [URL] ),
	xml_pp_list( Literals, [0'	|Indent] ), write( ' )' ). %'
xml_pp_indented( local, _Indent ) :-
	write( local ).
xml_pp_indented( local(Literals), Indent ) :-
	write( 'local(' ), nl,
	xml_pp_list( Literals, [0'	|Indent] ), write( ' )' ). %'
xml_pp_indented( dtd_literal(String), Indent ) :-
	format( '~sdtd_literal(', [Indent] ), pp_string(String), put( 0') ). %'
xml_pp_indented( out_of_context(Tag), Indent ) :-
	format( '~s/* SYNTAX ERROR */ out_of_context( ~q )', [Indent,Tag] ).
xml_pp_indented( unparsed(String), Indent ) :-
	format( '~s/* SYNTAX ERROR */ unparsed( ', [Indent] ),
	pp_string(String), put( 0') ). %'

xml_pp_list( [], Indent ) :-
	format( '~s[]', [Indent] ).
xml_pp_list( [H|T], Indent ) :-
	format( '~s[~n', [Indent] ),
	xml_pp_indented( H, Indent ),
	xml_pp_list1( T, Indent ),
	format( '~s]', [Indent] ).

xml_pp_list1( [], _Indent ) :-
	nl.
xml_pp_list1( [H|T], Indent ) :-
	put( 0', ), nl, %'
	xml_pp_indented( H, Indent ),
	xml_pp_list1( T, Indent ).

pp_attributes( [], Indent ) :-
	format( '~s[]', [Indent] ).
pp_attributes( [Attribute|Attributes], Indent ) :-
	format( '~s[', [Indent] ),
	pp_attributes1( Attributes, Attribute ),
	put( 0'] ). %'

pp_attributes1( [], Name=Value ) :-
	pp_name( Name ), pp_string( Value ).
pp_attributes1( [H|T], Name=Value ) :-
	pp_name( Name ), pp_string( Value ), write( ', ' ),
	pp_attributes1( T, H ).


pp_name( Name ) :-
	( possible_operator( Name ) ->
		format( '(~w)=', [Name] )
	; otherwise ->
		format( '~q=', [Name] )
	).

possible_operator( (abolish) ).
possible_operator( (attribute) ).
possible_operator( (check_advice) ).
possible_operator( (compile_command) ).
possible_operator( (delay) ).
possible_operator( (demon) ).
possible_operator( (discontiguous) ).
possible_operator( (div) ).
possible_operator( (do) ).
possible_operator( (document_export) ).
possible_operator( (document_import) ).
possible_operator( (dy) ).
possible_operator( (dynamic) ).
possible_operator( (edb) ).
possible_operator( (eexport) ).
possible_operator( (else) ).
possible_operator( (except) ).
possible_operator( (export) ).
possible_operator( (foreign_pred) ).
possible_operator( (from) ).
possible_operator( (from_chars) ).
possible_operator( (from_file) ).
possible_operator( (from_stream) ).
possible_operator( (global) ).
possible_operator( (help) ).
possible_operator( (hilog) ).
possible_operator( (if) ).
possible_operator( (import) ).
possible_operator( (index) ).
possible_operator( (initialization) ).
possible_operator( (is) ).
possible_operator( (listing) ).
possible_operator( (local) ).
possible_operator( (locked) ).
possible_operator( (meta_predicate) ).
possible_operator( (mod) ).
possible_operator( (mode) ).
possible_operator( (module_transparent) ).
possible_operator( (multifile) ).
possible_operator( (namic) ).
possible_operator( (nocheck_advice) ).
possible_operator( (nospy) ).
possible_operator( (not) ).
possible_operator( (of) ).
possible_operator( (once) ).
possible_operator( (onto_chars) ).
possible_operator( (onto_file) ).
possible_operator( (onto_stream) ).
possible_operator( (parallel) ).
possible_operator( (public) ).
possible_operator( (r) ).
possible_operator( (rem) ).
possible_operator( (skipped) ).
possible_operator( (spy) ).
possible_operator( (table) ).
possible_operator( (then) ).
possible_operator( (thread_local) ).
possible_operator( (ti) ).
possible_operator( (ti_off) ).
possible_operator( (traceable) ).
possible_operator( (unskipped) ).
possible_operator( (untraceable) ).
possible_operator( (use_subsumptive_tabling) ).
possible_operator( (use_variant_tabling) ).
possible_operator( (volatile) ).
possible_operator( (with) ).
possible_operator( (with_input_from_chars) ).
possible_operator( (with_output_to_chars) ).
possible_operator( (xor) ).
