/* xml_acquisition.pl : XML -> Document translation.
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

/* xml_to_document( +Controls, +XML, ?Document ) translates the list of
 * character codes XML into the Prolog term Document. Controls is a list
 * of terms controlling the treatment of layout characters and character
 * entities.
 */
xml_to_document( Controls, XML, Document ) :-
	initial_context( Controls, Context ),
	( xml_declaration( Attributes0, XML, XML1 ) ->
		Attributes = Attributes0
	; otherwise ->
		XML1 = XML,
		Attributes = []
	),
	xml_to_document( XML1, Context, Terms, [], WellFormed ),
	xml_to_document1( WellFormed, Attributes, Terms, Document ).

xml_to_document1( true,  Attributes, Terms, xml(Attributes, Terms) ).
xml_to_document1( false, Attributes, Terms, malformed(Attributes, Terms) ).

% unparsed( +Unparsed, +Context, ?Terms, ?Residue, ?WellFormed )
unparsed( Unparsed, _Context, [unparsed(Unparsed)], [], false ).

xml_declaration( Attributes ) -->
	spaces,
	"<?",
	nmtoken( xml ),
	xml_declaration_attributes( Attributes ),
	spaces,
	"?>".

xml_to_document( [], Context, Terms, [], WF ) :-
	close_context( Context, Terms, WF ).
xml_to_document( [Char|Chars], Context, Terms, Residue, WF ) :-
	( Char =:= "<" ->
		xml_markup_structure( Chars, Context, Terms, Residue, WF )
	; Char =:= "&" ->
		entity_reference( Chars, Context, Terms, Residue, WF )
	; Char =< " ",
	  \+ space_preserve( Context ) ->		
		layouts( Chars, Context, [Char|T], T, Terms, Residue, WF )
	; void_context( Context ) ->
		unparsed( [Char|Chars], Context, Terms, Residue, WF )
	; otherwise ->
		Terms = [pcdata([Char|Chars1])|Terms1],
		acquire_pcdata( Chars, Context, Chars1, Terms1, Residue, WF )
	).

layouts( [], Context, _Plus, _Minus, Terms, [], WF ) :-
	close_context( Context, Terms, WF ).
layouts( [Char|Chars], Context, Plus, Minus, Terms, Residue, WF ) :-
	( Char =:= "<" ->
		xml_markup_structure( Chars, Context, Terms, Residue, WF )
	; Char =:= "&" ->
		reference_in_layout( Chars, Context, Plus, Minus, Terms, Residue, WF )
	; Char =< " " ->
		Minus = [Char|Minus1],
		layouts( Chars, Context, Plus, Minus1, Terms, Residue, WF )
	; void_context( Context ) ->
		unparsed( [Char|Chars], Context, Terms, Residue, WF )
	; otherwise ->
		Terms = [pcdata(Plus)|Terms1],
		Minus = [Char|Chars1],
		context_update( space_preserve, Context, true, Context1 ),
		acquire_pcdata( Chars, Context1, Chars1, Terms1, Residue, WF )
	).

acquire_pcdata( [], Context, [], Terms, [], WF ) :-
	close_context( Context, Terms, WF ).
acquire_pcdata( [Char|Chars], Context, Chars1, Terms, Residue, WF ) :-
	( Char =:= "<" ->
		Chars1 = [],
		xml_markup_structure( Chars, Context, Terms, Residue, WF )
	; Char =:= "&" ->
		reference_in_pcdata( Chars, Context, Chars1, Terms, Residue, WF )
	; otherwise ->
		Chars1 = [Char|Chars2],
		acquire_pcdata( Chars, Context, Chars2, Terms, Residue, WF )
	).

xml_markup_structure( [], Context, Terms, Residue, WF ) :-
	unparsed( "<", Context, Terms, Residue, WF ).
xml_markup_structure( Chars, Context, Terms, Residue, WF ) :-
	Chars = [Char|Chars1],
	( Char =:= "/" ->
		closing_tag( Context, Chars1, Terms, Residue, WF )
	; Char =:= "?" ->
		pi_acquisition( Chars1, Context, Terms, Residue, WF )
	; Char =:= "!" ->
		declaration_acquisition( Chars1, Context, Terms, Residue, WF )
	; open_tag(Tag,Context,Attributes,Type, Chars, Chars2 ) ->
		push_tag( Tag, Chars2, Context, Attributes, Type, Terms, Residue, WF )
	; otherwise ->
		unparsed( [0'<|Chars], Context, Terms, Residue, WF ) %'
	).

push_tag( Tag, Chars, Context, Attributes, Type, Terms, Residue, WF ) :-
	new_element(Tag, Chars, Context, Attributes, Type, Term, Rest, WF0),
	push_tag1( WF0, Context, Term, Rest, Terms, Residue, WF ).

push_tag1( true, Context, Term, Chars, [Term|Terms], Residue, WF ) :-
	xml_to_document( Chars, Context, Terms, Residue, WF ).
push_tag1( false, _Context, Term, Chars, [Term], Chars, false ).

new_element( TagChars, Chars, Context, Attributes0, Type, Term, Residue, WF ) :-
	namespace_attributes( Attributes0, Context, Context1, Attributes1 ),
	( append( NSChars, [0':|TagChars1], TagChars ), %'
	  specific_namespace( NSChars, Context1, SpecificNamespace ) ->
		Namespace0 = SpecificNamespace
	; otherwise ->
		NSChars = "",
		TagChars1 = TagChars,
		default_namespace( Context1, Namespace0 )
	),
	current_namespace( Context1, CurrentNamespace ),
	( Namespace0 == CurrentNamespace ->
		Term = element(Tag, Attributes, Contents),
		Context2 = Context1
	; otherwise ->
		Term = namespace( Namespace0, NSChars,
					element(Tag, Attributes, Contents)
					),
		context_update( current_namespace, Context1, Namespace0, Context2 )
	),
	input_attributes( Attributes1, Context2, Attributes ),
	atom_codes( Tag, TagChars1 ),
	close_tag( Type, Chars, Context2, Contents, Residue, WF ).

close_tag( empty, Residue, _Context, [], Residue, true ).
close_tag( push(Tag), Chars, Context0, Contents, Residue, WF ) :-
	context_update( element, Context0, Tag, Context1 ),
	xml_to_document( Chars, Context1, Contents, Residue, WF ).

pi_acquisition( Chars, Context, Terms, Residue, WellFormed ) :-
	( inline_instruction(Target, Processing, Chars, Rest ),
	  Target \== xml ->
		Terms = [instructions(Target, Processing)|Terms1],
		xml_to_document( Rest, Context, Terms1, Residue, WellFormed )
	; otherwise ->
		unparsed( [0'<,0'?|Chars], Context, Terms, Residue, WellFormed )
	).

declaration_acquisition( Chars, Context, Terms, Residue, WF ) :-
	( declaration_type( Chars, Type, Chars1 ),
	  declaration_parse( Type, Context, Term, Context1, Chars1, Rest ) ->
		Terms = [Term|Terms1],
		xml_to_document( Rest, Context1, Terms1, Residue, WF )
	; otherwise ->
		unparsed( [0'<,0'!|Chars], Context, Terms, Residue, WF )
	).

open_tag( Tag, Namespaces, Attributes, Termination ) -->
	nmtoken_chars( Tag ),
	attributes( Attributes, [], Namespaces ),
	spaces,
	open_tag_terminator( Tag, Termination ).

open_tag_terminator( Tag, push(Tag) ) -->
	">".
open_tag_terminator( _Tag, empty ) -->
	"/>".

declaration_parse( comment, Namespaces, comment(Comment), Namespaces ) -->
	comment(Comment).
declaration_parse( cdata, Namespaces, cdata(CData), Namespaces ) -->
	cdata( CData ).
declaration_parse( doctype, Namespaces0, doctype(Name, Names), Namespaces ) -->
	doctype( Name, Names, Namespaces0, Namespaces ),
	spaces,
	">".

inline_instruction( Target, Processing, Plus, Minus  ) :-
	nmtoken(Target, Plus, Mid0 ),
	spaces( Mid0, Mid1 ),
	append( Processing, [0'?,0'>|Minus], Mid1 ),
	!.

entity_reference_name( Reference ) -->
	nmtoken_chars( Reference ),
	";".

declaration_type( [Char1,Char2|Chars1], Class, Rest ) :-
	Chars = [Char1,Char2|Chars1],
	( declaration_type1( Char1, Char2, Chars1, Class0, Residue ) ->
		Class = Class0,
		Rest = Residue
	; otherwise ->
		Class = generic,
		Rest = Chars
	).

declaration_type1( 0'-, 0'-, Chars, comment, Chars ).
declaration_type1( 0'[, 0'C, Chars, cdata, Residue ) :-
	append( "DATA[", Residue, Chars ).
declaration_type1( 0'D, 0'O, Chars, doctype, Residue ) :-
	append( "CTYPE", Residue, Chars ).

closing_tag( Context, Chars, Terms, Residue, WellFormed ) :-
	( closing_tag_name( Tag, Chars, Rest ),
	  current_tag( Context, Tag ) ->
		Terms = [],
		Residue = Rest,
		WellFormed = true
	; otherwise ->
		unparsed( [0'<,0'/|Chars], Context, Terms, Residue, WellFormed )
	).

closing_tag_name( Tag ) -->
	nmtoken_chars( Tag ),
	spaces,
	">".

entity_reference( Chars, Context, Terms, Residue, WF ) :-
	reference_in_layout( Chars, Context, L, L, Terms, Residue, WF ).

reference_in_layout( Chars, Context, Plus, Minus, Terms, Residue, WF ) :-
	( standard_character_entity( Char, Chars, Rest ) ->
		Minus = [Char|Chars1],
		Terms = [pcdata(Plus)|Terms1],
		acquire_pcdata( Rest, Context, Chars1, Terms1, Residue, WF )
	; entity_reference_name( Reference, Chars, Rest ),
	  defined_entity( Reference, Context, String ) ->
		append( String, Rest, Full ),
		xml_to_document( Full, Context, Terms, Residue, WF )
	; allow_ampersand( Context ) ->
		Minus = [0'&|Chars1], %'
		Terms = [pcdata(Plus)|Terms1],
		acquire_pcdata( Chars, Context, Chars1, Terms1, Residue, WF )
	; otherwise ->
		unparsed( [0'&|Chars], Context, Terms, Residue, WF ) %'
	).

reference_in_pcdata( Chars0, Context, Chars1, Terms, Residue, WF ) :-
	( standard_character_entity( Char, Chars0, Rest ) ->
		Chars1 = [Char|Chars2],
		acquire_pcdata( Rest, Context, Chars2, Terms, Residue, WF )
	; entity_reference_name( Reference, Chars0, Rest ),
	  defined_entity( Reference, Context, String ) ->
		append( String, Rest, Full ),
		acquire_pcdata( Full, Context, Chars1, Terms, Residue, WF )
	; allow_ampersand( Context ) ->
		Chars1 = [0'&|Chars2],
		acquire_pcdata( Chars0, Context, Chars2, Terms, Residue, WF )
	; otherwise ->
		Chars1 = [],
		unparsed( [0'&|Chars0], Context, Terms, Residue, WF )
	).

namespace_attributes( [], Context, Context, [] ).
namespace_attributes( Attributes0, Context0, Context, Attributes ) :-
	Attributes0 = [_|_],
	append( "xmlns:", Unqualified, QualifiedNameChars ),
	( select( "xmlns"=Value, Attributes0, Attributes1 ) ->
		atom_codes( URI, Value ),
		context_update( default_namespace, Context0, URI, Context1 ),
		namespace_attributes( Attributes1, Context1, Context, Attributes )
	; select( QualifiedNameChars=Value, Attributes0, Attributes1 ) ->
		Attributes = [QualifiedNameChars=Value|Attributes2],
		atom_codes( URI, Value ),
		context_update( ns_prefix(Unqualified), Context0, URI, Context1 ),
		namespace_attributes( Attributes1, Context1, Context, Attributes2 )
	; member( "xml:space"="preserve", Attributes0 ) ->
		Attributes = Attributes0,
		context_update( space_preserve, Context0, true, Context )
	; otherwise ->
		Context = Context0,
		Attributes = Attributes0
	).

input_attributes( [], _Context, [] ).
input_attributes( [NameChars=Value|Attributes0], Context,
		[Name=Value|Attributes] ) :-
	( remove_attribute_prefixes( Context ),
	  append( NSChars, [0':|NameChars1], NameChars ), %'
	  NSChars \== "xmlns",
	  specific_namespace( NSChars, Context, Namespace ),
	  current_namespace( Context, Namespace ) ->
		atom_codes( Name, NameChars1 )
	; otherwise ->
		atom_codes( Name, NameChars )
	),
	input_attributes( Attributes0, Context, Attributes ).

attributes( [Name=Value|Attributes], Seen, Namespaces ) -->
	spaces,
	nmtoken_chars( Name ),
	{\+ member(Name, Seen)},
	spaces,
	"=",
	spaces,
	attribute_value( Value, Namespaces ),
	attributes( Attributes, [Name|Seen], Namespaces ).
attributes( [], _Seen, _Namespaces ) --> "".

xml_declaration_attributes( [] ) --> "".
xml_declaration_attributes( [Name=Value|Attributes] ) -->
	spaces,
	nmtoken( Name ),
	spaces,
	"=",
	spaces,
	xml_string( Value ),
	{xml_declaration_attribute_valid(Name, Value)},
	xml_declaration_attributes( Attributes ),
	spaces.

doctype( Name, External, Namespaces0, Namespaces1 ) -->
	spaces,
	nmtoken( Name ),
	spaces,
	doctype_id( External0 ),
	spaces,
	doctype1( Namespaces0, Literals, Namespaces1 ),
	{doctype_extension(Literals, External0, External)}.

doctype_extension( [], External, External ).
doctype_extension( [Literal|Literals], External0, External ) :-
	extended_doctype( External0, [Literal|Literals], External ).

extended_doctype( system(URL), Literals, system(URL,Literals) ).
extended_doctype( public(URN,URL), Literals, public(URN,URL,Literals) ).
extended_doctype( local, Literals, local(Literals) ).

doctype1( Namespaces0, Literals, Namespaces1 ) -->
	"[",
	!,
	dtd( Namespaces0, Literals, Namespaces1 ),
	"]".
doctype1( Namespaces, [], Namespaces ) --> "".

doctype_id( system(URL) ) -->
	"SYSTEM",
	spaces,
	uri( URL ).
doctype_id( public(URN,URL) ) -->
	"PUBLIC",
	spaces,
	uri( URN ),
	spaces,
	uri( URL ).
doctype_id( local ) --> "".

dtd( Namespaces0, Literals, Namespaces1 ) -->
	spaces,
	"<!ENTITY",
	!,
	spaces,
	nmtoken_chars( Name ),
	spaces,
	quote( Quote ),
	entity_value( Quote, Namespaces0, String ),
	spaces,
	">",
	{\+ character_entity( Name, _StandardChar ), 
	 % Don't allow &lt; &quote; etc. to be updated
	 context_update( entity(Name), Namespaces0, String, Namespaces2 )
	 },
	dtd( Namespaces2, Literals, Namespaces1 ).
dtd( Namespaces0, Literals, Namespaces1 ) -->
	spaces,
	"<!--",
	!,
	dtd_comment,
	">",
	dtd( Namespaces0, Literals, Namespaces1 ).
dtd( Namespaces0, [dtd_literal(Literal)|Literals], Namespaces1 ) -->
	spaces,
	"<!",
	!,
	dtd_literal( Literal ),
	dtd( Namespaces0, Literals, Namespaces1 ).
dtd( Namespaces, [], Namespaces ) --> spaces.

dtd_literal( [] ) --> ">", !.
dtd_literal( Chars ) -->
	"--",
	!,
	dtd_comment,
	dtd_literal( Chars ).
dtd_literal( [Char|Chars] ) -->
	[Char],
	dtd_literal( Chars ).

dtd_comment( Plus, Minus ) :-
	append( _Chars, [0'-,0'-|Minus], Plus ),
	!.

nmtokens( [Name|Names] ) -->
	spaces,
	nmtoken( Name ),
	nmtokens( Names ).
nmtokens( [] ) --> [].

entity_value( Quote, Namespaces, String, [Char|Plus], Minus ) :-
	( Char == Quote ->
		String = [],
		Minus = Plus
	; Char =:= "&" ->
		reference_in_entity( Namespaces, Quote, String, Plus, Minus )
	; otherwise ->
		String = [Char|String1],
		entity_value( Quote, Namespaces, String1, Plus, Minus )
	).

attribute_value( String, Namespaces ) -->
	quote( Quote ),
	attribute_leading_layouts( Quote, Namespaces, String ).

attribute_leading_layouts( _Quote, _Namespace, [], [], [] ).
attribute_leading_layouts( Quote, Namespaces, String, [Char|Plus], Minus ) :-
	( Char == Quote ->
		String = [],
		Minus = Plus
	; Char =:= "&" ->
		ref_in_attribute_layout( Namespaces, Quote, String, Plus, Minus )
	; Char > 32, Char \== 160 ->
		String = [Char|String1],
		attribute_layouts( Quote, Namespaces, false, String1, Plus, Minus )
	; otherwise ->
		attribute_leading_layouts( Quote, Namespaces, String, Plus, Minus )
	).

attribute_layouts( _Quote, _Namespaces, _Layout, [], [], [] ).
attribute_layouts( Quote, Namespaces, Layout, String, [Char|Plus], Minus ) :-
	( Char == Quote ->
		String = [],
		Minus = Plus
	; Char =:= "&" ->
		reference_in_value( Namespaces, Quote, Layout, String, Plus, Minus )
	; Char > 32, Char \== 160 ->
		( Layout == true ->
			String = [0' ,Char|String1] %'
		; otherwise ->
			String = [Char|String1]
		),
		attribute_layouts( Quote, Namespaces, false, String1, Plus, Minus )
	; otherwise ->
		attribute_layouts( Quote, Namespaces, true, String, Plus, Minus )
	).

ref_in_attribute_layout( NS, Quote, String, Plus, Minus ) :-
	( standard_character_entity( Char, Plus, Mid ) ->
		String = [Char|String1],
		attribute_layouts( Quote, NS, false,  String1, Mid, Minus )
	; entity_reference_name( Name, Plus, Suffix ),
	  defined_entity( Name, NS, Text ) ->
		append( Text, Suffix, Mid ),
		attribute_leading_layouts( Quote, NS, String, Mid, Minus )
	; otherwise -> % Just & is okay in a value
		String = [0'&|String1], %'
		attribute_layouts( Quote, NS, false, String1, Plus, Minus )
	).

reference_in_value( Namespaces, Quote, Layout, String, Plus, Minus ) :-
	( standard_character_entity( Char, Plus, Mid ) ->
		( Layout == true ->
			String = [0' ,Char|String1] %'
		; otherwise ->
			String = [Char|String1]
		),
		Layout1 = false
	; entity_reference_name( Name, Plus, Suffix ),
	  defined_entity( Name, Namespaces, Text ) ->
		String = String1,
		append( Text, Suffix, Mid ),
		Layout1 = Layout
	; otherwise -> % Just & is okay in a value
		Mid = Plus,
		String = [0'&|String1], %'
		Layout1 = false
	),
	attribute_layouts( Quote, Namespaces, Layout1, String1, Mid, Minus ).

/* References are resolved backwards in Entity defintions so that
 * circularity is avoided.
 */
reference_in_entity( Namespaces, Quote, String, Plus, Minus ) :-
	( standard_character_entity( _SomeChar, Plus, _Rest ) ->
		String = [0'&|String1], % ' Character entities are unparsed
		Mid = Plus
	; entity_reference_name( Name, Plus, Suffix ), 
	  defined_entity( Name, Namespaces, Text ) -> 
		String = String1,
		append( Text, Suffix, Mid )
	),
	entity_value( Quote, Namespaces, String1, Mid, Minus ).

standard_character_entity( Char ) -->
	"#x", hex_character_reference( Char ), ";".
standard_character_entity( Char ) -->
	"#", digit( Digit ), digits( Digits ), ";",
	{number_chars( Char, [Digit|Digits])}.
standard_character_entity( C ) -->
	chars( String ),
	";",
	!,
	{character_entity(String, C)}.

uri( URI ) -->
	quote( Quote ),
	uri1( Quote, URI ).

uri1( Quote, [] ) -->
	quote( Quote ),
	!.
uri1( Quote, [Char|Chars] ) -->
	[Char],
	uri1( Quote, Chars ).

comment( Chars, Plus, Minus ) :-
	append( Chars, [0'-,0'-,0'>|Minus], Plus ), %'
	!.

cdata( Chars, Plus, Minus ) :-
	append( Chars, [0'],0'],0'>|Minus], Plus ), %'
	!.
% Syntax Components

hex_character_reference( Code ) -->
	hex_character_reference1( 0, Code ).

hex_character_reference1( Current, Code ) -->
	hex_digit_char( Value ),
	!,
	{New is (Current << 4) + Value},
	hex_character_reference1( New, Code ).
hex_character_reference1( Code, Code ) --> "".

hex_digit_char( 0 ) --> "0".
hex_digit_char( 1 ) --> "1".
hex_digit_char( 2 ) --> "2".
hex_digit_char( 3 ) --> "3".
hex_digit_char( 4 ) --> "4".
hex_digit_char( 5 ) --> "5".
hex_digit_char( 6 ) --> "6".
hex_digit_char( 7 ) --> "7".
hex_digit_char( 8 ) --> "8".
hex_digit_char( 9 ) --> "9".
hex_digit_char( 10 ) --> "A".
hex_digit_char( 11 ) --> "B".
hex_digit_char( 12 ) --> "C".
hex_digit_char( 13 ) --> "D".
hex_digit_char( 14 ) --> "E".
hex_digit_char( 15 ) --> "F".
hex_digit_char( 10 ) --> "a".
hex_digit_char( 11 ) --> "b".
hex_digit_char( 12 ) --> "c".
hex_digit_char( 13 ) --> "d".
hex_digit_char( 14 ) --> "e".
hex_digit_char( 15 ) --> "f".

quote( 0'" ) --> %'
	"""".
quote( 0'' ) -->
	"'".

spaces( [], [] ).
spaces( [Char|Chars0], Chars1 ) :-
	( Char =< 32 ->
		spaces( Chars0, Chars1 )
	; otherwise ->
		Chars1 = [Char|Chars0]
	).

nmtoken( Name ) -->
	nmtoken_chars( Chars ),
	{atom_codes(Name, Chars)}.

nmtoken_chars( [Char|Chars] ) -->
	[Char],
	{nmtoken_first( Char )},
	nmtoken_chars_tail( Chars ).

nmtoken_chars_tail( [Char|Chars] ) -->
	[Char],
	{nmtoken_char(Char)},
	!,
	nmtoken_chars_tail( Chars ).
nmtoken_chars_tail([]) --> "".

nmtoken_first( 0': ).
nmtoken_first( 0'_ ).
nmtoken_first( Char ) :-
	alphabet( Char ).

nmtoken_char( 0'a ).
nmtoken_char( 0'b ).
nmtoken_char( 0'c ).
nmtoken_char( 0'd ).
nmtoken_char( 0'e ).
nmtoken_char( 0'f ).
nmtoken_char( 0'g ).
nmtoken_char( 0'h ).
nmtoken_char( 0'i ).
nmtoken_char( 0'j ).
nmtoken_char( 0'k ).
nmtoken_char( 0'l ).
nmtoken_char( 0'm ).
nmtoken_char( 0'n ).
nmtoken_char( 0'o ).
nmtoken_char( 0'p ).
nmtoken_char( 0'q ).
nmtoken_char( 0'r ).
nmtoken_char( 0's ).
nmtoken_char( 0't ).
nmtoken_char( 0'u ).
nmtoken_char( 0'v ).
nmtoken_char( 0'w ).
nmtoken_char( 0'x ).
nmtoken_char( 0'y ).
nmtoken_char( 0'z ).
nmtoken_char( 0'A ).
nmtoken_char( 0'B ).
nmtoken_char( 0'C ).
nmtoken_char( 0'D ).
nmtoken_char( 0'E ).
nmtoken_char( 0'F ).
nmtoken_char( 0'G ).
nmtoken_char( 0'H ).
nmtoken_char( 0'I ).
nmtoken_char( 0'J ).
nmtoken_char( 0'K ).
nmtoken_char( 0'L ).
nmtoken_char( 0'M ).
nmtoken_char( 0'N ).
nmtoken_char( 0'O ).
nmtoken_char( 0'P ).
nmtoken_char( 0'Q ).
nmtoken_char( 0'R ).
nmtoken_char( 0'S ).
nmtoken_char( 0'T ).
nmtoken_char( 0'U ).
nmtoken_char( 0'V ).
nmtoken_char( 0'W ).
nmtoken_char( 0'X ).
nmtoken_char( 0'Y ).
nmtoken_char( 0'Z ).
nmtoken_char( 0'0 ).
nmtoken_char( 0'1 ).
nmtoken_char( 0'2 ).
nmtoken_char( 0'3 ).
nmtoken_char( 0'4 ).
nmtoken_char( 0'5 ).
nmtoken_char( 0'6 ).
nmtoken_char( 0'7 ).
nmtoken_char( 0'8 ).
nmtoken_char( 0'9 ).
nmtoken_char( 0'. ).
nmtoken_char( 0'- ).
nmtoken_char( 0'_ ).
nmtoken_char( 0': ).

xml_string( String ) -->
	quote( Quote ),
	xml_string1( Quote, String ).

xml_string1( Quote, [] ) -->
	quote( Quote ),
	!.
xml_string1( Quote, [Char|Chars] ) -->
	[Char],
	xml_string1( Quote, Chars ).

alphabet( 0'a ).
alphabet( 0'b ).
alphabet( 0'c ).
alphabet( 0'd ).
alphabet( 0'e ).
alphabet( 0'f ).
alphabet( 0'g ).
alphabet( 0'h ).
alphabet( 0'i ).
alphabet( 0'j ).
alphabet( 0'k ).
alphabet( 0'l ).
alphabet( 0'm ).
alphabet( 0'n ).
alphabet( 0'o ).
alphabet( 0'p ).
alphabet( 0'q ).
alphabet( 0'r ).
alphabet( 0's ).
alphabet( 0't ).
alphabet( 0'u ).
alphabet( 0'v ).
alphabet( 0'w ).
alphabet( 0'x ).
alphabet( 0'y ).
alphabet( 0'z ).
alphabet( 0'A ).
alphabet( 0'B ).
alphabet( 0'C ).
alphabet( 0'D ).
alphabet( 0'E ).
alphabet( 0'F ).
alphabet( 0'G ).
alphabet( 0'H ).
alphabet( 0'I ).
alphabet( 0'J ).
alphabet( 0'K ).
alphabet( 0'L ).
alphabet( 0'M ).
alphabet( 0'N ).
alphabet( 0'O ).
alphabet( 0'P ).
alphabet( 0'Q ).
alphabet( 0'R ).
alphabet( 0'S ).
alphabet( 0'T ).
alphabet( 0'U ).
alphabet( 0'V ).
alphabet( 0'W ).
alphabet( 0'X ).
alphabet( 0'Y ).
alphabet( 0'Z ).

digit( C ) --> [C], {digit_table( C )}.

digit_table( 0'0 ).
digit_table( 0'1 ).
digit_table( 0'2 ).
digit_table( 0'3 ).
digit_table( 0'4 ).
digit_table( 0'5 ).
digit_table( 0'6 ).
digit_table( 0'7 ).
digit_table( 0'8 ).
digit_table( 0'9 ).

digits( [Digit|Digits] ) -->
	digit( Digit ),
	digits( Digits ).
digits( [] ) --> [].

character_entity( "quot", 0'" ). %'
character_entity( "amp", 0'&  ). %'
character_entity( "lt", 0'< ). %'
character_entity( "gt", 0'> ). %'
character_entity( "apos", 0'' ).

end_of_file.

/* For reference, this is a comprehensive recognizer for namechar, based on
 * the definition of in http://www.w3.org/TR/2000/REC-xml-20001006 .
 */
namechar -->
	( letter
	| unicode_digit
	|  "."
	|  "-"
	|  "_"
	|  ":"
	|  combiningchar
	|  extender
	).

letter  --> (basechar | ideographic).

basechar  --> 
	( range( 16'0041, 16'005A )
	| range( 16'0061, 16'007A )
	| range( 16'00C0, 16'00D6 )
	| range( 16'00D8, 16'00F6 )
	| range( 16'00F8, 16'00FF )
	| range( 16'0100, 16'0131 )
	| range( 16'0134, 16'013E )
	| range( 16'0141, 16'0148 )
	| range( 16'014A, 16'017E )
	| range( 16'0180, 16'01C3 )
	| range( 16'01CD, 16'01F0 )
	| range( 16'01F4, 16'01F5 )
	| range( 16'01FA, 16'0217 )
	| range( 16'0250, 16'02A8 )
	| range( 16'02BB, 16'02C1 )
	| [16'0386]
	| range( 16'0388, 16'038A )
	| [16'038C]
	| range( 16'038E, 16'03A1 )
	| range( 16'03A3, 16'03CE )
	| range( 16'03D0, 16'03D6 )
	| [16'03DA]
	| [16'03DC]
	| [16'03DE]
	| [16'03E0]
	| range( 16'03E2, 16'03F3 )
	| range( 16'0401, 16'040C )
	| range( 16'040E, 16'044F )
	| range( 16'0451, 16'045C )
	| range( 16'045E, 16'0481 )
	| range( 16'0490, 16'04C4 )
	| range( 16'04C7, 16'04C8 )
	| range( 16'04CB, 16'04CC )
	| range( 16'04D0, 16'04EB )
	| range( 16'04EE, 16'04F5 )
	| range( 16'04F8, 16'04F9 )
	| range( 16'0531, 16'0556 )
	| [16'0559]
	| range( 16'0561, 16'0586 )
	| range( 16'05D0, 16'05EA )
	| range( 16'05F0, 16'05F2 )
	| range( 16'0621, 16'063A )
	| range( 16'0641, 16'064A )
	| range( 16'0671, 16'06B7 )
	| range( 16'06BA, 16'06BE )
	| range( 16'06C0, 16'06CE )
	| range( 16'06D0, 16'06D3 )
	| [16'06D5]
	| range( 16'06E5, 16'06E6 )
	| range( 16'0905, 16'0939 )
	| [16'093D]
	| range( 16'0958, 16'0961 )
	| range( 16'0985, 16'098C )
	| range( 16'098F, 16'0990 )
	| range( 16'0993, 16'09A8 )
	| range( 16'09AA, 16'09B0 )
	| [16'09B2]
	| range( 16'09B6, 16'09B9 )
	| range( 16'09DC, 16'09DD )
	| range( 16'09DF, 16'09E1 )
	| range( 16'09F0, 16'09F1 )
	| range( 16'0A05, 16'0A0A )
	| range( 16'0A0F, 16'0A10 )
	| range( 16'0A13, 16'0A28 )
	| range( 16'0A2A, 16'0A30 )
	| range( 16'0A32, 16'0A33 )
	| range( 16'0A35, 16'0A36 )
	| range( 16'0A38, 16'0A39 )
	| range( 16'0A59, 16'0A5C )
	| [16'0A5E]
	| range( 16'0A72, 16'0A74 )
	| range( 16'0A85, 16'0A8B )
	| [16'0A8D]
	| range( 16'0A8F, 16'0A91 )
	| range( 16'0A93, 16'0AA8 )
	| range( 16'0AAA, 16'0AB0 )
	| range( 16'0AB2, 16'0AB3 )
	| range( 16'0AB5, 16'0AB9 )
	| [16'0ABD]
	| [16'0AE0]
	| range( 16'0B05, 16'0B0C )
	| range( 16'0B0F, 16'0B10 )
	| range( 16'0B13, 16'0B28 )
	| range( 16'0B2A, 16'0B30 )
	| range( 16'0B32, 16'0B33 )
	| range( 16'0B36, 16'0B39 )
	| [16'0B3D]
	| range( 16'0B5C, 16'0B5D )
	| range( 16'0B5F, 16'0B61 )
	| range( 16'0B85, 16'0B8A )
	| range( 16'0B8E, 16'0B90 )
	| range( 16'0B92, 16'0B95 )
	| range( 16'0B99, 16'0B9A )
	| [16'0B9C]
	| range( 16'0B9E, 16'0B9F )
	| range( 16'0BA3, 16'0BA4 )
	| range( 16'0BA8, 16'0BAA )
	| range( 16'0BAE, 16'0BB5 )
	| range( 16'0BB7, 16'0BB9 )
	| range( 16'0C05, 16'0C0C )
	| range( 16'0C0E, 16'0C10 )
	| range( 16'0C12, 16'0C28 )
	| range( 16'0C2A, 16'0C33 )
	| range( 16'0C35, 16'0C39 )
	| range( 16'0C60, 16'0C61 )
	| range( 16'0C85, 16'0C8C )
	| range( 16'0C8E, 16'0C90 )
	| range( 16'0C92, 16'0CA8 )
	| range( 16'0CAA, 16'0CB3 )
	| range( 16'0CB5, 16'0CB9 )
	| [16'0CDE]
	| range( 16'0CE0, 16'0CE1 )
	| range( 16'0D05, 16'0D0C )
	| range( 16'0D0E, 16'0D10 )
	| range( 16'0D12, 16'0D28 )
	| range( 16'0D2A, 16'0D39 )
	| range( 16'0D60, 16'0D61 )
	| range( 16'0E01, 16'0E2E )
	| [16'0E30]
	| range( 16'0E32, 16'0E33 )
	| range( 16'0E40, 16'0E45 )
	| range( 16'0E81, 16'0E82 )
	| [16'0E84]
	| range( 16'0E87, 16'0E88 )
	| [16'0E8A]
	| [16'0E8D]
	| range( 16'0E94, 16'0E97 )
	| range( 16'0E99, 16'0E9F )
	| range( 16'0EA1, 16'0EA3 )
	| [16'0EA5]
	| [16'0EA7]
	| range( 16'0EAA, 16'0EAB )
	| range( 16'0EAD, 16'0EAE )
	| [16'0EB0]
	| range( 16'0EB2, 16'0EB3 )
	| [16'0EBD]
	| range( 16'0EC0, 16'0EC4 )
	| range( 16'0F40, 16'0F47 )
	| range( 16'0F49, 16'0F69 )
	| range( 16'10A0, 16'10C5 )
	| range( 16'10D0, 16'10F6 )
	| [16'1100]
	| range( 16'1102, 16'1103 )
	| range( 16'1105, 16'1107 )
	| [16'1109]
	| range( 16'110B, 16'110C )
	| range( 16'110E, 16'1112 )
	| [16'113C]
	| [16'113E]
	| [16'1140]
	| [16'114C]
	| [16'114E]
	| [16'1150]
	| range( 16'1154, 16'1155 )
	| [16'1159]
	| range( 16'115F, 16'1161 )
	| [16'1163]
	| [16'1165]
	| [16'1167]
	| [16'1169]
	| range( 16'116D, 16'116E )
	| range( 16'1172, 16'1173 )
	| [16'1175]
	| [16'119E]
	| [16'11A8]
	| [16'11AB]
	| range( 16'11AE, 16'11AF )
	| range( 16'11B7, 16'11B8 )
	| [16'11BA]
	| range( 16'11BC, 16'11C2 )
	| [16'11EB]
	| [16'11F0]
	| [16'11F9]
	| range( 16'1E00, 16'1E9B )
	| range( 16'1EA0, 16'1EF9 )
	| range( 16'1F00, 16'1F15 )
	| range( 16'1F18, 16'1F1D )
	| range( 16'1F20, 16'1F45 )
	| range( 16'1F48, 16'1F4D )
	| range( 16'1F50, 16'1F57 )
	| [16'1F59]
	| [16'1F5B]
	| [16'1F5D]
	| range( 16'1F5F, 16'1F7D )
	| range( 16'1F80, 16'1FB4 )
	| range( 16'1FB6, 16'1FBC )
	| [16'1FBE]
	| range( 16'1FC2, 16'1FC4 )
	| range( 16'1FC6, 16'1FCC )
	| range( 16'1FD0, 16'1FD3 )
	| range( 16'1FD6, 16'1FDB )
	| range( 16'1FE0, 16'1FEC )
	| range( 16'1FF2, 16'1FF4 )
	| range( 16'1FF6, 16'1FFC )
	| [16'2126]
	| range( 16'212A, 16'212B )
	| [16'212E]
	| range( 16'2180, 16'2182 )
	| range( 16'3041, 16'3094 )
	| range( 16'30A1, 16'30FA )
	| range( 16'3105, 16'312C )
	| range( 16'AC00, 16'D7A3 )
	).
ideographic  -->
	( range( 16'4E00, 16'9FA5 )
	| [16'3007]
	| range( 16'3021, 16'3029 )
	).
combiningchar  -->
	( range( 16'0300, 16'0345 )
	| range( 16'0360, 16'0361 )
	| range( 16'0483, 16'0486 )
	| range( 16'0591, 16'05A1 )
	| range( 16'05A3, 16'05B9 )
	| range( 16'05BB, 16'05BD )
	| [16'05BF]
	| range( 16'05C1, 16'05C2 )
	| [16'05C4]
	| range( 16'064B, 16'0652 )
	| [16'0670]
	| range( 16'06D6, 16'06DC )
	| range( 16'06DD, 16'06DF )
	| range( 16'06E0, 16'06E4 )
	| range( 16'06E7, 16'06E8 )
	| range( 16'06EA, 16'06ED )
	| range( 16'0901, 16'0903 )
	| [16'093C]
	| range( 16'093E, 16'094C )
	| [16'094D]
	| range( 16'0951, 16'0954 )
	| range( 16'0962, 16'0963 )
	| range( 16'0981, 16'0983 )
	| [16'09BC]
	| [16'09BE]
	| [16'09BF]
	| range( 16'09C0, 16'09C4 )
	| range( 16'09C7, 16'09C8 )
	| range( 16'09CB, 16'09CD )
	| [16'09D7]
	| range( 16'09E2, 16'09E3 )
	| [16'0A02]
	| [16'0A3C]
	| [16'0A3E]
	| [16'0A3F]
	| range( 16'0A40, 16'0A42 )
	| range( 16'0A47, 16'0A48 )
	| range( 16'0A4B, 16'0A4D )
	| range( 16'0A70, 16'0A71 )
	| range( 16'0A81, 16'0A83 )
	| [16'0ABC]
	| range( 16'0ABE, 16'0AC5 )
	| range( 16'0AC7, 16'0AC9 )
	| range( 16'0ACB, 16'0ACD )
	| range( 16'0B01, 16'0B03 )
	| [16'0B3C]
	| range( 16'0B3E, 16'0B43 )
	| range( 16'0B47, 16'0B48 )
	| range( 16'0B4B, 16'0B4D )
	| range( 16'0B56, 16'0B57 )
	| range( 16'0B82, 16'0B83 )
	| range( 16'0BBE, 16'0BC2 )
	| range( 16'0BC6, 16'0BC8 )
	| range( 16'0BCA, 16'0BCD )
	| [16'0BD7]
	| range( 16'0C01, 16'0C03 )
	| range( 16'0C3E, 16'0C44 )
	| range( 16'0C46, 16'0C48 )
	| range( 16'0C4A, 16'0C4D )
	| range( 16'0C55, 16'0C56 )
	| range( 16'0C82, 16'0C83 )
	| range( 16'0CBE, 16'0CC4 )
	| range( 16'0CC6, 16'0CC8 )
	| range( 16'0CCA, 16'0CCD )
	| range( 16'0CD5, 16'0CD6 )
	| range( 16'0D02, 16'0D03 )
	| range( 16'0D3E, 16'0D43 )
	| range( 16'0D46, 16'0D48 )
	| range( 16'0D4A, 16'0D4D )
	| [16'0D57]
	| [16'0E31]
	| range( 16'0E34, 16'0E3A )
	| range( 16'0E47, 16'0E4E )
	| [16'0EB1]
	| range( 16'0EB4, 16'0EB9 )
	| range( 16'0EBB, 16'0EBC )
	| range( 16'0EC8, 16'0ECD )
	| range( 16'0F18, 16'0F19 )
	| [16'0F35]
	| [16'0F37]
	| [16'0F39]
	| [16'0F3E]
	| [16'0F3F]
	| range( 16'0F71, 16'0F84 )
	| range( 16'0F86, 16'0F8B )
	| range( 16'0F90, 16'0F95 )
	| [16'0F97]
	| range( 16'0F99, 16'0FAD )
	| range( 16'0FB1, 16'0FB7 )
	| [16'0FB9]
	| range( 16'20D0, 16'20DC )
	| [16'20E1]
	| range( 16'302A, 16'302F )
	| [16'3099]
	| [16'309A]
	).

unicode_digit  -->
	( range( 16'0030, 16'0039 )
	| range( 16'0660, 16'0669 )
	| range( 16'06F0, 16'06F9 )
	| range( 16'0966, 16'096F )
	| range( 16'09E6, 16'09EF )
	| range( 16'0A66, 16'0A6F )
	| range( 16'0AE6, 16'0AEF )
	| range( 16'0B66, 16'0B6F )
	| range( 16'0BE7, 16'0BEF )
	| range( 16'0C66, 16'0C6F )
	| range( 16'0CE6, 16'0CEF )
	| range( 16'0D66, 16'0D6F )
	| range( 16'0E50, 16'0E59 )
	| range( 16'0ED0, 16'0ED9 )
	| range( 16'0F20, 16'0F29 )
	).

extender  -->
	( [16'00B7]
	| [16'02D0]
	| [16'02D1]
	| [16'0387]
	| [16'0640]
	| [16'0E46]
	| [16'0EC6]
	| [16'3005]
	| range( 16'3031, 16'3035 )
	| range( 16'309D, 16'309E )
	| range( 16'30FC, 16'30FE )
	).

range( Low, High ) -->
	[Char],
	{Char >= Low, Char =< High}.
