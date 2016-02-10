/* xml_generation.pl : Document -> XML translation
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

/* document_generation( +Format, +Document ) is a DCG generating Document
 * as a list of character codes. Format is true|false defining whether layouts,
 * to provide indentation, should be added between the element content of
 * the resultant "string". Note that formatting is disabled for elements that
 * are interspersed with pcdata/1 terms, such as XHTML's 'inline' elements.
 * Also, Format is over-ridden, for an individual element, by an explicit
 * 'xml:space'="preserve" attribute.
 */
document_generation( Format, xml(Attributes, Document) ) -->
	document_generation_body( Attributes, Format, Document ).

document_generation_body( [], Format, Document ) -->
	generation( Document, "", Format, [], _Format1 ).
document_generation_body( Attributes, Format, Document ) -->
	{	Attributes = [_|_],
		xml_declaration_attributes_valid( Attributes )
	},
	"<?xml",
	generated_attributes( Attributes, Format, Format0 ),
	"?>",
	indent( true, [] ),
	generation( Document, "", Format0, [], _Format1 ).

generation( [], _Prefix, Format, _Indent, Format ) --> [].
generation( [Term|Terms], Prefix, Format0, Indent, Format ) -->
	generation( Term, Prefix, Format0, Indent, Format1 ),
	generation( Terms, Prefix, Format1, Indent, Format ).
generation( doctype(Name, External), _Prefix, Format, [], Format ) -->
	"<!DOCTYPE ",
	generated_name( Name ),
	generated_external_id( External ),
	">".
generation( instructions(Target,Process), _Prefix, Format, Indent, Format ) -->
	indent( Format, Indent ),
	"<?", generated_name(Target), " ", chars( Process ) ,"?>".
generation( pcdata(Chars), _Prefix, Format0, _Indent, Format1 ) -->
	pcdata_generation( Chars ),
	{pcdata_format( Chars, Format0, Format1 )}.
generation( comment( Comment ), _Prefix, Format, Indent, Format ) -->
	indent( Format, Indent ),
	"<!--", chars( Comment ), "-->".
generation( namespace(URI, Prefix, element(Name, Atts, Content)),
		_Prefix0, Format, Indent, Format ) -->
	indent( Format, Indent ),
	"<", generated_prefixed_name( Prefix, Name ),
	generated_prefixed_attributes( Prefix, URI, Atts, Format, Format1 ), 
	generated_content( Content, Format1, Indent, Prefix, Name ).
generation( element(Name, Atts, Content), Prefix, Format, Indent, Format ) -->
	indent( Format, Indent ),
	"<", generated_prefixed_name( Prefix, Name ),
	generated_attributes( Atts, Format, Format1 ), 
	generated_content( Content, Format1, Indent, Prefix, Name ).
generation( cdata(CData), _Prefix, Format, Indent, Format ) -->
	indent( Format, Indent ),
	"<![CDATA[", cdata_generation(CData), "]]>".

generated_attributes( [], Format, Format  ) --> [].
generated_attributes( [Name=Value|Attributes], Format0, Format  ) -->
	{(	Name == 'xml:space',
		Value="preserve" ->
			Format1 = false
	  ; otherwise ->
			Format1 = Format0
	  )},
	" ",
	generated_name( Name ),
	"=""",
	quoted_string( Value ),
	"""",
	generated_attributes( Attributes, Format1, Format  ).

generated_prefixed_name( [], Name ) -->
	generated_name( Name ).
generated_prefixed_name( Prefix, Name ) -->
	{Prefix = [_|_]},
	chars( Prefix ), ":",
	generated_name( Name ).

generated_content( [], _Format, _Indent, _Prefix, _Namespace ) -->
	" />". % Leave an extra space for XHTML output.
generated_content( [H|T], Format, Indent, Prefix, Namespace ) -->
	">",
	generation( H, Prefix, Format, [0' |Indent], Format1 ),
	generation( T, Prefix, Format1, [0' |Indent], Format2 ),
	indent( Format2, Indent ),
	"</", generated_prefixed_name( Prefix, Namespace ), ">".

generated_prefixed_attributes( [_|_Prefix], _URI, Atts, Format0, Format ) -->
	generated_attributes( Atts, Format0, Format  ).
generated_prefixed_attributes( [], URI, Atts, Format0, Format  ) -->
	{atom_codes( URI, Namespace ),
	 findall( Attr, (member(Attr, Atts), \+ Attr=(xmlns=_Val)), Atts1 )
	},
	generated_attributes( [xmlns=Namespace|Atts1], Format0, Format  ).

generated_name( Name, Plus, Minus ) :-
	atom_codes( Name, Chars ),
	append( Chars, Minus, Plus ).

generated_external_id( local ) --> "".
generated_external_id( local(Literals) ) --> " [",
	generated_doctype_literals( Literals ), "
	]".
generated_external_id( system(URL) ) -->
	" SYSTEM """,
	chars( URL ),
	"""".
generated_external_id( system(URL,Literals) ) -->
	" SYSTEM """,
	chars( URL ),
	""" [",
	generated_doctype_literals( Literals ), "
	]".
generated_external_id( public(URN,URL) ) -->
	" PUBLIC """,
	chars( URN ),
	""" """,
	chars( URL ),
	"""".
generated_external_id( public(URN,URL,Literals) ) -->
	" PUBLIC """,
	chars( URN ),
	""" """,
	chars( URL ),
	""" [",
	generated_doctype_literals( Literals ), "
	]".

generated_doctype_literals( [] ) --> "".
generated_doctype_literals( [dtd_literal(String)|Literals] ) --> "
	<!", cdata_generation( String ), ">",
	generated_doctype_literals( Literals ).

/* quoted_string( +Chars ) is a DCG representing Chars, a list of character
 * codes, as a legal XML attribute string. Any leading or trailing layout
 * characters are removed. &, " and < characters are replaced by &amp;, &quot;
 * and &lt; respectively, .
 */
quoted_string( Raw, Plus, Minus ) :-
	quoted_string1( Raw, NoLeadingLayouts ),
	quoted_string2( NoLeadingLayouts, Layout, Layout, Plus, Minus ).

quoted_string1( [], [] ).
quoted_string1( [Char|Chars], NoLeadingLayouts ) :-
	( Char > 32 ->
		NoLeadingLayouts = [Char|Chars]
	; otherwise ->
		quoted_string1( Chars, NoLeadingLayouts )
	).

quoted_string2( [], _LayoutPlus, _LayoutMinus, List, List ).
quoted_string2( [Char|Chars], LayoutPlus, LayoutMinus, Plus, Minus ) :-
	( Char =< " " ->
		Plus = Plus1,
		LayoutMinus = [Char|LayoutMinus1],
		LayoutPlus = LayoutPlus1
	; Char =< 127 ->
		Plus = LayoutPlus,
		pcdata_7bit( Char, LayoutMinus, Plus1 ),
		LayoutPlus1 = LayoutMinus1
	; legal_xml_unicode( Char ) ->
		Plus = LayoutPlus,
		number_codes( Char, Codes ),
		pcdata_8bits_plus( Codes, LayoutMinus, Plus1 ),
		LayoutPlus1 = LayoutMinus1
	; otherwise ->
		LayoutPlus = LayoutPlus1,
		LayoutMinus = LayoutMinus1,
		Plus = Plus1
	),
	quoted_string2( Chars, LayoutPlus1, LayoutMinus1, Plus1, Minus ).

indent( false, _Indent ) --> [].
indent( true, Indent ) -->
	"
",	chars( Indent ).

/* pcdata_generation( +Chars ) is a DCG representing Chars, a list of character
 * codes as legal XML "Parsed character data" (PCDATA) string. Any codes
 * which cannot be represented by a 7-bit character are replaced by their
 * decimal numeric character entity e.g. code 160 (non-breaking space) is
 * represented as &#160;. Any character codes disallowed by the XML
 * specification are not encoded.
 */
pcdata_generation( [], Plus, Plus ).
pcdata_generation( [Char|Chars], Plus, Minus ) :-
	( Char =< 127 ->
		pcdata_7bit( Char, Plus, Mid )
	; legal_xml_unicode( Char ) ->
		number_codes( Char, Codes ),
		pcdata_8bits_plus( Codes, Plus, Mid )
	; otherwise ->
		Plus = Mid
	),
	pcdata_generation( Chars, Mid, Minus ).

/* pcdata_7bit(+Char) represents the ascii character set in its
 * simplest format, using the character entities &amp; &quot; &lt; and &gt;
 * which are common to both XML and HTML. The numeric entity &#39; is used in
 * place of &apos;, because browsers don't recognize it in HTML.
 */
pcdata_7bit( 0 ) --> "".
pcdata_7bit( 1 ) --> "".
pcdata_7bit( 2 ) --> "".
pcdata_7bit( 3 ) --> "".
pcdata_7bit( 4 ) --> "".
pcdata_7bit( 5 ) --> "".
pcdata_7bit( 6 ) --> "".
pcdata_7bit( 7 ) --> "".
pcdata_7bit( 8 ) --> "".
pcdata_7bit( 9 ) --> [9].
pcdata_7bit( 10 ) --> [10].
pcdata_7bit( 11 ) --> "".
pcdata_7bit( 12 ) --> "".
pcdata_7bit( 13 ) --> [13].
pcdata_7bit( 14 ) --> "".
pcdata_7bit( 15 ) --> "".
pcdata_7bit( 16 ) --> "".
pcdata_7bit( 17 ) --> "".
pcdata_7bit( 18 ) --> "".
pcdata_7bit( 19 ) --> "".
pcdata_7bit( 20 ) --> "".
pcdata_7bit( 21 ) --> "".
pcdata_7bit( 22 ) --> "".
pcdata_7bit( 23 ) --> "".
pcdata_7bit( 24 ) --> "".
pcdata_7bit( 25 ) --> "".
pcdata_7bit( 26 ) --> "".
pcdata_7bit( 27 ) --> "".
pcdata_7bit( 28 ) --> "".
pcdata_7bit( 29 ) --> "".
pcdata_7bit( 30 ) --> "".
pcdata_7bit( 31 ) --> "".
pcdata_7bit( 32 ) --> " ".
pcdata_7bit( 33 ) --> "!".
pcdata_7bit( 34 ) --> "&quot;".
pcdata_7bit( 35 ) --> "#".
pcdata_7bit( 36 ) --> "$".
pcdata_7bit( 37 ) --> "%".
pcdata_7bit( 38 ) --> "&amp;".
pcdata_7bit( 39 ) --> "&#39;".
pcdata_7bit( 40 ) --> "(".
pcdata_7bit( 41 ) --> ")".
pcdata_7bit( 42 ) --> "*".
pcdata_7bit( 43 ) --> "+".
pcdata_7bit( 44 ) --> ",".
pcdata_7bit( 45 ) --> "-".
pcdata_7bit( 46 ) --> ".".
pcdata_7bit( 47 ) --> "/".
pcdata_7bit( 48 ) --> "0".
pcdata_7bit( 49 ) --> "1".
pcdata_7bit( 50 ) --> "2".
pcdata_7bit( 51 ) --> "3".
pcdata_7bit( 52 ) --> "4".
pcdata_7bit( 53 ) --> "5".
pcdata_7bit( 54 ) --> "6".
pcdata_7bit( 55 ) --> "7".
pcdata_7bit( 56 ) --> "8".
pcdata_7bit( 57 ) --> "9".
pcdata_7bit( 58 ) --> ":".
pcdata_7bit( 59 ) --> ";".
pcdata_7bit( 60 ) --> "&lt;".
pcdata_7bit( 61 ) --> "=".
pcdata_7bit( 62 ) --> "&gt;".
pcdata_7bit( 63 ) --> "?".
pcdata_7bit( 64 ) --> "@".
pcdata_7bit( 65 ) --> "A".
pcdata_7bit( 66 ) --> "B".
pcdata_7bit( 67 ) --> "C".
pcdata_7bit( 68 ) --> "D".
pcdata_7bit( 69 ) --> "E".
pcdata_7bit( 70 ) --> "F".
pcdata_7bit( 71 ) --> "G".
pcdata_7bit( 72 ) --> "H".
pcdata_7bit( 73 ) --> "I".
pcdata_7bit( 74 ) --> "J".
pcdata_7bit( 75 ) --> "K".
pcdata_7bit( 76 ) --> "L".
pcdata_7bit( 77 ) --> "M".
pcdata_7bit( 78 ) --> "N".
pcdata_7bit( 79 ) --> "O".
pcdata_7bit( 80 ) --> "P".
pcdata_7bit( 81 ) --> "Q".
pcdata_7bit( 82 ) --> "R".
pcdata_7bit( 83 ) --> "S".
pcdata_7bit( 84 ) --> "T".
pcdata_7bit( 85 ) --> "U".
pcdata_7bit( 86 ) --> "V".
pcdata_7bit( 87 ) --> "W".
pcdata_7bit( 88 ) --> "X".
pcdata_7bit( 89 ) --> "Y".
pcdata_7bit( 90 ) --> "Z".
pcdata_7bit( 91 ) --> "[".
pcdata_7bit( 92 ) --> [92].
pcdata_7bit( 93 ) --> "]".
pcdata_7bit( 94 ) --> "^".
pcdata_7bit( 95 ) --> "_".
pcdata_7bit( 96 ) --> "&#96;".
pcdata_7bit( 97 ) --> "a".
pcdata_7bit( 98 ) --> "b".
pcdata_7bit( 99 ) --> "c".
pcdata_7bit( 100 ) --> "d".
pcdata_7bit( 101 ) --> "e".
pcdata_7bit( 102 ) --> "f".
pcdata_7bit( 103 ) --> "g".
pcdata_7bit( 104 ) --> "h".
pcdata_7bit( 105 ) --> "i".
pcdata_7bit( 106 ) --> "j".
pcdata_7bit( 107 ) --> "k".
pcdata_7bit( 108 ) --> "l".
pcdata_7bit( 109 ) --> "m".
pcdata_7bit( 110 ) --> "n".
pcdata_7bit( 111 ) --> "o".
pcdata_7bit( 112 ) --> "p".
pcdata_7bit( 113 ) --> "q".
pcdata_7bit( 114 ) --> "r".
pcdata_7bit( 115 ) --> "s".
pcdata_7bit( 116 ) --> "t".
pcdata_7bit( 117 ) --> "u".
pcdata_7bit( 118 ) --> "v".
pcdata_7bit( 119 ) --> "w".
pcdata_7bit( 120 ) --> "x".
pcdata_7bit( 121 ) --> "y".
pcdata_7bit( 122 ) --> "z".
pcdata_7bit( 123 ) --> "{".
pcdata_7bit( 124 ) --> "|".
pcdata_7bit( 125 ) --> "}".
pcdata_7bit( 126 ) --> "~".
pcdata_7bit( 127 ) --> "&#127;".

pcdata_8bits_plus( Codes ) -->
	"&#", chars( Codes ), ";".

/* pcdata_format( +Chars, +Format0, ?Format1 ) holds when Format0 and Format1
 * are the statuses of XML formatting before and after Chars - which may be
 * null.
 */
pcdata_format( [], Format, Format ).
pcdata_format( [_Char|_Chars], _Format, false ).

/* cdata_generation( +Chars ) is a DCG representing Chars, a list of character
 * codes as a legal XML CDATA string. Any character codes disallowed by the XML
 * specification are not encoded.
 */
cdata_generation( [] ) --> "".
cdata_generation( [Char|Chars] ) -->
	( {legal_xml_unicode( Char )}, !, [Char]
	| ""
	),
	cdata_generation( Chars ).

legal_xml_unicode( 9 ).
legal_xml_unicode( 10 ).
legal_xml_unicode( 13 ).
legal_xml_unicode( Code ) :-
	Code >= 32,
	Code =< 55295.
legal_xml_unicode( Code ) :-
	Code >= 57344,
	Code =< 65533.
legal_xml_unicode( Code ) :-
	Code >= 65536,
	Code =< 1114111.
