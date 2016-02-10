/* xml_diagnosis.pl : XML exception diagnosis.
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

:- ensure_loaded( xml_generation ).

/* xml_fault( +Term, +Indentation, ?SubTerm, ?Path, ?Message ) identifies SubTerm
 * as a sub-term of Term which cannot be serialized after Indentation.
 * Message is an atom naming the type of error; Path is a string encoding a
 * list of SubTerm's ancestor elements in the form <tag>{(id)}* where <tag> is the
 * element tag and <id> is the value of any attribute _named_ id.
 */
xml_fault( Term, _Indent, Term, [], "Illegal Variable" ) :-
	var( Term ).
xml_fault( xml(Attributes,_Content), _Indent, Term, [], Message ) :-
	member( Attribute, Attributes ),
	attribute_fault( Attribute, Term, Message ).
xml_fault( xml(_Attributes,Content), Indent, Culprit, Path, Message ) :-
	xml_content_fault( Content, Indent, Culprit, Path, Message ).
xml_fault( Term, _Indent, Term, [], "Illegal Term" ).

xml_content_fault( Term, _Indent, Term, [], "Illegal Variable" ) :-
	var( Term ).
xml_content_fault( pcdata(Chars), _Indent, Chars, [], "Invalid Character Data" ) :-
	\+ is_chars( Chars ).
xml_content_fault( cdata(Chars), _Indent, Chars, [], "Invalid Character Data" ) :-
	\+ is_chars( Chars ).
xml_content_fault( [H|_T], Indent, Culprit, Path, Message ) :-
	xml_content_fault( H, Indent, Culprit, Path, Message ).
xml_content_fault( [_H|T], Indent, Culprit, Path, Message ) :-
	xml_content_fault( T, Indent, Culprit, Path, Message ).
xml_content_fault( namespace(_URI,_Prefix,Element), Indent, Culprit, Path, Message ) :-
	element_fault( Element, [0' |Indent], Culprit, Path, Message ).
xml_content_fault( Element, Indent, Culprit, Path, Message ) :-
	element_fault( Element, [0' |Indent], Culprit, Path, Message ).
xml_content_fault( Term, Indent, Term, [], "Illegal Term" ) :-
	\+ generation(Term, "", false, Indent, _Format, _Plus, _Minus ).

element_fault( element(Tag, _Attributes, _Contents), _Indent, Tag, [], "Tag must be an atom" ) :-
	\+ atom( Tag ).
element_fault( element(Tag, Attributes, _Contents), _Indent, Tag, [], "Attributes must be instantiated" ) :-
	var( Attributes ).
element_fault( element(Tag, Attributes, _Contents), _Indent, Faulty, Path, Message ) :-
	fault_path( Tag, Attributes, Path, [] ),
	member( Attribute, Attributes ),
	attribute_fault( Attribute, Faulty, Message ).
element_fault( element(Tag, Attributes, Contents), Indent, Culprit, Path, Message ) :-
	fault_path( Tag, Attributes, Path, Path1 ),
	xml_content_fault( Contents, Indent, Culprit, Path1, Message ).

attribute_fault( Attribute, Attribute, "Illegal Variable" ) :-
	var( Attribute ).
attribute_fault( Name=Value, Name=Value, "Attribute Name must be atom" ) :-
	\+ atom(Name).
attribute_fault( Name=Value, Name=Value, "Attribute Value must be chars" ) :-
	\+ is_chars( Value ).
attribute_fault( Attribute, Attribute, "Malformed Attribute" ) :-
	\+ Attribute = (_Name=_Value).

is_chars( Chars ) :-
	is_list( Chars ),
	\+ (member( Char, Chars ), \+ (integer(Char), Char >=0, Char =< 255)).

fault_path( Tag, Attributes ) -->
	{atom_codes( Tag, Chars )},
	chars( Chars ),
	fault_id( Attributes ),
	" ".

fault_id( Attributes ) -->
	{member( id=Chars, Attributes ), is_chars( Chars )},
	!,
	"(", chars(Chars), ")".
fault_id( _Attributes ) --> "".
