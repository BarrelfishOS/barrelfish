/* xml.pl : Contains xml_parse/[2,3] a bi-directional XML parser written in
 * Prolog.
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
 *
 */
:- module( xml,
	[
	xml_parse/2,
	xml_parse/3,
	xml_subterm/2,
	xml_pp/1
	] ).

/* xml_parse( {+Controls}, +?Chars, ?+Document ) parses Chars to/from a data
 * structure of the form xml(<atts>, <content>). <atts> is a list of
 * <atom>=<string> attributes from the (possibly implicit) XML signature of the
 * document. <content> is a (possibly empty) list comprising occurrences of :
 *
 *    pcdata(<string>)                    :  Text
 *    comment(<string>)                   :  An xml comment;
 *    element(<tag>,<atts>,<content>)     :  <tag>..</tag> encloses <content>
 *                                        :  <tag /> if empty
 *    instructions(<atom>, <string>)      :  Processing <? <atom> <params> ?>"
 *     cdata( <string> )                  :  <![CDATA[ <string> ]]>
 *    doctype(<atom>, <doctype id>)       :  DTD <!DOCTYPE .. >
 *
 * The conversions are not completely symmetrical, in that weaker XML is
 * accepted than can be generated. Specifically, in-bound (Chars -> Document)
 * does not  require strictly well-formed XML. Document is instantiated to the
 * term malformed(Attributes, Content) if Chars does not represent well-formed
 * XML. The Content of a malformed/2 structure can contain:
 *
 *    unparsed( <string> )                :  Text which has not been parsed
 *    out_of_context( <tag> )             :  <tag> is not closed
 *
 * in addition to the standard term types.
 *
 * Out-bound (Document -> Chars) parsing _does_ require that Document defines
 * strictly well-formed XML. If an error is detected a 'domain' exception is
 * raised.
 *
 * The domain exception will attempt to identify the particular sub-term in
 * error and the message will show a list of its ancestor elements in the form
 * <tag>{(id)}* where <id> is the value of any attribute _named_ id.
 *
 * At this release, the Controls applying to in-bound (Chars -> Document)
 * parsing are:
 *
 *    extended_characters(<bool>)         :  Use the extended character
 *                                        :  entities for XHTML (default true)
 *
 *    format(<bool>)                      :  Strip layouts when no character data
 *                                        :  appears between elements.
 *                                        :  (default true)
 *
 *    remove_attribute_prefixes(<bool>)   :  Remove namespace prefixes from
 *                                        :  attributes when it's the same as the
 *                                        :  prefix of the parent element
 *                                        :  (default false).
 *
 *    allow_ampersand(<bool>)             :  Allow unescaped ampersand
 *                                        :  characters (&) to occur in PCDATA.
 *                                        :  (default false).
 *
 *    [<bool> is one of 'true' or 'false']
 *
 * For out-bound (Document -> Chars) parsing, the only available option is:
 *
 *    format(<Bool>)                      :  Indent the element content
 *                                        :  (default true)
 *
 * Different DCGs for input and output are used because input parsing is
 * more flexible than output parsing. Errors in input are recorded as part
 * of the data structure. Output parsing throws an exception if the document
 * is not well-formed, diagnosis tries to identify the specific culprit term.
 */
xml_parse( Chars, Document ) :-
	xml_parse( [], Chars, Document ).

xml_parse( Controls, Chars, Document ) :-
	( ground( Chars ) ->
		xml_to_document( Controls, Chars, Document )
	; otherwise ->
		document_to_xml( Controls, Document, Chars )
	).

document_to_xml( Controls, Document, Chars ) :-
	( member( format(false), Controls ) ->
		Format = false
	; otherwise ->
		Format = true
	),
	( ground( Document ),
	  document_generation(Format, Document, Chars0, [] ) ->
			Chars = Chars0
	; otherwise ->
		xml_fault( Document, [], Culprit, Path, Message ),
		throw(
			application_error('XML Parse: ~s in ~q~nCulprit: ~q~nPath: ~s', 
				[Message,Document,Culprit,Path] )
			)
	).

/* xml_subterm( +XMLTerm, ?Subterm ) unifies Subterm with a sub-term of Term.
 * Note that XMLTerm is a sub-term of itself. 
 */
xml_subterm( Term, Term ).
xml_subterm( xml(_Attributes, Content), Term ) :-
	xml_subterm( Content, Term ).	
xml_subterm( [H|T], Term ) :-
	( xml_subterm( H, Term )
	; xml_subterm( T, Term )
	).
xml_subterm( element(_Name,_Attributes,Content), Term ) :-
	xml_subterm( Content, Term ).
xml_subterm( namespace(_URI,_Prefix,Content), Term ) :-
	xml_subterm( Content, Term ).

/* xml is intended to be a rather modular module: it should be easy to
 * build a program that can output XML, but not read it, or vice versa.
 * Similarly, you may be happy to dispense with diagnosis once you are
 * sure that your code will only try to make valid calls to xml_parse/2.
 *
 * It is intended that the code should be very portable too. Clearly,
 * some small changes will be needed between platforms, but these should
 * be limited to xml_utilities. xml_utilities contains most of the shared
 * code and most of the potentially non-portable code.
 */
:- ensure_loaded( xml_acquisition ).
:- ensure_loaded( xml_diagnosis ).
:- ensure_loaded( xml_generation ).
:- ensure_loaded( xml_pp ).
:- ensure_loaded( xml_utilities ).
