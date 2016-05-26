%
% $Id: xml.ecl,v 1.2 2012/02/06 13:30:55 jschimpf Exp $
%
% slightly modified version of xml.pl for ECLiPSe
%

/* xml.pl : Contains xml_parse/[2,3] a bi-directional XML parser written in
 * Prolog.
 *
 * Copyright (C) 2001-2003 Binding Time Limited
 *
 * Current Release: 1.6
 * 
 * TERMS AND CONDITIONS:
 *
 * This program is offered free of charge, as unsupported source code. You may
 * use it, copy it, distribute it, modify it or sell it without restriction. 
 * 
 * We hope that it will be useful to you, but it is provided "as is" without
 * any warranty express or implied, including but not limited to the warranty
 * of non-infringement and the implied warranties of merchantability and fitness
 * for a particular purpose.
 * 
 * Binding Time Limited will not be liable for any damages suffered by you as
 * a result of using the Program. In no event will Binding Time Limited be
 * liable for any special, indirect or consequential damages or lost profits
 * even if Binding Time Limited has been advised of the possibility of their
 * occurrence. Binding Time Limited will not be liable for any third party
 * claims against you.
 *
 * History:
 * $Log: xml.ecl,v $
 * Revision 1.2  2012/02/06 13:30:55  jschimpf
 * Deprecated block/3 and exit_block/1, making iso compatible catch/3 and throw/1 generally available instead.
 *
 * Revision 1.1.1.1  2006/09/23 01:45:20  snovello
 * Cisco initial import
 *
 * Revision 1.4  2005/05/03 17:23:59  js10
 * Added is_list/1 builtin, including exploitation for indexing.
 *
 * Revision 1.3  2003/03/31 13:58:02  js10
 * Upgraded to latest version from John Fletcher's web site
 *
 * Revision 1.3  2003-03-12 00:12:04+00  john
 * Set Current Release to 1.6.
 *
 * Revision 1.2  2002-05-25 23:17:54+01  john
 * Set Current Release to 1.5.
 *
 * Revision 1.1  2002-01-31 21:04:45+00  john
 * Updated Copyright statements.
 *
 * Revision 1.0  2001-10-17 20:46:24+01  john
 * Initial revision
 *
 *
 *
 */
:- module( xml,
	[
	xml_parse/2,
	xml_parse/3,
	xml_subterm/2,
	xml_pp/1
	], sicstus ).		% ECLiPSe

:- local
	member/2,
	select/3,
	is_list/1,
	throw/1.		% ECLiPSe


/* xml_parse( {+Controls}, +?Chars, ?+Document ) parses Chars to/from a data
 * structure of the form xml(<atts>, <content>). <atts> is a list of
 * <atom>=<string> attributes from the (possibly implicit) XML signature of the
 * document. <content> is a (possibly empty) list comprising occurrences of :
 *
 *	pcdata(<string>)					:	Text
 *	comment(<string>)					:	An xml comment;
 *  element(<tag>,<atts>,<content>)		:	<tag>..</tag> encloses <content>
 *										:   <tag /> if empty
 *	instructions(<atom>, <string>)		:	Processing <? <atom> <params> ?>"
 * 	cdata( <string> )					:	<![CDATA[ <string> ]]>
 *	doctype(<atom>, <doctype id>)		:	DTD <!DOCTYPE .. >
 *
 * The conversions are not completely symmetrical, in that weaker XML is
 * accepted than can be generated. Specifically, in-bound (Chars -> Document)
 * does not  require strictly well-formed XML. Document is instantiated to the
 * term malformed(Attributes, Content) if Chars does not represent well-formed
 * XML. The Content of a malformed/2 structure can contain:
 *
 *	unparsed( <string> )				:	Text which has not been parsed
 *	out_of_context( <tag> )				:	<tag> is not closed
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
 *	extended_characters(<bool>)			:	Use the extended character
 *										:	entities for XHTML (default true)
 *
 *	format(<bool>)						:	Strip layouts when no character data
 *										:	appears between elements.
 *										:	(default true)
 *
 *	[<bool> is one of 'true' or 'false']
 *
 * For out-bound (Document -> Chars) parsing, the only available option is:
 *
 *	format(<Bool>)						:	Indent the element content
 *										:	(default true)
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

:- local chtab(0'", string_quote).	% ECLiPSe
:- comment(include, xml_comments).	% ECLiPSe
