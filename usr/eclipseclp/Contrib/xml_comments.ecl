:-comment(categories, ["Interfacing"]).
:- comment(summary,'A bi-directional XML parser').
:- comment(author, "John Fletcher, ECLiPSe wrapper by Joachim Schimpf").
:- comment(copyright, "Copyright (C) 2001-2005 Binding Time Limited, 2005,2006 John Fletcher").
:- comment(desc, html("
<h2>Note for ECLiPSe users</h2>
<p>
This code creates and accepts character lists rather than ECLiPSe strings. 
To convert between character lists and (UTF8 or ASCII) strings, use the
ECLiPSe built-in string_list/3. For example, to parse a UTF-8 encoded
XML file, use the following code:
</p><pre>
xml_parse_file(File, Document) :-
	open(File, read, Stream),
	read_string(Stream, end_of_file, _, Utf8String),
	close(Stream),
	string_list(Utf8String, Chars, utf8),
	xml_parse(Chars, Document).
</pre><p>
This is Revision 2.0 of John Fletcher's code.
Most of the subsequent text is taken literally from
<a href=\"http://www.binding-time.co.uk/xmlpl.html\">
http://www.binding-time.co.uk/xmlpl.html</a>.
</p>

<h2>TERMS AND CONDITIONS</h2>
<p>
This program is offered free of charge, as unsupported source code. You may
use it, copy it, distribute it, modify it or sell it without restriction,
but entirely at your own risk.
</p><p>
We hope that it will be useful to you, but it is provided \"as is\" without
any warranty express or implied, including but not limited to the warranty
of non-infringement and the implied warranties of merchantability and fitness
for a particular purpose.
</p>
<pre>
History:
$Log: xml_comments.ecl,v $
Revision 1.4  2009/07/16 09:11:23  jschimpf
Merged patches_6_0 branch up to merge_2009_07_16

Revision 1.3.2.2  2009/04/09 02:11:38  jschimpf
Updated the url in documentation

Revision 1.3.2.1  2009/02/19 06:26:40  jschimpf
Added comment(categories,...) annotations for better documentation

Revision 1.3  2006/10/17 22:06:22  jschimpf
Reinserted lost licensing paragraph.

Revision 1.2  2006/10/17 22:02:21  jschimpf
Upgraded to John Fletcher's revision 2.0, released 2006/06/18,
available at http://www.zen37763.zen.co.uk/xml_download.html

Revision 1.1  2003/03/31 13:58:02  js10
Upgraded to latest version from John Fletcher's web site

Revision 1.2  2002/03/26 22:56:55  js10
Added John Fletcher's public domain XML parser/generator

Revision 1.1  2002/03/26 22:50:07  js10
Added John Fletcher's public domain XML parser/generator
</pre>

  <h2>Background</h2>
  <p>xml.pl is a module for parsing <acronym title=\"eXtensible Markup Language\">XML</acronym> with Prolog, which provides
Prolog applications with a simple \"Document Value Model\"
interface to XML documents. It has been used successfully in a number of applications.</p>
  <p>It supports a subset of XML suitable
for XML Data and Worldwide Web applications. It is not as strict nor as
comprehensive as the <a href=\"http://www.w3.org/TR/2000/REC-xml-20001006\">XML 1.0 Specification</a> mandates.</p>
  <p>It is not as strict, because, while the
specification must eliminate ambiguities, not all errors need to be regarded as
faults, and some reasonable examples of real XML usage would have to be
rejected if they were.</p>
  <p>It is not as comprehensive, because,
where the XML specification makes provision for more or less complete <acronym title=\"Document Type Declaration\">DTD</acronym>s to be provided as part of a
document, xml.pl actions the local definition of ENTITIES only. Other <acronym title=\"Document Type Declaration\">DTD</acronym> extensions are treated as
commentary.</p>
  <p>
   <a href=\"http://www.binding-time.co.uk/xml_download.html\">The code, and a
small Windows application which embodies it</a>, has been placed into the public domain, to
encourage the use of Prolog with XML.</p>
  <p>I hope that they will be useful to
you, but they are not supported, and they are provided without any warranty of any kind.</p>
  <h2>Specification</h2>
  <p>Three predicates are exported by the
module: <abbr>xml_parse/[2,3]</abbr>, <abbr>xml_subterm/2</abbr> and <abbr>xml_pp/1.</abbr></p>
  <p>
   <dfn>xml_parse( {+Controls}, +?Chars,
?+Document )</dfn> parses <var>Chars</var>, a list of character codes,
to/from a data structure of the form <code>
xml(<span class=\"Nested BNF\">
&lt;attributes&gt;</span>, <span class=\"Nested BNF\">
&lt;content&gt;</span>)
</code> , where:</p>
  <p>
   <span class=\"BNF\">
    
&lt;attributes&gt;
   </span> is a list of <span class=\"BNF\">

&lt;name&gt;</span>=<span class=\"BNF\">
&lt;char data&gt;</span>
 attributes from the (possibly implicit) XML signature of the
document.</p>
  <p>
   <span class=\"BNF\">
    
&lt;content&gt;
   </span> is a (possibly empty) list comprising occurrences of :</p>
  <dl>
   <dt>
    <code>
pcdata(<span class=\"Nested BNF\">
&lt;char data&gt;</span>)
</code>
   </dt>
   <dd>Text</dd>
   <dt>
    <code>
comment(<span class=\"Nested BNF\">
&lt;char data&gt;</span>)
</code>
   </dt>
   <dd>An xml comment;</dd>
   <dt>
    <code>
namespace(<span class=\"Nested BNF\">
&lt;URI&gt;</span>,<span class=\"Nested BNF\">
&lt;prefix&gt;</span>,<span class=\"Nested BNF\">
&lt;element&gt;</span>)
</code>
   </dt>
   <dd>a Namespace</dd>
   <dt>
    <code>
element(<span class=\"Nested BNF\">
&lt;tag&gt;</span>, <span class=\"Nested BNF\">
&lt;attributes&gt;</span>, <span class=\"Nested BNF\">
&lt;content&gt;</span>)
</code>
   </dt>
   <dd>
    <span class=\"BNF\">
     
&lt;tag&gt;..&lt;/tag&gt;
    </span> encloses <span class=\"BNF\">
&lt;content&gt;</span> or <span class=\"BNF\">
&lt;tag /&gt;</span> if empty.</dd>
   <dt>
    <code>
instructions(<span class=\"Nested BNF\">
&lt;name&gt;</span>, <span class=\"Nested BNF\">
&lt;char data&gt;</span>)
</code>
   </dt>
   <dd>A PI  
&lt;?<span class=\"BNF\">
&lt;name&gt;</span><span class=\"BNF\">
&lt;char data&gt;</span>
?&gt;</dd>
   <dt>
    <code>
cdata(<span class=\"Nested BNF\">
&lt;char data&gt;</span>)
</code>
   </dt>
   <dd>&lt;![CDATA[<span class=\"BNF\">
&lt;char data&gt;</span>]]&gt;
</dd>
   <dt>
    <code>
doctype(<span class=\"Nested BNF\">
&lt;tag&gt;</span>, <span class=\"Nested BNF\">
&lt;doctype id&gt;</span>)
</code>
   </dt>
   <dd>
    
DTD &lt;!DOCTYPE .. &gt;
   </dd>
  </dl>
  <p>The conversions are not completely
symmetrical, in that weaker XML is accepted than can be generated.
Specifically, in-bound <em>(Chars -&gt;
Document)</em> parsing does not require strictly well-formed XML. If <var>Chars</var> does not represent well-formed
XML, <var>Document</var> is instantiated
to the term <code>malformed(<span class=\"Nested BNF\">
&lt;attributes&gt;</span>, <span class=\"Nested BNF\">
&lt;content&gt;</span>)


</code> .</p>
  <p>The <span class=\"BNF\">
&lt;content&gt;</span> of a <abbr>malformed/2</abbr>
structure can include:</p>
  <dl>
   <dt>
    <code>
unparsed( <span class=\"Nested BNF\">
&lt;char data&gt;</span> )
</code>
   </dt>
   <dd>Text which has not been parsed</dd>
   <dt>
    <code>
out_of_context( <span class=\"Nested BNF\">
&lt;tag&gt;</span> )
</code>
   </dt>
   <dd>
    <span class=\"BNF\">
     
&lt;tag&gt;
    </span> is not closed
</dd>
  </dl>
  <p>in addition to the parsed term
types.</p>
  <p>Out-bound <em>(Document -&gt; Chars)</em> parsing <em>does</em> require that <var>Document</var> defines well-formed XML. If
an error is detected a 'domain' exception is raised.</p>
  <p>The domain exception will attempt to
identify the particular sub-term in error and the message will show a list of
its ancestor elements in the form <span class=\"BNF\">
&lt;tag&gt;{(id)}*</span> where <span class=\"BNF\">

&lt;id&gt;
</span> is the value of any attribute <em>named</em> id.</p>
  <p>At this release, the <var>Controls</var> applying
to in-bound <em>(Chars -&gt;
Document)</em> parsing are:</p>
  <dl>
   <dt>
    <code>
extended_characters(<span class=\"Nested BNF\">
&lt;bool&gt;</span>)
</code>
   </dt>
   <dd>Use the extended character entities for XHTML (default true).</dd>
   <dt>
    <code>
format(<span class=\"Nested BNF\">
&lt;bool&gt;</span>)
</code>
   </dt>
   <dd>Remove layouts
when no non-layout character data appears between elements (default true).</dd>
   <dt>
    <code>
remove_attribute_prefixes(<span class=\"Nested BNF\">
&lt;bool&gt;</span>)
</code>
   </dt>
   <dd>Remove redundant prefixes from attributes - i.e. prefixes
   denoting the namespace of the parent element (default false).</dd>
   <dt>
    <code>
allow_ampersand(<span class=\"Nested BNF\">
&lt;bool&gt;</span>)
</code>
   </dt>
   <dd>Allow unescaped ampersand characters (&amp;) to occur in PCDATA
(default false).</dd>
  </dl>
  <p>For out-bound <em>(Document -&gt; Chars)</em> parsing, the
only available option is:</p>
  <dl>
   <dt>
    <code>
format(<span class=\"Nested BNF\">
&lt;bool&gt;</span>)
</code>
   </dt>
   <dd>Indent the element content, (default true)</dd>
  </dl>
  <h3>Types</h3>
  <dl>
   <dt>
    <span class=\"BNF\">
     
&lt;tag&gt;
    </span>
   </dt>
   <dd>An atom naming an element</dd>
   <dt>
    <span class=\"BNF\">
     
&lt;name&gt;
    </span>
   </dt>
   <dd>An atom, not naming an element</dd>
   <dt>
    <span class=\"BNF\">
     
&lt;URI&gt;
    </span>
   </dt>
   <dd>An atom giving the URI of a Namespace</dd>
   <dt>
    <span class=\"BNF\">
     
&lt;char data&gt;
    </span>
   </dt>
   <dd>A \"string\": list of character codes.</dd>
   <dt>
    <span class=\"BNF\">
     
&lt;doctype id&gt;
    </span>
   </dt>
   <dd>one of <code>
public(<span class=\"Nested BNF\">
&lt;char data&gt;</span>, <span class=\"Nested BNF\">
&lt;char data&gt;</span>)
</code>,
<code>public(<span class=\"Nested BNF\">
&lt;char data&gt;</span>,
<span class=\"Nested BNF\">
&lt;char data&gt;</span>,
<span class=\"Nested BNF\">
&lt;dtd literals&gt;</span>)</code>,
<code>system(<span class=\"Nested BNF\">
&lt;char data&gt;</span>)</code>,
<code>system(<span class=\"Nested BNF\">
&lt;char data&gt;</span>,
<span class=\"Nested BNF\">
&lt;dtd literals&gt;</span>)</code>,
<code>local</code> or <code>local(<span class=\"Nested BNF\">
&lt;dtd literals&gt;</span>)</code></dd>
   <dt>
    <span class=\"BNF\">
     
&lt;dtd literals&gt;
    </span>
   </dt>
   <dd>A non-empty list of
	<code>dtd_literal(<span class=\"Nested BNF\">
&lt;char data&gt;</span>)</code> terms - e.g. <a href=\"http://www.w3.org/TR/2000/REC-xml-20001006#NT-AttlistDecl\">attribute-list
declarations</a>.</dd>
   <dt>
    <span class=\"BNF\">
     
&lt;bool&gt;
    </span>
   </dt>
   <dd>one of <code>true</code>
or <code>false</code></dd>
  </dl>
  <p>
   <dfn>xml_subterm( +XMLTerm, ?Subterm )</dfn> unifies <var>Subterm</var>
   with a sub-term
of <var>Term</var>. This can be especially
useful when trying to test or retrieve a deeply-nested subterm from a document
- as demonstrated in this <a href=\"http://www.binding-time.co.uk/xml_example.html\">example program</a>.
Note that <var>XMLTerm</var> is a sub-term of itself.</p>
  <p>
   <dfn>xml_pp( +XMLDocument )</dfn>&nbsp;\"pretty
prints\" <var>XMLDocument</var> on the
current output stream.</p>
  <h2>Availability</h2>
  <p>The module is available from <a href=\"http://www.binding-time.co.uk/xml_download.html\">this site</a>, and is supplied as a library with the following Prologs:</p>
  <ul>
   <li>It is available in the <a href=\"http://eclipse-clp.org\">ECLiPSe Constraint Programming System</a>, as a
third-party library;</li>
   <li>It has been ported to <a href=\"http://www.probp.com/\">B-Prolog</a>
by Neng-Fa Zhou.</li>
   <li>It has been adapted for <a href=\"http://www.sics.se/sicstus/\">SICStus Prolog</a> version <a href=\"http://www.sics.se/sicstus/docs/latest/html/relnotes.html/3.11.0-Changes.html\">3.11+</a>
by Mats Carlsson.</li>
   <li>It is included in <a href=\"http://www.sics.se/isl/quintuswww/site/rel-history.html\">Quintus Prolog Release 3.5</a>.</li>
  </ul>
  <h2>Features of xml.pl</h2>
  <p>The <code>xml/2</code> data structure has some useful properties.</p>
  <h3>Reusability</h3>
  <p>Using an \"abstract\" Prolog
representation of XML, in which terms represent document \"nodes\", makes the
parser reuseable for any XML application.</p>
  <p>In effect, xml.pl encapsulates the
application-independent tasks of document parsing and generation, which is
essential where documents have components from more than one Namespace.</p>
  <h3>Same Structure</h3>
  <p>The Prolog term representing a document
has the same structure as the document itself, which makes the correspondence
between the literal representation of the Prolog term and the XML source
readily apparent.</p>
  <p>For example, this simple <a href=\"http://www.w3.org/Graphics/SVG/Overview.htm8\"><acronym title=\"Scalable Vector Graphics\">SVG</acronym></a> image:</p>
  <pre>   
&lt;?xml version=\"1.0\" standalone=\"no\"?&gt;
&lt;!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.0//EN\" \"http://www.w3.org/.../svg10.dtd\"
    [
    &lt;!ENTITY redblue \"fill: red; stroke: blue; stroke-width: 1\"&gt;
    ]&gt;
&lt;svg xmlns=\"http://www.w3.org/2000/svg\" width=\"500\" height=\"500\"&gt;
 &lt;circle cx=\" 25 \" cy=\" 25 \" r=\" 24 \" style=\"&amp;redblue;\"/&gt;
&lt;/svg&gt;
  </pre>
  <p>... translates into this Prolog
term:</p>
  <pre>   
xml( [version=\"1.0\", standalone=\"no\"],
    [
    doctype( svg, public( \"-//W3C//DTD SVG 1.0//EN\", \"http://www.w3.org/.../svg10.dtd\" ) ),
    namespace( 'http://www.w3.org/2000/svg', \"\",
        element( svg,
            [width=\"500\", height=\"500\"],
            [
            element( circle,
                [cx=\"25\", cy=\"25\", r=\"24\", style=\"fill: red; stroke: blue; stroke-width: 1\"],
                [] )
            ] )
        )
    ] ).
  </pre>
  <h3>Efficient Manipulation</h3>
  <p>Each type of node in an XML document is
represented by a different Prolog functor, while data, (PCDATA, CDATA and
Attribute Values), are left as \"strings\", (lists of character codes).</p>
  <p>The use of distinct functors for
mark-up structures enables the efficient recursive traversal of a document,
while leaving the data as strings facilitates application-specific parsing of
data content (aka <a href=\"http://www.google.com/search?q=%22Micro-parsing%22+XML\">Micro-parsing</a>).</p>
  <div id=\"cdata_to_pcdata\" class=\"Predicate\">
   <p>For example, to turn every CDATA node
into a PCDATA node with tabs expanded into spaces:</p>
   <pre>    
cdata_to_pcdata( cdata(CharsWithTabs), pcdata(CharsWithSpaces) ) :-
    tab_expansion( CharsWithTabs, CharsWithSpaces ).
cdata_to_pcdata( xml(Attributes, Content1), xml(Attributes, Content2) ) :-
    cdata_to_pcdata( Content1, Content2 ).
cdata_to_pcdata( namespace(URI,Prefix,Content1), namespace(URI,Prefix,Content2) ) :-
    cdata_to_pcdata( Content1, Content2 ).
cdata_to_pcdata( element(Name,Attrs,Content1), element(Name,Attrs,Content2) ) :-
    cdata_to_pcdata( Content1, Content2 ).
cdata_to_pcdata( [], [] ).
cdata_to_pcdata( [H1|T1], [H2|T2] ) :-
    cdata_to_pcdata( H1, H2 ),
    cdata_to_pcdata( T1, T2 ).
cdata_to_pcdata( pcdata(Chars), pcdata(Chars) ).
cdata_to_pcdata( comment(Chars), comment(Chars) ).
cdata_to_pcdata( instructions(Name, Chars), instructions(Name, Chars) ).
cdata_to_pcdata( doctype(Tag, DoctypeId), doctype(Tag, DoctypeId) ).
   </pre>
  </div>
  <p>The above uses no 'cuts', but will not
create any choice points with ground input.</p>
  <h3>Elegance</h3>
  <p>The resolution of entity references and
the decomposition of the document into distinct nodes means that the calling
application is not concerned with the occasionally messy syntax of XML
documents.</p>
  <p>For example, the clean separation of
namespace nodes means that Namespaces, which are useful in combining
specifications developed separately, have similar usefulness in combining
applications developed separately.</p>
  <p>
   <a href=\"http://www.binding-time.co.uk/xml_download.html\">The source code is available here</a>.
Although it is unsupported, please feel free to <a href=\"mailto:john_fletcher@tesco.net\">e-mail queries and suggestions</a>. I
will respond as time allows.</p>
")).


:- comment(xml_parse/3, [
	summary:"Parse or generate XML documents",
	amode:xml_parse(+,+,-),
	amode:xml_parse(+,-,+),
	args:[
	    "Controls":"List of options",
	    "Chars":"List of characters (XML text)",
	    "Document":"Document as structured term"
	],
	see_also:[xml_parse/2],
	desc:ascii("
xml_parse( {+Controls}, +?Chars, ?+Document ) parses Chars to/from a data
structure of the form xml(<atts>, <content>). <atts> is a list of
<atom>=<string> attributes from the (possibly implicit) XML signature of the
document. <content> is a (possibly empty) list comprising occurrences of :

pcdata(<string>)		:	Text
comment(<string>)		:	An xml comment;
element(<tag>,<atts>,<content>)	:	<tag>..</tag> encloses <content>
				:       <tag /> if empty
instructions(<atom>, <string>)	:	Processing <? <atom> <params> ?>
cdata( <string> )		:	<![CDATA[ <string> ]]>
doctype(<atom>, <doctype id>)	:	DTD <!DOCTYPE .. >

The conversions are not completely symmetrical, in that weaker XML is
accepted than can be generated. Specifically, in-bound (Chars -> Document)
does not  require strictly well-formed XML. Document is instantiated to the
term malformed(Attributes, Content) if Chars does not represent well-formed
XML. The Content of a malformed/2 structure can contain:

unparsed( <string> )		:	Text which has not been parsed
out_of_context( <tag> )		:	<tag> is not closed

in addition to the standard term types.

Out-bound (Document -> Chars) parsing _does_ require that Document defines
strictly well-formed XML. If an error is detected a 'domain' exception is
raised.

The domain exception will attempt to identify the particular sub-term in
error and the message will show a list of its ancestor elements in the form
<tag>{(id)}* where <id> is the value of any attribute _named_ id.

At this release, the Controls applying to in-bound (Chars -> Document)
parsing are:

extended_characters(<bool>)	    :	Use the extended character
				    :	entities for XHTML (default true)

format(<bool>)			    :	Strip layouts when no character data
				    :	appears between elements.
				    :	(default true)

remove_attribute_prefixes(<bool>)   :  Remove namespace prefixes from
                                    :  attributes when it's the same as the
                                    :  prefix of the parent element
                                    :  (default false).

allow_ampersand(<bool>)             :  Allow unescaped ampersand
                                    :  characters (&) to occur in PCDATA.
                                    :  (default false).

[<bool> is one of 'true' or 'false']

For out-bound (Document -> Chars) parsing, the only available option is:

format(<Bool>)			    :	Indent the element content
				    :	(default true)

Different DCGs for input and output are used because input parsing is
more flexible than output parsing. Errors in input are recorded as part
of the data structure. Output parsing throws an exception if the document
is not well-formed, diagnosis tries to identify the specific culprit term.
")]).

:- comment(xml_parse/2, [
	summary:"Parse or generate XML documents",
	amode:xml_parse(+,-),
	amode:xml_parse(-,+),
	args:[
	    "Chars":"List of characters",
	    "Document":"Document as structured term"
	],
	see_also:[xml_parse/3,xml_subterm/2,xml_pp/1]
]).

:- comment(xml_subterm/2, [
	summary:"Unifies Subterm with a sub-term of Term.",
	amode:xml_subterm(+,?),
	args:[
	    "XMLTerm":"Structured term",
	    "Subterm":"Structured term"
	],
	desc:html("
    This can be especially useful when trying to test or retrieve a
    deeply-nested subterm from a document - as demonstrated in this
    <a href=\"http://www.binding-time.co.uk/xml_example.html\">example
    program</a>.  Note that <code>XMLTerm</code> is a sub-term of itself."),
	see_also:[xml_parse/2,xml_parse/3,xml_pp/1]
]).


:- comment(xml_pp/1, [
	summary:"Pretty-prints XMLDocument on the current output stream",
	amode:xml_pp(+),
	args:[
	    "XMLDocument":"Document as structured term"
	],
	see_also:[xml_parse/2,xml_parse/3,xml_subterm/2]
]).

