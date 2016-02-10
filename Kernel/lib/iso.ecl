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
% Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: iso.ecl,v 1.2 2013/03/06 22:06:45 jschimpf Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe PROLOG LIBRARY MODULE
%
% $Id: iso.ecl,v 1.2 2013/03/06 22:06:45 jschimpf Exp $
%
% IDENTIFICATION:	iso.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		see export directives
%
% DESCRIPTION:		ISO Prolog compatibility package. It follows
%			* ISO/IEC 13211-1 (1995)
%			* ISO/IEC 13211-1 TC1 (2007)
%			* ISO/IEC 13211-1 TC2 (2012)
%

:- module(iso).

:- pragma(deprecated_warnings(not_reexports)).
:- reexport iso_light.
:- ensure_loaded(iso_error).

:- comment(categories, [`Compatibility`]).
:- comment(summary, `ISO Prolog compatibility library`).
:- comment(author, `Joachim Schimpf, ECRC, IC-Parc and Coninfer Ltd`).
:- comment(copyright, `Cisco Systems, Inc (2006), Coninfer Ltd (modifications 2007-2012)`).
:- comment(date, `$Date: 2013/03/06 22:06:45 $`).
:- comment(see_also, [library(multifile),library(iso_strict),library(iso_light)]).
:- comment(desc, html(`\
<h3>Overview</h3>\n\
    This library provides an implementation of Standard Prolog as\n\
    defined in ISO/IEC 13211-1 (Information Technology, Programming\n\
    Languages, Prolog, Part 1, General Core, 1995) and the technical\n\
    corrigenda ISO/IEC 13211-1 TC1 (2007) and TC2 (2012).\n\
    The library is provided in source form.\n\
    <P>\n\
    There are libraries for three degrees of compatibility:\n\
    <DL>\n\
    <DT><STRONG>iso</STRONG></DT>\n\
        <DD>A blend of ISO and ECLiPSe functionality.  All ISO features\n\
        are available, plus such ECLiPSe features that do not significantly\n\
        conflict with ISO. But as some of these extensions go beyond what the\n\
        letter of the standard allows, and because error checking may be\n\
        less strict than required by ISO, this is not a fully compliant mode.\n\
    <DT><STRONG>iso_light</STRONG></DT>\n\
        <DD>The same as 'iso', with the exception of error handling.\n\
        This is sufficient for code that does not rely on a particular\n\
        form of error terms being thrown by built-in predicates.</DD>\n\
    <DT><STRONG>iso_strict</STRONG></DT>\n\
        <DD>This aims to be fully ISO compliant, and represents the\n\
        'strict mode' required by the standard.</DD>\n\
    </DL>\n\
    For more details on ISO compliance, see library(iso_strict).\n\
    <P>\n\
<h3>Usage</h3>\n\
    The effect of this compatibility library is (with minor exceptions)\n\
    local to the module where it is loaded.  An ISO-program should always\n\
    be contained in a separate module, starting with a directive like\n\
    <PRE>\n\
    :- module(myisomodule, [], iso).\n\
    </PRE>\n\
    Here, the last argument of the module/3 directive indicates the language.\n\
    It is not advisable to use ":-lib(iso)" or ":-ensure_loaded(library(iso))"\n\
    within an eclipse_language module, because this would lead to import\n\
    conflicts between the different versions of built-in predicates.\n\
    <P>\n\
    Alternatively, in order to use ISO-Prolog without having different\n\
    modules, one can invoke eclipse with a "-L iso" command line option,\n\
    or set the ECLIPSEDEFFAULTLANGUAGE environment variable to 'iso'.\n\
    This will launch eclipse with a default module accepting 'iso' language\n\
    instead of the usual 'eclipse_language'.\n\
    <P>\n\
<h3>Differences</h3>\n\
    The main differences of this extended iso-language compared to the\n\
    default eclipse_language are the following:\n\
    <UL>\n\
    <LI>The syntax is more restricted, and ISO style is preferred, e.g.\n\
        for writing hexadecimal numbers.</LI>\n\
    <LI>Double quotes denote character lists, while in ECLiPSe they denote\n\
        string constants.  ECLiPSe-strings can however be written using\n\
        back-quotes, e.g. \`abc\`.</LI>\n\
    <LI>Arithmetic functions like floor/1 return integer results.</LI>\n\
    <LI>All ISO built-in predicates are available.</LI>\n\
    </UL>\n\
<h3>Limitations</h3>\n\
    ISO's idiosyncratic char_conversion features is not implemented.\n\
`)).

