.\"	$Id: tkeclipse.l,v 1.2 2009/07/16 09:11:24 jschimpf Exp $
.\" BEGIN LICENSE BLOCK
.\" Version: CMPL 1.1
.\"
.\" The contents of this file are subject to the Cisco-style Mozilla Public
.\" License Version 1.1 (the "License"); you may not use this file except
.\" in compliance with the License.  You may obtain a copy of the License
.\" at www.eclipse-clp.org/license.
.\" 
.\" Software distributed under the License is distributed on an "AS IS"
.\" basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
.\" the License for the specific language governing rights and limitations
.\" under the License. 
.\" 
.\" The Original Code is  The ECLiPSe Constraint Logic Programming System. 
.\" The Initial Developer of the Original Code is  Cisco Systems, Inc. 
.\" Portions created by the Initial Developer are
.\" Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
.\" 
.\" Contributor(s): 
.\" 
.\" END LICENSE BLOCK
.TH TKECLIPSE L "January 2003"
.UC 4
.SH NAME
tkeclipse \- ECLiPSe Constraint Logic Programming System (graphical user interface)
.SH SYNOPSIS
.B tkeclipse [ options ...]
.SH DESCRIPTION
.I ECLiPSe
is a Constraint Logic Programming system, initially developed at the
European Computer-Industry Research Centre (ECRC) in Munich, and
between 1995 and 2005 at the Centre for Planning and Resource Control (IC-Parc)
at Imperial College London. It became open source in 2006.

.I ECLiPSe
can be used simply as a Prolog system, and provides compatibility
libraries for a variety of other Prolog dialects and the ISO Prolog standard.
As a Prolog system,
.I ECLiPSe
implements a number of nonstandard language features, including
an advanced module system,
attributed variables,
an elaborate implementation of data-driven goal execution,
array syntax,
structures with named fields,
string data type,
arithmetic with big integers, rational numbers and intervals,
iteration, matching,
synchronous and asynchronous event handling,
nonlogical stores and an
external language interface.

The main purpose of
.I ECLiPSe
however is to be used as a constraint programming language. In particular,
it is aimed at problem solving with a variety of classical methods
(e.g. constraint propagation, mathematical programming, and local search),
and the combination of these basic methods into new hybrid methods.
The system's constraint solving facilities are largely based on the primitives
for data-driven execution, and are generally provided in the form
of constraint solver libraries.

.SH OPTIONS
If started from a command line, the following options are currently 
recognized by
.B tkeclipse:
.TP 13
.B \-g size
This option specifies to which limit the memory consumption of the
ECLiPSe global/trail stack can grow.  The size is specified in
kilobytes (followed by an optional K), in megabytes (followed by M) or
in gigabytes (followed by G).  The default can be specified in the tkeclipse 
preference editor; otherwise it defaults to 128M, ie. 128 Megabytes.
The amount required for this stack depends on the program's data
structures and may need to be increased for very large applications.
.TP
.B \-l size
This option specifies to which limit the memory consumption of the
ECLiPSe local/control stack can grow.  The size is specified in
kilobytes (followed by an optional K), in megabytes (followed by M) or
in gigabytes (followed by G).  The default can be specified in the tkeclipse 
preference editor; otherwise it defaults is 128M, ie. 128 Megabytes.
The local/control stack is unlikely to require more than this default.
If it does, it is probably caused by a programming error.


.SH ENVIRONMENT VARIABLES
The following environment variable settings are recognised by ECLiPSe,
but none of them needs to be set in order to run eclipse:
.TP 20
.B ECLIPSEDIR
the ECLiPSe installation directory. The default is fixed during
ECLiPSe installation.
.TP
.B ECLIPSETMP
the directory in which ECLiPSe may store temporary files, by default /tmp.
.TP
.B ECLIPSELIBRARYPATH
a colon-separated sequence of directory names, which get prepended to
ECLiPSe's list of library directories.


.SH FILES
.TP 20
.B $ECLIPSEDIR
usually /usr/local/eclipse, the ECLiPSe installation directory
.TP
.B $ECLIPSEDIR/bin/`arch`/eclipse
an executable eclipse (console interface)
.TP
.B $ECLIPSEDIR/bin/`arch`/tkeclipse
an executable eclipse (graphical user interface)
.TP
.B $ECLIPSEDIR/bin/`arch`/tktools
standalone ECLiPSe GUI debugging tools that can be attached to a
separate ECLiPSe session.
.TP
.B $ECLIPSEDIR/doc
online documentation and manuals
.TP
.B $ECLIPSEDIR/include
contains the .h files necessary to compile code using ECLiPSe's
C/C++ interfaces.
.TP
.B $ECLIPSEDIR/{lib,lib_public}
contains the ECLiPSe libraries
.TP
.B $ECLIPSETMP/eclipse.XXX.<pid>
temporary file created by ECLiPSe, e.g. for the profiler.


.SH SEE ALSO
.nf
.IR eclipse (1), tktools (1)
.sp
.I "ECLiPSe Web Site"
http://www.eclipse-clp.org
.sp
.I "Online Documentation"
In $ECLIPSEDIR/doc/index.html, containing the following:
.sp
.IR "ECLiPSe, An Introduction" ,
Andrew M. Cheadle et al, IC-PARC.
.sp
.IR "ECLiPSe User Manual" ,
Abderrahamane Aggoun et al, IC-PARC.
.sp
.IR "ECLiPSe Constraint Library Manual" ,
Pascal Brisset et al, IC-PARC.
.sp
.I "ECLiPSe Embedding and Interfacing Manual"
Stefan Novello, Joachim Schimpf, Kish Shen and Josh Singer, IC-PARC.
.sp
.IR "ECLiPSe Visualisation Manual" ,
Kish Shen, Josh Singer and Andrew Sadler, IC-PARC.


.SH BUGS
Bugs should be reported via the web site or by email
.IP
http://www.eclipse-clp.org/bugs.html
mailto:eclipse-clp-bugs@lists.sourceforge.net
.br
.LP
