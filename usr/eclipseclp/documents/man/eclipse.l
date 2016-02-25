.\"	$Id: eclipse.l,v 1.3 2013/05/30 17:44:46 jschimpf Exp $
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

.TH ECLIPSE L "January 2003"
.UC 4
.SH NAME
eclipse \- ECLiPSe Constraint Logic Programming System (console interface)
.SH SYNOPSIS
.B eclipse [ options ...]
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
The following options are currently recognized by
.B eclipse:
.TP 13
.B \-b bootfile
the same as -f bootfile.
.TP
.B \-f bootfile
compile the file
.I bootfile
before starting the session.
Multiple -f options are allowed.  The file is processed by
ensure_loaded/1 and can be a precompiled file or a source file.
.TP
.B \-e goal
Instead of starting an interactive toplevel, the system will
execute the goal
.I goal.
This goal is given in normal Prolog syntax,
and has to be quoted if it contains any characters that would normally
be interpreted by the shell.  The -e option can be used together with
the -b option and is executed afterwards.  The exit status of the
ECLiPSe process reflects success or failure of the executed Prolog
goal (0 for success, 1 for failure, 2 for abort). 
.br
When you only have a runtime installation of eclipse, the -e option
is compulsory because a runtime system does not have an interactive
toplevel.
.TP
.B \-g size
This option specifies to which limit the memory consumption of the
ECLiPSe global/trail stack can grow.  The size is specified in
kilobytes (followed by an optional K), in megabytes (followed by M) or
in gigabytes (followed by G).
The default is 512M on 64-bit, or 256M on 32-bit machines.
The amount required for this stack depends on the program's data
structures and may need to be increased for very large applications.
.TP
.B \-l size
This option specifies to which limit the memory consumption of the
ECLiPSe local/control stack can grow.  The size is specified in
kilobytes (followed by an optional K), in megabytes (followed by M) or
in gigabytes (followed by G).
The default is 128M on 64-bit, or 64M on 32-bit machines.
The local/control stack is unlikely to require more than this default.
If it does, it is probably caused by a programming error.
.TP
.B \-D directory
This options allows to explicitly specify the ECLiPSe installation
directory, i.e. the directory under which the system tries to find the
ECLiPSe runtime system and libraries.  This option overrides (and
renders unnecessary) any setting of the ECLIPSEDIR environment
variable (Unix) or, respectively, an ECLIPSEDIR registry entry
(Windows) that may be in effect. 
.TP
.B \-L language
The name of the language dialect used in the top level module.
The default is eclipse_language, other possible values include
iso, iso_strict, quintus etc.
This property can also be set via an ECLIPSEDEFAULTLANGUAGE
environment variable.
.TP
.B \-t module
The name of the top level module.  This is an initially empty module,
created by the system, which serves as the context for -f and
-e options, and in which interactive toplevel queries are executed.
This can be an arbitrary name, as long as it does not conflict with
important library names.  The default is eclipse.
.TP
.B \-\-
The ECLiPSe system will ignore this argument and everything that
follows on the commmand line.  The Prolog program will only see the
part of the command line that follows this argument. 


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
.B ECLIPSEINIT
the name of an initialization file which is compiled just before
starting the interactive toplevel.
If ECLIPSEINIT is unset, the default is '.eclipserc' or ~/.eclipserc.
If ECLIPSEINIT is set to the empty string, no initialization is made.
.TP
.B ECLIPSELIBRARYPATH
a colon-separated sequence of directory names, which get prepended to
ECLiPSe's list of library directories.
.TP
.B ECLIPSEDEFAULTLANGUAGE
The name of the language dialect used in the top level module.
The default is eclipse_language, other possible values include
iso, iso_strict, quintus etc.


.SH FILES
.TP 20
.B $ECLIPSEDIR
the ECLiPSe installation directory
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
.IR tkeclipse (1), tktools (1)
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


.SH SIGNAL HANDLING
The eclipse runtime system (eclipse -e) modifies the handler for SIGALRM
in order to implement ECLiPSe's timer facilities.
The interactive development system additionally defines handlers for SIGPROF
and SIGINT to implement profiling and user interrupt.
The development system also tries to catch all other signals and handle them
using appropriate defaults.


.SH BUGS
Bugs should be reported via the web site or by email
.IP
http://www.eclipse-clp.org/bugs.html
mailto:eclipse-clp-bugs@lists.sourceforge.net
.br
.LP
