.\"	$Id: tktools.l,v 1.2 2015/01/14 01:31:10 jschimpf Exp $
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
.TH TKTOOLS L "January 2003"
.SH NAME
tktools \- standalone debugging tools for ECLiPSe constraint logic programming system
.SH SYNOPSIS
.B tktools
.SH DESCRIPTION
.I tktools
connects
.I tktools
to an executing instance of ECLiPSe. It starts a small graphical user
interface to which the
.I host
and
.I port
of the target ECLiPSe instance must be supplied. This target can be
any variant of ECLiPSe, for instance an interactive console eclipse,
a tkeclipse development system,
a standalone runtime eclipse, or even an eclipse instance which is
embedded into some host process.

The cooperating instance of ECLiPSe must invoke
remote_tools:attach_tools/0 or remote_tools:attach_tools/3 in order
to make the connection.
If attach_tools/0 is used, the system chooses a port number and prints
it. This number must be entered into the tktools port entry field.
Alternatively, a port number can be choosen, and supplied to both the
tktools port entry field, and to attach_tools/3 on the eclipse side.
The host machine can be
.I localhost
if tktools and eclipse are on the same machine, otherwise the full
domain name of the machine where the eclipse process is running.


.SH OPTIONS
Instead of entering the host/port information through the graphical user
interface, it can be supplied via the following command line options:
.TP 13
.B \-h host
The name of the host on which the target ECLiPSe instance is running.
.TP
.B \-p port
The port number to use to connect to the target ECLiPSe instance.


.SH SEE ALSO
.sp
.IR eclipse (1),
.IR tkeclipse (1)
.sp
.I "Online Documentation"
In $ECLIPSEDIR/doc/index.html, in particular:
.sp
.IR "ECLiPSe User Manual" ,
Abderrahamane Aggoun et al, IC-PARC.
Chapter "The TkECLiPSe Development Tools"
