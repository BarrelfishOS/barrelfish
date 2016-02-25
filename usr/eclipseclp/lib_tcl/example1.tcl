#! /bin/sh
# \
	exec wish $0 ${1+"$@"}

# BEGIN LICENSE BLOCK
# Version: CMPL 1.1
#
# The contents of this file are subject to the Cisco-style Mozilla Public
# License Version 1.1 (the "License"); you may not use this file except
# in compliance with the License.  You may obtain a copy of the License
# at www.eclipse-clp.org/license.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
# the License for the specific language governing rights and limitations
# under the License. 
# 
# The Original Code is  The ECLiPSe Constraint Logic Programming System. 
# The Initial Developer of the Original Code is  Cisco Systems, Inc. 
# Portions created by the Initial Developer are
# Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
# 
# Contributor(s): 
# 
# END LICENSE BLOCK
#----------------------------------------------------------------------
# Preamble: Find and load the eclipse package
#----------------------------------------------------------------------

switch $tcl_platform(platform) {
    unix {
	if {![info exists env(ECLIPSEDIR)]} {
	    puts "Cannot run Eclipse: ECLIPSEDIR environment variable is undefined."
	    exit -1
	}
	set tkecl(ECLIPSEDIR) $env(ECLIPSEDIR)
    }
    windows {
	package require registry
	set tkecl(ECLIPSEDIR) [registry get \
	    HKEY_LOCAL_MACHINE\\SOFTWARE\\IC-Parc\\Eclipse\\6.2 ECLIPSEDIR]
    }
    default {
	error "$tcl_platform(platform) not supported"
	exit
    }
}

lappend auto_path [file join $tkecl(ECLIPSEDIR) lib_tcl]

package require eclipse



#----------------------------------------------------------------------
# Make a standard menu bar and a 'file' menu
#----------------------------------------------------------------------

wm title . "The smallest possible ECLiPSe/Tcl/Tk Application"

. config -menu .mbar

menu .mbar
.mbar add cascade -label File -menu .mbar.file

menu .mbar.file
.mbar.file add command -label Run -command run_eclipse
.mbar.file add command -label Exit -command exit


#----------------------------------------------------------------------
# The command to run the Eclipse-goal
#----------------------------------------------------------------------

proc run_eclipse {} {
    ec_post_goal { writeln("hello world"),flush(output) }
    ec_resume
}


#----------------------------------------------------------------------
# Initialise the embedded ECLiPSe
#----------------------------------------------------------------------

ec_init
# allow ECLiPSe engine to shutdown when exiting
bind . <Destroy> {ec_cleanup}
