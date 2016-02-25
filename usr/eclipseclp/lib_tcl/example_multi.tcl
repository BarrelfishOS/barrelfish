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
# 
# Example for Tcl Peer Multitasking. See example_multi.ecl for more
# information
#
#----------------------------------------------------------------------
# Find and load the eclipse package
#----------------------------------------------------------------------
set tkecl(ECLIPSEDIR) $env(ECLIPSEDIR)
lappend auto_path [file join $tkecl(ECLIPSEDIR) lib_tcl]

package require eclipse_peer_multitask
package require remote_eclipse

set argstate flag
set host [info hostname]
set port ""
set return_code continue

foreach arg $argv {
    switch -- $argstate {
	flag {
	    switch -exact -- $arg {
		-h {set argstate host}
		-p {set argstate port}
		default: {error "Unknown flag $arg"}
	    }
	}
	host {
	    set host $arg
	    set argstate flag
	}
	port {
	    set port $arg
	    set argstate flag
	}
    }
}


#----------------------------------------------------------------------
# Registration and deregistration
#----------------------------------------------------------------------

proc register_for_multi {} {
    ec_multi:peer_register [list start multi_start_handler interact multi_interact_handler]
    ;# toggle the .reg button
    .reg configure  -command {deregister_for_multi}
    .reg configure -text "Deregister multitasking"
}

proc deregister_for_multi {} {
    ec_multi:peer_deregister
    ;# toggle the .reg button
    .reg configure  -command {register_for_multi}
    .reg configure -text "Register multitasking"
}


#----------------------------------------------------------------------
# Handlers
#----------------------------------------------------------------------

proc multi_start_handler {type} {
    global return_code

    if {$type == "demo"} {
	set return_code continue
	enable_buttons
    } else {
	disable_buttons
	set return_code no 
    }
    return $return_code
}

proc multi_interact_handler {type} {
    global return_code

    if {$return_code == "terminate"} {
            disable_buttons
    }
    return $return_code
}

proc ec_end {} {
    if {[ec_multi:get_multi_status] != "on"} {
	enable_buttons
    }
}

proc ec_start {} {
    if {[ec_multi:get_multi_status] != "on"} {
	disable_buttons
    }
}

 
#----------------------------------------------------------------------
# Actions for buttons
#----------------------------------------------------------------------

proc end_interaction {} {
    global return_code
    set return_code terminate
    if {[ec_multi:get_multi_status] != "on"} {
	;# we are not multitasking, hand control back to ECLiPSe directly
	ec_resume
    }
}

proc enable_buttons {} {
    .run  configure -state normal
    .end configure -state normal
    .reg configure -state normal
}

proc disable_buttons {} {
    .run  configure -state disabled
    .end configure -state disabled
    .reg configure -state disabled
}


#----------------------------------------------------------------------
# Intialisation
#----------------------------------------------------------------------

pack [button .run -state disabled -command {ec_rpc \
    [list writeln [list - ok [ec_peer_name]]] {((()()))}} -text run]
pack [button .end -state disabled -command {end_interaction} -text "end interaction"]
pack [button .reg -state disabled]


ec_remote_init $host $port 
ec_running_set_commands ec_start ec_end {} disable_buttons
register_for_multi

ec_resume



