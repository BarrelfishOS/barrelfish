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

#  mapremote.tcl:
#  Tcl code for the embedded variant. The bulk of the code here is for the
#  GUI that gets the host and port number for the ECLiPSe side. When these
#  are supplied by the user, the code then connects to the ECLiPSe side and
#  calls map_init

set tkecl(version) 6.2	 ;# update also in eclipse_tools and examples!

switch $tcl_platform(platform) {
    unix {
	set tkecl(ECLIPSEDIR) $env(ECLIPSEDIR)
    }
    windows {
	package require registry
	set tkecl(ECLIPSEDIR) [registry get \
	    HKEY_LOCAL_MACHINE\\SOFTWARE\\IC-Parc\\Eclipse\\$tkecl(version) ECLIPSEDIR]
    }
    default {
	error "$tcl_platform(platform) not supported"
	exit
    }
}

lappend auto_path [file join $tkecl(ECLIPSEDIR) lib_tcl]

package require remote_eclipse 

set map_remote(connected) 0
set map_remote(host) localhost
set map_remote(port) ""

set argstate flag
set host localhost
set port ""

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

ec_running_set_commands {} {} {} shut_down

proc shut_down {} {
    destroy .
}

proc disconnect {} {
    if {![ec_running]} {
	ec_disconnect tcl
    }
    exit
}

proc setup_map {stream {n {}}} {

    ;# factor is used to scale the map to a reasonable size
    ;# in a more sophisticated implementation, this could be calculated
    ;# by the program
    set factor 17
    set stname [ec_streamnum_to_channel $stream]
    .f.c delete all

    set info [ec_read_exdr $stname]
    while {$info != "end"} {
	set country [lindex $info 1]
	set x1 [expr [lindex $info 2] * $factor + 1]
	set y1 [expr [lindex $info 3] * $factor + 1]
	set x2 [expr [lindex $info 4] * $factor - 1]
	set y2 [expr [lindex $info 5] * $factor - 1]

        ;# the tag c$country is used to associate this 
        ;# area with the other areas for the same country
	.f.c create rect $x1 $y1 $x2 $y2 -tag c$country -fill darkgray -outline ""
        .f.c create text [expr ($x1 +$x2)/2] [expr ($y1 + $y2)/2] -text $country
	set info [ec_read_exdr $stname]
    }   
    
}

# queue handler for update_map. Reads the new colour for a country and updates
# the displayed colour. 
proc update_map {stream {n {}}} {
    set stname [ec_streamnum_to_channel $stream]
    set info [ec_read_exdr $stname]

    set country [lindex $info 1]
    set colour  [lindex $info 2]
    ;# the tag c$country is used to update all regions of the country
    .f.c itemconfigure c$country -fill $colour -outline ""
}

proc init_mapdata {{mapfile "map_data.map"}} {
    global mapsize maxmapsize

    set res [ec_rpc [list init_map $mapfile _] (()_)]
    switch $res {
	fail -
	throw {
	    tk_messageBox -type ok -icon error -message "Unable to compile $mapfile as map data..."
	}
	default {
	    set maxmapsize [lindex  $res 2]
	    set mapsize $maxmapsize
	    ec_rpc [list get_map_data $mapsize] (I)
	}
    }

}

# after a map has been successfully coloured, this asks the user if s/he
# wants to continue to colour the map (find alternative solutions).
proc continue_colouring {n} {
    global continue_state solution_count

    .b.more configure -state normal
    .b.done configure -state normal
    tkwait variable continue_state
    incr solution_count
    .b.more configure -state disabled
    .b.done configure -state disabled
    ec_write_exdr [ec_streamnum_to_channel $n] $continue_state ()
    ec_flush $n
    
}

proc map_demo {} {
    frame  .b
    button .b.more -text More -command "set continue_state yes" -state disabled
    button .b.done -text Done -command disconnect -state disabled
    pack .b -side top -fill x -expand 1
    pack .b.more -side left -fill x -expand 1
    pack .b.done -side right -fill x -expand 1

    frame .f

    pack [canvas .f.c -width 23c -height 13c] -expand 1 -fill both 
    pack .f -expand 1 -fill both

    ec_queue_create setup_map   fromec setup_map
    ec_queue_create update_map fromec update_map
    ec_queue_create continue toec  cont_colouring
    init_mapdata "buggy_data.map"
    ec_resume
}

proc cont_colouring {n} {
    global continue_state 

    .b.more configure -state normal
    .b.done configure -state normal
    tkwait variable continue_state
    .b.more configure -state disabled
    .b.done configure -state disabled
    ec_write_exdr [ec_streamnum_to_channel $n] $continue_state ()
    ec_flush $n
    
}


ec_remote_init $host $port map_demo









