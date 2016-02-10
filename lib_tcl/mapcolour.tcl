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

#   mapcolour.tcl
#   Core Tcl side code for mapcolouring example, shared by both
#   the embedded and remote variants

#----------------------------------------------------------------------
# Make a standard menu bar and a 'File' and 'Tools' menu
#----------------------------------------------------------------------

wm title . "Map Colouring"

# is_colouring is a global variable which indicates if the program is currently
# colouring a map: 1 if yes, 0 if no
set  is_colouring 0 

. config -menu .mbar

menu .mbar
.mbar add cascade -label File -menu .mbar.file
.mbar add cascade -label Method -menu .mbar.method
.mbar add cascade -label Tools -menu .mbar.tools

menu .mbar.file
.mbar.file add command -label "New Map..." -command read_new_map
.mbar.file add command -label "Map Size..." -command set_map_size
.mbar.file add command -label Exit -command exit

menu .mbar.method
.mbar.method add cascade -label Solver -menu .mbar.file.solver
.mbar.method add cascade -label "Value Choice" -menu .mbar.file.choice
.mbar.method add cascade -label "Variable Selection" -menu .mbar.file.select

set solver fd
set m2 [menu .mbar.file.solver]
$m2 add radio -label FD -variable solver -value fd
$m2 add radio -label IC -variable solver -value ic
$m2 add radio -label "Delay till ground" -variable solver -value delay
$m2 add radio -label "Prolog (generate & test)" -variable solver -value prolog

set choice indomain
set m2 [menu .mbar.file.choice]
$m2 add radio -label "indomain" -variable choice -value indomain
$m2 add radio -label "indomain_random" -variable choice -value indomain_random
$m2 add radio -label "rotate colours" -variable choice -value rotate

set select input_order
set m2 [menu .mbar.file.select]
$m2 add radio -label "input order" -variable select -value input_order
$m2 add radio -label "first-fail" -variable select  -value first_fail
$m2 add radio -label "occurence" -variable select  -value occurrence
$m2 add radio -label "most constrained" -variable select  -value most_constrained
$m2 add radio -label "antifirst-fail" -variable select  -value anti_first_fail

# select a new map data file and get ECLiPSe side to compile the file
# via an ERPC call to init_map/2. The maximum size for a map using the 
# data file is returned by ECLiPSe and stored in a global variable maxmapsize
proc read_new_map {} {
    global is_colouring

    if {$is_colouring} return   ;# only allow selection if no problem is running

    set file [tk_getOpenFile -defaultextension ".map" -filetypes {{{Map Data} {.map}}} -title "Select a Map" -initialdir [pwd]]
    if {$file != ""} {
	init_mapdata $file
    }
}

# allow the user to set the actual map size to colour with a particular
# map data file. An ERPC call is made to get_map_data/1 when the size is
# selected, and the ECLiPSe side returns the information on the shape and
# position of the countries for a map of this size. This produces the
# initial uncoloured map
proc set_map_size {} {
    global mapsize maxmapsize shadowsize is_colouring

    if {$is_colouring} return  ;# change map size only when not colouring

    if {[winfo exists .size]} {
	wm deiconify .size
	raise .size
    } else {
	toplevel .size
	set shadowsize $mapsize
	pack [scale .size.scale -from 1 -to $maxmapsize -orient hori -tickinterval 25 -length 80m -sliderlength 4m -variable shadowsize] -expand 1 -fill x
	pack [frame .size.f] -side bottom -fill x
	pack [button .size.f.make -text "Create Map" -command {change_mapsize $shadowsize}] -side left -fill x
	pack [button .size.f.exit -text "Dismiss" -command "destroy .size"] -side right -fill x
    }
}

# starts the map colouring process. An ERPC is made to colouring/5
proc run {} {
    global solver select choice mapsize solution_count is_colouring

    set solution_count 0
    .f.c itemconfigure all -fill darkgray
    .b.run configure -state disabled
    set is_colouring 1
    set res [ec_rpc [list colouring $solver $select $choice $mapsize _ _] \
		 (()()()I__)] 
    set is_colouring 0
    set backtracks [lindex $res 5]
    set time [lindex $res 6]
    .m insert end "Execution time for $solver, $select, $choice, size $mapsize: $time sec, $backtracks backtracks for $solution_count solution(s)\n"
    .m see end
    .b.run configure -state normal
}


# queue handler for setup_map. This takes the shape and position information
# sent by the ECLiPSe side and displays it
# n is a dummy argument for compatibility with remote interface
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

# changes the number of countries that are to be coloured (using the same
# map data file), by making an ERPC call to get_map_data/1
proc change_mapsize {size} {
    global mapsize 

    set mapsize $size
    ec_rpc [list get_map_data $mapsize] (I)
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

# initialise the program. Sets up a default map with a default size so that
# the map colouring can be done immediately
proc map_init {} {
    global tkecl

    ec_tools_init .mbar.tools

    frame  .b
    button .b.run -text Run -command run
    button .b.more -text More -command "set continue_state yes" -state disabled
    button .b.done -text Done -command "set continue_state no" -state disabled
    pack .b -side top -fill x -expand 1
    pack .b.run -side left -fill x -expand 1
    pack .b.more -side left -fill x -expand 1
    pack .b.done -side right -fill x -expand 1
    frame .f 
    pack [canvas .f.c -width 23c -height 13c] -expand 1 -fill both 
    pack .f -expand 1 -fill both
    pack [text .m -relief sunken -height 5] -expand 1 -fill both

    ec_queue_create setup_map   fromec setup_map
    ec_queue_create update_map fromec update_map
    ec_queue_create continue toec  continue_colouring

    cd [file join $tkecl(ECLIPSEDIR) lib_tcl]
    ec_rpc [list compile mapcolour] (())
    init_mapdata
}




