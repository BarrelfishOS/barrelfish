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
# Contributor(s):  Kish Shen, IC-Parc
# 
# END LICENSE BLOCK

#
# $Id: tktools.tcl,v 1.5 2013/07/05 01:34:47 jschimpf Exp $
# 
# This file is the front end for the remote development tools
#
# Author:   Kish Shen   11 Aug 2000
#
# Do NOT include any development support here!
# Do NOT assume the existence of an interactive ECLiPSe toplevel!
#
# 
#----------------------------------------------------------------------
# Find and load the eclipse package
#----------------------------------------------------------------------
set tkecl(version) 6.2	 ;# update also in eclipse_tools and examples!

lappend tkecl(helpfiles) tktools "Remote TkTools" tktoolshelp.txt

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

package require eclipse_tools 
package require AllWidgets
package require remote_eclipse 

proc tkecl:freeze_control {} {
    global tkecl

    if {[ec_multi:get_multi_status] == "off"} {
	.f.cont configure -state disabled
	.f.status configure -text "ECLiPSe Active" -fg darkgray -bg lightgray
	while {1} {
	    ;# this is needed to avoid problems when something else has grab -
	    ;# e.g. if a menu is being pulled down from window by user
	    if [catch {grab .f}] {
		continue
	    } else {break}
	}
    }

}

proc tkecl:thaw_control {} {
    if {[ec_multi:get_multi_status] == "off"} {
	.f.status configure -text "TkTools Active" -fg #00b000 -bg beige
	grab release .f
    }
}

proc ecyield_control {} {
    if {[ec_multi:get_multi_status] == "off"} {
	update idletasks  ;# get rid of any events on the disabled button
	.f.cont configure -state normal
    }
}

proc ec_continue {} {
    update 
    ec_resume resume
}

proc detach_tools {} {

    if {![ec_running]} {
	ec_disconnect tcl
	disconnect_control
    }
}

proc disconnect_control {} {

    if [winfo exists .f] {
	.f.cont configure -state disabled
	.f.cont configure -text "Disconnected"
    }
}

proc quit_tools {} {

    detach_tools   ;# will do nothing if already detached
    destroy .
}

#-------------------------------------------------------------------
# Attach
#-------------------------------------------------------------------

proc attach_tools {} {
    global tk_tools

    if {($tk_tools(host) != "" && $tk_tools(port) != "")} {
	if [catch {ec_remote_init $tk_tools(host) $tk_tools(port) "ec_tools_init .mbar.tools"} err] {
	    # exit with error as the initialisation failed (which can then
            # be caugth if tktools is run using exec)
	    puts stderr $err
	    exit -1
	}
	destroy .e
    }
}

set argstate flag
set tk_tools(host) [info hostname] 
set tk_tools(port) ""


. config -menu .mbar
menu .mbar
.mbar add cascade -label File -menu .mbar.file
.mbar add cascade -label Tools -menu .mbar.tools
  menu .mbar.file 
  .mbar.file add command -label Disconnect -command detach_tools
  .mbar.file add command -label Exit -command quit_tools
pack [frame .f] -expand 1 -fill both
pack [label .f.status -width 25 -borderwidth 3 -relief ridge] -expand 1 -fill x -side top 
pack [button .f.cont -text "Resume ECLiPSe" -command ec_continue -relief groove -borderwidth 5] -expand 1 -fill both -padx 10 -pady 10
bind .f.cont <Destroy> "ec_disconnect tcl"

foreach arg $argv {
    switch -- $argstate {
	flag {
	    switch -exact -- $arg {
		-h {set argstate host}
		-p {set argstate port}
		default {puts stderr "Unknown flag $arg"; exit -1}
	    }
	}
	host {
	    set tk_tools(host) $arg
	    set argstate flag
	}
	port {
	    set tk_tools(port) $arg
	    set argstate flag
	}
    }
}

if {$argstate != "flag"} { 
    puts stderr "No argument supplied with option $arg"
    exit -1
}

if {$tk_tools(port) == ""} {

    ;# popup a window to ask for host and port...
    toplevel .e
    wm title .e "Connecting tktools to..."
    pack [message .e.ins -justify center -aspect 350 -padx 10 -pady 10 -relief ridge -borderwidth 2 -text \
  "Specify hostname and port number of ECLiPSe session to connect \
   to (Use attach_tools/0 from lib(remote_tools) in ECLiPSe session)."] \
    -expand true -fill both
    pack [frame .e.host] -expand true -fill both
    pack [label .e.host.l -text "Host:"] -side left
    pack [entry .e.host.e -textvariable tk_tools(host) -relief sunken] -side right
    pack [frame .e.port] -expand true -fill both
    pack [label .e.port.l -text "Port:"] -side left
    pack [ventry .e.port.e -textvariable tk_tools(port) -relief sunken \
       -vcmd {regexp {^[0-9]*$} %P} -validate key -invalidcmd bell ] -side right
    pack [button .e.ok -text "OK" -command attach_tools] -expand true -fill both -side bottom

    wm title . "Detached Development Tools"

    bind .e.port.e <Return> attach_tools
    bind .e.port.e <Destroy> {if {![ec_connected]} exit}

    wm withdraw .
    tkwait window .e
    wm deiconify .
} else {

    ;# host and port already specified...
    attach_tools
}
    
ec_running_set_commands tkecl:freeze_control tkecl:thaw_control ecyield_control disconnect_control



.mbar add cascade -label "Help" -menu .mbar.help -underline 0
menu .mbar.help
     .mbar.help add check -label "Balloon Help" -variable tkecl(pref,balloonhelp) 
     .mbar.help add separator

foreach {key topic filename} $tkecl(helpfiles) {
    .mbar.help add command -label $topic -command "tkecl:Get_helpfileinfo $key {}"
}

update 

ec_resume resume  


