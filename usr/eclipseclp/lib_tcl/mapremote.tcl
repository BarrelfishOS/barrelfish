#  mapremote.tcl:
#  Tcl code for the embedded variant. The bulk of the code here is for the
#  GUI that gets the host and port number for the ECLiPSe side. When these
#  are supplied by the user, the code then connects to the ECLiPSe side and
#  calls map_init

switch $tcl_platform(platform) {
    unix {
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

set lib_tcl_path [file join $tkecl(ECLIPSEDIR) lib_tcl]
lappend auto_path $lib_tcl_path

package require eclipse_tools 
package require remote_eclipse 

proc attach_remote_ec {} {
    global map_remote

    if {($map_remote(host) != "" && $map_remote(port) != "")} {
	ec_remote_init $map_remote(host) $map_remote(port) map_init
	set map_remote(connected) 1
	destroy .e
    }
}


proc quit {} {

    if {![ec_running]} {
	ec_disconnect tcl
    }

    destroy .
}

set map_remote(connected) 0
set map_remote(host) localhost
set map_remote(port) ""

source [file join $lib_tcl_path "mapcolour.tcl"]

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
	    set map_remote(host) $arg
	    set argstate flag
	}
	port {
	    set map_remote(port) $arg
	    set argstate flag
	}
    }
}

if {$map_remote(port) == ""} {

    ;# popup a window to ask for host and port...
    toplevel .e
    wm title .e "Connecting to..."
    pack [message .e.ins -justify center -aspect 350 -padx 10 -pady 10 -relief ridge -borderwidth 2 -text \
  "Specify hostname and port number of ECLiPSe session to connect \
   to (Use remote_connect/3)"] \
    -expand true -fill both
    pack [frame .e.host] -expand true -fill both
    pack [label .e.host.l -text "Host:"] -side left
    pack [entry .e.host.e -textvariable map_remote(host) -relief sunken] -side right
    pack [frame .e.port] -expand true -fill both
    pack [label .e.port.l -text "Port:"] -side left
    pack [entry .e.port.e -textvariable map_remote(port) -relief sunken] -side right
    pack [button .e.ok -text "OK" -command attach_remote_ec] -expand true -fill both -side bottom

    wm title . "Map Colouring Demo (Remote Tcl Interface)"

    bind .e.port.e <Return> attach_remote_ec
    bind .e.port.e <Destroy> {if {!$map_remote(connected)} exit}

    wm withdraw .
    tkwait window .e
    wm deiconify .
}

