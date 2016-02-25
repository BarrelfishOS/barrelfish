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
# $Id: tkec_remote.tcl,v 1.1 2006/09/23 01:54:19 snovello Exp $
# 
# This file contains some primitive procedures that are needed to
# connect ECLiPSe to Tcl via the remote interface
#
# Do NOT include any development support here!
# Do NOT assume the existence of an interactive ECLiPSe toplevel!
#

package provide remote_eclipse 1.0

# remote protocol version. Must correspond to the ECLiPSe version;
# which is accessed via get_flag(remote_protocol_version, V)
set ec_remote(ec_remote_version) {remote_protocol 1}

set ec_remote(ec_running) 1
set ec_remote(ec_connected) 0
set ec_remote(interaction_nesting) 0
set ec_remote(ec_running_start_command) {}
set ec_remote(ec_yield_command) {}
set ec_remote(ec_running_end_command) {}
set ec_remote(ec_disconnect_command) {}

proc ec_running_set_commands {{startcmd {}} {endcmd {}} {yieldcmd {}} {disconcmd {}}} {
    global ec_remote

    set ec_remote(ec_running_start_command) $startcmd
    set ec_remote(ec_running_end_command) $endcmd
    set ec_remote(ec_yield_command) $yieldcmd
    set ec_remote(ec_disconnect_command) $disconcmd
}

proc ec_running {} {
    global ec_remote

    return $ec_remote(ec_running)
}

proc ec_connected {} {
    global ec_remote

    return $ec_remote(ec_connected)
}

proc ec_resume {{type resume} {format ()}} {
    global ec_remote 

    if {[ec_running]} {
	error "Calling ec_resume while ECLiPSe side has control"
    }

    while {1} {
	set ec_remote(ec_running) 1
	if {$ec_remote(ec_running_start_command) != {}} {
	    eval $ec_remote(ec_running_start_command)
	}
	ec_write_exdr $ec_remote(ec_rpc_control_channel) $type $format
	flush $ec_remote(ec_rpc_control_channel)  ;# this hands over to ECLiPSe

	incr ec_remote(interaction_nesting) 1
	if [catch {wait_ecyield $type} return] {
	    ;# if error occurs, unilaterally disconnect
	    ec_disconnect_tcl_side
	    set return disconnect
	}
	incr ec_remote(interaction_nesting) -1

	;# execute yield command if at outer level and not disconnected
        if {($ec_remote(ec_yield_command) != {} && \
	     $ec_remote(interaction_nesting) == 0 && $return != "disconnect")} {
	    eval $ec_remote(ec_yield_command)
	}
	if {$return != "resume"} {
	    break  ;# break out of resume 
	} else {
	    set type resume
	    set format ()
	}
    }
    return $return
}


proc wait_ecyield {type}  {
    global ec_remote

    update idletasks ;# update windows before handing over
    if [catch {ec_read_exdr $ec_remote(ec_rpc_control_channel)} reslist] {
	;# cannot read from control channel, assume connection lost...
	;# Tk bug: the tk_messageBox freezes the whole program!
#	tk_messageBox -icon error -type ok -message "Control connection to ECLiPSe lost"
	error "connection to eclipse side lost"
    }

    set ec_remote(ec_running) 0 
    if {$ec_remote(ec_running_end_command) != {}} {
	eval $ec_remote(ec_running_end_command)
    }

    set res [lindex $reslist 0]
    switch $res {
	ec_flushio {
	    set stream [lindex $reslist 1]
	    set length [lindex $reslist 2]
	    if [catch "ec_flushio_stream $stream $length" err] {
		tk_messageBox -icon error -type ok -message $err
	    }

	    update
	    if {$type == "rpc"} {
		return [ec_resume resume]
	    } else {
		return resume
	    }
	}
	ec_waitio {
	    set stream [lindex $reslist 1]
	    if [catch "ec_waitio_stream $stream" err] {
		tk_messageBox -icon error -type ok -message $err
	    }

	    update
	    if {$type == "rpc"} {
		return [ec_resume resume]
	    } else {
		return resume
	    }
	}
	yield {
	    return yield
	}
	socket_client {
	    set port [lindex $reslist 1]
	    set eclipse_name [lindex $reslist 2]
	    set queue_type [lindex $reslist 3]
	    set access [lindex $reslist 4]
	    if {$queue_type == "sync"} {
		ec_sync_queue_connect $port $eclipse_name $access 
	    } else {
	        ec_async_queue_connect $port $eclipse_name 
	    }
	    return resume
	}
	socket_accept {
	    if {[lindex $type 0] == "socket_connect"} {
		;# must be a reply to a resume on socket_connect
		set eclipse_name [lindex $reslist 1]
		set nr [lindex $reslist 2]
		if {[lindex $type 1] == $eclipse_name} {
		    ;# eclipse queue name must be the same
		    if {$nr == "fail"} {
			if [info exists ec_remote(name_channel,$eclipse_name)] {
			    close $ec_remote(name_channel,$eclipse_name)
			    unset $ec_remote(name_channel,$eclipse_name)
			    
			}
			tk_messageBox -icon error -type ok -message "Unable to establish queue connection $eclipse_name with host ECLiPSe"

		    } else {
			;# set the book-keeping info
			set socketchannel $ec_remote(name_channel,$eclipse_name)

			set ec_remote(nr_name,$nr) $eclipse_name
			set ec_remote(channel_nr,$socketchannel) $nr
		    }
		    return socket_accept
		}
                ;# error has occurred
		error "Unexpected control messages received during queue creation"
	    }
	    return socket_accept
	}
	queue_close {
	    set nr [lindex $reslist 1]
	    close_queue_tcl_side $nr
	    return resume
	}
	    
	disconnect {
	    ec_disconnect eclipse
	    return disconnect
	}
	disconnect_yield {
	    ;# perform disconnection on Tcl side
	    ec_disconnect_tcl_side
	    return disconnect
	}
	default {
	    error "Unexpected return from ec_resume: $reslist"
	}
    }
}




#----------------------------------------------------------------------
# Handling ECLiPSe queues
#----------------------------------------------------------------------

proc ec_queue_create {eclipse_name access {command {}} {event {}}} {
    global ec_queue_out_handlers ec_queue_in_handlers
    global ec_remote  

    if {(($command != {}) && ($event != {}))} {
	error "Cannot define handlers on both ECLiPSe and Tcl sides for a synchronous queue $eclipse_name"
    }

    switch -- $access {
	fromec -
	r {
	    set access fromec ;# ec_mode is the mode on ECLiPSe side
	}

	toec -
	w {
	    set access toec
	}

	default {error "$access is an invalid access mode for remote ECLiPSe synchrnous queue ($eclipse_name)"}
    }

    if [info exists ec_remote(name_channel,$eclipse_name)] {
	error "Queue name $eclipse_name already in use (ec_queue_create)"
    }

    ec_resume [list queue_create $eclipse_name sync $access [list $event]] {(()()()())}
    if [info exists ec_remote(name_channel,$eclipse_name)] {
	if {$command != ""} { 
	    ec_set_queue_handler $eclipse_name $access $command
	}
	;# return the channel name as per embedded interface
	return $ec_remote(name_channel,$eclipse_name)
    } else {
	;# something went wrong; queue not created
	error "Unable to create queue $eclipse_name in ec_queue_create"
    }
}

proc ec_queue_close {eclipse_name} {
    global ec_remote

    if [catch "set nr [ec_streamname_to_streamnum $eclipse_name]"] {
	error "No such ECLiPSe queue $eclipse_name in ec_queue_close"
    } else {
	ec_resume [list queue_close $nr] {(I)}
	close_queue_tcl_side $nr
    }

}

proc close_queue_tcl_side {nr} {
    global ec_remote

    if [info exists ec_remote(nr_name,$nr)] {
	set name $ec_remote(nr_name,$nr)
	unset ec_remote(nr_name,$nr)
	set channel $ec_remote(name_channel,$name)
	unset ec_remote(name_channel,$name)
	unset ec_remote(channel_nr,$channel)
	catch {close $channel}
    }
}
	
proc ec_sync_queue_connect {port eclipse_name access} {
    global ec_remote

    set try 1
    while {1} {
	if [catch "socket $ec_remote(host) $port" socketchannel] {
	    incr try
	    if {$try > 10} {
		set status fail
		set socketchannel fail
		break
	    }
	} else {
	    set status success
	    set ec_remote(name_channel,$eclipse_name) $socketchannel

	    switch $access {
		fromec {
		    fconfigure $socketchannel -translation binary -blocking 1
		}
		toec   {
		    fconfigure $socketchannel -translation binary -blocking 0
		}
	    }
	    break
	}
    }

    ec_resume [list socket_connect $eclipse_name $status] {(()())}
    return socketchannel
}

proc ec_async_queue_create {eclipse_name {access {}} {rcommand {}} {wevent {}}} {
    global ec_remote

    if {$access == "r"} {
	set access fromec
    } elseif {$access == "w"} {
	set access toec
    }

    if [info exists ec_remote(name_channel,$eclipse_name)] {
	error "Queue name $eclipse_name already in use (ec_async_queue_create)"
    }

    ec_resume [list queue_create $eclipse_name async $access [list $wevent]] {(()()()())}
    if [info exists ec_remote(name_channel,$eclipse_name)] {
	ec_set_queue_handler $eclipse_name r $rcommand
	return $ec_remote(name_channel,$eclipse_name)
    } else {
	;# something went wrong; queue not created
	error "Unable to create queue $eclipse_name in ec_async_queue_create"
    }
}

proc ec_async_queue_connect {port eclipse_name} {
    global ec_remote

    set try 1
    while {1} {
	if [catch "socket $ec_remote(host) $port" socketchannel] {
	    incr try
	    if {$try > 10} {
		set status fail
		set socketchannel fail
		break
	    }
	} else {
	    set status success
	    set ec_remote(name_channel,$eclipse_name) $socketchannel

	    fconfigure $socketchannel -blocking 0 -translation binary
	    break
	}
    }

    ec_resume [list socket_connect $eclipse_name $status] {(()())}
    set ec_remote(async,$eclipse_name) 1  ;# is a asynchron. stream
    return $socketchannel
}


proc ec_async_io {stream command} {
    set socketchannel [ec_streamnum_to_channel $stream]
    if [eof $socketchannel] {
	catch {close $socketchannel}
	tk_messageBox -icon error -type ok -message "Connection for remote queue $stream to ECLiPSe lost"
	return
    }
    eval $command $stream
}

# mapping from ECLiPSe stream name to Tcl channel name
proc ec_streamname_to_channel {eclipse_name} {
    global ec_remote

    if [info exists ec_remote(name_channel,$eclipse_name)] {
	return $ec_remote(name_channel,$eclipse_name)
    } else {
	error "No such ECLiPSe stream (ec_streamname_to_channel $eclipse_name)"
    }
}

# mapping from ECLiPSe physical stream number to Tcl channel name
proc ec_streamnum_to_channel {nr} {
    global ec_remote

    if [info exists ec_remote(nr_name,$nr)] {
	return $ec_remote(name_channel,$ec_remote(nr_name,$nr))
    } else {
	error "No such ECLiPSe stream (ec_streamnum_to_channel $nr)"
    }
}

# mapping from Tcl channel name to ECLiPSe stream number
proc ec_channel_to_streamnum {channel} {
    global ec_remote

    if [info exists ec_remote(channel_nr,$channel)] {
	return $ec_remote(channel_nr,$channel)
    } else {
	error "No such ECLiPSe stream (ec_channel_to_streamnum $channel)"
    }
}

# mapping from ECLiPSe stream name to physical number
proc ec_streamname_to_streamnum {eclipse_name} {
    global ec_remote

    if [info exists ec_remote(name_channel,$eclipse_name)] {
	return $ec_remote(channel_nr,$ec_remote(name_channel,$eclipse_name))
    } else {
	error "No such ECLiPSe stream (ec_stream_nr $eclipse_name)"
    }
}

proc ec_stream_nr {eclipse_name} {
    ec_streamname_to_streamnum $eclipse_name
}



proc ec_write_exdr {channel data {format S}} {
    puts -nonewline $channel [ec_tcl2exdr $data $format]
}

proc ec_flush {nr {len {}}} {
    global ec_remote

    if {![ec_running]} {
	set channel [ec_streamnum_to_channel $nr]
	flush $channel ;# non-blocking; may be buffered
	if {$len != {}} {
	    ec_resume [list rem_flushio $nr $len] {(II)}
	} else { 
	    ec_resume [list rem_flushio $nr] {(I)}
	}
    } else {
	error "Cannot perform an ec_flush while ECLiPSe is active."
    }
}

proc ec_set_queue_handler {eclipse_name access command} {
    global ec_queue_out_handlers ec_queue_in_handlers
    global ec_remote

    if [info exists ec_remote(async,$eclipse_name)] {
	;# async stream
	switch -- $access {
	    fromec -
	    r {
		if {$command != {}} {
		    set channel [ec_streamname_to_channel $eclipse_name]
		    set nr [ec_streamname_to_streamnum $eclipse_name]
		    fileevent $channel readable "eval ec_async_io $nr $command "
		}
	    }

	    toec -
	    w {
		if {$command != {}} {
		    error "Cannot specify a write handler with asynchronous queues"
		}
	    }

	    default { error "ec_set_queue_handler: bad access mode, should be r" }
	}

    } else {
	;# non-async stream
	switch -- $access {
	    fromec -
	    r  { 
		set ec_queue_out_handlers([ec_stream_nr $eclipse_name]) $command 
	    }

	    toec   -
	    w  { 
		set ec_queue_in_handlers([ec_stream_nr $eclipse_name]) $command 
	    }
	    default { error "ec_set_queue_handler: bad access mode, should be r or w" }
	}
    }
}


proc ec_flushio_stream {stream length} {
    global ec_queue_out_handlers ec_socketstream_r

    set channel [ec_streamnum_to_channel $stream]
    if [eof $channel] {
	catch {close $channel}
	tk_messageBox -icon error -type ok -message "Connection for remote queue $stream to ECLiPSe lost"
	return
    }
    if [info exists ec_queue_out_handlers($stream)] {
	eval $ec_queue_out_handlers($stream) $stream $length
    } else {
	ec_stream_output_popup "Output occurred on ECLiPSe stream $stream" $stream $length
    }
}

proc ec_waitio_stream {stream} {
    global ec_queue_in_handlers

    set channel [ec_streamnum_to_channel $stream]
    if [eof $channel] {
	catch {close $channel}
	tk_messageBox -icon error -type ok -message "Connection for remote queue $stream lost"
	return
    }

    if [info exists ec_queue_in_handlers($stream)] {
	eval $ec_queue_in_handlers($stream) $stream
    } else {
	ec_stream_input_popup "Input expected on ECLiPSe stream $stream" $stream
    }
}

proc ec_queue_write {eclipse_name data} {
    puts -nonewline [ec_streamname_to_channel $eclipse_name] $data
}

proc ec_queue_read {eclipse_name size} {
    read [ec_streamname_to_channel $eclipse_name] $size
}

#---------------------------------------------------------------------
# Disconnect
#---------------------------------------------------------------------

# disconnect from ECLiPSe. side is the side that initiated
proc ec_disconnect {{side tcl}} {
    global ec_remote 

    switch -- $side {
	tcl {
	    if {[ec_connected]} {
		# only need to disconnect if still connected.
		if {![ec_running]} {
		    if {[ec_resume disconnect] == "disconnect"} {
			;# disconnect status returned only when disconnect on Tcl
			;# side has occurred
			return
		    } else { 
			;# something is wrong... disconnect on Tcl side anyway
			tk_messageBox -icon error -type ok -message "Unexpected response from ELiPSe to disconnect request.\n ECLiPSe side may not have disconnected properly."
			ec_disconnect_tcl_side
		    }
		
		} else {
		    ;# eclipse running currently, so disconnect cannot be done
		    ;# in both directions at the moment. Disconnect at Tcl end
		    ;# only
		    ec_write_exdr $ec_remote(ec_rpc_control_channel) disconnect_resume ()
		    flush $ec_remote(ec_rpc_control_channel)
		    ec_disconnect_tcl_side
		}
	    }
		
	}

	eclipse {
	    ec_write_exdr $ec_remote(ec_rpc_control_channel) disconnect_resume ()
	    flush $ec_remote(ec_rpc_control_channel)
	    ec_disconnect_tcl_side

	}
    }

}

#------------------------------------------------------------------
# Disconnect actions on Tcl side
#------------------------------------------------------------------
proc ec_disconnect_tcl_side {} {
    global ec_remote 

    catch {close $ec_remote(ec_rpc_in_channel)}
    set ec_remote(ec_rpc_in_channel) {}
    set ec_remote(ec_rpc_out_channel) {}

    catch {close $ec_remote(ec_rpc_control_channel)}
    set ec_remote(ec_rpc_control_channel) {}

    foreach streamindex [array names ec_remote name_channel,* ] {
	close_queue_tcl_side [ec_channel_to_streamnum $ec_remote($streamindex)]
    }

    set ec_remote(ec_running) 1
    set ec_remote(ec_connected) 0
    if {$ec_remote(ec_disconnect_command) != {}} {
	eval $ec_remote(ec_disconnect_command)
    }
}

#----------------------------------------------------------------------
# Sample stream I/O handlers
#----------------------------------------------------------------------

set ec_stream_input_string {}

proc ec_stream_input_popup {Msg Stream} {
    global ec_stream_input_string

    toplevel .ec_stream_input_box
    label .ec_stream_input_box.prompt  -width 40 -text $Msg
    entry .ec_stream_input_box.input -bg white -width 40 -textvariable ec_stream_input_string
    button .ec_stream_input_box.clear -text "clear" -command {.ec_stream_input_box.input delete 0 end}
    button .ec_stream_input_box.ok -text "ok" -command {destroy .ec_stream_input_box}
    bind .ec_stream_input_box.input <Return> {destroy .ec_stream_input_box}

    pack .ec_stream_input_box.prompt -side top -fill x
    pack .ec_stream_input_box.input -side top -fill x
    pack .ec_stream_input_box.clear -side left -expand 1 -fill x
    pack .ec_stream_input_box.ok -side left -expand 1 -fill x

    focus .ec_stream_input_box.input
    tkwait window .ec_stream_input_box
    puts -nonewline [ec_streamnum_to_channel $Stream] $ec_stream_input_string
    ec_flush $Stream [string length $ec_stream_input_string]
}


# Sample queue_out_handler: output into text widget

proc ec_stream_to_window_sync {Tag Window Stream Length} {

    set channel [ec_streamnum_to_channel $Stream]
    if [eof $channel] {
	catch {close $socketchannel}
	tk_messageBox -icon error -type ok -message "Connection for remote queue $Stream to ECLiPSe lost"
	return
    }

    set data [read $channel $Length]

    $Window insert end $data $Tag
    $Window see end
}


proc ec_stream_to_window {Tag Window Stream} {

    set channel [ec_streamnum_to_channel $Stream]
    if [eof $channel] {
	catch {close $socketchannel}
	tk_messageBox -icon error -type ok -message "Connection for remote queue $Stream to ECLiPSe lost"
	return
    }

    set data [read $channel 2000]

    while {$data != ""} {
	regexp {^([0-9]+)[.]([0-9]+)$} [$Window index end-1char] whole line charp
	if {$charp < 2000} {
	    ;# always truncate
	    $Window insert end $data $Tag
	} else {
	    ;# truncate printing of line if too long
	    if {[lsearch [$Window tag names] trunc] != -1} {
		;# not yet defined...
		$Window tag configure trunc -background pink
	    }
	    if {[lsearch [$Window tag names end-2char] trunc] == -1} {
		    ;# line is first truncated. Note -2 needed (rather than -1)
		$Window insert end "..." trunc
	    }
	    set nl [string first "\n" $data]
	    if {$nl != -1} {
		;# if there is a nl, then a new line was started
		$Window insert end [string range $data $nl end] $Tag
	    }
	}
	update idletasks
	set data [read $channel 1000]
    }
    
    $Window see end
}

# Sample queue_out_handler: output into message popup

proc ec_stream_output_popup {Msg Stream length} {
    if ![winfo exists .ec_stream_output_box] {
	toplevel .ec_stream_output_box
	label .ec_stream_output_box.msg  -width 40 -text $Msg
	text .ec_stream_output_box.text -width 40 -height 5 -bg white -yscrollcommand ".ec_stream_output_box.vscroll set" -wrap none -xscrollcommand ".ec_stream_output_box.hscroll set"
	scrollbar .ec_stream_output_box.vscroll -command ".ec_stream_output_box.text yview"
	scrollbar .ec_stream_output_box.hscroll -command ".ec_stream_output_box.text xview" -orient horizontal
	button .ec_stream_output_box.ok -text "ok" -command {destroy .ec_stream_output_box}
	pack .ec_stream_output_box.msg -side top -fill x
	pack .ec_stream_output_box.ok -side bottom -fill x
	pack .ec_stream_output_box.vscroll -side left -fill y
	pack .ec_stream_output_box.hscroll -side bottom -fill x
	pack .ec_stream_output_box.text -expand 1 -fill both
    }
    ec_stream_to_window_sync {} .ec_stream_output_box.text $Stream $length
}




#----------------------------------------------------------------------
# ec_rpc goal ?format?
#	returns: instantiated goal, "fail" or "throw"
#----------------------------------------------------------------------


proc ec_rpc {Goal {Format S}} {
    global ec_remote

    if {[ec_running]} {
	error "Cannot perform an rpc while ECLiPSe is active or disconnected"
    }
    ec_write_exdr $ec_remote(ec_rpc_out_channel) $Goal $Format
    flush $ec_remote(ec_rpc_out_channel)

    set return [ec_resume rpc] ;# hand over to ECLiPSe for rpc
    if {$return != "disconnect"} {
	ec_read_exdr $ec_remote(ec_rpc_in_channel)
    }
}


#----------------------------------------------------------------------
# Load the parts of the interface which are implemented in C:
#
# ec_read_exdr
# ec_tcl2exdr
# ec_exdr2tcl
#----------------------------------------------------------------------

set eclipsedir [file dirname [file dirname [info script]]]
source [file join $eclipsedir lib_tcl eclipse_arch.tcl]
load [file join $eclipsedir lib [ec_arch] tkexdr[info sharedlibextension]]


# obsolete, for compatibility only
proc ec_control_name {} {
    return [ec_control_name]
}

#----------------------------------------------------------------------
# Init
#----------------------------------------------------------------------

proc ec_remote_init {host port {init {}} {pass {}} {format S}} {

    global ec_remote 

    set ec_remote(ec_running) 0
    set ec_remote(host) $host
    set ec_remote(port) $port 
    set ec_remote(ec_rpc_control_channel) [socket $ec_remote(host) $ec_remote(port)]
    fconfigure $ec_remote(ec_rpc_control_channel) -blocking 1 -translation binary
    ec_write_exdr $ec_remote(ec_rpc_control_channel) $ec_remote(ec_remote_version) (I)
    flush $ec_remote(ec_rpc_control_channel)
    set version_response [ec_read_exdr $ec_remote(ec_rpc_control_channel)]
    if {$version_response != "yes"} {
	close $ec_remote(ec_rpc_control_channel)
	error "Incompatible remote versions. Expect $ec_remote(ec_remote_version), got $version_response"
    }
    ec_write_exdr $ec_remote(ec_rpc_control_channel) $pass $format
    flush $ec_remote(ec_rpc_control_channel)
    set ec_remote(control_stream) [ec_read_exdr $ec_remote(ec_rpc_control_channel)] 
    ;# get ECLiPSe name of rpc control stream
    ec_write_exdr $ec_remote(ec_rpc_control_channel) tcl
    flush $ec_remote(ec_rpc_control_channel)
    set ec_remote(ec_rpc_in_channel) [socket $ec_remote(host) $ec_remote(port)]
    fconfigure $ec_remote(ec_rpc_in_channel) -blocking 1 -translation binary
    set ec_remote(ec_rpc_out_channel) $ec_remote(ec_rpc_in_channel)
    if {[ec_read_exdr $ec_remote(ec_rpc_in_channel)] != $ec_remote(control_stream)} {
	ec_disconnect
    } else {
	set ec_remote(ec_connected) 1
	if {$init != {}} {
	    eval $init
	}
    }
#    ec_resume resume
}

# cope with Tcl side root window dying properly
wm protocol . WM_DELETE_WINDOW "ec_disconnect tcl; exit"

#------------------------------------------------------------------------
# interface type + info
#------------------------------------------------------------------------

proc ec_interface_type {} {
    return remote
}

proc ec_peer_name {} {
    global ec_remote

    if [info exists ec_remote(control_stream)] {
	return $ec_remote(control_stream)
    } else {
	error "Connection with ECLiPSe not yet established."
    }

}




