#
# $Id: eclipse.tcl,v 1.3 2012/02/19 17:54:49 jschimpf Exp $
# 
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
# This file contains some primitive procedures that are needed to
# embed ECLiPSe into Tcl applications.
#
# Do NOT include any development support here!
# Do NOT assume the existence of an interactive ECLiPSe toplevel!
#

package provide eclipse 1.0


#----------------------------------------------------------------------
# ec_resume ?async?
#	resume ECLiPSe execution and execute posted goals and events
#	returns success,fail,yield (or raises an error)
#	if async, ECLiPSe is run in a separate thread
#----------------------------------------------------------------------

set ec_resume_active 0

proc ec_resume {{async 0}} {
    global ec_resume_active

    if $ec_resume_active { error "ec_resume cannot be nested" }
    set $ec_resume_active 1

    while { 1 } {
	set reslist [ec_resume_ $async]
	if $async {
	    # wait for the eclipse thread to finish
	    # but allow gui interaction every 100 milliseconds
	    while 1 {
		set reslist [ec_resume_status 100]
		if {$reslist != "running"} break
		catch update
	    }
	}
	set res [lindex $reslist 0]
	switch $res {
	    flushio {
		set stream [lindex $reslist 1]
		if [catch "ec_flushio_stream $stream" err] {
		    if [catch  "tk_messageBox -icon error -type ok -message" $err"] {
			return
		    }

		}
		catch update
	    }
	    waitio {
		set stream [lindex $reslist 1]
		if [catch "ec_waitio_stream $stream" err] {
		    if [catch "tk_messageBox -icon error -type ok -message" $err"] {
			# unable to display messageBox, tk process gone
			return
		    }

		}
	    }
	    success -
	    fail -
	    yield {
		set $ec_resume_active 0
		return $res
	    }
	    running {
		error "Cannot do ec_resume while another ec_resume is running"
	    }
	    default {
		set $ec_resume_active 0
		error "Unexpected return from ec_resume: $reslist"
	    }
	}
    }
}

#----------------------------------------------------------------------
# ec_flush StreamNum ?length?
#       remote interface compatible use of ec_resume
#----------------------------------------------------------------------

proc ec_flush {StreamNr {length {}}} {
    flush [ec_streamnum_to_channel $StreamNr]
    ec_rpc true
}

#----------------------------------------------------------------------
# ec_handle_events
#	restricted form of ec_resume
#	execute only events (e.g. queue events, posted events)
#	returns success (or raises an error)
#----------------------------------------------------------------------

proc ec_handle_events {} {

    set reslist [ec_handle_events_]
    while { 1 } {
	set res [lindex $reslist 0]
	switch $res {
	    flushio {
		set stream [lindex $reslist 1]
		if [catch "ec_flushio_stream $stream" err] {
		    if [catch "tk_messageBox -icon error -type ok -message $err"] {
			# unable to display messageBox, tk process gone
			return 
		    }
		}
		catch update
	    }
	    waitio {
		set stream [lindex $reslist 1]
		if [catch "ec_waitio_stream $stream" err] {
		    if [catch "tk_messageBox -icon error -type ok -message $err"] {
			return 
		    }
		}
	    }
	    success {
		return $res
	    }
	    running {
		error "Cannot do ec_handle_events while ec_resume running"
	    }
	    default {
		error "Unexpected return from ec_resume: $reslist"
	    }
	}
	# now resume the Eclipse handler
	set reslist [ec_resume_ 0]
    }
}

#----------------------------------------------------------------------
# Handling sockets
#----------------------------------------------------------------------

proc ec_open_socket {host port} {
    global ec_socket

    set ec_socket [socket $host $port]
}

#----------------------------------------------------------------------
# Handling ECLiPSe queues
#----------------------------------------------------------------------

proc ec_queue_connect {eclipse_name access {command {}}} {
    global ec_queue_out_handlers
    global ec_queue_in_handlers

    set channelid [ec_queue_open_ $eclipse_name $access]
    if {$command != ""} { ec_set_queue_handler $eclipse_name $access $command }
    return $channelid
}

proc ec_queue_create {eclipse_name access {command {}} {event {}}} {
    global ec_queue_out_handlers
    global ec_queue_in_handlers

    if {(($command != {}) && ($event != {}))} {
	error "Cannot define handlers on both ECLiPSe and Tcl sides for a queue $eclipse_name"
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

	default {error "$access is an invalid access mode for embedded ECLiPSe queue ($eclipse_name)"}
    }

    ec_rpc [list : sepia_kernel [list ecl_create_embed_queue $eclipse_name $access [list $event]]] (()(()()()))

    return [ec_queue_connect $eclipse_name $access $command]

}
    

# mostly for compatibility with socket remote queues
proc ec_async_queue_create {eclipse_name access {command {}} {event {}}} {

    ec_queue_create $eclipse_name $access $command $event
}

proc ec_queue_close {eclipse_name} {
    ec_rpc [list peer_queue_close $eclipse_name] (())
}

proc ec_write_exdr {channel data {format S}} {
    puts -nonewline $channel [ec_tcl2exdr $data $format]
}

proc ec_set_queue_handler {eclipse_name access command} {
    global ec_queue_out_handlers
    global ec_queue_in_handlers

    switch -- $access {
	fromec  -
	r { 
	    set ec_queue_out_handlers([ec_stream_nr $eclipse_name]) $command 
	}
	toec    -
	w { 
	    set ec_queue_in_handlers([ec_stream_nr $eclipse_name]) $command 
	}

	default { 
	    error "ec_set_queue_handler: bad access mode, should be r or w" 
	}
    }
}

proc ec_flushio_stream {stream} {
    global ec_queue_out_handlers
    if [info exists ec_queue_out_handlers($stream)] {
	eval $ec_queue_out_handlers($stream) $stream
    } else {
	ec_stream_output_popup "Output occurred on ECLiPSe stream $stream" $stream
    }
}

proc ec_waitio_stream {stream} {
    global ec_queue_in_handlers
    if [info exists ec_queue_in_handlers($stream)] {
	eval $ec_queue_in_handlers($stream) $stream
    } else {
	ec_stream_input_popup "Input expected on ECLiPSe stream $stream" $stream
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
    bind .ec_stream_input_box.input <Return> {append ec_stream_input_string \n ; destroy .ec_stream_input_box}

    pack .ec_stream_input_box.prompt -side top -fill x
    pack .ec_stream_input_box.input -side top -fill x
    pack .ec_stream_input_box.clear -side left -expand 1 -fill x
    pack .ec_stream_input_box.ok -side left -expand 1 -fill x

    focus .ec_stream_input_box.input
    tkwait window .ec_stream_input_box 
    ec_queue_write $Stream $ec_stream_input_string
    set ec_stream_input_string [string trimright $ec_stream_input_string \n]
}


# Sample queue_out_handler: output into text widget

# Length is optional dummy arg. for compatibility with socket queues
proc ec_stream_to_window_sync {Tag Window Stream {Length {}}} {
    ec_stream_to_window $Tag $Window $Stream
}

proc ec_stream_to_window {Tag Window Stream} {

    set data [ec_queue_read $Stream 1000]
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
	set data [ec_queue_read $Stream 1000]
    }
    
    $Window see end
}

# Sample queue_out_handler: output into message popup

proc ec_stream_output_popup {Msg Stream} {
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
    ec_stream_to_window {} .ec_stream_output_box.text $Stream
}

#----------------------------------------------------------------------
# Handler for embed_info queue
#----------------------------------------------------------------------

proc ec_info_queue_handler {InfoStream} {
    global ec_embed_info_channel

    set message [ec_read_exdr [ec_streamnum_to_channel $InfoStream]]
    set command [lindex $message 0]
    switch $command {
	queue_connect {
	    set eclipse_name [lindex $message 1]
	    set access [lindex $message 3]
	    ec_queue_connect $eclipse_name $access
	}

	queue_close {
	    close [ec_streamnum_to_channel [lindex $message 1]]
	}

	default { error "Unrecognised message $message from embedded ECLiPSe."}
    }
}

#----------------------------------------------------------------------
# Init
#    ec_init ?Name?
#----------------------------------------------------------------------

proc ec_init {{name host}} {
    global tkecl
    global ec_rpc_in_channel
    global ec_rpc_out_channel
    global ec_embed_info_channel

    set res [ec_init_]
    ;# ec_rpc channels are treated specially as the rpc connections is not
    ;# yet formed at this point 
    set ec_rpc_in_channel [ec_queue_open_ ec_rpc_in w]
    set ec_rpc_out_channel [ec_queue_open_ ec_rpc_out r]

    if {[lindex [ec_rpc \
          [list : sepia_kernel [list set_embed_peer $name tcl]] (()(()S))
         ] 0] == "fail"} {
	error "Peer name $name already in use."
    }
    ;# embed_info must be created after embed peer info has been set
    set ec_embed_info_channel [ec_queue_create embed_info fromec ec_info_queue_handler]
    set tkecl(ec_peer_name) $name

    return $res
}

#----------------------------------------------------------------------
# ec_rpc goal ?format?
#	returns: instantiated goal, "fail" or "throw"
#----------------------------------------------------------------------

proc ec_rpc {Goal {Format S}} {
    global ec_rpc_in_channel
    global ec_rpc_out_channel

    if [ec_running] {
	error "Cannot do ec_handle_events while ec_resume running"
    }
    ec_write_exdr $ec_rpc_in_channel $Goal $Format
    ec_handle_events
    catch "ec_read_exdr $ec_rpc_out_channel" res
    return $res
}

#----------------------------------------------------------------------
#

proc ec_streamnum_to_channel {nr} {
    return ec_queue$nr
}

proc ec_streamname_to_channel {eclipse_name} {
    return [ec_streamnum_to_channel [ec_stream_nr $eclipse_name]]
}

proc ec_streamname_to_streamnum {eclipse_name} {
    return [ec_stream_nr $eclipse_name]
}

proc ec_channel_to_streamnum {channel} {
    if {![regexp {^ec_queue([0-9]+)$} $channel cname nr]} {
	error "$channel is not a valid channel name for a ECLiPSe-Tcl queue."
    }
    return $nr
}

#----------------------------------------------------------------------
# interface type + info
#----------------------------------------------------------------------

proc ec_interface_type {} {
    return embedded
}

proc ec_peer_name {} {
    global tkecl

    if [info exists tkecl(ec_peer_name)] {
	return $tkecl(ec_peer_name)
    } else {
	error "ECLiPSe side not yet initialised."
    }
}

#----------------------------------------------------------------------
# Load the parts of the interface which are implemented in C:
#
# ec_init_
# ec_cleanup
# ec_set_option
# ec_post_goal
# ec_post_event
# ec_resume_
# ec_running
# ec_handle_events_
# ec_queue_write
# ec_queue_read
# ec_stream_nr
# ec_queue_open_
# ec_read_exdr
# ec_tcl2exdr
# ec_exdr2tcl
#
# CAUTION: ECLIPSEDIR is derived from the location of this Tcl file!
# Before loading tkeclipse.so we cd to the right directory in order
# to be able to find the dependencies without LD_LIBRARY_PATH.
#----------------------------------------------------------------------

set eclipsedir [file dirname [file dirname [info script]]]
source [file join $eclipsedir lib_tcl eclipse_arch.tcl]

set prev [pwd]
cd [file join $eclipsedir lib [ec_arch]]
if { [catch {
	load [file join . tkexdr[info sharedlibextension]]
	load [file join . tkeclipse[info sharedlibextension]]
    } error]
 } {
    cd $prev
    error "Problem loading the ECLiPSe shared libraries: $error"
}
cd $prev


#----------------------------------------------------------------------
# Set defaults
# The user can change these before calling ec_init
#----------------------------------------------------------------------

# use queues for stdin/stdout/stderr and connect them to popups for now
ec_set_option io 2
set ec_queue_in_handlers(0) "ec_stream_input_popup {Input expected on ECLiPSe input stream:}"
set ec_queue_out_handlers(1) "ec_stream_output_popup {Output occurred on ECLiPSe output stream:}"
set ec_queue_out_handlers(2) "ec_stream_output_popup {Output occurred on ECLiPSe error stream:}"
return ok

