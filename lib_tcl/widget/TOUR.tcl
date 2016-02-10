#!/bin/sh
# The next line is executed by /bin/sh, but not tcl \
exec wish $0 ${1+"$@"}

# tour --
#
#	Tour of the megawidget of package ::Widget
#
# Copyright (c) 1997 Jeffrey Hobbs
#

package require Tk
if {![info exists TOUR(LOADED)]} {
    set TOUR(SCRIPT) [info script]
    if {[lsearch -exact $auto_path [file dirname $TOUR(SCRIPT)]]==-1} {
	lappend auto_path [file dirname $TOUR(SCRIPT)]
    }
    if {[string compare unix $tcl_platform(platform)]} {
	# get rid of the console on windows/mac
	catch {rename console winconsole}
    }
    set TOUR(LOADED) 0
}

## AllWidgets will end up including:
# Widget
#   ::Utility
#   ::Utility::dump
#   ::Utility::string
#   ::Utility::number
#   ::Utility::tk
#   ::Utility::expand
# BalloonHelp
# Calculator
# Combobox
# Console
# Hierarchy
# Megalist
# Pane  
# Progressbar
# Tabnotebook
# Ventry
package require AllWidgets

destroy .tab .exit

tabnotebook .tab
button .exit -text "Exit" -command exit

namespace import -force ::Utility::*

grid .tab -sticky news
grid .exit -sticky ew
grid rowconfigure . 0 -weight 1
grid columnconfig . 0 -weight 1

foreach n {
    Main Combobox Console Hierarchy Megalist Progressbar Ventry Script
} {
    set TOUR($n) [frame .tab.[string tolower $n]]
    .tab add $n -window $TOUR($n)
}
.tab activate Main

# get_comments --
#
#   Gets the major comments out of a file.  If not a real filename,
#   assume it is a package name that has a particular ifneeded setup.
#
# Arguments:
#   file	file to get comments out of.
# Results:
#   Returns the related comments.
#
proc get_comments {file} {
    if {![file exists $file]} {
	set version [package provide $file]
	if {[string match {} $version]} { return "## No Comments Found" }
	set loadstr [package ifneeded $file $version]
	if {[string match tclPkgSetup* $loadstr]} {
	    set file [lindex [lindex [lindex $loadstr 4] 0] 0]
	} else {
	    # this expects 8.1 regexps
	    regexp {(\w+.tcl)} $loadstr file
	}
	if {![file exists $file]} {
	    return "## Couldn't determine file for package '$file'"
	}
    }
    set fid [open $file]
    set comments {}
    set last 0
    while {[gets $fid line] != -1} {
	if {[string match "##*" $line]} {
	    append comments $line\n
	    set last 1
	} elseif {$last} {
	    append comments \n
	    set last 0
	}
    }
    return $comments
}


## Main Tab
##
set f $TOUR(Main)
pack [scrolledtext $f.t] -fill both -expand 1
$f.t insert 1.0 "Welcome to the widget tour.

In the above tabs you will find basic examples with small feature
descriptions of the various widgets in this package.

This tour itself uses the Tabnotebook widget as the basic container
for the widget tour.
"

## Combobox Tab
##
set f $TOUR(Combobox)
scrolledtext $f.t -height 5
frame $f.p
pack [combobox $TOUR(Combobox).p.c1 -labeltext "Basic Combobox: "] -fill x
pack [combobox $TOUR(Combobox).p.c2 -width 15 -textvariable myvar \
	-list {{first choice} {second} {another choice} {final choice}}]
pane $f.t $f.p -orient vertical -dynamic yes

$f.t insert 1.0 "Combobox class widget.

The combobox emulates the Tix widget of the same name.

Major comments for class:
[get_comments Combobox]
"

## Console Tab
##
set f $TOUR(Console)
pack [scrolledtext $f.t -height 5] -fill x
pack [console $f.c -height 10] -fill both -expand 1
pane $f.t $f.c -orient vertical

$f.t insert 1.0 "Console class widget.

This is an interactive console for Tcl/Tk derived from TkCon.  It
presents an interactive window into the interpreter, with many
features to assist in interactive debugging.

Major comments for class:
[get_comments Console]
"

## Hierarchy Tab
##
set f $TOUR(Hierarchy)
scrolledtext $f.t -height 5
frame $f.p
set TOUR(Hierarchy,w) [hierarchy_widget $f.p.w -root .]
set TOUR(Hierarchy,d) [hierarchy_dir $f.p.d -root [file dirname [pwd]] \
	-showparent "Parent" -showfiles 1]
pane $f.t $f.p -orient vertical
pane $f.p.w $f.p.d -dynamic 1



## Megalist Tab
##
set f $TOUR(Megalist)
pack [scrolledtext $f.t -height 5] -fill x

$f.t insert 1.0 "Major comments for class Megalist:
[get_comments Megalist]
"

## Progressbar Tab
##
set f $TOUR(Progressbar)
pack [scrolledtext $f.t -height 5] -fill x

$f.t insert 1.0 "Major comments for class Progressbar:
[get_comments Progressbar]
"

## Ventry Tab
##
set f $TOUR(Ventry)
pack [scrolledtext $f.t -height 5] -fill x
pack [ventry $f.v1 -labeltext "Integers:" -labelwidth 14 -validate key \
	-vcmd {regexp {^[-+]?[0-9]*$} %P}] -fill x
pack [ventry $f.v2 -labeltext "Max 8 chars:" -labelwidth 14 -validate key \
	-vcmd {expr {[string length %P]<=8}}] -fill x
pack [ventry $f.v3 -labeltext "Date on focus:" -labelwidth 14 \
	-validate focusout -validatecmd {check_date %W %s} \
	-invalidcmd {warn "Invalid Date specified"}] -fill x

proc check_date {w date} {
    if {[string match {} $date]} { return 1 }
    $w delete 0 end
    set code [validate date $date]
    if {$code} {
	$w insert 0 [clock format [clock scan $date]]
    } else {
	focus -force [$w subwidget entry]
    }
    return $code
}

$f.t insert 1.0 "Ventry class widget.

Major comments for class:
[get_comments Ventry]
"

## Script Tab
##
set f $TOUR(Script)
scrolledtext $f.t -wrap none
button $f.rerun -text "Rerun Buffer" -command rerun
button $f.reload -text "Reload Script" -command {reload 1}

grid $f.t -
grid $f.rerun $f.reload
grid rowconfig $f 0 -weight 1
grid columnconfig $f 0 -weight 1
grid columnconfig $f 1 -weight 1

proc reload {{force 0}} {
    global TOUR
    
    set text $TOUR(Script).t
    $text delete 1.0 end
    if {$force || !$TOUR(LOADED)} {
	if {[catch {open $TOUR(SCRIPT)} fid]} {
	    $text insert 1.0 $fid
	} else {
	    $text insert 1.0 [read $fid]
	    close $fid
	}
    } else {
	$text insert 1.0 $TOUR(BUFFER)
    }
    set TOUR(BUFFER) [$text get 1.0 end-1c]
}

proc rerun {} {
    global TOUR
    ## Get data from text widget
    set TOUR(BUFFER) [$TOUR(Script).t get 1.0 end-1c]
    uplevel \#0 $TOUR(BUFFER)
}

reload

## Balloon Help
##
balloonhelp clear
balloonhelp .exit "The Exit Button"
balloonhelp $TOUR(Hierarchy,w) "Widget hierarchy"
balloonhelp $TOUR(Hierarchy,d) "Directory hierarchy"

set TOUR(LOADED) 1
