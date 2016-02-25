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
# Copyright (C) 1999 - 2006 Cisco Systems, Inc.  All Rights Reserved.
# 
# Contributor(s): 
# 
# END LICENSE BLOCK
#
# ECLiPSe Development Tools in Tcl
#
#
# $Id: eclipse_tools.tcl,v 1.43 2015/01/14 01:31:10 jschimpf Exp $
#
# Code in this file must only rely on primitives in eclipse.tcl.
# Don't assume these tools to be embedded into a particular
# application (like the tkeclipse toplevel)!
#
# All tools in this package has .ec_tools as the root frame. New
# tools should be added under .ec_tools, and the code should be
# placed after the creation and initialisation of the widget defaults

#----------------------------------------------------------------------
# Find and load the eclipse package
# Also determines font preferences
#----------------------------------------------------------------------

package provide eclipse_tools 1.0

set tkecl(version) 6.2 ;# update also in tkeclipse and examples!
# including mapdebugdemo.tcl in <ECLiPSe>/document/tutorial/mapdebugdemo.tcl


switch $tcl_platform(platform) {
    unix {
	set tkecl(ECLIPSEDIR) $env(ECLIPSEDIR)
	lappend tkecl(preferences) \
		{monofont_family   fixed       font     tkeclipsetoolsrc \
                  "Font used for monospaced font (Tk font family)"} \
		{monofont_size     ""          fontsize tkeclipsetoolsrc \
                  "Font size used for monospace font in points (+ integer)" } \
		{labelfont_family  helvetica   font     tkeclipsetoolsrc \
                  "Font used for labels (Tk font family)"} \
		{labelfont_size    ""          fontsize tkeclipsetoolsrc \
                  "Font size used for labels in points (+ integer)" } 
    }
    windows {
	# For Windows 64 bit, the 64 bit version of Tcl must be run to
	# access the correct (i.e. 64 bit) set of registry entries!
	package require registry
	set tkecl(ECLIPSEDIR) [registry get \
	    HKEY_LOCAL_MACHINE\\SOFTWARE\\IC-Parc\\Eclipse\\$tkecl(version) ECLIPSEDIR]
	# fixed does not alias to a mono-spaced font in Windows!
	set tkecl(windows_registry_path) HKEY_CURRENT_USER\\Software\\IC-Parc\\ECLiPSe\\
	lappend tkecl(preferences) \
		{monofont_family   courier       font     tkeclipsetoolsrc \
                 "Font used for monospaced font (Tk font family)"} \
		{monofont_size     8            fontsize tkeclipsetoolsrc \
                 "Font size used for monospace font in points (+ integer)" } \
		{labelfont_family  helvetica     font     tkeclipsetoolsrc \
                 "Font used for labels (Tk font family)"} \
		{labelfont_size    8            fontsize tkeclipsetoolsrc \
                 "Font size used for labels in points (+ integer)"}
    }
    default {
	error "$tcl_platform(platform) not supported"
	exit
    }
}

lappend auto_path [file join $tkecl(ECLIPSEDIR) lib_tcl]


#----------------------------------------------------------------------
# Setup the defaults for preferences and set them to the defaults
# Note fonts preferences have already been set
#----------------------------------------------------------------------

set tkecl(pref,editor) ""
if [info exists env(VISUAL)] { set tkecl(pref,editor) $env(VISUAL) }
if {$tkecl(pref,editor) == ""} { 
    if [catch {set pf $env(PROGRAMFILES)}] { set pf "C:\\Program Files" }
    if [file exists "$pf\\Windows NT\\Accessories\\wordpad.exe"] {
	set tkecl(pref,editor) "$pf\\Windows NT\\Accessories\\wordpad.exe"
    } elseif [file exists "$pf\\Accessories\\wordpad.exe"] {
	set tkecl(pref,editor) "$pf\\Accessories\\wordpad.exe"
    }
}

switch -glob $tkecl(pref,editor) {
    *emacs -
    *emacs.* -
    *vile {
	set tkecl(pref,edit_line_option) "+"
    }
    *notepad++ {
	set tkecl(pref,edit_line_option) "-n"
    }
    default {
	set tkecl(pref,edit_line_option) ""
    }
}

# the preferences are defined in tkecl(preferences), which is a list of the
# preferences and information on them. To add a preference, append the 
# following list of information for the perference to the the variable:
#     {<name>  <default value>  <type>  <family> <description>}
#
#  <name>              Name of the preference parameter.
#  <default value>     The system default value for the parameter.
#  <type>              Type of the parameter. This will determine how the
#                      initialisation routines and preference editor will
#                      handle the parameter.
#  <family>            The family the parameter belongs to. Currently 
#                      either tkeclipsetoolsrc or tkeclipserc. The 
#                      preference values for the family will be stored in
#                      a file named .<family> in Unix, or with <family>
#                      being the last path of the registry path.
#  <description>       This is the description that will be displayed 
#                      with the parameter in the editor
#
#  A corresponding variable tkecl(pref,<name>) will be created for each
#  parameter in the development tools, storing its current value. The 
#  variable need to be created for the other families.

lappend tkecl(preferences) \
	{background_colour ""  colour tkeclipsetoolsrc \
	   "Default background colour for widgets (colour)" }  \
        {defaultextension   .ecl   string          tkeclipsetoolsrc \
           "Default extension for file browser (string)"}   \
	{stats_interval     1      stats_interval  tkeclipsetoolsrc \
           "Interval for updating statistics tool (+ float)"}  \
	{text_truncate      2000   +integer        tkeclipsetoolsrc \
           "Threshold length for truncation of text lines (+ int)" } \
	{tracer_prdepth     5      tracer_prdepth  tkeclipsetoolsrc \
           "Print depth used by tracer tool (+ int)"} \
	{balloonhelp        1      boolean         tkeclipsetoolsrc \
           "Balloon help"}  \
	{trace_source   1      boolean         tkeclipsetoolsrc \
	     "Show source while tracing"}  \
	{trace_refresh_dg   1      boolean         tkeclipsetoolsrc \
           "Refresh delayed goals display at every trace line"}  \
	{trace_refresh_stack   0      boolean         tkeclipsetoolsrc \
           "Refresh tracer stack display at every trace line"}  \
	{trace_raise_tracer   1      boolean         tkeclipsetoolsrc \
           "Raise tracer window at every trace line"}  \
	{dgf_spiedonly      0      boolean         tkeclipsetoolsrc \
           "Show spied goals in delayed goals tool"}  \
        {dgf_tracedonly     1      boolean         tkeclipsetoolsrc \
           "Show traced goals in delayed goals tool"}  \
        {dgf_wakeonly       0      boolean         tkeclipsetoolsrc \
           "Show scheduled goals in delayed goals tool"}  \
	{inspect_prdepth    5      +integer        tkeclipsetoolsrc \
           "Print depth for inspector tool"} \
	{inspect_ldepth    20      +integer        tkeclipsetoolsrc \
           "List depth for inspector tool"} \
	{inspect_nosymbols  1      boolean         tkeclipsetoolsrc \
           "Display symbols for inspector tool"} \
        [list editor  $tkecl(pref,editor)  string  tkeclipsetoolsrc {Text editor to use (command)}] \
	[list edit_line_option $tkecl(pref,edit_line_option) string tkeclipsetoolsrc \
	      "Editor's command line option to start at a specific line"]

# use procedure to avoid creating extra global variables
proc tkecl:set_initial_prefs {} {
    global tkecl

    foreach preference $tkecl(preferences) {
	foreach {option default type family help} $preference {
		set tkecl(pref,$option) $default
	}
    }
}

tkecl:set_initial_prefs

#----------------------------------------------------------------------
# Load packages and initialise global settings
#----------------------------------------------------------------------

package require AllWidgets
package require tkinspect
package require eclipse_peer_multitask

balloonhelp enable .
balloonhelp delay 1000

# other global variables

set tkecl(last_source_file) {}

set tkecl(filetypes) {
	{{ECLiPSe Files} {.ecl .pl}}
	{{ECLiPSe specific Files} {.ecl}}
	{{Prolog Files} {.pl}}
	{{ECLiPSe Precompiled Files} {.eco}}
	{{All Files} {*}}
}

#--------------------------------------------
# setting tk-based preferences/defaults
#-------------------------------------------

# don't set size, use default instead; tk seems to have a bug with size 12
# fonts are created here; their settings can be changed later to the user
# defaults
font create tkeclmono -family $tkecl(pref,monofont_family) 
font create tkeclmonobold -family $tkecl(pref,monofont_family) -weight bold
font create tkecllabel -family $tkecl(pref,labelfont_family) -weight bold 

if ![regexp "^\[ \t]*$" $tkecl(pref,background_colour)] {
    tk_setPalette background $tkecl(pref,background_colour)
}

# this sets the Tk defaults for widgets that has $root as a parent. This 
# should be called before any widgets of root are created!
proc tkecl:set_tkecl_tkdefaults {root} {
    option add *$root*font tkecllabel userDefault ;# the default
    option add *$root*Text.font tkeclmono
    option add *$root*Entry.font tkeclmono
    option add *$root*Hierarchy.font tkeclmono
    option add *$root*Text.font tkeclmono
}

tkecl:set_tkecl_tkdefaults ec_tools

frame .ec_tools  ;# dummy toplevel frame for all eclipse tools

#----------------------------------------------------------------------
# Testing code
#----------------------------------------------------------------------

proc tkecl:test {} {
    ec_rpcq_check {exit_block abort} (())
}

proc tkecl:rpc {} {
    global tkecl

    set ec_rpc .ec_tools.ec_rpc
    if ![winfo exists $ec_rpc] {
	toplevel $ec_rpc
	wm title $ec_rpc "ECLiPSe Simple Query"
	pack [label $ec_rpc.entrylabel -justify left -text "Enter a goal in ECLiPSe syntax:"] -fill x
	pack [entry $ec_rpc.entry -bg white -textvariable tkecl(rpc_goal)] \
		-fill x
	pack [label $ec_rpc.textlabel -text "Reply:"] -fill x
	pack [text $ec_rpc.text -bg white -height 8] -expand 1 -fill both
	bind $ec_rpc.entry <Return> tkecl:run_rpc
	button $ec_rpc.run -text "Run (once)" -command tkecl:run_rpc
	button $ec_rpc.close -text Close -command "destroy $ec_rpc"
	pack $ec_rpc.run $ec_rpc.close -side left -expand 1 -fill x
	focus $ec_rpc.entry
	balloonhelp $ec_rpc.run "Execute an ECLiPSe goal once at a new break level."
	bind $ec_rpc <Alt-h> "tkecl:Get_helpfileinfo rpc $ec_rpc"
    } else {
	tkinspect:RaiseWindow $ec_rpc
    }
}

proc tkecl:run_rpc {} {
    global tkecl
    .ec_tools.ec_rpc.text insert end [ec_rpc $tkecl(rpc_goal)]
    .ec_tools.ec_rpc.text insert end "\n"
    .ec_tools.ec_rpc.text see end
}

proc ec_rpc_check {goal {format S}} {
    set result [ec_rpc $goal $format]
    switch $result {
	fail {
	    tk_messageBox -type ok -message "ECLiPSe goal failed: $goal"
	}
	throw {
	    tk_messageBox -type ok -message "ECLiPSe goal aborted: $goal"
	}
    }
    return $result
}

# Call a module-qualified (default:eclipse_language) predicate.
# Return fail, throw, or module-less goal term on success.
proc ec_rpcq {goal exdr_type {module eclipse_language}} {
#    .tkecl.pane.stdio.tout insert end "qcall $goal\n"
    set result [ec_rpc [list : $module $goal] (()$exdr_type)] 
#    .tkecl.pane.stdio.tout insert end "qexit $result\n"
    update
    switch $result {
	fail -
	throw {return $result}
    }
    lindex $result 2
}

# Like ec_rpcq, but message on fail/throw
proc ec_rpcq_check {goal exdr_type {module eclipse_language}} {
#    .tkecl.pane.stdio.tout insert end "ccall $goal\n"
    set result [ec_rpc [list : $module $goal] (()$exdr_type)] 
#    .tkecl.pane.stdio.tout insert end "cexit $result\n"
    update
    switch $result {
	fail {
	    tk_messageBox -type ok -message "ECLiPSe goal failed: $goal"
	    return $result
	}
	throw {
	    tk_messageBox -type ok -message "ECLiPSe goal aborted: $goal"
	    return $result
	}
    }
    lindex $result 2
}

# Call a goal with given context-module (and optional lookup-module)
# Return fail, throw, or module-less goal term on success.
# We call lm:(lm:goal@cm) because @/2 may not be visible (ISO).
proc ec_rpcatq {goal exdr_type at_module {module eclipse_language} } {
#    .tkecl.pane.stdio.tout insert end "atqcall $goal\n"
    set result [ec_rpc [list : $module [list @ [list : $module $goal] $at_module]]\
    		(()((()$exdr_type)())) ] 
#    .tkecl.pane.stdio.tout insert end "atqexit $result\n"
    switch $result {
	fail -
	throw {return $result}
    }
    lindex $result 2 1 2
}


#----------------------------------------------------------------------
# Library browser and help tool
#----------------------------------------------------------------------

proc tkecl:library_browser {} {
    global tkecl

    set lb .ec_tools.ec_libbrowse
    set tkecl(lbloadtext) "No library selected"
    set tkecl(lbmodule) ""
    if ![winfo exists $lb] {
	toplevel $lb
	ec_rpcq init_library_info () tracer_tcl
	set htmldoc [lindex [ec_rpcq {return_html_root _} (_) tracer_tcl] 1]
	wm title $lb "ECLiPSe Library Browser and Help"

	set htmlinfo [text $lb.ref -relief groove  -borderwidth 3 -height 3 ] 

	bind $htmlinfo <Any-Key> "tkecl:readonly_keypress %A"
	bind $htmlinfo <ButtonRelease-2> {break} ;# disable paste
	$htmlinfo tag configure highlight  -justify center -font tkecllabel
	$htmlinfo insert end "To obtain more information on ECLiPSe, point your browser at:\n$htmldoc" highlight

	set close [button $lb.close -text "Close" -command "destroy $lb"]

	set top [frame $lb.top -width 700 -height 500]
	  set treeframe [frame $top.tframe]

            set tree [hierarchy $treeframe.tree -browsecmd tkecl:lb_getchildren \
	       -nodelook tkecl:lbnode_look -expand 2 -selectmode single \
	       -selectcmd tkecl:lbnode_info \
	       -background white -selectbackground gray -root top \
	       -paddepth 20 -padstack 3]

	   set loadsel [button $treeframe.load -textvariable tkecl(lbloadtext) \
                 -state disabled -command "tkecl:lb_load_module $tree"]

	  set tf [frame $top.tf]
	    set tlabel [label $tf.label -justify left -text \
               "Type in a string to match, or predicate_name/arity:"]

	    set tinput [entry $tf.input -bg white -width 86 \
               -textvariable tkecl(help_input)]

	    set t [text $tf.t -setgrid true -relief sunken \
               -background white -width 86 \
               -yscrollcommand "$tf.y set" -xscrollcommand "$tf.x set"]

	    bind $tinput <Return> "tkecl:display_help $tinput $t"

	    bind $t <Any-Key> "tkecl:readonly_keypress %A"   ;# read only
	    bind $t <ButtonRelease-2> {break} ;# disable paste
	    bind $t <Button-1> "tkecl:lb_insert_input $tinput $t"
	    bind $t <Double-Button-1> "tkecl:display_help $tinput $t; break"

	    $t configure -cursor left_ptr
	    $t tag configure highlight -foreground blue -wrap none
	    $t tag configure normal -lmargin2 0 -wrap none
 	    $t tag configure heading -underline 1 -spacing1 5 -spacing3 5


	pack $close -side bottom -fill x -expand true
	pack $htmlinfo -side bottom -fill x -expand true
	pack $top -side top -fill both -expand true 
          pane $treeframe $tf -orient horizontal -initfrac [list 0.4 0.6]
	    pack $loadsel -side top -fill x
	    pack $tree -side bottom -expand 1 -fill both

	    pack $tlabel -side top -fill x
	    pack $tinput -side top -fill x
	    scrollbar $tf.y -orient vert -command "$t yview"
	    pack $tf.y -side right -fill y
	    scrollbar $tf.x -orient hori -command "$t xview"
	    pack $tf.x -side bottom -fill x
	    pack $t -side right -fill both -expand true

	 ;#pack $treeframe -expand true -fill both -side left 
	 ;#pack $tf -expand true -fill both -side right

	bind $lb <Alt-h> "tkecl:Get_helpfileinfo help $lb"
	focus $tinput


	balloonhelp $t "Help Information Window: displays description of ECLiPSe libraries or predicates\n selected from either the tree display or the entry window.\nSelect item from tree display to obtain short description here,\n or type in entry window for longer description of predicates.\nLeft click on any word to put it in entry\nDouble left-click to look word up directly"
	balloonhelp $tinput "Entry window: enter a string to match built-in predicates, or Name/Arity for exact match."
	balloonhelp $tree "Hierarchical tree display of available libraries and their exported interface.\nLibraries in blue are currently loaded, green are unloaded libraries.\n Left-click to select an item; Double left-click to expand and item;\n select an expanded item to display more information in information window."
	balloonhelp $loadsel "This shows the currently selected library (if any) of the tree display.\nClick the load button to load the library."
	balloonhelp $htmlinfo "On-line webpages of the ECLiPSe manual should be available at this URL.\nCopy it to a browser to view."
	balloonhelp $close "Close this window."

    } else {
	tkinspect:RaiseWindow $lb
    }
}


proc tkecl:lb_insert_input {tinput t} {
    $tinput delete 0 end
    $tinput insert end [$t get "current wordstart" "current wordend"]
}

proc tkecl:lb_load_module {tree} {
    global tkecl

    if {$tkecl(lbmodule) != ""} {
	ec_rpcq_check [list lbnode_loadmodule $tkecl(lbmodule)] (()) tracer_tcl
	$tree refresh
    }
}

proc tkecl:lb_getchildren {tree path} {
    return [lindex [ec_rpcq\
    		[list expand_lbnode $path _] {([S*]_)} tracer_tcl] 2]
    
}

proc tkecl:lbnode_look {tree path isopen} {
    foreach {pred in nodetext highlight isopen} \
    	[ec_rpcq [list lbnode_display $path _ _] {([S*]__)} tracer_tcl] {
          switch -exact -- $highlight {
	      highlight {
		  set colour #00b000
	      }
	      current {
		  set colour blue
	      }
	      none {
		  set colour black
	      }
	  }
      }
    return [list $nodetext {} {} $colour]
}

proc tkecl:lbnode_info {t selected prevsel} {
    global tkecl

    set lb .ec_tools.ec_libbrowse 
    $t centreitem $selected 0.1 0.9 0.0 1.0
    set path [lindex [$t get $selected] 0]
    set isopen [$t isopen $path]
    foreach {infoitems tkecl(lbmodule)} [lrange \
      [ec_rpcq [list lbnode_info $path $isopen _ _] {([S*]I__)} tracer_tcl]\
      3 4] {break}
     if {$tkecl(lbmodule) != ""} {
	 set toplevel [lindex [ec_rpcq {get_flag toplevel_module _} (()_)] 2]
	 set tkecl(lbloadtext) "load $tkecl(lbmodule) library into  module $toplevel"
	 $lb.top.tframe.load configure -state normal
     } else {
	 set tkecl(lbloadtext) "No library selected"
	 $lb.top.tframe.load configure -state disabled
     }

    $lb.top.tf.t tag remove highlight 1.0 end

    foreach item $infoitems {

	foreach {format text} $item {
	    break
	}
        $lb.top.tf.t insert end $text [list $format highlight]
	$lb.top.tf.t insert end "\n"
    }
    if {$infoitems != ""} {
    ;# only insert newline if there are some infoitems
	$lb.top.tf.t insert end "\n"
	$lb.top.tf.t see end
    }
}

proc tkecl:display_help {input text} {
    global tkecl
    $input selection range 0 end
    $text tag remove highlight 1.0 end
    $text configure -cursor watch ; update idletasks
    $text insert end [lindex [ec_rpcq\
	 [list gui_help_string $tkecl(help_input) _] (S_) tracer_tcl] 2]\
	highlight
    $text see end
    $text configure -cursor left_ptr

}


#----------------------------------------------------------------------
# Predicate properties window
#----------------------------------------------------------------------

set tkecl(predproppred) ""
set tkecl(predpropmodule) ""

proc tkecl:combo_add_modules {w} {
    foreach item [tkecl:list_modules] {
	$w add $item
    }
}

proc tkecl:list_modules {} {
    # use string because of shared variable
    # fullstop at end in case we are in strict_iso context
    lindex [ec_rpc_check {eclipse_language:setof(X,eclipse_language:current_module(X),L).}] 2 3
}

proc tkecl:popup_pred_prop {} {
    global tkecl

    set predprop .ec_tools.predprop
    if ![winfo exists $predprop] {
	toplevel $predprop
	wm title $predprop "ECLiPSe Predicate Browser"

	set tkecl(predpropwhich) defined
	set tkecl(predpropauxfilter) 1
	frame $predprop.f1 -relief raised -bd 1
	combobox $predprop.which -click single \
		-list {defined exported imported local visible} \
		-listheight 5 \
		-labeltext "Predicates " \
		-textvariable tkecl(predpropwhich) -editable 0 \
		-command tkecl:display_predicates 
	pack $predprop.which -in $predprop.f1 -side left -expand 1 -fill x

	pack [checkbutton $predprop.filter -text "filter aux." \
	         -variable tkecl(predpropauxfilter) \
		 -command {tkecl:display_predicates dummy} \
	     ] -in $predprop.f1 -side right  -expand 1 -fill x

	set modules [tkecl:list_modules]
	combobox $predprop.modules -list $modules -click single \
		-labeltext " in module: " \
		-listheight 6  \
		-textvariable tkecl(predpropmodule) -editable 0 \
		-command tkecl:display_predicates
	pack $predprop.modules -in $predprop.f1 -side left -expand 1 -fill x

	listbox $predprop.preds -width 20 \
		-yscrollcommand "$predprop.vscroll set"
	scrollbar $predprop.vscroll -command "$predprop.preds yview"
	bind $predprop.preds <<ListboxSelect>> {+tkecl:display_predprops .ec_tools.predprop.preds}

	bind $predprop.preds <Enter> "tkecl:listbox_search_init $predprop.preds"
	bind $predprop.preds <Leave> "tkecl:listbox_search_exit $predprop.preds"
	bind $predprop.preds <Control-KeyPress> {continue}
	bind $predprop.preds <Control-Key-s> "tkecl:listbox_search $predprop.preds %A Control_S %X %Y"
	bind $predprop.preds <KeyPress> "tkecl:listbox_search $predprop.preds %A %K %X %Y"
	
	button $predprop.close -text Close -command "destroy $predprop"

	frame $predprop.f2 -relief groove -bd 1
	pack [label $predprop.predlabel -text "Properties of Predicate:"] -in $predprop.f2 -side top -fill x
	pack [label $predprop.predname -relief sunken] -in $predprop.f2 -side top -fill x
	tkecl:add_rb $predprop.f2 disabled auxiliary {off on}
	tkecl:add_rb $predprop.f2 disabled defined {off on}
	tkecl:add_rb $predprop.f2 disabled debugged {off on}
	tkecl:add_rb $predprop.f2 disabled stability {static dynamic}
	tkecl:add_rb $predprop.f2 disabled call_type {prolog external}
	tkecl:add_rb $predprop.f2 disabled type {built_in user}
	tkecl:add_rb $predprop.f2 disabled tool {off on}
#	tkecl:add_rb $predprop.f2 disabled visibility {local imported exported global}
	tkecl:add_rb $predprop.f2 disabled demon {off on}
	tkecl:add_rb $predprop.f2 disabled parallel {off on}
#	tkecl:add_rb $predprop.f2 disabled statistics {off on}
	tkecl:add_rb $predprop.f2 active leash {stop notrace}
	tkecl:add_rb $predprop.f2 active skip {off on}
	tkecl:add_rb $predprop.f2 active start_tracing {off on}
	tkecl:add_rb $predprop.f2 active spy {off on}

	button $predprop.f2.show -text "Show source" -command tkecl:display_source
	pack $predprop.f2.show -side top -fill x

	pack $predprop.f1 -side top -expand 1 -fill x
	pack $predprop.preds -side left -expand 1 -fill both
	pack $predprop.vscroll -side left -fill y
	pack $predprop.f2 -side top -expand 1 -fill x -padx 3 -pady 3 -ipadx 3 -ipady 3
	pack $predprop.close -side top -fill x

        balloonhelp $predprop.preds "Predicates list - select one to view its \
	    properties\n (see manual for details on properties)\n\
	    Typing in this window will search for matching predicate.\n\
	    Type escape to stop search, or Control-S to find next."
        balloonhelp $predprop.which "Type of predicates listed in predicates list.\n\
	    click arrow on right to change type"
        balloonhelp $predprop.modules "Module of predicates listed in predicates list.\n\
	    click arrow on right to change module"
        balloonhelp $predprop.predname "Name, operator and mode information for predicate if known"
	bind $predprop <Alt-h> "tkecl:Get_helpfileinfo pred $predprop"
        tkecl:display_predicates dummy
    } else {
	tkinspect:RaiseWindow $predprop
    }

}

proc tkecl:display_predicates {dummy} {
    global tkecl

    set predprop .ec_tools.predprop
    $predprop.preds delete 0 end
    set preds [lindex [ec_rpcq_check [list \
	    list_predicates $tkecl(predpropwhich) $tkecl(predpropmodule) $tkecl(predpropauxfilter) _] \
            (()()I_) tracer_tcl] 4]
    foreach item $preds {
	$predprop.preds insert end $item
    }
}

proc tkecl:add_rb {parent state name values} {
    global tkecl
#    frame $parent.$name -relief groove -bd 1
    frame $parent.$name
    label $parent.$name.label -text $name -anchor w -width 20
    pack $parent.$name.label -side left
    foreach val $values {
	radiobutton $parent.$name.$val -text $val -variable tkecl(pp_$name) \
		-value $val -anchor w -state $state -command "tkecl:update_predprop $name"
	pack $parent.$name.$val -side left
    }
    pack $parent.$name -side top -fill x
}

proc tkecl:update_predprop {name} {
    global tkecl
    if {$tkecl(predproppred) != ""} {
	;# only update if a predicate has been selected...
	tkecl:set_pred_flag $tkecl(predproppred) $tkecl(predpropmodule) $name $tkecl(pp_$name)
    }
}

proc tkecl:display_predprops {w} {
    global tkecl

    set selected [$w curselection]
    if ![string match "" $selected] { 
	set tkecl(predproppred) [$w get $selected]
    }
    set home [tkecl:pred_flag_value $tkecl(predproppred) $tkecl(predpropmodule) definition_module]
    set mode [tkecl:pred_flag_value $tkecl(predproppred) $tkecl(predpropmodule) mode]
    .ec_tools.predprop.predname configure -text "$home : $mode"
    foreach name {auxiliary call_type debugged defined leash \
	    skip spy stability tool type demon parallel statistics start_tracing} {
	set tkecl(pp_$name) [tkecl:pred_flag_value $tkecl(predproppred) $tkecl(predpropmodule) $name]
    }
    if [winfo exists .ec_source] {
	tkecl:display_source
    }
}

proc tkecl:pred_flag_value {pred module name} {
    set result [ec_rpcq \
    	[list flag_value $pred $name $module _] (S()()_) tracer_tcl]
    # rpc can fail, return "" in that case
    lindex $result 4
}

proc tkecl:set_pred_flag {pred module name value} {
    ec_rpcq [list set_flag_string $pred $name $value $module] (S()()()) tracer_tcl
}


#----------------------------------------------------------------------
# Predicate source window
#----------------------------------------------------------------------

proc tkecl:display_source {} {
    global tkecl

    if {$tkecl(predproppred) == ""} return

    set res [ec_rpcq [list get_source_info $tkecl(predproppred) $tkecl(predpropmodule) _ _] (S()__) tracer_tcl]
    switch $res {
	throw -
	fail {
	    if [winfo exists .ec_tools.ec_tracer] {
		set parent .ec_tools.ec_tracer
	    } else {
		set parent .
	    }
	    tk_messageBox -type ok -parent $parent -icon info -message "No source information found for $tkecl(predproppred) in module $tkecl(predpropmodule)." 
	    return
	}
	default {
	    set file [lindex [lindex $res 3] 0]	;# atom type (singleton list)
	    set offset [lindex $res 4]
	}
    }

    tkecl:popup_tracer
    if {$tkecl(source_debug,file) != $file} {
	if {[tkecl:load_source_debug_file $file] == 0} {
	    tk_messageBox -type ok -parent .ec_tools.ec_tracer -icon info -message "Can't load source file $file"
	    return
	}
    }

    set ec_tracer .ec_tools.ec_tracer.tab
    $ec_tracer activate "Source Context"
    incr offset   ;# increment to get pass newline normally at end of last item
    set idx [$ec_tracer.source.context.text index "1.0 + $offset chars"] 
    $ec_tracer.source.context.text see $idx
}


proc tkecl:set_and_display_source {pred module} {
    global tkecl
    set tkecl(predproppred) $pred
    set tkecl(predpropmodule) $module
    tkecl:display_source
}

proc tkecl:display_source_for_callport {t} {
    global tkecl

    if {$tkecl(source_debug,file) == ""} return
    set line [tkecl:get_current_text_line $t]
    # Caution: the predicate expects an atom. For quoting-sensitive arguments
    # like file names, we have to pass a 1-element list with the () type.
    set res [ec_rpcq [list find_exact_callinfo [list $tkecl(source_debug,file)] $line _] (()I_) tracer_tcl]

    switch $res {
	throw -
	fail {
	    # no port at line, no action
	    return
	}
	default {
	    set callport [lindex $res 3]
	}
    }
    set calldefmodule [lindex $callport 1]
    set callspec [lindex $callport 2]
    # need to convert spec to a string as that is expected
    # no modle needed for call as only need '/'/2 to be defined normally
    set predspecs [lindex [ec_rpcq \
	[list term_string $callspec _] ((()I)_)] 2]
    tkecl:set_and_display_source $predspecs $calldefmodule
}

#----------------------------------------------------------------------
# Global settings window
#----------------------------------------------------------------------

proc tkecl:popup_global_state {} {
    global tkecl

    set gstate .ec_tools.gstate
    if ![winfo exists $gstate] {
	toplevel $gstate
	wm withdraw $gstate
	wm title $gstate "ECLiPSe Global Settings"
	
	tkecl:add_radiobutton $gstate after_event_timer "real virtual"
	tkecl:add_radiobutton $gstate breal_exceptions "off on"
	tkecl:add_radiobutton $gstate coroutine "off on"
	tkecl:add_radiobutton $gstate debugging "nodebug creep leap"
	tkecl:add_radiobutton $gstate debug_compile "off on"
	tkecl:add_radiobutton $gstate enable_interrupts "off on"
	tkecl:add_radiobutton $gstate gc "off on verbose"
	tkecl:add_radiobutton $gstate gc_policy "adaptive fixed"
	tkecl:add_radiobutton $gstate goal_expansion "off on"
	tkecl:add_radiobutton $gstate macro_expansion "off on"
	tkecl:add_radiobutton $gstate prefer_rationals "off on"
	tkecl:add_radiobutton $gstate variable_names "off on check_singletons"

	tkecl:add_popupentry $gstate output_mode "tkecl:edit_output_mode global" Change {}
	tkecl:add_entry $gstate gc_interval number I
	tkecl:add_entry $gstate gc_interval_dict number I
#    tkecl:add_entry $gstate output_mode none S
	tkecl:add_entry $gstate print_depth number I
#    tkecl:add_entry $gstate cwd none S
	tkecl:add_popupentry $gstate cwd {tkecl:get_newcwd} Change S
	tkecl:add_menuentry $gstate library_path tkecl:paths_menu Change S
	button $gstate.close -text Close -command "destroy $gstate"
	pack $gstate.close -side top -fill x
	wm minsize $gstate 380 30
	wm resizable $gstate 1 0
	wm deiconify $gstate

	balloonhelp $gstate "ECLiPSe global state - see manual for descriptions of flags"
	balloonhelp $gstate.library_path "left click in entry to see all paths"
	bind $gstate <Alt-h> "tkecl:Get_helpfileinfo glob $gstate"
    } else {
	tkinspect:RaiseWindow $gstate
    }
}


proc tkecl:add_radiobutton {parent name values} {
    global tkecl

    set tkecl($name) [lindex [ec_rpcq_check [list get_flag $name _] (()_)] 2]
#    frame $parent.$name -relief groove -bd 1
    frame $parent.$name
    label $parent.$name.label -text $name -anchor w -width 20
    pack $parent.$name.label -side left
    foreach val $values {
	radiobutton $parent.$name.$val -text $val -variable tkecl($name) \
		-value $val -anchor w -command "tkecl:set_flag $name ()"
	pack $parent.$name.$val -side left
    }
    pack $parent.$name -side top -fill x
}

proc tkecl:add_popupentry {parent name command ctext exdr_type} {
    global tkecl

    set f [frame $parent.$name]
    pack [label $f.label -text $name -anchor w -width 20] -side left
    if {$exdr_type == ""} {
	set info [label $f.val -justify right -relief groove -textvariable tkecl($name)]
    } else {
	set info [entry $f.val -bg white -justify right -relief sunken -textvariable tkecl($name)]
	bind $f.val <Return> "tkecl:set_flag $name $exdr_type"
    }
    pack $info -side left -expand 1 -fill x
#    bind $parent.$name.val <Return> "tkecl:set_flag $name S"
    set tkecl($name) [lindex [ec_rpcq_check [list get_flag $name _] (()_)] 2]
    pack [button $f.b -anchor e -text $ctext -command $command] -side right 
    pack $f -side top -fill x
}

proc tkecl:add_menuentry {parent name buildmenu mtext exdr_type} {
    global tkecl

    set f [frame $parent.$name]
    pack [label $f.label -text $name -anchor w -width 20] -side left
    if {$exdr_type == ""} {
	set info [label $f.val -justify right -relief groove -textvariable tkecl($name)]
    } else {
	set info [entry $f.val -bg white -justify right -relief sunken -textvariable tkecl($name)]
	bind $f.val <Return> "tkecl:set_flag $name $exdr_type"
    }
    pack $info -side left -expand 1 -fill x
#    bind $parent.$name.val <Return> "tkecl:set_flag $name S"
    set tkecl($name) [lindex [ec_rpcq_check [list get_flag $name _] (()_)] 2]
    pack [menubutton $f.b -text $mtext -menu $f.b.m -relief raised] -side right 
    $buildmenu $f.b $name
    pack $f -side top -fill x
}

proc tkecl:add_entry {parent name vtype exdr_type} {
    global tkecl
#    frame $parent.$name -relief groove -bd 1
    switch -exact -- $vtype {
	number {
	    set vstring {regexp {^[0-9]*$} %P}
	}
	none {
	    set vstring {regexp {.*} %P}
	}
    }
    frame $parent.$name
    label $parent.$name.label -text $name -anchor w -width 20
    set tkecl($name) [lindex [ec_rpcq_check [list get_flag $name _] (()_)] 2]
    if {$exdr_type != ""} {
	ventry $parent.$name.val -bg white -justify right -relief sunken -textvariable tkecl($name) -validate key -invalidcmd bell -vcmd $vstring
	bind $parent.$name.val <Return> "tkecl:set_flag $name $exdr_type"
    } else {
	entry $parent.$name.val -relief groove -justify right -textvariable tkecl($name) 
	bind $parent.$name.val <Any-Key> {break}
	bind $parent.$name.val <Button-2> {break}
	bind $parent.$name.val <ButtonRelease-2> {break}
	bind $parent.$name.val <Button-1> {break}
    }
    pack $parent.$name.label -side left
    pack $parent.$name.val -side right -expand 1 -fill x
    pack $parent.$name -side top -fill x
}

# Set eclipse flag name from the tcl variable $tkecl(name)
proc tkecl:set_flag {name exdr_type} {
    global tkecl
    ec_rpcq_check [list set_flag $name $tkecl($name)] (()$exdr_type)
}


#----------------------------------------------------------------------
# Change output modes and print depth (both global and tracer settings)
#----------------------------------------------------------------------

set tkecl(output_mode_spec_nr) 7
set tkecl(output_mode_spec) {
	{{Variables}		{"" v V _}	{"X" "_123" "X_123" "_"}}
	{{Attributes}		{"" m M}	{none pretty full}}
	{{Operators}		{"" O}		{1+2 +(1,2)}}
	{{Spaces}		{"" K}		{"a,  b" "a,b"}}
	{{Quoting}		{"" Q}		{A 'A'}}
	{{Lists}		{"" .}		{{[a,b|_]} {.(a,.(b,_))}}}
	{{Use portray/1,2  }	{"" P}		{no yes}}
	{{Transformations  }	{T ""}		{no yes}}
}
# These are almost never used and mostly confusing for the user:
#	{{Treat as clause}	{"" C}		{no yes}}
#	{{Treat as goal}	{"" G}		{no yes}}


proc tkecl:edit_output_mode {which} {
    global tkecl
    set w .ec_tools.ec_om_$which
    if [winfo exists $w] {
	tkinspect:RaiseWindow $w
	return
    }

    # get the old settings
    switch -- $which {
	tracer {
	    set title "Tracer Output Options"
	    set tkecl(prdepth_$which) [lindex [ec_rpcatq\
		{getval dbg_print_depth _} (()_) tracer_tcl] 2]
	    set oldmode [lindex [ec_rpcq_check {get_tracer_output_modes _}\
		(_) tracer_tcl] 1]
	}
	global {
	    set title "Global Output Options"
	    set tkecl(prdepth_$which) [lindex [ec_rpcq_check\
		{get_flag print_depth _} (()_) ] 2]
	    set oldmode [lindex [ec_rpcq_check\
		{get_flag output_mode _} (()_) ] 2]
	}
    }

    toplevel $w
    wm transient $w .
    wm title $w $title

    # Make radiobuttons for the different options, linked to
    # variables tkecl(om_$which0)..tkecl(om_$which$tkecl(output_mode_spec_nr))
    frame $w.flags -relief raised -bd 1
    set row 0
    foreach descr $tkecl(output_mode_spec) {
	# set the button variables according to the old mode
	set tkecl(om_$which$row) ""
	foreach letter [lindex $descr 1] {
	    set occ [string first $letter $oldmode]
	    if {$occ >= 0} {
		set oldmode [string replace $oldmode $occ $occ {}]
		set tkecl(om_$which$row) $letter
	    }
	}
	grid [label $w.flags.label$row -text [lindex $descr 0]] -row $row -column 0 -sticky w
	set rb_name $w.flags.rb$row
	append rb_name _
	set col 1
	foreach val [lindex $descr 1] what [lindex $descr 2] {
	    grid [radiobutton $rb_name$col -text $what -value $val -variable tkecl(om_$which$row)] \
	    	-row $row -column $col -sticky w
	    incr col
	}
	incr row
    }
    # Make a scale and a "full"-checkbutton for the print depth
    label $w.label$row -text "Print depth"
    scale $w.scale -from 0 -to 100 -orient horizontal \
    	-tickinterval 10 -length 60m  -sliderlength 4m \
	-variable tkecl(prdepth_$which)
    set occ [string first "D" $oldmode]
    if {$occ >= 0} {
	set oldmode [string replace $oldmode $occ $occ {}]
	set tkecl(om_fullpd$which) D
    } else {
	set tkecl(om_fullpd$which) {}
    }
    checkbutton $w.fulldepth -text full -offvalue {} -onvalue D \
    	-variable tkecl(om_fullpd$which) -command "tkecl:toggle_scale om_fullpd$which $w.scale"

    frame $w.buttons
    pack [button $w.buttons.apply -text Apply -command [list tkecl:apply_output_mode $which $oldmode]] -side left -expand 1 -fill both
    pack [button $w.buttons.cancel -text Cancel -command "destroy $w"] -side left -expand 1 -fill both
    pack [button $w.buttons.ok -text Ok -command "[list tkecl:apply_output_mode $which $oldmode] ; destroy $w"] -side left -expand 1 -fill both

    pack $w.flags -side top -expand 1 -fill both
    pack $w.buttons -side bottom -expand 1 -fill both
    pack $w.label$row -side left -expand 1 -fill both
    pack $w.scale -side left
    pack $w.fulldepth -side left
}

# the scale is only active if the "full" button is not checked
proc tkecl:toggle_scale {var scale} {
    global tkecl
    if [string match "" $tkecl($var)] {
    	$scale configure -state normal -foreground black
    } else {
    	$scale configure -state disabled -foreground grey
    }
}

proc tkecl:apply_output_mode {which newmode} {
    global tkecl
    # newmode contains the remainder of oldmode that was ignored by the gui
    for {set i 0} {$i <= $tkecl(output_mode_spec_nr)} {incr i} {
        append newmode $tkecl(om_$which$i)
    }
    append newmode $tkecl(om_fullpd$which)

    switch -- $which {
	tracer {
	    ec_rpcq_check [list set_tracer_output_modes $newmode] (S) tracer_tcl
	    ec_rpcq_check [list set_tracer_print_depth $tkecl(prdepth_$which)] (I) tracer_tcl
	    tkecl:refresh_current_trace_line
	}
	global {
	    ec_rpcq_check [list set_flag output_mode $newmode] (()S)
	    ec_rpcq_check [list set_flag print_depth $tkecl(prdepth_$which)] (()I)
	    # these two are only for updating the Global Settings window:
	    set tkecl(output_mode) $newmode
	    set tkecl(print_depth) $tkecl(prdepth_$which)
	}
    }
}


#----------------------------------------------------------------------
# Files window
#----------------------------------------------------------------------

proc tkecl:compile_popup {dir} {

    set file [tkecl:getEcFile $dir "Compile File"]

    if {$file != ""} {
	tkecl:compile_file $file
    }
}

proc tkecl:xref_popup {} {

    set file [tkecl:getEcFile [pwd] "Xref File"]

    if {$file != ""} {
	if {[file exists $file] && [file readable $file]} {
	    set file [lindex [ec_rpcq [list os_file_name _ $file] (_S)] 1]
	    ec_rpcq [list xref $file [list [list : output graphviz]]] \
		   {(S[(()())])} xref
	} else {
	    tk_messageBox -icon error -type ok -message "Cannot access file $file"
	}
    }
}

proc tkecl:lint_popup {} {

    set file [tkecl:getEcFile [pwd] "Lint File"]

    if {$file != ""} {
	if {[file exists $file] && [file readable $file]} {
	    set file [lindex [ec_rpcq [list os_file_name _ $file] (_S)] 1]
	    ec_rpcq [list lint $file] (S) lint
	} else {
	    tk_messageBox -icon error -type ok -message "Cannot access file $file"
	}
    }
}

proc tkecl:compile_file {file {module ""}} {
    if {$file != ""} {
	if {$module == ""} {
	    set module [lindex [ec_rpcq_check {get_flag toplevel_module _} (()_) ] 2]
	}
	if {[file exists $file] && [file readable $file]} {
	    ec_rpcq [list compile_os_file $file $module] (S()) tracer_tcl
	} else {
	    tk_messageBox -icon error -type ok -message "Cannot access file $file"
	}
	tkecl:refresh_file_window
    }
}

proc tkecl:use_module_popup {} {

    set file [tkecl:getEcFile [pwd] "Use Module"]

    if {$file != ""} {
	tkecl:use_module $file
    }
}

proc tkecl:use_module {file {module ""}} {
    if {$file != ""} {
	if {$module == ""} {
	    set module [lindex [ec_rpcq_check {get_flag toplevel_module _} (()_) ] 2]
	}
	if {[file exists $file] && [file readable $file]} {
	    ec_rpcq [list use_module_os $file $module] (S()) tracer_tcl
	} else {
	    tk_messageBox -icon error -type ok -message "Cannot access file $file"
	}
	tkecl:refresh_file_window
    }
}

proc tkecl:edit_popup {} {

    set file [tkecl:getEcFile [pwd] "Edit File"]

    if {$file != ""} {
	tkecl:edit_file $file
	tkecl:add_source_file $file
    }
}

proc tkecl:edit_new_popup {} {

    set file [tkecl:getNewEcFile [pwd] "New Source File"]

    if {$file != ""} {
	tkecl:edit_file $file
	tkecl:add_source_file $file
    }
}

proc tkecl:edit_file {file {line -1}} {
    global tkecl

    if {$tkecl(pref,editor) == ""} {
	tk_messageBox -icon error -type ok -message "Cannot start an editor, as none is defined.\nDefine a third-party text editor using\nTools->'TkECLiPSe Preference Editor'\nto edit programs."
	return
    }
    if {![file exists $file]} {
	# Create the file (some editors require it)
	close [open $file w]
    }
    if {$line != -1 && $tkecl(pref,edit_line_option) != ""} {
	eval [list exec $tkecl(pref,editor) $tkecl(pref,edit_line_option)$line $file &]
    } else {
	eval [list exec $tkecl(pref,editor) $file &]
    }
}

proc tkecl:popup_file_window {} {

    set ec_files .ec_tools.ec_files
    if ![winfo exists $ec_files] {
	toplevel $ec_files
	wm title $ec_files "ECLiPSe Source File Manager"

	listbox $ec_files.names -selectmode single -width 20 -height 25\
		-yscrollcommand "tkecl:scroll_lb_sb $ec_files.state $ec_files.vscroll"
	listbox $ec_files.state -selectmode browse -width 11 -height 25\
		-yscrollcommand "tkecl:scroll_lb_sb $ec_files.names $ec_files.vscroll"
	scrollbar $ec_files.vscroll -command "tkecl:scroll_lb_lb $ec_files.names $ec_files.state"
	bind $ec_files.names <Double-Button-1> {
	    tkecl:edit_file [.ec_tools.ec_files.names get [.ec_tools.ec_files.names curselection]]
	}

	frame $ec_files.buttons
	button $ec_files.buttons.browse -text "Add file" -command {
	    set file [tkecl:getEcFile [pwd] "Add Source File"]

		if {$file != ""} [list tkecl:add_source_file $file]
	    }
	    pack $ec_files.buttons.browse -side left -fill x -expand 1
	button $ec_files.buttons.edit -text Edit -command {
		set sel [.ec_tools.ec_files.names curselection]
		if {$sel != ""} {
		    tkecl:edit_file [.ec_tools.ec_files.names get $sel]
		} else {
		    set file [tkecl:getNewEcFile "" "New Source File"]

		    if {$file != ""} {
			;# add_source done later in case edit_file fails
			tkecl:edit_file $file
			tkecl:add_source_file $file
		    }
		}}
	    pack $ec_files.buttons.edit -side left -fill x -expand 1
	button $ec_files.buttons.compile -text Compile -command {
		set sel [.ec_tools.ec_files.names curselection]
		if {$sel != ""} {
		    tkecl:compile_file [.ec_tools.ec_files.names get $sel]
		} else {
		    tkecl:compile_popup [pwd]
		}}
	    pack $ec_files.buttons.compile -side left -fill x -expand 1
	button $ec_files.buttons.refresh -text Redisplay -command tkecl:refresh_file_window
	    pack $ec_files.buttons.refresh -side left -fill x -expand 1
	button $ec_files.buttons.make -text Make -command {
	    	ec_rpcq_check make ()
	    	ec_rpcq_check {flush output} (())
	    	ec_rpcq_check {flush error} (())
		tkecl:refresh_file_window }
	    pack $ec_files.buttons.make -side left -fill x -expand 1
	button $ec_files.buttons.close -text Close -command "destroy $ec_files"
	    pack $ec_files.buttons.close -side left -fill x -expand 1

	pack $ec_files.buttons -side bottom -fill x
	pack $ec_files.vscroll -side left -fill y
	pack $ec_files.names -side left -fill both -expand 1
	pack $ec_files.state -side left -fill y
	balloonhelp $ec_files.names "ECLiPSe source files - files tracked by ECLiPSe for compilation by `make'"
	balloonhelp $ec_files.state "`ok' - previously compiled file\n \
		`modified' - previously compiled file that has been modified \
		(will be recompiled with `make')\n `new' - file names added to source list \
		(will not be compiled by `make' until it is explicitly compiled first)"
	balloonhelp $ec_files.buttons.browse "Add a file to list"
	balloonhelp $ec_files.buttons.edit "edit a file. If file is not in source list, it will be added."
	balloonhelp $ec_files.buttons.compile "compile selected file from source list"
	balloonhelp $ec_files.buttons.refresh "Refresh display - update status of files in source list"
	bind $ec_files <Alt-h> "tkecl:Get_helpfileinfo file $ec_files "
    } else {
	tkinspect:RaiseWindow $ec_files
    }
    tkecl:refresh_file_window
}

proc tkecl:add_source_file {file} {
    ec_rpcq_check [list record_source_file $file] (S) tracer_tcl 
    tkecl:refresh_file_window
}

proc tkecl:scroll_lb_lb {lb1 lb2 args} {
    eval "$lb1 yview $args"
    eval "$lb2 yview $args"
}

proc tkecl:scroll_lb_sb {lb sb from to} {
    $lb yview moveto $from
    $sb set $from $to
}

proc tkecl:refresh_file_window {} {

    set ec_files .ec_tools.ec_files
    if [winfo exists $ec_files] {
	$ec_files.names delete 0 end
	$ec_files.state delete 0 end
	set files [lindex [ec_rpcq_check {list_files _} (_) tracer_tcl] 1]
	foreach item [lsort -index 0 $files] {
	    $ec_files.names insert end [lindex $item 0]
	    $ec_files.state insert end [lindex $item 1]
	}
	# adjust view such that nothing is hidden to the right
	set current [.ec_tools.ec_files.names xview]
	.ec_tools.ec_files.names xview moveto [expr 1 - [lindex $current 1] + [lindex $current 0]]
    }
}


#----------------------------------------------------------------------
# Delayed goals
#----------------------------------------------------------------------

proc tkecl:popup_dg_window {} {
    global tkecl

    set ec_dg .ec_tools.ec_dg
    if ![winfo exists $ec_dg] {
	toplevel $ec_dg
	wm title $ec_dg "ECLiPSe Delayed Goals"

	set tkecl(dg_select_triggers) 0
	set tkecl(dg_trigger) postponed

	text $ec_dg.text -bg white -yscrollcommand "$ec_dg.vscroll set" -wrap none -xscrollcommand "$ec_dg.hscroll set" 
	scrollbar $ec_dg.vscroll -command "$ec_dg.text yview"
	scrollbar $ec_dg.hscroll -command "$ec_dg.text xview" -orient horizontal

	set ff [frame $ec_dg.filters]
	pack [checkbutton $ff.traced -text "traced only" -variable tkecl(pref,dgf_tracedonly)] -side left
	pack [checkbutton $ff.spied -text "spied only" -variable tkecl(pref,dgf_spiedonly)] -side left
	pack [checkbutton $ff.wake -text "scheduled only" -variable tkecl(pref,dgf_wakeonly)] -side left
	set tf [frame $ff.triggers -relief ridge -borderwidth 2] 
	pack [combobox $tf.triggers -click single -listheight 5 -bg white \
		  -postcommand "tkecl:dg_get_triggers $tf.triggers" \
		  -textvariable tkecl(dg_trigger) -editable 0 -click single \
		  -labeltext "Select from triggers:" -state disabled] \
	    -expand y -side right -fill x
	pack [checkbutton $tf.select_trig -variable tkecl(dg_select_triggers) \
		  -command "tkecl:select_dg_triggers $tf.triggers"] -side left
	pack $tf -side right -expand y -fill x

	menu $ec_dg.mbar
	$ec_dg config -menu $ec_dg.mbar
	menu $ec_dg.mbar.options
	$ec_dg.mbar add cascade -label Options -menu $ec_dg.mbar.options
	$ec_dg.mbar.options add command -label "Change print options ..." -command "tkecl:edit_output_mode tracer"
	$ec_dg.mbar.options add check -label "Refresh delayed goals at every trace line" -variable tkecl(pref,trace_refresh_dg)
	menu $ec_dg.mbar.help
	$ec_dg.mbar add cascade -label Help -menu $ec_dg.mbar.help
        $ec_dg.mbar.help add command -label "Delayed Goals Help" -command "tkecl:Get_helpfileinfo dela $ec_dg"

	frame $ec_dg.buttons
	button $ec_dg.buttons.refresh -text Refresh -command {tkecl:refresh_dg}
	    pack $ec_dg.buttons.refresh -side left -fill x -expand 1
	button $ec_dg.buttons.close -text Close -command "destroy $ec_dg"
	    pack $ec_dg.buttons.close -side left -fill x -expand 1

	pack $ec_dg.filters -side top -fill x
	pack $ec_dg.buttons -side bottom -fill x
	pack $ec_dg.vscroll -side left -fill y
	pack $ec_dg.hscroll -side bottom -fill x
	pack $ec_dg.text -expand 1 -fill both
	bind $ec_dg.text <Any-Key> "tkecl:readonly_keypress %A"
	bind $ec_dg.text <ButtonRelease-2> {break}

	balloonhelp $ec_dg.text "Delayed goals are displayed here. Green indicates goal has been scheduled.\n Right (or control-left) click on goal for a popup menu related to that goal and\n double left click to inspect goal (only if goal has invocation number)."
	balloonhelp $ec_dg.buttons "List of goals that are currently being delayed.\n\
		Can be set to automatically refresh at every trace line from tracer window."
	balloonhelp $ff "Filter options for filtering displayed delayed goals."
	balloonhelp $ff.traced "Show only goals which can be traced when selected."
	balloonhelp $ff.spied "Show only goals which are being spied when selected."
	balloonhelp $ff.wake "Show only goals which have been scheduled when selected."
	balloonhelp $tf "Show only goals which have been suspended on a global trigger.\n Select the trigger from the list."
	bind $ec_dg <Alt-h> "tkecl:Get_helpfileinfo dela $ec_dg"
    } else {
	tkinspect:RaiseWindow $ec_dg
    }
    tkecl:refresh_dg
}

proc tkecl:refresh_dg {} {
    global tkecl

    set ec_dg .ec_tools.ec_dg
    if [winfo exists $ec_dg] {
	$ec_dg.text delete 1.0 end
	eval $ec_dg.text tag delete [$ec_dg.text tag names]
	$ec_dg.text tag configure highlight -foreground #00b000
	$ec_dg.text tag configure truncated -background pink
	ec_rpcq_check [list gui_dg\
			$tkecl(dg_select_triggers)\
			$tkecl(dg_trigger)\
			[list dg_filter\
			    $tkecl(pref,dgf_tracedonly)\
			    $tkecl(pref,dgf_spiedonly)\
			    $tkecl(pref,dgf_wakeonly)]]\
		   (I()(III)) tracer_tcl
    }
}

proc tkecl:handle_dg_print {stream {length {}}} {
    global tkecl

    set gui_dg_info [ec_streamnum_to_channel $stream]
    set info [ec_read_exdr $gui_dg_info]
    while {$info != "end"} {
	set state [lindex $info 1]
	set prio [lindex $info 2]
	set invoc [lindex $info 3]
	set linelength [lindex $info 4]
	set line [lindex $info 5]
	if {$state == 1} {
	    set Tag highlight
	} else {
	    set Tag {}
	}
	    
	set ec_dg .ec_tools.ec_dg
	if [winfo exists $ec_dg] {
	    if {[string length $line] >= $tkecl(pref,text_truncate)} {
		set line [string range $line 0 $tkecl(pref,text_truncate)]
		set truncated 1
	    } else {
		set truncated 0
	    }
	    set gstart [$ec_dg.text index end]
	    $ec_dg.text insert end $line $Tag
	    if $truncated {
		$ec_dg.text insert end "..." truncated
	    }
	    $ec_dg.text tag bind g$invoc <Button-3> "tkecl:popup_delaymenu $ec_dg.text $invoc $prio %X %Y; break"
	    $ec_dg.text tag bind g$invoc <Control-Button-1> "tkecl:popup_delaymenu $ec_dg.text $invoc $prio %X %Y; break"
	    $ec_dg.text tag bind g$invoc <Double-Button-1> "tkinspect:Inspect_term_init invoc($invoc); break"
	    $ec_dg.text tag add g$invoc $gstart "$gstart lineend"
	    $ec_dg.text tag raise g$invoc
	}
	set info [ec_read_exdr $gui_dg_info]
    }
}

proc tkecl:select_dg_triggers {w} {
    global tkecl

    if {$tkecl(dg_select_triggers) == 1} {
	$w configure -state normal
	$w configure -editable 0
    } else {
	$w configure -state disabled
    }
}

proc tkecl:dg_get_triggers {w} {

    $w configure -list [lindex [ec_rpcq [list get_triggers _] (_) tracer_tcl] 1]
}

proc tkecl:popup_delaymenu {w invoc prio x y} {
    global tkecl

    if [winfo exists $w.gpopup] {
	destroy $w.gpopup
    }
    set m [menu $w.gpopup -tearoff 0]

    if {$invoc != 0} {
	set greturn [ec_rpcq_check [list get_goal_info_by_invoc $invoc _ _ _ _ _ _ _] (I_______) tracer_tcl]
	set spec [lindex  $greturn 2]
	set tspec [lindex $greturn 3]
	set module [lindex $greturn 4]
	;# spec should be Name/Arity if valid
	if {$spec != "unknown"} {
	    set spied [tkecl:pred_flag_value $spec $module spy]
	    if {$spied == "on"} {
		set spytext "Nospy $spec"
		set spyval off
	    } else {
		set spytext "Spy $spec"
		set spyval on
	    }
	    if {$invoc != 0} {
		set invtext "($invoc)"
	    } else {
		set invtext ""
	    }
	    $m add command -label "$tspec @ $module $invtext <$prio>" -state disabled
	    $m add command -label $spytext -command \
		    [list tkecl:set_pred_flag $spec $module spy $spyval]
	    $m add command -label "Display source for this predicate" -command \
		    [list tkecl:set_and_display_source $spec $module]
	    $m add command -label "Inspect this goal" -command \
		    "tkinspect:Inspect_term_init invoc($invoc)"
	    $m add command -label "Observe this goal" -command "tkecl:observe_goal $invoc"
	} else {
	    $m add command -label "No goal found for invocation $invoc. Please refresh." \
		    -state disabled
	}
    } else {
	$m add command -label "Goal information unavailable: please use tracer." \
		-state disabled
    }

    tk_popup $m $x $y
}


#----------------------------------------------------------------------
# Tracer
#----------------------------------------------------------------------

proc tkecl:set_fail_invoc {invoc} {
    global tkecl

    set tkecl(fail_invoc) $invoc
    tkecl:set_tracercommand f
}

proc tkecl:set_jumpto_invoc {invoc} {
    global tkecl

    if [regexp -- {^[0-9]+$} $invoc]  {
	set tkecl(cont_invoc) $invoc
	tkecl:set_tracercommand i
    }
}

proc tkecl:set_jumpto_depth {depth} {
    global tkecl

    if [regexp -- {^[0-9]+$} $depth]  {
	set tkecl(cont_mindepth) $depth
	set tkecl(cont_maxdepth) $depth
	tkecl:set_tracercommand j
    }
}

proc tkecl:setup_creep {} {
    global tkecl
    
    set tkecl(press_creep) 1
    tkecl:set_tracercommand c
}

proc tkecl:end_creep {} {
    global tkecl

    after cancel $tkecl(creepwaitevent)
    set tkecl(press_creep) 0
    set tkecl(creepwaitover) 1
}

proc tkecl:analyze_failure {parent} {
    global tkecl

    set result [ec_rpcq {failure_culprit _ _} (__) sepia_kernel]
    switch $result {
	throw -
	fail {
	    tk_messageBox -type ok -icon info -parent $parent \
		    -message "No failure culprit stored yet"
	}
	default {
	    set fculprit [lindex $result 1]
	    set invoc   [lindex $result 2]
	    if { $fculprit > $invoc } {
		set answer [ tk_messageBox -type yesno -icon question -parent $parent \
			-message "Most recent failure was caused by goal with invocation number ($fculprit).\
			Do you want to jump there now?" ]
		switch $answer {
		    yes { tkecl:set_jumpto_invoc $fculprit }
		}
	    } elseif {[string match $tkecl(tracer_state) disabled]} {
		tk_messageBox -type ok -icon info -parent $parent \
			-message "Most recent failure was caused by goal with invocation number ($fculprit).\
			To jump there\n\
			1. re-run the query\n\
			2. select \"Analyze failure\" immediately"
	    } elseif {$fculprit == $invoc && ![regexp $tkecl(current_port) fail|leave] } {
		tk_messageBox -type ok -icon info -parent $parent \
			    -message "Most recent failure was caused by goal with invocation number ($fculprit).\
			    This is the goal you are currently at."
	    } else {
		tk_messageBox -type ok -icon info -parent $parent \
		    -message "Most recent failure was caused by goal with invocation number ($fculprit).\
		    To jump there\n\
		    1. click \"Abort\" or \"Nodebug\"\n\
		    2. re-run the query\n\
		    3. select \"Analyze failure\" immediately"
	    }
	}
    }
}

proc tkecl:kill_tracer {} {
    set ec_tracer .ec_tools.ec_tracer

    if [winfo exists $ec_tracer] {
	destroy $ec_tracer
    }
}

proc tkecl:refresh_current_trace_line {} {
    global tkecl

    set ec_tracer .ec_tools.ec_tracer
    if ![winfo exists $ec_tracer] return 

    tkecl:edit_output_mode tracer
    set trace_info [ec_rpcq [list get_current_traceline _ _ _ _] (____) tracer_tcl]
    set invoc [lindex $trace_info 4]
    set style [lindex $trace_info 2]
    if {$style == "fail_style"} return ;# no point refreshing if failure/abort
    set depth [expr [lindex $trace_info 1] + 1]
    set line  [lindex $trace_info 3]
    if {[string length $line] >= $tkecl(pref,text_truncate)} {
	set truncated 1
	set line [string range $line 0 $tkecl(pref,text_truncate)]
    } else {
	set truncated 0
    }

    ;# only refresh current trace line if the current line has the same tag
    ;# (invocation number) as the current debug goal from ECLiPSe
    if {[lsearch [$ec_tracer.stack.text tag names $depth.0] $invoc] != -1} {
	$ec_tracer.stack.text delete $depth.0 "$depth.end+1 char"
	$ec_tracer.stack.text insert $depth.0 $line $style
	if $truncated {
	    $ec_tracer.stack.text insert end "..." truncate_style
	}
	$ec_tracer.stack.text insert $depth.end "\n" $style
	;# add the tag back to the refreshed line...
	$ec_tracer.stack.text tag add $invoc $depth.0 $depth.end
	$ec_tracer.stack.text tag raise $invoc 
    }
}

proc tkecl:popup_tracer {} {
    global tkecl

    set ec_tracer .ec_tools.ec_tracer
    if ![winfo exists $ec_tracer] {
	toplevel $ec_tracer
	wm title $ec_tracer "ECLiPSe Tracer"

	# initialize global tracer variables
	set tkecl(tracercommand) N
	set tkecl(tracercommand_issued) 0
	set tkecl(current_port) ....
	set tkecl(next_trace_line_depth) 1
	set tkecl(press_creep) 0
	set tkecl(creepwaitevent) 0
	set tkecl(cont_invoc) 0
	set tkecl(cont_mindepth) 0
	set tkecl(cont_maxdepth) 9999
	set tkecl(zap_port) {Not Current}
	set tkecl(filter_predtype) any
	set tkecl(filter_predmodule2) eclipse
	set tkecl(filter_mininvoc) 0
	set tkecl(filter_maxinvoc) 999999999 
	set tkecl(filter_mindepth) 0
	set tkecl(filter_maxdepth) 999999999
	set tkecl(filter_count) 1
	set tkecl(filter_hits) 0
	set tkecl(portlist) [lindex [ec_rpcq_check {debug_port_names _} (_) sepia_kernel] 1]
	set tkecl(portsets) {all none current previous entering exiting failing}
	set tkecl(portset,current) $tkecl(portlist)
	set tkecl(portset,previous) $tkecl(portlist)
	set tkecl(portset,all) $tkecl(portlist)
	set tkecl(portset,none) {}
	set tkecl(portset,entering) {call redo resume}
	set tkecl(portset,exiting) {exit *exit fail leave}
	set tkecl(portset,failing) {fail next else}
	foreach port $tkecl(portlist) {
	    set tkecl(filter_port,$port) 1
	}

	# filter,changable is a list of filter properties for the tracer filter
        # that can be changed for a filter command. Each property is 
        # represented by the variables tkecl(filter_<name>) (current value) and
        # tkecl(filter_last<name>) (previous value). The last values are for
        # determining if the property has been changed since the last filter
	set tkecl(filter,changable) \
            [list mininvoc maxinvoc mindepth maxdepth  wanted_ports predtype]
	# filterpred are the properties for the `specific predicate instance'
        # filter. These are treated separately from the other filter properties
	set tkecl(filterpred,changable) \
            [list predcondition predmatch predmodule predmodule2]
	foreach filterprop $tkecl(filter,changable) {
	    set tkecl(filter_last$filterprop) {}
	}
	foreach filterprop $tkecl(filterpred,changable) {
	    set tkecl(filter_last$filterprop) {}
	}

	set tmbar [menu $ec_tracer.menubar]
	$ec_tracer config -menu $tmbar
	$tmbar add cascade -label "Windows" -menu $tmbar.win -underline 0
	menu $tmbar.win
	$tmbar.win add command -label "Raise top-level" -command "tkinspect:RaiseWindow ."
	$tmbar.win add command -label "Predicate Browser" -command tkecl:popup_pred_prop
	$tmbar.win add command -label "Delayed Goals" -command tkecl:popup_dg_window
	$tmbar.win add separator
	$tmbar.win add command -label "Close Tracer" -command "destroy $ec_tracer"
	$tmbar add cascade -label "Options" -menu $tmbar.opt -underline 0
	menu $tmbar.opt
	$tmbar.opt add command -label "Configure filter ..." -command tkecl:popup_filter
	$tmbar.opt add command -label "Change print options ..." -command "tkecl:edit_output_mode tracer"
	$tmbar.opt add command -label "Analyze failure ..." -command "tkecl:analyze_failure $ec_tracer"
	$tmbar.opt add command -label "Refresh goal stack now" -command tkecl:refresh_goal_stack
	$tmbar.opt add check -label "Refresh goal stack at every trace line" -variable tkecl(pref,trace_refresh_stack)
	$tmbar.opt add check -label "Refresh delayed goals at every trace line" -variable tkecl(pref,trace_refresh_dg)
	$tmbar.opt add check -label "Raise tracer window at every trace line" -variable tkecl(pref,trace_raise_tracer)
	$tmbar add cascade -label "Help" -menu $tmbar.help -underline 0
	menu $tmbar.help
        $tmbar.help add command -label "Tracer Help" -command "tkecl:Get_helpfileinfo trac $ec_tracer"

	set ec_tracertab $ec_tracer.tab
	tabnotebook $ec_tracertab -padx 14 -pady 4 -background darkgray \
	    -activebackground #f0f0f0 -disabledbackground darkgray \
	    -normalbackground gray -borderwidth 0 -font tkecllabel
	frame $ec_tracertab.trace
	$ec_tracertab add "Trace Log" -window $ec_tracertab.trace
#	$ec_tracertab activate "Trace Log"
#	label $ec_tracertab.trace.label -text "Trace Log"
	text $ec_tracertab.trace.text -bg white -yscrollcommand "$ec_tracertab.trace.vscroll set" -wrap none -xscrollcommand "$ec_tracertab.trace.hscroll set"
	$ec_tracertab.trace.text tag configure call_style -foreground blue
	$ec_tracertab.trace.text tag configure exit_style -foreground #00b000
	$ec_tracertab.trace.text tag configure fail_style -foreground red
	$ec_tracertab.trace.text tag configure truncate_style -background pink
	scrollbar $ec_tracertab.trace.vscroll -command "$ec_tracertab.trace.text yview"
	scrollbar $ec_tracertab.trace.hscroll -command "$ec_tracertab.trace.text xview" -orient horizontal
	pack $ec_tracertab.trace.vscroll -side left -fill y
	pack $ec_tracertab.trace.hscroll -side bottom -fill x
	pack $ec_tracertab.trace.text -side bottom -expand 1 -fill both
#	pack $ec_tracertab.trace.label -side left -expand 1 -fill x

	bind $ec_tracertab.trace.text <Any-Key> "tkecl:readonly_keypress %A"
	bind $ec_tracertab.trace.text <ButtonRelease-2> {break}

	tkecl:setup_source_debug_window

	frame $ec_tracer.stack
	label $ec_tracer.stack.label -text "Call Stack"
	text $ec_tracer.stack.text -height 15 -bg white -yscrollcommand "$ec_tracer.stack.vscroll set" -wrap none -xscrollcommand "$ec_tracer.stack.hscroll set"
	$ec_tracer.stack.text tag configure call_style -foreground blue
	$ec_tracer.stack.text tag configure exit_style -foreground #00b000
	$ec_tracer.stack.text tag configure fail_style -foreground red
	$ec_tracer.stack.text tag configure truncate_style -background pink
	$ec_tracer.stack.text configure -cursor left_ptr
	scrollbar $ec_tracer.stack.vscroll -command "$ec_tracer.stack.text yview"
	scrollbar $ec_tracer.stack.hscroll -command "$ec_tracer.stack.text xview" -orient horizontal
	pack $ec_tracer.stack.vscroll -side left -fill y
	pack $ec_tracer.stack.hscroll -side bottom -fill x
	pack $ec_tracer.stack.text -side bottom -expand 1 -fill both
	pack $ec_tracer.stack.label -side left -expand 1 -fill x

	bind $ec_tracer.stack.text <Any-Key> "tkecl:readonly_keypress %A"
	bind $ec_tracer.stack.text <ButtonRelease-2> {break}

	frame $ec_tracer.buttons
	bind $ec_tracer <Enter> "tkecl:enable_tracer_keys $ec_tracer"
        # remember underline for button if keyboard shortcut added!
	bind $ec_tracer.buttons <Key-c> {tkecl:set_tracercommand c}
	bind $ec_tracer.buttons <Key-l> {tkecl:set_tracercommand l}
	bind $ec_tracer.buttons <Key-s> {tkecl:set_tracercommand s}
	bind $ec_tracer.buttons <Key-u> {tkecl:set_tracercommand up}
	bind $ec_tracer.buttons <Key-p> {tkecl:set_tracercommand z}
	bind $ec_tracer.buttons <Key-f> {tkecl:set_tracercommand filter}
	bind $ec_tracer.buttons <Key-i> {tkecl:set_tracercommand i}
	bind $ec_tracer.buttons <Key-d> {tkecl:set_tracercommand j}
#	bind $ec_tracer.buttons <Key-plus> {tkecl:set_tracercommand +}
#	bind $ec_tracer.buttons <Key-minus> {tkecl:set_tracercommand -}
	button $ec_tracer.buttons.creep -text Creep -underline 0 -command {}
	bind $ec_tracer.buttons.creep <Button-1> {tkecl:setup_creep}
	bind $ec_tracer.buttons.creep <ButtonRelease-1> {tkecl:end_creep}
	    pack $ec_tracer.buttons.creep -side left -fill x -expand 1
	;# destroy are sent to all widgets of a window, chose one for 
	;# code to cope with the closing of the tracer window
	bind $ec_tracer.buttons.creep <Destroy> "if {![ec_running]} {tkecl:tracer_off}"
	button $ec_tracer.buttons.skip -text Skip -underline 0 -command {tkecl:set_tracercommand s}
	    pack $ec_tracer.buttons.skip -side left -fill x -expand 1
	button $ec_tracer.buttons.up -text Up -underline 0 -command {tkecl:set_tracercommand up}
	    pack $ec_tracer.buttons.up -side left -fill x -expand 1
	button $ec_tracer.buttons.leap -text Leap -underline 0 -command {tkecl:set_tracercommand l}
	    pack $ec_tracer.buttons.leap -side left -fill x -expand 1
	button $ec_tracer.buttons.filter -text {Filter} -underline 0 -command {tkecl:set_tracercommand filter}
	    pack $ec_tracer.buttons.filter -side left -fill x -expand 1
	button $ec_tracer.buttons.abort -text Abort -command {tkecl:set_tracercommand a}
	    pack $ec_tracer.buttons.abort -side left -fill x -expand 1
	button $ec_tracer.buttons.nodebug -text Nodebug -command {tkecl:set_tracercommand n ; tkinspect:RaiseWindow .}
	    pack $ec_tracer.buttons.nodebug -side left -fill x -expand 1

	frame $ec_tracer.cont
	button $ec_tracer.cont.button -text "To Invoc:" -underline 3 -command {tkecl:set_tracercommand i}
	    pack $ec_tracer.cont.button -side left -fill x -expand 1
	ventry $ec_tracer.cont.invoc \
		-vcmd {regexp {^[0-9]*$} %P} -validate key -invalidcmd bell \
		-width 8 -textvariable tkecl(cont_invoc) -bg white
	    pack $ec_tracer.cont.invoc -side left
	    bind $ec_tracer.cont.invoc <Return> "tkecl:set_tracercommand i"
	button $ec_tracer.cont.jump -text "To Depth:" -underline 3 -command {tkecl:set_tracercommand j}
	    pack $ec_tracer.cont.jump -side left -fill x -expand 1
	ventry $ec_tracer.cont.mindepth \
		-vcmd {regexp {^[0-9]*$} %P} -validate key -invalidcmd bell \
		-width 5 -textvariable tkecl(cont_mindepth) -bg white
	    pack $ec_tracer.cont.mindepth -side left
	ventry $ec_tracer.cont.maxdepth -labeltext ..  \
		-vcmd {regexp {^[0-9]*$} %P} -validate key -invalidcmd bell \
		-width 5 -textvariable tkecl(cont_maxdepth) -bg white
	    pack $ec_tracer.cont.maxdepth -side left
	button $ec_tracer.cont.zap -text "To Port:" -underline 3 -command {tkecl:set_tracercommand z}
	    pack $ec_tracer.cont.zap -side left

	combobox $ec_tracer.cont.ports -click single -listheight 16 -bg white \
		-width 10 -list "{Not Current} $tkecl(portlist)" -textvariable tkecl(zap_port)
	    pack $ec_tracer.cont.ports -side left
	button $ec_tracer.close -text Close -command "destroy $ec_tracer"
	tkecl:configure_tracer_buttons disabled

	pack $ec_tracer.stack -side top -expand 1 -fill both
	pack $ec_tracer.buttons -side top -fill x
	pack $ec_tracer.cont -side top -fill x
	pack $ec_tracertab -expand 1 -fill both
	pack $ec_tracer.close -side top -fill x

	ec_rpcq {set_flag debugging creep} (()())

#--------------------------------------------------------------------
# Balloon Help for tracer
#--------------------------------------------------------------------
       balloonhelp $ec_tracer "Tracer for ECLiPSe execution - start execution from main window"
       balloonhelp $ec_tracer.stack.label "Execution call stack - \
   shows the current goal and its ancestors.\n \
   Calls for current goal in blue, failure in red, success in green. \ 
   Ancestors printed with non-current bindings in black\n \
   Press right (or control-left) mouse button over a stack item for popup \
 menu related to that goal/predicate.\n Double-click left mouse button over \
 a stack item to inspect it.\n Single click left mouse button on the \
 information (left) part of\n the stack item to show source contxt\n "
       balloonhelp $ec_tracertab.trace "Trace log: chronological log of traced goals.\n Calls in blue, successes in green, failures in red\n Leading indentation indicates depth"
       balloonhelp $ec_tracer.buttons.creep "Creep to next tracable goal's debug port.\n\
   Keyboard shortcut: `c'\nPress and hold button for continuous creep."
       balloonhelp $ec_tracer.buttons.skip "Skip to exit/fail port of goal (creep\
   if already at port).\nKeyboard shortcut: `s'"
       balloonhelp $ec_tracer.buttons.leap "Leap to next spied predicate port or next breakpoint.\n\
   Keyboard shortcut: `l'"
       balloonhelp $ec_tracer.buttons.up "Continue until back to parent's\
   depth\nKeyboard shortcut: `u'"
       balloonhelp $ec_tracer.buttons.filter "Continue until filter \
   conditions hold.\nKeyboard shortcut: `f'.\n\
   See Options for how to configure the filter."
       balloonhelp $ec_tracer.buttons.abort "Abort execution"
       balloonhelp $ec_tracer.buttons.nodebug "Turn off debugging and\
   continue execution\n(Further outputs will be displayed on main window)"
       balloonhelp $ec_tracer.cont.button "Jump to port for goal with \
   invocation number on right\nKeyboard shortcut: `i'"
       balloonhelp $ec_tracer.cont.jump "Jump to port for next goal with \
   depth in the ranges on the right.\nKeyboard shortcut: `d'"
       balloonhelp $ec_tracer.cont.zap "Jump to port selected on the right\
   \nKeyboard shortcut: `p'"
       bind $ec_tracer <Alt-h> "tkecl:Get_helpfileinfo trac $ec_tracer"
   } else {
       tkinspect:RaiseWindow $ec_tracer
   }   
}

# enable tracing via keyboard shortcuts if tracer is enabled
proc tkecl:enable_tracer_keys {ec_tracer} {
    global tkecl

    if {$tkecl(tracer_state) == "normal"} {
	focus $ec_tracer.buttons
    }
}

proc tkecl:handle_debug_output {stream {length {}}} {
    if {![winfo exists .ec_tools.ec_tracer]} {
	return
    }
    ec_stream_to_window_sync {} .ec_tools.ec_tracer.tab.trace.text $stream $length
}

# CAUTION: text widgets positions are a bit weird: the text widget always
# has a newline at the end, and the end-index is just after that. Therefore,
# an empty text widget has a newline at 1.0 and end == 2.0

proc tkecl:handle_trace_line {stream {length {}}} {
    global tkecl

    set ec_tracer .ec_tools.ec_tracer
    if ![winfo exists $ec_tracer] {
	tkecl:popup_tracer
    }
    set tkecl(tracercommand_issued) 0
    set trace_info [ec_read_exdr [ec_streamnum_to_channel $stream]]
    if {[llength $trace_info] == 0} {
	# start of new trace session
	# make sure current source file is reloaded
	# cannot simply set file to "" as we may need the file name (for
        # placing breakpoints etc.)
	if {$tkecl(source_debug,file) != ""} {
	    tkecl:load_source_debug_file $tkecl(source_debug,file)
	}
	return
    }
    set depth [lindex $trace_info 0]
    set style [lindex $trace_info 1]
    set line [lindex $trace_info 2]
    set invoc [lindex $trace_info 3]
    set tkecl(current_port) [lindex $trace_info 4]
    set prio [lindex $trace_info 5]
    set fpath_info [lindex $trace_info 6]
    set from [lindex $trace_info 7]
    set to [lindex $trace_info 8]
    set tkecl(cont_invoc) $invoc  ;# defaults to current 
    set tkecl(tracer_up_depth) [expr $depth>0 ? $depth-1 : 0]

    if {[string length $line] >= $tkecl(pref,text_truncate)} {
	set truncated 1
	set line [string range $line 0 $tkecl(pref,text_truncate)]
    } else {
	set truncated 0
    }
    $ec_tracer.tab.trace.text tag configure $depth -lmargin1 "$depth m"
    $ec_tracer.tab.trace.text insert end $line "$style $depth"
    if $truncated {
	$ec_tracer.tab.trace.text insert end "..." truncate_style
    }
    ;# make sure at least a partial line at the start is visible 
    $ec_tracer.tab.trace.text see "end -1 line linestart +40 chars"
    $ec_tracer.tab.trace.text insert end "\n" $style

    set stdepth [expr $depth + 1] ;# actual depth in printed stack
    set next_line [lindex [split [$ec_tracer.stack.text index end-1chars] .] 0]
    if {$style == "fail_style" && $next_line > $stdepth} {
	;# we did not jump to this fail port..
	$ec_tracer.stack.text tag remove call_style $stdepth.0 end
	if {[$ec_tracer.stack.text compare $stdepth.end == $stdepth.0]} {
	    ;# if the line is empty, we don't have the port, print it
	    ;# don't bother to add a popup...not very useful here
	    $ec_tracer.stack.text insert $stdepth.0 $line $style
	}
	$ec_tracer.stack.text tag add fail_style $stdepth.0 end
	$ec_tracer.stack.text see $stdepth.0
	set tkecl(next_trace_line_depth) $stdepth
    } else {
	if {$next_line > $tkecl(next_trace_line_depth)} {
	    # delete leftover exit/fail lines
	    # and tags to goals that are no longer accessible
	    tkecl:cleanup_goal_stack_line $tkecl(next_trace_line_depth) [expr $next_line - 1]
	    set next_line $tkecl(next_trace_line_depth)
	}
	if {$next_line < $stdepth} {
	    while {$next_line < $stdepth} {
		$ec_tracer.stack.text insert end "\n"
		incr next_line
	    }
	} elseif {$next_line > $stdepth} {
	    tkecl:cleanup_goal_stack_line $stdepth [expr $next_line - 1]
	}
	$ec_tracer.stack.text tag remove call_style 1.0 end
	$ec_tracer.stack.text insert end $line $style
	if $truncated {
	    $ec_tracer.stack.text insert end "..." truncate_style
	}
	$ec_tracer.stack.text insert end "\n" $style
	tkecl:set_goalpopup $depth $invoc $prio $line
	$ec_tracer.stack.text see end
	if {$style == "call_style"} {
	    ;# extract into tkecl(next_trace_line_depth) the line number 
	    ;# from an index of the form line.char
	    scan [$ec_tracer.stack.text index end-1chars] \
		    {%u} tkecl(next_trace_line_depth)
	} else {
	    set tkecl(next_trace_line_depth) $stdepth
	}
    }

    # Refresh stack, delayed goals and debug source displays
    if {$tkecl(pref,trace_refresh_stack) && $style != "fail_style"} {
	# don't refresh during failures because we'd lose displayed information
    	tkecl:refresh_goal_stack
    }
    if {$tkecl(pref,trace_refresh_dg)} { tkecl:refresh_dg }
    tkecl:update_source_debug $style $from $to $fpath_info 
}

proc tkecl:handle_tracer_port_start {} {
    global tkecl

    # Enable the buttons, and add some delay if repeating creep from mouse hold
    tkecl:configure_tracer_buttons normal
    if {($tkecl(press_creep) > 0) && \
	    [string match $tkecl(tracercommand) "c"]} {
	if {$tkecl(press_creep) == 1} { ;# initial press, wait longer
	    set interval 700
	} else {
	    set interval 50
	    set tkecl(press_creep) 2
	}
	set tkecl(creepwaitevent) [after $interval {set tkecl(creepwaitover) 1}]
	vwait tkecl(creepwaitover)
	if {($tkecl(press_creep) > 0) && \
		[string match $tkecl(tracercommand) "c"]} {
	    # did not select any other tracer command during wait...
	    set tkecl(press_creep) 2
	    tkecl:set_tracercommand c
	} 
    }
    # update the filter hits
    set tkecl(filter_hits) [lindex \
	[ec_rpcatq [list getval filter_hits _] (()_) tracer_tcl] 2]
}

proc tkecl:send_tracer_command {cmd {type S}} {

    ec_rpcq [list set_tracer_command $cmd] ($type) tracer_tcl
}

proc tkecl:handle_tracer_command {} {
    global tkecl

    # interpret the command and configure Eclipse for continuation
    # tracer_state must be set to disabled before command is handled
    # as this indicates that we are ready to continue from the debug port
    switch -exact -- $tkecl(tracercommand) {
	N {
	    # caution: if tracercommand = N the window is already destroyed!
	    set tkecl(tracer_state) disabled
	    tkecl:send_tracer_command N
	}
	i {
	    if [regexp -- {^[0-9]+$} $tkecl(cont_invoc)] {
		tkecl:configure_tracer_buttons disabled
		ec_rpcq_check [list configure_prefilter $tkecl(cont_invoc) _ _ _ _]\
			(I____) sepia_kernel 
		tkecl:send_tracer_command i
	    }
	}
	j {
	    if {[regexp -- {^[0-9]+$} $tkecl(cont_mindepth)] && \
		    [regexp -- {^[0-9]+$} $tkecl(cont_mindepth)]} {
		tkecl:configure_tracer_buttons disabled
		ec_rpcq_check [list configure_prefilter _ [list .. $tkecl(cont_mindepth) $tkecl(cont_maxdepth)] _ _ _]\
			(_(II)___) sepia_kernel 
		tkecl:send_tracer_command j
	    }
	}
	up { ;# jump one level up
	    tkecl:configure_tracer_buttons disabled
	    ec_rpcq_check [list configure_prefilter _ [list .. 0 $tkecl(tracer_up_depth)] _ _ _]\
		    (_(II)___) sepia_kernel 
	    tkecl:send_tracer_command j
	}
	f { ;# fail to $tkecl(fail_invoc)
	    tkecl:configure_tracer_buttons disabled
	    tkecl:send_tracer_command [list f $tkecl(fail_invoc)] {(I)}
	}
	z { ;# zap to $tkecl(zap_port)
	    tkecl:configure_tracer_buttons disabled
	    if {$tkecl(zap_port) != "Not Current"} {
		ec_rpcq_check [list configure_prefilter _ _ $tkecl(zap_port) _ dontcare]]\
			(__()_()) sepia_kernel
		tkecl:send_tracer_command ""
	    } else {
		tkecl:send_tracer_command z
	    }
	}
	filter {
	    tkecl:configure_tracer_buttons disabled

	    # for the third case we only need to stop at predicates 
	    # with spypoints as we will set one up on the template 
	    # predicate.

	    set changed 0

	    # now set the count
	    if {$tkecl(filter_count) < 1} { set tkecl(filter_count) 1}

	    # prepare ECLiPSe side for filter command. This must be done
	    # before setting any specialised condition (e.g. goal filtering).
	    ec_rpcq [list prepare_filter $tkecl(filter_count)] (I) tracer_tcl

	    switch -exact -- $tkecl(filter_predtype) {
		any {
		    set filter_spy all
		}
		anyspy {
		    set filter_spy spied
		}
		goalmatching {
		    switch [tkecl:configure_pred] {
			    error {
				tkecl:reset_traceport
				return
			    }
			    spy_set {
				set filter_spy spied
				set tkecl(last_filter_spy) $filter_spy
				incr changed 
			    }
			    continue {
				# same filter, no need to change
				set filter_spy $tkecl(last_filter_spy)
			    }
			    default {
				set filter_spy all
				set tkecl(last_filter_spy) $filter_spy
				incr changed 
			    }
		    }
		}
	    }

	    set tkecl(filter_wanted_ports) {}
	    foreach port $tkecl(portlist) {
		if $tkecl(filter_port,$port) {
		    lappend tkecl(filter_wanted_ports) $port
		}
	    }
	    if {$tkecl(filter_wanted_ports) != $tkecl(portset,current)} {
		set tkecl(portset,previous) $tkecl(portset,current)
		set tkecl(portset,current) $tkecl(filter_wanted_ports)
	    }

	    # sepia_kernel:configure_prefilter(Invoc, Depth, Ports, Preds, Module)
	    foreach filterprop $tkecl(filter,changable) {
		if [tkecl:check_if_changed $filterprop] { incr changed}
	    }

	    if [catch { ec_rpcq_check [list configure_prefilter \
				       [list .. $tkecl(filter_mininvoc) $tkecl(filter_maxinvoc)] \
				       [list .. $tkecl(filter_mindepth) $tkecl(filter_maxdepth)] \
				       $tkecl(filter_wanted_ports) \
				       $filter_spy \
				       dontcare] \
			    {((II)(II)[()*]()())} sepia_kernel }\
		   ] {
		tk_messageBox -icon error -type ok -message "Filter Error: some entries for filter conditions are invalid. "
		tkecl:reset_traceport
		return
	    }

	    if {$changed > 0} {
		;# change in filter condition, reset filter count
		ec_rpcatq [list setval filter_hits 0] (()I) tracer_tcl
	    }
	    tkecl:send_tracer_command filter
	}
	default {
	    tkecl:configure_tracer_buttons disabled
	    tkecl:send_tracer_command $tkecl(tracercommand)
	}
    }
    ec_multi:terminate_phase
}

proc tkecl:check_if_changed {filterprop} {
    global tkecl

    if {$tkecl(filter_$filterprop) != $tkecl(filter_last$filterprop)} {
	set tkecl(filter_last$filterprop) $tkecl(filter_$filterprop)
	return 1
    } else {
	return 0
    }
}

proc tkecl:reset_traceport {} {
    global tkecl

    tkecl:configure_tracer_buttons normal
    set tkecl(tracercommand) N
    set tkecl(tracercommand_issued) 0
}

proc tkecl:set_tracercommand {command} {
    global tkecl

    if [winfo exists .ec_tools.ec_tracer] {
	set tkecl(tracercommand) $command
	set tkecl(tracercommand_issued) 1
    }
}

proc tkecl:check_tracer_interaction {} {
    global tkecl tcl_platform

    if {[winfo exists .ec_tools.ec_tracer]} {
	if {$tkecl(tracercommand_issued) == 1} {
	tkecl:handle_tracer_command
	}
    }
}


proc tkecl:tracer_off {} {
    global tkecl

    if [string match $tkecl(tracer_state) disabled] {
	ec_rpcq {set_flag debugging nodebug} (()())
    } else {
	# tracer window may have already disappeared, pass command directly
	set tkecl(tracercommand) N
	tkecl:handle_tracer_command 
    }
}

proc tkecl:configure_tracer_buttons {state} {
    global tkecl
    set tkecl(tracer_state) $state	;# normal or disabled
    set ec_tracer .ec_tools.ec_tracer
    $ec_tracer.buttons.creep configure -state $state
    $ec_tracer.buttons.leap configure -state $state
    $ec_tracer.buttons.up configure -state $state
    $ec_tracer.buttons.filter configure -state $state
    $ec_tracer.buttons.skip configure -state $state
    $ec_tracer.buttons.abort configure -state $state
    $ec_tracer.buttons.nodebug configure -state $state
    $ec_tracer.cont.button configure -state $state
    $ec_tracer.cont.jump configure -state $state
    $ec_tracer.cont.zap configure -state $state
    if [winfo exists $ec_tracer.filter] {
	$ec_tracer.filter.go configure -state $state
# Don't see any reason why this should be done (?)
#	if {$tkecl(predtype) == "goalmatching"} {
#	    if {$state == "disabled"} {
#		tkecl:fields_disable $ec_tracer
#	    }
#	    if {$state == "normal"} {
#		tkecl:enable_pred $ec_tracer
#	    }
#	}
    }
    if {$state == "normal"} {
	if {[tkecl:pointer_window] == "$ec_tracer"} { 
	    focus $ec_tracer.buttons
	}
	if {$tkecl(pref,trace_raise_tracer)} {
	    tkinspect:RaiseWindow $ec_tracer
	}
    } else { ;# $state == "disabled"
	if {[focus] == "$ec_tracer.buttons"} {
	    ;# assume buttons had focus, so remove it to ignore any
            ;# stray key presses while buttons are disabled
	    focus $ec_tracer
	}
    }
}


proc tkecl:popup_goalmenu {w invoc depth prio greturn x y} {
    global tkecl

    if [winfo exists $w.gpopup] {
	destroy $w.gpopup
    }
    set m [menu $w.gpopup -tearoff 0]
    set spec [lindex  $greturn 2]
    set tspec [lindex $greturn 3]
    set module [lindex $greturn 4]
    set lookup_module [lindex $greturn 5]
    set path_info [lindex $greturn 6]

    if {![string match unknown $spec] } {
	$m add command -label "$tspec @ $module <$prio>" -state disabled
	set spied [tkecl:pred_flag_value $spec $lookup_module spy]
	if {$spied == "on"} {
	    set spytext "Nospy $spec"
	    set spyval off
	} else {
	    set spytext "Spy $spec"
	    set spyval on
	}
	$m add command -label $spytext -command \
		[list tkecl:set_pred_flag $spec $lookup_module spy $spyval]
	$m add command -label "Display source for this predicate" -command \
		[list tkecl:set_and_display_source $spec $module]
	if {$path_info == "no"} {set gstate disabled} else {set gstate normal} 
	$m add command -label "Display source context for this call" -command \
	    "tkecl:show_source_context $invoc {$greturn}" -state $gstate
	$m add command -label "Inspect this goal" -command \
		"tkinspect:Inspect_term_init invoc($invoc)"
	$m add command -label "Observe this goal" -command "tkecl:observe_goal $invoc"
	$m add command -label "Force failure of this goal" -command \
		"tkecl:set_fail_invoc $invoc"
	$m add command -label "Jump to this invocation number ($invoc)" -command \
		"tkecl:set_jumpto_invoc $invoc"

    }
    $m add command -label "Jump to this depth $depth" -command \
    	"tkecl:set_jumpto_depth $depth"
    $m add separator
    $m add command -label "Refresh goal stack" -command \
	    "tkecl:refresh_goal_stack"

    tk_popup $m $x $y
}

proc tkecl:cleanup_goal_stack_line {depth next_line} {
    set ec_tracer .ec_tools.ec_tracer

    for {set line $depth} {$line <= $next_line} {incr line 1} {
	set taglist [$ec_tracer.stack.text tag names $line.0]
	set invocidx [lsearch -regexp $taglist  {^[0-9]+$}]
	;# tags in the lines are also deleted
	if {$invocidx >= 0} {
	    $ec_tracer.stack.text tag delete [lindex $taglist $invocidx]
	}
    }
    $ec_tracer.stack.text delete $depth.0 $next_line.end+1char
}

proc tkecl:refresh_goal_stack {} {
    global tkecl

    foreach anc  [lindex [ec_rpcq {get_ancestors _} (_) tracer_tcl] 1] {
	foreach {pred depth invoc prio line} $anc {break}
	set stdepth [expr $depth+1]
	;# only clean up line if it is actually there!
	if [.ec_tools.ec_tracer.stack.text compare end-1char > $stdepth.0] {
	    tkecl:cleanup_goal_stack_line $stdepth $stdepth
	}
	if {[string length $line] >= $tkecl(pref,text_truncate)} {
	    set line [string range $line 0 $tkecl(pref,text_truncate)]
	    .ec_tools.ec_tracer.stack.text insert $stdepth.0 "\n"
            ;# put in the newline first, then insert things before it
	    .ec_tools.ec_tracer.stack.text insert $stdepth.0 $line call_style
	    .ec_tools.ec_tracer.stack.text insert $stdepth.end "..." truncate_style
	} else {
	    .ec_tools.ec_tracer.stack.text insert $stdepth.0 "\n"
	    .ec_tools.ec_tracer.stack.text insert $stdepth.0 $line call_style
	}
	tkecl:set_goalpopup $depth $invoc $prio $line
    }

    .ec_tools.ec_tracer.stack.text see end
}

proc tkecl:set_goalpopup {depth invoc prio line} {
# print goal line in the stack display and set up the tag for it
    set ec_tracer .ec_tools.ec_tracer
    set greturn [ec_rpcq_check\
	    [list get_goal_info_by_invoc $invoc _ _ _ _ _ _ _] (I_______) tracer_tcl]
    $ec_tracer.stack.text tag bind $invoc <Button-3> \
	"tkecl:popup_goalmenu $ec_tracer.stack.text $invoc $depth $prio {$greturn} %X %Y; break"
    $ec_tracer.stack.text tag bind $invoc <Control-Button-1> \
	"tkecl:popup_goalmenu $ec_tracer.stack.text $invoc $depth $prio {$greturn} %X %Y; break"
    $ec_tracer.stack.text tag bind info$invoc <Button-3> \
	"tkecl:popup_goalmenu $ec_tracer.stack.text $invoc $depth $prio {$greturn} %X %Y; break"
    $ec_tracer.stack.text tag bind info$invoc <Control-Button-1> \
	"tkecl:popup_goalmenu $ec_tracer.stack.text $invoc $depth $prio {$greturn} %X %Y; break"
    $ec_tracer.stack.text tag bind $invoc <Double-Button-1> "tkinspect:Inspect_term_init invoc($invoc); break"
    $ec_tracer.stack.text tag bind info$invoc <Button-1> "tkecl:show_source_context $invoc {$greturn}; break"

    # find the information part (the part before the goal) of the line
    # if the format for this part changes, the regexp may also need to change
    if {[regexp {[^)]+\) [^ ]+ [^ ]+} $line info] == 1} {
	set length [string length $info]
    } else {
	# this probably shouldn't happen
	set length 0
    }
    set stdepth [expr $depth + 1]
    # $stdepth.$length is one char after the port name
    $ec_tracer.stack.text tag add info$invoc $stdepth.0 $stdepth.$length
    $ec_tracer.stack.text tag raise info$invoc
    incr length
    $ec_tracer.stack.text tag add $invoc $stdepth.$length $stdepth.end
    $ec_tracer.stack.text tag raise $invoc
}

proc tkecl:popup_filter {} {
    global tkecl

    set ec_tracer .ec_tools.ec_tracer
    if [winfo exists $ec_tracer.filter] {
	tkinspect:RaiseWindow $ec_tracer.filter
	return
    }
    
    toplevel $ec_tracer.filter
    wm title $ec_tracer.filter "Filter"

    label $ec_tracer.filter.label -text "Continue to a port with all of the following properties:"
    pack $ec_tracer.filter.label -side top
    frame $ec_tracer.filter.depthsettings -relief groove -bd 1
    pack $ec_tracer.filter.depthsettings -side top -ipadx 3 -ipady 3 -pady 5 -padx 5 -fill x 

    set row 0
    set col 0
    set cols 4

    label $ec_tracer.filter.depthsettings.mininvoclabel -text "Invocation number from .."

    ventry $ec_tracer.filter.depthsettings.mininvoc  \
	    -vcmd {regexp {^[0-9]*$} %P} -validate key -invalidcmd bell \
	    -width 8 -textvariable tkecl(filter_mininvoc) -bg white
	
    label $ec_tracer.filter.depthsettings.maxinvoclabel -text ".. to .."

    ventry $ec_tracer.filter.depthsettings.maxinvoc \
	    -vcmd {regexp {^[0-9]*$} %P} -validate key -invalidcmd bell \
	    -width 10 -textvariable tkecl(filter_maxinvoc) -bg white
	
    grid $ec_tracer.filter.depthsettings.mininvoclabel $ec_tracer.filter.depthsettings.mininvoc $ec_tracer.filter.depthsettings.maxinvoclabel $ec_tracer.filter.depthsettings.maxinvoc 

    incr row

    label $ec_tracer.filter.depthsettings.mindepthlabel -text "Depth from .."  

    ventry $ec_tracer.filter.depthsettings.mindepth \
	    -vcmd {regexp {^[0-9]*$} %P} -validate key -invalidcmd bell \
	    -width 8 -textvariable tkecl(filter_mindepth) -bg white

    label $ec_tracer.filter.depthsettings.maxdepthlabel -text ".. to .."

    ventry $ec_tracer.filter.depthsettings.maxdepth \
	    -vcmd {regexp {^[0-9]*$} %P} -validate key -invalidcmd bell \
	    -width 10 -textvariable tkecl(filter_maxdepth) -bg white

    grid $ec_tracer.filter.depthsettings.mindepthlabel $ec_tracer.filter.depthsettings.mindepth $ec_tracer.filter.depthsettings.maxdepthlabel $ec_tracer.filter.depthsettings.maxdepth -sticky w

    frame $ec_tracer.filter.settings -relief groove -bd 1
    pack $ec_tracer.filter.settings -side top -ipadx 3 -ipady 3 -pady 5 -padx 5 -fill x 

    set row 0
    set col 0
    set cols 7

    label $ec_tracer.filter.settings.ports  -anchor w -text "Port types:"
    grid $ec_tracer.filter.settings.ports -columnspan $cols -sticky ew
    incr row


    foreach port $tkecl(portlist) {
	checkbutton $ec_tracer.filter.settings.port_$port -text $port -variable tkecl(filter_port,$port)
	grid $ec_tracer.filter.settings.port_$port -row $row -column $col -sticky w
	set col [expr ($col+1)%$cols]
	set row [expr $col?$row:$row+1]
    }
    set w $ec_tracer.filter.settings.portsets
    combobox $w -labeltext Tick -click single -editable 0 \
	-listheight [llength $tkecl(portsets)] -width 8 \
	-postcommand [list tkecl:combo_add_portsets $w] \
	-command tkecl:tick_portset
    grid $w -row $row -column $col -sticky w


    frame $ec_tracer.filter.predsettings -relief groove -bd 1
    pack $ec_tracer.filter.predsettings -side top -ipadx 3 -ipady 3 -pady 5 -padx 5 -fill x 

    set row 0
    set col 0
    set cols 5

    label $ec_tracer.filter.predsettings.predtypetitle -text "Predicate specification:"
    grid $ec_tracer.filter.predsettings.predtypetitle -columnspan $cols -sticky w
    incr row

    radiobutton $ec_tracer.filter.predsettings.predtype1 -text "Any predicate" \
	    -variable tkecl(filter_predtype) -value any -command "tkecl:fields_disable $ec_tracer" \

    grid $ec_tracer.filter.predsettings.predtype1 -columnspan $cols -sticky w
    incr row
    radiobutton $ec_tracer.filter.predsettings.predtype2 -text "Any predicate with a spypoint or call with a breakpoint" \
	    -variable tkecl(filter_predtype) -value anyspy -command "tkecl:fields_disable $ec_tracer"
    grid $ec_tracer.filter.predsettings.predtype2 -columnspan 5 -sticky w
    incr row
    radiobutton $ec_tracer.filter.predsettings.predtype3 -text "Specific predicate instance:" \
	    -variable tkecl(filter_predtype) -value goalmatching -command "tkecl:enable_pred $ec_tracer"
    grid $ec_tracer.filter.predsettings.predtype3 -columnspan $cols -sticky w

    incr row

    label $ec_tracer.filter.predsettings.predmodule2label -text "Defining module:"
    label $ec_tracer.filter.predsettings.blank -text " "

    label $ec_tracer.filter.predsettings.predmatchlabel -text "Goal template:"

    grid x $ec_tracer.filter.predsettings.predmodule2label $ec_tracer.filter.predsettings.blank $ec_tracer.filter.predsettings.predmatchlabel -sticky w 

    incr row

    combobox $ec_tracer.filter.predsettings.predmodule2combo -click single -listheight 6 -width 15 -editable 0 \
	-postcommand [list tkecl:combo_add_modules $ec_tracer.filter.predsettings.predmodule2combo] \
	-textvariable tkecl(filter_predmodule2)

    label $ec_tracer.filter.predsettings.predmodule2colon -text ":"


    ventry $ec_tracer.filter.predsettings.predmatch  -textvariable tkecl(filter_predmatch) -state disabled -width 40

    grid x $ec_tracer.filter.predsettings.predmodule2combo $ec_tracer.filter.predsettings.predmodule2colon $ec_tracer.filter.predsettings.predmatch -sticky w 

    incr row

    label $ec_tracer.filter.predsettings.predconditionlabel -text "Condition:"

    grid x $ec_tracer.filter.predsettings.predconditionlabel -sticky w 

    incr row

    ventry $ec_tracer.filter.predsettings.predcondition  -textvariable tkecl(filter_predcondition) -state disabled -width 70

    grid x $ec_tracer.filter.predsettings.predcondition -columnspan 3 -sticky w 
    incr row

    label $ec_tracer.filter.predsettings.predmodulelabel -text "Calling module:"

    grid x $ec_tracer.filter.predsettings.predmodulelabel -sticky w  

    incr row

    combobox $ec_tracer.filter.predsettings.predmodule -click single -listheight 6 -width 15 -editable 1 \
	-postcommand [list tkecl:combo_add_modules $ec_tracer.filter.predsettings.predmodule] \
	-textvariable tkecl(filter_predmodule)

    grid x $ec_tracer.filter.predsettings.predmodule -sticky w  


    tkecl:fields_disable $ec_tracer


    pack [frame $ec_tracer.filter.after -relief groove -bd 1] \
	 -side top -ipadx 3 -ipady 3 -pady 5 -padx 5 -fill x 
    pack [frame $ec_tracer.filter.after.hits] -fill x
    pack [label $ec_tracer.filter.after.hits.left -text "Conditions already met "] -side left
    pack [label $ec_tracer.filter.after.hits.hits -textvariable tkecl(filter_hits)] -side left
    pack [label $ec_tracer.filter.after.hits.right -text " times using this filter."] -side left

    
    pack [frame $ec_tracer.filter.after.count] -fill x
    pack [label $ec_tracer.filter.after.count.label -text \
	      "Stop after the conditions have been met"] -side left
    pack [ventry $ec_tracer.filter.after.count.entry \
	      -vcmd {regexp {^[0-9]*$} %P} \-validate key -invalidcmd bell \
	      -width 10 -textvariable tkecl(filter_count) -bg white \
	 ] -side left
    pack [label $ec_tracer.filter.after.count.endlabel -text "time(s)."] -side left

    button $ec_tracer.filter.go -text "Go" -state $tkecl(tracer_state) \
	-command {tkecl:set_tracercommand filter}
    balloonhelp $ec_tracer.filter.go "Continue program execution until filter conditions hold"
    button $ec_tracer.filter.close -text "Close" -command "wm withdraw $ec_tracer.filter"
    pack $ec_tracer.filter.go $ec_tracer.filter.close -side left -expand 1 -fill x

    focus [$ec_tracer.filter.depthsettings.mininvoc subwidget entry]
    return $ec_tracer.filter
}

proc tkecl:combo_add_portsets {w} {
    global tkecl
    foreach portset $tkecl(portsets) {
	$w add $portset
    }
}

proc tkecl:tick_portset {portset} {
    global tkecl

    foreach port $tkecl(portlist) {
	set tkecl(filter_port,$port) 0
    }
    foreach port $tkecl(portset,$portset) {
	set tkecl(filter_port,$port) 1
    }
}

proc tkecl:configure_pred {} {
    global tkecl

    set changed 0

    if {$tkecl(filter_predcondition) == ""} then {
	set usepredcondition true 
    } else {
	set usepredcondition $tkecl(filter_predcondition)
    }
    if {$tkecl(filter_predmatch) == ""} then {
	set usepredmatch "_" 
    } else {
	set usepredmatch $tkecl(filter_predmatch)
    }
    if {$tkecl(filter_predmodule) == ""} then {
	set usepredmodule "_" 
    } else {
	set usepredmodule $tkecl(filter_predmodule)
    }

    foreach filterprop $tkecl(filterpred,changable) {
	if [tkecl:check_if_changed $filterprop] { incr changed }
    }

    # set_usepred_info($usepredmatch,
    #                  $usepredmodule,
    #                  $usepredmodule2,
    #                  $usepredcondition, 
    #                  Status)
    if {$changed > 0} {
	# predmodule2 cannot be undefined: it is taken from a list of modules
	# the eclipse side code also assumes it cannot be a variable
	set res [ec_rpcq [list set_usepred_info \
		 $usepredmatch $usepredmodule $tkecl(filter_predmodule2) $usepredcondition _] \
		 (SSSS_) tracer_tcl]

	switch $res {
	    fail  -
	    throw {
		tk_messageBox -icon error -type ok -message "Filter Error: Exception raised when setting the conditional goal filter. Please check goal template/condition for syntax error."
		set status error
	    }
	    default {
		set status [lindex $res 5]
		if {$status == "not_found"} {
		    tk_messageBox -icon warning -type ok -message "Filter Error: Failed to set conditional goal filter. Goal template or module may be undefined."
		    ;# treat as an error
		    set status error
		}
	    }
	}
	set tkecl(filter,status) $status
    } elseif {$tkecl(filter,status) != "error"} {
	# enable filter goal
	set res [ec_rpcq reenable_usepred () tracer_tcl]
	switch $res {
	    fail  -
	    throw {
		tk_messageBox -icon error -type ok -message "Filter Error: Exception raised when setting the conditional goal filter. Please check goal template/condition for syntax error."
		set tkecl(filter,status) error
	    }
	    default {
		set tkecl(filter,status) continue
	    }
	}
}

    return $tkecl(filter,status) 
}


proc tkecl:fields_disable {ec_tracer} {
    $ec_tracer.filter.predsettings.predmatch configure -state disabled 
    $ec_tracer.filter.predsettings.predmatch config -foreground darkgray 
    $ec_tracer.filter.predsettings.predmatch config -background lightgray 
    $ec_tracer.filter.predsettings.predmodule configure -state disabled 
    $ec_tracer.filter.predsettings.predmodule config -foreground darkgray 
    $ec_tracer.filter.predsettings.predmodule config -background lightgray 
    $ec_tracer.filter.predsettings.predmodule2combo configure -state disabled 
    $ec_tracer.filter.predsettings.predmodule2combo config -foreground darkgray
    $ec_tracer.filter.predsettings.predmodule2combo config -background lightgray 
    $ec_tracer.filter.predsettings.predcondition configure -state disabled 
    $ec_tracer.filter.predsettings.predcondition config -foreground darkgray 
    $ec_tracer.filter.predsettings.predcondition config -background lightgray

    $ec_tracer.filter.settings.port_fail configure -state normal
    $ec_tracer.filter.settings.port_leave configure -state normal

}

proc tkecl:enable_pred {ec_tracer} {
    global tkecl

    $ec_tracer.filter.predsettings.predmatch configure -state normal 
    $ec_tracer.filter.predsettings.predmatch config -foreground black 
    $ec_tracer.filter.predsettings.predmatch config -background white 
    $ec_tracer.filter.predsettings.predmodule configure -state normal 
    $ec_tracer.filter.predsettings.predmodule configure -editable 1
    $ec_tracer.filter.predsettings.predmodule config -foreground black 
    $ec_tracer.filter.predsettings.predmodule config -background white 
    $ec_tracer.filter.predsettings.predmodule2combo configure -state normal 
    $ec_tracer.filter.predsettings.predmodule2combo configure -editable 0
    $ec_tracer.filter.predsettings.predmodule2combo config -foreground black 
    $ec_tracer.filter.predsettings.predmodule2combo config -background white 
    $ec_tracer.filter.predsettings.predcondition configure -state normal 
    $ec_tracer.filter.predsettings.predcondition config -foreground black 
    $ec_tracer.filter.predsettings.predcondition config -background white 

    set tkecl(filter_port,fail) 0
    $ec_tracer.filter.settings.port_fail configure -state disabled
    set tkecl(filter_port,leave) 0
    $ec_tracer.filter.settings.port_leave configure -state disabled

}


proc tkecl:observe_goal {invoc} {

    tkinspect:inspect_command invoc($invoc) [list record_observed invoc($invoc) [list 1] Invocation:$invoc] {S[S*]S}
}


#---------------------------------------------------------------
# Directory selection
#---------------------------------------------------------------
proc tkecl:get_newcwd {} {
    tkecl:newcwd [tkecl:getDirectory [pwd] "Set Current Working Directory"]
}

# change eclipse's cwd and set $tkecl(cwd) to its eclipse name
proc tkecl:newcwd {newdir} {
    global tkecl

    if {![string match "" $newdir]} {
	set tkecl(cwd) [lindex [ec_rpcq [list os_file_name _ $newdir] {(_S)}] 1]
	;# cd now done in ECLiPSe to ensure that it is the ECLiPSe side's
	;# cwd that is changed
	switch [ec_rpcq [list cd $tkecl(cwd)] {(S)}] {
	    fail -
	    throw {
		tk_messageBox -icon warning -type ok -message "Unable to set current directory to $newdir"
	    }
	}
	
    }
}

proc tkecl:paths_menu {p name} {
    set menu [menu $p.m -tearoff 0 -postcommand [list tkecl:build_path_menu $p.m $p $name]]
}

proc tkecl:build_path_menu {menu p name} {
    global tkecl

    $menu delete 0 end ;# get rid of old entries
    $menu add command -label "Add a new directory" -command \
	    [list tkecl:add_new_path $name]
    $menu add separator

    set i 0
    foreach {item} $tkecl($name) {
	;# probably treat all spaces as breaks in name!
	$menu add command -label $item -command [list tkecl:change_one_path $name $p $item $i]
	incr i
    }
}
	
proc tkecl:add_new_path {name} {
    global tkecl

    tkecl:gui_edit_one_path Insert $name [pwd] 0 

    if {[llength $tkecl($name)] != 0} {
	ec_rpcq [list set_flag $name $tkecl($name)] {(()[S*])}
    }

}

proc tkecl:getDirectory {initdir title} {
    return [tkecl:get_path_popup $initdir directory \
		[list tk_chooseDirectory -initialdir $initdir -title $title]]
}

proc tkecl:getEcFile {initdir title} {
    global tkecl

    # we used to have -initialfile $tkecl(last_source_file), but that
    # overrides -initialdir, and is not available on Aqua Tk (b418)
    set tkecl(last_source_file) \
	[tkecl:get_path_popup $initdir "file" [list tk_getOpenFile \
	       -defaultextension $tkecl(pref,defaultextension) \
	       -filetypes $tkecl(filetypes) -title $title \
	       -initialdir $initdir \
	       ] \
	]
    return $tkecl(last_source_file)
}

# like tkecl:getEcFile but allows non-existing files to be selected
# note that underlying widget has `Save' for the select button, and also
# a warning about overwritting the file if the file already exists.
# *No* file is saved, only the filename is returned. Should try and see
# if we can disable this `feature'
proc tkecl:getNewEcFile {initdir title} {
    global tkecl

    set tkecl(last_source_file) \
    	[tkecl:get_path_popup $initdir "file" [list tk_getSaveFile \
               -defaultextension $tkecl(pref,defaultextension) \
	       -filetypes $tkecl(filetypes) -title $title -initialdir $initdir \
	       ] \
	]
    return $tkecl(last_source_file)
}


# only allow a GUI path selection if embedded, or if Tcl side has same host as 
# ECLiPSe side, as filespace may be different otherwise
proc tkecl:get_path_popup {initpath pathtype browsecmd} {
    global tkecl

    set echostname [lindex [ec_rpcq [list get_flag hostname _] (()_)] 2]
    if {([ec_interface_type] == "embedded") ||
       ([string compare [info hostname] $echostname] == 0)} {
	    return [eval $browsecmd]

    } else {
	;# ask user to type in path name instead
	set tkecl(get_path_name) $initpath
	set gdir [toplevel .ec_tools.get_path]
	wm title $gdir "Get $pathtype name"
	pack [frame $gdir.bf] -side bottom -expand true -fill x
	pack [entry $gdir.e -relief sunken -width 25 -textvariable tkecl(get_dir_name)] -side right -expand true -fill x
	pack [label $gdir.l -text "Please type in the $pathtype name"] -side left
	pack [button $gdir.bf.ok -command "destroy $gdir" -text OK] -side left -expand true -fill x
	pack [button $gdir.bf.cancel -text Cancel -command "set tkecl(get_path_name) {}; destroy $gdir"] -side right -expand true -fill x
	bind $gdir.e <Return> "destroy $gdir"
	$gdir.e xview moveto 1.0
	$gdir.e icursor end
	focus $gdir.e
	tkwait window $gdir
	return $tkecl(get_path_name)
    }
}

proc tkecl:change_one_path {name p item i} {
    global tkecl

    set w $p.change

    if ![winfo exists $w] {
	set old [focus]
	set tkecl(path_to_change) [lindex [ec_rpcq [list os_file_name $item _] \
		(S_)] 2]
	toplevel $w
	wm title $w "Change one path for $name"
	tkwait visibility $w
	focus $w
	grab $w
	pack [entry $w.e -bg white -width 40 -textvariable tkecl(path_to_change) \
		-relief sunken] -side top -expand 1 -fill both
	bind $w.e <Return> [list tkecl:perform_path_change Replace $name \
		$tkecl(path_to_change) $i]
	pack [button $w.replace -command [list tkecl:gui_edit_one_path Replace $name\
		$item $i] -text Replace] -side left -expand 1 -fill both
	pack [button $w.delete -command [list tkecl:perform_path_change Delete $name \
		$item $i] -text Delete] -side left -expand 1 -fill both
	pack [button $w.insert -command [list tkecl:gui_edit_one_path Insert $name \
		$item $i] -text Insert] -side left -expand 1 -fill both
	pack [button $w.cancel -text Cancel -command "destroy $w; set tkecl($name) [list $tkecl($name)]"] -side left -expand 1 -fill both
    }
    tkwait variable tkecl($name)

    if {[llength $tkecl($name)] == 0} {
	ec_rpcq [list set_flag $name $tkecl($name)] {(()[])}
    } else {
	ec_rpcq [list set_flag $name $tkecl($name)] {(()[S*])}
    }
    grab release $w
    focus $old
    destroy $w

}

proc tkecl:gui_edit_one_path {action name path i} {
    global tkecl

    set path [lindex [ec_rpcq [list os_file_name $path _] (S_) ] 2]
    set new [tkecl:getDirectory $path "$action a path"]
    if ![string match "" $new] {
	set new [lindex [ec_rpcq [list os_file_name _ $new] (_S) ] 1]
	tkecl:perform_path_change $action $name $new $i
    } else {
	set tkecl($name) $tkecl($name) ;# make sure that tkwait does get its `changes'
    }
}

proc tkecl:perform_path_change {action name new i} {
    global tkecl

    switch -exact -- $action {
	Replace {
	    set tkecl($name) [lreplace $tkecl($name) $i $i $new]
	}
	Insert {
	    set tkecl($name) [linsert $tkecl($name) $i $new]
	}
	Delete {
	    set tkecl($name) [lreplace $tkecl($name) $i $i]
	}
    }
}

#---------------------------------------------------------------
# Change Output mode
#---------------------------------------------------------------

proc tkecl:Set_output_mode {popmode return} {
    global outputmodes

    bind $popmode <Enter> {focus %W}
    foreach {f modes status descr unsetd triopts tridesc tristatus} [lindex $return 2] {
	set i -1 
	foreach m $modes s $status d $descr u $unsetd { 
	    set l $m
	    if [string match "." $m] {set m period} ;# catch special chars. here
	    set outputmodes($popmode.l$m) $s
	    set outputmodes($popmode.l$m,set) $d
	    set outputmodes($popmode.l$m,unset) $u
	    incr i
	    grid [checkbutton $popmode.c$m -onvalue 1 -offvalue 0 -text $l\
		    -anchor w -variable outputmodes($popmode.l$m) -command "tkecl:Change_output_options $m $popmode.l$m"] \
		    -sticky news -row $i -column 0
	    if {$s == 1} {
		set label $d
	    } else {
		set label $u
	    }
	    grid [label $popmode.l$m -text $label] -sticky w -row $i -column 1
	    bind $popmode <Key-$m> {
		regexp {^(.+)\.[^\.]+$} %W null parent
		set lw $parent.l%K
		if {$outputmodes($lw) == 1} {
		    set outputmodes($lw) 0
		} else {
		    set outputmodes($lw) 1
		}
		tkecl:Change_output_options %K $lw
	    }

#	    balloonhelp $popmode.c$m $d
	} ;# foreach m ...

	set trinames ""
	foreach tri0 $triopts tdes0 $tridesc s $tristatus {
	    incr i
	    set f [frame $popmode.c$i]
	    set tri [lrange $tri0 1 end] ;# drop functor
	    set tdes [lrange $tdes0 1 end]
	    set name ""
	    append name [lindex $tri 0] [lindex $tri 1]
	    lappend trinames $name
	    set j 0
	    set outputmodes($popmode,t$name) $s
	    set outputmodes($popmode,t$name,s) $tri
	    set outputmodes($popmode,t$name,d) $tdes
	    foreach mode $tri d $tdes {
		grid [radiobutton $f.b$mode -variable outputmodes($popmode,t$name) \
			-text $mode -value $mode -anchor w\
		        -command "tkecl:Change_output_trioptions $mode $name \
			    $popmode.l$name $popmode"] -row 0 -column $j
		incr j
		if [string match $mode $s] {
		    grid [label $popmode.l$name -text $d] -sticky w -row $i -column 1
		}
	    }
	    grid $f -sticky news -row $i -column 0
	}
	grid [button $popmode.end -command "destroy $popmode" -text Set] \
	        -sticky news -row [expr $i + 1] -column 0 -columnspan 2
#	        -sticky news -row [expr ($i/3) + 1] -column 0 -columnspan 3
    }
    tkwait window $popmode
    set newmodes "\""
    foreach m $modes {
	set l $m
	if [string match "." $m] {set m period} ;# catch special chars. here
	if {$outputmodes($popmode.l$m) == 1} {
	    append newmodes $l
	}
    }
    foreach name $trinames { ;# add in the tristate modes
	if {![string match $outputmodes($popmode,t$name) off]} {
	    append newmodes $outputmodes($popmode,t$name)
	}
    }
    return [append newmodes \"]

}

# update label for the simple output options
proc tkecl:Change_output_options {mode w} {
    global outputmodes

    ;# called after mode has been changed to new value
    if {$outputmodes($w) == 1} {
	$w configure -text $outputmodes($w,set)
    } else {
	$w configure -text $outputmodes($w,unset)
    }
}

# update label for the tri-state options
proc tkecl:Change_output_trioptions {selected name label w} {
    global outputmodes

    foreach opt $outputmodes($w,t$name,s) d $outputmodes($w,t$name,d) { 
	;# find the one that matches selected
	if [string match $selected $opt] {
	    $label configure -text $d
	}
    }
}


#----------------------------------------------------------------------
# Compile note pad
#----------------------------------------------------------------------

proc tkecl:compile_pad {} {

    set w .ec_tools
    if [winfo exists $w.cpad] {
	tkinspect:RaiseWindow $w.cpad
	return
    }
    set pad [toplevel $w.cpad]
    wm title $pad "Compile scratch-pad"
    text $pad.t -wrap none -bg white -yscrollcommand "$pad.vscroll set" -xscrollcommand "$pad.hscroll set" 
    set bbar [frame $pad.bbar]
    pack $bbar -side bottom -fill x
       pack [button $bbar.com -text "Compile All" -command "tkecl:do_compile_all $pad.t"] -side left -expand 1 -fill x
       pack [button $bbar.sel -text "Compile Selection" -command "tkecl:do_compile_sel $pad.t"] -side left -expand 1 -fill x
       pack [button $bbar.end -text Close -command "wm withdraw $w.cpad"] -side left -expand 1 -fill x
    pack [scrollbar $pad.vscroll -command "$pad.t yview"] -side right -fill y
    pack [scrollbar $pad.hscroll -command "$pad.t xview" -orient horizontal] -side bottom -fill x
    pack $pad.t -expand 1 -fill both
    bind $pad <Alt-h> "tkecl:Get_helpfileinfo scra $pad"
    balloonhelp $bbar "Type in (short) ECLiPSe code for compilation. Can compile everything in window, or only selection."
    focus $pad.t

}

proc tkecl:do_compile_all {t} {
    ec_rpcq_check [list compile_string [$t get 1.0 end]] (S) tracer_tcl
}

proc tkecl:do_compile_sel {t} {
    foreach {start end} [$t tag ranges sel] {
       ec_rpcq_check [list compile_string [$t get $start $end]] (S) tracer_tcl
    }
}


#----------------------------------------------------------------------
# Statistics display
#----------------------------------------------------------------------
proc tkecl:handle_statistics {} {
    global tkecl

    tkecl:create_stat_window
    set data [lindex [ec_rpcq_check [list report_stats $tkecl(pref,stats_interval) _] (D_) tracer_tcl] 2] 
    tkecl:display_stat $data
}

proc tkecl:display_stat {data} {
    global tkecl

    ;# colours are in pairs: dark and light versions
    set ec_stats .ec_tools.ec_stats
    if ![winfo exists $ec_stats] {
	return
    }

    set colours [list #00d040 #00f090 #c00000 #f00000 #c0c000 #ffff00 \
	    #b000b0 #f000f0  #c07000 #ff9000 #50d0b0 #a0ffe0 #000090 #0000ff]
    set cindex 0
    set h 85   ;# these are for the pie charts
    set w 85
    foreach item  $data {
        switch -exact -- [lindex $item 0] {
	    times  {
		set user [lindex $item 1]
		set real [lindex $item 2]
		foreach {gctime ngc gccol gcratio} [lrange [lindex $item 3] 1 end] {
		    break
		}
		set tframe $ec_stats.times
		set textf $tframe.text
		set pie $tframe.pie
		if ![winfo exists $tframe] {
		    pack [frame $tframe] -side top
		    pack [canvas $pie -width [expr $w + 10] -height [expr $h + 10]] -side left
		    pack [frame $textf] -side right
		    pack [frame $textf.times -relief ridge -borderwidth 3] -side top -padx 2 -pady 2
		    grid [label $textf.times.a -text "total time" -width 15 -anchor e] -row 1 -column 0
		    grid [label $textf.times.b -text "gc time" -width 15 -anchor e] -row 1 -column 1
		    grid [label $textf.times.c -text "\% user" -width 10 -anchor e] -row 1 -column 2
		    grid [label $textf.times.user -width 15 -anchor e] -row 2 -column 0
		    grid [label $textf.times.gc -width 15 -anchor e] -row 2 -column 1 -padx 2 -pady 2
		    grid [label $textf.times.userf -width 10 -anchor e] -row 2 -column 2 -padx 2 -pady 2
		    grid [label $textf.times.label -text "User CPU Time"] -row 0 -column 0 -columnspan 2 -sticky news
		    pack [frame $textf.gc -relief ridge -borderwidth 3] -side bottom
		    grid [label $textf.gc.a -text "total collected" -width 16 -anchor e] -row 1 -column 0
		    grid [label $textf.gc.b -text "\# gc" -width 9 -anchor e] -row 1 -column 1
		    grid [label $textf.gc.c -text "% recovered" -width 15 -anchor e] -row 1 -column 2
		    grid [label $textf.gc.col -width 16 -anchor e] -row 2 -column 0
		    grid [label $textf.gc.ngc -width 9 -anchor e] -row 2 -column 1
		    grid [label $textf.gc.ratio -width 15 -anchor e] -row 2 -column 2
		    grid [label $textf.gc.label -text "Garbage Collection"] -row 0 -column 0 -columnspan 3 -sticky news
		    set tkecl(stat,times,user) 0
		    set tkecl(stat,times,real) 0
		    balloonhelp $pie "Portion of total time spent on garbage collection with respect to total user CPU time"
		    balloonhelp $textf.gc "Garbage collection statistics"
		    balloonhelp $textf.times "Timing statistics"
		}
		    
		$textf.times.user configure -text "$user"
		$textf.times.gc configure -text "[expr round($gctime*100)/100.0]"
		$textf.times.userf configure -text \
		    "[expr round( ($user - $tkecl(stat,times,user)) / \
		    ($real - $tkecl(stat,times,real)) * 10000) / 100.0]"
		set tkecl(stat,times,user) $user 
		set tkecl(stat,times,real) $real

		$textf.gc.ngc configure -text "$ngc"
		$textf.gc.ratio configure -text "[expr round($gcratio*100)/100.0]"
		$textf.gc.col configure -text "$gccol"
		$pie create oval 10 10 $h $w -fill white
		if {$ngc != 0} {
		    set extent [expr -360*$gctime/$user]
		    $pie create arc 10 10 $h $w -start 90 -extent $extent -style pieslice -fill blue
		}
	    }

	    memory {
		set total [lindex $item 2]
		set mname [lindex $item 1]
		set ref   [lindex $item 3] 
		set mframe $ec_stats.$mname
		set pie $mframe.pie
		set textf $mframe.text
		if ![winfo exists $mframe] {
		    pack [frame $mframe -relief sunken -borderwidth 2] -side top
		    pack [canvas $pie -width [expr $w + 20] -height [expr $h + 10]] -side left
		    pack [frame $textf] -side right
		    pack [frame $textf.headings] -side top -expand 1 -fill x
		    grid [label $textf.headings.main -text [string toupper $mname 0 0] -anchor w] -row 0 -column 0 -columnspan 4 -sticky news
		    grid [label $textf.headings.a -text {} -width 8 -anchor e] -row 1 -column 0 -sticky news
		    grid [label $textf.headings.b -text used -width 11 -anchor e] -row 1 -column 1 -sticky news
		    grid [label $textf.headings.c -text alloc -width 11 -anchor e] -row 1 -column 2 -sticky news
		    grid [label $textf.headings.d -text peak -width 11 -anchor e] -row 1 -column 3 -sticky news

		    balloonhelp $textf "Memory statistics (in bytes) for the $mname memory area"
		    balloonhelp $pie "Proportion of memory used/allocated in the $mname area with respect to $ref"

		}
		$pie create oval 10 10 $h $w -fill white
		
		set direction -1.0
		foreach component [lrange $item 4 end] {
		    switch -exact -- [lindex $component 0] {
			stack {
			    foreach {cname alloc used peak} [lrange $component 1 end] {
				break
			    }
			    # without round() here we get funny effects with the pie charts on Windows
			    set startused 90
			    set extentused [expr round($direction*$used/$total*360)]
			    set startfree [expr $startused + $extentused]
			    set extentfree [expr round($direction*($alloc-$used)/$total*360)]
			    set dcol [lindex $colours $cindex]
			    incr cindex 1
			    set lcol [lindex $colours $cindex]
			    incr cindex 1

			    set cframe $textf.$cname
			    if ![winfo exists $cframe] {
				pack [frame $cframe] -side bottom -expand 1 -fill x
				grid [label $cframe.name -text $cname -width 8 -anchor e] -row 0 -column 0 -sticky news
				grid [label $cframe.used -foreground $dcol -width 11 -anchor e] -row 0 -column 1 -sticky news
				grid [label $cframe.alloc -foreground $lcol -width 11 -anchor e] -row 0 -column 2 -sticky news 
				grid [label $cframe.peak -width 11 -anchor e] -row 0 -column 3 -sticky news 
			    }
			    $cframe.alloc configure -text $alloc
			    $cframe.used configure -text  $used
			    $cframe.peak configure -text  $peak

			    $pie create arc 10 10 $h $w -start $startused -extent $extentused -style pieslice -fill $dcol
			    $pie create arc 10 10 $h $w -start $startfree -extent $extentfree -style pieslice -fill $lcol
			}
		    }
		    set direction [expr -$direction]
		}
	    }
	}
    }
}

proc tkecl:create_stat_window {} {

    set ec_stats .ec_tools.ec_stats
    if {![winfo exists $ec_stats]} {
	toplevel $ec_stats
	wm title $ec_stats "ECLiPSe statistics"
	wm resizable $ec_stats 0 0
	pack [frame $ec_stats.buttons] -side bottom -expand 1 -fill x
	pack [button $ec_stats.buttons.change -command "tkecl:change_stat_interval" -text "Change interval"] -side left -expand 1 -fill x
	pack [button $ec_stats.buttons.close -command "tkecl:kill_stat_window" -text "Close"] -side right -expand 1 -fill x
	bind $ec_stats <Alt-h> "tkecl:Get_helpfileinfo stat $ec_stats"

	balloonhelp $ec_stats.buttons.change "Change the time interval with which the statistics are updated"
	balloonhelp $ec_stats.buttons.close "Close this window and quit monitoring statistics"
    } else {
	tkinspect:RaiseWindow $ec_stats
    }
}


proc tkecl:change_stat_interval {} {
    global tkecl

    set tkecl(stats_interval1) $tkecl(pref,stats_interval)
    set w .ec_tools.ec_stats.interval
    if {![winfo exists $w]} {
	toplevel $w
	wm title $w "Statistics Reporting Interval"
	pack [frame $w.f] -side top
	pack [label $w.f.l -text "New reporting interval (sec.)"] -side left
	pack [entry $w.f.e -relief sunken -width 10 -textvariable tkecl(stats_interval1)] -side right -expand 1 -fill both
	pack [button $w.set -text "Set" -command "tkecl:set_stat_interval $w"] -side left -fill x -expand 1
	pack [button $w.cancel -text "Cancel" -command "destroy $w"] -side left -fill x -expand 1
	bind $w.f.e <Return> "tkecl:set_stat_interval $w"
	focus $w.f.e

	balloonhelp $w "Change time interval at which the statistics are \
		updated in the statistics window.\nType in a positive number \
		and click `Set' to change, or `Cancel' to not change"
    } else {
	tkinspect:RaiseWindow $w
	focus $w.f.e
    }
}

proc tkecl:set_stat_interval {w} {
    global tkecl

    if [regexp {^([0-9]+[.][0-9]+)|([0-9]+)$} $tkecl(stats_interval1)] {
	set tkecl(pref,stats_interval) $tkecl(stats_interval1)
	ec_rpcq_check [list change_report_interval $tkecl(pref,stats_interval)] (D) tracer_tcl
	destroy $w
    } else {
	set tkecl(stats_interval1) $tkecl(pref,tats_interval)
	bell 
    }
}

proc tkecl:kill_stat_window {} {
    ec_rpcq stop_report_stats () tracer_tcl
    destroy .ec_tools.ec_stats
}

proc tkecl:handle_stats_report {stream {length {}}} {
    tkecl:display_stat [ec_read_exdr [ec_streamnum_to_channel $stream]]
}

#----------------------------------------------------------------------
# Grace-style term matrix display
#----------------------------------------------------------------------
proc tkecl:handle_mat_flush {stream {length {}}} {
    global tkecl_displayvals

    set commandline [ec_read_exdr [ec_streamnum_to_channel $stream]]
    set command [lindex $commandline 0]
    ;#puts "line-$commandline"
    set name [lindex $commandline 1] ;# name is the numeric identifier for matrix

    set ec_matdisplay .ec_tools.ec_matdisplay$name
    if {![winfo exists $ec_matdisplay]} {
	if {[string match setup $command]} { ;# initial setup
	    foreach {ecname row col module} [lrange $commandline 2 end] {
		append title $ecname "@" $module
		set tkecl_displayvals($name,ecname) $ecname
		set tkecl_displayvals($name,module) $module
		tkecl:setup_disptable $name $title $row $col
	    }
	    return
	} else {
	    ;# matrix display window not there, and we are not initialising
	    ;# been kill explicitly, do not redisplay
	    return
	}
    }
		
    switch -exact -- $command {
	setup {
	    tk_messageBox -type ok -message "Display matrix protocol error: trying to initialise existing matrix"
	}

	disp {

	    foreach {row col new ground back} [lrange $commandline 2 end] {
		if {$tkecl_displayvals($name,$row,$col,stop) == 1}  {
		    append id r $row c $col
		    set tkecl_displayvals($name,$row,$col,prev) \
			   [lindex [$ec_matdisplay.$id config -text] end]
		    if {$tkecl_displayvals($name,update) == 0 && \
			    [string match nonground $ground]} {
			return
		    }
		    $ec_matdisplay.$id config -text $new
		    if [string match $back back] {
			;#set tkecl_displayvals($name,back) 1
			set tkecl_displayvals($name,back) [list $row $col]
			set colour pink
		    } else {
			;#set tkecl_displayvals($name,back) 0
			set colour beige
		    }
		    $ec_matdisplay.$id config -foreground black
		    $ec_matdisplay.$id config -background $colour
		    $ec_matdisplay.b.cont configure -state normal
		    tkinspect:RaiseWindow $ec_matdisplay
		    tkwait variable tkecl_displayvals($name,cont)
		    set tkecl_displayvals($name,back) [list 0 0]
		    if [winfo exists $ec_matdisplay] {
			$ec_matdisplay.$id config -background lightgray
			$ec_matdisplay.b.cont configure -state disabled
		    }
		} elseif {(($tkecl_displayvals($name,update) == 1) ||
		    ![string match nonground $ground])} {
			append id r $row c $col
			set tkecl_displayvals($name,$row,$col,prev) \
			   [lindex [$ec_matdisplay.$id config -text] end]
			$ec_matdisplay.$id config -text $new
		    if [string match $back back] {
			$ec_matdisplay.$id config -foreground red
		    } else {
			$ec_matdisplay.$id config -foreground black
		    }
 		}
	    }

	}

	interact {
	    $ec_matdisplay.b.cont configure -state normal
	    tkwait variable tkecl_displayvals($name,cont)
	    if [winfo exists $ec_matdisplay] {
		$ec_matdisplay.b.cont configure -state disabled
	    }
	}

	kill { 
	    destroy $ec_matdisplay
	}
	    
	    
    }
}

proc tkecl:setup_disptable {name title row col} {
    global tkecl_displayvals

    set tkecl_displayvals($name,cont) 0
    ;#set tkecl_displayvals($name,back) 0
    set tkecl_displayvals($name,back) [list 0 0]
    set parent [toplevel .ec_tools.ec_matdisplay$name]
    wm title $parent "Term display for $title"
    set tkecl_displayvals($name,row) $row
    set tkecl_displayvals($name,col) $col
    set tkecl_displayvals($name,update) 1
    bind $parent <Button-3> "tkecl:display_popup $parent %W $name $row %X %Y"
    bind $parent <Control-Button-1> "tkecl:display_popup $parent %W $name $row %X %Y"

    for {set i 1} {$i <= $row} {incr i 1} {
	grid [label $parent.r$i -text $i -relief groove -width 5 -fg red -bg lightblue] -row $i -column 0 -sticky news
    }
    for {set i 1} {$i <= $col} {incr i 1} {
	grid [label $parent.c$i -text $i -relief groove -width 15 -fg red -bg lightblue] -row 0 -column $i -sticky news
    }
    for {set i 1} {$i <= $row} {incr i 1} {
	for {set j 1} {$j <= $col} {incr j 1} {
	    set id ""
	    append id r $i c $j
	    grid [label $parent.$id -text "-- unknown --" -relief ridge -width 15] -row $i -column $j -sticky news
	    bind $parent.$id <Double-Button-1> "tkinspect:Inspect_term_init display($name,$i,$j)"
	    set tkecl_displayvals($name,$i,$j,stop) 0
	}
    }
    grid [frame $parent.b] -row [expr $row + 1] -column 0 -columnspan [expr $col + 1] -sticky news
    pack [button $parent.b.cont -text "Continue" -command \
	    "set tkecl_displayvals($name,cont) 1"] -side left -fill x -expand 1
    pack [button $parent.b.kill -text "Kill display" -command "destroy $parent"] -side right -fill x
    pack [checkbutton $parent.b.update -text "Update on ground" -variable \
	    tkecl_displayvals($name,update) -onvalue 0 -offvalue 1] \
	    -side right -fill x
    pack [button $parent.b.stop -text "stop all" -command \
	    "tkecl:all_mat_break 1 $name $row $col"] -side right -fill x
    pack [button $parent.b.go -text "stop none" -command \
	    "tkecl:all_mat_break 0 $name $row $col"] -side right -fill x

    bind $parent.b.kill <Destroy> "tkecl:kill_display_matrix $name"

    for {set j 1} {$j <= $col} {incr j 1} {
	grid columnconfigure $parent $j -weight 1
    }

    for {set j 1} {$j <= [expr $row]} {incr j 1} {
	grid rowconfigure $parent $j -weight 1
    }
    balloonhelp $parent "Monitor changes on terms: each matrix cell represents\
	     a term and show its current value.\n Right (or control-left) click on cell to get \
	     options. Double left click on cell to inspect\n the term in the \
             cell. Current and previous (pre-update) values are shown.\n \
             On break, changes due to forward execution shown in yellow,\
	    changes due to backtracking shown in pink."
    balloonhelp $parent.b.cont "Click to continue execution until next break-point.\n (if set, a break-point occurs when a cell is updated)"
    balloonhelp $parent.b.stop "Set break-points on all cells"
    balloonhelp $parent.b.go "Unset break-points on all cells"
    balloonhelp $parent.b.update "Control update events -- if set, only update when cell becomes ground.\n Otherwise, updates depends on make_display_matrix"
    balloonhelp $parent.b.kill "Click to kill this display matrix -- program will continue to run without the display matrix"
    bind $parent <Alt-h> "tkecl:Get_helpfileinfo disp $parent"
}


proc tkecl:kill_display_matrix {name} {
global tkecl_displayvals
# if needed, will go to ECLiPSe side to execute kill_display_matrix


  set tkecl_displayvals($name,cont) 1 
  ;# make sure execute will continue
  ;# clean up and remove all Tcl vars associated with this display matrix
  foreach matvar [array names tkecl_displayvals $name,*] {
      unset tkecl_displayvals($matvar)
  }
}

proc tkecl:all_mat_break {state name row col} {
    global tkecl_displayvals

    for {set i 1} {$i <= $row} {incr i 1} {
	for {set j 1} {$j <= $col} {incr j 1} {
	    set tkecl_displayvals($name,$i,$j,stop) $state
	}
    }
}

proc tkecl:display_popup {p w name nrow x y} {
    global tkecl_displayvals


    if [string match disabled [lindex [$p.b.cont configure -state] end]] {return}
    set widgetinfo [grid info $w]
    foreach {option value} $widgetinfo {
	set widget($option) $value
    }
    if {(![info exists widget(-row)] || $widget(-row) == 0 || $widget(-column) == 0)} {
	return
    }
    if [winfo exists $p.popup] {
	destroy $p.popup
    }
    set m [menu $p.popup -tearoff 0]
    $m add command -label "current: [lindex [$w configure -text] end]" ;#-state disabled
    $m add command -label "previous: $tkecl_displayvals($name,$widget(-row),$widget(-column),prev)" -state disabled
    $m add check -label "Break on updates" -onvalue 1 -offvalue 0 \
	    -variable tkecl_displayvals($name,$widget(-row),$widget(-column),stop)
    ;#if {$tkecl_displayvals($name,back) == 0} 
    foreach {brow bcol} $tkecl_displayvals($name,back) {break}
    if {$brow != $widget(-row) || $bcol != $widget(-column)} {
	    $m add command -label "Inspect this term" -command \
		    "tkinspect:Inspect_term_init display($name,$widget(-row),$widget(-column))"
    }

#    $m add command -label "row: $widget(-row) col: $widget(-column)"
    tk_popup $m $x $y
}

#----------------------------------------------------------------------
# Source Display
#----------------------------------------------------------------------

proc tkecl:setup_source_debug_window {} {
    global tkecl

    # setup source debug window, text display for source is not packed, as
    # it needs to have source text added before displaying it
    set ec_source .ec_tools.ec_tracer.tab.source
    set tkecl(source_debug,file) ""

    .ec_tools.ec_tracer.tab add "Source Context" -window [frame $ec_source] 
#    label $ec_source.label -text "Source Context"
    frame $ec_source.context -relief sunken -borderwidth 1 -bg white
    frame $ec_source.control

    pack $ec_source.context -side bottom -fill both -expand 1
#    pack $ec_source.label -side top  -fill x
    scrollbar $ec_source.context.vscroll -command "$ec_source.context.text yview" 
    scrollbar $ec_source.context.hscroll -command "$ec_source.context.text xview" -orient horizontal
    text $ec_source.context.lineno -borderwidth 0  -bg white -width 5 -wrap none -yscrollcommand [list tkecl:vscroll-sync "$ec_source.context.status $ec_source.context.text"]
    text $ec_source.context.status -borderwidth 0  -bg white -width 1 -wrap none -yscrollcommand [list tkecl:vscroll-sync "$ec_source.context.lineno $ec_source.context.text"]
    text $ec_source.context.text -borderwidth 0  -bg white -xscrollcommand "$ec_source.context.hscroll set" -wrap none -yscrollcommand [list tkecl:vscroll-sync "$ec_source.context.lineno $ec_source.context.status"]
    pack $ec_source.context.vscroll -side left -fill y 
    pack $ec_source.context.hscroll -side bottom -fill x 
    pack $ec_source.context.lineno -side left -fill y 
    pack $ec_source.context.status -side left -fill y 
    pack $ec_source.context.text -side right -fill both -expand 1
    bind $ec_source.context.text <Double-Button-1> \
	"tkecl:display_source_for_callport $ec_source.context.text; break"
    bind $ec_source.context.lineno <Any-Key> "tkecl:readonly_keypress %A"
    bind $ec_source.context.lineno <ButtonRelease-2> {break}

    bind $ec_source.context.status <Any-Key> "tkecl:readonly_keypress %A"
    bind $ec_source.context.status <ButtonRelease-2> {break}
    bind $ec_source.context.status <Button-1> "tkecl:toggle_breakpoint $ec_source.context.status; break"

    menu $ec_source.context.text.popupmenu -tearoff 0
    menu $ec_source.context.text.popupmenu.predmenu
    bind $ec_source.context.text <Any-Key> "tkecl:readonly_keypress %A"
    bind $ec_source.context.text <ButtonRelease-2> {break}
    bind $ec_source.context.text <Button-3> "tkecl:popup_sourcetext_menu $ec_source.context.text %X %Y; break"
    bind $ec_source.context.text <Control-Button-1> "tkecl:popup_sourcetext_menu $ec_source.context.text %X %Y; break"
    $ec_source.context.text tag configure call_style -foreground #7070ff \
	-underline 1 -font tkeclmonobold
    $ec_source.context.text tag configure exit_style -foreground #00b000 \
	-underline 1 -font tkeclmonobold
    $ec_source.context.text tag configure fail_style -foreground red \
	-underline 1 -font tkeclmonobold
    $ec_source.context.text tag configure ancestor_style -background lightblue \
	-relief raised -borderwidth 1
    $ec_source.context.text tag configure debug_line -background beige -relief raised -borderwidth 1
    $ec_source.context.text tag configure hidden_cr -elide 1
    $ec_source.context.text configure -cursor left_ptr 

    $ec_source.context.status tag configure on -foreground red 
    $ec_source.context.status tag configure off -foreground lightgray
    $ec_source.context.status configure -cursor left_ptr 
    $ec_source.context.lineno configure -cursor left_ptr 

    combobox $ec_source.control.select -click single -bg white -listheight 16 -editable 0 \
        -postcommand [list tkecl:get_source_debug_filenames $ec_source.control.select] \
	-textvariable tkecl(source_debug,file) -labeltext "File:" \
	-command tkecl:load_source_debug_file 

    pack $ec_source.control.select -side left -fill x -expand 1
    pack $ec_source.control -side bottom -fill x -expand 1

    .ec_tools.ec_tracer.tab activate "Source Context"

    balloonhelp $ec_source.context.text "Source context for execution traced by the tracer

 Display source file for debugging. Source line for
 most recent goal is highlighted, and the current
 goal is coloured in blue (call), green (success), or red (failure).

 Source context for ancestor goals can also be shown, 
 highlighted in blue. Hold right mouse button for a 
 popup menu.

 Double-click left mouse button on a port line to display 
 the source for the predicate called."

    balloonhelp $ec_source.context.status "Show port status for line in selected source file: a light gray '#' indicates a port line (not active)\n a red '#' indicates an active breakpoint\nClick left mouse button to toggle the setting of a nearby breakpoint."
    balloonhelp $ec_source.context.lineno "Show line numbers for selected source line"
    balloonhelp $ec_source.control.select "Select from popup list the source file to display"

    # tkwait visibility $ec_source  

}

# adapted from tkdiff
proc tkecl:vscroll-sync {windowlist y0 y1} {
    global tkecl

    set ec_sourcecon .ec_tools.ec_tracer.tab.source.context
    $ec_sourcecon.vscroll set $y0 $y1

    # if syncing is disabled, we're done. This prevents a nasty
    # set of recursive calls
    if {[info exists tkecl(disableSyncing)]} {
        return
    }

    # set the flag; this makes sure we only get called once
    set tkecl(disableSyncing) 1

    # scroll the other windows 
    foreach window $windowlist {
        $window yview moveto $y0
    }

    # we apparently automatically process idle events after this
    # proc is called. Once that is done we'll unset our flag
    after idle {catch {unset tkecl(disableSyncing)}}
}

proc tkecl:popup_sourcetext_menu {t x y} {
    global tkecl

    # return if no file loaded into source context window
    if {[string compare $tkecl(source_debug,file) ""] == 0} return

    set m $t.popupmenu 
    if [winfo exists $m] {
	$m delete 0 end
    } else {
	menu $m -tearoff 0
    }

    set xypos [winfo pointerxy .ec_tools.ec_tracer]
    set line [tkecl:get_current_text_line $t]
    $m add command -label "Find..." -command "tkecl:show-find source_debug .ec_tools.ec_tracer.tab.source.context.text .ec_tools.ec_tracer"

    $m add cascade -label "Display Predicate..." -menu $m.predmenu
    $m add separator
    $m add command -label "Refresh this file" -command \
	[list tkecl:load_source_debug_file $tkecl(source_debug,file) [$t xview] [$t yview]]
    $m add command -label "Edit this file" -command [list tkecl:edit_file $tkecl(source_debug,file)  $line]
    set callinfo [tkecl:get_nearest_port_call $tkecl(source_debug,file) $line]
    if {$callinfo != ""} {
	$m add separator
	set parent [lindex $callinfo 0]
	set callport [lindex $callinfo 1]
	set calldefmodule [lindex $callport 1]
	set callspec [lindex $callport 2]
	set callname [lindex $callspec 1]
	set callarity [lindex $callspec 2]
	$m add command -state disabled -label "Nearest tracable call\n$callname/$callarity in $parent"
	$m add command -label "Show predicate property for ths predicate" \
	    -command [list tkecl:show_pred_prop $calldefmodule $callspec]
    }
    tk_popup $m $x $y
}

proc tkecl:show_pred_prop {module callspec} {
    global tkecl

    set tkecl(predproppred) [lindex [ec_rpcq [list term_string $callspec _] {((()I)_)}] 2]
    set tkecl(predpropmodule) $module

    tkecl:popup_pred_prop
    tkecl:display_predicates dummy
    tkecl:display_predprops .ec_tools.predprop.preds
}


proc tkecl:check_port_call_source {module callspec} {

    if [winfo exists .ec_tools.ec_tracer] {
	set parent .ec_tools.ec_tracer
    } else {
	set parent .
    }

    switch [ec_rpcq [list current_module $module] {(())}] {
	fail -
	throw {
	    tk_messageBox -parent $parent -type ok -message "Definition module $module for call $callspec does not exist" 
	    return 0
	}
    }
    switch [ec_rpcatq [list is_predicate $callspec] ((()I)) $module] {
	fail -
	throw {
	    tk_messageBox -parent $parent -type ok -message "$callspec is not a user defined predicate in module $module"
	    return 0
	}
    }
    switch [ec_rpcatq [list get_flag $callspec source_file _] ((()I)()_) $module] {
	fail -
	throw {
	    tk_messageBox -parent $parent -type ok -message "Unable to access source information for $callspec defined in module $module"
	    return 0
	}
    }

    return 1

}

proc tkecl:get_nearest_port_call {file line} {

    set result [ec_rpcq [list find_matching_callinfo $file $line _ _] (SI__) tracer_tcl]

    switch $result {
	throw -
	fail {
	    return ""
	}
	default {
	    return [lrange $result 3 4]
	}
    }
}

proc tkecl:toggle_breakpoint {t} {
    global tkecl

    set line [tkecl:get_current_text_line $t]
    set result [ec_rpcq [list toggle_source_breakpoint $tkecl(source_debug,file) $line _ _ _] (SI___) tracer_tcl]
    if [winfo exists .ec_tools.ec_tracer] {
	set parent .ec_tools.ec_tracer
    } else {
	set parent .
    }

    switch $result {
	fail {
	    tk_messageBox  -parent $parent -type ok -message "No break ports found in file $tkecl(source_debug,file)"

	}
	throw {
	    # shouldn't happen!
	    bell
	}
	default {
	    set breakline [lindex $result 3]
	    set old_style [lindex $result 4]
	    set new_style [lindex $result 5]
	    set ec_breakstatus .ec_tools.ec_tracer.tab.source.context.status

	    $ec_breakstatus tag remove $old_style $breakline.0 $breakline.end
	    $ec_breakstatus tag add $new_style $breakline.0 $breakline.end

	}
    }
}

proc tkecl:get_source_debug_filenames {w} {

    set source_files \
	[lindex [ec_rpcq [list current_files_with_port_lines _] (_) tracer_tcl] 1]
    foreach file $source_files {
	$w add [lindex $file 0]		;# $file is an atom (1-element list)
    }

}
 
proc tkecl:handle_source_debug_print {stream {length {}}} {

    set ec_sourcecon .ec_tools.ec_tracer.tab.source.context
#    pack forget $ec_sourcecon.text ;# do not display text as it is added....
    set source_stream [ec_streamnum_to_channel $stream]
    set part [ec_read_exdr $source_stream]
    if {$part != ""} {
	$ec_sourcecon.text insert end $part
    } else {
#	pack $ec_sourcecon.text -fill both -expand 1

	# Find and hide CR characters (for Windows) - we can't delete them
	# because that would break our offset-based positioning within the
	# file (we are getting the file in binary from ECLiPSe).
	set i 1.0
	while {1} {
	    set i [$ec_sourcecon.text search "\r" $i]
	    if {$i == ""} { break }
	    $ec_sourcecon.text tag add hidden_cr $i
	    set i "$i+1chars"
	}

	# Initialise the line and breakpoint columns
	$ec_sourcecon.status delete 1.0 end
	$ec_sourcecon.lineno delete 1.0 end
	# find out the actual number of lines in the file. 
	regexp {^[0-9]+} [$ec_sourcecon.text index end] lastline
	# check if the actual last line (lastline-1) has a newline or not. 
	# If it does, the last char position will be 0
	regexp {^[0-9]+[.]([0-9]+)} [$ec_sourcecon.text index [expr $lastline-1].end] whole  lastchar
	# actual number of lines is one less than end if there is a newline
	if {$lastchar == 0} {
	    set terminating_nl 1
	    incr lastline -1 
	} else {
	    set terminating_nl 0
	}
        # this only works if the source has at least 1 line!
	set sstuff {}
	set lstuff {1}
	for {set i 2} {$i < $lastline} {incr i} {
	    append sstuff "\n"
	    append lstuff "\n$i"
	}
	# only add a terminating  newline if the source file has one
	if {$terminating_nl == 1} {
	    append sstuff "\n"
	    append lstuff "\n"
	}

	$ec_sourcecon.status insert end $sstuff
	$ec_sourcecon.lineno insert end $lstuff
    }

}

proc tkecl:show_source_context {invoc greturn} {

    set path_info [lindex $greturn 6]
    set from [lindex $greturn 7]
    set to [lindex $greturn 8]
    # is_current_goal/2 must be execute when source is viewed to get 
    # the current information
    set rpc_result [ec_rpcq [list is_current_goal $invoc _] (I_) tracer_tcl] 
    if {$rpc_result != "fail"} {
	set gstyle [lindex $rpc_result 2]
    } else {
	set gstyle ancestor_style
    }

    # path_info in quotes because it may have spaces
    tkecl:update_source_debug $gstyle $from $to "$path_info"
}



proc tkecl:update_source_debug {style from to fpath_info} {
    global tkecl

    set ec_source .ec_tools.ec_tracer.tab.source

    if {![winfo exists $ec_source]} { 
	return
    }

    set ec_sourcetext $ec_source.context.text
    if {$style != "ancestor_style"} {
	# reset previous trace call annotations (except debug_line)
	$ec_sourcetext tag remove call_style 1.0 end
	$ec_sourcetext tag remove exit_style 1.0 end
	$ec_sourcetext tag remove fail_style 1.0 end
    }
    $ec_sourcetext tag remove ancestor_style 1.0 end

    if {$fpath_info == "no" || $from < 0} {
	# .ec_tools.ec_tracer.tab itemconfigure "Source Context" -state disabled
	return
    } else {
	# get the pathname
	set fpath [lindex [lindex $fpath_info 1] 0] ;# atom type (singleton list)
    }

     if {$tkecl(source_debug,file) != $fpath} {
	 if {[tkecl:load_source_debug_file $fpath] == 0} return
    } else {
	if {$style != "ancestor_style"} {
	    $ec_sourcetext tag remove debug_line 1.0 end
	}	
    }

    # assume $from, $to -- position information on an annotated term from
    # ECLiPSe maps into number of characters from start of file
    set from_idx [$ec_sourcetext index "1.0 + $from chars"]
    set to_idx [$ec_sourcetext index "1.0 + $to chars"]
    $ec_sourcetext tag add $style $from_idx $to_idx
    if {$style != "ancestor_style"} {
	$ec_sourcetext tag add debug_line "$from_idx linestart" "$to_idx lineend"
    }
    $ec_sourcetext see $from_idx
	
}


proc tkecl:get_current_text_line {t} {

    regexp {^[0-9]+} [$t index current] line
    return $line
}


proc tkecl:load_source_debug_file {fpath {xfracs "0 1"} {yfracs "0 1"}} {
    global tkecl

    set ec_source .ec_tools.ec_tracer.tab.source
    set ec_sourcetext $ec_source.context.text
    set xfrac [lindex $xfracs 0]
    set yfrac [lindex $yfracs 0]

    switch [ec_rpcq [list file_is_readable $fpath] (S) tracer_tcl] {
	    fail -
	    throw {
		# source not readable, no display
		return 0
	    }
     }

    $ec_sourcetext delete 1.0 end
    ec_rpcq [list read_file_for_gui $fpath] (S) tracer_tcl
    set tkecl(source_debug,file) $fpath
    $ec_source.context.text xview moveto $xfrac 
    $ec_source.context.text yview moveto $yfrac 

    set result [ec_rpcq [list breakpoints_for_file $fpath _ _ _] (S___) tracer_tcl]
    switch $result {
	fail -
	throw {
	    return 0
	}
	default {
	    set actives [lindex $result 2]

	    set ports [lindex $result 3]
	    set predslist [lindex $result 4]
	    foreach line $ports {
		$ec_source.context.status insert $line.0 "#" off
	    }
	    foreach line $actives {
		$ec_source.context.status tag remove off $line.0 $line.end
		$ec_source.context.status tag add on $line.0 $line.end
	    }
	    set predmenu $ec_source.context.text.popupmenu.predmenu
	    $predmenu delete 0 end
	    set i 0
	    foreach pred $predslist {
		incr i
		if {[expr $i % 30] == 0} {
		    set brk 1
		} else {
		    set brk 0
		}
		$predmenu add command -label [lindex $pred 1] -command "$ec_source.context.text see [lindex $pred 2].0" -columnbreak $brk
	    }
	}
    }

    return 1
#    $ec_source.control.load configure -state normal
}

# the find code is adapted from tkdiff
# name is the `user' name of the text window being search. It is also used to
# distinguish the tkecl variables used by the find window. 
# source is the  path to the text widget being searched
# top is the path of the toplevel window for source
proc tkecl:show-find {name source top} {
    global tkecl

    set ff $source.find.content.findFrame
    if {![winfo exists $source.find]} {
        toplevel $source.find
        wm group $source.find $top
        wm transient $source.find $top
        wm title $source.find "$name Find"

        # we don't want the window to be deleted, just hidden from view
# following lines seems to produce a collasped window - commented out
#        wm protocol $source.find WM_DELETE_WINDOW [list wm withdraw \
          $source.find]

#        wm withdraw $source.find
        update idletasks

        frame $source.find.content -bd 2 -relief groove
        pack $source.find.content -side top -fill both -expand y -padx 0 \
          -pady 5

        frame $source.find.buttons
        pack $source.find.buttons -side bottom -fill x -expand n

        button $source.find.buttons.doit -text "Find Next" -command "tkecl:do-find $name $source $top"
        button $source.find.buttons.dismiss -text "Dismiss" -command \
          "wm withdraw $source.find"
        pack $source.find.buttons.dismiss -side right -pady 5 -padx 0
        pack $source.find.buttons.doit -side right -pady 5 -padx 1

        frame $ff -height 100 -bd 2 -relief flat
        pack $ff -side top -fill x -expand n -padx 0 -pady 5

        label $ff.label -text "Find what:" -underline 2

        entry $ff.entry -textvariable tkecl($name,findString)

        checkbutton $ff.searchCase -text "Ignore Case" -offvalue 0 -onvalue 1 \
          -indicatoron true -variable tkecl($name,findIgnoreCase)

        grid $ff.label -row 0 -column 0 -sticky e
        grid $ff.entry -row 0 -column 1 -sticky ew
        grid $ff.searchCase -row 0 -column 2 -sticky w
        grid columnconfigure $ff 0 -weight 0
        grid columnconfigure $ff 1 -weight 1
        grid columnconfigure $ff 2 -weight 0

        # we need this in other places...
        set tkecl($name,findEntry) $ff.entry

        bind $ff.entry <Return> "tkecl:do-find $name $source $top"

        set of $source.find.content.optionsFrame
        frame $of -bd 2 -relief flat
        pack $of -side top -fill y -expand y -padx 10 -pady 10

        label $of.directionLabel -text "Search Direction:" -anchor e
        radiobutton $of.directionForward -indicatoron true -text "Down" \
          -value "-forward" -variable tkecl($name,findDirection)
        radiobutton $of.directionBackward -text "Up" -value "-backward" \
          -indicatoron true -variable tkecl($name,findDirection)


        label $of.searchLabel -text "Search Type:" -anchor e
        radiobutton $of.searchExact -indicatoron true -text "Exact" \
          -value "-exact" -variable tkecl($name,findType)
        radiobutton $of.searchRegexp -text "Regexp" -value "-regexp" \
          -indicatoron true -variable tkecl($name,findType)

        grid $of.directionLabel -row 0 -column 0 -sticky w
        grid $of.directionForward -row 0 -column 1 -sticky w
        grid $of.directionBackward -row 0 -column 2 -sticky w

        grid $of.searchLabel -row 1 -column 0 -sticky w
        grid $of.searchExact -row 1 -column 1 -sticky w
        grid $of.searchRegexp -row 1 -column 2 -sticky w

        grid columnconfigure $of 0 -weight 0
        grid columnconfigure $of 1 -weight 0

        set tkecl($name,findDirection) "-forward"
        set tkecl($name,findType) "-exact"
        set tkecl($name,findIgnoreCase) 1
        set tkecl($name,lastSearch) ""
    }

    wm deiconify $source.find
    raise $source.find
    after idle focus $ff.entry
}

# search for the text in the find dialog
proc tkecl:do-find {name source top} {
    global tkecl

    if {![winfo exists $source.find] || ![winfo ismapped $source.find]} {
        tkecl:show-find $name $source $top
        return
    }

    if {$tkecl($name,lastSearch) != ""} {
        if {$tkecl($name,findDirection) == "-forward"} {
            set start [$source index "insert +1c"]
        } else {
            set start insert
        }
    } else {
        set start 1.0
    }

    if {$tkecl($name,findIgnoreCase)} {
        set result [$source search $tkecl($name,findDirection) $tkecl($name,findType) -nocase \
          -- $tkecl($name,findString) $start]
    } else {
        set result [$source search $tkecl($name,findDirection) $tkecl($name,findType) \
          -- $tkecl($name,findString) $start]
    }
    if {[string length $result] > 0} {
        # if this is a regular expression search, get the whole line and try
        # to figure out exactly what matched; otherwise we know we must
        # have matched the whole string...
        if {$tkecl($name,findType) == "-regexp"} {
            set line [$source get $result "$result lineend"]
            regexp $tkecl($name,findString) $line matchVar
            set length [string length $matchVar]
        } else {
            set length [string length $tkecl($name,findString)]
        }
        set tkecl($name,lastSearch) $result
        $source mark set insert $result
        $source tag remove sel 1.0 end
        $source tag add sel $result "$result + ${length}c"
        $source see $result
        focus $source
        # should I somehow snap to the nearest diff? Probably not...
    } else {
        bell

    }
}

#---------------------------------------------------------------------
# Balloon Help Toggle
#---------------------------------------------------------------------

trace variable tkecl(pref,balloonhelp) w tkecl:ToggleBalloonHelp

proc tkecl:ToggleBalloonHelp {name dummy op} {
    global tkecl

    if {$tkecl(pref,balloonhelp) == 1} {
	balloonhelp enable
    } else {
	balloonhelp disable
    }
}

#----------------------------------------------------------------------
# Help Files procedures
#----------------------------------------------------------------------

# find the right help file given key (four letter unique id) and display
# help file as a subwindow of parent
proc tkecl:Get_helpfileinfo {key parent} {
    global tkecl

    set i [lsearch -glob $tkecl(helpfiles) $key]
    if {$i == -1} {
	tk_messageBox -type ok -message "Invalid topic name for help files"
	return [list $key "invalid"]
    }
    set topic [lindex $tkecl(helpfiles) [expr $i+1]]
    set filename [lindex $tkecl(helpfiles) [expr $i+2]]
    eval tkinspect:helpinfo [concat {$parent} [list $topic $filename $key]]
}

#----------------------------------------------------------------------
# Routines for handling initial user defaults
#----------------------------------------------------------------------

proc tkecl:read_defaults_file {family} {
    global env tkecl

    set defaults ""
    set file_exists 0
    set filename .$family  ;# filename is the family name with leading .
    if [file exists $filename] {
	set file_exists 1
    } else {
	set filename [file join $env(HOME) $filename]  ;# check in homedir
	if [file exists $filename] { set file_exists 1 }
    }
    if {$file_exists == 1} {
	if {[catch  {open $filename r} fid]} return $defaults  ;# unable to open file
	   
	while {[gets $fid line] >= 0} {
	    set option [lindex $line 0]
	    if {[lsearch $tkecl(preferences) [list $option * * $family *]] != -1} {

		;# get the part of the line from the start of the second word
		;# (first word is $option)
		set tkecl(prefset,$option) [string trimleft [string range \
			$line [string wordend $line [string first $option \
                        $line]] end]]
		lappend defaults $option
	    } else {
		;# not a valid option
		tk_messageBox -icon warning -message "$option is not a valid preference for $family"
	    }

	}
	close $fid
    }
    return $defaults
}


proc tkecl:get_user_defaults {family} {
    global tcl_platform tkecl

    ;# read in user defined defaults for family (tkeclipserc or tkeclipsetoolsrc)
    switch $tcl_platform(platform) {
	unix {
	    return [tkecl:read_defaults_file $family]
	}
	
	windows {
	    package require registry
	    set regpath $tkecl(windows_registry_path)$family
	    registry set $regpath  ;# make sure the key is there
	    set defaults ""

	    foreach option [registry values $regpath] {
		if {[lsearch $tkecl(preferences) [list $option * * $family *]] != -1} {
		    set tkecl(prefset,$option) [registry get $regpath $option]
		    lappend defaults $option
		} else {
		    ;# not a valid option
		    tk_messageBox -icon warning -message "$option is not a valid preference for $family"
		}
	    }
	    return $defaults

	}
    }
}


proc tkecl:set_tools_defaults {} {
    global tkecl

    set userdefaults [tkecl:get_user_defaults tkeclipsetoolsrc]

    foreach preference $tkecl(preferences) {
	foreach {dname default type family help} $preference {
	    if {$family == "tkeclipsetoolsrc"} {
		if {[lsearch -exact $userdefaults $dname] != -1} {
		    set value $tkecl(prefset,$dname)
		} else {
		    set value $default
		}
		tkecl:set_one_tools_default $dname $value $type
	    }
	} 
    }
}
	

proc tkecl:set_one_tools_default {dname dvalue type} {
    global tkecl

    if {[string trimleft $dvalue] != ""} {
	;# only set if dvalue is not empty or whitespaces
	switch -- $type {

	    boolean {
		;# 0 or 1 options
		if {$dvalue == 1 || $dvalue == 0} {
		    set tkecl(pref,$dname) $dvalue
		} else {
		    tk_messageBox -icon warning -message "$dvalue is an invalid value for $dname (0/1 expected)" -type ok
		}
	    }

	    +integer {
		;# straight positve integers, no special routines to call
		if [regexp {^[0-9]+$} $dvalue size] {
		    set tkecl(pref,$dname) $dvalue
		} else {
		    tk_messageBox -icon warning -message "$dvalue is an invalid value for $dname (positive integer expected" -type ok
		}
	    }

	    tracer_prdepth {
		if [regexp {^[0-9]+$} $dvalue size] {
		    set tkecl(pref,tracer_prdepth) $dvalue
		    ec_rpcq [list set_tracer_print_depth $tkecl(pref,tracer_prdepth)] (I) tracer_tcl
		} else {
		    tk_messageBox -icon warning -message "$dvalue is an invalid value for tracer_prdepth (positive integer expected" -type ok
		}
	    }

	    stats_interval {
		if [regexp {^([0-9]+[.][0-9]+)$|^([0-9]+)$} $dvalue] {
		    set tkecl(pref,$dname) $dvalue
		    ;# interval will be set later on via rpc
		} else {
		    tk_messageBox -icon warning -message "$dvalue is an invalid value for stats_interval (number expected)"
		}

	    }

	    string {
		set tkecl(pref,$dname) $dvalue
	    }

	    colour { ;# background colour only 
		if [catch {tk_setPalette $dvalue}] {
		    tk_messageBox -icon error -type ok -message \
			"Unable to change default background colour to $dvalue"
		} else {
		    set tkecl(pref,$dname) $dvalue
		}
	    }

	    fontsize  {
		if [regexp {^[0-9]+$} $dvalue size] {
		    if {[string compare $dname monofont_size] == 0} {
			font configure tkeclmono -size $dvalue
			font configure tkeclmonobold -size $dvalue
		    } else {
			font configure tkecllabel -size $dvalue
		    }
		    set tkecl(pref,$dname) $dvalue
		} else {
		    tk_messageBox -icon warning -message "$dvalue is an invalid valid for $dname (positive integer expected" -type ok
		}
	    }

	    font {
		if {[string compare $dname monofont_family] == 0} {
		    font configure tkeclmono -family $dvalue
		    font configure tkeclmonobold -family $dvalue
		} else {
		    font configure tkecllabel -family $dvalue
		}
		set tkecl(pref,$dname) $dvalue
	    }
	}
    } 
}


proc tkecl:popup_edit_defaults {} {
    global tkecl

    set edit .ec_tools.pref_edit
    if {![winfo exists $edit]} {
	toplevel $edit
        wm title $edit "TkECLiPSe Preference Editor"
	wm resizable $edit 0 0
	foreach preference $tkecl(preferences) {
	    tkecl:display_one_default $edit $preference 
	}
	pack [frame $edit.bf] -expand 1 -fill x
	pack [button $edit.bf.apply -text "Apply Preferences" -command tkecl:apply_prefs] -expand 1 -fill x -side left
	pack [button $edit.bf.save -text "Save Preferences" -command tkecl:save_prefs] -expand 1 -fill x -side left
	pack [button $edit.bf.close -text "Close" -command "destroy $edit"] -expand 1 -fill x -side right

	balloonhelp $edit "Change various preference settings for TkECLiPSe"
	balloonhelp $edit.bf.save "Save the preferences in the editor (the values will be used for the initial settings for the next session)."
	balloonhelp $edit.bf.close "Close the preference editor"
	balloonhelp $edit.bf.apply "Apply the preferences in the editor to the current session."
	bind $edit <Alt-h> "tkecl:Get_helpfileinfo pref $edit"
    } else {
	tkinspect:RaiseWindow $edit
    }
}

proc tkecl:display_one_default {w preference} {
    global tkecl

    foreach {option sysdefault type family help} $preference {
	set default $tkecl(pref,$option)
	set tkecl(prefset,$option) $default

	switch -exact -- $type {
	    boolean {
		pack [frame $w.$option]  -fill both
		pack [label $w.$option.l -text $help -anchor w -width 50] -side left -expand 1 -fill both 
		pack [radiobutton $w.$option.on -text on -value 1 \
                    -variable tkecl(prefset,$option) -anchor w] -side left -expand 1 -fill both
		pack [radiobutton $w.$option.off -text off -value 0 \
                    -variable tkecl(prefset,$option) -anchor w] -side left -expand 1 -fill both
	    }

	    fontsize  -
	    tracer_prdepth -
	    +integer {
		pack [ventry $w.$option -labeltext $help -labelwidth 50 \
                      -vcmd {regexp {^[0-9]*$} %P} -validate key -labelanchor w\
                      -invalidcmd bell -textvariable tkecl(prefset,$option) \
                     ] -fill both -expand 1
	    }

	    stats_interval {
		pack [ventry $w.$option -labeltext $help -labelwidth 50 \
                      -vcmd {regexp {^([0-9]*[.][0-9]*)$|^([0-9]*)$} %P} \
                      -validate key -invalidcmd bell -labelanchor w \
                      -textvariable tkecl(prefset,$option)\
                     ] -fill both -expand 1
	    }

	    colour -
	    font  -
	    string {
		pack [ventry $w.$option -labeltext $help -labelwidth 50 \
                     -labelanchor w -textvariable tkecl(prefset,$option)\
                     ] -fill both -expand 1
	    }


	}
    }
}

proc tkecl:apply_prefs {} {
    global tkecl

    foreach preference $tkecl(preferences) {
	foreach {option default type family help} $preference {
	    break
	}
	tkecl:set_one_tools_default $option $tkecl(prefset,$option) $type
    }
}

proc tkecl:save_prefs {} {
    global tcl_platform tkecl env

    foreach preference $tkecl(preferences) {
	foreach {option default type family help} $preference {
	    lappend group($family) $option
	}
    }
    switch $tcl_platform(platform) {
	unix {
	    foreach rootname [array names group] {
		if [file exists .$rootname] {
		    set filename .$rootname
		} else {
		    set filename [file join $env(HOME) .$rootname]
		}
		if {[catch {open $filename w} fid]} {
		    tk_messageBox -type ok -icon error -message "Unable to write the preference file. Permission problems?"
		    return
		}
		foreach option $group($rootname) {
		    if {[string trimleft $tkecl(prefset,$option)] != ""} {
			puts $fid "$option $tkecl(prefset,$option)"
		    }
		}
		close $fid
	    }
	}

	windows {
	    foreach rootname [array names group] {
		set regpath $tkecl(windows_registry_path)$rootname
		foreach option $group($rootname) {
		    registry set $regpath $option $tkecl(prefset,$option)
		}
	    }
	}
    }
}

#--------------------------------------------------------------------
# 
#--------------------------------------------------------------------

proc tkecl:listbox_search {lbox key keycode x y} {
    global tkecl

    if {$key == {}} {return -code continue}  ;# return if modifier key only

    set s $lbox.search
    if {![winfo exists $s]} {
	toplevel $s
	wm overrideredirect $s 1
        wm positionfrom $s program
        wm withdraw $s
	pack [label $s.l -highlightthickness 0 -relief raised -bd 1 \
		-background lightblue -textvariable tkecl(lboxstring)]

        ;# for some reason x  position of the popup window needs to be 
        ;# somewhat displaced from the mouse position to work
	set x [expr $x + 10]
	wm geometry $s +$x+$y
	wm deiconify $s
        raise $s
    } else {
	raise $s
    }

    switch -exact  -- $keycode  {
	Delete    -
	BackSpace {
	    set tkecl(lboxstring) [string range "$tkecl(lboxstring)" 0 end-1]
	    tkecl:do_listbox_search $lbox [$lbox get 0 end] \
		    $tkecl(lboxstring)* 0
	}
	Escape {
	    tkecl:listbox_search_exit $lbox
	}
	Return {
	    ;# disabled because selection does not activate <<ListboxSelect>>
	    ;# $lbox selection set active 
	}
	Control_S {
	    ;# search from active element
	    set start [expr [$lbox index active] + 1]
	    tkecl:do_listbox_search $lbox [$lbox get $start end] \
		    $tkecl(lboxstring)* $start
	}
	default {
	    ;# printable character
	    append tkecl(lboxstring) $key
	    tkecl:do_listbox_search $lbox [$lbox get 0 end] \
		    $tkecl(lboxstring)* 0
	}
    }
    return -code continue
}

# search for string, list may be a sublist starting from start of listbox
proc tkecl:do_listbox_search {lbox list search_string start} {
    set offset [lsearch $list $search_string]
    if {$offset != -1} {
	set index [expr $offset + $start] ;# index in original list
	$lbox yview $index 
	$lbox activate $index
    } else {
	bell
    }
}

proc tkecl:listbox_search_init {lbox} {
    global tkecl

    if [winfo exists $lbox.search] {
	destroy $lbox.search
    }

    set tkecl(lboxstring) "*"
    focus $lbox
}

proc tkecl:listbox_search_exit {lbox} {
    global tkecl

    if [winfo exists $lbox.search] {
	destroy $lbox.search
    }

    focus -lastfor $lbox
}

#--------------------------------------------------------------------
# handling keypresses in read-only windows
#--------------------------------------------------------------------

proc tkecl:readonly_keypress {keycode} {

    switch -exact -- $keycode {
	"\x3" {  ;#^C -- allow default handling for window copy operation
	    return 0
	}
	default {
	    return -code break
	}
    }
}

#--------------------------------------------------------------------
# Utility for locating the window the pointer is in
#--------------------------------------------------------------------

proc tkecl:pointer_window {} {

    set win [winfo containing -displayof . \
		 [winfo pointerx .] [winfo pointery .]]
    if {$win != ""} { ;# pointer is in a window for the application
	return [winfo toplevel $win] ;# we want the toplevel path only
    } else {
	return ""  ;# not in any window 
    }
}

#---------------------------------------------------------------------
# Handling multitasking
#---------------------------------------------------------------------

proc tkecl:multi_start_handler {type} {

    switch $type {
	tracer {
	    # only do handling of port if the tracer window exists
	    if [winfo exists .ec_tools.ec_tracer] {
		tkecl:handle_tracer_port_start
		set of_interest  continue
	    } else {
		set of_interest no ;# do nothing (no tracer window)
	    }
	}
	default {
	    set of_interest no
	    # do nothing
	}
    }

    return $of_interest
}

proc tkecl:multi_interact_handler {type} {
    global tkecl

    switch $type {
	tracer {
	    tkecl:check_tracer_interaction
	    if [string match tkecl(tracer_state) disabled] {
		return terminate
	    } else {
		return continue
	    }
	}
	default {
	    # do nothing
	    return continue
	}
    }
}

proc tkecl:multi_end_handler {type} {
    global tkecl

    if {[ec_interface_type] == "remote"} {
	tkecl:freeze_control
    }
}

#---------------------------------------------------------------------
# Visualisation client
#---------------------------------------------------------------------

proc tkecl:start_vc {} {
    switch [ec_rpcq_check {ensure_loaded {library java_vc}} ((()))] {
    	fail - throw { return }
    }
    ec_rpcq_check {start_vc _} (_) java_vc
}

#---------------------------------------------------------------------
# Viztool
#---------------------------------------------------------------------

proc tkecl:start_viztool {} {
    switch [ec_rpcq_check {ensure_loaded {library cpviz}} ((()))] {
    	fail - throw { return }
    }
    ec_rpcq_check viztool () cpviz
}

#----------------------------------------------------------------------
# Initalise and create menu/toolbar
#----------------------------------------------------------------------

proc ec_tools_init {w} {
    global tkinspectvalues tkecl


# Init the Eclipse part (must be done after ec_init !!!)
    ec_rpcq {ensure_loaded {library development_support}} ((()))
    ec_rpcq {ensure_loaded {library tracer_tcl}} ((()))
    ec_rpcq install_guitools () tracer_tcl
    ec_queue_create debug_traceline r tkecl:handle_trace_line
    ec_queue_create debug_output r tkecl:handle_debug_output
    ec_queue_create gui_source_file r tkecl:handle_source_debug_print
    ec_queue_create matrix_out_queue r tkecl:handle_mat_flush
    ec_queue_create gui_dg_info r tkecl:handle_dg_print
    ec_queue_create statistics_out_queue r tkecl:handle_stats_report
    set tkecl(toplevel_module) [lindex [ec_rpcq_check {get_flag toplevel_module _} (()_)] 2]
    set tkecl(predpropmodule) $tkecl(toplevel_module)

    ec_multi:peer_register [list interact tkecl:multi_interact_handler start tkecl:multi_start_handler end tkecl:multi_end_handler] 

# Create the tools launcher menu and set up help files

    menu $w 
    $w add command -label "Compile Scratch-pad" -command "tkecl:compile_pad"
    lappend tkecl(helpfiles) scra {Compile Scratch-Pad} scratchhelp.txt
    $w add command -label "Source File Manager" -command tkecl:popup_file_window
    lappend tkecl(helpfiles) file {Source Files Tool} sourcehelp.txt
    $w add command -label "Predicate Browser" -command tkecl:popup_pred_prop
    lappend tkecl(helpfiles) pred {Predicates Property Tool} predprophelp.txt
    $w add separator
    $w add command -label "Delayed Goals" -command tkecl:popup_dg_window
    lappend tkecl(helpfiles) dela {Delayed Goals Viewer} delayhelp.txt
    $w add command -label "Tracer" -command tkecl:popup_tracer
    lappend tkecl(helpfiles) trac Tracer tracerhelp.txt
    $w add command -label "Inspector" -command "tkinspect:Inspect_term_init current"
    lappend tkecl(helpfiles) insp Inspector inspecthelp.txt
    $w add command -label "Visualisation Client" -command "tkecl:start_vc"
    $w add command -label "CP-Viz Viztool" -command "tkecl:start_viztool"
    $w add separator
    $w add command -label "Global Settings" -command tkecl:popup_global_state
    lappend tkecl(helpfiles) glob {Global Settings Tool} globalsethelp.txt
    $w add command -label "Statistics" -command tkecl:handle_statistics
    lappend tkecl(helpfiles) stat {Statistics Window} stathelp.txt
    $w add command -label "Simple Query" -command tkecl:rpc
    lappend tkecl(helpfiles) rpc {Simple Query Tool} rpchelp.txt
    $w add command -label "ECLiPSe Library Browser and Help" -command tkecl:library_browser
    lappend tkecl(helpfiles) help {Library Browser and Help Tool} helphelp.txt
    $w add separator
#    $w add command -label "ECLiPSe Help" -command tkecl:popup_help_window
#    lappend tkecl(helpfiles) help {ECLiPSe Help Tool} helphelp.txt
    $w add command -label "TkECLiPSe Preference Editor" -command tkecl:popup_edit_defaults 
    lappend tkecl(helpfiles) pref {Preference Editor} prefhelp.txt
    $w add separator
    $w add check -label "Balloon Help" -variable tkecl(pref,balloonhelp)
#    $w add command -label "Test" -command tkecl:test
    lappend tkecl(helpfiles) disp {Display Matrix} matdisplayhelp.txt

    tkecl:set_tools_defaults 
    ;# set user defined defaults for tools

    return $w
}

