##
## Copyright 1996-8 Jeffrey Hobbs, jeff.hobbs@acm.org
##
## Based off previous work for TkCon
##
package require Widget 2.0
package require ::Utility 1.0
package provide Console 2.0

##------------------------------------------------------------------------
## PROCEDURE
##	console
##
## DESCRIPTION
##	Implements a console mega-widget
##
## ARGUMENTS
##	console <window pathname> <options>
##
## OPTIONS
##	(Any frame widget option may be used in addition to these)
##
##  -blinkcolor color			DEFAULT: #FFFF00 (yellow)
##	Specifies the background blink color for brace highlighting.
##	This doubles as the highlight color for the find box.
##
##  -blinkrange TCL_BOOLEAN		DEFAULT: 1
##	When doing electric brace matching, specifies whether to blink
##	the entire range or just the matching braces.
##
##  -blinktime delay			DEFAULT: 500
##	For electric brace matching, specifies the amount of time to
##	blink the background for.
##
##  -grabputs TCL_BOOLEAN		DEFAULT: 1
##	Whether this console should grab the "puts" default output
##
##  -lightbrace TCL_BOOLEAN		DEFAULT: 1
##	Specifies whether to activate electric brace matching.
##
##  -lightcmd TCL_BOOLEAN		DEFAULT: 1
##	Specifies whether to highlight recognized commands.
##
##  -proccolor color			DEFAULT: #008800 (darkgreen)
##	Specifies the color to highlight recognized procs.
##
##  -prompt string	DEFAULT: {([file tail [pwd]]) [history nextid] % }
##	The equivalent of the tcl_prompt1 variable.
##
##  -promptcolor color			DEFAULT: #8F4433 (brown)
##	Specifies the prompt color.
##
##  -stdincolor color			DEFAULT: #000000 (black)
##	Specifies the color for "stdin".
##	This doubles as the console foreground color.
##
##  -stdoutcolor color			DEFAULT: #0000FF (blue)
##	Specifies the color for "stdout".
##
##  -stderrcolor color			DEFAULT: #FF0000 (red)
##	Specifies the color for "stderr".
##
##  -showmultiple TCL_BOOLEAN		DEFAULT: 1
##	For file/proc/var completion, specifies whether to display
##	completions when multiple choices are possible.
##
##  -showmenu TCL_BOOLEAN		DEFAULT: 1
##	Specifies whether to show the menubar.
##
##  -subhistory TCL_BOOLEAN		DEFAULT: 1
##	Specifies whether to allow substitution in the history.
##
##  -varcolor color			DEFAULT: #FFC0D0 (pink)
##	Specifies the color for "stderr".
##
## RETURNS: the window pathname
##
## BINDINGS (these are the bindings for Console, used in the text widget)
##
## <<Console_ExpandFile>>	<Key-Tab>
## <<Console_ExpandProc>>	<Control-Shift-Key-P>
## <<Console_ExpandVar>>	<Control-Shift-Key-V>
## <<Console_Tab>>		<Control-Key-i>
## <<Console_Eval>>		<Key-Return> <Key-KP_Enter>
##
## <<Console_Clear>>		<Control-Key-l>
## <<Console_KillLine>>		<Control-Key-k>
## <<Console_Transpose>>	<Control-Key-t>
## <<Console_ClearLine>>	<Control-Key-u>
## <<Console_SaveCommand>>	<Control-Key-z>
##
## <<Console_Prev>>		<Key-Up>
## <<Console_Next>>		<Key-Down>
## <<Console_NextImmediate>>	<Control-Key-n>
## <<Console_PrevImmediate>>	<Control-Key-p>
## <<Console_PrevSearch>>	<Control-Key-r>
## <<Console_NextSearch>>	<Control-Key-s>
##
## <<Console_Exit>>		<Control-Key-q>
## <<Console_New>>		<Control-Key-N>
## <<Console_Close>>		<Control-Key-w>
## <<Console_About>>		<Control-Key-A>
## <<Console_Help>>		<Control-Key-H>
## <<Console_Find>>		<Control-Key-F>
##
## METHODS
##	These are the methods that the console megawidget recognizes.
##
## configure ?option? ?value option value ...?
## cget option
##	Standard tk widget routines.
##
## load ?filename?
##	Loads the named file into the current interpreter.
##	If no file is specified, it pops up the file requester.
##
## save ?filename?
##	Saves the console buffer to the named file.
##	If no file is specified, it pops up the file requester.
##
## clear ?percentage?
##	Clears a percentage of the console buffer (1-100).  If no
##	percentage is specified, the entire buffer is cleared.
##
## error
##	Displays the last error in the interpreter in a dialog box.
##
## hide
##	Withdraws the console from the screen
##
## history ?-newline?
##	Prints out the history without numbers (basically providing a
##	list of the commands you've used).
##
## show
##	Deiconifies and raises the console
##
## subwidget widget
##	Returns the true widget path of the specified widget.  Valid
##	widgets are console, yscrollbar, menubar.
##
## NAMESPACE & STATE
##	The megawidget creates a global array with the classname, and a
## global array which is the name of each megawidget is created.  The latter
## array is deleted when the megawidget is destroyed.
##	Public procs of $CLASSNAME and [string tolower $CLASSNAME] are used.
## Other procs that begin with $CLASSNAME are private.  For each widget,
## commands named .$widgetname and $CLASSNAME$widgetname are created.
##
## EXAMPLE USAGE:
##
## console .con -height 20 -showmenu false
## pack .con -fill both -expand 1
##------------------------------------------------------------------------

foreach pkg [info loaded {}] {
    set file [lindex $pkg 0]
    set name [lindex $pkg 1]
    if {![catch {set version [package require $name]}]} {
	if {[string match {} [package ifneeded $name $version]]} {
	    package ifneeded $name $version "load [list $file $name]"
	}
    }
}
catch {unset file name version}

# Create this to make sure there are registered in auto_mkindex
# these must come before the [widget create ...]
proc Console args {}
proc console args {}
widget create Console -type frame -base text -components {
    {base console console {-wrap char -setgrid 1 \
	    -yscrollcommand [list $data(yscrollbar) set] \
	    -foreground $data(-stdincolor)}}
    {frame menubar menubar {-relief raised -bd 1}}
    {scrollbar yscrollbar sy {-takefocus 0 -bd 1 \
	    -command [list $data(console) yview]}}
} -options {
    {-blinkcolor	blinkColor	BlinkColor	\#FFFF00}
    {-proccolor		procColor	ProcColor	\#008800}
    {-promptcolor	promptColor	PromptColor	\#8F4433}
    {-stdincolor	stdinColor	StdinColor	\#000000}
    {-stdoutcolor	stdoutColor	StdoutColor	\#0000FF}
    {-stderrcolor	stderrColor	StderrColor	\#FF0000}
    {-varcolor		varColor	VarColor	\#FFC0D0}

    {-blinkrange	blinkRange	BlinkRange	1}
    {-blinktime		blinkTime	BlinkTime	500}
    {-grabputs		grabPuts	GrabPuts	1}
    {-lightbrace	lightBrace	LightBrace	1}
    {-lightcmd		lightCmd	LightCmd	1}
    {-showmultiple	showMultiple	ShowMultiple	1}
    {-showmenu		showMenu	ShowMenu	1}
    {-subhistory	subhistory	SubHistory	1}

    {-abouttext		aboutText	AboutText	{}}
}

if {[info exists ::embed_args] || [info exists ::browser_args]} {
    widget add Console option {-prompt prompt Prompt {[history nextid] % }}
} else {
    widget add Console option {-prompt prompt Prompt \
	    {([file tail [pwd]]) [history nextid] % }}
}

widget add Console option [list -abouttitle aboutTitle AboutTitle \
	"About Console v[package provide Console]"]

##
## BEGIN CONSOLE DIALOG
##

# Create this to make sure there are registered in auto_mkindex
# these must come before the [widget create ...]
proc ConsoleDialog args {}
proc consoledialog args {}
widget create ConsoleDialog -type toplevel -base console -options {
    {-title	title	Title	"Console Dialog"}
}

namespace eval ::Widget::ConsoleDialog {;

variable class
array set class [list version [package provide Console]]

;proc construct {w} {
    set namesp [namespace current]
    upvar \#0 ${namesp}::$w data
    variable class

    wm title $w $data(-title)

    grid $data(console) -in $w -sticky news
    grid columnconfig $w 0 -weight 1
    grid rowconfig $w 0 -weight 1
}

;proc configure {w args} {
    upvar \#0 [namespace current]::$w data
    #variable class

    #set truth {^(1|yes|true|on)$}
    foreach {key val} $args {
	switch -- $key {
	    -title	{ wm title $w $val }
	}
	set data($key) $val
    }
}

;proc _hide w { if {[winfo exists $w]} { wm withdraw $w } }

;proc _show w { if {[winfo exists $w]} { wm deiconify $w; raise $w } }

}; # end namespace ::Widget::ConsoleDialog

##
## END CONSOLE DIALOG
##

##
## CONSOLE MEGAWIDGET
##

namespace eval ::Widget::Console {;

variable class
array set class {
    release	{20 October 1997}
    contact	"jeff.hobbs@acm.org"
    docs	"http://www.cs.uoregon.edu/research/tcl/script/tkcon/"
    slavealias	{ console }
    slaveprocs	{ alias dir dump lremove puts echo unknown tcl_unknown which }
}
if {![info exists class(active)]} { set class(active) {} }
set class(version) [package provide Console]
set class(WWW) [expr [info exists ::embed_args]||[info exists ::browser_args]]

catch {highlight}
namespace import -force ::Utility::*

## console -
# ARGS:	w	- widget pathname of the Console console
# Calls:	InitUI
# Outputs:	errors found in Console resource file
##
;proc construct {w} {
    upvar \#0 [namespace current]::$w data

    global auto_path tcl_pkgPath tcl_interactive
    set tcl_interactive 0

    ## Private variables
    array set data {
	app {} appname {} apptype {} namesp {} deadapp 0
	cmdbuf {} cmdsave {} errorInfo {}
	event 1 histid 0 find {} find,case 0 find,reg 0
    }

    if {![info exists tcl_pkgPath]} {
	set dir [file join [file dirname [info nameofexec]] lib]
	if {[string compare {} [info commands @scope]]} {
	    set dir [file join $dir itcl]
	}
	catch {namespace eval :: [list source [file join $dir pkgIndex.tcl]]}
    }
    catch {tclPkgUnknown dummy-name dummy-version}

    InitMenus $w

    grid $data(menubar) - -sticky ew
    grid $data(console) $data(yscrollbar) -sticky news
    grid columnconfig $w 0 -weight 1
    grid rowconfig $w 1 -weight 1

    prompt $w "console display active\n"

    set c $data(console)
    foreach col {prompt stdout stderr stdin proc} {
	$c tag configure $col -foreground $data(-${col}color)
    }
    $c tag configure var -background $data(-varcolor)
    $c tag configure blink -background $data(-blinkcolor)
}

;proc init {w} {
    upvar \#0 [namespace current]::$w data
    variable class
    bind $w <Destroy> [bind $class(class) <Destroy>]
    bindtags $w [list $w [winfo toplevel $w] all]
    set c $data(console)
    bindtags $c [list $c Console PostConsole $w all]
    if {$data(-grabputs) && [lsearch $class(active) $c] == -1} {
	set class(active) [linsert $class(active) 0 $c]
    }
}

;proc destruct w {
    variable class
    upvar \#0 [namespace current]::$w data
    set class(active) [lremove $class(active) $data(console)]
}

;proc configure { w args } {
    set namesp [namespace current]
    upvar \#0 ${namesp}::$w data
    variable class

    set truth {^(1|yes|true|on)$}
    set c $data(console)
    foreach {key val} $args {
	switch -- $key {
	    -blinkcolor	{
		$c tag config blink -background $val
		$c tag config __highlight -background $val
	    }
	    -proccolor   { $c tag config proc   -foreground $val }
	    -promptcolor { $c tag config prompt -foreground $val }
	    -stdincolor  {
		$c tag config stdin -foreground $val
		$c config -foreground $val
	    }
	    -stdoutcolor { $c tag config stdout -foreground $val }
	    -stderrcolor { $c tag config stderr -foreground $val }

	    -blinktime		{
		if {![regexp {[0-9]+} $val]} {
		    return -code error "$key option requires an integer value"
		} elseif {$val < 100} {
		    return -code error "$key option must be greater than 100"
		}
	    }
	    -grabputs	{
		if {[set val [regexp -nocase $truth $val]]} {
		    set class(active) [linsert $class(active) 0 $c]
		} else {
		    set class(active) [lremove -all $class(active) $c]
		}
	    }
	    -prompt		{
		if {[catch {namespace eval :: [list subst $val]} err]} {
		    return -code error "\"$val\" threw an error:\n$err"
		}
	    }
	    -showmenu	{
		if {[set val [regexp -nocase $truth $val]]} {
		    grid $data(menubar)
		} else {
		    grid remove $data(menubar)
		}
	    }
	    -lightbrace	-
	    -lightcmd	-
	    -showmultiple -
	    -subhistory	{ set val [regexp -nocase $truth $val] }
	}
	set data($key) $val
    }
}

;proc Exit {w args} {
    exit
}

## Eval - evaluates commands input into console window
## This is the first stage of the evaluating commands in the console.
## They need to be broken up into consituent commands (by CmdSep) in
## case a multiple commands were pasted in, then each is eval'ed (by
## EvalCmd) in turn.  Any uncompleted command will not be eval'ed.
# ARGS:	w	- console text widget
# Calls:	CmdGet, CmdSep, EvalCmd
## 
;proc Eval {w} {
    set incomplete [CmdSep [CmdGet $w] cmds last]
    $w mark set insert end-1c
    $w insert end \n
    if {[llength $cmds]} {
	foreach c $cmds {EvalCmd $w $c}
	$w insert insert $last {}
    } elseif {!$incomplete} {
	EvalCmd $w $last
    }
    $w see insert
}

## EvalCmd - evaluates a single command, adding it to history
# ARGS:	w	- console text widget
# 	cmd	- the command to evaluate
# Calls:	prompt
# Outputs:	result of command to stdout (or stderr if error occured)
# Returns:	next event number
## 
;proc EvalCmd {w cmd} {
    ## HACK to get $W as we need it
    set W [winfo parent $w]
    upvar \#0 [namespace current]::$W data

    $w mark set output end
    if {[string compare {} $cmd]} {
	set code 0
	if {$data(-subhistory)} {
	    set ev [EvalSlave history nextid]
	    incr ev -1
	    if {[string match !! $cmd]} {
		set code [catch {EvalSlave history event $ev} cmd]
		if {!$code} {$w insert output $cmd\n stdin}
	    } elseif {[regexp {^!(.+)$} $cmd dummy evnt]} {
		## Check last event because history event is broken
		set code [catch {EvalSlave history event $ev} cmd]
		if {!$code && ![string match ${evnt}* $cmd]} {
		    set code [catch {EvalSlave history event $evnt} cmd]
		}
		if {!$code} {$w insert output $cmd\n stdin}
	    } elseif {[regexp {^\^([^^]*)\^([^^]*)\^?$} $cmd dummy old new]} {
		set code [catch {EvalSlave history event $ev} cmd]
		if {!$code} {
		    regsub -all -- $old $cmd $new cmd
		    $w insert output $cmd\n stdin
		}
	    }
	}
	if {$code} {
	    $w insert output $cmd\n stderr
	} else {
	    ## We are about to evaluate the command, so move the limit
	    ## mark to ensure that further <Return>s don't cause double
	    ## evaluation of this command - for cases like the command
	    ## has a vwait or something in it
	    $w mark set limit end
	    EvalSlave history add $cmd
	    if {[catch {EvalAttached $cmd} res]} {
		if {[catch {EvalAttached {set errorInfo}} err]} {
		    set data(errorInfo) "Error getting errorInfo:\n$err"
		} else {
		    set data(errorInfo) $err
		}
		$w insert output $res\n stderr
	    } elseif {[string compare {} $res]} {
		$w insert output $res\n stdout
	    }
	}
    }
    prompt $W
    set data(event) [EvalSlave history nextid]
}

## EvalSlave - evaluates the args in the associated slave
## args should be passed to this procedure like they would be at
## the command line (not like to 'eval').
# ARGS:	args	- the command and args to evaluate
##
;proc EvalSlave {args} {
    uplevel \#0 $args
}

## EvalAttached
##
;proc EvalAttached {args} {
    uplevel \#0 eval $args
}

## CmdGet - gets the current command from the console widget
# ARGS:	w	- console text widget
# Returns:	text which compromises current command line
## 
;proc CmdGet w {
    if {[string match {} [$w tag nextrange prompt limit end]]} {
	$w tag add stdin limit end-1c
	return [$w get limit end-1c]
    }
}

## CmdSep - separates multiple commands into a list and remainder
# ARGS:	cmd	- (possible) multiple command to separate
# 	list	- varname for the list of commands that were separated.
#	rmd	- varname of any remainder (like an incomplete final command).
#		If there is only one command, it's placed in this var.
# Returns:	constituent command info in varnames specified by list & rmd.
## 
;proc CmdSep {cmd list last} {
    upvar 1 $list cmds $last inc
    set inc {}
    set cmds {}
    foreach c [split [string trimleft $cmd] \n] {
	if {[string compare $inc {}]} {
	    append inc \n$c
	} else {
	    append inc [string trimleft $c]
	}
	if {[info complete $inc] && ![regexp {[^\\]\\$} $inc]} {
	    ## FIX: is this necessary?
	    if {[regexp "^\[^#\]" $inc]} {lappend cmds $inc}
	    set inc {}
	}
    }
    set i [string compare $inc {}]
    if {!$i && [string compare $cmds {}] && ![string match *\n $cmd]} {
	set inc [lindex $cmds end]
	set cmds [lreplace $cmds end end]
    }
    return $i
}

## prompt - displays the prompt in the console widget
# ARGS:	w	- console text widget
# Outputs:	prompt (specified in data(-prompt)) to console
## 
;proc prompt {W {pre {}} {post {}} {prompt {}}} {
    upvar \#0 [namespace current]::$W data

    set w $data(console)
    if {[string compare {} $pre]} { $w insert end $pre stdout }
    set i [$w index end-1c]
    if {[string compare {} $data(appname)]} {
	$w insert end ">$data(appname)< " prompt
    }
    if {[string compare {} $prompt]} {
	$w insert end $prompt prompt
    } else {
	$w insert end [EvalSlave subst $data(-prompt)] prompt
    }
    $w mark set output $i
    $w mark set insert end
    $w mark set limit insert
    $w mark gravity limit left
    if {[string compare {} $post]} { $w insert end $post stdin }
    $w see end
}

## About - gives about info for Console
##
;proc About W {
    variable class
    upvar \#0 [namespace current]::$W data

    set w $W.about
    if {[winfo exists $w]} {
	wm deiconify $w
    } else {
	global tk_patchLevel tcl_patchLevel tcl_platform
	toplevel $w
	wm title $w $data(-abouttitle)
	button $w.b -text Dismiss -command [list wm withdraw $w]
	text $w.text -height 9 -bd 1 -width 60
	pack $w.b -fill x -side bottom
	pack $w.text -fill both -side left -expand 1
	$w.text tag config center -justify center
	if {[string compare unix $tcl_platform(platform)] || \
		[info tclversion] >= 8} {
	    $w.text tag config title -justify center -font {Courier 18 bold}
	} else {
	    $w.text tag config title -justify center -font *Courier*Bold*18*
	}
	$w.text insert 1.0 $data(-abouttitle) title \
		"$data(-abouttext)\n\nConsole Copyright 1995-1998\
		Jeffrey Hobbs, $class(contact)\
		\nRelease Date: v$class(version), $class(release)\
		\nDocumentation available at:\n$class(docs)\
		\nUsing: Tcl v$tcl_patchLevel / Tk v$tk_patchLevel" center
    }
}

## InitMenus - inits the menubar and popup for the console
# ARGS:	W	- console megawidget
## 
;proc InitMenus W {
    set V [namespace current]::$W
    upvar \#0 $V data

    set w    $data(menubar)
    set text $data(console)

    if {[catch {menu $w.pop -tearoff 0}]} {
	label $w.label -text "Menus not available in plugin mode"
	pack $w.label
	return
    }
    #bind [winfo toplevel $w] <Button-3> "tk_popup $w.pop %X %Y"
    bind $text <Button-3> "tk_popup $w.pop %X %Y"

    ## Console Menu
    ## FIX - get the attachment stuff working
    set n cons
    set l "Console"
    pack [menubutton $w.$n  -text $l -underline 0 -menu $w.$n.m] -side left
    $w.pop add cascade -label $l -underline 0 -menu $w.pop.$n
    foreach m [list [menu $w.$n.m -disabledforeground $data(-promptcolor)] \
	    [menu $w.pop.$n -disabledforeground $data(-promptcolor)]] {
	$m add command -label "Console $W" -state disabled
	$m add command -label "Clear Console " -underline 1 \
		-accelerator [event info <<Console_Clear>>] \
		-command [namespace code [list _clear $W]]
	$m add command -label "Load File" -underline 0 \
		-command [namespace code [list _load $W]]
	$m add cascade -label "Save ..." -underline 0 -menu $m.save
	$m add separator
	$m add cascade -label "Attach Console" -underline 7 -menu $m.apps \
		-state disabled
	$m add cascade -label "Attach Namespace" -underline 7 -menu $m.name \
		-state disabled
	$m add separator
	$m add command -label "Exit" -underline 1 \
		-accelerator [event info <<Console_Exit>>] \
		-command [namespace code [list Exit $W]]

	## Save Menu
	##
	set s $m.save
	menu $s -disabledforeground $data(-promptcolor) -tearoff 0
	$s add command -label "All"	-underline 0 \
		-command [namespace code [list _save $W all]]
	$s add command -label "History"	-underline 0 \
		-command [namespace code [list _save $W history]]
	$s add command -label "Stdin"	-underline 3 \
		-command [namespace code [list _save $W stdin]]
	$s add command -label "Stdout"	-underline 3 \
		-command [namespace code [list _save $W stdout]]
	$s add command -label "Stderr"	-underline 3 \
		-command [namespace code [list _save $W stderr]]

	## Attach Console Menu
	##
	menu $m.apps -disabledforeground $data(-promptcolor) \
		-postcommand [namespace code [list AttachMenu $W $m.apps]]

	## Attach Interpreter Menu
	##
	menu $m.int -disabledforeground $data(-promptcolor) -tearoff 0 \
		-postcommand [namespace code [list AttachMenu $W $m.int interp]]

	## Attach Namespace Menu
	##
	menu $m.name -disabledforeground $data(-promptcolor) -tearoff 0 \
		-postcommand [namespace code [list AttachMenu $W $m.name namespace]]
    }

    ## Edit Menu
    ##
    set n edit
    set l "Edit"
    pack [menubutton $w.$n -text $l -underline 0 -menu $w.$n.m] -side left
    $w.pop add cascade -label $l -underline 0 -menu $w.pop.$n
    foreach m [list [menu $w.$n.m] [menu $w.pop.$n]] {
	$m add command -label "Cut"   -underline 1 \
		-accelerator [lindex [event info <<Cut>>] 0] \
		-command [namespace code [list Cut $text]]
	$m add command -label "Copy"  -underline 1 \
		-accelerator [lindex [event info <<Copy>>] 0] \
		-command [namespace code [list Copy $text]]
	$m add command -label "Paste" -underline 0 \
		-accelerator [lindex [event info <<Paste>>] 0] \
		-command [namespace code [list Paste $text]]
	$m add separator
	$m add command -label "Find"  -underline 0 \
		-accelerator [lindex [event info <<Console_Find>>] 0] \
		-command [namespace code [list FindBox $W]]
	$m add separator
	$m add command -label "Last Error" -underline 0 \
		-command [list $W error]
    }

    ## Prefs Menu
    ##
    set n pref
    set l "Prefs"
    pack [menubutton $w.$n -text $l -underline 0 -menu $w.$n.m] -side left
    $w.pop add cascade -label $l -underline 0 -menu $w.pop.$n
    foreach m [list [menu $w.$n.m] [menu $w.pop.$n]] {
	$m add checkbutton -label "Brace Highlighting" \
		-variable $V\(-lightbrace\)
	$m add checkbutton -label "Command Highlighting" \
		-variable $V\(-lightcmd\)
	$m add checkbutton -label "Grab Puts Output" \
		-variable $V\(-grabputs\) \
		-command [namespace code "configure [list $W] \
		-grabputs \[set ${V}(-grabputs)\]"]
	$m add checkbutton -label "History Substitution" \
		-variable $V\(-subhistory\)
	$m add checkbutton -label "Show Multiple Matches" \
		-variable $V\(-showmultiple\)
	$m add checkbutton -label "Show Menubar" \
		-variable $V\(-showmenu\) \
		-command [namespace code "configure [list $W] \
		-showmenu \[set ${V}(-showmenu)\]"]
    }

    ## History Menu
    ##
    set n hist
    set l "History"
    pack [menubutton $w.$n -text $l -underline 0 -menu $w.$n.m] -side left
    $w.pop add cascade -label $l -underline 0 -menu $w.pop.$n
    foreach m [list $w.$n.m $w.pop.$n] {
	menu $m -disabledforeground $data(-promptcolor) \
		-postcommand [namespace code [list HistoryMenu $W $m]]
    }

    ## Help Menu
    ##
    set n help
    set l "Help"
    pack [menubutton $w.$n -text $l -underline 0 -menu $w.$n.m] -side right
    $w.pop add cascade -label $l -underline 0 -menu $w.pop.$n
    foreach m [list [menu $w.$n.m] [menu $w.pop.$n]] {
	$m config -disabledfore $data(-promptcolor)
	$m add command -label "About " -underline 0 \
		-accelerator [event info <<Console_About>>] \
		-command [namespace code [list About $W]]
    }

    bind $W <<Console_Exit>>	[namespace code [list Exit $W]]
    bind $W <<Console_About>>	[namespace code [list About $W]]
    bind $W <<Console_Help>>	[namespace code [list Help $W]]
    bind $W <<Console_Find>>	[namespace code [list FindBox $W]]

    ## Menu items need null PostConsole bindings to avoid the TagProc
    ##
    foreach ev [bind $W] {
	bind PostConsole $ev {
	    # empty
	}
    }
}

# AttachMenu --
#
#   ADD COMMENTS HERE
#
# Arguments:
#   args	comments
# Results:
#   Returns ...
#
;proc AttachMenu {W m {type default}} {
    upvar \#0 [namespace current]::$W data
}

## HistoryMenu - dynamically build the menu for attached interpreters
##
# ARGS:	w	- menu widget
##
;proc HistoryMenu {W w} {
    upvar \#0 [namespace current]::$W data

    if {![winfo exists $w]} return
    set id [EvalSlave history nextid]
    if {$data(histid)==$id} return
    set data(histid) $id
    $w delete 0 end
    set con $data(console)
    while {$id>0 && ($id>$data(histid)-10) && \
	    ![catch {EvalSlave history event [incr id -1]} tmp]} {
	set lbl [lindex [split $tmp "\n"] 0]
	if {[string len $lbl]>32} { set lbl [string range $tmp 0 29]... }
	$w add command -label "$id: $lbl" -command [namespace code "
	$con delete limit end
	$con insert limit [list $tmp]
	$con see end
	Eval $con\n"]
    }
}

## FindBox - creates minimal dialog interface to Find
# ARGS:	w	- text widget
#	str	- optional seed string for data(find)
##
;proc FindBox {W {str {}}} {
    set V [namespace current]::$W
    upvar \#0 $V data

    highlight_dialog $data(console)
    return

    set t $data(console)
    set base $W.find
    if {![winfo exists $base]} {
	toplevel $base
	wm withdraw $base
	wm title $base "Console Find"

	pack [frame $base.f] -fill x -expand 1
	label $base.f.l -text "Find:"
	entry $base.f.e -textvar $V\(find\)
	pack [frame $base.opt] -fill x
	checkbutton $base.opt.c -text "Case Sensitive" -var ${V}(find,case)
	checkbutton $base.opt.r -text "Use Regexp" -var ${V}(find,reg)
	pack $base.f.l -side left
	pack $base.f.e $base.opt.c $base.opt.r -side left -fill both -expand 1
	pack [frame $base.sep -bd 2 -relief sunken -height 4] -fill x
	pack [frame $base.btn] -fill both
	button $base.btn.fnd -text "Find" -width 6
	button $base.btn.clr -text "Clear" -width 6
	button $base.btn.dis -text "Dismiss" -width 6
	eval pack [winfo children $base.btn] -padx 4 -pady 2 \
		-side left -fill both

	focus $base.f.e

	bind $base.f.e <Return> [list $base.btn.fnd invoke]
	bind $base.f.e <Escape> [list $base.btn.dis invoke]
    }
    $base.btn.fnd config -command [namespace code \
	    "highlight [list $data(console)] \[set ${V}(find)\] \
	    \[expr {\[set ${V}(find,case)\]?{}:{-nocase}}] \
	    \[expr {\[set ${V}(find,reg)\]?{-regexp}:{}}] \
	    -tag __highlight -color [list $data(-blinkcolor)]"]
    $base.btn.clr config -command "
    $t tag remove __highlight 1.0 end
    set ${V}(find) {}
    "
    $base.btn.dis config -command "
    $t tag remove __highlight 1.0 end
    wm withdraw $base
    "
    if {[string compare {} $str]} {
	set data(find) $str
	$base.btn.fnd invoke
    }

    if {[string compare normal [wm state $base]]} {
	wm deiconify $base
    } else { raise $base }
    $base.f.e select range 0 end
}

## savecommand - saves a command in a buffer for later retrieval
#
##
;proc savecommand {w} {
    upvar \#0 [namespace current]::[winfo parent $w] data

    set tmp $data(cmdsave)
    set data(cmdsave) [CmdGet $w]
    if {[string match {} $data(cmdsave)]} {
	set data(cmdsave) $tmp
    } else {
	$w delete limit end-1c
    }
    $w insert limit $tmp
    $w see end
}

## _load - sources a file into the console
# ARGS:	fn	- (optional) filename to source in
# Returns:	selected filename ({} if nothing was selected)
## 
;proc _load {W {fn ""}} {
    set types {
	{{Tcl Files}	{.tcl .tk}}
	{{Text Files}	{.txt}}
	{{All Files}	*}
    }
    if {
	[string match {} $fn] &&
	([catch {tk_getOpenFile -filetypes $types \
	    -title "Source File into Attached Interpreter"} fn]
	|| [string match {} $fn])
    } { return }
    EvalAttached [list source $fn]
}

## _save - saves the console buffer to a file
## This does not eval in a slave because it's not necessary
# ARGS:	w	- console text widget
# 	fn	- (optional) filename to save to
## 
;proc _save {W {type ""} {fn ""}} {
    upvar \#0 [namespace current]::$W data

    set c $data(console)
    if {![regexp -nocase {^(all|history|stdin|stdout|stderr)$} $type]} {
	array set s { 0 All 1 History 2 Stdin 3 Stdout 4 Stderr 5 Cancel }
	## Allow user to specify what kind of stuff to save
	set type [tk_dialog $W.savetype "Save Type" \
		"What part of the console text do you want to save?" \
		questhead 0 $s(0) $s(1) $s(2) $s(3) $s(4) $s(5)]
	if {$type == 5 || $type == -1} return
	set type $s($type)
    }
    if {[string match {} $fn]} {
	set types {
	    {{Text Files}	{.txt}}
	    {{Tcl Files}	{.tcl .tk}}
	    {{All Files}	*}
	}
	if {[catch {tk_getSaveFile -filetypes $types -title "Save $type"} fn] \
		|| [string match {} $fn]} return
    }
    set type [string tolower $type]
    set output {}
    switch $type {
	stdin -	stdout - stderr {
	    foreach {first last} [$c tag ranges $type] {
		lappend output [$c get $first $last]
	    }
	    set output [join $data \n]
	}
	history		{ set output [_history $W] }
	all - default	{ set output [$c get 1.0 end-1c] }
    }
    if {[catch {open $fn w} fid]} {
	return -code error "Save Error: Unable to open '$fn' for writing\n$fid"
    }
    puts $fid $output
    close $fid
}

## clear - clears the buffer of the console (not the history though)
## 
;proc _clear {W {pcnt 100}} {
    upvar \#0 [namespace current]::$W data

    set data(tmp) [CmdGet $data(console)]
    if {![regexp {^[0-9]*$} $pcnt] || $pcnt < 1 || $pcnt > 100} {
	return -code error \
		"invalid percentage to clear: must be 1-100 (100 default)"
    } elseif {$pcnt == 100} {
	$data(console) delete 1.0 end
    } else {
	set tmp [expr $pcnt/100.0*[$data(console) index end]]
	$data(console) delete 1.0 "$tmp linestart"
    }
    prompt $W {} $data(tmp)
}

;proc _error {W} {
    ## Outputs stack caused by last error.
    upvar \#0 [namespace current]::$W data
    set info $data(errorInfo)
    if {[string match {} $info]} { set info {errorInfo empty} }
    catch {destroy $W.error}
    set w [toplevel $W.error]
    wm title $w "Console Last Error"
    button $w.close -text Dismiss -command [list destroy $w]
    scrollbar $w.sy -takefocus 0 -bd 1 -command [list $w.text yview]
    text $w.text -yscrollcommand [list $w.sy set]
    pack $w.close -side bottom -fill x
    pack $w.sy -side right -fill y
    pack $w.text -fill both -expand 1
    $w.text insert 1.0 $info
    $w.text config -state disabled
}

## _event - searches for history based on a string
## Search forward (next) if $int>0, otherwise search back (prev)
# ARGS:	W	- console widget
##
;proc _event {W int {str {}}} {
    upvar \#0 [namespace current]::$W data

    if {!$int} return
    set w $data(console)

    set nextid [EvalSlave history nextid]
    if {[string compare {} $str]} {
	## String is not empty, do an event search
	set event $data(event)
	if {$int < 0 && $event == $nextid} { set data(cmdbuf) $str }
	set len [string len $data(cmdbuf)]
	incr len -1
	if {$int > 0} {
	    ## Search history forward
	    while {$event < $nextid} {
		if {[incr event] == $nextid} {
		    $w delete limit end
		    $w insert limit $data(cmdbuf)
		    break
		} elseif {![catch {EvalSlave history event $event} res] \
			&& ![string compare $data(cmdbuf) \
			[string range $res 0 $len]]} {
		    $w delete limit end
		    $w insert limit $res
		    break
		}
	    }
	    set data(event) $event
	} else {
	    ## Search history reverse
	    while {![catch {EvalSlave history event [incr event -1]} res]} {
		if {![string compare $data(cmdbuf) \
			[string range $res 0 $len]]} {
		    $w delete limit end
		    $w insert limit $res
		    set data(event) $event
		    break
		}
	    }
	} 
    } else {
	## String is empty, just get next/prev event
	if {$int > 0} {
	    ## Goto next command in history
	    if {$data(event) < $nextid} {
		$w delete limit end
		if {[incr data(event)] == $nextid} {
		    $w insert limit $data(cmdbuf)
		} else {
		    $w insert limit [EvalSlave history event $data(event)]
		}
	    }
	} else {
	    ## Goto previous command in history
	    if {$data(event) == $nextid} {set data(cmdbuf) [CmdGet $w]}
	    if {[catch {EvalSlave history event [incr data(event) -1]} res]} {
		incr data(event)
	    } else {
		$w delete limit end
		$w insert limit $res
	    }
	}
    }
    $w mark set insert end
    $w see end
}

;proc _history {W args} {
    set sub {\2}
    if {[string match -n* $args]} { append sub "\n" }
    set h [EvalSlave history]
    regsub -all "( *\[0-9\]+  |\t)(\[^\n\]*\n?)" $h $sub h
    return $h
}

##
## Some procedures to make up for lack of built-in shell commands
##

## puts
## This allows me to capture all stdout/stderr to the console window
# ARGS:	same as usual	
# Outputs:	the string with a color-coded text tag
## 
if {![catch {rename ::puts ::console_tcl_puts}]} {
    ;proc ::puts args {
	if {![catch {widget value Console active} active] && \
		[winfo exists [lindex $active 0]]} {
	    set w [lindex $active 0]
	    set len [llength $args]
	    if {$len==1} {
		eval $w insert output $args stdout {\n} stdout
		$w see output
	    } elseif {$len==2 && [regexp {(stdout|stderr|-nonewline)} \
		    [lindex $args 0] junk tmp]} {
		if {[string compare $tmp -nonewline]} {
		    eval $w insert output [lreplace $args 0 0] $tmp {\n} $tmp
		} else {
		    eval $w insert output [lreplace $args 0 0] stdout
		}
		$w see output
	    } elseif {$len==3 && \
		    [regexp {(stdout|stderr)} [lreplace $args 2 2] junk tmp]} {
		if {[string compare [lreplace $args 1 2] -nonewline]} {
		    eval $w insert output [lrange $args 1 1] $tmp
		} else {
		    eval $w insert output [lreplace $args 0 1] $tmp
		}
		$w see output
	    } else {
		global errorCode errorInfo
		if {[catch "::console_tcl_puts $args" msg]} {
		    regsub console_tcl_puts $msg puts msg
		    regsub -all console_tcl_puts \
			    $errorInfo puts errorInfo
		    error $msg
		}
		return $msg
	    }
	    if {$len} update
	} else {
	    global errorCode errorInfo
	    if {[catch "::console_tcl_puts $args" msg]} {
		regsub console_tcl_puts $msg puts msg
		regsub -all console_tcl_puts $errorInfo puts errorInfo
		error $msg
	    }
	    return $msg
	}
    }
}

if {!$class(WWW)} {;
## We exclude the reworking of unknown for the plugin

## Unknown changed to get output into Console window
# unknown:
# Invoked automatically whenever an unknown command is encountered.
# Works through a list of "unknown handlers" that have been registered
# to deal with unknown commands.  Extensions can integrate their own
# handlers into the "unknown" facility via "unknown_handle".
#
# If a handler exists that recognizes the command, then it will
# take care of the command action and return a valid result or a
# Tcl error.  Otherwise, it should return "-code continue" (=2)
# and responsibility for the command is passed to the next handler.
#
# Arguments:
# args -	A list whose elements are the words of the original
#		command, including the command name.

proc ::unknown args {
    global unknown_handler_order unknown_handlers errorInfo errorCode

    #
    # Be careful to save error info now, and restore it later
    # for each handler.  Some handlers generate their own errors
    # and disrupt handling.
    #
    set savedErrorCode $errorCode
    set savedErrorInfo $errorInfo

    if {![info exists unknown_handler_order] || \
	    ![info exists unknown_handlers]} {
	set unknown_handlers(tcl) tcl_unknown
	set unknown_handler_order tcl
    }

    foreach handler $unknown_handler_order {
        set status [catch {uplevel $unknown_handlers($handler) $args} result]

        if {$status == 1} {
            #
            # Strip the last five lines off the error stack (they're
            # from the "uplevel" command).
            #
            set new [split $errorInfo \n]
            set new [join [lrange $new 0 [expr [llength $new] - 6]] \n]
            return -code $status -errorcode $errorCode \
		    -errorinfo $new $result

        } elseif {$status != 4} {
            return -code $status $result
        }

        set errorCode $savedErrorCode
        set errorInfo $savedErrorInfo
    }

    set name [lindex $args 0]
    return -code error "invalid command name \"$name\""
}

# tcl_unknown:
# Invoked when a Tcl command is invoked that doesn't exist in the
# interpreter:
#
#	1. See if the autoload facility can locate the command in a
#	   Tcl script file.  If so, load it and execute it.
#	2. If the command was invoked interactively at top-level:
#	    (a) see if the command exists as an executable UNIX program.
#		If so, "exec" the command.
#	    (b) see if the command requests csh-like history substitution
#		in one of the common forms !!, !<number>, or ^old^new.  If
#		so, emulate csh's history substitution.
#	    (c) see if the command is a unique abbreviation for another
#		command.  If so, invoke the command.
#
# Arguments:
# args -	A list whose elements are the words of the original
#		command, including the command name.

proc ::tcl_unknown args {
    global auto_noexec auto_noload env unknown_pending tcl_interactive
    global errorCode errorInfo

    # Save the values of errorCode and errorInfo variables, since they
    # may get modified if caught errors occur below.  The variables will
    # be restored just before re-executing the missing command.

    set savedErrorCode $errorCode
    set savedErrorInfo $errorInfo
    set name [lindex $args 0]
    if {![info exists auto_noload]} {
	#
	# Make sure we're not trying to load the same proc twice.
	#
	if {[info exists unknown_pending($name)]} {
	    return -code error "self-referential recursion in \"unknown\" for command \"$name\"";
	}
	set unknown_pending($name) pending;
	set ret [catch {auto_load $name} msg]
	unset unknown_pending($name);
	if {$ret != 0} {
	    return -code $ret -errorcode $errorCode \
		    "error while autoloading \"$name\": $msg"
	}
	if {![array size unknown_pending]} {
	    unset unknown_pending
	}
	if {$msg} {
	    set errorCode $savedErrorCode
	    set errorInfo $savedErrorInfo
	    set code [catch {uplevel 1 $args} msg]
	    if {$code ==  1} {
		#
		# Strip the last five lines off the error stack (they're
		# from the "uplevel" command).
		#

		set new [split $errorInfo \n]
		set new [join [lrange $new 0 [expr [llength $new] - 6]] \n]
		return -code error -errorcode $errorCode \
			-errorinfo $new $msg
	    } else {
		return -code $code $msg
	    }
	}
    }
    if {[info level] == 1 && [string match {} [info script]] \
	    && [info exists tcl_interactive] && $tcl_interactive} {
	if {![info exists auto_noexec]} {
	    set new [auto_execok $name]
	    if {[string compare $new ""]} {
		set errorCode $savedErrorCode
		set errorInfo $savedErrorInfo
                set redir ""
                if {[info commands console] == ""} {
                    set redir ">&@stdout <@stdin"
                }
                return [uplevel exec $redir $new [lrange $args 1 end]]
	    }
	}
	set errorCode $savedErrorCode
	set errorInfo $savedErrorInfo
	##
	## History substitution moved into EvalCmd
	##
	set ret [catch {set cmds [info commands $name*]} msg]
	if {![string compare $name "::"]} {
	    set name ""
	}
	if {$ret != 0} {
	    return -code $ret -errorcode $errorCode \
		"error in unknown while checking if \"$name\" is a unique command abbreviation: $msg"
	}
	if {[llength $cmds] == 1} {
	    return [uplevel [lreplace $args 0 0 $cmds]]
	}
	if {[llength $cmds]} {
	    if {$name == ""} {
		return -code error "empty command name \"\""
	    } else {
		return -code error \
			"ambiguous command name \"$name\": [lsort $cmds]"
	    }
	}
    }
    return -code continue
}

}; # end switch on proc unknown

switch -glob $tcl_platform(platform) {
    win* { set META Alt }
    mac* { set META Command }
    default { set META Meta }
}

# ClipboardKeysyms --
# This procedure is invoked to identify the keys that correspond to
# the "copy", "cut", and "paste" functions for the clipboard.
#
# Arguments:
# copy -	Name of the key (keysym name plus modifiers, if any,
#		such as "Meta-y") used for the copy operation.
# cut -		Name of the key used for the cut operation.
# paste -	Name of the key used for the paste operation.

;proc ClipboardKeysyms {copy cut paste} {
    bind Console <$copy>	{Copy %W}
    bind Console <$cut>		{Cut %W}
    bind Console <$paste>	{Paste %W}
}

;proc Cut w {
    if {[string match $w [selection own -displayof $w]]} {
	clipboard clear -displayof $w
	catch {
	    clipboard append -displayof $w [selection get -displayof $w]
	    if {[$w compare sel.first >= limit]} {$w delete sel.first sel.last}
	}
    }
}
;proc Copy w {
    if {[string match $w [selection own -displayof $w]]} {
	clipboard clear -displayof $w
	catch {clipboard append -displayof $w [selection get -displayof $w]}
    }
}

;proc Paste w {
    if {
	![catch {selection get -displayof $w} tmp] ||
	![catch {selection get -displayof $w -type TEXT} tmp] ||
	![catch {selection get -displayof $w -selection CLIPBOARD} tmp]
    } {
	if {[$w compare insert < limit]} {$w mark set insert end}
	$w insert insert $tmp
	$w see insert
	if {[string match *\n* $tmp]} {Eval $w}
    }
}

## Get all Text bindings into Console
foreach ev [bind Text] { bind Console $ev [namespace code [bind Text $ev]] }
## We don't want newline insertion
bind Console <Control-Key-o> {}

foreach {ev key} {
    <<Console_Prev>>		<Key-Up>
    <<Console_Next>>		<Key-Down>
    <<Console_NextImmediate>>	<Control-Key-n>
    <<Console_PrevImmediate>>	<Control-Key-p>
    <<Console_PrevSearch>>	<Control-Key-r>
    <<Console_NextSearch>>	<Control-Key-s>

    <<Console_Expand>>		<Key-Tab>
    <<Console_ExpandFile>>	<Key-Escape>
    <<Console_ExpandProc>>	<Control-Shift-Key-P>
    <<Console_ExpandVar>>	<Control-Shift-Key-V>
    <<Console_Tab>>		<Control-Key-i>
    <<Console_Tab>>		<Meta-Key-i>
    <<Console_Eval>>		<Key-Return>
    <<Console_Eval>>		<Key-KP_Enter>

    <<Console_Clear>>		<Control-Key-l>
    <<Console_KillLine>>	<Control-Key-k>
    <<Console_Transpose>>	<Control-Key-t>
    <<Console_ClearLine>>	<Control-Key-u>
    <<Console_SaveCommand>>	<Control-Key-z>

    <<Console_Exit>>		<Control-Key-q>
    <<Console_New>>		<Control-Key-N>
    <<Console_Close>>		<Control-Key-w>
    <<Console_About>>		<Control-Key-A>
    <<Console_Help>>		<Control-Key-H>
    <<Console_Find>>		<Control-Key-F>
} {
    event add $ev $key
    bind Console $key {}
}
catch {unset ev key}

## Redefine for Console what we need
##
event delete <<Paste>> <Control-V>
ClipboardKeysyms <Copy> <Cut> <Paste>

bind Console <Insert> {catch {Insert %W [selection get -displayof %W]}}

bind Console <Triple-1> {+
catch {
    eval %W tag remove sel [%W tag nextrange prompt sel.first sel.last]
    eval %W tag remove sel sel.last-1c
    %W mark set insert sel.first
}
}

bind Console <<Console_Expand>> [namespace code {
    if {[%W compare insert > limit]} {Expand %W}
    break
}]
bind Console <<Console_ExpandFile>> [namespace code {
    if {[%W compare insert > limit]} {Expand %W path}
    break
}]
bind Console <<Console_ExpandProc>> [namespace code {
    if {[%W compare insert > limit]} {Expand %W proc}
    break
}]
bind Console <<Console_ExpandVar>> [namespace code {
    if {[%W compare insert > limit]} {Expand %W var}
    break
}]
bind Console <<Console_Tab>> [namespace code {
    if {[%W compare insert >= limit]} {	Insert %W \t }
}]
bind Console <<Console_Eval>> [namespace code { Eval %W }]
bind Console <Delete> {
    if {[string compare {} [%W tag nextrange sel 1.0 end]] \
	    && [%W compare sel.first >= limit]} {
	%W delete sel.first sel.last
    } elseif {[%W compare insert >= limit]} {
	%W delete insert
	%W see insert
    }
}
bind Console <BackSpace> {
    if {[string compare {} [%W tag nextrange sel 1.0 end]] \
	    && [%W compare sel.first >= limit]} {
	%W delete sel.first sel.last
    } elseif {[%W compare insert != 1.0] && [%W compare insert > limit]} {
	%W delete insert-1c
	%W see insert
    }
}
bind Console <Control-h> [bind Console <BackSpace>]

bind Console <KeyPress> [namespace code { Insert %W %A }]

bind Console <Control-a> {
    if {[%W compare {limit linestart} == {insert linestart}]} {
	tkTextSetCursor %W limit
    } else {
	tkTextSetCursor %W {insert linestart}
    }
}
bind Console <Control-d> {
    if {[%W compare insert < limit]} break
    %W delete insert
}
bind Console <<Console_KillLine>> {
    if {[%W compare insert < limit]} break
    if {[%W compare insert == {insert lineend}]} {
	%W delete insert
    } else {
	%W delete insert {insert lineend}
    }
}
bind Console <<Console_Clear>> [namespace code { _clear [winfo parent %W] }]
bind Console <<Console_Prev>> [namespace code {
    if {[%W compare {insert linestart} != {limit linestart}]} {
	tkTextSetCursor %W [tkTextUpDownLine %W -1]
    } else {
	_event [winfo parent %W] -1
    }
}]
bind Console <<Console_Next>> [namespace code {
    if {[%W compare {insert linestart} != {end-1c linestart}]} {
	tkTextSetCursor %W [tkTextUpDownLine %W 1]
    } else {
	_event [winfo parent %W] 1
    }
}]
bind Console <<Console_NextImmediate>> [namespace code {
    _event [winfo parent %W] 1
}]
bind Console <<Console_PrevImmediate>> [namespace code {
    _event [winfo parent %W] -1
}]
bind Console <<Console_PrevSearch>> [namespace code {
    _event [winfo parent %W] -1 [CmdGet %W]
}]
bind Console <<Console_NextSearch>> [namespace code {
    _event [winfo parent %W] 1 [CmdGet %W]
}]
bind Console <<Console_Transpose>> {
    ## Transpose current and previous chars
    if {[%W compare insert > limit]} { tkTextTranspose %W }
}
bind Console <<Console_ClearLine>> {
    ## Clear command line (Unix shell staple)
    %W delete limit end
}
bind Console <<Console_SaveCommand>> [namespace code {
    ## Save command buffer (swaps with current command)
    savecommand %W
}]
catch {bind Console <Key-Page_Up>   { tkTextScrollPages %W -1 }}
catch {bind Console <Key-Prior>     { tkTextScrollPages %W -1 }}
catch {bind Console <Key-Page_Down> { tkTextScrollPages %W 1 }}
catch {bind Console <Key-Next>      { tkTextScrollPages %W 1 }}
bind Console <$META-d> {
    if {[%W compare insert >= limit]} {
	%W delete insert {insert wordend}
    }
}
bind Console <$META-BackSpace> {
    if {[%W compare {insert -1c wordstart} >= limit]} {
	%W delete {insert -1c wordstart} insert
    }
}
bind Console <$META-Delete> {
    if {[%W compare insert >= limit]} {
	%W delete insert {insert wordend}
    }
}
bind Console <ButtonRelease-2> {
    ## Try and get the default selection, then try and get the selection
    ## type TEXT, then try and get the clipboard if nothing else is available
    if {
	(!$tkPriv(mouseMoved) || $tk_strictMotif) &&
	(![catch {selection get -displayof %W} tkPriv(junk)] ||
	![catch {selection get -displayof %W -type TEXT} tkPriv(junk)] ||
	![catch {selection get -displayof %W \
		-selection CLIPBOARD} tkPriv(junk)])
    } {
	if {[%W compare @%x,%y < limit]} {
	    %W insert end $tkPriv(junk)
	} else {
	    %W insert @%x,%y $tkPriv(junk)
	}
	if {[string match *\n* $tkPriv(junk)]} {
	    namespace inscope ::Widget::Console { Eval %W }
	}
    }
}

##
## End Console bindings
##

##
## Bindings for doing special things based on certain keys
##
bind PostConsole <Key-parenright> [namespace code {
    if {[string compare \\ [%W get insert-2c]]} {MatchPair %W \( \) limit}
}]
bind PostConsole <Key-bracketright> [namespace code {
    if {[string compare \\ [%W get insert-2c]]} {MatchPair %W \[ \] limit}
}]
bind PostConsole <Key-braceright> [namespace code {
    if {[string compare \\ [%W get insert-2c]]} {MatchPair %W \{ \} limit}
}]
bind PostConsole <Key-quotedbl> [namespace code {
    if {[string compare \\ [%W get insert-2c]]} {MatchQuote %W limit}
}]

bind PostConsole <KeyPress> [namespace code {
    if {[string compare {} %A]} {TagProc %W}
}]


## TagProc - tags a procedure in the console if it's recognized
## This procedure is not perfect.  However, making it perfect wastes
## too much CPU time...  Also it should check the existence of a command
## in whatever is the connected slave, not the master interpreter.
##
;proc TagProc w {
    upvar \#0 [namespace current]::[winfo parent $w] data
    if {!$data(-lightcmd)} return
    if {[info tclversion] > 8.0} {
	set exp {[^\E][[ \t\n\r;\{\"$]}
	set i [$w search -backwards -regexp $exp insert-1c limit-1c]
	if {[string compare {} $i]} {append i +2c} {set i limit}
	regsub -all {[[\E\?\*]} [$w get $i "insert-1c wordend"] {\\\0} c
    } else {
	set exp "\[^\\]\[ \t\n\r\[\;\{\"\$]"
	set i [$w search -backwards -regexp $exp insert-1c limit-1c]
	if {[string compare {} $i]} {append i +2c} {set i limit}
	regsub -all {[[\\\?\*]} [$w get $i "insert-1c wordend"] {\\\0} c
    }
    if {[string compare {} [EvalAttached info commands [list $c]]]} {
	$w tag add proc $i "insert-1c wordend"
    } else {
	$w tag remove proc $i "insert-1c wordend"
    }
    if {[string compare {} [EvalAttached info vars [list $c]]]} {
	$w tag add var $i "insert-1c wordend"
    } else {
	$w tag remove var $i "insert-1c wordend"
    }
}

## MatchPair - blinks a matching pair of characters
## c2 is assumed to be at the text index 'insert'.
## This proc is really loopy and took me an hour to figure out given
## all possible combinations with escaping except for escaped \'s.
## It doesn't take into account possible commenting... Oh well.  If
## anyone has something better, I'd like to see/use it.  This is really
## only efficient for small contexts.
# ARGS:	w	- console text widget
# 	c1	- first char of pair
# 	c2	- second char of pair
# Calls:	blink
## 
;proc MatchPair {w c1 c2 {lim 1.0}} {
    upvar \#0 [namespace current]::[winfo parent $w] data
    if {!$data(-lightbrace) || $data(-blinktime)<100} return
    if {[string compare [set ix [$w search -back $c1 insert $lim]] {}]} {
	while {[string match {\\} [$w get $ix-1c]] && \
		[string compare [set ix [$w search -back $c1 $ix-1c $lim]] {}]} {}
	set i1 insert-1c
	while {[string compare $ix {}]} {
	    set i0 $ix
	    set j 0
	    while {[string compare [set i0 [$w search $c2 $i0 $i1]] {}]} {
		append i0 +1c
		if {[string match {\\} [$w get $i0-2c]]} continue
		incr j
	    }
	    if {!$j} break
	    set i1 $ix
	    while {$j && [string compare \
		    [set ix [$w search -back $c1 $ix $lim]] {}]} {
		if {[string match {\\} [$w get $ix-1c]]} continue
		incr j -1
	    }
	}
	if {[string match {} $ix]} { set ix [$w index $lim] }
    } else {
	set ix [$w index $lim]
    }
    if {$data(-blinkrange)} {
	blink $w $data(-blinktime) $ix [$w index insert]
    } else {
	blink $w $data(-blinktime) $ix $ix+1c \
		[$w index insert-1c] [$w index insert]
    }
}

## MatchQuote - blinks between matching quotes.
## Blinks just the quote if it's unmatched, otherwise blinks quoted string
## The quote to match is assumed to be at the text index 'insert'.
# ARGS:	w	- console text widget
# Calls:	blink
## 
;proc MatchQuote {w {lim 1.0}} {
    upvar \#0 [namespace current]::[winfo parent $w] data
    if {!$data(-lightbrace) || $data(-blinktime)<100} return
    set i insert-1c
    set j 0
    while {[string compare {} [set i [$w search -back \" $i $lim]]]} {
	if {[string match {\\} [$w get $i-1c]]} continue
	if {!$j} {set i0 $i}
	incr j
    }
    if {[expr $j%2]} {
	if {$data(-blinkrange)} {
	    blink $w $data(-blinktime) $i0 [$w index insert]
	} else {
	    blink $w $data(-blinktime) $i0 $i0+1c \
		    [$w index insert-1c] [$w index insert]
	}
    } else {
	blink $w $data(-blinktime) [$w index insert-1c] \
		[$w index insert]
    }
}

## blink - blinks between 2 indices for a specified duration.
# ARGS:	w	- console text widget
#	delay	- millisecs to blink for
# 	args	- indices of regions to blink
# Outputs:	blinks selected characters in $w
## 
;proc blink {w delay args} {
    eval $w tag add blink $args
    after $delay eval $w tag remove blink $args
    return
}


## Insert
## Insert a string into a text console at the point of the insertion cursor.
## If there is a selection in the text, and it covers the point of the
## insertion cursor, then delete the selection before inserting.
# ARGS:	w	- text window in which to insert the string
# 	s	- string to insert (usually just a single char)
# Outputs:	$s to text widget
## 
;proc Insert {w s} {
    if {[string match {} $s] || [string match disabled [$w cget -state]]} {
	return
    }
    if {[$w comp insert < limit]} {
	$w mark set insert end
    }
    catch {
	if {[$w comp sel.first <= insert] && [$w comp sel.last >= insert]} {
	    $w delete sel.first sel.last
	}
    }
    $w insert insert $s
    $w see insert
}

## Expand - 
# ARGS:	w	- text widget in which to expand str
# 	type	- type of expansion (path / proc / variable)
# Calls:	Expand(Pathname|Procname|Variable)
# Outputs:	The string to match is expanded to the longest possible match.
#		If data(-showmultiple) is non-zero and the user longest match
#		equaled the string to expand, then all possible matches are
#		output to stdout.  Triggers bell if no matches are found.
# Returns:	number of matches found
## 
## FIX: make namespace aware
;proc Expand {w {type ""}} {
    if {[info tclversion] > 8.0} {
	set exp {[^\E][[ \t\n\r\{\"$]}
    } else {
	set exp "\[^\\]\[ \t\n\r\[\{\"\$]"
    }
    set tmp [$w search -backwards -regexp $exp insert-1c limit-1c]
    if {[string compare {} $tmp]} {append tmp +2c} else {set tmp limit}
    if {[$w compare $tmp >= insert]} return
    set str [$w get $tmp insert]
    switch -glob $type {
	pa* { set res [ExpandPathname $str] }
	pr* { set res [ExpandProcname $str] }
	v*  { set res [ExpandVariable $str] }
	default {
	    set res {}
	    foreach t {Pathname Procname Variable} {
		if {[string compare {} [set res [Expand$t $str]]]} break
	    }
	}
    }
    set len [llength $res]
    if {$len} {
	$w delete $tmp insert
	$w insert $tmp [lindex $res 0]
	if {$len > 1} {
	    upvar \#0 [namespace current]::[winfo parent $w] data
	    if {$data(-showmultiple) && \
		    ![string compare [lindex $res 0] $str]} {
		puts stdout [lsort [lreplace $res 0 0]]
	    }
	}
    } else { bell }
    return [incr len -1]
}

## ExpandPathname - expand a file pathname based on $str
## This is based on UNIX file name conventions
# ARGS:	str	- partial file pathname to expand
# Calls:	ExpandBestMatch
# Returns:	list containing longest unique match followed by all the
#		possible further matches
## 
;proc ExpandPathname str {
    set pwd [EvalAttached pwd]
    if {[catch {EvalAttached [list cd [file dirname $str]]} err]} {
	return -code error $err
    }
    if {[catch {lsort [EvalAttached [list glob [file tail $str]*]]} m]} {
	set match {}
    } else {
	if {[llength $m] > 1} {
	    global tcl_platform
	    if {[string match windows $tcl_platform(platform)]} {
		## Windows is screwy because it's can be case insensitive
		set tmp [best_match [string tolower $m] \
			[string tolower [file tail $str]]]
	    } else {
		set tmp [best_match $m [file tail $str]]
	    }
	    if {[string match ?*/* $str]} {
		set tmp [file dirname $str]/$tmp
	    } elseif {[string match /* $str]} {
		set tmp /$tmp
	    }
	    regsub -all { } $tmp {\\ } tmp
	    set match [linsert $m 0 $tmp]
	} else {
	    ## This may look goofy, but it handles spaces in path names
	    eval append match $m
	    if {[file isdir $match]} {append match /}
	    if {[string match ?*/* $str]} {
		set match [file dirname $str]/$match
	    } elseif {[string match /* $str]} {
		set match /$match
	    }
	    regsub -all { } $match {\\ } match
	    ## Why is this one needed and the ones below aren't!!
	    set match [list $match]
	}
    }
    EvalAttached [list cd $pwd]
    return $match
}

## ExpandProcname - expand a tcl proc name based on $str
# ARGS:	str	- partial proc name to expand
# Calls:	best_match
# Returns:	list containing longest unique match followed by all the
#		possible further matches
## 
;proc ExpandProcname str {
    set match [EvalAttached [list info commands $str*]]
    if {[llength $match] > 1} {
	regsub -all { } [best_match $match $str] {\\ } str
	set match [linsert $match 0 $str]
    } else {
	regsub -all { } $match {\\ } match
    }
    return $match
}

## ExpandVariable - expand a tcl variable name based on $str
# ARGS:	str	- partial tcl var name to expand
# Calls:	best_match
# Returns:	list containing longest unique match followed by all the
#		possible further matches
## 
;proc ExpandVariable str {
    if {[regexp {([^\(]*)\((.*)} $str junk ary str]} {
	## Looks like they're trying to expand an array.
	set match [EvalAttached [list array names $ary $str*]]
	if {[llength $match] > 1} {
	    set vars $ary\([best_match $match $str]
	    foreach var $match {lappend vars $ary\($var\)}
	    return $vars
	} else {set match $ary\($match\)}
	## Space transformation avoided for array names.
    } else {
	set match [EvalAttached [list info vars $str*]]
	if {[llength $match] > 1} {
	    regsub -all { } [best_match $match $str] {\\ } str
	    set match [linsert $match 0 $str]
	} else {
	    regsub -all { } $match {\\ } match
	}
    }
    return $match
}

## resource - re'source's this script into current console
## Meant primarily for my development of this program.  It follows
## links until the ultimate source is found.
##
set class(SCRIPT) [info script]
if {!$class(WWW)} {
    while {[string match link [file type $class(SCRIPT)]]} {
	set link [file readlink $class(SCRIPT)]
	if {[string match relative [file pathtype $link]]} {
	    set class(SCRIPT) [file join \
		    [file dirname $class(SCRIPT)] $link]
	} else {
	    set class(SCRIPT) $link
	}
    }
    catch {unset link}
    if {[string match relative [file pathtype $class(SCRIPT)]]} {
	set class(SCRIPT) [file join [pwd] $class(SCRIPT)]
    }
}

;proc resource {} {
    upvar \#0 [namespace current] class
    uplevel \#0 [list source $class(SCRIPT)]
}

}; # end namespace ::Widget::Console
