##
## Copyright 1996-8 Jeffrey Hobbs, jeff.hobbs@acm.org
##
package require Widget 2.0
package provide Combobox 2.0

## FIX:
## popdown listbox on Configure
## 
## Modified by Kish Shen, 99-3-12, to allow listbox to be closed if cursor
## leaves the window (-closeonleave option)
##
## Kish 2008-07-04: Added [list..] for -command so that the argument can 
## have spaces.
##
##------------------------------------------------------------------------
## PROCEDURE
##	combobox
##
## DESCRIPTION
##	Implements a Combobox megawidget
##
## ARGUMENTS
##	combobox <window pathname> <options>
##
## OPTIONS
##	(Any entry widget option may be used in addition to these)
##
## -click single|double			DEFAULT: double
##	Whether a single or double-click will select an item in the listbox.
##	If you choose single click, then the selection will follow the
##	motion of the mouse.
##
## -closeonleave TCL_BOOLEAN            DEFAULT: 1
##    Added by Kish Shen
##    The pulldown list will be exited without making a selection if cursor
##    leaves the list.
##
## -command script			DEFAULT: {}
##	Script to evaluate when a selection is made.
##
## -editable TCL_BOOLEAN		DEFAULT: 1
##	Whether to allow the user to edit the entry widget contents
##
## -grab type				DEFAULT: local
##	Type of grab (local, none, global) to use when listbox appears.
##
## -labelanchor anchor			DEFAULT: c
##	Anchor for the label.  Reasonable values are c, w and e.
##
## -labeltext string			DEFAULT: {}
##	Text for the label
##
## -labelwidth #			DEFAULT: 0 (self-sizing)
##	Width for the label
##
## -list list				DEFAULT: {}
##	List for the listbox
##
## -listheight #			DEFAULT: 5
##	Height of the listbox.  If the number of items exceeds this
##	height, a scrollbar will automatically be added. 0 means auto-size
##
## -postcommand script			DEFAULT: {}
##	A command which is evaluated before the listbox pops up.
##
## -prunelist TCL_BOOLEAN		DEFAULT: 0
##	Whether to prevent duplicate listbox items
##
## -state normal|disabled		DEFAULT: normal
##	Same as for entry, but also disables the button
##
## -tabexpand TCL_BOOLEAN		DEFAULT: 1
##	Whether to allow tab expansion in entry widget (uses listbox items)
##
## RETURNS: the window pathname
##
## BINDINGS (in addition to default widget bindings)
##
## <Double-1> or <Escape> in the entry widget, or selecting the
## button will toggle the listbox portion.
## 
## <Escape> will close the listbox without a selection.
## 
## <Tab> in the entry widget searches the listbox for a unique match.
## 
## <(Double-)1> in the listbox selects that item, configurable with -click.
##
## METHODS
##	These are the methods that the Combobox recognizes.  Aside from
##	those listed here, it accepts what is valid for entry widgets.
##
## configure ?option? ?value option value ...?
## cget option
##	Standard tk widget routines.
##
## add ?string?
##	Adds the string to the listbox.
##	If string is not specified, it uses what's in the entry widget.
##
## expand ?string?
##	Expands the string based on the contents of the listbox.
##	If string is not specified, it uses what's in the entry widget.
##
## popdown
##	Pops the listbox down (no error when already unmapped)
##
## popup
##	Pops the listbox up (no error when already mapped)
##
## toggle
##	Toggles whether the listbox is mapped or not.
##
## set string
##	Sets the entry widget (or its textvariable, if it exists) to
##	the value of string.
##
## subwidget widget
##	Returns the true widget path of the specified widget.  Valid
##	widgets are label, listbox, entry, toplevel, scrollbar.
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
## pack [combobox .combo -label "Hello: "]
## pack [combobox .combo -width 15 -textvariable myvar]
##
##------------------------------------------------------------------------


# Create this to make sure there are registered in auto_mkindex
# these must come before the [widget create ...]
proc Combobox args {}
proc combobox args {}
widget create Combobox -type frame -base entry -components {
    label
    {button button button {-image ::Widget::Combobox::Image \
	    -command [namespace code [list _toggle $w]]}}
    {toplevel toplevel drop {-cursor arrow}}
    {listbox listbox drop.lbox {-selectmode single \
	    -width 5 -height $data(-listheight) \
	    -yscrollcommand [list $data(scrollbar) set]}}
    {scrollbar scrollbar drop.sy {-orient vertical \
	    -command [list $data(listbox) yview]}}
} -options {
    {-bd		-borderwidth}
    {-borderwidth	borderWidth	BorderWidth	0}
    {-bg		-background}
    {-background	ALIAS entry -background}
    {-click		click		Click		double}
    {-closeonleave      closeOnLeave    CloseOnLeave    1}
    {-command		command	Command		{}}
    {-editable		editable	Editable	1}
    {-grab		grab		Grab		local}
    {-labeltext		labelText	Text		{}}
    {-labelwidth	labelWidth	Width		0}
    {-labelanchor	ALIAS label -anchor labelAnchor Anchor}
    {-list		list		List		{}}
    {-listheight	listHeight	ListHeight	5}
    {-postcommand	postCommand	Command		{}}
    {-prunelist		pruneList	PruneList	0}
    {-relief		relief		Relief		flat}
    {-state		ALIAS entry -state}
    {-tabexpand		tabExpand	TabExpand	1}
}

namespace eval ::Widget::Combobox {;

namespace import -force ::Utility::best_match

;proc construct {w} {
    variable $w
    upvar 0 $w data

    ## Removable List Box
    wm overrideredirect $data(toplevel) 1
    wm transient $data(toplevel) [winfo toplevel $w]
    wm group $data(toplevel) [winfo toplevel $w]
    ## this shouldn't be necessary... (bug on Windows?)
    wm withdraw $data(toplevel)

    bind $data(toplevel) <Unmap> [list catch [list grab release $w]]

    grid $data(label) $data(entry) $data(button) -in $w -sticky news
    grid configure $data(button) -sticky ns
    grid columnconfig $w 1 -weight 1
    grid $data(listbox) $data(scrollbar) -in $data(toplevel) -sticky ns
    grid configure $data(listbox) -sticky news
    grid remove $data(scrollbar) $data(label)
    grid columnconfig $data(toplevel) 0 -weight 1
    grid rowconfigure $data(toplevel) 0 -weight 1

    ## These are not in a class (like ComboboxList) because we need to
    ## allow -click to work on an instance basis.  For the same reason,
    ## we can't use any virtual events.
    bind $data(listbox) <Escape>   [namespace code [list $w popdown]]
    bind $data(listbox) <Double-1> \
	    [namespace code "get [list $w] \[%W get \[%W nearest %y\]\]"]
    if {$data(-closeonleave)} {
	bind $data(toplevel) <Leave> [namespace code [list _kill_if_same %W $data(toplevel) $w]]
    }
    bind $data(listbox) <Return> \
	    [namespace code "get [list $w] \[%W get active\]"]
}

;proc _kill_if_same {w1 w2 w} {
    if [string match $w1 $w2] {
	_popdown $w
#	[namespace code [list $w popdown]]
    }
}

;proc configure { w args } {
    variable $w
    upvar 0 $w data

    set truth {^(1|yes|true|on)$}
    foreach {key val} $args {
	switch -- $key {
	    -borderwidth - -relief { .$w configure $key $val }
	    -background	{
		$data(basecmd) configure -bg $val
		$data(listbox) configure -bg $val
	    }
	    -click	{
		switch [string tolower $val] {
		    single	{
			bind $data(listbox) <Double-1> {}
			bind $data(listbox) <1> [namespace code "get \
				[list $w] \[%W get \[%W nearest %y\]\]"]
			bind $data(listbox) <Motion> {
			    %W selection clear 0 end
			    %W selection set [%W nearest %y]
			}
		    }
		    double	{
			bind $data(listbox) <Double-1> [namespace code "get \
				[list $w] \[%W get \[%W nearest %y\]\]"]
			bind $data(listbox) <1> {}
			bind $data(listbox) <Motion> {}
		    }
		    default	{
			return -code error "bad $key option \"$val\": must be\
				single or double"
		    }
		}
	    }
	    ;# added by Kish Shen, 99-3-12
	    -closeonleave  {
		if {![set val [regexp $truth $val]]} {
		    bind $data(toplevel) <Leave> {break}
		}
	    }
	    -editable	{
		if {[set val [regexp $truth $val]]} {
		    $data(basecmd) configure -state normal
		} else {
		    ;# Kish Shen, 2002-11-18: prob. with backspace in disabled state for Tcl 8.4
                    if {[info tclversion] > 8.3} {
			;# use new readonly state instead
			$data(basecmd) configure -state readonly
		    } else {
			$data(basecmd) configure -state disabled
		    }
		}
	    }
	    -grab	{
		if {![regexp {^(local|global|none)$} $val junk val]} {
		    return -code error "bad $key option \"$val\": must be\
			    local, grab, or none"
		}
	    }
	    -list	{
		$data(listbox) delete 0 end
		eval $data(listbox) insert end $val
	    }
	    -labelanchor { $data(label) configure -anchor $val }
	    -labeltext	{
		$data(label) configure -text $val
		if {[string compare {} $val]} {
		    grid $data(label)
		} else {
		    grid remove $data(label)
		}
	    }
	    -labelwidth	{ $data(label) configure -width $val }
	    -listheight	{ $data(listbox) configure -height $val }
	    -state	{
		$data(basecmd) configure -state $val
		$data(button) configure -state $val
		if {[string match "disabled" $val] && \
			[winfo ismapped $data(toplevel)]} {
		    wm withdraw $data(toplevel)
		    catch {grab release $w}
		}
	    }
	    -prunelist	-
	    -tabexpand	{ set val [regexp $truth $val] }
	}
	set data($key) $val
    }
}

#bind Combobox <Double-1>	{ %W toggle }
bind Combobox <Escape>		{ %W toggle }
bind Combobox <Tab>		{ %W expand [%W get]; break }
bind Combobox <Unmap>		{ catch {grab release %W} }
bind Combobox <Destroy>		{ catch {grab release %W} }

;proc _toggle {w} {
    variable $w
    upvar 0 $w data

    if {[winfo ismapped $data(toplevel)]} {
	_popdown $w
    } else {
	_popup $w
    }
}

;proc _popup {w} {
    variable $w
    upvar 0 $w data

    if {[winfo ismapped $data(toplevel)]} {
	return
    }
    global tcl_platform
    uplevel \#0 $data(-postcommand)
    focus $data(entry)
    set size [$data(listbox) size]
    ## If -listheight is 0, the listbox will auto-size
    if {$data(-listheight) && ($size > $data(-listheight))} {
	$data(listbox) configure -height $data(-listheight)
	grid $data(scrollbar)
    } else {
	$data(listbox) configure -height $size
	grid remove $data(scrollbar)
    }
    ## The update is required to get the window to properly size itself
    ## before it is popped up the first time.
    update idletasks
    set W [expr {[winfo width $data(entry)]+[winfo width $data(button)]}]
    set H [winfo reqheight $data(toplevel)]
    set y [expr {[winfo rooty $data(entry)]+[winfo height $data(entry)]}]
    ## Make it pop up upwards if there is not enough screen downwards
    if {($y+$H)>[winfo screenheight $w]} {
	set y [expr {[winfo rooty $data(entry)]-$H}]
    }
    set x [winfo rootx $data(entry)]
    wm geometry $data(toplevel) ${W}x${H}+${x}+${y}
    ## This is required to get the window to pop up in the right place
    ## on Windows, doesn't affect Unix
    update idletasks
    wm deiconify $data(toplevel)
    if {[string match local $data(-grab)]} {
	grab $w
    } elseif {[string match global $data(-grab)]} {
	grab -global $w
    }
    raise $data(toplevel)
    focus $data(listbox)
}

;proc _popdown {w} {
    variable $w
    upvar 0 $w data

    if {![winfo ismapped $data(toplevel)]} {
	return
    }
    wm withdraw $data(toplevel)
    catch {grab release $w}
    focus $data(entry)
}

;proc _expand {w {str {}}} {
    variable $w
    upvar 0 $w data

    if {!$data(-tabexpand)} return
    if {[string match {} $str]} { set str [$data(basecmd) get] }
    set found 0
    foreach item [$data(listbox) get 0 end] {
	if {[string match ${str}* $item]} {
	    incr found
	    lappend match $item
	}
    }
    if {$found} {
	set state [$data(basecmd) cget -state]
	$data(basecmd) config -state normal
	$data(basecmd) delete 0 end
	if {$found>1} {
	    set match [best_match $match $str]
	} else {
	    set match [lindex $match 0]
	}
	$data(basecmd) insert end $match
	$data(basecmd) config -state $state
    } else { bell }
}

;proc _add {w {str {}}} {
    variable $w
    upvar 0 $w data

    if {[string match {} $str]} { set str [$data(basecmd) get] }
    set i 1
    if {!$data(-prunelist)} {
	foreach l [$data(listbox) get 0 end] {
	    if {![string compare $l $str]} { set i 0 ; break }
	}
    }
    if {$i} { $data(listbox) insert end $str }
}

;proc _set {w str} {
    variable $w
    upvar 0 $w data

    set var [$data(basecmd) cget -textvar]
    if {[string compare {} $var] && [uplevel \#0 info exists [list $var]]} {
	global $var
	set $var $str
    } else {
	set state [$data(basecmd) cget -state]
	$data(basecmd) config -state normal
	$data(basecmd) delete 0 end
	$data(basecmd) insert 0 $str
	$data(basecmd) config -state $state
    }
}

;proc get {w i} {
    variable $w
    upvar 0 $w data

    set e $data(basecmd)
    if {[$data(listbox) size]} {
	set state [$e cget -state]
	$e config -state normal
	$e delete 0 end
	$e insert end $i
	$e config -state $state
	if {[string compare $data(-command) {}]} {
	    uplevel \#0 [list $data(-command) $i]
	}
    }
    wm withdraw $data(toplevel)
    focus $data(base)
}

}; # end namespace ::Widget::Combobox

## Button Bitmap
##
image create bitmap ::Widget::Combobox::Image -data {#define downbut_width 14
#define downbut_height 14
static char downbut_bits[] = {
    0x00, 0x00, 0xe0, 0x01, 0xe0, 0x01, 0xe0, 0x01, 0xe0, 0x01, 0xfc, 0x0f,
    0xf8, 0x07, 0xf0, 0x03, 0xe0, 0x01, 0xc0, 0x00, 0x00, 0x00, 0xfe, 0x1f,
    0xfe, 0x1f, 0x00, 0x00};
}

return
