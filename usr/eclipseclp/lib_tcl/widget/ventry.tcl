## self-validating entry widget
##
## Copyright 1997-8 Jeffrey Hobbs, jeff.hobbs@acm.org, CADIX International
##
package require Widget 2.0
package provide Ventry 2.0

##------------------------------------------------------------------------
## PROCEDURE
##	ventry
##
## DESCRIPTION
##	Implements a self-validating entry widget
##
## ARGUMENTS
##	ventry <widget> ?options?
##
## OPTIONS
##
## -invalidcmd
##
##
## -validatecmd (-vcmd)
##
##
## -validate
##
##
## SUBSTITUTIONS
##
## %d	the type of validation (delete, insert, ...)
## %i	the index of the insert or delete
## %P	the potential new string value
## %s	the current value of the entry
## %S	the chars to be inserted or deleted
## %v	the value of -validate
## %W	the widget name
##
## METHODS
##
## validate
##
## BINDINGS
##	This works entirely off the textvariable, so no extra bindings
##	are required.
##
## NAMESPACE & STATE
##	The megawidget creates a global array with the classname, and a
## global array which is the name of each megawidget is created.  The latter
## array is deleted when the megawidget is destroyed.
##	Public procs of $CLASSNAME and [string tolower $CLASSNAME] are used.
## Other procs that begin with $CLASSNAME are private.  For each widget,
## commands named .$widgetname and $CLASSNAME$widgetname are created.
##
##------------------------------------------------------------------------
## EXAMPLES:
## # A number only entry widget
## ventry .v -vcmd {regexp {^[-+]?[0-9]*$} %P} -validate key -invalidcmd bell
## # An entry widget limited to 8 chars
## ventry .l -vcmd {expr {[string length %P]<=8}} -validate key
##

# Create this to make sure there are registered in auto_mkindex
# these must come before the [widget create ...]
proc Ventry args {}
proc ventry args {}
widget create Ventry -type frame -base entry -components {
    label
} -options {
    {-bd		-borderwidth}
    {-borderwidth	borderWidth	BorderWidth	0}
    {-invalidcmd	invalidCmd	InvalidCmd	bell}
    {-labeltext		labelText	LabelText	{}}
    {-labelwidth	labelWidth	Width		0}
    {-labelanchor	ALIAS label -anchor labelAnchor Anchor}
    {-labelfont		ALIAS label -font labelFont Font}
    {-labelforeground	ALIAS label -foreground labelForeground Foreground}
    {-relief		relief		Relief		flat}
    {-validatecmd	-vcmd}
    {-vcmd		validateCmd	ValidateCmd	{}}
    {-validate		validate	Validate	none}
    {-textvariable	textVariable	TextVariable	{}}
}

namespace eval ::Widget::Ventry {;

;proc construct {w} {
    upvar \#0 [namespace current]::$w data

    set data(flags) {}

    grid $data(label) $data(entry) -in $w -sticky ns
    grid configure $data(entry) -sticky news
    grid columnconfig $w 1 -weight 1
    grid rowconfig $w 0 -weight 1
    grid remove $data(label)

    bind $data(entry) <FocusIn>  [namespace code [list focus $w in]]
    bind $data(entry) <FocusOut> [namespace code [list focus $w out]]
}

;proc configure {w args} {
    upvar \#0 [namespace current]::$w data
    set truth {^(1|yes|true|on)$}
    foreach {key val} $args {
	switch -- $key {
	    -borderwidth - -relief { .$w configure $key $val }
	    -labelanchor	{ $data(label) configure -anchor $val }
	    -labelfont		{ $data(label) configure -font $val }
	    -labelforeground	{ $data(label) configure -foreground $val }
	    -labeltext		{
		$data(label) configure -text $val
		if {[string compare {} $val]} {
		    grid $data(label)
		} else {
		    grid remove $data(label)
		}
	    }
	    -labelwidth		{ $data(label) configure -width $val }
	    -validate {
		if {![regexp {^(focus|focusin|focusout|all|none|key)$} $val]} {
		    return -code error "Invalid validation type \"$val\""
		}
	    }
	    -textvariable	{ $data(basecmd) configure -textvariable $val }
	}
	set data($key) $val
    }
}

;proc _insert {w index string} {
    upvar \#0 [namespace current]::$w data

    if {[regexp {^(all|key)$} $data(-validate)]} {
	set index [$data(basecmd) index $index]
	set cur [$data(basecmd) get]
	set new [string range $cur 0 [expr $index-1]]$string[string range $cur $index end]
	if {[catch {validate $w $string $new $index insert} err]} {
	    return
	}
    }
    return [uplevel [list $data(basecmd) insert $index $string]]
}

;proc _delete {w first {last {}}} {
    upvar \#0 [namespace current]::$w data

    if {[regexp {^(all|key)$} $data(-validate)]} {
	set first [$data(basecmd) index $first]
	if {[string match {} $last]} {
	    set last [expr $first+1]
	} else {
	    set last [$data(basecmd) index $last]
	}
	set cur [$data(basecmd) get]
	set new [string range $cur 0 [expr $first-1]][string range $cur $last end]
	if {[catch {validate $w [string range $cur $first \
		[expr $last-1]] $new $first delete} err]} {
	    return
	}
    }
    return [uplevel [list $data(basecmd) delete $first] $last]
}

;proc _validate {w} {
    upvar \#0 [namespace current]::$w data

    set old $data(-validate)
    set data(-validate) all
    set code [catch {validate $w {} [$data(basecmd) get] \
	    [$data(basecmd) index insert] validate} err]
    set data(-validate) $old
    return [expr {$code?0:1}]
}

;proc focus {w which} {
    upvar \#0 [namespace current]::$w data

    if {[regexp "^(all|focus($which)?)\$" $data(-validate)]} {
	catch {validate $w {} [$data(basecmd) get] \
		[$data(basecmd) index insert] focus$which}
    }
}

;proc validate {w str new index type} {
    upvar \#0 [namespace current]::$w data

    if {[string match {} $data(-vcmd)] || \
	    [string match none $data(-validate)] || \
	    [string match VALIDATING $data(flags)]} {
	return
    }
    set data(flags) VALIDATING

    set cmd [substitute $w $data(-vcmd) $str $new $index $type]

    set code [catch {uplevel \#0 $cmd} result]
    if {$code != 0 && $code != 2} {
	global errorInfo
	append errorInfo "\n\t(in $w validation command)"
	bgerror $result
	set code 1
    } else {
	set val [regexp {^(1|yes|true|on)$} $result]
	if $val { set code 0 } else { set code 3 }
	set result {}
    }

    # If e->validate has become VALIDATE_NONE during the validation,
    # it means that a loop condition almost occured.  Do not allow
    # this validation result to finish.
    if {[string match none $data(-validate)] || \
	    [string match VALIDATE_VAR $data(flags)]} {
	set code 1
    }
    # If validate will return ERROR, then disallow further validations
    # Otherwise, if it didn't accept the new string (returned TCL_BREAK)
    # then eval the invalidCmd (if it's set)
    if {$code} {
	if {$code == 3} {
	    ## TCL_BREAK
	    if {[string compare {} $data(-invalidcmd)]} {
		set cmd [substitute $w $data(-invalidcmd) \
			$str $new $index $type]
		if {[catch {uplevel \#0 $cmd} result]} {
		    global errorInfo
		    append errorInfo "\n\t(in $w validation command)"
		    bgerror $result
		    set code 1
		    set data(-validate) none
		}
	    }
	} else {
	    set data(-validate) none
	}
    }
    set data(flags) {}
    return -code $code $result
}

;proc substitute {w cmd change newstr index type} {
    upvar \#0 [namespace current]::$w data

    set old $cmd
    set i [string first % $cmd]
    if {$i < 0} { return $old }
    set new [string range $cmd 0 [incr i -1]]
    while 1 {
	set c [string index $cmd [incr i 2]]
	switch $c {
	    d		{ append new $type }
	    i		{ append new $index }
	    P		{ append new [list $newstr] }
	    s		{ append new [list [$data(basecmd) get]] }
	    S		{ append new [list $change] }
	    v		{ append new $data(-validate) }
	    W		{ append new [list $w] }
	    {}		{ append new %; return $new }
	    default	{ append new [list $c] }
	}
	set cmd [string range $cmd [incr i] end]
	set i [string first % $cmd]
	if {$i < 0} { return $new$cmd }
	append new [string range $cmd 0 [incr i -1]]
    }
}

}; #end namespace ::Widget::Ventry