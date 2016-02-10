##
## megalist.tcl
##
## Copyright 1997-8 Jeffrey Hobbs
##
package require Widget 2.0
package provide Megalist 1.0

##------------------------------------------------------------------------
## PROCEDURE
##	megalist
##
## ARGUMENTS && DESCRIPTION
##
## megalist  <window pathname> <options>
##        Implements a megalist which displays a sorted and filtered 
## list of lists.
##
## OPTIONS
## 
## -sortby item                                 DEFAULT: none
##      Specifies which item to sort on.
##
## -sort TCL_BOOLEAN                            DEFAULT: 1
##      If true the sort buttons appear and the lists are sorted 
##      by the item specified by -sortby. If false the sort buttons
##      disappear and the lists are not sorted.
##
##
## -showfilters TCL_BOOLEAN                     DEFAULT: 0 
##
## -shownames TCL_BOOLEAN                       DEFAULT: 1 
##
## METHODS
##	These are the methods that the megalist object recognizes.
##	(ie - megalist .m ; .m <method> <args>)
##	Any unique substring is acceptable
##
## load list
##      Each element in the list is displayed as a row in the widget.
##      Each element in the row is assigned to an item starting from the left.
##      If there are less item elements than items blanks are assigned.
##      If there are more item elements than items they are ignored.    
##
## add item ?args?
##      Adds a display for the new item in the list. 
##
## delete item ?item ...?
##      Deletes the item(s) and removes the display(s).
##
## itemconfigure item ?option? ?value option value ...?
##      Query or modify the configuration options of the item.
##
## itemcget item option
##      Returns the current value of the item's configuration option.
##      
## names ?pattern?
##      Returns the item names that match pattern. Defaults to *.
##
## BINDINGS
##
## NAMESPACE & STATE
##	The megawidget creates a global array with the classname, and a
## global array which is the name of each megawidget is created.  The latter
## array is deleted when the megawidget is destroyed.
##	Public procs of $CLASSNAME and [string tolower $CLASSNAME] are used.
## Other procs that begin with $CLASSNAME are private.  For each widget,
## commands named .$widgetname and $CLASSNAME$widgetname are created.
##
##
##------------------------------------------------------------------------
##
##

# Create this to make sure there are registered in auto_mkindex
# these must come before the [widget create ...]
proc Megalist args {}
proc megalist args {}

widget create Megalist -type frame -base frame -components {
    {frame hold hold {-height 200 -width 200 -bg pink}}
    {scrollbar yscrollbar sy {-bd 1 -takefocus 0 -highlightthickness 0 \
	    -orient v -command [namespace code [list lbset $w yview]]}}
} -options {
    {-sortby		sortby		SortBy		{}}
    {-sortcmd		sortcmd		SortCmd	::Widget::Megalist::sort}  
    {-shownames		shownames	ShowNames	1}
    {-showfilters	showfilters	ShowFilters	0}
    {-style		style		Style		listbox}
    {-font		font		Font		fixed}
    {-selectproc	selectProc	SelectProc	{}}
    {-dataproc		dataProc	DataProc	{}}
    {-filtercmd		filtercmd	FilterCmd ::Widget::Megalist::filter}
}

namespace eval ::Widget::Megalist {;

;proc construct w {
    upvar \#0 [namespace current]::$w data

    ## Private variables
    array set data [list \
	order	{} \
	lists	{} \
	count	0  \
	height	0  \
	reload	{} \
	data	{} \
	]

    grid $data(hold) $data(yscrollbar) -sticky news
    grid config $data(yscrollbar) -sticky ns
    grid columnconfig $w 0 -weight 1
    grid rowconfig $w 0 -weight 1

}

;proc init w {
}

;proc destruct w {
    upvar \#0 [namespace current]::$w data
    catch {pane forget $data(hold)}
}

;proc configure {w args} {
    upvar \#0 [namespace current]::$w data

    set truth {^(1|yes|true|on)$}
    set reload 0
    foreach {key val} $args {
	switch -- $key {
	    -sortby {
		if {[llength $val]>1} {
		    return -code error "multiple sort fields not supported"
		}
		if {![string compare $val $data(-sortby)]} continue
		foreach v $val {
		    if {[lsearch $data(order) $v] == -1} {
			return -code error "unrecognized field \"$name\""
		    }
		}
		if {[info exists set($v)]} {
		    return -code error "field \"$name\" set twice"
		}
		set set($v) 0
		set reload 1
	    }

	    -showfilters {
		set val [regexp -nocase $truth $val]
		if {$data(-showfilters) == $val} continue
		if $data(-showfilters) {
		    foreach name $data(order) {
			pack forget $data(hold).$name.e
		    }
		} else {
		    foreach name $data(order) {
			pack $data(hold).$name.e -fill x \
				-before $data(hold).$name.c
		    } 
		}
	    }
	    -shownames {
		set val [regexp -nocase $truth $val]
		if { $data(-shownames) == $val } {continue}
		if $data(-shownames) {
		    foreach name $data(order) {
			pack forget $data(hold).$name.b
		    }
		} else {
		    set x c
		    if [winfo exists $data(hold).$name.e] {
			set x e
		    }
		    foreach name $data(order) {
			pack $data(hold).$name.b -fill x \
				-before $data(hold).$name.$x
		    } 
		}
	    }
	}
	set data($key) $val
    }
    if {$reload} {_refresh $w}
}

;proc _load {w args} {
    upvar \#0 [namespace current]::$w data

    if {[string match {} $data(order)]} {
	return -code error "no fields in megalist"
    }
    set data(data) $args

    catch {$data(err) load $args} result
    _refresh $w

    return $result
}

#refresh or reload?
;proc _refresh {w} {
    upvar \#0 [namespace current]::$w data
    after cancel $data(reload)
    set data(reload) [after idle load $w]
    return
}

;proc load {w} {
    upvar \#0 [namespace current]::$w data

    if {[string match {} $data(data)]} return
  
    foreach name $data(order) {
	set box $data(hold).$name.c
	$box delete 0 end
	eval $box insert end [$data(err) fldmsg $name]
    }
}

;proc setsort {w b name} {
    upvar \#0 [namespace current]::$w data
    array set config $data(I:$name)
    if {[string compare $name $data(-sortby)]} {
	set data(-sortby) $name
	set config(-order) increasing
    } else {
	if {[string compare increasing $config(-order)]} {
	    set config(-order) increasing
	} else {
	    set config(-order) decreasing
	}
    }
    set data(I:$name) [array get config]
    _refresh $w
}

;proc _add {w name args} {
    upvar \#0 [namespace current]::$w data

    if {[info exists data(I:$name)]} {
	# Ensure name doesn't already exist
	return -code error "field \"$name\" already exists"
    }
    if {[regexp {(^[\.A-Z]|[ \.])} $name]} {
	return -code error "invalid item name \"$name\": it cannot begin\
		with a capital letter, or contain spaces or \".\""
    }
    if {[llength $args]&1} {
	return -code error "wrong \# of args to add method \"$args\""
    }

    get_opts2 config $args {
	-filtertype	match
	-match		*
	-sort		ascii
    }
    set data(I:$name) [array get config]
    set data(IF:$name) $config(-match)

    lappend data(order) $name

    if {[catch {additem $w} result]} {
	set data(order) [lreplace $data(order) end end] 
	unset data(I:$name) data(IF:$name)
	return -code error $result
    }

    add $w $name

    return $name
}

;proc additem {w args} {
    upvar \#0 [namespace current]::$w data
    
    foreach name $data(order) {
	array set config $data(I:$name)
	set field [list $name -sort $config(-sort)]
	lappend fields $field
	unset config
    }
}

;proc add {w name} {
    upvar \#0 [namespace current]::$w data

    array set config $data(I:$name)
    set f [frame $data(hold).$name]
    button $f.b -text $name -bd 1 -highlightthickness 0 \
	    -takefocus 0 -padx 6 -pady 2 \
	    -command [namespace code [list setsort $w $f.b $name]]
    entry $f.e -textvariable ${w}(IF:$name) -bd 1 \
	    -highlightthickness 0 -takefocus 0 -justify center
    set box [listbox $f.c -highlightthickness 0 -bd 0 -takefocus 0 \
	    -yscrollcommand [namespace code [list scroll $w]] -exportsel 0]
    $f.c xview moveto 0
    if $data(-shownames) { pack $f.b -fill x}
    if $data(-showfilters) {pack $f.e -fill x}
    pack $f.c -fill both -expand 1
    pane $f -parent $data(hold) -handlelook {-bd 1 -width 2}

    bind $f.c <ButtonRelease-1> [namespace code [list select $w $f.c]]
    bind $f.e <Return> [namespace code [list _refresh $w]]

    set $data(IF:$name) $config(-match)
}

;proc select {w p} {
    upvar \#0 [namespace current]::$w data
    if [string match {} [set idx [$p curselection]]] {return}
    foreach i $data(order) {
	$w.hold.$i.c selection clear 0 end
	$w.hold.$i.c selection set $idx
    }
    if {[string compare {} $data(-selectproc)]} {
	foreach i $data(order) {
	    lappend select [$w.hold.$i.c get $idx]
	}
	## No $select here!
	if {[string compare {} $select]} {
	    eval $data(-selectproc) $w $select
	}
    }
}

;proc _delete {w args} {
    upvar \#0 [namespace current]::$w data

    foreach name $args {
	## Don't complain about unknown items when deleting
	set wid $data(hold).$name
	catch {
	    unset data(I:$name) data(IF:$name)
	    pane forget $data(hold) $wid
	    destroy $wid
	}
	if {[set i [lsearch -exact $data(order) $name]] != -1} {
	    set data(order) [lreplace $data(order) $i $i]
	}
	if {[set i [lsearch -exact $data(-sortby) $name]] != -1} {
	    set data(-sortby) [lreplace $data(-sortby) $i $i]
	}
    }
}

## _itemconfigure
## configure a progressbar constituent item
##
;proc _itemconfigure {w name args} {
    upvar \#0 [namespace current]::$w data

    if {![info exists data(I:$name)]} {
	return -code error "unknown field \"$name\""
    }

    array set config $data(I:$name)
    if {[catch {$data(err) field $name $args} result]} {
	$data(err) field $name [array get config]
	return -code error $result
    } 

    if {[llength $args] > 1} {
	array set config $args
	set data(IF:$name) $config(match)
	set data(I:$name) $args
	_refresh $w
    }
    return $result
}

## _itemcget
## Returns a single item option
##
;proc _itemcget {w name opt} {
    upvar \#0 [namespace current]::$w data

    if {![info exists data(I:$name)]} {
	return -code error "unknown item \"$name\""
    }
    array set config $data(I:$name)
    ## Ensure that we are getting a -'ed value
    if {![info exists config(-[string range $opt 1 end])]} {
	return -code error "unknown option \"$opt\""
    }
    return $config($opt)
}

## _names
## Return a list of item names
##
;proc _names {w {pattern *}} {
    upvar \#0 [namespace current]::$w data

    set names {}
    foreach name $data(order) {
	if {[string match $pattern $name]} {
	    lappend names $name
	}
    }
    return $names
}

;proc lbset {w args} {
    upvar \#0 [namespace current]::$w data

    foreach name $data(order) {
	eval [list $data(hold).$name.c] $args
    }
}

;proc scroll {w args} {
    upvar \#0 [namespace current]::$w data

    eval $data(yscrollbar) set $args
    lbset $w yview moveto [lindex $args 0]
}

}; # end namespace ::Widget::Megalist
