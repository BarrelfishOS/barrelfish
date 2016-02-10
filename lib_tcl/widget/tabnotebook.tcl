##
## Copyright 1997-8 Jeffrey Hobbs, jeff.hobbs@acm.org, CADIX International
##
package require Widget 2.0
package provide Tabnotebook 2.0

## FIX:
## option state of subitems could be kept in a clearer array

##------------------------------------------------------------------------
## PROCEDURE
##	tabnotebook
##
## DESCRIPTION
##	Implements a Tabbed Notebook megawidget
##
## ARGUMENTS
##	tabnote <window pathname> <options>
##
## OPTIONS
##	(Any entry widget option may be used in addition to these)
##
## -activebackground color		DEFAULT: {}
##	The background color given to the active tab.  A value of {}
##	means these items will pick up the widget's background color.
##
## -background color			DEFAULT: DEFAULT
##	The background color for the container subwidgets.
##
## -browsecmd script			DEFAULT: {}
##	A script that is evaluated each time a tab changes.  It appends
##	the old tab and the new tab to the script.  An empty string ({})
##	represents the blank (empty) tab.  This is eval'ed before the
##	tab actually changes, allowing tab transitions to be aborted by
##	returning an error value in this script.
##
## -disabledbackground color		DEFAULT: #c0c0c0 (dark gray)
##	The background color given to disabled tabs.
##
## -font				DEFAULT: {Helvetica -12}
##	The font for the tab text.  All tabs use the same font.
##
## -justify justification		DEFAULT: center
##	The justification applied to the text in multi-line tabs.
##	Must be one of: left, right, center.
##
## -linewidth pixels			DEFAULT: 2
##	The width of the line surrounding the tabs.  Must be at least 1.
##
## -linecolor color			DEFAULT: black
##	The color of the line surrounding the tabs.
##
## -normalbackground			DEFAULT: {}
##	The background color of items with normal state.  A value of {}
##	means these items will pick up the widget's background color.
##
## -padx pixels				DEFAULT: 6
##	The X padding for folder tabs around the items.
##
## -pady pixels				DEFAULT: 4
##	The Y padding for folder tabs around the items.
##
## RETURNS: the window pathname
##
## BINDINGS (in addition to default widget bindings)
##
## <1> in a tabs activates that tab.
##
## METHODS
##	These are the methods that the Tabnote widget recognizes.  Aside from
##	these, it accepts methods that are valid for entry widgets.
##
## activate id
##	Activates the tab specified by id.  id may either by the unique id
##	returned by the add command or the string used in the add command.
##
## add string ?options?
##	Adds a tab to the tab notebook with the specified string, unless
##	the string is the name of an image, in which case the image is used.
##	Each string must be unique.  See ITEM OPTIONS for the options.
##	A unique tab id is returned.
##
## delete id
##	Deletes the tab specified by id.  id may either by the unique id
##	returned by the add command or the string used in the add command.
##
## itemconfigure ?option? ?value option value ...?
## itemcget option
##	Configure or retrieve the option of a tab notebook item.
##
## name tabId
##	Returns the text name for a given tabId.
##
## subwidget widget
##	Returns the true widget path of the specified widget.  Valid
##	widgets are hold (a frame), tabs (a canvas), blank (a frame).
##
## ITEM OPTIONS
##	These are options for the items (tabs) of the notebook
##
## -window widget				DEFAULT: {}
##	Specifies the widget to show when the tab is pressed.  It must be
##	a child of the tab notebook (required for grid management) and exist
##	prior to this command.
##
## -state normal|disabled|active		DEFAULT: normal
##	The optional state can be normal, active or disabled.
##	If active, then this tab becomes the active (displayed) tab.
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
## pack [tabnotebook .t] -fill both -expand 1
## text .t.t -height 10 -width 20
## .t add "Text Widget" -window .t.t
##------------------------------------------------------------------------

# Create this to make sure there are registered in auto_mkindex
# these must come before the [widget create ...]
proc Tabnotebook args {}
proc tabnotebook args {}

widget create Tabnotebook -type frame -base frame -components {
    {frame hold hold {-relief raised -bd 1}}
    {frame blank}
    {frame hide hide {-background $data(-background) -height 1 -width 40}}
    {canvas tabs tabs {-bg $data(-background) -highlightthick 0 -takefocus 0}}
} -options {
    {-activebackground	activeBackground ActiveBackground {}}
    {-bg		-background}
    {-background	ALIAS frame -background}
    {-bd		-borderwidth}
    {-borderwidth	ALIAS frame -borderwidth}
    {-browsecmd		browseCmd	BrowseCommand	{}}
    {-disabledbackground	disabledBackground DisabledBackground #a3a3a3}
    {-normalbackground	normalBackground normalBackground #c3c3c3}
    {-font		font		Font		{Helvetica -12}}
    {-justify		justify		Justify		center}
    {-minwidth		minWidth	Width		-1}
    {-minheight		minHeight	Height		-1}
    {-padx		padX		PadX		6}
    {-pady		padY		PadY		4}
    {-relief		ALIAS frame -relief}
    {-linewidth		lineWidth	LineWidth	1}
    {-linecolor		lineColor	LineColor	black}
}

namespace eval ::Widget::Tabnotebook {;

;proc construct {w} {
    upvar \#0 [namespace current]::$w data

    ## Private variables
    array set data {
	curtab	{}
	numtabs	0
	width	0
	height	0
	ids	{}
    }

    $data(tabs) itemconfigure TEXT -font $data(-font)

    $data(tabs) yview moveto 0
    $data(tabs) xview moveto 0

    grid $data(tabs) -sticky ew
    grid $data(hold) -sticky news
    grid $data(blank) -in $data(hold) -row 0 -column 0 -sticky nsew
    grid columnconfig $w 0 -weight 1
    grid rowconfigure $w 1 -weight 1
    grid columnconfig $data(hold) 0 -weight 1
    grid rowconfigure $data(hold) 0 -weight 1

    bind $data(tabs) <Configure> [namespace code \
	    "if {!\[string compare $data(tabs) %W\]} { resize [list $w] %w }"]
    bind $data(tabs) <2>		{ %W scan mark %x 0 }
    bind $data(tabs) <B2-Motion>	[namespace code {
	%W scan dragto %x 0
	resize [winfo parent %W] [winfo width %W]
    }
    ]
}

;proc configure {w args} {
    upvar \#0 [namespace current]::$w data

    set truth {^(1|yes|true|on)$}
    set post {}
    foreach {key val} $args {
	switch -- $key {
	    -activebackground {
		if {[string compare $data(curtab) {}]} {
		    $data(tabs) itemconfig POLY:$data(curtab) -fill $val
		}
		if {[string compare $val {}]} {
		    $data(hide) config -bg $val
		} else {
		    lappend post \
			    [list $data(hide) config -bg $data(-background)]
		}
	    }
	    -background	{
		$data(tabs) config -bg $val
		$data(hold) config -bg $val
		$data(blank) config -bg $val
	    }
	    -borderwidth {
		$data(hold) config -bd $val
		$data(hide) config -height $val
	    }
	    -disabledbackground {
		foreach i $data(ids) {
		    if {[string match disabled $data(:$i:-state)]} {
			$data(tabs) itemconfig POLY:$i -fill $val
		    }
		}
	    }
	    -font	{
		$data(tabs) itemconfigure TEXT -font $val
		recalculate $w
	    }
	    -justify	{ $data(tabs) itemconfigure TEXT -justify $val }
	    -linewidth	{ $data(tabs) itemconfigure LINE -width $val }
	    -linecolor	{ $data(tabs) itemconfigure LINE -fill $val }
	    -minwidth	{
		if {$val < 0} { set val 0 }
		grid columnconfig $w 0 -minsize $val
	    }
	    -minheight	{
		if {$val < 0} { set val 0 }
		grid rowconfigure $w 1 -minsize $val
	    }
	    -normalbackground {
		foreach i $data(ids) {
		    if {[string match normal $data(:$i:-state)]} {
			$data(tabs) itemconfig POLY:$i -fill $val
		    }
		}
	    }
	    -padx - -pady {
		if {$val < 1} { set val 1 }
	    }
	    -relief	{
		$data(hold) config -relief $val
	    }
	}
	set data($key) $val
    }
    if {[string compare $post {}]} {
	eval [join $post \n]
    }
}

;proc _add { w text args } {
    upvar \#0 [namespace current]::$w data

    set c $data(tabs)
    if {[string match {} $text]} {
	return -code error "non-empty text required for noteboook label"
    } elseif {[string compare {} [$c find withtag ID:$text]]} {
	return -code error "tab \"$text\" already exists"
    }
    array set s {
	-window	{}
	-state	normal
    }
    foreach {key val} $args {
	switch -glob -- $key {
	    -w*	{
		if {[string compare $val {}]} {
		    if {![winfo exist $val]} {
			return -code error "window \"$val\" does not exist"
		    } elseif {[string comp $w [winfo parent $val]] && \
			    [string comp $data(hold) [winfo parent $val]]} {
			return -code error "window \"$val\" must be a\
				child of the tab notebook ($w)"
		    }
		}
		set s(-window) $val
	    }
	    -s* {
		if {![regexp {^(normal|disabled|active)$} $val]} {
		    return -code error "unknown state \"$val\", must be:\
			    normal, disabled or active"
		}
		set s(-state) $val
	    }
	    default {
		return -code error "unknown option '$key', must be:\
			[join [array names s] {, }]"
	    }
	}
    }
    set tab [incr data(numtabs)]
    set px [expr {int(ceil($data(-padx)/2))}]
    set py [expr {int(ceil($data(-pady)/2))}]
    if {[lsearch -exact [image names] $text] != -1} {
	set i [$c create image $px $py -image $text -anchor nw \
		-tags [list IMG M:$tab ID:$text TAB:$tab]]
    } else {
	set i [$c create text [expr {$px+1}] $py -text $text -anchor nw \
		-tags [list TEXT M:$tab ID:$text TAB:$tab] \
		-justify $data(-justify)]
    }
    foreach {x1 y1 x2 y2} [$c bbox $i] {
	set W  [expr {$x2-$x1+$px}]
	set FW [expr {$W+$px}]
	set FH [expr {$y2-$y1+3*$py}]
    }
    set diff [expr {$FH-$data(height)}]
    if {$diff > 0} {
	$c move all 0 $diff
	$c move $i 0 -$diff
	set data(height) $FH
    }
    $c create poly 0 $FH $px $py $W $py $FW $FH -fill {} \
	    -tags [list POLY POLY:$tab TAB:$tab]
    $c create line 0 $FH $px $py $W $py $FW $FH -joinstyle round \
	    -tags [list LINE LINE:$tab TAB:$tab] \
	    -width $data(-linewidth) -fill $data(-linecolor)
    $c move TAB:$tab $data(width) [expr {($diff<0)?-$diff:0}]
    $c raise $i
    $c raise LINE:$tab
    incr data(width) $FW
    $c configure -width $data(width) -height $data(height) \
	    -scrollregion "0 0 $data(width) $data(height)"
    $c bind TAB:$tab <1> [namespace code [list _activate $w $tab]]
    array set data [list :$tab:-window $s(-window) :$tab:-state $s(-state)]
    if {[string compare $s(-window) {}]} {
	grid $s(-window) -in $data(hold) -row 0 -column 0 -sticky nsew
	lower $s(-window)
    }
    switch $s(-state) {
	active	{_activate $w $tab}
	disabled {$c itemconfig POLY:$tab -fill $data(-disabledbackground)}
	normal	{$c itemconfig POLY:$tab -fill $data(-normalbackground)}
    }
    lappend data(ids) $tab
    return $tab
}

;proc _activate { w id } {
    upvar \#0 [namespace current]::$w data

    if {[string compare $data(-browsecmd) {}] && \
	    [catch {uplevel \#0 $data(-browsecmd) \
	    [list [_name $w $oldtab] [_name $w $tab]]}]} {
	return
    }
    if {[string compare $id {}]} {
	set tab [verify $w $id]
	if {[string match disabled $data(:$tab:-state)]} return
    } else {
	set tab {}
    }
    if {[string match $data(curtab) $tab]} return
    set c $data(tabs)
    set oldtab $data(curtab)
    if {[string compare $oldtab {}]} {
	$c itemconfig POLY:$oldtab -fill $data(-normalbackground)
	$c move TAB:$oldtab 0 2
	set data(:$oldtab:-state) normal
    }
    set data(curtab) $tab
    if {[string compare $tab {}]} {
	set data(:$tab:-state) active
	$c itemconfig POLY:$tab -fill $data(-activebackground)
	$c move TAB:$tab 0 -2
    }
    if {[info exists data(:$tab:-window)] && \
	    [winfo exists $data(:$tab:-window)]} {
	raise $data(:$tab:-window)
    } else {
	raise $data(blank)
    }
    resize $w [winfo width $w]
}

;proc _delete { w id } {
    upvar \#0 [namespace current]::$w data

    set tab [verify $w $id]
    set c $data(tabs)
    foreach {x1 y1 x2 y2} [$c bbox TAB:$tab] { set W [expr {$x2-$x1-3}] }
    $c delete TAB:$tab
    for { set i [expr {$tab+1}] } { $i <= $data(numtabs) } { incr i } {
	$c move TAB:$i -$W 0
    }
    foreach {x1 y1 x2 y2} [$c bbox all] { set H [expr {$y2-$y1-3}] }
    if {$H<$data(height)} {
	$c move all 0 [expr {$H-$data(height)}]
	set data(height) $H
    }
    incr data(width) -$W
    $c config -width $data(width) -height $data(height) \
	    -scrollregion "0 0 $data(width) $data(height)"
    set i [lsearch $data(ids) $tab]
    set data(ids) [lreplace $data(ids) $i $i]
    catch {grid forget $data(:$tab:-window)}
    unset data(:$tab:-state) data(:$tab:-window)
    if {[string match $tab $data(curtab)]} {
	set data(curtab) {}
	raise $data(blank)
    }
}

;proc _itemcget { w id key } {
    upvar \#0 [namespace current]::$w data

    set tab [verify $w $id]
    set opt [array names data :$tab:$key*]
    set len [llength $opt]
    if {$len == 1} {
	return $data($opt)
    } elseif {$len == 0} {
	set all [array names data :$tab:-*]
	foreach o $all { lappend opts [lindex [split $o :] end] }
	return -code error "unknown option \"$key\", must be one of:\
		[join $opts {, }]"
    } else {
	foreach o $opt { lappend opts [lindex [split $o :] end] }
	return -code error "ambiguous option \"$key\", must be one of:\
		[join $opts {, }]"
    }
}

;proc _itemconfigure { w id args } {
    upvar \#0 [namespace current]::$w data

    set tab [verify $w $id]
    set len [llength $args]
    if {$len == 1} {
	return [uplevel 1 _itemcget $w $tab $args]
    } elseif {$len&1} {
	return -code error "uneven set of key/value pairs in \"$args\""
    }
    if {[string match {} $args]} {
	set all [array names data :$tab:-*]
	foreach o $all { lappend res [lindex [split $o :] end] $data($o) }
	return $res
    }
    foreach {key val} $args {
	switch -glob -- $key {
	    -w*	{
		if {[string comp $val {}]} {
		    if {![winfo exist $val]} {
			return -code error "window \"$val\" does not exist"
		    } elseif {[string comp $w [winfo parent $val]] && \
			    [string comp $data(hold) [winfo parent $val]]} {
			return -code error "window \"$val\" must be a\
				child of the tab notebook ($w)"
		    }
		}
		set old $data(:$tab:-window)
		if {[winfo exists $old]} { grid forget $old }
		set data(:$tab:-window) $val
		if {[string comp $val {}]} {
		    grid $val -in $data(hold) -row 0 -column 0 \
			    -sticky nsew
		    lower $val
		}
		if {[string match active $data(:$tab:-state)]} {
		    if {[string comp $val {}]} {
			raise $val
		    } else {
			raise $data(blank)
		    }
		}
	    }
	    -s* {
		if {![regexp {^(normal|disabled|active)$} $val]} {
		    return -code error "unknown state \"$val\", must be:\
			    normal, disabled or active"
		}
		if {[string match $val $data(:$tab:-state)]} return
		set old $data(:$tab:-state)
		switch $val {
		    active		{
			set data(:$tab:-state) $val
			_activate $w $tab
		    }
		    disabled	{
			if {[string match active $old]} { _activate $w {} }
			$data(tabs) itemconfig POLY:$tab \
				-fill $data(-disabledbackground)
			set data(:$tab:-state) $val
		    }
		    normal		{
			if {[string match active $old]} { _activate $w {} }
			$data(tabs) itemconfig POLY:$tab -fill {}
			set data(:$tab:-state) $val
		    }
		}
	    }
	    default {
		return -code error "unknown option '$key', must be:\
			[join [array names s] {, }]"
	    }
	}
    }
}

## given a tab number, return the text
;proc _name { w id } {
    upvar \#0 [namespace current]::$w data

    if {[string match {} $id]} return
    set text {}
    foreach item [$data(tabs) find withtag TAB:$id] {
	set tags [$data(tabs) gettags $item]
	if {[set i [lsearch -glob $tags {ID:*}]] != -1} {
	    set text [string range [lindex $tags $i] 3 end]
	    break
	}
    }
    return $text
}

#;proc _order {w args} {
#    upvar \#0 [namespace current]::$w data
#
#    foreach i $data(ids) {
#    }
#}

## Take all the tabs and reculate space requirements
;proc recalculate {w} {
    upvar \#0 [namespace current]::$w data

    set c $data(tabs)
    set px [expr {int(ceil($data(-padx)/2))}]
    set py [expr {int(ceil($data(-pady)/2))}]
    set data(width) 0
    set data(height) 0
    foreach i $data(ids) {
	$c coords M:$i [expr \
		{[string match text [$c type M:$i]]?$px+1:$px}] $py
	foreach {x1 y1 x2 y2} [$c bbox M:$i] {
	    set W  [expr {$x2-$x1+$px}]
	    set FW [expr {$W+$px}]
	    set FH [expr {$y2-$y1+3*$py}]
	}
	set diff [expr {$FH-$data(height)}]
	if {$diff > 0} {
	    $c move all 0 $diff
	    $c move M:$i 0 -$diff
	    set data(height) $FH
	}
	$c coords POLY:$i 0 $FH $px $py $W $py $FW $FH
	$c coords LINE:$i 0 $FH $px $py $W $py $FW $FH
	$c move TAB:$i $data(width) [expr {($diff<0)?-$diff:0}]
	incr data(width) $FW
    }
    $c configure -width $data(width) -height $data(height) \
	    -scrollregion "0 0 $data(width) $data(height)"
}

;proc resize {w x} {
    upvar \#0 [namespace current]::$w data

    if {[string compare $data(curtab) {}]} {
	set x [expr {round(-[$data(tabs) canvasx 0])}]
	foreach {x1 y1 x2 y2} [$data(tabs) bbox TAB:$data(curtab)] {
	    place $data(hide) -y [winfo y $data(hold)] -x [expr {$x1+$x+3}]
	    $data(hide) config -width [expr {$x2-$x1-5}]
	}
    } else {
	place forget $data(hide)
    }
}

;proc see {w id} {
    upvar \#0 [namespace current]::$w data

    set c $data(tabs)
    set box [$c bbox $id]
    if {[string match {} $box]} return
    foreach {x y x1 y1} $box {left right} [$c xview] \
	    {p q xmax ymax} [$c cget -scrollregion] {
	set xpos [expr {(($x1+$x)/2.0)/$xmax - ($right-$left)/2.0}]
    }
    $c xview moveto $xpos
}

;proc verify { w id } {
    upvar \#0 [namespace current]::$w data

    set c $data(tabs)
    if {[string compare [set i [$c find withtag ID:$id]] {}]} {
	if {[regexp {TAB:([0-9]+)} [$c gettags [lindex $i 0]] junk id]} {
	    return $id
	}
    } elseif {[string compare [$c find withtag TAB:$id] {}]} {
	return $id
    }
    return -code error "unrecognized tab \"$id\""
}

}; #end of namespace ::Widget::Tabnotebook