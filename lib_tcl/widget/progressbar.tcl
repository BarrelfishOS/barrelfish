##
## Copyright 1996-1997 Jeffrey Hobbs, jeff.hobbs@acm.org
## Some Enhancements done by Steve Ball
##
package require Widget 2.0
package provide Progressbar 2.0

##------------------------------------------------------------------------
## PROCEDURE
##	progressbar
##
## DESCRIPTION
##	Implements a Progressbar mega-widget
##
## ARGUMENTS
##	progressbar <window pathname> <options>
##
## OPTIONS
##	(Any canvas widget option may be used in addition to these)
##
## -indicatorcolor			DEFAULT: #5ae6fe
##	The color of the progressbar.  Must be in #rgb format.
##	This is also the default item start foreground color.
##
## -itembackground			DEFAULT: {}
##	Default item background color.  {} means transparent.
##
## -itemfgfinished			DEFAULT: #00ff00 (green)
##	Default item finished foreground color.  Must be in #rgb format.
##
## -itemtype				DEFAULT: document
##	Default item type (currently 'document' and 'image' are supported).
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
## -maxvalue #				DEFAULT: 0 (percentage-based)
##	This represents what the representative max value of the progress
##	bar is.  If it is 0, the progress bar interprets the -value option
##	like a percentage (with an implicit 100 value for -maxvalue),
##	otherwise it is representative of what -value would have to reach
##	for the progress to be at 100%.
##
## -orientation horizontal|vertical	DEFAULT: horizontal
##	Orientation of the progressbar
##
## -showvalue TCL_BOOLEAN		DEFAULT: 1
##	Whether or not to show the exact value beside the bar (it is
##	displayed as a percentage of the possible max value).
##
## -showerror TCL_BOOLEAN		DEFAULT: 1
##	Whether to raise an error in the trace on the -variable if the
##	appropriate range is exceeded.
##
## -value #				DEFAULT: 0
##	The value of the progress bar.  This will be used to calculate the
##	overall progress percentage in conjunction with the -maxvalue option.
##
## -variable varname			DEFAULT: {}
##	A variable from which to get the value for the bar.  This variable
##	will have a trace set upon it that forces a postive value.  It cannot
##	be unset until the widget is destroyed or you change this option.
##
## METHODS
##	These are the methods that this megawidget recognizes.  Aside from
##	those listed here, it accepts what is valid for canvas widgets.
##
## create ?item? ?-option value ...?
##	Start displaying the progress of an item.  "item" is the
##	name to associate with the item.  If no name is supplied, a unique
##	name is generated.  If an item of the same name already exists, then
##	a new unique name is generated.  Returns the name of the created item.
##
## delete item
##	Remove the given item from the list of items being displayed.
##	Total progress is updated appropriately.
##
## itemconfigure item ?-option value?
##	Sets option(s) for an item.
##
##	VALID ITEM OPTIONS
##
##	-background color	Background color of icon associated with item.
##	-fgstart #rgb		Initial foreground color of item's icon.
##	-fgfinished #rgb	Final foreground color of item's icon.
##				The progressbar changes the shade of the icon
##				from the initial to the final color in
##				conjunction with the %age of maxvalue.
##	-maxvalue #	max value that represents 100% of possible value
##	-type type	item type (document and image currently supported)
##			This can only be set at creation.
##	-value #	current progress toward full value of maxvalue
##
## itemcget item -option
##	Returns the current value of the option for the given item
##
## names ?pattern?
##	Returns the names of the progressbar's constituent items.
##	An optional pattern can limit the return result.
##
## recalculate
##	Recalculates the value and maxvalue of the progressbar based
##	on the values of the consituent items, if any.  This is only
##	necessary when changing from using the progressbar without items
##	to using it with items.
##
## subwidget widget
##	Returns the true widget path of the specified widget.  Valid
##	widgets are label, canvas.
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
## EXAMPLE USAGE:
##
## pack [progressbar .p -labeltext "Usage:" -variable usage] -fill x -exp 1
## for {set i 0} {$i <= 10} {incr i} { set usage ${i}0; update; after 1000 }
##
##
##------------------------------------------------------------------------

# Create this to make sure there are registered in auto_mkindex
# these must come before the [widget create ...]
proc Progressbar args {}
proc progressbar args {}
widget create Progressbar -type frame -base canvas -components {
    label
    {base canvas canvas {-highlightthickness 0 \
	    -bd 1 -relief ridge -width 100 -height 25}}
} -options {
    {-bd		-borderwidth}
    {-borderwidth	borderWidth	BorderWidth	0}
    {-font		ALIAS label -font}
    {-fg		-foreground}
    {-foreground	ALIAS label -foreground}
    {-indicatorcolor	indicatorColor	Color		#5ae6fe}
    {-indicatorcolour	-indicatorcolor}
    {-itembackground	itemBackground	Background	{}}
    {-itemfgfinished	itemForegroundFinished Foreground #00ff00}
    {-itemtype		itemType	ItemType	document}
    {-labelanchor	labelAnchor	Anchor		c}
    {-labeltext		labelText	Text		{}}
    {-labelwidth	labelWidth	Width		0}
    {-maxvalue		maxValue	Value		0}
    {-orientation	orientation	Orientation	horizontal}
    {-relief		relief		Relief		flat}
    {-showvalue		showValue	ShowValue	1}
    {-showerror		showError	ShowError	1}
    {-value		value		Value		0}
    {-variable		variable	Variable	{}}
}

namespace eval ::Widget::Progressbar {;

;proc construct {w} {
    upvar \#0 [namespace current]::$w data

    ## Private variables
    array set data {
	counter		0
    }
    set data(items) $data(class)${w}ITEMS

    grid $data(label) $data(canvas) -in $w -sticky ns
    grid configure $data(canvas) -sticky news
    grid columnconfig $w 1 -weight 1
    grid rowconfig $w 0 -weight 1
    grid remove $data(label)

    bind $data(canvas) <Configure> [namespace code [list resize $w %w %h]]
}

;proc init {w} {
    upvar \#0 [namespace current]::$w data
    $data(basecmd) create rect -1 0 0 25 -fill $data(-indicatorcolor) \
	    -tags bar -outline {}
    $data(basecmd) create text 25 12 -fill $data(-foreground) \
	    -tags text -anchor c
    $data(basecmd) xview moveto 0
    $data(basecmd) yview moveto 0
}

;proc configure { w args } {
    upvar \#0 [namespace current]::$w data

    set truth {^(1|yes|true|on)$}
    set resize 0
    set force  0
    foreach {key val} $args {
	switch -- $key {
	    -borderwidth - -relief { .$w configure $key $val }
	    -font	{
		$data(label) configure -font $val
		$data(basecmd) itemconfigure text -font $val
	    }
	    -foreground		{
		$data(label) configure -foreground $val
		$data(basecmd) itemconfigure text -fill $val
	    }
	    -indicatorcolor	{
		$data(basecmd) itemconfigure bar -fill $val
	    }
	    -labelanchor	{ $data(label) configure -anchor $val }
	    -labeltext		{
		$data(label) configure -text $val
		if {[string compare {} $val]} {
		    grid $data(label)
		} else {
		    grid remove $data(label)
		}
	    }
	    -labelwidth		{ $data(label) configure -width $val }
	    -maxvalue		{
		if {![regexp {^[0-9]+$} $val] || $val<0} {
		    return -code error "$key must be a positive integer"
		}
		set force 1
	    }
	    -orientation	{
		if {[string match h* $val]} {
		    set val horizontal
		} elseif {[string match v* $val]} {
		    set val vertical
		} else {
		    return -code error \
			    "orientation must be horizontal or vertical"
		}
		if {[string compare $data($key) $val]} {
		    set W [$data(basecmd) cget -width]
		    set H [$data(basecmd) cget -height]
		    $data(basecmd) configure -height $W -width $H
		    set resize 1
		}
	    }
	    -showvalue	{
		set val [regexp -nocase $truth $val]
		set resize 1
	    }
	    -showerror	{ set val [regexp -nocase $truth $val] }
	    -value	{
		if {[catch {barset $w $val} err] && \
			$data(-showerror)} {
		    return -code error $err
		}
		if {$resize} { set resize 0 }
		if {$force} { set force 0 }
		continue
	    }
	    -variable	{
		if {![string compare $val $data(-variable)]} return
		if {[string compare {} $data(-variable)]} {
		    uplevel \#0 [list trace vdelete $data(-variable) wu \
			    [namespace code [list bartrace $w]]]
		    set data(-variable) {}
		}
		if {[string compare {} $val]} {
		    set data(-variable) $val
		    upvar \#0 $val var
		    if {![info exists var] || \
			    [catch {barset $w $var} err]} {
			set var $data(-value)
		    }
		    uplevel \#0 [list trace var $val wu \
			    [namespace code [list bartrace $w]]]
		}
		## avoid the set data($key)
		continue
	    }
	}
	set data($key) $val
    }
    if {$force || ($resize && [winfo ismapped $data(canvas)])} {
	resize $w [winfo width $data(canvas)] [winfo height $data(canvas)]
    }
}

;proc destruct w {
    upvar \#0 [namespace current]::$w data
    catch {configure $w -variable {}}
}

;proc bartrace {w name el op} {
    upvar \#0 [namespace current]::$w data
    upvar \#0 $data(-variable) var
    if {[string match u $op]} {
	set var $data(-value)
	uplevel \#0 [list trace var $data(-variable) wu \
		[namespace code [list bartrace $w]]]
    } elseif {[catch {barset $w $var} err]} {
	set var $data(-value)
	if $data(-showerror) { return -code error $err }
    }
}

;proc resize {w W H} {
    upvar \#0 [namespace current]::$w data

    ## Assume a maxvalue of 100 if maxvalue is 0 (works like %age)
    if {$data(-maxvalue)} {
	set pcnt [expr {$data(-value)/double($data(-maxvalue))}]
    } else {
	set pcnt [expr {$data(-value)/100.0}]
    }
    if {[string match h* $data(-orientation)]} {
	$data(basecmd) coords bar -1 0 [expr {$pcnt*$W}] $H
    } else {
	## Vertical orientation needs testing
	$data(basecmd) coords bar -1 $H $W [expr {$pcnt*$H}]
    }
    if $data(-showvalue) {
	$data(basecmd) coords text [expr {$W/2}] [expr {$H/2-2}]
	$data(basecmd) itemconfigure text -text [expr $pcnt*100.0]%
    } else {
	$data(basecmd) coords text $W $H
    }
}

;proc barset {w val} {
    upvar \#0 [namespace current]::$w data
    if {![regexp {^[0-9]+$} $val] || $val<0} {
	return -code error "value must be an integer greater than 0"
    }
    if {[string comp {} $data(-variable)]} {
	upvar \#0 $data(-variable) var
	if {[catch {set var $val} err]} {
	    return -code error $err
	}
    }
    set data(-value) $val
    resize $w [winfo width $data(canvas)] [winfo height $data(canvas)]
}

# Manage progress items.  These may be documents or images.
# (There needs to be an extensible system to allow other types, eg. Tclets)
# Each item may have a max value and a current value.
# The total download progress is calculated from the sums of item sizes.

;proc _create {w args} {
    set namesp [namespace current]
    upvar \#0 ${namesp}::$w data

    set cnt [incr data(counter)] 
    if {[string match -* [lindex $args 0]]} {
	# Invent a name
	set item progress$cnt
    } else {
	set item [lindex $args 0]
	set args [lrange $args 1 end]
	if {[info exists data(I:$item)]} {
	    # Ensure name doesn't already exist
	    return -code error "item \"$item\" already exists"
	}
    }

    array set config [list \
	    -background $data(-itembackground) \
	    -fgstart	$data(-indicatorcolor) \
	    -fgfinished	$data(-itemfgfinished) \
	    -maxvalue	100 \
	    -type	$data(-itemtype) \
	    -value	0 \
	    ]
    array set configargs $args
    if {[info exists configargs(-type)]} {
	if {[string match {} \
		[info commands ${namesp}::icon_$configargs(-type)]]} {
	    return -code error "invalid item type $configargs(-type)"
	}
	set config(-type) $configargs(-type)
	unset configargs(-type)
    }
    incr data(-maxvalue) $config(-maxvalue)
    incr data(-value) $config(-value)
    # Add to display
    set config(image) [image create bitmap $w:$item \
	    -data [${namesp}::icon_$config(-type) cget -data] \
	    -foreground $config(-fgstart) \
	    -background $config(-background)]
    set config(w) [label $w.item$cnt -image $config(image)]
    foreach {ncols nrows} [grid size $w] break
    if {[string match h* $data(-orientation)]} {
	grid $config(w) -row 0 -column $ncols
    } else {
	grid $config(w) -row $nrows -column 0
    }

    set data(I:$item) [array get config]

    if {[string compare {} $args]} {
	eval _itemconfigure [list $w] [list $item] [array get configargs]
    } else {
	barset $w $data(-value)
    }

    return $item
}

# Turns #rgb into 3 elem list of decimal vals.
;proc parse_color c {
    set c [string tolower $c]
    if {[regexp {^\#([0-9a-f])([0-9a-f])([0-9a-f])$} $c x r g b]} {
	# appending "0" right-shifts 4 bits
	scan "${r}0 ${g}0 ${b}0" "%x %x %x" r g b
    } else {
	if {![regexp {^\#([0-9a-f]+)$} $c junk hex] || \
		[set len [string length $hex]]>12 || $len%3 != 0} {
	    return -code error "bad color value \"$c\""
	}
	set len [expr {$len/3}]
    	scan $hex "%${len}x%${len}x%${len}x" r g b
    }
    return [list $r $g $b]
}

## Returns a shade between two colors based on the frac (0.0-1.0)
;proc shade {orig dest frac} {
    if {$frac >= 1.0} { return $dest } elseif {$frac <= 0.0} { return $orig }
    foreach {origR origG origB} [parse_color $orig] \
	    {destR destG destB} [parse_color $dest] {
	set shade [format "\#%02x%02x%02x" \
		[expr {int($origR+double($destR-$origR)*$frac)}] \
		[expr {int($origG+double($destG-$origG)*$frac)}] \
		[expr {int($origB+double($destB-$origB)*$frac)}]]
	return $shade
    }
}

;proc _delete {w args} {
    upvar \#0 [namespace current]::$w data

    foreach item $args {
	## Don't complain about unknown items when deleting
	if {![info exists data(I:$item)]} continue

	array set config $data(I:$item)

	incr data(-value) -$config(-value)
	incr data(-maxvalue) -$config(-maxvalue)
	if {$data(-value) < 0} { set data(-value) 0 }
	if {$data(-maxvalue) < 0} { set data(-maxvalue) 0 }

	destroy $config(w)
	image delete $config(image)
	unset data(I:$item)
    }
    barset $w $data(-value)
}

## _itemconfigure
## configure a progressar constituent item
##
;proc _itemconfigure {w item args} {
    upvar \#0 [namespace current]::$w data

    if {![info exists data(I:$item)]} {
	return -code error "unknown item \"$item\""
    }

    array set config $data(I:$item)
    if {[string match {} $args]} { return [array get config -*] }

    set valChanged 0
    foreach {key val} $args {
	if {[string match {} [set arg [array names config $key]]]} {
	    set arg [array names config ${key}*]
	}
	set num [llength $arg]
	if {$num==0} {
	    return -code error "unknown option \"$key\", must be:\
		    [join [array names config -*] {, }]"
	} elseif {$num>1} {
	    return -code error "ambiguous option \"$args\",\
		    must be one of: [join $arg {, }]"
	} else {
	    set key $arg
	}
	switch -- $key {
	    -maxvalue	{
		if {![regexp {^[0-9]+$} $val] || $val<=0} {
		    return -code error "$key must be an integer greater than 0"
		}
		incr data(-maxvalue) [expr {$val-$config(-maxvalue)}]
		if {$data(-maxvalue) < 0} { set data(-maxvalue) 0 }
		set valChanged 1
	    }
	    -value	{
		if {![regexp {^[0-9]+$} $val] || $val<0} {
		    return -code error "$key must be a postive integer"
		}
		incr data(-value) [expr {$val-$config(-value)}]
		if {$data(-value) < 0} { set data(-value) 0 }
		set valChanged 1
	    }
	    -type	{
		## Should we allow this to be changed?
		return -code error "-type cannot be changed after creation"
	    }
	    -fgstart	{
		if {![regexp {^\#([0-9a-f]+)$} $val]} {
		    return -code error "color value must be in \#rgb format"
		}
	    }
	    -fgfinished	{
		if {![regexp {^\#([0-9a-f]+)$} $val]} {
		    return -code error "color value must be in \#rgb format"
		}
	    }
	}
	set config($key) $val
    }
    set data(I:$item) [array get config]

    if {$config(-maxvalue)} {
	$config(image) configure -background $config(-background) \
		-foreground [shade $config(-fgstart) $config(-fgfinished) \
		[expr {double($config(-value))/$config(-maxvalue)}]]
    }
    if {$valChanged} { barset $w $data(-value) }
}

## _itemcget
## Returns a single item option
##
;proc _itemcget {w item opt} {
    upvar \#0 [namespace current]::$w data

    if {![info exists data(I:$item)]} {
	return -code error "unknown item \"$item\""
    }
    array set config $data(I:$item)
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

    set items {}
    foreach name [array names data I:$pattern] {
	lappend items [string range $name 2 end]
    }
    return $items
}

## _recalculate
## recalculates the percentage based purely on the constituent items
## If there are no items, it just ensures that -(max)value is >= 0
##
;proc _recalculate {w} {
    upvar \#0 [namespace current]::$w data

    set items [array names data I:*]
    if {[string compare {} $items]} {
	set data(-maxvalue) 0
	set data(-value) 0
	foreach item $items {
	    array set config $data($item)
	    if {$config(-value) < 0} {set config(-value) 0}
	    if {$config(-maxvalue) < 0} {set config(-maxvalue) 0}
	    incr data(-value) $config(-value)
	    incr data(-maxvalue) $config(-maxvalue)
	    set data($item) [array get config]
	}
    } else {
	if {$data(-value) < 0} {set data(-value) 0}
	if {$data(-maxvalue) < 0} {set data(-maxvalue) 0}
    }
    barset $w $data(-value)
    return
}

image create bitmap [namespace current]::icon_document -data {#define document_width 20
#define document_height 23
static char document_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0xfc, 0x1f, 0x00, 0x04, 0x30, 0x00, 0x04, 0x50, 0x00, 0x04, 0x90, 0x00,
   0x04, 0x10, 0x01, 0x04, 0xf0, 0x03, 0x04, 0x00, 0x02, 0x04, 0x00, 0x02,
   0x04, 0x00, 0x02, 0x04, 0x00, 0x02, 0x04, 0x00, 0x02, 0x04, 0x00, 0x02,
   0x04, 0x00, 0x02, 0x04, 0x00, 0x02, 0x04, 0x00, 0x02, 0x04, 0x00, 0x02,
   0x04, 0x00, 0x02, 0x04, 0x00, 0x02, 0xfc, 0xff, 0x03};
}

image create bitmap [namespace current]::icon_image -data {#define image_width 20
#define image_height 23
static char image_bits[] = {
   0xe0, 0xff, 0xff, 0x20, 0xe0, 0xff, 0xe0, 0xff, 0xff, 0x30, 0xff, 0xff,
   0xe8, 0xf8, 0xff, 0xdf, 0xf7, 0xff, 0xbb, 0xff, 0xff, 0x7b, 0xff, 0xff,
   0xfb, 0xfe, 0xff, 0xfb, 0xfd, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xff,
   0xcf, 0x1f, 0xfc, 0x03, 0x0e, 0xf8, 0x20, 0x70, 0xf0, 0x18, 0x80, 0xf1,
   0x07, 0x00, 0xf0, 0x00, 0x1e, 0xf0, 0xf8, 0x01, 0xf0, 0x00, 0x00, 0xf0,
   0xc0, 0x7f, 0xf3, 0x00, 0x80, 0xf0, 0x40, 0x00, 0xf0};
}

}; #end namespace ::Widget::Progressbar
