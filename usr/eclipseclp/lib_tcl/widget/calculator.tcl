##
## Copyright 1996-1997 Jeffrey Hobbs, jeff.hobbs@acm.org
##
## WORK IN PROGRESS - NOT FUNCTIONAL
##

package require Widget 2.0
package provide Calculator 1.0

proc Calculator args {}
proc calculator args {}
widget create Calculator -type frame -base frame -components {
    {frame menubar mbar {-relief raised -bd 1}}
    {listbox data data {-height 5 -bg white \
	    -yscrollcommand [list $data(yscrollbar) set] \
	    -xscrollcommand [list $data(xscrollbar) set] \
	    -selectbackground yellow -selectborderwidth 0 \
	    -selectmode single -takefocus 1}}
    {scrollbar yscrollbar sy {-takefocus 0 -bd 1 -orient v \
	    -command [list $data(data) yview]}}
    {scrollbar xscrollbar sx {-takefocus 0 -bd 1 -orient h \
	    -command [list $data(data) xview]}}
    {entry entry e {-bg white -takefocus 1}}
    {frame modef}
    {frame buttons}
    {label label lbl {-fg \#0000FF -textvariable ${w}(message)}}
} -options {
    {-base	base	Base	DEC}
    {-degree	degree	Degree	RAD}
    {-menubar	menuBar	MenuBar	1}
    {-mode	mode	Mode	Trig}
    {-status	status	Status	0}
    {-type	type	Type	REG}
}

namespace eval ::Widget::Calculator {;

variable class
array set class {
    constants	{
	pi	3.141592654
	e	2.718281828
    }
}

;proc construct w {
    upvar \#0 [namespace current]::$w data

    array set data {
	index		end
	modes		{Scientific Logical Financial}
    }

    grid $data(menubar)	-	-sticky ew
    grid $data(data)    $data(yscrollbar)	-sticky news
    grid $data(xscrollbar)	-sticky ew
    grid $data(entry)	-	-sticky ew
    grid $data(modef)	-	-sticky ew
    grid $data(buttons)	-	-sticky news
    grid $data(label)	-	-sticky ew
    grid columnconfig $w 0 -weight 1
    grid rowconfigure $w 1 -weight 1
    grid remove $data(yscrollbar) $data(xscrollbar) $data(label)

    initMenus $w

    set b $data(buttons)
    for {set i 0} {$i < 10} {incr i} {
	button $b.$i -text $i -width 3 -bg \#d9d9FF \
		-command [namespace code [list _num $i]]
    }
    foreach i {A B C D E F} {
	button $b.[string tolower $i] -text $i -width 3 -bg \#d9d9FF \
		-command [namespace code [list _num $i]]
    }
    button $b.del  -text DEL	-command [namespace code [list _backspace $w]]
    button $b.clr  -text CLR	-command [namespace code [list _clear $w]]
    button $b.drop -text Drop	-command [namespace code [list _drop $w]]
    button $b.swap -text Swap	-command [namespace code [list _swap $w]]
    button $b.sign -text +/-	-command [namespace code [list _changesign $w]]
    button $b.inv  -text 1/x	-command [namespace code [list _invert $w]]
    button $b.xtoy -text x^y	-command [namespace code [list _func $w pow]]
    button $b.sqr  -text x^2	-command [namespace code [list _sqr $w]]
    button $b.sqrt -text Sqrt	-command [namespace code [list _sqrt $w]]
    button $b.perc -text %	-command [namespace code [list _percent $w]]
    button $b.dot  -text .	-command [namespace code [list _decimal $w]]
    button $b.add  -text + -bg yellow \
	    -command [namespace code [list _binary $w +]]
    button $b.sub  -text - -bg yellow \
	    -command [namespace code [list _binary $w -]]
    button $b.mul  -text * -bg yellow \
	    -command [namespace code [list _binary $w *]]
    button $b.div  -text / -bg yellow \
	    -command [namespace code [list _binary $w /]]

    grid $b.inv $b.sqr $b.sqrt $b.perc -sticky news
    grid $b.d $b.e $b.f $b.clr -sticky nsew
    grid $b.a $b.b $b.c $b.del -sticky nsew
    grid $b.7 $b.8 $b.9 $b.add -sticky nsew
    grid $b.4 $b.5 $b.6 $b.sub -sticky nsew
    grid $b.1 $b.2 $b.3 $b.mul -sticky nsew
    grid $b.sign $b.0 $b.dot $b.div -sticky nsew
    grid columnconfig $b 0 -weight 1
    grid columnconfig $b 1 -weight 1
    grid columnconfig $b 2 -weight 1
    grid columnconfig $b 3 -weight 1

    ## Standard bindings
    ##
    bind Calculator <<CalcNumber>>	[namespace code [list _num $i]]
    bind Calculator <<CalcBinOp>>	[namespace code [list _binary $w %K]]
    event add <<CalcBinOp>> <KP_Add> <KP_Subtract> <KP_Multiply> <KP_Divide> \
	    + - * /
    bind Calculator <Return>		[namespace code [list _enter $w]]
    bind Calculator <KP_Enter>		[namespace code [list _enter $w]]
    bind Calculator <KP_Decimal>	[namespace code [list _decimal $w]]
    bind Calculator .			[namespace code [list _decimal $w]]

    bind Calculator <Shift-BackSpace>	[namespace code [list _drop $w]]
    bind Calculator <BackSpace>		[namespace code [list _backspace $w]]

    _push $w {}
    recursive_bind $w
}

;proc configure {w args} {
    upvar \#0 [namespace current]::$w data

    set truth {^(1|yes|true|on)$}
    foreach {key val} $args {
	switch -- $key {
	    -base	{
		if {![regexp -nocase {^(DEC|HEX|OCT|BIN)$} $val]} {
		    return -code error "bad value \"$val\", must be one of:\
			    dec, hex, oct, bin"
		}
		set val [string toupper $val]
	    }
	    -degree	{
		if {![regexp -nocase {^(RAD|GRAD|DEG)$} $val]} {
		    return -code error "bad value \"$val\",\
			    must be one of: rad, grad, deg"
		}
		set val [string toupper $val]
	    }
	    -menubar	{
		if {[set val [regexp -nocase $truth $val]]} {
		    grid $data(menubar)
		} else {
		    grid remove $data(menubar)
		}
	    }
	    -mode	{
		if {![regexp -nocase ^([join $data(modes) |])\$ $val]} {
		    return -code error "bad value \"$val\",\
			    must be one of: [join $data(modes) {, }]"
		}
		set val [string toupper $val]
	    }
	    -status	{
		if {[set val [regexp -nocase $truth $val]]} {
		    grid $data(label)
		} else {
		    grid remove $data(label)
		}
	    }
	    -type	{
		if {![regexp -nocase ^([join $data(types) |])\$ $val]} {
		    return -code error "bad value \"$val\",\
			    must be one of: [join $data(types) {, }]"
		}
		set val [string toupper $val]
	    }
	}
	set data($key) $val
    }
}

;proc recursive_bind w {
    foreach c [winfo children $w] {
	if {[string compare Entry [winfo class $c]]} {
	    bindtags $c [concat [bindtags $c] Calculator]
	}
	recursive_bind $c
    }
}

;proc initMenus w {
    upvar \#0 [namespace current]::$w data

    ## File Menu
    ##
    set m $data(menubar).file
    pack [menubutton $m -text "File" -underline 0 -menu $m.m] -side left
    set m [menu $m.m]
    $m add command -label "Save" -underline 0

    ## Math Menu
    ##
    set m $data(menubar).math
    pack [menubutton $m -text "Math" -underline 0 -menu $m.m] -side left
    set m [menu $m.m]
    $m add cascade -label "Constants" -menu $m.const

    ## Constants Menu
    ##
    menu $m.const -postcommand [list winConst $w $m.const]

    ## Help Menu
    ##
    set m $data(menubar).help
    pack [menubutton $m -text "Help" -underline 0 -menu $m.m] -side right
    set m [menu $m.m]
    $m add command -label "About" -command [list _about $w]
}

;proc calcerror {w msg} {
    upvar \#0 [namespace current]::$w data

    if {[string compare $msg {}]} {
	tk_dialog $w.error "Calculator Error" $msg error 0 Oops
    }
}

;proc _constant {w type} {
    upvar \#0 [namespace current]::$w data

    array set const $data(constants)
    _push $w {}
}

;proc _convert {w from to args} {
    upvar \#0 [namespace current]::$w data

    foreach num $args {
    }
}

;proc _changesign w {
    upvar \#0 [namespace current]::$w data

    set arg1 [_pop $w]
    if {[string match {} $arg1]} { return 0 }
    _push $w [expr 0 - $arg1]
    _push $w {}
}

;proc _drop w {
    upvar \#0 [namespace current]::$w data

    _pop $w
    _push $w {}
}

;proc _backspace w {
    upvar \#0 [namespace current]::$w data

    if {[string match {} [_peek $w]]} {
	_pop $w
	_push $w {}
	return
    }
    set arg1 [_pop $w]
    set arg2 [string trimright $arg1 .]
    _push $w $arg2
}

;proc _binary {w op} { 
    upvar \#0 [namespace current]::$w data

    set arg1 [_pop $w]
    set arg2 [_pop $w]
    if {[string match {} $arg2]} {
	_push $w $arg1
	if {[string compare $arg1 {}]} { _push $w {} }
	return
    }
    _push $w [expr double($arg2) $op $arg1]
    _push $w {}
}

;proc commify {w ip} {
    upvar \#0 [namespace current]::$w data

    if {[string len $ip] > 3} {
	set fmt {([0-9])([0-9])([0-9])}
	switch [expr [string len $ip]%3] {
	    0 { regsub -all $fmt $ip {\1\2\3,} ip }
	    1 { regsub -all $fmt $ip {\1,\2\3} ip }
	    2 { regsub -all $fmt $ip {\1\2,\3} ip }
	}
	set ip [string trimright $ip ,]
    }
    return $ip
}

;proc _decimal w {
    upvar \#0 [namespace current]::$w data

    if {[string match {} [$data(data) get $data(index)]]} {
	_push $w 0.
    } else {
	set arg1 [_pop $w]
	_push $w [string trimright $arg1 .].
    }
}

;proc _enter w { 
    upvar \#0 [namespace current]::$w data

    set push 0
    if {[string match {} [$data(data) get $data(index)]]} {
	set push 1
    }
    set stk [_pop $w]
    if {[string match {} $stk]} { return 0 }
    _push $w $stk
    if $push { _push $w $stk }
    _push $w {}
}

;proc _func {w op} {
    upvar \#0 [namespace current]::$w data

    set arg1 [_pop $w]
    set arg2 [_pop $w]
    if {[string match {} $arg2]} {
	_push $w $arg1
	if {[string compare $arg1 {}]} { _push $w {} }
	return 0
    }
    _push $w [expr $op ($arg2, $arg1)]
    _push $w {}
}

;proc _invert w {
    upvar \#0 [namespace current]::$w data

    if {[string match {} [set arg1 [_pop $w]]} { return 0 }
    if {$arg1 == 0} {
	calcerror $w "Division by 0 error"
	return 0
    }
    _push $w [expr 1.0 / $arg1]
    _push $w {}
}

;proc _num {w val} {
    upvar \#0 [namespace current]::$w data

    set idx $data(index)
    if {[string match {} [$data(data) get $idx]]} {
	$data(data) delete $idx
	set stk {}
    } else {
	set stk [_pop $w $idx]
    }
    _push $w $stk$val $idx
}

;proc _swap w { 
    upvar \#0 [namespace current]::$w data

    set arg1 [_pop $w]
    set arg2 [_pop $w]
    if {[string compare $arg2 {}]} {
	_push $w $arg1
	if {[string compare $arg1 {}]} { _push $w {} }
	return 0
    }
    _push $w $arg1
    _push $w $arg2
    _push $w {}
}

;proc _unary {w op} {
    upvar \#0 [namespace current]::$w data

    set arg1 [_pop $w]
    if {[llength $arg1]} {
	_push $w [expr $op ($arg1)]
	_push $w {}
    } else { return 0 }
}

;proc _peek {w {idx {}}} {
    upvar \#0 [namespace current]::$w data

    if {[string match {} $idx]} { set idx [$data(data) curselection] }
    if {[string match {} [$data(data) get $idx]]} { $data(data) delete $idx }
    regsub -all {,} [$data(data) get $idx] {} val
    return $val
}

;proc _pop {w {idx {}}} {
    upvar \#0 [namespace current]::$w data

    if {[string match {} $idx]} { set idx [$data(data) curselection] }
    if {[string match {} [$data(data) get $idx]]} { $data(data) delete $idx }
    set val [$data(data) get $idx]
    if {[string match {} $val]} {
	calcerror $w "Not enough arguments"
	return
    }
    $data(data) delete $idx
    regsub -all {,} $val {} val
    $data(data) selection clear 0 end
    $data(data) selection set $idx $idx
    $data(data) see $idx
    if {[string compare end $data(index)]} {
	set data(index) [expr $idx+1]
    }
    return $val
}

;proc _push {w val {idx {}}} {
    upvar \#0 [namespace current]::$w data

    if {[string match {} $idx]} { set idx $data(index) }
    if {[string match {} [$data(data) get $idx]]} { $data(data) delete $idx }

    switch $data(-base) {
	DEC {
	    regsub -all {^0+} $val {} val
	    ## break into sign, integer, fractional and exponent parts
	    if {[regexp {^([-+])?([0-9]*)(\.)?([0-9]*)?(e[-+]?[0-9]+)?$} \
		    $val full sign ip dec fp ee]} {
		if {[string match {} $ip]} {
		    if {[string compare $full {}]} { set ip 0 }
		} else { set ip [commify $w $ip] }
		set val $sign$ip$dec$fp$ee
	    } else {
		#if [scan $val %d]
	    }
	}
	OCT {
	    if {![regsub -all {^0+} $val {0} val]} {
		set val 0$val
	    }
	}
	HEX {  }
	BIN {  }
    }
    $data(data) insert $idx $val
    $data(data) selection clear 0 end
    $data(data) selection set $idx $idx
    $data(data) see $idx
    #set data(index) $idx
}

}; # end namespace ::Widget::Calculator
