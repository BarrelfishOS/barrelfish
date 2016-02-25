#!/bin/sh
# The next line is executed by /bin/sh, but not tcl \
#exec wish $0 ${1+"$@"}

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
# Contributor(s): Kish Shen
# 
# END LICENSE BLOCK

## Inspect subterm Tcl/Tk interface to Eclipse
##
##
## By Kish Shen
##
## Last Update: 23 Oct 2000
##
## Notes on path: The path to a subterm is a list of individual argument
## positions. If the position is not a number, it is a specialised position
## that will is handled by the Prolog side for its special meaning, examples
## include attributes and structure arguments with fieldnames. It is also
## designed to allow lists to be handled specially. The Prolog side provides
## routine to handle the movement around (left, right, down) these special
## positions, as well as interpreting their meaning. Currently the special
## positions are:
## Pos=FieldName    Structure arguments with fieldnames
## Pos-Attribute    Attributes
## list(Nth)        Nth element of a list
## tail(Nth)        Tail of a list (at Nth position)
##
## Added support for follow-node y scroll
## Speeded up moving down structures in navigation panel by use of the new
## expandbranch hierarchy method (no intermediate updates). 
## Sept-Oct'00:
## Changed communications with ECLiPSe to be by ec_rpc goals only
## Display flat lists + number normal args

package provide tkinspect 1.0

set tkinspectvalues(invocnum)  -1
set tkinspectvalues(twinhdef)   3
set tkinspectvalues(defaultheight) 12c
set tkinspectvalues(defaultwidth)  14c
set tkinspectvalues(summary_truncate)	100


#-----------------------------------------------------------------------
# Icon setup
#-----------------------------------------------------------------------

package require tkec_icons 1.0

set tkinspectvalues(atomimage) stamp-16
set tkinspectvalues(varimage) xapp-16
set tkinspectvalues(attrimage) kcmx-16
#set tkinspectvalues(varimage) package-16
#set tkinspectvalues(attrimage) package_favourite-16
set tkinspectvalues(numimage) kit-16
set tkinspectvalues(structureimage) package-16
#set tkinspectvalues(structureimage) folder_red_open-16
set tkinspectvalues(stringimage) document2-16
set tkinspectvalues(deadimage) gv-16
set tkinspectvalues(schedimage) connect_creating-16
set tkinspectvalues(suspimage) connect_no-16
set tkinspectvalues(handleimage) exec-16
set tkinspectvalues(defaultimage) help-16
set tkinspectvalues(blankimage) blank-16
set tkinspectvalues(upimage) 1uparrow-16
set tkinspectvalues(downimage) 1downarrow-16
set tkinspectvalues(leftimage) 1leftarrow-16
set tkinspectvalues(rightimage) 1rightarrow-16
set tkinspectvalues(closeimage) fileclose-16


#-----------------------------------------------------------------------
# Create inspector window
#-----------------------------------------------------------------------

proc tkinspect:Inspect_term_init {source} {
    global tkinspectvalues tcl_platform tkecl

    set iw .ec_tools.inspect
    if {[winfo exists $iw]} {
	tkinspect:RaiseWindow $iw
	return
    }

    set tkinspectvalues($iw,source) $source
    set tkinspectvalues($iw,obslabel) " "
    set initstatus [tkinspect:select_command $iw $source]
    if {![string match "ok" $initstatus]} {
	tk_messageBox -type ok -message "Invalid term: unable to start Inspector..."
	return
    }

    toplevel $iw
    wm title $iw "ECLiPSe Inspector"


#-----------------------------------------------------------------------
# Menu Bar setup
#-----------------------------------------------------------------------
    set mbar [menu $iw.menubar]
    $iw config -menu $mbar
    $mbar add cascade -label "Windows" -menu $mbar.win -underline 0
#    $mbar add cascade -label "Options" -menu $mbar.opt -underline 0
    $mbar add cascade -label "Select" -menu $mbar.select -underline 0
    $mbar add cascade -label "Help" -menu $mbar.help -underline 0


#-------------------------------------------------------------------------
# Frames setup
#-------------------------------------------------------------------------

    set top [frame $iw.top]

    set tkinspectvalues($top.m,iw) $iw

    ;# $top.m should be $h. Need to set var. before widget is called :-(
    set h [hierarchy $top.m -root 1 -browsecmd tkinspect:Get_subterms \
	    -nodelook tkinspect:Look_term -expand 1 -selectmode single \
	    -paddepth 25 -padstack 6 -autoscrollbar 1 \
	    -command tkinspect:MayCentre -background white \
	    -selectbackground gray]

    # if we have saved a previous position and size, restore it
    if [info exists tkinspectvalues($iw,geometry)] {
    	wm geometry $iw $tkinspectvalues($iw,geometry)
    }

    set tf [frame $top.tf] 
    set bot [frame $iw.bot]


# Alert frame---------------------------------
    set alert_text [text $bot.alert -height 1 -background lightgray]
    bind $alert_text <Any-Key> {break} ;# read only
    bind $alert_text <Any-Button> {break} ;# really read only!
    bind $alert_text <Any-Motion> {break}
    bind $alert_text <Any-ButtonRelease> {break} 
    bind $alert_text <Leave> {break}
    $alert_text tag configure bold -font tkecllabel -justify left

# Control frame---------------------------------
    set cf [frame $top.control]
    tkinspect:PackCF $cf $iw $h $alert_text

#------------------------------------------------
# Frame packings
#------------------------------------------------
    pack $top -side top -expand true -fill both
    pack $bot -side bottom -fill x

    pack $cf -side top -fill x
    pack $h -expand true -fill both -side top
    pack $alert_text -side top -fill x
    pack [ttk::sizegrip $bot.alert.grip] -side right -anchor se


#------------------------------------------------------------------------
# Details
#------------------------------------------------------------------------

# text frame-----------------------------------------------------
    set t [text $tf.t -setgrid true -relief sunken -height $tkinspectvalues(twinhdef)\
	    -yscrollcommand "$tf.y set" -background white]
#    set tkinspectvalues($t,twinheight) $tkinspectvalues(twinhdef)
    scrollbar $tf.y -orient vert -command "$t yview"
    pack $tf.y -side right -fill y
    pack $t -side right -fill both -expand true
     set tkinspectvalues($h,twin) $t
    bind $t <Any-Key> {break}   ;# read only
    switch $tcl_platform(platform) {
	windows  { ;# strange fonts in windows!
	  $t tag configure highlight -foreground blue -underline true 
          $t tag configure phighlight -foreground darkgreen  
        }
	default {
	  $t tag configure highlight -foreground blue -font tkecllabel
          $t tag configure phighlight -foreground darkgreen \
		  -font tkecllabel
	}
    }
    $t tag configure truncated -background pink
    bind $t <ButtonRelease-2> {break}   ;# disable paste


# Menu bar-----------------------------------

    menu $mbar.select 
    $mbar.select add command -label "Current goal" \
	    -command "tkinspect:SelectCurrent $iw $h $alert_text"
    $mbar.select add command -label "Invoked goal ..." \
	    -command "tkinspect:SelectInvoc $iw $h $alert_text"

    menu $mbar.help 
    $mbar.help add check -label "Balloon help" -variable tkecl(pref,balloonhelp) -onvalue 1 -offvalue 0
    $mbar.help add command -label "Inspector help" -underline 0 -command "tkecl:Get_helpfileinfo insp $iw"

    menu $mbar.win 
    $mbar.win add command -label "Browser y-scroll control" \
	    -command "tkinspect:Deal_with_yscroll $top.ys $iw $h"
    $mbar.win add command -label "Symbol key" \
	    -command "tkinspect:DisplayKey $iw"
    $mbar.win add command -label "Inspector help" \
	    -command "tkecl:Get_helpfileinfo insp $iw"
    $mbar.win add separator
    $mbar.win add command -label "Close this Inspector" \
	    -command "tkinspect:Exit $iw"

   # pane $h $tf -orient vertical -initfrac [list 0.75 0.25]


#------------------------------------------------------------------
# Balloon Help
#------------------------------------------------------------------

    balloonhelp $h "Term browser area - browsing and inspection of term\n \
      Double leftmouse click toggles node expansion\n Arrow keys move \
      selected term relative to current position.\n Right (or control-left) click on a term \
      brings up a menu with summary information and options. \n Note: cannot interact \
      with other ECLiPSe windows while inspector is active.\n\
      Y scrollbars defaults to `follow-node' mode - change with yscroll control panel."
    balloonhelp $tf "Text display area - printed form of term and path information"
    balloonhelp $alert_text "Message area - alert/error messages from Inspector"
#    balloonhelp $top.__h1 "Press and drag mouse button 1 to adjust text/Term window sizes"

#-------------------------------------------------------------------
# Window bindings
#-------------------------------------------------------------------
    bind $iw <Alt-h> "tkecl:Get_helpfileinfo insp $iw"
    bind $iw <Key-Up> "tkinspect:Move $iw $h left $alert_text"
    bind $iw <Key-Left> "tkinspect:Move $iw $h up $alert_text"
    bind $iw <Key-Right> "tkinspect:MoveDown $iw $h $alert_text"
    bind $iw <Key-Down> "tkinspect:Move $iw $h right $alert_text"
    bind $h <Button-3> "tkinspect:Popup_menu $iw $h %X %Y %x %y $alert_text"
    bind $h <Control-Button-1> "tkinspect:Popup_menu $iw $h %X %Y %x %y $alert_text"

#-------------------------------------------------------------------
# Initialisations
#-------------------------------------------------------------------


    set tkinspectvalues($iw,movesize) 1
    set tkinspectvalues($iw,argpos) 2  ;# default for tail of list
    set hroot [$h indexnp 1]
    foreach {yfollow le re te be} [$h yfollowstate] {
	set tkinspectvalues(yfollow,$iw) $yfollow
	set tkinspectvalues(ysle,$iw) [expr round($le*100)]
	set tkinspectvalues(ysre,$iw) [expr round($re*100)]
	set tkinspectvalues(yste,$iw) [expr round($te*100)]
	set tkinspectvalues(ysbe,$iw) [expr round($be*100)]
    }
    $h selection set $hroot ;# select top-level
#    tkinspect:Display $h $hroot {} 
    if {![winfo viewable $iw]} {
	tkwait visibility $iw   ;# the grab may sometimes happen before visibility 
    }
    grab $iw
}



proc tkinspect:PackYS {iw h ys} {
    global tkinspectvalues

    set bf [frame $ys.b]
    pack [button $bf.update -text "Update" -command "tkinspect:Update_ys $iw $h"] \
	    -side left -expand true -fill x
    pack [button $bf.close -text "Close" -command "destroy $ys"] \
	    -side right -expand true -fill x
    pack $bf -side bottom -fill x

    set yff [frame $ys.yff -borderwidth 3 -relief ridge]
    pack [checkbutton $yff.yfollow -text "Viewable x area follows node on y scroll" -variable tkinspectvalues(yfollow,$iw)] 
    pack $yff -side top -expand true -fill x

    set xf [frame $ys.xef -borderwidth 3 -relief ridge]
    grid [label $xf.l -text "Centre when leading node is:"] -row 0 \
	    -column 0 -columnspan 2 -sticky news
    grid [scale $xf.le -label "% from left edge" -from 0 -to 50 -length 200 \
            -orient horizontal -tickinterval 25 -variable \
            tkinspectvalues(ysle,$iw)] -row 1 -column 0 -sticky news
    grid [scale $xf.re -label "% from right edge" -from 0 -to 50 -length 200 \
	    -orient horizontal -tickinterval 25 -variable \
            tkinspectvalues(ysre,$iw)] -row 1 -column 1 -sticky news
    pack $xf -side bottom -expand true

    set yf [frame $ys.xf -borderwidth 3 -relief ridge]
    grid [label $yf.l -text "Leading node is:"] -row 0 -column 0 \
	    -columnspan 2 -sticky news
    grid [scale $yf.te -label "% from top edge" -from 0 -to 50 -length 200 \
	    -orient horizontal -tickinterval 25 -variable \
	    tkinspectvalues(yste,$iw)] -row 1 -column 0 -sticky news
    grid [scale $yf.be -label "% from bottom edge" -from 0 -to 50 -length 200 \
	    -orient horizontal -tickinterval 25 -variable \
	    tkinspectvalues(ysbe,$iw)] -row 1 -column 1 -sticky news
    pack $yf -side bottom -expand true

    balloonhelp $yff "Enable or disable follow-node mode for y scrollbar in term browser area."
    balloonhelp $xf "These scales control the placement of the boundaries in the x viewable area of the term browser area.\nThe leading node will be displayed within these boundaries, by adjustment of the positioning of y viewable area,\n when the y scrollbar is manipulated in follow-node mod. Expressed as % of viewable width"
    balloonhelp $yf "These scales control the position of where the leading node will be selected in the term browser area.\nExpressed as % of viewable height."
    balloonhelp $bf.update "Update y scroll parameters - note that these parameters are not updated until this button is clicked."
    balloonhelp $bf.close "Close this window"
}

proc tkinspect:Update_ys {iw h} {
    global tkinspectvalues

    ;# .0 forces real arithmatic...
    set le [expr $tkinspectvalues(ysle,$iw).0/100]
    set re [expr $tkinspectvalues(ysre,$iw).0/100]
    set te [expr $tkinspectvalues(yste,$iw).0/100]
    set be [expr $tkinspectvalues(ysbe,$iw).0/100]
    $h yfollowitem  $le $re $te $be
	     
    if {!$tkinspectvalues(yfollow,$iw)} {
	$h ynofollowitem
    }
}


# Packing of control frame cf

proc tkinspect:PackCF {cf iw h alert_text} {
    global tkinspectvalues

    set side left
    set ef true
    set pad 5
    set fill x

    pack [button $cf.left -image $tkinspectvalues(leftimage) \
	    -command "tkinspect:Move $iw $h up $alert_text"] -side $side -padx $pad
    pack [button $cf.up -image $tkinspectvalues(upimage) \
	    -command "tkinspect:Move $iw $h left $alert_text"] -side $side -padx $pad
    pack [button $cf.down -image $tkinspectvalues(downimage) \
	    -command "tkinspect:Move $iw $h right $alert_text"] -side $side  -padx $pad
    pack [button $cf.right -image $tkinspectvalues(rightimage) \
	    -command "tkinspect:MoveDown $iw $h $alert_text"] -side $side -padx $pad

    pack [frame $cf.fill1] -side $side -expand $ef -fill $fill
    pack [label $cf.lchild -image view_tree-16] -side $side
    pack [spinbox $cf.child -from 1 -to 1000000000 -width 3 -justify right \
       -validate key -validatecommand {regexp {^([1-9][0-9]*)?$} %P} \
       -textvariable tkinspectvalues($iw,argpos)] \
       -side $side -fill x -padx $pad -pady 4

    pack [frame $cf.fill2] -side $side -expand $ef -fill $fill
    pack [label $cf.lrepeat -image redo-16] -side $side
    pack [spinbox $cf.repeat -from 1 -to 1000000000 -width 3 -justify right \
       -validate key -validatecommand {regexp {^([1-9][0-9]*)?$} %P} \
       -textvariable tkinspectvalues($iw,movesize)] \
       -side $side -fill x -padx $pad -pady 4

    pack [frame $cf.fill3] -side $side -expand $ef -fill $fill
    pack [label $cf.lprdepth -image goto-16] -side $side
    pack [spinbox $cf.prdepth -from 1 -to 1000000000 -width 3 -justify right \
       -validate key -validatecommand {regexp {^([1-9][0-9]*|0)?$} %P} \
       -textvariable tkecl(pref,inspect_prdepth)] \
       -side $side -fill x -padx $pad -pady 4

    pack [frame $cf.fill4] -side $side -expand $ef -fill $fill
    pack [label $cf.lldepth -image text_multirow-16] -side $side
    pack [spinbox $cf.ldepth -from 1 -to 1000000000 -width 3 -justify right \
       -validate key -validatecommand {regexp {^([1-9][0-9]*|0)?$} %P} \
       -textvariable tkecl(pref,inspect_ldepth)] \
       -side $side -fill x -padx $pad -pady 4

    pack [frame $cf.fill5] -side $side -expand $ef -fill $fill
    pack [label $cf.lsym -image flag-16] -side $side
    pack [ttk::checkbutton $cf.sym -variable tkecl(pref,inspect_nosymbols) -onvalue 0 -offvalue 1] \
       -side $side -padx 0

    pack [frame $cf.fill6] -side $side -expand true -fill $fill
    pack [button $cf.exit -image $tkinspectvalues(closeimage) -command "tkinspect:Exit $iw"] \
	    -side left -padx $pad
    # an empty label to work around a packer problem on tk8.5/linux
    pack [label $cf.fill7] -side $side

    balloonhelp $cf.up "Move to previous sibling term"
    balloonhelp $cf.left "Move to the parent term"
    balloonhelp $cf.right "Move to Nth child term"
    balloonhelp $cf.down "Move to next sibling term"
    balloonhelp $cf.lrepeat "Repeat count for movements"
    balloonhelp $cf.lchild "Argument position for child term move"
    balloonhelp $cf.lprdepth "Print depth limit"
    balloonhelp $cf.lldepth "List unfolding chunk size"
    balloonhelp $cf.lsym "Display term type symbols"
    balloonhelp $cf.exit "Close inspector"
}


proc tkinspect:MarkLevels {n np t} {

	$t delete 1.0 end
	$t insert end "remaining levels: $n"
        update idletasks
}
    
proc tkinspect:MoveDown {iw h t} {
    global tkinspectvalues

    set cur_idx [tkinspect:CurrentSelection $h]
    set current [lindex [$h get $cur_idx] 0]
    set downlevels [tkinspect:Get_numentry tkinspectvalues($iw,movesize) 1]
    set arg [tkinspect:Get_numentry tkinspectvalues($iw,argpos) 2]    
    foreach {status left np} [$h expandbranch $current $arg $downlevels \
	    [list tkinspect:MarkLevels $t]] {
	break
    }
    $t delete 1.0 end
    if {!$status} {
	set n [expr $downlevels - $left]
	$t insert end "Out of range after traversing down $n levels at \
		argument position $arg" bold
	bell
    } 
    set cur_idx [$h indexnp $np]
    tkinspect:Newselection $h $cur_idx
    $h centreitem $cur_idx 1.0 0.0 1.0 0.0 ;# always centre
}


# move to a new selection, clear old selection and display new one
proc tkinspect:Newselection {h cur_idx} {
    set selected [$h curselection]
    $h selection clear
    $h selection set $cur_idx 
    ;# $h see $cur_idx
    tkinspect:Display $h $cur_idx $selected
}


proc tkinspect:Numentry {w ltext tvar default matchtype width row col pad} {
    upvar #0 $tvar entryvar

    set entryvar $default
    switch -exact -- $matchtype {
	-1 { ;# can take negative integers
	    set vstring {regexp {^-?[0-9]*$} %P}
	}

	0  {;# can take zero + positive integers
	    set vstring {regexp {^[0-9]*$} %P}
	}

	1  {;# can take positive integers
	    set vstring {regexp {^([1-9][0-9]*|[1-9]?)$} %P}
	}
    }
    set entry [ventry $w -labeltext $ltext -vcmd $vstring \
       -validate key -invalidcmd bell -relief sunken -width $width \
       -textvariable $tvar -selectbackground red]
    grid  $entry -row $row -column $col -padx $pad -pady $pad -sticky news
}


proc tkinspect:Move {iw h dir t} {
    global tkinspectvalues

    set current [tkinspect:CurrentSelection $h]
    foreach {status current}  [tkinspect:move_command $iw $dir $current] {break}
    ;# current is now new current subterm's path
    set index [$h indexnp $current]

    tkinspect:Newselection $h $index
    $h centreitem $index 1.0 0.0 1.0 0.0 ;# always centre
    if {[string match $status "true"]} {
	$t delete 1.0 end
    } else {
	$t delete 1.0 end 
	$t insert end "Out of range. Stop before move completed" bold
	bell
    }
}


proc tkinspect:Exit {iw} {
    global tkinspectvalues

    tkinspect:inspect_command $tkinspectvalues($iw,source) [list end] {}
    set tkinspectvalues($iw,geometry) [wm geometry $iw]
    destroy $iw
}


proc tkinspect:Get_subterms {hw path} {
    global tkinspectvalues

    set iw $tkinspectvalues($hw,iw)
    foreach {termtype arity} [tkinspect:Get_subterm_info $iw $path \
	    termname summary] {break}
    return [tkinspect:Expand_termtype $iw $path $termtype $termname $arity $summary]
}


proc tkinspect:Expand_termtype {iw path termtype termname arity summary} {
    global tkinspectvalues

    ;# code changed so that ECLiPSe now does the expansion, always.
    return [tkinspect:inspect_command $tkinspectvalues($iw,source) [list childnodes $termtype $arity \
       [tkinspect:Get_numentry tkecl(pref,inspect_ldepth) 20] $path] {()II[S*]}]
}

proc tkinspect:Look_term {hw np isopen} {
    global tkinspectvalues tkecl

    set iw $tkinspectvalues($hw,iw)
    foreach {termtype arity} [tkinspect:Get_subterm_info $iw $np termname0 \
	    summary0] {break}
    set lastpos [lindex $np end]
    set modifier [tkinspect:Modify_name $iw $lastpos]
    if {[string match modifier {}]} {
	set termname $termname0
	set summary $summary0
    } else {
	append termname $modifier $termname0
	append summary  $modifier $summary0
    }
    ;# modify name text string if required (e.g. at top-level of attribute)
    ;# now truncate string if too long...
    if {[string length $termname] >= $tkecl(pref,text_truncate)} {
	set termname [string range $termname 0 $tkecl(pref,text_truncate)]
	append termname "..."
    }
    switch -exact -- $termtype {
	db_reference -
	handle      {
	    set image [tkinspect:DisplayImage $hw handleimage]
	    return [list $termname {} $image {black}]
	}

	exphandle      {
	    set image [tkinspect:DisplayImage $hw handleimage]
	    if {$isopen == 1} {
		set colour black
	    } else {
		set colour red
	    }
	    return [list $termname $font $image $colour]
	}

	atom {
	    set image [tkinspect:DisplayImage $hw atomimage]
	    return [list $termname {}  $image {black}]
	}

	list -
	ncompound -
	compound {
	    set image [tkinspect:DisplayImage $hw structureimage]
	    if {$isopen == 1} {
		set retname $summary
		set colour black
		set font tkeclmono
	    } else {
		set retname $termname
		set colour red
		set font tkecllabel
	    }
	    return [list $retname $font $image $colour]
	}

	var {
	    set image [tkinspect:DisplayImage $hw varimage]
	    return [list $termname {} $image {black}]
	}

	attributed {
	    set image [tkinspect:DisplayImage $hw attrimage]
	    if {$isopen == 1} {
		set font tkeclmono
		set colour black
	    } else {
		set font tkecllabel
		set colour red
	    }
	    return [list $termname $font $image $colour]
	}

	float    -
	rational -
	breal    -
	integer {
	    set image [tkinspect:DisplayImage $hw numimage]
	    return [list $termname {} $image {black}]
	}

	string {
	    set image [tkinspect:DisplayImage $hw stringimage]
	    return [list $termname {} $image {black}]
	}

	suspended {
	    set image [tkinspect:DisplayImage $hw suspimage]
	    if {$isopen == 1} {
		set font {-family fixed}
		set colour black
	    } else {
		set font {-family fixed -weight bold}
		set colour red
	    }
	    return [list $termname $font $image $colour]
	}

	scheduled {
	    set image [tkinspect:DisplayImage $hw schedimage]
	    if {$isopen == 1} {
		set font {-family fixed}
		set colour black
	    } else {
		set font {-family fixed -weight bold}
		set colour red
	    }
	    return [list $termname $font $image $colour]
	}


	dead {
	    set image [tkinspect:DisplayImage $hw deadimage]
	    return [list $termname {} $image {black}]
	}

	default     { 
	    tk_messageBox -icon warning -type ok -message "Unknown subterm type `$termtype'. Please report this problem (follow the links in About this ECLiPSe)"
	    set image [tkinspect:DisplayImage $hw defaultimage]
	    return [list $termname {} $image {black}]
	}

    }
}


proc tkinspect:DisplayImage {hw itype} {
    global tkinspectvalues tkecl

    if {$tkecl(pref,inspect_nosymbols) == 0} {
	set image $tkinspectvalues($itype)
    } else {
	set image $tkinspectvalues(blankimage)
	;# get around bug with hierarchy widget
    }
}

proc tkinspect:info_command {iw np} {
    global tkecl tkinspectvalues

    return [tkinspect:inspect_command $tkinspectvalues($iw,source) \
       [list info [tkinspect:Get_numentry tkecl(pref,inspect_prdepth) 6] $np] \
       {I[S*]}]
}

proc tkinspect:move_command {iw dir np} {
    global tkecl tkinspectvalues

    return [tkinspect:inspect_command $tkinspectvalues($iw,source) \
       [list movepath $dir [tkinspect:Get_numentry \
        tkinspectvalues($iw,movesize) 1] $np]  {()I[S*]}]
}

proc tkinspect:select_command {iw source} {
    global tkecl tkinspectvalues

    return [tkinspect:inspect_command $tkinspectvalues($iw,source) [list select $source] S]
}


proc tkinspect:inspect_command {source command type} {

    return [lindex [ec_rpcq \
    	[list inspect_command $source $command _] (S($type)_) tracer_tcl] 3]
}

proc tkinspect:Get_subterm_info {iw np subname sumname} {
    global tkinspectvalues tkecl
    upvar $subname subterm
    upvar $sumname summary

    foreach {subterm summary type arity} [tkinspect:info_command $iw $np] {break}
    return "$type $arity"
}



proc tkinspect:MayCentre {h np isopen} {
    global tkinspectvalues

    set iw $tkinspectvalues($h,iw)
    set current [tkinspect:CurrentSelection $h]
    if {![string match "" $current]} {
	;# for some reason, sometimes CurrentSelection does not return
	;# a correct selection...
	$h centreitem $current 0.05 0.85 0.0 0.75
    }
}

proc tkinspect:Display {h selected prevsel} {
    global tkinspectvalues tkecl

    set iw $tkinspectvalues($h,iw) 
    ;# need to get iw (inspect window) this way because proc. may be called
    ;# by hierarchy widget where iw is not one of the arguments.

    if {[lsearch $prevsel $selected] != -1} {return 0} 
    ;# return immediately if previously selected
    set tw $tkinspectvalues($h,twin)
    set current [tkinspect:CurrentSelection $h]

    set wake 0
    after 500 {set wake 1}
    vwait wake
    
    if {![string match "" $current]} {
	$h centreitem $current 0.05 0.9 0.1 0.8
	foreach {subterm summary type arity } \
		[tkinspect:info_command $iw $current] {break}
	if {[string length $subterm] >= $tkecl(pref,text_truncate)} {
	    set subterm [string range $subterm 0 $tkecl(pref,text_truncate)]
	    set truncated 1
	} else {
	    set truncated 0
	}

	if {[string match *compound $type]} {
	    if {[$h isopen $current]} {
		set printedterm $subterm
	    } else {
		set printedterm $summary
		set truncated 0
	    }
	} else {
	    set printedterm $subterm
	}
	$tw tag remove highlight 1.0 end
	$tw tag remove phighlight 1.0 end
	$tw insert end "\n\n"
	$tw insert end  $printedterm highlight
	if $truncated {
	    $tw insert end "..." truncated
	}
	$tw yview moveto 1.0

	return 1
    } else {
	return 0
    }

}



proc tkinspect:Get_numentry {tvar default} {
    upvar #0 $tvar numvar

    if {[string length $numvar] == 0} {set numvar $default}
    return $numvar
}

#------------------------------------------------------------



package require AllWidgets

proc tkinspect:ec_resume_inspect {} {
    ec_rpcq_check inspect_term () tracer_tcl
}


# center the child over the parent window
# (adapted from the wm man page)
proc tkinspect:center_over {child parent} {
    wm withdraw $child
    update
    set x [expr {max(0,[winfo x $parent]+([winfo width $parent]-[winfo width $child])/2)}]
    set y [expr {max(0,[winfo y $parent]+([winfo height $parent]-[winfo height $child])/2)}]
    wm geometry  $child +$x+$y
    wm transient $child $parent
    wm deiconify $child
}


proc tkinspect:EditDepth {iw type min max message typetext} {
    global tkecl 

    set w $iw.predit$type
    if {[winfo exists $w]} {
	tkinspect:RaiseWindow $w
    } else {
	set oldprdepth $tkecl(pref,inspect_$type)

	toplevel $w
	wm title $w "Change Inspector's $typetext"
	tkinspect:center_over $w $iw
	wm resizable $w 1 0
	message $w.m -justify center -aspect 1000 -text $message
	pack [frame $w.f] -expand 1 -fill both -side bottom
	pack [frame $w.f.b] -side bottom -expand 1 -fill both
	pack [label $w.f.label -text $typetext] -side left
	pack [ventry $w.f.e -vcmd {regexp {^([1-9][0-9]*|[1-9]?)$} %P} -validate key -invalidcmd bell -relief sunken -width 4 -textvariable tkecl(pref,inspect_$type)] -side left
	pack [scale $w.f.scale -from $min -to $max -orient horizontal \
		-tickinterval 10 -length 60m -sliderlength 4m \
		-variable tkecl(pref,inspect_$type)] -expand 1 -fill x
	pack [button $w.f.b.exit -command "destroy $w" -text "Done"] -side left -expand 1 -fill both
	pack [button $w.f.b.cancel -text "Cancel" -command "set tkecl(pref,inspect_$type) $oldprdepth ; destroy $w"] -side right -expand 1 -fill both
	pack $w.m -side top -expand 1 -fill both 
    }
}


proc tkinspect:Popnumentry {iw tvar default valname exclusive} {
    upvar #0 $tvar numvar

    if {[string match [grab current $iw] $iw]} {
	set hasgrab 1
    } else {
	set hasgrab 0
    }
    set origval $numvar
    set popup $iw.numpop$tvar ;# $tvar should be in a appendable format! 
    if {[winfo exists $popup]} {
	tkinspect:RaiseWindow $popup
	return
    }
    toplevel $popup
    wm title $popup "Change $valname"

    tkinspect:center_over $popup $iw

    if {$exclusive == 1} {
	tkwait visibility $popup
	grab $popup
    }
    tkinspect:Numentry $popup.entry {New Value: } $tvar $default 0 4 0 0 0
    ;# zero or positive numbers acceptable
    set getvalue "tkinspect:Get_numentry $tvar $default; destroy $popup"
    $popup config -borderwidth 5
    frame $popup.enter -borderwidth 2 -relief sunken

    pack [button $popup.enter.b -text "Finish" -command $getvalue] \
	    -padx 1 -pady 1
    grid $popup.enter -row 1 -column 0

    grid [button $popup.cancel -text "Cancel" -command "destroy $popup; \
	    set $tvar $origval"] -row 1 -column 2
    bind $popup.entry <Return> $getvalue

    if {$exclusive == 1} {
	tkwait window $popup
	if {$hasgrab} {grab $iw}
    }
}


proc tkinspect:DisplayKey {iw} {
    global tkinspectvalues

    if {[winfo exists $iw.inspectkey]} {
	tkinspect:RaiseWindow $iw.inspectkey
	return
    }
    set keywin [toplevel $iw.inspectkey] 
    wm title $keywin "Key to Symbols"
    set i 0
    foreach {imagename type} "\
    	    $tkinspectvalues(atomimage)		atom\
    	    $tkinspectvalues(handleimage)	handle\
	    $tkinspectvalues(numimage)		number\
	    $tkinspectvalues(stringimage)	string\
	    $tkinspectvalues(structureimage)	structure\
	    $tkinspectvalues(suspimage)		{sleeping suspension}\
	    $tkinspectvalues(schedimage)	{scheduled suspension}\
	    $tkinspectvalues(deadimage)		{dead suspension}\
	    $tkinspectvalues(varimage)		variable\
	    $tkinspectvalues(attrimage)		{attributed variable}" {
	grid [label $keywin.i$type -image $imagename] -row $i -column 0
	grid [label $keywin.$type -text $type -justify left] \
		-row $i -column 1 -sticky w
	incr i 1
    }
    grid [button $keywin.exit -text "Dismiss" \
	    -command "destroy $keywin"] -row $i -column 0 -columnspan 2 -sticky ew
    tkinspect:center_over $iw.inspectkey $iw
}


proc tkinspect:SelectCurrent {iw h t} {
    global tkinspectvalues

    tkinspect:PostSelect $iw $h $t current [tkinspect:select_command $iw current]

}


proc tkinspect:SelectInvoc {iw h t} {
    global tkinspectvalues

    tkinspect:Popnumentry $iw tkinspectvalues(invocnum) 1 "Goal invocation number" 1
    set source invoc($tkinspectvalues(invocnum))
    tkinspect:PostSelect $iw $h $t $source [tkinspect:select_command $iw $source]

}


proc tkinspect:PostSelect {iw h t source status} {
    global tkinspectvalues

    if {[string match $status "ok"]} {
	$t delete 1.0 end
	set hroot [$h index root]
	$h close $hroot
	set tkinspectvalues($iw,source) $source ;# set only after close!
	$h open $hroot  ;# update root term. Also consistent with initial display
	$h selection set $hroot
#	tkinspect:Display $h $hroot {}
    } else {
	$t delete 1.0 end
	$t insert end "Inspected term not changed due to invalid \
		specification." bold
	bell
    }
}



# Pass the last argument position back to Prolog to see if term name should be
# modified, e.g. to indicate attribute (another possibility is structure
# field names. This might be done in Tcl, but want to leave all interpretation
# of path position to Prolog
proc tkinspect:Modify_name {iw lastpos} {
    global tkinspectvalues

    if {[regexp {^[0-9]+$} $lastpos] == 0} {
	;# goto Prolog if not a pure number
	return [tkinspect:inspect_command $tkinspectvalues($iw,source) [list modify $lastpos] S]
    } else {
	return {}
    }
}


proc tkinspect:CurrentSelection {h} {
    ;# we only allow one selected item at a time. so get first element only
    return [lindex [$h get [$h curselection]] 0] 
}


proc tkinspect:DisplayPath   {h t} {
    global tkinspectvalues

    set iw $tkinspectvalues($h,iw) 
    set current [tkinspect:CurrentSelection $h]

    ;# do not remove highlight from previously displayed goal because that is
    ;# the selected goal for which this is the valid path
    $t insert end "\n"
    $t insert end "path:  "
    set path [$h get index $current]
    tkinspect:TkOutputPath [list $current] $t $iw
    $t yview moveto 1.0
}


# Raise window w
proc tkinspect:RaiseWindow {w} {
    if {![winfo exists $w]} {
	return 0
    } else {
	wm deiconify $w
	raise $w
	return 1
    }
}

proc tkinspect:TkOutputPath {rawpath tw iw} {
    global tkinspectvalues

    set path [lrange [lindex $rawpath 0] 1 end] ;# miss out top-level 1

    $tw insert end "{  " phighlight
    foreach pos $path {
	if [regexp {^[0-9]+$} $pos match] {
	    ;# A simple number, no need for special handling
	    $tw insert end $pos phighlight
	    $tw insert end "  "
	} else {
	    ;# let Prolog handle the appearance of special path positions
	    
	    $tw insert end [tkinspect:inspect_command \
                    $tkinspectvalues($iw,source) [list translate $pos] S] phighlight
	    $tw insert end "  "

	}
    }
    $tw insert end "}\n" phighlight

}


proc tkinspect:Deal_with_yscroll {ys iw h} {
    if {![tkinspect:RaiseWindow $ys]} {
	toplevel $ys
        wm resizable $ys 0 0
        wm title $ys "Inspector YScroll Control Panel "
	tkinspect:PackYS $iw $h $ys
    }
}

proc tkinspect:helpinfo {iw topic filename key} {
    global tkecl

    set w $iw.helpwindow$key
    if [tkinspect:RaiseWindow $w] {
	return
    }
    toplevel $w
    wm title $w "$topic Help"
    set t [text $w.t -relief groove -background lightgray -width 80 \
	    -yscrollcommand "$w.y set"]
    scrollbar $w.y -orient vert -command "$t yview"
    pack $w.y -side right -fill y
    pack [button $w.exit -text "Close" -command "destroy $w"] \
	    -side bottom -fill x
    pack $t -expand true -fill both
    bind $t <Any-Key> {break} ;# read only
    bind $t <ButtonRelease-2> {break}   ;# disable paste
    bind $t <Leave> {break}
    set file [file join $tkecl(ECLIPSEDIR) lib_tcl $filename]
    if {![file exists $file]} {
	$t insert end "Help file for $topic is missing"
    } else {
	set input [open $file r]
	$t insert end [read $input]
	close $input
    }

}

#--------------------------------------------------
# Menu display
#--------------------------------------------------

proc tkinspect:Popup_menu {iw h x y cx cy t} {
# x and y are root-window relative co-ordinates (for placing menu)
# cx and cy are widget-relative co-ordinates (for knowing where mouse is)
# t is the alert text message box
    global tkinspectvalues

    if [winfo exists $h.hpopup] {
	destroy $h.hpopup
    }

    set idx [$h index @$cx,$cy]
    set np [lindex [$h get $idx] 0]
    set lastargpos [lindex $np end]
    foreach {subterm summary type arity} [tkinspect:info_command $iw $np] {
	break
    }
    if {[string length $summary] >= $tkinspectvalues(summary_truncate)} {
	set summary [string range $summary 0 $tkinspectvalues(summary_truncate)]
	append summary ...
    }
    set posinfo [tkinspect:inspect_command $tkinspectvalues($iw,source) [list translate $lastargpos] S]

    set m [menu $h.hpopup -tearoff 0]
    $m add command -label "$summary" -state disabled
    $m add command -label "type: $type" -state disabled
    $m add command -label "arg pos: $posinfo" -state disabled
    $m add command -label "Observe this term" -command "tkinspect:Make_observed $iw {$np} $t"
    tk_popup $m $x $y
}

proc tkinspect:Make_observed {iw np t} {
    global tkinspectvalues

    toplevel $iw.o
    set tkinspectvalues($iw,obslabel) ""
    pack [label $iw.o.l -text "Please give a label for this observed term"] -side top
    pack [entry $iw.o.e -textvariable tkinspectvalues($iw,obslabel)] \
	    -side top -expand 1 -fill both
    bind $iw.o.e <Return> "destroy $iw.o"
    pack [button $iw.o.ok -text "OK" -command "destroy $iw.o"] \
	    -side left -expand 1 -fill x
    pack [button $iw.o.cancel -text "Cancel" -command "set tkinspectvalues($iw,obslabel) {Cancel Observed}; destroy $iw.o"]
    focus $iw.o.e
    balloonhelp $iw.o.l \
"If you are at a tracer debug port, a display matrix will be created with \n\
 this term which you have selected for observing."
    balloonhelp $iw.o.e "Enter the label you want to see next to the term in the display matrix"
    balloonhelp $iw.o.cancel "Click this button if you don't want to add the term for observation."
    tkwait window $iw.o

    $t delete 1.0 end
    if {$tkinspectvalues($iw,obslabel) != "Cancel Observed"} {
	tkinspect:inspect_command $tkinspectvalues($iw,source) \
          [list record_observed $tkinspectvalues($iw,source) $np \
           $tkinspectvalues($iw,obslabel)] {S[S*]S}

        $t insert end "Added term for observation with label $tkinspectvalues($iw,obslabel)" bold
    } else {
	$t insert end "Cancelled adding term for observation" bold
    }
}

