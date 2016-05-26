# Tcl package index file, version 1.0

package ifneeded AllWidgets 1.0 {
    package require Widget
    package require BalloonHelp
    package require Calculator
    package require Combobox
    package require Console
    package require Hierarchy
    package require Megalist
    package require Pane
    package require Progressbar
    package require Tabnotebook
    package require Ventry
    package provide AllWidgets 1.0
}

package ifneeded BalloonHelp 2.0 [list tclPkgSetup $dir BalloonHelp 2.0 {
    {balloonhelp.tcl source {
	balloonhelp
}   }   }]

## Not ready yet
package ifneeded Calculator 1.0 [list tclPkgSetup $dir Calculator 1.0 {
    {calculator.tcl source {
	Calculator
}}}]

package ifneeded Combobox 2.0 [list tclPkgSetup $dir Combobox 2.0 {
    {combobox.tcl source {
	Combobox combobox
}   }   }]

package ifneeded Console 2.0 [list tclPkgSetup $dir Console 2.0 {
    {console.tcl source {
	Console ConsoleDialog console consoledialog
}   }   }]

package ifneeded Hierarchy 2.1 [list tclPkgSetup $dir Hierarchy 2.1 {
    {hierarchy.tcl source {
	Hierarchy hierarchy hierarchy_dir hierarchy_widget
}   }   }]

package ifneeded Megalist 1.0 [list tclPkgSetup $dir Megalist 1.0 {
    {megalist.tcl source {
	megalist
}   }   }]

package ifneeded Pane 1.0 [list tclPkgSetup $dir Pane 1.0 {
    {pane.tcl source {
	pane
}   }   }]

package ifneeded Progressbar 2.0 [list tclPkgSetup $dir Progressbar 2.0 {
    {progressbar.tcl source {
	Progressbar progressbar
}   }   }]

package ifneeded Tabnotebook 2.0 [list tclPkgSetup $dir Tabnotebook 2.0 {
    {tabnotebook.tcl source {
	Tabnotebook tabnotebook
}   }   }]

package ifneeded Ventry 2.0 [list tclPkgSetup $dir Ventry 2.0 {
    {ventry.tcl source {
	Ventry ventry
}   }   }]

package ifneeded Widget 2.0 [list tclPkgSetup $dir Widget 2.0 {
    {widget.tcl source {
	widget scrolledtext ScrolledText
}   }   }]

package ifneeded ::Utility 1.0 [list source [
    subst {[file join $dir util.tcl]}
]]
#	get_opts get_opts2 randrng best_match grep
#	lremove lrandomize lunique luniqueo
#	line_append echo alias which ls dir validate fit_format

package ifneeded ::Utility::dump 1.0 [list source [
    subst {[file join $dir util-dump.tcl]}
]]
#	dump

package ifneeded ::Utility::string 1.0 [list source [
    subst {[file join $dir util-string.tcl]}
]]
#	string_reverse obfuscate untabify tabify wrap_lines

package ifneeded ::Utility::number 1.0 [list source [
    subst {[file join $dir util-number.tcl]}
]]
#	get_square_size roman2dec bin2hex hex2bin

package ifneeded ::Utility::tk 1.0 [list source [
    subst {[file join $dir util-tk.tcl]}
]]
#	warn place_window canvas_center canvas_see

package ifneeded ::Utility::expand 1.0 [list source [
    subst {[file join $dir util-expand.tcl]}
]]
#	expand
