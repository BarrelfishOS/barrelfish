#! /bin/sh
# \
	exec wish8.0 $0 ${1+"$@"}

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
# Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
# 
# Contributor(s): IC-Parc, Imperial College London
# 
# END LICENSE BLOCK
#
# ECLiPSe Installation Script for Windows
#
# System:	ECLiPSe Constraint Logic Programming System
# Author/s:	Joachim Schimpf, IC-Parc
# Version:	$Id: ecl_inst.tcl,v 1.6 2013/07/05 01:34:47 jschimpf Exp $
#

set tkecl(scriptdir) [file dirname [info script]]
set tkecl(eclipseversion) "6.2"

# Path name syntax: Tcl commands can handle both native (backslash)
# or generic (unix-like) syntax. But when we give a path to Windows
# we should always convert it to native format.

# The following variables should contain native format because they
# are reflected in the installation window's entry boxes:
set tkecl(eclipsedir) [file nativename $tkecl(scriptdir)]
set tkecl(workdir) "C:\\"

switch $tcl_platform(platform) {
    windows {
	package require registry
    }
    default {
	error "$tcl_platform(platform) not supported"
	exit
    }
}

# set a registry entry (we delete the key first to avoid permission problems)
proc registry_set {key value data} {
    registry delete $key
    registry set $key $value $data
}

proc registry_set_version {} {
    global tkecl
    # make the registry entry for this version
    registry_set HKEY_LOCAL_MACHINE\\SOFTWARE\\IC-Parc\\Eclipse\\$tkecl(eclipseversion) \
	ECLIPSEDIR [file nativename $tkecl(eclipsedir)]
}


# Checks Windows registry for Java Runtime Environment entry. If JRE
# is present, and version is >= 1.2, adds the JRE location as an entry
# called JRE_HOME in Eclipse's registry area and return this location.

proc get_jre_home {} {
    global tkecl

    set noentry [catch {set jre_version [registry get "HKEY_LOCAL_MACHINE\\SOFTWARE\\JavaSoft\\Java Runtime Environment\\" CurrentVersion]}]

    if {!$noentry && $jre_version >= "1.2"} {
	set jre [registry get "HKEY_LOCAL_MACHINE\\SOFTWARE\\JavaSoft\\Java Runtime Environment\\$jre_version" JavaHome]

	# make the JRE_HOME registry entry.
	registry set HKEY_LOCAL_MACHINE\\SOFTWARE\\IC-Parc\\Eclipse\\$tkecl(eclipseversion) \
	    JRE_HOME [file nativename $jre]

	return $jre
    }
    return ""
}


# Process command-line arguments, if any.

if {$argc >= 1} {
    # Non-interactive: only do the bits specified on the command line.
    foreach arg $argv {
    	switch -- $arg {
	    --registry-eclipsedir {
		# Set the ECLIPSEDIR entry in the registry.
		registry_set_version
	    }
	    --create-jeclipse {
		# Create the JRE_HOME entry in the registry.
		get_jre_home
	    }
	    default: {
	    	error "Unknown option $arg"
		exit 1
	    }
	}
    }

    # We're done.
    exit
}

# We do this here, once we're sure we want to interact with the user.
set prev [pwd]
cd [file join $tkecl(scriptdir) lib_tcl]
set have_shortcuts [file readable shortcut.dll]
if {$have_shortcuts} {
    load shortcut.dll
}
cd $prev

wm title . "ECLiPSe $tkecl(eclipseversion) Installation"
wm iconname . ECLiPSe
set tkecl(ec_image) [image create photo -format ppm -file \
	[file join $tkecl(scriptdir) lib_tcl Images eclipse_logo.ppm]]

label .logo -image $tkecl(ec_image) -relief raised
label .q1 -text "This is ECLiPSe $tkecl(eclipseversion), located in folder:"
entry .e1 -width 40 -textvariable tkecl(eclipsedir)
label .q2 -text "Where do you want your working\ndirectory when ECLiPSe starts up:"
entry .e2 -width 40 -textvariable tkecl(workdir)
frame .buttons
button .buttons.install -text "Install/Reinstall" -command do_install
button .buttons.uninstall -text "Uninstall" -command do_uninstall
button .buttons.exit -text Cancel -command exit

pack .buttons.install .buttons.uninstall .buttons.exit -side left -expand 1
pack .logo -side left -padx 15 -pady 15
pack .q1 -side top -pady 5
pack .e1 -side top -padx 5
pack .q2 -side top -pady 5
pack .e2 -side top -padx 5
pack .buttons -side top -expand 1 -fill both -pady 5

update


proc do_install {} {
    global tkecl

    if [catch registry_set_version] {
	tk_messageBox -title "ECLiPSe Installation Problem" -icon error \
	    -message "You do not have permission to create registry entries" -type ok
	exit
    }

    # make the registry entries for the .ecl file class
    registry_set HKEY_CLASSES_ROOT\\.ecl {} \
    	ECLiPSeSource
    registry_set HKEY_CLASSES_ROOT\\ECLiPSeSource {} \
    	"ECLiPSe Source"
    registry_set HKEY_CLASSES_ROOT\\ECLiPSeSource\\shell\\Edit\\command {} \
    	{notepad "%1"}
    registry_set HKEY_CLASSES_ROOT\\ECLiPSeSource\\shell\\Compile\\command {} \
	"\"[file nativename [file join $tkecl(eclipsedir) lib i386_nt eclipse.exe]]\" -b \"%1\""

    # delete obsolete preferences to avoid warnings
    catch [list registry delete HKEY_CURRENT_USER\\Software\\IC-Parc\\ECLiPSe\\tkeclipserc sharedsize]

    # create the default working directory
    if [catch [list file mkdir $tkecl(workdir)] err] {
	tk_messageBox -title "ECLiPSe Installation Warning" \
	    -message "Can't create working directory\n[file nativename $tkecl(workdir)]\n($err)" -type ok
    }

    # create the jeclipse script (an alternative standalone eclipse)
    set jre_home [get_jre_home]

    if {$have_shortcuts} {

	# make the start menu entries
	set eclmenu [start_menu_folder 1]
	if {$eclmenu != ""} {
	    set msg "Installation complete\nECLiPSe is now available from your Start menu"
	} else {
	    # if not possible, create a local menu directory
	    set eclmenu [file join $tkecl(eclipsedir) Menu]
	    file mkdir $eclmenu
	    set msg "Installation complete\nECLiPSe menu is in [file nativename $eclmenu]\n(Sorry, couldn't create Start menu entry)"
	}

	# cd to the menu directory because the shortcut command has problems
	# when the link-name contains non-ascii characters
	cd $eclmenu

# The following works, but is not too useful and probably just confusing people
#        if {$jre_home != ""} {
#	    shortcut create JEclipse.lnk \
#	        -workingDirectory [file nativename $tkecl(workdir)] \
#	        -objectPath [file nativename [file join $jre_home bin java]] \
#	        -arguments "-Declipse.directory=\"[file nativename $tkecl(eclipsedir)]\" -classpath \"[file nativename [file join $tkecl(eclipsedir) lib eclipse.jar]]\" com.parctechnologies.eclipse.JEclipse"
#        }
	shortcut create DosEclipse.lnk \
	    -workingDirectory [file nativename $tkecl(workdir)] \
	    -objectPath [file nativename [file join $tkecl(eclipsedir) lib i386_nt eclipse.exe]]
	shortcut create TkEclipse.lnk \
	    -workingDirectory [file nativename $tkecl(workdir)] \
	    -objectPath [file nativename [info nameofexecutable]] \
	    -arguments \"[file nativename [file join $tkecl(eclipsedir) lib_tcl tkeclipse.tcl]]\"
	shortcut create TkRemoteTools.lnk \
	    -workingDirectory [file nativename $tkecl(workdir)] \
	    -objectPath [file nativename [info nameofexecutable]] \
	    -arguments \"[file nativename [file join $tkecl(eclipsedir) lib_tcl tktools.tcl]]\"
	shortcut create Documentation.lnk \
	    -objectPath [file nativename [file join $tkecl(eclipsedir) doc index.html]]
	shortcut create Readme.lnk \
	    -objectPath [file nativename [file join $tkecl(eclipsedir) README_WIN.TXT]]
	shortcut create "Uninstall ECLiPSe $tkecl(eclipseversion).lnk" \
	    -workingDirectory [file nativename $tkecl(eclipsedir)] \
	    -objectPath [file nativename [info nameofexecutable]] \
	    -arguments \"[file nativename [file join $tkecl(eclipsedir) ecl_inst.tcl]]\"

    } else {
	set msg "Installation complete, but could not create Start menu"
    }

    tk_messageBox -title "ECLiPSe Installation" \
	-message $msg -type ok

    exit
}

proc do_uninstall {} {
    global tkecl

    # delete the entry for this version
    registry delete HKEY_LOCAL_MACHINE\\SOFTWARE\\IC-Parc\\Eclipse\\$tkecl(eclipseversion)

    # if no other versions exist, delete also the .ecl class entry
    set otherversions [registry keys HKEY_LOCAL_MACHINE\\SOFTWARE\\IC-Parc\\Eclipse]
    if {$otherversions == {}} {
	registry delete HKEY_CLASSES_ROOT\\.ecl
	registry delete HKEY_CLASSES_ROOT\\ECLiPSeSource
    }

    # delete the start menu entry
    start_menu_folder 0

    tk_messageBox -title "ECLiPSe Installation" \
	-message "Uninstallation complete" -type ok
    exit
}

# create or delete the start menu folder in one of the appropriate locations
# returns the name of the created/deleted folder
proc start_menu_folder {create} {
    global env tcl_platform tkecl

    set f "ECLiPSe $tkecl(eclipseversion)"
    set noentry [catch {set d [registry get "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders" "Common Programs"]}]
    if {! $noentry && [usable_dir $d $f $create]} { return [file join $d $f] }

    set noentry [catch {set d [registry get "HKEY_CURRENT_USER\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders" "Programs"]}]
    if {! $noentry && [usable_dir $d $f $create]} { return [file join $d $f] }

    if [info exists env(WINDIR)] {
	set windir $env(WINDIR)
    } else {
	set windir $env(windir)
    }
    
    foreach d [list [file join $windir "Start Menu" Programs] \
	    [file join $windir Profiles "All Users" "Start Menu" Programs] \
	    [file join $windir Profiles $tcl_platform(user) "Start Menu" Programs] \
	    ] {
	if [usable_dir $d $f $create] {
	    return [file join $d $f]
	}
    }
    return {}
}

# try to create/delete folder $f in folder $d
# return 1 on success, 0 on failure
proc usable_dir {d f create} {
    if {[file isdirectory $d]} {
	set eclmenu [file join $d $f]
	if {$create} {
	    set err [catch [list file mkdir $eclmenu] ]
	    if {$err} { return 0 }
	    return 1
	} else {
	    if [file isdirectory $eclmenu] {
		file delete -force $eclmenu
		return 1
	    }
	    return 0
	}
    }
    return 0
}


