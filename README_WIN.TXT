----------------------------------------------
ECLiPSe - Constraint Logic Programming System
----------------------------------------------

-----------------------------------------------------------------------------
ECLiPSe on Windows (see README_UNIX for UNIX/Linux, README_MACOSX for MacOSX)
-----------------------------------------------------------------------------

*** These instructions are for installing a binary distribution!
*** If you want to compile from source, see the file INSTALL,
*** and document/internal/SetupGuide.pdf for more detailed help. 

ECLiPSe for Windows should be run on Windows XP/Vista/7 or newer;
earlier versions may have restricted functionality.

For 32-bit Windows use the "i386_nt" distribution of ECLiPSe.
For 64-bit Windows use the "x86_64_nt" distribution of ECLiPSe.


--------------------------------------
Using the Windows Installer
--------------------------------------

For the most common Windows install configurations, use the ECLiPSe
Windows Installer. To do so, download and execute the single file

	ECLiPSe<Version>_<Build>.exe

Run this installer program as a user with "administrator" rights.

The installer contains the ECLiPSe kernel, basic libraries,
and the following optional packages:

- Documentation (html, txt, pdf)
- An interface to COIN-OR's open source solvers CLP and CBC
- An interface to a version of Dash Optimization's XPRESS-MP solver
- 3rd party libraries
- A bundled standalone distribution of GraphViz
  (to support the visualisation tools)
- A bundled (but not standalone) distribution of Tcl/Tk
  (to support the TkECLiPSE GUI)

For installation, double click on ECLiPSe<Version>_<Build>.exe and
the install wizard will guide you through the installation.

Then try to launch tkeclipse via the Start menu, or look at the
documentation, e.g. the tutorial.


---------------------------------------------------
Alternative: Manual Installation from .tgz Packages
---------------------------------------------------

You may want to use this method if you want to install ECLiPSe in a
non-standard location.

You need to get the following files for the "i386_nt" or the "x86_64_nt"
architecture. These are tar-files compressed with gzip.  Note that
these .tgz Packages for Windows may not be available for download via
SourceForge, but they are available from the www.eclipseclp.org site.

Base:
    README_WIN.TXT		The file you are reading now
    eclipse_basic.tgz        	Kernel and basic libraries
    eclipse_doc.tgz		Documentation (html,txt,man,pdf)

Optional:
    eclipse_misc.tgz		Free/3rd party libraries
    if_osiclpcbc.tgz            Interface to COIN-OR's open source solvers 
                                CLP and CBC

Support:
    tcltk.tgz			A matching Tcl/Tk release (8.5)
    				(you may have that already).
				Needed for the tkeclipse development GUI.
    editors_eclipse_support.tgz Support for various editors for editing
                                ECLiPSe code.  

These other files are only needed under special circumstances:
    eclipse_rt.tgz		Runtime only (subset of eclipse_basic.tgz)
    if_xpressXY.tgz		Interface to XPRESS-MP version XY

You can make installations (with reduced functionality) from subsets of
these files. The minimum you always need is either eclipse_basic (for a
development system) or eclipse_rt (for embedded applications only).



Manual Installation steps

1.  Make a folder for ECLiPSe on your machine and move the
    distribution files there.

    In the following we will refer to this folder as <eclipsedir>. 
    Note that you can have different versions of ECLiPSe installed
    at the same time (in different folders).

    The ECLiPSe installation folder can also be shared between
    machines even with different hardware/operating system
    architectures, e.g. i386_nt and sparc_sunos5.  To do so, just
    unpack the distributions for the different architectures in the
    same installation directory.  All architecture-dependent files
    will go in separate subdirectories.


2.  Unpack the distribution files in this folder

    You can use a tool like WinZip, GNU tar/gzip etc.
    Beware of your download or unpacking tool trying to do clever
    modifications of the file contents!  E.g. with WinZip be sure to
    disable Options->Configurations->Smart Tarfile CR/LF Conversion.

    After unpacking, you should have the ECLiPSe directory structure
    inside the <eclipsedir> folder.

    If you are on  Windows-9X (not NT/XP), you need to make sure you have
    the 3 system DLLs WS2HELP.DLL, WS2_32.DLL (Winsock2) and MSVCRT.DLL.


3.  Make a registry entry for ECLiPSe. You can do this by running
    <eclipsedir>/ecl_inst.js

    Alternatively, you can do this manually, by


    - run regedit (Start->Run...->regedit)

    - create the registry key
      HKEY_LOCAL_MACHINE\SOFTWARE\IC-Parc\ECLiPSe\<version>
      where <version> is e.g. 5.10

    - create a "String Value" under this key, with name ECLIPSEDIR and
      value <eclipsedir>, i.e. the folder where you have unpacked the
      distribution, e.g. "C:\Program Files\ECLiPSe 5.10"

    You should now be able to run the command line version of eclipse, i.e.

	<eclipsedir>/lib/i386_nt/eclipse.exe


4.  If you have a regular installation of Tcl/Tk, you can now also execute
    the tkeclipse development environment by simply double-clicking on

	<eclipsedir>/lib_tcl/tkeclipse.tcl

    If, however, you have only unpacked the Tcl/Tk that is bundled with
    ECLiPSe, then this will not work, since no bindings for the .tcl
    extension have been defined.  You can still execute tkeclipse.tcl
    (or the tcl examples) by either

     - dragging them onto the wish-executable, which is
	<eclipsedir>\tcltk\i386_nt\bin\wish<version>.exe

     - by running the wish-executable with the .tcl script as its
       command line argument, e.g.
       <eclipsedir>\tcltk\i386_nt\bin\wish<version>.exe tkeclipse.tcl

     - create a shortcut that invokes the wish-executable with the .tcl
       script as its argument

    where wish<version>.exe is the actual name of the wish application, e.g.
    wish85.exe. See the debugdemo.ecl example for how to do this from ECLiPSe


5.  Some ECLiPSe components, e.g. the Visualisation Tools, need Java.
    In order for them to find the Java installation, you need to create
    a further registry entry.  Under the the registry key
      HKEY_LOCAL_MACHINE\SOFTWARE\IC-Parc\ECLiPSe\<version>
    create a "String Value" with name JRE_HOME and set it to the folder
    where the Java runtime environemnt is located, e.g.
    C:\Program Files\Java\jre1.5.0



----------------------------------------------
General Topics
----------------------------------------------

Directory structure
-------------------

Directories in the binary distribution:

    lib			supported libraries, object files, executables
    lib_public		third party Prolog libraries
    lib_tcl		tcl files for the graphical interface
    include		include files for C/C++ interface
    doc			online documentation, pdf files and example code
    tcltk		the bundled Tcl/Tk distribution


The important files are in <eclipsedir>\lib\i386_nt:

    eclipse.exe		ECLiPSe standalone executable (needs eclipse.dll)
    eclipse.dll		ECLiPSe linkable library
    eclipse.lib		ECLiPSe linkable library stubs
    eclipse.def		Export specification of the dll
    eclipse.reg		Registry file for use with regedit

To compile an application using the eclipse.dll you will need the
include files in <eclipsedir>\include\i386_nt.

The files related to the GUI version are in <eclipsedir>\lib_tcl:

    tkeclipse.tcl	ECLiPSe development GUI (executable)
    eclipse.tcl		eclipse package for Tcl/Tk-embedding
    eclipse_tools.tcl	eclipse_tools package with support tools
    example*.tcl	examples for using eclipse from Tcl/Tk

Code for embedding into Visual Basic is in <eclipsedir>\doc\examples.



Differences between UNIX and Windows versions of ECLiPSe
--------------------------------------------------------

Pathnames: Eclipse internally always works with platform independent
path names using forward slashes, e.g.

Windows pathname:	C:\Software\Eclipse
ECLiPSe form:		//C/Software/Eclipse


Documentation
-------------

The installation subdirectory <eclipsedir>/doc contains html and pdf
versions of the user documentation. To read the html documentation,
set your browser to e.g.

    file:<eclipsedir>/doc/index.html


The printable documentation is contained in the files:

    tutorial.pdf	Tutorial

    userman.pdf		Basic user manual

    libman.pdf		Library user manual (constraints libraries)

    embedding.pdf	Interfacing and embedding manual

The Reference Manual (the detailed descriptions of the built-in
predicates and all the libraries) is available exclusively in html
format and via the help/1 predicate from within ECLiPSe.


Installation hints for the 'eplex' library
----------------------------------------------

The ECLiPSe 'eplex' library is an interface to an external LP/MIP solver,
which can be either a free COIN-OR solver (www.coin-or.org), or a
commercial Xpress-MP or CPLEX solver.

For the COIN-OR solvers, you do not need a licence to use them. For 
Xpress-MP or CPLEX, you need to own a separate Xpress-MP or CPLEX licence
to use the 'eplex' library with the corresponding solver. In this case, you
need to download a version of 'eplex' that matches the version of the
Xpress-MP licence that you have (although it may be possible to
run an older version with a newer licence). For example, for Xpress-MP X.Y,
you need the package if_xpressXY.tgz. You can install a mix of different
versions by just unpacking multiple packages.

Note that we no longer distribute the CPLEX solver for eplex in binary 
form, as the CPLEX library is statically linked. You need to compile 
ECLiPSe from source if you want to use CPLEX with the `eplex' library.

To tell ECLiPSe which licence you have on which machine, add the
necessary lines to the text file <eclipsedir>/lib/eplex_lic_info.ecl,
according to the instructions provided there.  With this setup, you
can load the eplex library from ECLiPSe by just invoking lib(eplex),
and you will get the correct version for each machine.

Academic licences for CPLEX are available from ILOG at
    http://www.ilog.com/partners/academic/license.cfm
Academic licences for XPRESS-MP are available from Dash Optimization at
    http://www.dashoptimization.com/home/users/app.html


Installation hints for the 'dbi' library
-----------------------------------------

The ECLiPSe 'dbi' library is an interface to an external
database. Currently the MySQL database is supported. Because of licensing
issues, we do not include the MySQL dynamic load library (libmysql.dll) in
our binary distributions. You will need to obtain this library from
MySQL AB:

    http://dev.mysql.com/downloads

When 'dbi' is loaded (for example by lib(dbi)), it will need to find and
load this MySQL dynamic library. If it is in one of the standard system
library paths, then the library can be loaded without further
action. Otherwise, you can copy libmysql.dll file to where your ECLiPSe
dynamic libraries are, i.e. <eclipsedir>/lib/i386_nt. 

You need to be able to connect to a running MySQL server in order to use
the library. This can be on your local machine, or on a remote machine
reachable from your network.


Graphics
--------

The Embedding Interface and the TkEclipse Development Environment
require Tcl/Tk version 8.3 or later.
This might already be installed on your machine, otherwise install
it by executing tcl831.exe.

Several visualisation features require Java. The recommended version
is at least 1.4, but versions from 1.2 upwards should still work.


Emacs ECLiPSe mode
------------------

If you use the GNU emacs editor, an ECLiPSe mode is now provided for it.
It is not part of the ECLiPSe distribution, and you need to download it
separately. **Version 20 (or later) of GNU emacs is required.** 
This mode is based on the prolog mode by Umeda, and provides extensive 
syntax colouring  for ECLiPSe-specific features. See http://www.gnu.org/ 
if you need more information on GNU and its software. 

In order to use this mode, add the following line to your .emacs file:

(autoload 'eclipse-mode "<eclipsedir>/lib_public/eclipse.el" "ECLIPSE editing mode" t)

where <eclipsedir> is the path to your ECLiPSe installation directory.

You may also need to add these lines so that the .pl and .ecl extensions
are recognised to be ECLiPSe programs:

(setq auto-mode-alist (cons '("\\.pl" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ecl" . eclipse-mode) auto-mode-alist))

The eclipse.el file needs to be placed somewhere where your emacs can find
it.

See the start of the eclipse.el file for some instructions on using this mode.


Contact Addresses
-----------------

News about ECLiPSe are available from the following sources

    http://www.eclipseclp.org

    http://www.sourceforge.net/projects/eclipse-clp

    comp.constraints    (via news server or e.g. groups.google.com)

Via http://www.eclipseclp.org/mailinglist.html you can subscribe to
one of the ECLiPSe mailing lists

    eclipse-clp-announce@lists.sf.net	(release announcements)
    eclipse-clp-users@lists.sf.net	(user discussion)

Problem and bug reports can be submitted via the web site

    http://www.eclipseclp.org/bugs.html

Please take the time to report problems. Your feedback helps us to
make ECLiPSe more reliable!

