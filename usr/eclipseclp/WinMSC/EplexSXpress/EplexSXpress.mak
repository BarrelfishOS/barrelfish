# Microsoft Developer Studio Generated NMAKE File, Based on EplexSXpress.dsp
!IF "$(CFG)" == ""
CFG=EplexSXpress - Win32 Debug14
!MESSAGE No configuration specified. Defaulting to EplexSXpress - Win32\
 Debug14.
!ENDIF 

!IF "$(CFG)" != "EplexSXpress - Win32 Release13" && "$(CFG)" !=\
 "EplexSXpress - Win32 Debug13" && "$(CFG)" !=\
 "EplexSXpress - Win32 Release13icp" && "$(CFG)" !=\
 "EplexSXpress - Win32 Debug13icp" && "$(CFG)" !=\
 "EplexSXpress - Win32 Debug14icp" && "$(CFG)" != "EplexSXpress - Win32 Debug14"\
 && "$(CFG)" != "EplexSXpress - Win32 Release14icp" && "$(CFG)" !=\
 "EplexSXpress - Win32 Release14" && "$(CFG)" != "EplexSXpress - Win32 Debug15"\
 && "$(CFG)" != "EplexSXpress - Win32 Release15"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "EplexSXpress.mak" CFG="EplexSXpress - Win32 Debug14"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "EplexSXpress - Win32 Release13" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Debug13" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Release13icp" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Debug13icp" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Debug14icp" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Debug14" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Release14icp" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Release14" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Debug15" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Release15" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "EplexSXpress - Win32 Release13"

OUTDIR=.\Release13
INTDIR=.\Release13
# Begin Custom Macros
OutDir=.\Release13
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ELSE 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\EplexSXpress.dll"
	-@erase "$(OUTDIR)\EplexSXpress.exp"
	-@erase "$(OUTDIR)\EplexSXpress.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/xosl1326/i386_nt/include" /D "NDEBUG" /D XPRESS=13 /D\
 XPRESSMINOR=26 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__"\
 /Fp"$(INTDIR)\EplexSXpress.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Release13/
CPP_SBRS=.
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSXpress.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=xprs1326.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)\EplexSXpress.pdb" /machine:I386\
 /def:"..\..\icparc_solvers\seplex_xpress.def" /out:"$(OUTDIR)\EplexSXpress.dll"\
 /implib:"$(OUTDIR)\EplexSXpress.lib" /libpath:"../Eclipse/Release"\
 /libpath:"M:/Eclipse/xosl1326/i386_nt/lib" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_xpress.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj"

"$(OUTDIR)\EplexSXpress.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Release13
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\EplexSXpress.dll"
   if not exist ..\..\lib\i386_nt\express1326 mkdir ..\..\lib\i386_nt\express1326
	copy          M:\Eclipse\xosl1326\i386_nt\bin\xprs.dll                ..\..\lib\i386_nt\express1326\xprs.dll
	copy          M:\Eclipse\xosl13\lic_default\i386_nt\xpress.pwd               ..\..\lib\i386_nt\express1326\xpress.pwd
	copy Release13\EplexSXpress.dll                ..\..\lib\i386_nt\sexpress1326.dll
	copy Release13\EplexSXpress.dll               ..\..\icparc_solvers\i386_nt\sexpress1326.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Debug13"

OUTDIR=.\Debug13
INTDIR=.\Debug13
# Begin Custom Macros
OutDir=.\Debug13
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ELSE 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\EplexSXpress.dll"
	-@erase "$(OUTDIR)\EplexSXpress.exp"
	-@erase "$(OUTDIR)\EplexSXpress.ilk"
	-@erase "$(OUTDIR)\EplexSXpress.lib"
	-@erase "$(OUTDIR)\EplexSXpress.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/xosl1326/i386_nt/include" /D "_DEBUG" /D XPRESS=13 /D\
 XPRESSMINOR=26 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__"\
 /Fp"$(INTDIR)\EplexSXpress.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Debug13/
CPP_SBRS=.
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSXpress.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=xprs1326.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:yes /pdb:"$(OUTDIR)\EplexSXpress.pdb" /debug /machine:I386\
 /def:"..\..\icparc_solvers\seplex_xpress.def" /out:"$(OUTDIR)\EplexSXpress.dll"\
 /implib:"$(OUTDIR)\EplexSXpress.lib" /pdbtype:sept /libpath:"../Eclipse/Debug"\
 /libpath:"M:/Eclipse/xosl1326/i386_nt/lib" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_xpress.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj"

"$(OUTDIR)\EplexSXpress.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Debug13
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\EplexSXpress.dll"
   if not exist ..\..\lib\i386_nt\express1326 mkdir ..\..\lib\i386_nt\express1326
	copy          M:\Eclipse\xosl1326\i386_nt\bin\xprs.dll                ..\..\lib\i386_nt\express1326\xprs.dll
	copy          M:\Eclipse\xosl13\lic_default\i386_nt\xpress.pwd                ..\..\lib\i386_nt\express1326\xpress.pwd
	copy Debug13\EplexSXpress.dll                ..\..\lib\i386_nt\sexpress1326.dll
	copy Debug13\EplexSXpress.dll               ..\..\icparc_solvers\i386_nt\sexpress1326.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Release13icp"

OUTDIR=.\Release13icp
INTDIR=.\Release13icp
# Begin Custom Macros
OutDir=.\Release13icp
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ELSE 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\EplexSXpress.dll"
	-@erase "$(OUTDIR)\EplexSXpress.exp"
	-@erase "$(OUTDIR)\EplexSXpress.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/xosl1326icp/i386_nt/include" /D "NDEBUG" /D XPRESS=13 /D\
 XPRESSMINOR=26 /D "XPRESS_OEM_ICPARC_2002" /D "WIN32" /D "_WINDOWS" /D "DLL" /D\
 "__STDC__" /Fp"$(INTDIR)\EplexSXpress.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Release13icp/
CPP_SBRS=.
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSXpress.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=xprs1326icp.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)\EplexSXpress.pdb" /machine:I386\
 /def:"..\..\icparc_solvers\seplex_xpress.def" /out:"$(OUTDIR)\EplexSXpress.dll"\
 /implib:"$(OUTDIR)\EplexSXpress.lib" /libpath:"../Eclipse/Release"\
 /libpath:"M:/Eclipse/xosl1326icp/i386_nt/lib" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_xpress.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj"

"$(OUTDIR)\EplexSXpress.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Release13icp
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\EplexSXpress.dll"
   if not exist ..\..\lib\i386_nt\express1326icp mkdir ..\..\lib\i386_nt\express1326icp
	copy          M:\Eclipse\xosl1326icp\i386_nt\bin\xprs.dll                ..\..\lib\i386_nt\express1326icp\xprs.dll
	copy          M:\Eclipse\xosl13\lic_icp\i386_nt\xpress.pwd               ..\..\lib\i386_nt\express1326icp\xpress.pwd
	copy Release13icp\EplexSXpress.dll                ..\..\lib\i386_nt\sexpress1326icp.dll
	copy Release13icp\EplexSXpress.dll               ..\..\icparc_solvers\i386_nt\sexpress1326icp.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Debug13icp"

OUTDIR=.\Debug13icp
INTDIR=.\Debug13icp
# Begin Custom Macros
OutDir=.\Debug13icp
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ELSE 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\EplexSXpress.dll"
	-@erase "$(OUTDIR)\EplexSXpress.exp"
	-@erase "$(OUTDIR)\EplexSXpress.ilk"
	-@erase "$(OUTDIR)\EplexSXpress.lib"
	-@erase "$(OUTDIR)\EplexSXpress.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/xosl1326icp/i386_nt/include" /D "_DEBUG" /D XPRESS=13 /D\
 XPRESSMINOR=26 /D "XPRESS_OEM_ICPARC_2002" /D "WIN32" /D "_WINDOWS" /D "DLL" /D\
 "__STDC__" /Fp"$(INTDIR)\EplexSXpress.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Debug13icp/
CPP_SBRS=.
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSXpress.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=xprs1326icp.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:yes /pdb:"$(OUTDIR)\EplexSXpress.pdb" /debug /machine:I386\
 /def:"..\..\icparc_solvers\seplex_xpress.def" /out:"$(OUTDIR)\EplexSXpress.dll"\
 /implib:"$(OUTDIR)\EplexSXpress.lib" /pdbtype:sept /libpath:"../Eclipse/Debug"\
 /libpath:"M:/Eclipse/xosl1326icp/i386_nt/lib" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_xpress.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj"

"$(OUTDIR)\EplexSXpress.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Debug13icp
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\EplexSXpress.dll"
   if not exist ..\..\lib\i386_nt\express1326icp mkdir ..\..\lib\i386_nt\express1326icp
	copy          M:\Eclipse\xosl1326icp\i386_nt\bin\xprs.dll                ..\..\lib\i386_nt\express1326icp\xprs.dll
	copy          M:\Eclipse\xosl13\lic_icp\i386_nt\xpress.pwd                ..\..\lib\i386_nt\express1326icp\xpress.pwd
	copy Debug13icp\EplexSXpress.dll                ..\..\lib\i386_nt\sexpress1326icp.dll
	copy Debug13icp\EplexSXpress.dll               ..\..\icparc_solvers\i386_nt\sexpress1326icp.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Debug14icp"

OUTDIR=.\Debug14icp
INTDIR=.\Debug14icp
# Begin Custom Macros
OutDir=.\Debug14icp
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ELSE 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\EplexSXpress.dll"
	-@erase "$(OUTDIR)\EplexSXpress.exp"
	-@erase "$(OUTDIR)\EplexSXpress.ilk"
	-@erase "$(OUTDIR)\EplexSXpress.lib"
	-@erase "$(OUTDIR)\EplexSXpress.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/xosl1427/i386_nt/include" /D "_DEBUG" /D XPRESS=14 /D\
 XPRESSMINOR=27 /D "XPRESS_OEM_ICPARC_2002" /D "WIN32" /D "_WINDOWS" /D "DLL" /D\
 "__STDC__" /Fp"$(INTDIR)\EplexSXpress.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Debug14icp/
CPP_SBRS=.
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSXpress.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=xprs.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:yes /pdb:"$(OUTDIR)\EplexSXpress.pdb" /debug /machine:I386\
 /def:"..\..\icparc_solvers\seplex_xpress.def" /out:"$(OUTDIR)\EplexSXpress.dll"\
 /implib:"$(OUTDIR)\EplexSXpress.lib" /pdbtype:sept /libpath:"../Eclipse/Debug"\
 /libpath:"M:/Eclipse/xosl1427/i386_nt/lib" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_xpress.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj"

"$(OUTDIR)\EplexSXpress.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Debug14icp
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\EplexSXpress.dll"
   if not exist ..\..\lib\i386_nt\express1427icp mkdir ..\..\lib\i386_nt\express1427icp
	copy          M:\Eclipse\xosl1427\i386_nt\bin\xprs.dll                ..\..\lib\i386_nt\express1427icp\xprs.dll
	copy          M:\Eclipse\xosl1427\i386_nt\bin\xprl.dll                ..\..\lib\i386_nt\express1427icp\xprl.dll
	copy          M:\Eclipse\xosl14\lic_icp\xpress.lic                ..\..\lib\i386_nt\express1427icp\xpress.lic
	copy Debug14icp\EplexSXpress.dll                ..\..\lib\i386_nt\sexpress1427icp.dll
	copy Debug14icp\EplexSXpress.dll               ..\..\icparc_solvers\i386_nt\sexpress1427icp.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Debug14"

OUTDIR=.\Debug14
INTDIR=.\Debug14
# Begin Custom Macros
OutDir=.\Debug14
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ELSE 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\EplexSXpress.dll"
	-@erase "$(OUTDIR)\EplexSXpress.exp"
	-@erase "$(OUTDIR)\EplexSXpress.ilk"
	-@erase "$(OUTDIR)\EplexSXpress.lib"
	-@erase "$(OUTDIR)\EplexSXpress.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/xosl1427/i386_nt/include" /D "_DEBUG" /D XPRESS=14 /D\
 XPRESSMINOR=27 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__"\
 /Fp"$(INTDIR)\EplexSXpress.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Debug14/
CPP_SBRS=.
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSXpress.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=xprs.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:yes /pdb:"$(OUTDIR)\EplexSXpress.pdb" /debug /machine:I386\
 /def:"..\..\icparc_solvers\seplex_xpress.def" /out:"$(OUTDIR)\EplexSXpress.dll"\
 /implib:"$(OUTDIR)\EplexSXpress.lib" /pdbtype:sept /libpath:"../Eclipse/Debug"\
 /libpath:"M:/Eclipse/xosl1427/i386_nt/lib" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_xpress.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj"

"$(OUTDIR)\EplexSXpress.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Debug14
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\EplexSXpress.dll"
   if not exist ..\..\lib\i386_nt\express1427 mkdir ..\..\lib\i386_nt\express1427
	copy          M:\Eclipse\xosl1427\i386_nt\bin\xprs.dll                ..\..\lib\i386_nt\express1427\xprs.dll
	copy          M:\Eclipse\xosl1427\i386_nt\bin\xprl.dll                ..\..\lib\i386_nt\express1427\xprl.dll
	copy          M:\Eclipse\xosl14\lic\i386_nt\xpress.lic                ..\..\lib\i386_nt\express1427\xpress.lic
	copy Debug14\EplexSXpress.dll                ..\..\lib\i386_nt\sexpress1427.dll
	copy Debug14\EplexSXpress.dll               ..\..\icparc_solvers\i386_nt\sexpress1427.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Release14icp"

OUTDIR=.\Release14icp
INTDIR=.\Release14icp
# Begin Custom Macros
OutDir=.\Release14icp
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ELSE 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\EplexSXpress.dll"
	-@erase "$(OUTDIR)\EplexSXpress.exp"
	-@erase "$(OUTDIR)\EplexSXpress.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/xosl1427/i386_nt/include" /D "NDEBUG" /D XPRESS=14 /D\
 XPRESSMINOR=27 /D "XPRESS_OEM_ICPARC_2002" /D "WIN32" /D "_WINDOWS" /D "DLL" /D\
 "__STDC__" /Fp"$(INTDIR)\EplexSXpress.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Release14icp/
CPP_SBRS=.
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSXpress.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=xprs.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)\EplexSXpress.pdb" /machine:I386\
 /def:"..\..\icparc_solvers\seplex_xpress.def" /out:"$(OUTDIR)\EplexSXpress.dll"\
 /implib:"$(OUTDIR)\EplexSXpress.lib" /libpath:"../Eclipse/Release"\
 /libpath:"M:/Eclipse/xosl1427/i386_nt/lib" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_xpress.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj"

"$(OUTDIR)\EplexSXpress.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Release14icp
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\EplexSXpress.dll"
   if not exist ..\..\lib\i386_nt\express1427icp mkdir ..\..\lib\i386_nt\express1427icp
	copy          M:\Eclipse\xosl1427\i386_nt\bin\xprs.dll                ..\..\lib\i386_nt\express1427icp\xprs.dll
	copy          M:\Eclipse\xosl1427\i386_nt\bin\xprl.dll                ..\..\lib\i386_nt\express1427icp\xprl.dll
	copy          M:\Eclipse\xosl14\lic_icp\xpress.lic               ..\..\lib\i386_nt\express1427icp\xpress.lic
	copy Release14icp\EplexSXpress.dll                ..\..\lib\i386_nt\sexpress1427icp.dll
	copy Release14icp\EplexSXpress.dll               ..\..\icparc_solvers\i386_nt\sexpress1427icp.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Release14"

OUTDIR=.\Release14
INTDIR=.\Release14
# Begin Custom Macros
OutDir=.\Release14
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ELSE 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\EplexSXpress.dll"
	-@erase "$(OUTDIR)\EplexSXpress.exp"
	-@erase "$(OUTDIR)\EplexSXpress.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/xosl1427/i386_nt/include" /D "NDEBUG" /D XPRESS=14 /D\
 XPRESSMINOR=27 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__"\
 /Fp"$(INTDIR)\EplexSXpress.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Release14/
CPP_SBRS=.
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSXpress.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=xprs.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)\EplexSXpress.pdb" /machine:I386\
 /def:"..\..\icparc_solvers\seplex_xpress.def" /out:"$(OUTDIR)\EplexSXpress.dll"\
 /implib:"$(OUTDIR)\EplexSXpress.lib" /libpath:"../Eclipse/Release"\
 /libpath:"M:/Eclipse/xosl1427/i386_nt/lib" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_xpress.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj"

"$(OUTDIR)\EplexSXpress.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Release14
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\EplexSXpress.dll"
   if not exist ..\..\lib\i386_nt\express1427 mkdir ..\..\lib\i386_nt\express1427
	copy          M:\Eclipse\xosl1427\i386_nt\bin\xprs.dll                ..\..\lib\i386_nt\express1427\xprs.dll
	copy          M:\Eclipse\xosl1427\i386_nt\bin\xprl.dll                ..\..\lib\i386_nt\express1427\xprl.dll
	copy          M:\Eclipse\xosl14\lic\i386_nt\xpress.lic               ..\..\lib\i386_nt\express1427\xpress.lic
	copy Release14\EplexSXpress.dll                ..\..\lib\i386_nt\sexpress1427.dll
	copy Release14\EplexSXpress.dll               ..\..\icparc_solvers\i386_nt\sexpress1427.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Debug15"

OUTDIR=.\Debug15
INTDIR=.\Debug15
# Begin Custom Macros
OutDir=.\Debug15
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ELSE 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\EplexSXpress.dll"
	-@erase "$(OUTDIR)\EplexSXpress.exp"
	-@erase "$(OUTDIR)\EplexSXpress.ilk"
	-@erase "$(OUTDIR)\EplexSXpress.lib"
	-@erase "$(OUTDIR)\EplexSXpress.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/xosl1525/i386_nt/include" /D "_DEBUG" /D XPRESS=15 /D\
 XPRESSMINOR=25 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__"\
 /Fp"$(INTDIR)\EplexSXpress.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Debug15/
CPP_SBRS=.
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSXpress.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=xprs.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:yes /pdb:"$(OUTDIR)\EplexSXpress.pdb" /debug /machine:I386\
 /def:"..\..\icparc_solvers\seplex_xpress.def" /out:"$(OUTDIR)\EplexSXpress.dll"\
 /implib:"$(OUTDIR)\EplexSXpress.lib" /pdbtype:sept /libpath:"../Eclipse/Debug"\
 /libpath:"M:/Eclipse/xosl1525/i386_nt/lib" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_xpress.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj"

"$(OUTDIR)\EplexSXpress.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Debug15
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\EplexSXpress.dll"
   if not exist ..\..\lib\i386_nt\express1525 mkdir ..\..\lib\i386_nt\express1525
	copy          M:\Eclipse\xosl1525\i386_nt\bin\xprs.dll                ..\..\lib\i386_nt\express1525\xprs.dll
	copy          M:\Eclipse\xosl1525\i386_nt\bin\xprl.dll                ..\..\lib\i386_nt\express1525\xprl.dll
	copy          M:\Eclipse\xosl1525\i386_nt\bin\lmgrd.exe                ..\..\lib\i386_nt\express1525\lmgrd.exe
	copy          M:\Eclipse\xosl1525\i386_nt\bin\dash.exe                ..\..\lib\i386_nt\express1525\dash.exe
	copy          M:\Eclipse\xosl15\lic\xpress.lic                ..\..\lib\i386_nt\express1525\xpress.lic
	copy Debug15\EplexSXpress.dll                ..\..\lib\i386_nt\sexpress1525.dll
	copy Debug15\EplexSXpress.dll               ..\..\icparc_solvers\i386_nt\sexpress1525.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Release15"

OUTDIR=.\Release15
INTDIR=.\Release15
# Begin Custom Macros
OutDir=.\Release15
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ELSE 

ALL : "$(OUTDIR)\EplexSXpress.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\EplexSXpress.dll"
	-@erase "$(OUTDIR)\EplexSXpress.exp"
	-@erase "$(OUTDIR)\EplexSXpress.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/xosl1525/i386_nt/include" /D "NDEBUG" /D XPRESS=15 /D\
 XPRESSMINOR=25 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__"\
 /Fp"$(INTDIR)\EplexSXpress.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Release15/
CPP_SBRS=.
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSXpress.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=xprs.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)\EplexSXpress.pdb" /machine:I386\
 /def:"..\..\icparc_solvers\seplex_xpress.def" /out:"$(OUTDIR)\EplexSXpress.dll"\
 /implib:"$(OUTDIR)\EplexSXpress.lib" /libpath:"../Eclipse/Release"\
 /libpath:"M:/Eclipse/xosl1525/i386_nt/lib" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_xpress.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj"

"$(OUTDIR)\EplexSXpress.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Release15
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\EplexSXpress.dll"
   if not exist ..\..\lib\i386_nt\express1525 mkdir ..\..\lib\i386_nt\express1525
	copy          M:\Eclipse\xosl1525\i386_nt\bin\xprs.dll                ..\..\lib\i386_nt\express1525\xprs.dll
	copy          M:\Eclipse\xosl1525\i386_nt\bin\xprl.dll                ..\..\lib\i386_nt\express1525\xprl.dll
	copy          M:\Eclipse\xosl1525\i386_nt\bin\lmgrd.exe                ..\..\lib\i386_nt\express1525\lmgrd.exe
	copy          M:\Eclipse\xosl1525\i386_nt\bin\dash.exe                ..\..\lib\i386_nt\express1525\dash.exe
	copy          M:\Eclipse\xosl15\lic\xpress.lic               ..\..\lib\i386_nt\express1525\xpress.lic
	copy Release15\EplexSXpress.dll                ..\..\lib\i386_nt\sexpress1525.dll
	copy Release15\EplexSXpress.dll               ..\..\icparc_solvers\i386_nt\sexpress1525.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ENDIF 

.c{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<


!IF "$(CFG)" == "EplexSXpress - Win32 Release13" || "$(CFG)" ==\
 "EplexSXpress - Win32 Debug13" || "$(CFG)" ==\
 "EplexSXpress - Win32 Release13icp" || "$(CFG)" ==\
 "EplexSXpress - Win32 Debug13icp" || "$(CFG)" ==\
 "EplexSXpress - Win32 Debug14icp" || "$(CFG)" == "EplexSXpress - Win32 Debug14"\
 || "$(CFG)" == "EplexSXpress - Win32 Release14icp" || "$(CFG)" ==\
 "EplexSXpress - Win32 Release14" || "$(CFG)" == "EplexSXpress - Win32 Debug15"\
 || "$(CFG)" == "EplexSXpress - Win32 Release15"
SOURCE=..\..\icparc_solvers\seplex.c

!IF  "$(CFG)" == "EplexSXpress - Win32 Release13"

DEP_CPP_SEPLE=\
	"..\..\icparc_solvers\eplex_params.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"M:\Eclipse\xosl1326\i386_nt\include\xprs.h"\
	
NODEP_CPP_SEPLE=\
	"..\..\icparc_solvers\cplex.h"\
	

"$(INTDIR)\seplex.obj" : $(SOURCE) $(DEP_CPP_SEPLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Debug13"

DEP_CPP_SEPLE=\
	"..\..\icparc_solvers\eplex_params.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"M:\Eclipse\xosl1326\i386_nt\include\xprs.h"\
	

"$(INTDIR)\seplex.obj" : $(SOURCE) $(DEP_CPP_SEPLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Release13icp"

DEP_CPP_SEPLE=\
	"..\..\icparc_solvers\eplex_params.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"M:\Eclipse\xosl1326icp\i386_nt\include\xprs.h"\
	
NODEP_CPP_SEPLE=\
	"..\..\icparc_solvers\cplex.h"\
	

"$(INTDIR)\seplex.obj" : $(SOURCE) $(DEP_CPP_SEPLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Debug13icp"

DEP_CPP_SEPLE=\
	"..\..\icparc_solvers\eplex_params.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"M:\Eclipse\xosl1326icp\i386_nt\include\xprs.h"\
	

"$(INTDIR)\seplex.obj" : $(SOURCE) $(DEP_CPP_SEPLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Debug14icp"

DEP_CPP_SEPLE=\
	"..\..\icparc_solvers\eplex_params.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"M:\Eclipse\xosl1427\i386_nt\include\xprs.h"\
	
NODEP_CPP_SEPLE=\
	"..\..\icparc_solvers\cplex.h"\
	

"$(INTDIR)\seplex.obj" : $(SOURCE) $(DEP_CPP_SEPLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Debug14"

DEP_CPP_SEPLE=\
	"..\..\icparc_solvers\eplex_params.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"M:\Eclipse\xosl1427\i386_nt\include\xprs.h"\
	

"$(INTDIR)\seplex.obj" : $(SOURCE) $(DEP_CPP_SEPLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Release14icp"

DEP_CPP_SEPLE=\
	"..\..\icparc_solvers\eplex_params.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"M:\Eclipse\xosl1427\i386_nt\include\xprs.h"\
	

"$(INTDIR)\seplex.obj" : $(SOURCE) $(DEP_CPP_SEPLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Release14"

DEP_CPP_SEPLE=\
	"..\..\icparc_solvers\eplex_params.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"M:\Eclipse\xosl1427\i386_nt\include\xprs.h"\
	

"$(INTDIR)\seplex.obj" : $(SOURCE) $(DEP_CPP_SEPLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Debug15"

DEP_CPP_SEPLE=\
	"..\..\icparc_solvers\eplex_params.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"M:\Eclipse\xosl1525\i386_nt\include\xprs.h"\
	
NODEP_CPP_SEPLE=\
	"..\..\icparc_solvers\cplex.h"\
	

"$(INTDIR)\seplex.obj" : $(SOURCE) $(DEP_CPP_SEPLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Release15"

DEP_CPP_SEPLE=\
	"..\..\icparc_solvers\eplex_params.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"M:\Eclipse\xosl1525\i386_nt\include\xprs.h"\
	

"$(INTDIR)\seplex.obj" : $(SOURCE) $(DEP_CPP_SEPLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

