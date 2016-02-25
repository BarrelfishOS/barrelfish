# Microsoft Developer Studio Generated NMAKE File, Based on TkExdr.dsp
!IF "$(CFG)" == ""
CFG=TkExdr - Win32 Debug
!MESSAGE No configuration specified. Defaulting to TkExdr - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "TkExdr - Win32 Release" && "$(CFG)" != "TkExdr - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "TkExdr.mak" CFG="TkExdr - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "TkExdr - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "TkExdr - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "TkExdr - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\TkExdr.dll"

!ELSE 

ALL : "Eclipse - Win32 Release" "$(OUTDIR)\TkExdr.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Eclipse - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\tkexdr.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\TkExdr.dll"
	-@erase "$(OUTDIR)\TkExdr.exp"
	-@erase "$(OUTDIR)\TkExdr.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I\
 "C:/Program Files/Tcl/include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "__STDC__" /Fp"$(INTDIR)\TkExdr.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD\
 /c 
CPP_OBJS=.\Release/
CPP_SBRS=.

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

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o NUL /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\TkExdr.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=tcl83.lib tk83.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)\TkExdr.pdb" /machine:I386\
 /def:"..\..\sepia\i386_nt\tkexdr.def" /out:"$(OUTDIR)\TkExdr.dll"\
 /implib:"$(OUTDIR)\TkExdr.lib" /libpath:"../Eclipse/Release"\
 /libpath:"C:/Program Files/Tcl/lib" 
DEF_FILE= \
	"..\..\sepia\i386_nt\tkexdr.def"
LINK32_OBJS= \
	"$(INTDIR)\tkexdr.obj" \
	"..\Eclipse\Release\Eclipse.lib"

"$(OUTDIR)\TkExdr.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll and lib to lib/i386_nt
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

$(DS_POSTBUILD_DEP) : "Eclipse - Win32 Release" "$(OUTDIR)\TkExdr.dll"
   copy Release\TkExdr.dll             ..\..\lib\i386_nt\tkexdr.dll
	copy Release\TkExdr.lib             ..\..\lib\i386_nt\tkexdr.lib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "TkExdr - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\TkExdr.dll"

!ELSE 

ALL : "Eclipse - Win32 Debug" "$(OUTDIR)\TkExdr.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Eclipse - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\tkexdr.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\TkExdr.dll"
	-@erase "$(OUTDIR)\TkExdr.exp"
	-@erase "$(OUTDIR)\TkExdr.ilk"
	-@erase "$(OUTDIR)\TkExdr.lib"
	-@erase "$(OUTDIR)\TkExdr.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I\
 "C:/Program Files/Tcl/include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D\
 "__STDC__" /Fp"$(INTDIR)\TkExdr.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD\
 /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.

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

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\TkExdr.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=tcl83.lib tk83.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:yes /pdb:"$(OUTDIR)\TkExdr.pdb" /debug /machine:I386\
 /def:"..\..\sepia\i386_nt\tkexdr.def" /out:"$(OUTDIR)\TkExdr.dll"\
 /implib:"$(OUTDIR)\TkExdr.lib" /pdbtype:sept /libpath:"../Eclipse/Debug"\
 /libpath:"M:/Eclipse/tcltk8.0/i386_nt/lib" /libpath:"../Eclipse/Release"\
 /libpath:"C:/Program Files/Tcl/lib" 
DEF_FILE= \
	"..\..\sepia\i386_nt\tkexdr.def"
LINK32_OBJS= \
	"$(INTDIR)\tkexdr.obj" \
	"..\Eclipse\Debug\Eclipse.lib"

"$(OUTDIR)\TkExdr.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll and lib to lib/i386_nt
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

$(DS_POSTBUILD_DEP) : "Eclipse - Win32 Debug" "$(OUTDIR)\TkExdr.dll"
   copy Debug\TkExdr.dll             ..\..\lib\i386_nt\tkexdr.dll
	copy Debug\TkExdr.lib             ..\..\lib\i386_nt\tkexdr.lib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ENDIF 


!IF "$(CFG)" == "TkExdr - Win32 Release" || "$(CFG)" == "TkExdr - Win32 Debug"

!IF  "$(CFG)" == "TkExdr - Win32 Release"

"Eclipse - Win32 Release" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) /F .\Eclipse.mak CFG="Eclipse - Win32 Release" 
   cd "..\TkExdr"

"Eclipse - Win32 ReleaseCLEAN" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Eclipse.mak CFG="Eclipse - Win32 Release"\
 RECURSE=1 
   cd "..\TkExdr"

!ELSEIF  "$(CFG)" == "TkExdr - Win32 Debug"

"Eclipse - Win32 Debug" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) /F .\Eclipse.mak CFG="Eclipse - Win32 Debug" 
   cd "..\TkExdr"

"Eclipse - Win32 DebugCLEAN" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Eclipse.mak CFG="Eclipse - Win32 Debug"\
 RECURSE=1 
   cd "..\TkExdr"

!ENDIF 

SOURCE=..\..\sepia\i386_nt\tkexdr.c

!IF  "$(CFG)" == "TkExdr - Win32 Release"

DEP_CPP_TKEXD=\
	"c:\program files\tcl\include\tcl.h"\
	"c:\program files\tcl\include\tcldecls.h"\
	

"$(INTDIR)\tkexdr.obj" : $(SOURCE) $(DEP_CPP_TKEXD) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "TkExdr - Win32 Debug"

DEP_CPP_TKEXD=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\tkcommon.h"\
	"c:\program files\tcl\include\tcl.h"\
	"c:\program files\tcl\include\tcldecls.h"\
	

"$(INTDIR)\tkexdr.obj" : $(SOURCE) $(DEP_CPP_TKEXD) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

