# Microsoft Developer Studio Generated NMAKE File, Based on TkEclipse.dsp
!IF "$(CFG)" == ""
CFG=TkEclipse - Win32 Debug
!MESSAGE No configuration specified. Defaulting to TkEclipse - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "TkEclipse - Win32 Release" && "$(CFG)" !=\
 "TkEclipse - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "TkEclipse.mak" CFG="TkEclipse - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "TkEclipse - Win32 Release" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "TkEclipse - Win32 Debug" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "TkEclipse - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\TkEclipse.dll"

!ELSE 

ALL : "TkExdr - Win32 Release" "Eclipse - Win32 Release"\
 "$(OUTDIR)\TkEclipse.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Eclipse - Win32 ReleaseCLEAN" "TkExdr - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\tkeclipse.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\TkEclipse.dll"
	-@erase "$(OUTDIR)\TkEclipse.exp"
	-@erase "$(OUTDIR)\TkEclipse.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I\
 "C:/Program Files/Tcl/include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "__STDC__" /Fp"$(INTDIR)\TkEclipse.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 
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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\TkEclipse.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=tkexdr.lib tcl83.lib tk83.lib eclipse.lib kernel32.lib user32.lib\
 gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib\
 oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)\TkEclipse.pdb" /machine:I386\
 /def:"..\..\sepia\i386_nt\tkeclipse.def" /out:"$(OUTDIR)\TkEclipse.dll"\
 /implib:"$(OUTDIR)\TkEclipse.lib" /libpath:"../Eclipse/Release"\
 /libpath:"../TkExdr/Release" /libpath:"C:/Program Files/Tcl/lib" 
DEF_FILE= \
	"..\..\sepia\i386_nt\tkeclipse.def"
LINK32_OBJS= \
	"$(INTDIR)\tkeclipse.obj" \
	"..\Eclipse\Release\Eclipse.lib" \
	"..\TkExdr\Release\TkExdr.lib"

"$(OUTDIR)\TkEclipse.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

$(DS_POSTBUILD_DEP) : "TkExdr - Win32 Release" "Eclipse - Win32 Release"\
 "$(OUTDIR)\TkEclipse.dll"
   copy Release\TkEclipse.dll                  ..\..\lib\i386_nt\tkeclipse.dll
	copy Release\TkEclipse.lib                  ..\..\lib\i386_nt\tkeclipse.lib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "TkEclipse - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\TkEclipse.dll"

!ELSE 

ALL : "TkExdr - Win32 Debug" "Eclipse - Win32 Debug" "$(OUTDIR)\TkEclipse.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Eclipse - Win32 DebugCLEAN" "TkExdr - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\tkeclipse.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\TkEclipse.dll"
	-@erase "$(OUTDIR)\TkEclipse.exp"
	-@erase "$(OUTDIR)\TkEclipse.ilk"
	-@erase "$(OUTDIR)\TkEclipse.lib"
	-@erase "$(OUTDIR)\TkEclipse.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I\
 "C:/Program Files/Tcl/include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D\
 "__STDC__" /Fp"$(INTDIR)\TkEclipse.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 
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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\TkEclipse.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=tkexdr.lib tcl83.lib tk83.lib eclipse.lib kernel32.lib user32.lib\
 gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib\
 oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:yes /pdb:"$(OUTDIR)\TkEclipse.pdb" /debug /machine:I386\
 /def:"..\..\sepia\i386_nt\tkeclipse.def" /out:"$(OUTDIR)\TkEclipse.dll"\
 /implib:"$(OUTDIR)\TkEclipse.lib" /pdbtype:sept /libpath:"../Eclipse/Debug"\
 /libpath:"../TkExdr/Debug" /libpath:"M:/Eclipse/tcltk8.3/i386_nt/lib"\
 /libpath:"../Eclipse/Release" /libpath:"C:/Program Files/Tcl/lib" 
DEF_FILE= \
	"..\..\sepia\i386_nt\tkeclipse.def"
LINK32_OBJS= \
	"$(INTDIR)\tkeclipse.obj" \
	"..\Eclipse\Debug\Eclipse.lib" \
	"..\TkExdr\Debug\TkExdr.lib"

"$(OUTDIR)\TkEclipse.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

$(DS_POSTBUILD_DEP) : "TkExdr - Win32 Debug" "Eclipse - Win32 Debug"\
 "$(OUTDIR)\TkEclipse.dll"
   copy Debug\TkEclipse.dll                  ..\..\lib\i386_nt\tkeclipse.dll
	copy Debug\TkEclipse.lib                  ..\..\lib\i386_nt\tkeclipse.lib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ENDIF 


!IF "$(CFG)" == "TkEclipse - Win32 Release" || "$(CFG)" ==\
 "TkEclipse - Win32 Debug"

!IF  "$(CFG)" == "TkEclipse - Win32 Release"

"Eclipse - Win32 Release" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) /F .\Eclipse.mak CFG="Eclipse - Win32 Release" 
   cd "..\TkEclipse"

"Eclipse - Win32 ReleaseCLEAN" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Eclipse.mak CFG="Eclipse - Win32 Release"\
 RECURSE=1 
   cd "..\TkEclipse"

!ELSEIF  "$(CFG)" == "TkEclipse - Win32 Debug"

"Eclipse - Win32 Debug" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) /F .\Eclipse.mak CFG="Eclipse - Win32 Debug" 
   cd "..\TkEclipse"

"Eclipse - Win32 DebugCLEAN" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Eclipse.mak CFG="Eclipse - Win32 Debug"\
 RECURSE=1 
   cd "..\TkEclipse"

!ENDIF 

!IF  "$(CFG)" == "TkEclipse - Win32 Release"

"TkExdr - Win32 Release" : 
   cd "..\TkExdr"
   $(MAKE) /$(MAKEFLAGS) /F .\TkExdr.mak CFG="TkExdr - Win32 Release" 
   cd "..\TkEclipse"

"TkExdr - Win32 ReleaseCLEAN" : 
   cd "..\TkExdr"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\TkExdr.mak CFG="TkExdr - Win32 Release"\
 RECURSE=1 
   cd "..\TkEclipse"

!ELSEIF  "$(CFG)" == "TkEclipse - Win32 Debug"

"TkExdr - Win32 Debug" : 
   cd "..\TkExdr"
   $(MAKE) /$(MAKEFLAGS) /F .\TkExdr.mak CFG="TkExdr - Win32 Debug" 
   cd "..\TkEclipse"

"TkExdr - Win32 DebugCLEAN" : 
   cd "..\TkExdr"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\TkExdr.mak CFG="TkExdr - Win32 Debug"\
 RECURSE=1 
   cd "..\TkEclipse"

!ENDIF 

SOURCE=..\..\sepia\i386_nt\tkeclipse.c

!IF  "$(CFG)" == "TkEclipse - Win32 Release"

DEP_CPP_TKECL=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\eclipse.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"c:\program files\tcl\include\tcl.h"\
	"c:\program files\tcl\include\tcldecls.h"\
	

"$(INTDIR)\tkeclipse.obj" : $(SOURCE) $(DEP_CPP_TKECL) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "TkEclipse - Win32 Debug"

DEP_CPP_TKECL=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\eclipse.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\tkcommon.h"\
	"c:\program files\tcl\include\tcl.h"\
	"c:\program files\tcl\include\tcldecls.h"\
	

"$(INTDIR)\tkeclipse.obj" : $(SOURCE) $(DEP_CPP_TKECL) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

