# Microsoft Developer Studio Generated NMAKE File, Based on EplexSCplex.dsp
!IF "$(CFG)" == ""
CFG=EplexSCplex - Win32 Release
!MESSAGE No configuration specified. Defaulting to EplexSCplex - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "EplexSCplex - Win32 Release" && "$(CFG)" !=\
 "EplexSCplex - Win32 Debug" && "$(CFG)" != "EplexSCplex - Win32 DebugCplex75"\
 && "$(CFG)" != "EplexSCplex - Win32 ReleaseCplex75" && "$(CFG)" !=\
 "EplexSCplex - Win32 DebugCplex90" && "$(CFG)" !=\
 "EplexSCplex - Win32 ReleaseCplex90"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "EplexSCplex.mak" CFG="EplexSCplex - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "EplexSCplex - Win32 Release" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSCplex - Win32 Debug" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSCplex - Win32 DebugCplex75" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSCplex - Win32 ReleaseCplex75" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSCplex - Win32 DebugCplex90" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSCplex - Win32 ReleaseCplex90" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "EplexSCplex - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSCplex.dll"

!ELSE 

ALL : "Eclipse - Win32 Release" "$(OUTDIR)\EplexSCplex.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Eclipse - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\EplexSCplex.dll"
	-@erase "$(OUTDIR)\EplexSCplex.exp"
	-@erase "$(OUTDIR)\EplexSCplex.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/cplex6.0" /D CPLEX=6 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)\EplexSCplex.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSCplex.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=cplex60.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)\EplexSCplex.pdb" /machine:I386\
 /def:"..\..\icparc_solvers\seplex_cplex.def" /out:"$(OUTDIR)\EplexSCplex.dll"\
 /implib:"$(OUTDIR)\EplexSCplex.lib" /libpath:"../Eclipse/Release"\
 /libpath:"M:/Eclipse/cplex6.0" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_cplex.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj" \
	"..\Eclipse\Release\Eclipse.lib"

"$(OUTDIR)\EplexSCplex.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

$(DS_POSTBUILD_DEP) : "Eclipse - Win32 Release" "$(OUTDIR)\EplexSCplex.dll"
   copy Release\EplexSCplex.dll             ..\..\icparc_solvers\i386_nt\secplex60.dll
	copy Release\EplexSCplex.dll             ..\..\lib\i386_nt\secplex60.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSCplex.dll"

!ELSE 

ALL : "Eclipse - Win32 Debug" "$(OUTDIR)\EplexSCplex.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Eclipse - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\EplexSCplex.dll"
	-@erase "$(OUTDIR)\EplexSCplex.exp"
	-@erase "$(OUTDIR)\EplexSCplex.ilk"
	-@erase "$(OUTDIR)\EplexSCplex.lib"
	-@erase "$(OUTDIR)\EplexSCplex.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/cplex6.0" /D CPLEX=6 /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)\EplexSCplex.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSCplex.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=cplex60.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:yes /pdb:"$(OUTDIR)\EplexSCplex.pdb" /debug /machine:I386\
 /def:"..\..\icparc_solvers\seplex_cplex.def" /out:"$(OUTDIR)\EplexSCplex.dll"\
 /implib:"$(OUTDIR)\EplexSCplex.lib" /pdbtype:sept /libpath:"../Eclipse/Debug"\
 /libpath:"M:/Eclipse/cplex6.0" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_cplex.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj" \
	"..\Eclipse\Debug\Eclipse.lib"

"$(OUTDIR)\EplexSCplex.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

$(DS_POSTBUILD_DEP) : "Eclipse - Win32 Debug" "$(OUTDIR)\EplexSCplex.dll"
   copy Debug\EplexSCplex.dll             ..\..\icparc_solvers\i386_nt\secplex60.dll
	copy Debug\EplexSCplex.dll             ..\..\lib\i386_nt\secplex60.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 DebugCplex75"

OUTDIR=.\DebugCplex75
INTDIR=.\DebugCplex75
# Begin Custom Macros
OutDir=.\DebugCplex75
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSCplex.dll"

!ELSE 

ALL : "$(OUTDIR)\EplexSCplex.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\EplexSCplex.dll"
	-@erase "$(OUTDIR)\EplexSCplex.exp"
	-@erase "$(OUTDIR)\EplexSCplex.ilk"
	-@erase "$(OUTDIR)\EplexSCplex.lib"
	-@erase "$(OUTDIR)\EplexSCplex.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/cplex75/include/ilcplex" /D CPLEX=7 /D CPLEXMINOR=5 /D "WIN32" /D\
 "_DEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\EplexSCplex.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\DebugCplex75/
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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSCplex.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=cplex75.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:yes /pdb:"$(OUTDIR)\EplexSCplex.pdb" /debug /machine:I386\
 /def:"..\..\icparc_solvers\seplex_cplex.def" /out:"$(OUTDIR)\EplexSCplex.dll"\
 /implib:"$(OUTDIR)\EplexSCplex.lib" /pdbtype:sept /libpath:"../Eclipse/Debug"\
 /libpath:"M:/Eclipse/cplex75/lib/msvc6/stat_mt" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_cplex.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj"

"$(OUTDIR)\EplexSCplex.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\DebugCplex75
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\EplexSCplex.dll"
   copy DebugCplex75\EplexSCplex.dll             ..\..\icparc_solvers\i386_nt\secplex75.dll
	copy DebugCplex75\EplexSCplex.dll             ..\..\lib\i386_nt\secplex75.dll
	copy M:\Eclipse\cplex75\bin\msvc6\cplex75.dll             ..\..\lib\i386_nt\cplex75.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 ReleaseCplex75"

OUTDIR=.\ReleaseCplex75
INTDIR=.\ReleaseCplex75
# Begin Custom Macros
OutDir=.\ReleaseCplex75
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSCplex.dll"

!ELSE 

ALL : "$(OUTDIR)\EplexSCplex.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\EplexSCplex.dll"
	-@erase "$(OUTDIR)\EplexSCplex.exp"
	-@erase "$(OUTDIR)\EplexSCplex.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/cplex75/include/ilcplex" /D CPLEX=7 /D CPLEXMINOR=5 /D "WIN32" /D\
 "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\EplexSCplex.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\ReleaseCplex75/
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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSCplex.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=cplex75.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)\EplexSCplex.pdb" /machine:I386\
 /def:"..\..\icparc_solvers\seplex_cplex.def" /out:"$(OUTDIR)\EplexSCplex.dll"\
 /implib:"$(OUTDIR)\EplexSCplex.lib" /libpath:"../Eclipse/Release"\
 /libpath:"M:/Eclipse/cplex75/lib/msvc6/stat_mt" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_cplex.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj"

"$(OUTDIR)\EplexSCplex.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\ReleaseCplex75
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\EplexSCplex.dll"
   copy ReleaseCplex75\EplexSCplex.dll             ..\..\icparc_solvers\i386_nt\secplex75.dll
	copy ReleaseCplex75\EplexSCplex.dll             ..\..\lib\i386_nt\secplex75.dll
	copy M:\Eclipse\cplex75\bin\msvc6\cplex75.dll             ..\..\lib\i386_nt\cplex75.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 DebugCplex90"

OUTDIR=.\DebugCplex90
INTDIR=.\DebugCplex90
# Begin Custom Macros
OutDir=.\DebugCplex90
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSCplex.dll"

!ELSE 

ALL : "$(OUTDIR)\EplexSCplex.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\EplexSCplex.dll"
	-@erase "$(OUTDIR)\EplexSCplex.exp"
	-@erase "$(OUTDIR)\EplexSCplex.ilk"
	-@erase "$(OUTDIR)\EplexSCplex.lib"
	-@erase "$(OUTDIR)\EplexSCplex.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/cplex90/include/ilcplex" /D CPLEX=9 /D CPLEXMINOR=0 /D "WIN32" /D\
 "_DEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\EplexSCplex.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\DebugCplex90/
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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSCplex.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=cplex90.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:yes /pdb:"$(OUTDIR)\EplexSCplex.pdb" /debug /machine:I386\
 /def:"..\..\icparc_solvers\seplex_cplex.def" /out:"$(OUTDIR)\EplexSCplex.dll"\
 /implib:"$(OUTDIR)\EplexSCplex.lib" /pdbtype:sept /libpath:"../Eclipse/Debug"\
 /libpath:"M:/Eclipse/cplex90/lib/msvc6/stat_mt" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_cplex.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj"

"$(OUTDIR)\EplexSCplex.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\DebugCplex90
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\EplexSCplex.dll"
   copy DebugCplex90\EplexSCplex.dll             ..\..\icparc_solvers\i386_nt\secplex90.dll
	copy DebugCplex90\EplexSCplex.dll             ..\..\lib\i386_nt\secplex90.dll
	copy M:\Eclipse\cplex90\bin\msvc6\cplex90.dll             ..\..\lib\i386_nt\cplex90.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 ReleaseCplex90"

OUTDIR=.\ReleaseCplex90
INTDIR=.\ReleaseCplex90
# Begin Custom Macros
OutDir=.\ReleaseCplex90
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EplexSCplex.dll"

!ELSE 

ALL : "$(OUTDIR)\EplexSCplex.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\seplex.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\EplexSCplex.dll"
	-@erase "$(OUTDIR)\EplexSCplex.exp"
	-@erase "$(OUTDIR)\EplexSCplex.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I\
 "M:/Eclipse/cplex90/include/ilcplex" /D CPLEX=9 /D CPLEXMINOR=0 /D "WIN32" /D\
 "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\EplexSCplex.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\ReleaseCplex90/
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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EplexSCplex.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=cplex90.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)\EplexSCplex.pdb" /machine:I386\
 /def:"..\..\icparc_solvers\seplex_cplex.def" /out:"$(OUTDIR)\EplexSCplex.dll"\
 /implib:"$(OUTDIR)\EplexSCplex.lib" /libpath:"../Eclipse/Release"\
 /libpath:"M:/Eclipse/cplex90/lib/msvc6/stat_mt" 
DEF_FILE= \
	"..\..\icparc_solvers\seplex_cplex.def"
LINK32_OBJS= \
	"$(INTDIR)\seplex.obj"

"$(OUTDIR)\EplexSCplex.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\ReleaseCplex90
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\EplexSCplex.dll"
   copy ReleaseCplex90\EplexSCplex.dll             ..\..\icparc_solvers\i386_nt\secplex90.dll
	copy ReleaseCplex90\EplexSCplex.dll             ..\..\lib\i386_nt\secplex90.dll
	copy M:\Eclipse\cplex90\bin\msvc6\cplex90.dll             ..\..\lib\i386_nt\cplex90.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ENDIF 


!IF "$(CFG)" == "EplexSCplex - Win32 Release" || "$(CFG)" ==\
 "EplexSCplex - Win32 Debug" || "$(CFG)" == "EplexSCplex - Win32 DebugCplex75"\
 || "$(CFG)" == "EplexSCplex - Win32 ReleaseCplex75" || "$(CFG)" ==\
 "EplexSCplex - Win32 DebugCplex90" || "$(CFG)" ==\
 "EplexSCplex - Win32 ReleaseCplex90"

!IF  "$(CFG)" == "EplexSCplex - Win32 Release"

"Eclipse - Win32 Release" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) /F .\Eclipse.mak CFG="Eclipse - Win32 Release" 
   cd "..\EplexSCplex"

"Eclipse - Win32 ReleaseCLEAN" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Eclipse.mak CFG="Eclipse - Win32 Release"\
 RECURSE=1 
   cd "..\EplexSCplex"

!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 Debug"

"Eclipse - Win32 Debug" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) /F .\Eclipse.mak CFG="Eclipse - Win32 Debug" 
   cd "..\EplexSCplex"

"Eclipse - Win32 DebugCLEAN" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Eclipse.mak CFG="Eclipse - Win32 Debug"\
 RECURSE=1 
   cd "..\EplexSCplex"

!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 DebugCplex75"

!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 ReleaseCplex75"

!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 DebugCplex90"

!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 ReleaseCplex90"

!ENDIF 

SOURCE=..\..\icparc_solvers\seplex.c

!IF  "$(CFG)" == "EplexSCplex - Win32 Release"

DEP_CPP_SEPLE=\
	"..\..\icparc_solvers\eplex_params.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"M:\Eclipse\cplex6.0\cplex.h"\
	
NODEP_CPP_SEPLE=\
	"..\..\icparc_solvers\xprs.h"\
	

"$(INTDIR)\seplex.obj" : $(SOURCE) $(DEP_CPP_SEPLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 Debug"


"$(INTDIR)\seplex.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 DebugCplex75"

DEP_CPP_SEPLE=\
	"..\..\icparc_solvers\eplex_params.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"m:\eclipse\cplex75\include\ilcplex\cplex.h"\
	

"$(INTDIR)\seplex.obj" : $(SOURCE) $(DEP_CPP_SEPLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 ReleaseCplex75"

DEP_CPP_SEPLE=\
	"..\..\icparc_solvers\eplex_params.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"m:\eclipse\cplex75\include\ilcplex\cplex.h"\
	

"$(INTDIR)\seplex.obj" : $(SOURCE) $(DEP_CPP_SEPLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 DebugCplex90"

DEP_CPP_SEPLE=\
	"..\..\icparc_solvers\eplex_params.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"M:\Eclipse\cplex90\include\ilcplex\cplex.h"\
	
NODEP_CPP_SEPLE=\
	"..\..\icparc_solvers\xprs.h"\
	

"$(INTDIR)\seplex.obj" : $(SOURCE) $(DEP_CPP_SEPLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 ReleaseCplex90"

DEP_CPP_SEPLE=\
	"..\..\icparc_solvers\eplex_params.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"M:\Eclipse\cplex90\include\ilcplex\cplex.h"\
	
NODEP_CPP_SEPLE=\
	"..\..\icparc_solvers\xprs.h"\
	

"$(INTDIR)\seplex.obj" : $(SOURCE) $(DEP_CPP_SEPLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

