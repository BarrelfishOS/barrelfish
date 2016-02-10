# Microsoft Developer Studio Generated NMAKE File, Based on Ria.dsp
!IF "$(CFG)" == ""
CFG=Ria - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Ria - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Ria - Win32 Release" && "$(CFG)" != "Ria - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Ria.mak" CFG="Ria - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Ria - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Ria - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
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

!IF  "$(CFG)" == "Ria - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Ria.dll"

!ELSE 

ALL : "$(OUTDIR)\Ria.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\ria.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\Ria.dll"
	-@erase "$(OUTDIR)\Ria.exp"
	-@erase "$(OUTDIR)\Ria.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /D "WIN32" /D\
 "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\Ria.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Release/
CPP_SBRS=.
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Ria.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)\Ria.pdb" /machine:I386 /def:"..\..\icparc_solvers\ria.def"\
 /out:"$(OUTDIR)\Ria.dll" /implib:"$(OUTDIR)\Ria.lib"\
 /libpath:"../Eclipse/Release" 
DEF_FILE= \
	"..\..\icparc_solvers\ria.def"
LINK32_OBJS= \
	"$(INTDIR)\ria.obj"

"$(OUTDIR)\Ria.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\Ria.dll"
   copy Release\Ria.dll ..\..\icparc_solvers\i386_nt\ria.dll
	copy  Release\Ria.dll ..\..\lib\i386_nt\ria.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "Ria - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Ria.dll"

!ELSE 

ALL : "$(OUTDIR)\Ria.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\ria.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\Ria.dll"
	-@erase "$(OUTDIR)\Ria.exp"
	-@erase "$(OUTDIR)\Ria.ilk"
	-@erase "$(OUTDIR)\Ria.lib"
	-@erase "$(OUTDIR)\Ria.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /D "WIN32"\
 /D "_DEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\Ria.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Ria.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /incremental:yes\
 /pdb:"$(OUTDIR)\Ria.pdb" /debug /machine:I386\
 /def:"..\..\icparc_solvers\ria.def" /out:"$(OUTDIR)\Ria.dll"\
 /implib:"$(OUTDIR)\Ria.lib" /pdbtype:sept /libpath:"../Eclipse/Debug" 
DEF_FILE= \
	"..\..\icparc_solvers\ria.def"
LINK32_OBJS= \
	"$(INTDIR)\ria.obj"

"$(OUTDIR)\Ria.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\Ria.dll"
   copy Debug\Ria.dll ..\..\icparc_solvers\i386_nt\ria.dll
	copy  Debug\Ria.dll ..\..\lib\i386_nt\ria.dll
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


!IF "$(CFG)" == "Ria - Win32 Release" || "$(CFG)" == "Ria - Win32 Debug"
SOURCE=..\..\icparc_solvers\ria.c
DEP_CPP_RIA_C=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	

"$(INTDIR)\ria.obj" : $(SOURCE) $(DEP_CPP_RIA_C) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

