# Microsoft Developer Studio Generated NMAKE File, Based on IC.dsp
!IF "$(CFG)" == ""
CFG=IC - Win32 Debug
!MESSAGE No configuration specified. Defaulting to IC - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "IC - Win32 Release" && "$(CFG)" != "IC - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "IC.mak" CFG="IC - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "IC - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "IC - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "IC - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\IC.dll"

!ELSE 

ALL : "Eclipse - Win32 Release" "Bitmap - Win32 Release" "$(OUTDIR)\IC.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Bitmap - Win32 ReleaseCLEAN" "Eclipse - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\ic.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\IC.dll"
	-@erase "$(OUTDIR)\IC.exp"
	-@erase "$(OUTDIR)\IC.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /D "WIN32" /D\
 "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\IC.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\IC.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)\IC.pdb" /machine:I386 /def:"..\..\icparc_solvers\ic.def"\
 /out:"$(OUTDIR)\IC.dll" /implib:"$(OUTDIR)\IC.lib"\
 /libpath:"../Eclipse/Release" /libpath:"../Eclipse/Debug" 
DEF_FILE= \
	"..\..\icparc_solvers\ic.def"
LINK32_OBJS= \
	"$(INTDIR)\ic.obj" \
	"..\Bitmap\Release\Bitmap.lib" \
	"..\Eclipse\Release\Eclipse.lib"

"$(OUTDIR)\IC.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to lib/i386_nt
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

$(DS_POSTBUILD_DEP) : "Eclipse - Win32 Release" "Bitmap - Win32 Release"\
 "$(OUTDIR)\IC.dll"
   copy    Release\IC.dll ..\..\lib\i386_nt\ic.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "IC - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\IC.dll"

!ELSE 

ALL : "Eclipse - Win32 Debug" "Bitmap - Win32 Debug" "$(OUTDIR)\IC.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Bitmap - Win32 DebugCLEAN" "Eclipse - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\ic.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\IC.dll"
	-@erase "$(OUTDIR)\IC.exp"
	-@erase "$(OUTDIR)\IC.ilk"
	-@erase "$(OUTDIR)\IC.lib"
	-@erase "$(OUTDIR)\IC.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /D "WIN32"\
 /D "_DEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\IC.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\IC.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=bitmap.lib eclipse.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:yes /pdb:"$(OUTDIR)\IC.pdb" /debug /machine:I386\
 /def:"..\..\icparc_solvers\ic.def" /out:"$(OUTDIR)\IC.dll"\
 /implib:"$(OUTDIR)\IC.lib" /pdbtype:sept /libpath:"../Eclipse/Debug"\
 /libpath:"../Bitmap/Debug" 
DEF_FILE= \
	"..\..\icparc_solvers\ic.def"
LINK32_OBJS= \
	"$(INTDIR)\ic.obj" \
	"..\Bitmap\Debug\Bitmap.lib" \
	"..\Eclipse\Debug\Eclipse.lib"

"$(OUTDIR)\IC.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to lib/i386_nt
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

$(DS_POSTBUILD_DEP) : "Eclipse - Win32 Debug" "Bitmap - Win32 Debug"\
 "$(OUTDIR)\IC.dll"
   copy    Debug\IC.dll ..\..\lib\i386_nt\ic.dll
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ENDIF 


!IF "$(CFG)" == "IC - Win32 Release" || "$(CFG)" == "IC - Win32 Debug"

!IF  "$(CFG)" == "IC - Win32 Release"

"Bitmap - Win32 Release" : 
   cd "..\Bitmap"
   $(MAKE) /$(MAKEFLAGS) /F .\Bitmap.mak CFG="Bitmap - Win32 Release" 
   cd "..\IC"

"Bitmap - Win32 ReleaseCLEAN" : 
   cd "..\Bitmap"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Bitmap.mak CFG="Bitmap - Win32 Release"\
 RECURSE=1 
   cd "..\IC"

!ELSEIF  "$(CFG)" == "IC - Win32 Debug"

"Bitmap - Win32 Debug" : 
   cd "..\Bitmap"
   $(MAKE) /$(MAKEFLAGS) /F .\Bitmap.mak CFG="Bitmap - Win32 Debug" 
   cd "..\IC"

"Bitmap - Win32 DebugCLEAN" : 
   cd "..\Bitmap"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Bitmap.mak CFG="Bitmap - Win32 Debug"\
 RECURSE=1 
   cd "..\IC"

!ENDIF 

!IF  "$(CFG)" == "IC - Win32 Release"

"Eclipse - Win32 Release" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) /F .\Eclipse.mak CFG="Eclipse - Win32 Release" 
   cd "..\IC"

"Eclipse - Win32 ReleaseCLEAN" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Eclipse.mak CFG="Eclipse - Win32 Release"\
 RECURSE=1 
   cd "..\IC"

!ELSEIF  "$(CFG)" == "IC - Win32 Debug"

"Eclipse - Win32 Debug" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) /F .\Eclipse.mak CFG="Eclipse - Win32 Debug" 
   cd "..\IC"

"Eclipse - Win32 DebugCLEAN" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Eclipse.mak CFG="Eclipse - Win32 Debug"\
 RECURSE=1 
   cd "..\IC"

!ENDIF 

SOURCE=..\..\icparc_solvers\ic.c

!IF  "$(CFG)" == "IC - Win32 Release"

DEP_CPP_IC_C0=\
	"..\..\icparc_solvers\bitmap.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\intervals.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	

"$(INTDIR)\ic.obj" : $(SOURCE) $(DEP_CPP_IC_C0) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "IC - Win32 Debug"

DEP_CPP_IC_C0=\
	"..\..\icparc_solvers\bitmap.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\external.h"\
	"..\..\sepia\i386_nt\intervals.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	

"$(INTDIR)\ic.obj" : $(SOURCE) $(DEP_CPP_IC_C0) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

