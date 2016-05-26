# Microsoft Developer Studio Generated NMAKE File, Based on Regex.dsp
!IF "$(CFG)" == ""
CFG=Regex - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Regex - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Regex - Win32 Release" && "$(CFG)" != "Regex - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Regex.mak" CFG="Regex - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Regex - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Regex - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
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

!IF  "$(CFG)" == "Regex - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Regex.dll"

!ELSE 

ALL : "Eclipse - Win32 Release" "$(OUTDIR)\Regex.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Eclipse - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\eregex.obj"
	-@erase "$(INTDIR)\regcomp.obj"
	-@erase "$(INTDIR)\regerror.obj"
	-@erase "$(INTDIR)\regexec.obj"
	-@erase "$(INTDIR)\regfree.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\Regex.dll"
	-@erase "$(OUTDIR)\Regex.exp"
	-@erase "$(OUTDIR)\Regex.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I\
 "../../icparc_solvers/rxspencer" /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)\Regex.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Release/
CPP_SBRS=.
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Regex.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)\Regex.pdb" /machine:I386 /def:"..\..\icparc_solvers\eregex.def"\
 /out:"$(OUTDIR)\Regex.dll" /implib:"$(OUTDIR)\Regex.lib"\
 /libpath:"../Eclipse/Release" 
DEF_FILE= \
	"..\..\icparc_solvers\eregex.def"
LINK32_OBJS= \
	"$(INTDIR)\eregex.obj" \
	"$(INTDIR)\regcomp.obj" \
	"$(INTDIR)\regerror.obj" \
	"$(INTDIR)\regexec.obj" \
	"$(INTDIR)\regfree.obj" \
	"..\Eclipse\Release\Eclipse.lib"

"$(OUTDIR)\Regex.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

$(DS_POSTBUILD_DEP) : "Eclipse - Win32 Release" "$(OUTDIR)\Regex.dll"
   copy  Release\Regex.dll ..\..\lib\i386_nt\eregex.dll
	copy     Release\Regex.lib   ..\..\lib\i386_nt\eregex.lib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "Regex - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Regex.dll"

!ELSE 

ALL : "Eclipse - Win32 Debug" "$(OUTDIR)\Regex.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Eclipse - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\eregex.obj"
	-@erase "$(INTDIR)\regcomp.obj"
	-@erase "$(INTDIR)\regerror.obj"
	-@erase "$(INTDIR)\regexec.obj"
	-@erase "$(INTDIR)\regfree.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\Regex.dll"
	-@erase "$(OUTDIR)\Regex.exp"
	-@erase "$(OUTDIR)\Regex.ilk"
	-@erase "$(OUTDIR)\Regex.lib"
	-@erase "$(OUTDIR)\Regex.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I\
 "../../icparc_solvers/rxspencer" /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)\Regex.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Regex.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /incremental:yes\
 /pdb:"$(OUTDIR)\Regex.pdb" /debug /machine:I386\
 /def:"..\..\icparc_solvers\eregex.def" /out:"$(OUTDIR)\Regex.dll"\
 /implib:"$(OUTDIR)\Regex.lib" /pdbtype:sept /libpath:"../Eclipse/Debug" 
DEF_FILE= \
	"..\..\icparc_solvers\eregex.def"
LINK32_OBJS= \
	"$(INTDIR)\eregex.obj" \
	"$(INTDIR)\regcomp.obj" \
	"$(INTDIR)\regerror.obj" \
	"$(INTDIR)\regexec.obj" \
	"$(INTDIR)\regfree.obj" \
	"..\Eclipse\Debug\Eclipse.lib"

"$(OUTDIR)\Regex.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

$(DS_POSTBUILD_DEP) : "Eclipse - Win32 Debug" "$(OUTDIR)\Regex.dll"
   copy  Debug\Regex.dll ..\..\lib\i386_nt\eregex.dll
	copy     Debug\Regex.lib ..\..\lib\i386_nt\eregex.lib
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


!IF "$(CFG)" == "Regex - Win32 Release" || "$(CFG)" == "Regex - Win32 Debug"

!IF  "$(CFG)" == "Regex - Win32 Release"

"Eclipse - Win32 Release" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) /F .\Eclipse.mak CFG="Eclipse - Win32 Release" 
   cd "..\Regex"

"Eclipse - Win32 ReleaseCLEAN" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Eclipse.mak CFG="Eclipse - Win32 Release"\
 RECURSE=1 
   cd "..\Regex"

!ELSEIF  "$(CFG)" == "Regex - Win32 Debug"

"Eclipse - Win32 Debug" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) /F .\Eclipse.mak CFG="Eclipse - Win32 Debug" 
   cd "..\Regex"

"Eclipse - Win32 DebugCLEAN" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Eclipse.mak CFG="Eclipse - Win32 Debug"\
 RECURSE=1 
   cd "..\Regex"

!ENDIF 

SOURCE=..\..\icparc_solvers\eregex.c

!IF  "$(CFG)" == "Regex - Win32 Release"

DEP_CPP_EREGE=\
	"..\..\icparc_solvers\rxspencer\regex.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\eclipse.h"\
	"..\..\sepia\i386_nt\embed.h"\
	

"$(INTDIR)\eregex.obj" : $(SOURCE) $(DEP_CPP_EREGE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Regex - Win32 Debug"

DEP_CPP_EREGE=\
	"..\..\icparc_solvers\rxspencer\regex.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\eclipse.h"\
	"..\..\sepia\i386_nt\embed.h"\
	

"$(INTDIR)\eregex.obj" : $(SOURCE) $(DEP_CPP_EREGE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\icparc_solvers\rxspencer\regcomp.c

!IF  "$(CFG)" == "Regex - Win32 Release"

DEP_CPP_REGCO=\
	"..\..\icparc_solvers\rxspencer\cclass.h"\
	"..\..\icparc_solvers\rxspencer\cname.h"\
	"..\..\icparc_solvers\rxspencer\regcomp.ih"\
	"..\..\icparc_solvers\rxspencer\regex.h"\
	"..\..\icparc_solvers\rxspencer\regex2.h"\
	"..\..\icparc_solvers\rxspencer\utils.h"\
	

"$(INTDIR)\regcomp.obj" : $(SOURCE) $(DEP_CPP_REGCO) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Regex - Win32 Debug"

DEP_CPP_REGCO=\
	"..\..\icparc_solvers\rxspencer\cclass.h"\
	"..\..\icparc_solvers\rxspencer\cname.h"\
	"..\..\icparc_solvers\rxspencer\regcomp.ih"\
	"..\..\icparc_solvers\rxspencer\regex.h"\
	"..\..\icparc_solvers\rxspencer\regex2.h"\
	"..\..\icparc_solvers\rxspencer\utils.h"\
	

"$(INTDIR)\regcomp.obj" : $(SOURCE) $(DEP_CPP_REGCO) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\icparc_solvers\rxspencer\regerror.c

!IF  "$(CFG)" == "Regex - Win32 Release"

DEP_CPP_REGER=\
	"..\..\icparc_solvers\rxspencer\regerror.ih"\
	"..\..\icparc_solvers\rxspencer\regex.h"\
	"..\..\icparc_solvers\rxspencer\utils.h"\
	

"$(INTDIR)\regerror.obj" : $(SOURCE) $(DEP_CPP_REGER) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Regex - Win32 Debug"

DEP_CPP_REGER=\
	"..\..\icparc_solvers\rxspencer\regerror.ih"\
	"..\..\icparc_solvers\rxspencer\regex.h"\
	"..\..\icparc_solvers\rxspencer\utils.h"\
	

"$(INTDIR)\regerror.obj" : $(SOURCE) $(DEP_CPP_REGER) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\icparc_solvers\rxspencer\regexec.c

!IF  "$(CFG)" == "Regex - Win32 Release"

DEP_CPP_REGEX=\
	"..\..\icparc_solvers\rxspencer\engine.c"\
	"..\..\icparc_solvers\rxspencer\engine.ih"\
	"..\..\icparc_solvers\rxspencer\regex.h"\
	"..\..\icparc_solvers\rxspencer\regex2.h"\
	"..\..\icparc_solvers\rxspencer\utils.h"\
	

"$(INTDIR)\regexec.obj" : $(SOURCE) $(DEP_CPP_REGEX) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Regex - Win32 Debug"

DEP_CPP_REGEX=\
	"..\..\icparc_solvers\rxspencer\engine.c"\
	"..\..\icparc_solvers\rxspencer\engine.ih"\
	"..\..\icparc_solvers\rxspencer\regex.h"\
	"..\..\icparc_solvers\rxspencer\regex2.h"\
	"..\..\icparc_solvers\rxspencer\utils.h"\
	

"$(INTDIR)\regexec.obj" : $(SOURCE) $(DEP_CPP_REGEX) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\icparc_solvers\rxspencer\regfree.c

!IF  "$(CFG)" == "Regex - Win32 Release"

DEP_CPP_REGFR=\
	"..\..\icparc_solvers\rxspencer\regex.h"\
	"..\..\icparc_solvers\rxspencer\regex2.h"\
	"..\..\icparc_solvers\rxspencer\utils.h"\
	

"$(INTDIR)\regfree.obj" : $(SOURCE) $(DEP_CPP_REGFR) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Regex - Win32 Debug"

DEP_CPP_REGFR=\
	"..\..\icparc_solvers\rxspencer\regex.h"\
	"..\..\icparc_solvers\rxspencer\regex2.h"\
	"..\..\icparc_solvers\rxspencer\utils.h"\
	

"$(INTDIR)\regfree.obj" : $(SOURCE) $(DEP_CPP_REGFR) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

