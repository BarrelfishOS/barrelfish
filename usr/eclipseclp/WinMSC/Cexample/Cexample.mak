# Microsoft Developer Studio Generated NMAKE File, Based on Cexample.dsp
!IF "$(CFG)" == ""
CFG=Cexample - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Cexample - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Cexample - Win32 Release" && "$(CFG)" !=\
 "Cexample - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Cexample.mak" CFG="Cexample - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Cexample - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "Cexample - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "Cexample - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Cexample.exe"

!ELSE 

ALL : "Eclipse - Win32 Release" "$(OUTDIR)\Cexample.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Eclipse - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\eg_c_test.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\Cexample.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D\
 "_MBCS" /Fp"$(INTDIR)\Cexample.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD\
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

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Cexample.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)\Cexample.pdb" /machine:I386 /out:"$(OUTDIR)\Cexample.exe"\
 /libpath:"../Eclipse/Release" 
LINK32_OBJS= \
	"$(INTDIR)\eg_c_test.obj" \
	"..\Eclipse\Release\Eclipse.lib"

"$(OUTDIR)\Cexample.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Cexample - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Cexample.exe"

!ELSE 

ALL : "Eclipse - Win32 Debug" "$(OUTDIR)\Cexample.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Eclipse - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\eg_c_test.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\Cexample.exe"
	-@erase "$(OUTDIR)\Cexample.ilk"
	-@erase "$(OUTDIR)\Cexample.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /D "WIN32"\
 /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\Cexample.pch" /YX\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
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

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Cexample.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)\Cexample.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)\Cexample.exe" /pdbtype:sept /libpath:"../Eclipse/Debug" 
LINK32_OBJS= \
	"$(INTDIR)\eg_c_test.obj" \
	"..\Eclipse\Debug\Eclipse.lib"

"$(OUTDIR)\Cexample.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(CFG)" == "Cexample - Win32 Release" || "$(CFG)" ==\
 "Cexample - Win32 Debug"

!IF  "$(CFG)" == "Cexample - Win32 Release"

"Eclipse - Win32 Release" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) /F .\Eclipse.mak CFG="Eclipse - Win32 Release" 
   cd "..\Cexample"

"Eclipse - Win32 ReleaseCLEAN" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Eclipse.mak CFG="Eclipse - Win32 Release"\
 RECURSE=1 
   cd "..\Cexample"

!ELSEIF  "$(CFG)" == "Cexample - Win32 Debug"

"Eclipse - Win32 Debug" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) /F .\Eclipse.mak CFG="Eclipse - Win32 Debug" 
   cd "..\Cexample"

"Eclipse - Win32 DebugCLEAN" : 
   cd "..\Eclipse"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Eclipse.mak CFG="Eclipse - Win32 Debug"\
 RECURSE=1 
   cd "..\Cexample"

!ENDIF 

SOURCE=..\..\sepia\i386_nt\eg_c_test.c
DEP_CPP_EG_C_=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\eclipse.h"\
	"..\..\sepia\i386_nt\embed.h"\
	

"$(INTDIR)\eg_c_test.obj" : $(SOURCE) $(DEP_CPP_EG_C_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

