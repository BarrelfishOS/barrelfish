# Microsoft Developer Studio Generated NMAKE File, Based on EcJavaLoad.dsp
!IF "$(CFG)" == ""
CFG=EcJavaLoad - Win32 Debug
!MESSAGE No configuration specified. Defaulting to EcJavaLoad - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "EcJavaLoad - Win32 Release" && "$(CFG)" !=\
 "EcJavaLoad - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "EcJavaLoad.mak" CFG="EcJavaLoad - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "EcJavaLoad - Win32 Release" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EcJavaLoad - Win32 Debug" (based on\
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

!IF  "$(CFG)" == "EcJavaLoad - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\ec_java_load.dll"

!ELSE 

ALL : "$(OUTDIR)\ec_java_load.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\ec_java_load.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\ec_java_load.dll"
	-@erase "$(OUTDIR)\ec_java_load.exp"
	-@erase "$(OUTDIR)\ec_java_load.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "D:/JBuilder3/java/include" /I\
 "D:/JBuilder3/java/include/win32" /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)\EcJavaLoad.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Release/
CPP_SBRS=.
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EcJavaLoad.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)\ec_java_load.pdb" /machine:I386\
 /out:"$(OUTDIR)\ec_java_load.dll" /implib:"$(OUTDIR)\ec_java_load.lib" 
LINK32_OBJS= \
	"$(INTDIR)\ec_java_load.obj"

"$(OUTDIR)\ec_java_load.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\ec_java_load.dll"
   copy Release\ec_java_load.dll                   ..\..\lib\i386_nt\ec_java_load.dll
	copy Release\ec_java_load.lib                   ..\..\lib\i386_nt\ec_java_load.lib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "EcJavaLoad - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\ec_java_load.dll"

!ELSE 

ALL : "$(OUTDIR)\ec_java_load.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\ec_java_load.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\ec_java_load.dll"
	-@erase "$(OUTDIR)\ec_java_load.exp"
	-@erase "$(OUTDIR)\ec_java_load.ilk"
	-@erase "$(OUTDIR)\ec_java_load.lib"
	-@erase "$(OUTDIR)\ec_java_load.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "D:/JBuilder3/java/include" /I\
 "D:/JBuilder3/java/include/win32" /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)\EcJavaLoad.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EcJavaLoad.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /dll /incremental:yes\
 /pdb:"$(OUTDIR)\ec_java_load.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)\ec_java_load.dll" /implib:"$(OUTDIR)\ec_java_load.lib"\
 /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\ec_java_load.obj"

"$(OUTDIR)\ec_java_load.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\ec_java_load.dll"
   copy Debug\ec_java_load.dll                   ..\..\lib\i386_nt\ec_java_load.dll
	copy Debug\ec_java_load.lib                   ..\..\lib\i386_nt\ec_java_load.lib
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


!IF "$(CFG)" == "EcJavaLoad - Win32 Release" || "$(CFG)" ==\
 "EcJavaLoad - Win32 Debug"
SOURCE=..\..\JavaInterface\ec_java_load.c

!IF  "$(CFG)" == "EcJavaLoad - Win32 Release"

DEP_CPP_EC_JA=\
	"..\..\JavaInterface\com_parctechnologies_eclipse_NativeEclipse.h"\
	"d:\jbuilder3\java\include\jni.h"\
	"d:\jbuilder3\java\include\win32\jni_md.h"\
	

"$(INTDIR)\ec_java_load.obj" : $(SOURCE) $(DEP_CPP_EC_JA) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "EcJavaLoad - Win32 Debug"

DEP_CPP_EC_JA=\
	"..\..\JavaInterface\com_parctechnologies_eclipse_NativeEclipse.h"\
	"d:\jbuilder3\java\include\jni.h"\
	"d:\jbuilder3\java\include\win32\jni_md.h"\
	

"$(INTDIR)\ec_java_load.obj" : $(SOURCE) $(DEP_CPP_EC_JA) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

