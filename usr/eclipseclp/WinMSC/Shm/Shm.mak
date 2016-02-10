# Microsoft Developer Studio Generated NMAKE File, Based on Shm.dsp
!IF "$(CFG)" == ""
CFG=Shm - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Shm - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Shm - Win32 Release" && "$(CFG)" != "Shm - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Shm.mak" CFG="Shm - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Shm - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "Shm - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "Shm - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Shm.lib"

!ELSE 

ALL : "$(OUTDIR)\Shm.lib"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\alloc.obj"
	-@erase "$(INTDIR)\mutex.obj"
	-@erase "$(INTDIR)\private_mem.obj"
	-@erase "$(INTDIR)\shared_mem.obj"
	-@erase "$(INTDIR)\shmem_base.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\Shm.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)\Shm.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
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

BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Shm.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\Shm.lib" 
LIB32_OBJS= \
	"$(INTDIR)\alloc.obj" \
	"$(INTDIR)\mutex.obj" \
	"$(INTDIR)\private_mem.obj" \
	"$(INTDIR)\shared_mem.obj" \
	"$(INTDIR)\shmem_base.obj"

"$(OUTDIR)\Shm.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Shm - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Shm.lib"

!ELSE 

ALL : "$(OUTDIR)\Shm.lib"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\alloc.obj"
	-@erase "$(INTDIR)\mutex.obj"
	-@erase "$(INTDIR)\private_mem.obj"
	-@erase "$(INTDIR)\shared_mem.obj"
	-@erase "$(INTDIR)\shmem_base.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\Shm.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)\Shm.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
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

BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Shm.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\Shm.lib" 
LIB32_OBJS= \
	"$(INTDIR)\alloc.obj" \
	"$(INTDIR)\mutex.obj" \
	"$(INTDIR)\private_mem.obj" \
	"$(INTDIR)\shared_mem.obj" \
	"$(INTDIR)\shmem_base.obj"

"$(OUTDIR)\Shm.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 


!IF "$(CFG)" == "Shm - Win32 Release" || "$(CFG)" == "Shm - Win32 Debug"
SOURCE=..\..\Shm\i386_nt\alloc.c
DEP_CPP_ALLOC=\
	"..\..\Shm\i386_nt\config.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\alloc.obj" : $(SOURCE) $(DEP_CPP_ALLOC) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Shm\i386_nt\mutex.c
DEP_CPP_MUTEX=\
	"..\..\Shm\i386_nt\config.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\mutex.obj" : $(SOURCE) $(DEP_CPP_MUTEX) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Shm\i386_nt\private_mem.c
DEP_CPP_PRIVA=\
	"..\..\Shm\i386_nt\config.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\private_mem.obj" : $(SOURCE) $(DEP_CPP_PRIVA) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Shm\i386_nt\shared_mem.c
DEP_CPP_SHARE=\
	"..\..\Shm\i386_nt\config.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\shared_mem.obj" : $(SOURCE) $(DEP_CPP_SHARE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Shm\i386_nt\shmem_base.c

"$(INTDIR)\shmem_base.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

