# Microsoft Developer Studio Generated NMAKE File, Based on Eclipse.dsp
!IF "$(CFG)" == ""
CFG=Eclipse - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Eclipse - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Eclipse - Win32 Release" && "$(CFG)" !=\
 "Eclipse - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Eclipse.mak" CFG="Eclipse - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Eclipse - Win32 Release" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Eclipse - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
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

!IF  "$(CFG)" == "Eclipse - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Eclipse.dll"

!ELSE 

ALL : "GmpDll - Win32 Release" "Shm - Win32 Release" "$(OUTDIR)\Eclipse.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Shm - Win32 ReleaseCLEAN" "GmpDll - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\bigrat.obj"
	-@erase "$(INTDIR)\bip_arith.obj"
	-@erase "$(INTDIR)\bip_array.obj"
	-@erase "$(INTDIR)\bip_comp.obj"
	-@erase "$(INTDIR)\bip_control.obj"
	-@erase "$(INTDIR)\bip_db.obj"
	-@erase "$(INTDIR)\bip_delay.obj"
	-@erase "$(INTDIR)\bip_domain.obj"
	-@erase "$(INTDIR)\bip_io.obj"
	-@erase "$(INTDIR)\bip_load.obj"
	-@erase "$(INTDIR)\bip_misc.obj"
	-@erase "$(INTDIR)\bip_module.obj"
	-@erase "$(INTDIR)\bip_record.obj"
	-@erase "$(INTDIR)\bip_strings.obj"
	-@erase "$(INTDIR)\bip_tconv.obj"
	-@erase "$(INTDIR)\body.obj"
	-@erase "$(INTDIR)\code.obj"
	-@erase "$(INTDIR)\dict.obj"
	-@erase "$(INTDIR)\dummy_mps.obj"
	-@erase "$(INTDIR)\dummy_par.obj"
	-@erase "$(INTDIR)\dummy_upcalls.obj"
	-@erase "$(INTDIR)\dummy_wm.obj"
	-@erase "$(INTDIR)\dynamic.obj"
	-@erase "$(INTDIR)\eclipsedir.obj"
	-@erase "$(INTDIR)\elipsys_fd.obj"
	-@erase "$(INTDIR)\embed.obj"
	-@erase "$(INTDIR)\emu_c_env.obj"
	-@erase "$(INTDIR)\emu_util.obj"
	-@erase "$(INTDIR)\error.obj"
	-@erase "$(INTDIR)\external.obj"
	-@erase "$(INTDIR)\gc_stacks.obj"
	-@erase "$(INTDIR)\handle.obj"
	-@erase "$(INTDIR)\handlers.obj"
	-@erase "$(INTDIR)\head.obj"
	-@erase "$(INTDIR)\init.obj"
	-@erase "$(INTDIR)\intervals.obj"
	-@erase "$(INTDIR)\io.obj"
	-@erase "$(INTDIR)\lex.obj"
	-@erase "$(INTDIR)\lib1.obj"
	-@erase "$(INTDIR)\lib2.obj"
	-@erase "$(INTDIR)\lib3.obj"
	-@erase "$(INTDIR)\lib4.obj"
	-@erase "$(INTDIR)\lib5.obj"
	-@erase "$(INTDIR)\lib6.obj"
	-@erase "$(INTDIR)\lib7.obj"
	-@erase "$(INTDIR)\mem.obj"
	-@erase "$(INTDIR)\operator.obj"
	-@erase "$(INTDIR)\os_support.obj"
	-@erase "$(INTDIR)\pass2.obj"
	-@erase "$(INTDIR)\pass3.obj"
	-@erase "$(INTDIR)\pass4.obj"
	-@erase "$(INTDIR)\printam.obj"
	-@erase "$(INTDIR)\proc_desc.obj"
	-@erase "$(INTDIR)\procedure.obj"
	-@erase "$(INTDIR)\property.obj"
	-@erase "$(INTDIR)\read.obj"
	-@erase "$(INTDIR)\section.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\whereami_default.obj"
	-@erase "$(INTDIR)\write.obj"
	-@erase "$(OUTDIR)\Eclipse.dll"
	-@erase "$(OUTDIR)\Eclipse.exp"
	-@erase "$(OUTDIR)\Eclipse.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W2 /GX /O2 /I "../../Shm/i386_nt" /I\
 "../../Gmp/src/win32eclipse" /D "THREADED" /D "DEBUG_COMP" /D "OC" /D "DFID" /D\
 "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\Eclipse.pch" /YX\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Release/
CPP_SBRS=.
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Eclipse.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=gmp33.lib shm.lib ws2_32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib netapi32.lib /nologo /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)\Eclipse.pdb" /machine:I386\
 /def:"..\..\sepia\i386_nt\eclipse.def" /out:"$(OUTDIR)\Eclipse.dll"\
 /implib:"$(OUTDIR)\Eclipse.lib" /libpath:"../Shm/Release"\
 /libpath:"../../lib/i386_nt" 
DEF_FILE= \
	"..\..\sepia\i386_nt\eclipse.def"
LINK32_OBJS= \
	"$(INTDIR)\bigrat.obj" \
	"$(INTDIR)\bip_arith.obj" \
	"$(INTDIR)\bip_array.obj" \
	"$(INTDIR)\bip_comp.obj" \
	"$(INTDIR)\bip_control.obj" \
	"$(INTDIR)\bip_db.obj" \
	"$(INTDIR)\bip_delay.obj" \
	"$(INTDIR)\bip_domain.obj" \
	"$(INTDIR)\bip_io.obj" \
	"$(INTDIR)\bip_load.obj" \
	"$(INTDIR)\bip_misc.obj" \
	"$(INTDIR)\bip_module.obj" \
	"$(INTDIR)\bip_record.obj" \
	"$(INTDIR)\bip_strings.obj" \
	"$(INTDIR)\bip_tconv.obj" \
	"$(INTDIR)\body.obj" \
	"$(INTDIR)\code.obj" \
	"$(INTDIR)\dict.obj" \
	"$(INTDIR)\dummy_mps.obj" \
	"$(INTDIR)\dummy_par.obj" \
	"$(INTDIR)\dummy_upcalls.obj" \
	"$(INTDIR)\dummy_wm.obj" \
	"$(INTDIR)\dynamic.obj" \
	"$(INTDIR)\eclipsedir.obj" \
	"$(INTDIR)\elipsys_fd.obj" \
	"$(INTDIR)\embed.obj" \
	"$(INTDIR)\emu_c_env.obj" \
	"$(INTDIR)\emu_util.obj" \
	"$(INTDIR)\error.obj" \
	"$(INTDIR)\external.obj" \
	"$(INTDIR)\gc_stacks.obj" \
	"$(INTDIR)\handle.obj" \
	"$(INTDIR)\handlers.obj" \
	"$(INTDIR)\head.obj" \
	"$(INTDIR)\init.obj" \
	"$(INTDIR)\intervals.obj" \
	"$(INTDIR)\io.obj" \
	"$(INTDIR)\lex.obj" \
	"$(INTDIR)\lib1.obj" \
	"$(INTDIR)\lib2.obj" \
	"$(INTDIR)\lib3.obj" \
	"$(INTDIR)\lib4.obj" \
	"$(INTDIR)\lib5.obj" \
	"$(INTDIR)\lib6.obj" \
	"$(INTDIR)\lib7.obj" \
	"$(INTDIR)\mem.obj" \
	"$(INTDIR)\operator.obj" \
	"$(INTDIR)\os_support.obj" \
	"$(INTDIR)\pass2.obj" \
	"$(INTDIR)\pass3.obj" \
	"$(INTDIR)\pass4.obj" \
	"$(INTDIR)\printam.obj" \
	"$(INTDIR)\proc_desc.obj" \
	"$(INTDIR)\procedure.obj" \
	"$(INTDIR)\property.obj" \
	"$(INTDIR)\read.obj" \
	"$(INTDIR)\section.obj" \
	"$(INTDIR)\whereami_default.obj" \
	"$(INTDIR)\write.obj" \
	"..\..\sepia\i386_nt\emu.obj" \
	"..\GmpDll\Release\gmp33.lib" \
	"..\Shm\Release\Shm.lib"

"$(OUTDIR)\Eclipse.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying eclipse.dll somewhere sensible
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

$(DS_POSTBUILD_DEP) : "GmpDll - Win32 Release" "Shm - Win32 Release"\
 "$(OUTDIR)\Eclipse.dll"
   copy      Release\Eclipse.dll  ..\..\lib\i386_nt\eclipse.dll
	copy     Release\Eclipse.lib       ..\..\lib\i386_nt\eclipse.lib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Eclipse.dll"

!ELSE 

ALL : "GmpDll - Win32 Debug" "Shm - Win32 Debug" "$(OUTDIR)\Eclipse.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Shm - Win32 DebugCLEAN" "GmpDll - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\bigrat.obj"
	-@erase "$(INTDIR)\bip_arith.obj"
	-@erase "$(INTDIR)\bip_array.obj"
	-@erase "$(INTDIR)\bip_comp.obj"
	-@erase "$(INTDIR)\bip_control.obj"
	-@erase "$(INTDIR)\bip_db.obj"
	-@erase "$(INTDIR)\bip_delay.obj"
	-@erase "$(INTDIR)\bip_domain.obj"
	-@erase "$(INTDIR)\bip_io.obj"
	-@erase "$(INTDIR)\bip_load.obj"
	-@erase "$(INTDIR)\bip_misc.obj"
	-@erase "$(INTDIR)\bip_module.obj"
	-@erase "$(INTDIR)\bip_record.obj"
	-@erase "$(INTDIR)\bip_strings.obj"
	-@erase "$(INTDIR)\bip_tconv.obj"
	-@erase "$(INTDIR)\body.obj"
	-@erase "$(INTDIR)\code.obj"
	-@erase "$(INTDIR)\dict.obj"
	-@erase "$(INTDIR)\dummy_mps.obj"
	-@erase "$(INTDIR)\dummy_par.obj"
	-@erase "$(INTDIR)\dummy_upcalls.obj"
	-@erase "$(INTDIR)\dummy_wm.obj"
	-@erase "$(INTDIR)\dynamic.obj"
	-@erase "$(INTDIR)\eclipsedir.obj"
	-@erase "$(INTDIR)\elipsys_fd.obj"
	-@erase "$(INTDIR)\embed.obj"
	-@erase "$(INTDIR)\emu_c_env.obj"
	-@erase "$(INTDIR)\emu_util.obj"
	-@erase "$(INTDIR)\error.obj"
	-@erase "$(INTDIR)\external.obj"
	-@erase "$(INTDIR)\gc_stacks.obj"
	-@erase "$(INTDIR)\handle.obj"
	-@erase "$(INTDIR)\handlers.obj"
	-@erase "$(INTDIR)\head.obj"
	-@erase "$(INTDIR)\init.obj"
	-@erase "$(INTDIR)\intervals.obj"
	-@erase "$(INTDIR)\io.obj"
	-@erase "$(INTDIR)\lex.obj"
	-@erase "$(INTDIR)\lib1.obj"
	-@erase "$(INTDIR)\lib2.obj"
	-@erase "$(INTDIR)\lib3.obj"
	-@erase "$(INTDIR)\lib4.obj"
	-@erase "$(INTDIR)\lib5.obj"
	-@erase "$(INTDIR)\lib6.obj"
	-@erase "$(INTDIR)\lib7.obj"
	-@erase "$(INTDIR)\mem.obj"
	-@erase "$(INTDIR)\operator.obj"
	-@erase "$(INTDIR)\os_support.obj"
	-@erase "$(INTDIR)\pass2.obj"
	-@erase "$(INTDIR)\pass3.obj"
	-@erase "$(INTDIR)\pass4.obj"
	-@erase "$(INTDIR)\printam.obj"
	-@erase "$(INTDIR)\proc_desc.obj"
	-@erase "$(INTDIR)\procedure.obj"
	-@erase "$(INTDIR)\property.obj"
	-@erase "$(INTDIR)\read.obj"
	-@erase "$(INTDIR)\section.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(INTDIR)\whereami_default.obj"
	-@erase "$(INTDIR)\write.obj"
	-@erase "$(OUTDIR)\Eclipse.dll"
	-@erase "$(OUTDIR)\Eclipse.exp"
	-@erase "$(OUTDIR)\Eclipse.lib"
	-@erase "$(OUTDIR)\Eclipse.map"
	-@erase "$(OUTDIR)\Eclipse.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W2 /Gm /GX /Zi /I "../../Shm/i386_nt" /I\
 "../../Gmp/src/win32eclipse" /D "THREADED" /D "DEBUG_COMP" /D "OC" /D "DFID" /D\
 "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\Eclipse.pch" /YX\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Eclipse.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=gmp33.lib shm.lib ws2_32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib netapi32.lib /nologo /version:5.9\
 /subsystem:windows /dll /incremental:no /pdb:"$(OUTDIR)\Eclipse.pdb"\
 /map:"$(INTDIR)\Eclipse.map" /debug /machine:I386\
 /def:"..\..\sepia\i386_nt\eclipse.def" /out:"$(OUTDIR)\Eclipse.dll"\
 /implib:"$(OUTDIR)\Eclipse.lib" /pdbtype:sept /libpath:"../Shm/Debug"\
 /libpath:"../../lib/i386_nt" 
DEF_FILE= \
	"..\..\sepia\i386_nt\eclipse.def"
LINK32_OBJS= \
	"$(INTDIR)\bigrat.obj" \
	"$(INTDIR)\bip_arith.obj" \
	"$(INTDIR)\bip_array.obj" \
	"$(INTDIR)\bip_comp.obj" \
	"$(INTDIR)\bip_control.obj" \
	"$(INTDIR)\bip_db.obj" \
	"$(INTDIR)\bip_delay.obj" \
	"$(INTDIR)\bip_domain.obj" \
	"$(INTDIR)\bip_io.obj" \
	"$(INTDIR)\bip_load.obj" \
	"$(INTDIR)\bip_misc.obj" \
	"$(INTDIR)\bip_module.obj" \
	"$(INTDIR)\bip_record.obj" \
	"$(INTDIR)\bip_strings.obj" \
	"$(INTDIR)\bip_tconv.obj" \
	"$(INTDIR)\body.obj" \
	"$(INTDIR)\code.obj" \
	"$(INTDIR)\dict.obj" \
	"$(INTDIR)\dummy_mps.obj" \
	"$(INTDIR)\dummy_par.obj" \
	"$(INTDIR)\dummy_upcalls.obj" \
	"$(INTDIR)\dummy_wm.obj" \
	"$(INTDIR)\dynamic.obj" \
	"$(INTDIR)\eclipsedir.obj" \
	"$(INTDIR)\elipsys_fd.obj" \
	"$(INTDIR)\embed.obj" \
	"$(INTDIR)\emu_c_env.obj" \
	"$(INTDIR)\emu_util.obj" \
	"$(INTDIR)\error.obj" \
	"$(INTDIR)\external.obj" \
	"$(INTDIR)\gc_stacks.obj" \
	"$(INTDIR)\handle.obj" \
	"$(INTDIR)\handlers.obj" \
	"$(INTDIR)\head.obj" \
	"$(INTDIR)\init.obj" \
	"$(INTDIR)\intervals.obj" \
	"$(INTDIR)\io.obj" \
	"$(INTDIR)\lex.obj" \
	"$(INTDIR)\lib1.obj" \
	"$(INTDIR)\lib2.obj" \
	"$(INTDIR)\lib3.obj" \
	"$(INTDIR)\lib4.obj" \
	"$(INTDIR)\lib5.obj" \
	"$(INTDIR)\lib6.obj" \
	"$(INTDIR)\lib7.obj" \
	"$(INTDIR)\mem.obj" \
	"$(INTDIR)\operator.obj" \
	"$(INTDIR)\os_support.obj" \
	"$(INTDIR)\pass2.obj" \
	"$(INTDIR)\pass3.obj" \
	"$(INTDIR)\pass4.obj" \
	"$(INTDIR)\printam.obj" \
	"$(INTDIR)\proc_desc.obj" \
	"$(INTDIR)\procedure.obj" \
	"$(INTDIR)\property.obj" \
	"$(INTDIR)\read.obj" \
	"$(INTDIR)\section.obj" \
	"$(INTDIR)\whereami_default.obj" \
	"$(INTDIR)\write.obj" \
	"..\..\sepia\i386_nt\emu.obj" \
	"..\GmpDll\Debug\gmp33.lib" \
	"..\Shm\Debug\Shm.lib"

"$(OUTDIR)\Eclipse.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying eclipse.dll somewhere sensible
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

$(DS_POSTBUILD_DEP) : "GmpDll - Win32 Debug" "Shm - Win32 Debug"\
 "$(OUTDIR)\Eclipse.dll"
   copy         Debug\Eclipse.dll  ..\..\lib\i386_nt\eclipse.dll
	copy Debug\Eclipse.lib          ..\..\lib\i386_nt\eclipse.lib
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


!IF "$(CFG)" == "Eclipse - Win32 Release" || "$(CFG)" ==\
 "Eclipse - Win32 Debug"

!IF  "$(CFG)" == "Eclipse - Win32 Release"

"Shm - Win32 Release" : 
   cd "\EclipseDev\WinMSC\Shm"
   $(MAKE) /$(MAKEFLAGS) /F .\Shm.mak CFG="Shm - Win32 Release" 
   cd "..\Eclipse"

"Shm - Win32 ReleaseCLEAN" : 
   cd "\EclipseDev\WinMSC\Shm"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Shm.mak CFG="Shm - Win32 Release" RECURSE=1\
 
   cd "..\Eclipse"

!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

"Shm - Win32 Debug" : 
   cd "\EclipseDev\WinMSC\Shm"
   $(MAKE) /$(MAKEFLAGS) /F .\Shm.mak CFG="Shm - Win32 Debug" 
   cd "..\Eclipse"

"Shm - Win32 DebugCLEAN" : 
   cd "\EclipseDev\WinMSC\Shm"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\Shm.mak CFG="Shm - Win32 Debug" RECURSE=1 
   cd "..\Eclipse"

!ENDIF 

!IF  "$(CFG)" == "Eclipse - Win32 Release"

"GmpDll - Win32 Release" : 
   cd "\EclipseDev\WinMSC\GmpDll"
   $(MAKE) /$(MAKEFLAGS) /F .\GmpDll.mak CFG="GmpDll - Win32 Release" 
   cd "..\Eclipse"

"GmpDll - Win32 ReleaseCLEAN" : 
   cd "\EclipseDev\WinMSC\GmpDll"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\GmpDll.mak CFG="GmpDll - Win32 Release"\
 RECURSE=1 
   cd "..\Eclipse"

!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

"GmpDll - Win32 Debug" : 
   cd "\EclipseDev\WinMSC\GmpDll"
   $(MAKE) /$(MAKEFLAGS) /F .\GmpDll.mak CFG="GmpDll - Win32 Debug" 
   cd "..\Eclipse"

"GmpDll - Win32 DebugCLEAN" : 
   cd "\EclipseDev\WinMSC\GmpDll"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\GmpDll.mak CFG="GmpDll - Win32 Debug"\
 RECURSE=1 
   cd "..\Eclipse"

!ENDIF 

SOURCE=..\..\sepia\i386_nt\bigrat.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BIGRA=\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bigrat.obj" : $(SOURCE) $(DEP_CPP_BIGRA) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BIGRA=\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bigrat.obj" : $(SOURCE) $(DEP_CPP_BIGRA) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\bip_arith.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BIP_A=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\rounding_control.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_arith.obj" : $(SOURCE) $(DEP_CPP_BIP_A) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BIP_A=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\rounding_control.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_arith.obj" : $(SOURCE) $(DEP_CPP_BIP_A) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\bip_array.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BIP_AR=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_array.obj" : $(SOURCE) $(DEP_CPP_BIP_AR) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BIP_AR=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_array.obj" : $(SOURCE) $(DEP_CPP_BIP_AR) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\bip_comp.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BIP_C=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_comp.obj" : $(SOURCE) $(DEP_CPP_BIP_C) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BIP_C=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_comp.obj" : $(SOURCE) $(DEP_CPP_BIP_C) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\bip_control.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BIP_CO=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_control.obj" : $(SOURCE) $(DEP_CPP_BIP_CO) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BIP_CO=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_control.obj" : $(SOURCE) $(DEP_CPP_BIP_CO) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\bip_db.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BIP_D=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\dynamic.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_db.obj" : $(SOURCE) $(DEP_CPP_BIP_D) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BIP_D=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\dynamic.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_db.obj" : $(SOURCE) $(DEP_CPP_BIP_D) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\bip_delay.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BIP_DE=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_delay.obj" : $(SOURCE) $(DEP_CPP_BIP_DE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BIP_DE=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_delay.obj" : $(SOURCE) $(DEP_CPP_BIP_DE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\bip_domain.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BIP_DO=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\fd.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_domain.obj" : $(SOURCE) $(DEP_CPP_BIP_DO) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BIP_DO=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\fd.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_domain.obj" : $(SOURCE) $(DEP_CPP_BIP_DO) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\bip_io.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BIP_I=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\lex.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_io.obj" : $(SOURCE) $(DEP_CPP_BIP_I) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BIP_I=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\lex.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_io.obj" : $(SOURCE) $(DEP_CPP_BIP_I) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\bip_load.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BIP_L=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_load.obj" : $(SOURCE) $(DEP_CPP_BIP_L) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BIP_L=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_load.obj" : $(SOURCE) $(DEP_CPP_BIP_L) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\bip_misc.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BIP_M=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_misc.obj" : $(SOURCE) $(DEP_CPP_BIP_M) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BIP_M=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_misc.obj" : $(SOURCE) $(DEP_CPP_BIP_M) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\bip_module.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BIP_MO=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\dynamic.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_module.obj" : $(SOURCE) $(DEP_CPP_BIP_MO) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BIP_MO=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\dynamic.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_module.obj" : $(SOURCE) $(DEP_CPP_BIP_MO) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\bip_record.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BIP_R=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_record.obj" : $(SOURCE) $(DEP_CPP_BIP_R) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BIP_R=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_record.obj" : $(SOURCE) $(DEP_CPP_BIP_R) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\bip_strings.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BIP_S=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_strings.obj" : $(SOURCE) $(DEP_CPP_BIP_S) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BIP_S=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_strings.obj" : $(SOURCE) $(DEP_CPP_BIP_S) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\bip_tconv.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BIP_T=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_tconv.obj" : $(SOURCE) $(DEP_CPP_BIP_T) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BIP_T=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\bip_tconv.obj" : $(SOURCE) $(DEP_CPP_BIP_T) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\body.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_BODY_=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\body.obj" : $(SOURCE) $(DEP_CPP_BODY_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_BODY_=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\body.obj" : $(SOURCE) $(DEP_CPP_BODY_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\code.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_CODE_=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\code.obj" : $(SOURCE) $(DEP_CPP_CODE_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_CODE_=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\code.obj" : $(SOURCE) $(DEP_CPP_CODE_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\dict.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_DICT_=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\dict.obj" : $(SOURCE) $(DEP_CPP_DICT_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_DICT_=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\dict.obj" : $(SOURCE) $(DEP_CPP_DICT_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\dummy_mps.c

"$(INTDIR)\dummy_mps.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\sepia\i386_nt\dummy_par.c
DEP_CPP_DUMMY=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	

"$(INTDIR)\dummy_par.obj" : $(SOURCE) $(DEP_CPP_DUMMY) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\sepia\i386_nt\dummy_upcalls.c

"$(INTDIR)\dummy_upcalls.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\sepia\i386_nt\dummy_wm.c

"$(INTDIR)\dummy_wm.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\sepia\i386_nt\dynamic.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_DYNAM=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\dynamic.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\dynamic.obj" : $(SOURCE) $(DEP_CPP_DYNAM) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_DYNAM=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\dynamic.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\dynamic.obj" : $(SOURCE) $(DEP_CPP_DYNAM) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\eclipsedir.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_ECLIP=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	

"$(INTDIR)\eclipsedir.obj" : $(SOURCE) $(DEP_CPP_ECLIP) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_ECLIP=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	

"$(INTDIR)\eclipsedir.obj" : $(SOURCE) $(DEP_CPP_ECLIP) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\elipsys_fd.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_ELIPS=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\fd.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\elipsys_fd.obj" : $(SOURCE) $(DEP_CPP_ELIPS) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_ELIPS=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\fd.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\elipsys_fd.obj" : $(SOURCE) $(DEP_CPP_ELIPS) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\embed.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_EMBED=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\embed.obj" : $(SOURCE) $(DEP_CPP_EMBED) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_EMBED=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\embed.obj" : $(SOURCE) $(DEP_CPP_EMBED) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\emu_c_env.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_EMU_C=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\emu_c_env.obj" : $(SOURCE) $(DEP_CPP_EMU_C) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_EMU_C=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\emu_c_env.obj" : $(SOURCE) $(DEP_CPP_EMU_C) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\emu_util.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_EMU_U=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sav_res.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\emu_util.obj" : $(SOURCE) $(DEP_CPP_EMU_U) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_EMU_U=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sav_res.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\emu_util.obj" : $(SOURCE) $(DEP_CPP_EMU_U) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\error.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_ERROR=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\error.obj" : $(SOURCE) $(DEP_CPP_ERROR) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_ERROR=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\error.obj" : $(SOURCE) $(DEP_CPP_ERROR) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\external.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_EXTER=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\external.obj" : $(SOURCE) $(DEP_CPP_EXTER) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_EXTER=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\external.obj" : $(SOURCE) $(DEP_CPP_EXTER) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\gc_stacks.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_GC_ST=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\gc_stacks.obj" : $(SOURCE) $(DEP_CPP_GC_ST) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_GC_ST=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\gc_stacks.obj" : $(SOURCE) $(DEP_CPP_GC_ST) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\handle.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_HANDL=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\handle.obj" : $(SOURCE) $(DEP_CPP_HANDL) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_HANDL=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\handle.obj" : $(SOURCE) $(DEP_CPP_HANDL) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\handlers.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_HANDLE=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\handlers.obj" : $(SOURCE) $(DEP_CPP_HANDLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_HANDLE=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\handlers.obj" : $(SOURCE) $(DEP_CPP_HANDLE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\head.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_HEAD_=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\head.obj" : $(SOURCE) $(DEP_CPP_HEAD_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_HEAD_=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\head.obj" : $(SOURCE) $(DEP_CPP_HEAD_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\init.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_INIT_=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\init.obj" : $(SOURCE) $(DEP_CPP_INIT_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_INIT_=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\init.obj" : $(SOURCE) $(DEP_CPP_INIT_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\intervals.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_INTER=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\intervals.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\rounding_control.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\intervals.obj" : $(SOURCE) $(DEP_CPP_INTER) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_INTER=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\intervals.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\rounding_control.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\intervals.obj" : $(SOURCE) $(DEP_CPP_INTER) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\io.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_IO_C42=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\lex.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\io.obj" : $(SOURCE) $(DEP_CPP_IO_C42) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_IO_C42=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\lex.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\io.obj" : $(SOURCE) $(DEP_CPP_IO_C42) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\lex.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_LEX_C=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\lex.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\rounding_control.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\lex.obj" : $(SOURCE) $(DEP_CPP_LEX_C) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_LEX_C=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\lex.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\rounding_control.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\lex.obj" : $(SOURCE) $(DEP_CPP_LEX_C) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\lib1.c

"$(INTDIR)\lib1.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\sepia\i386_nt\lib2.c

"$(INTDIR)\lib2.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\sepia\i386_nt\lib3.c

"$(INTDIR)\lib3.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\sepia\i386_nt\lib4.c

"$(INTDIR)\lib4.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\sepia\i386_nt\lib5.c

"$(INTDIR)\lib5.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\sepia\i386_nt\lib6.c

"$(INTDIR)\lib6.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\sepia\i386_nt\lib7.c

"$(INTDIR)\lib7.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\sepia\i386_nt\mem.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_MEM_C=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\mem.obj" : $(SOURCE) $(DEP_CPP_MEM_C) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_MEM_C=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\mem.obj" : $(SOURCE) $(DEP_CPP_MEM_C) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\operator.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_OPERA=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\operator.obj" : $(SOURCE) $(DEP_CPP_OPERA) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_OPERA=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\operator.obj" : $(SOURCE) $(DEP_CPP_OPERA) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\os_support.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_OS_SU=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	

"$(INTDIR)\os_support.obj" : $(SOURCE) $(DEP_CPP_OS_SU) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_OS_SU=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	

"$(INTDIR)\os_support.obj" : $(SOURCE) $(DEP_CPP_OS_SU) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\pass2.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_PASS2=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\pass2.obj" : $(SOURCE) $(DEP_CPP_PASS2) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_PASS2=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\pass2.obj" : $(SOURCE) $(DEP_CPP_PASS2) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\pass3.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_PASS3=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\pass3.obj" : $(SOURCE) $(DEP_CPP_PASS3) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_PASS3=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\pass3.obj" : $(SOURCE) $(DEP_CPP_PASS3) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\pass4.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_PASS4=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\pass4.obj" : $(SOURCE) $(DEP_CPP_PASS4) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_PASS4=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\pass4.obj" : $(SOURCE) $(DEP_CPP_PASS4) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\printam.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_PRINT=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\dynamic.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\names.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\printam.obj" : $(SOURCE) $(DEP_CPP_PRINT) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_PRINT=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\dynamic.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\names.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\printam.obj" : $(SOURCE) $(DEP_CPP_PRINT) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\proc_desc.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_PROC_=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\dynamic.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\proc_desc.obj" : $(SOURCE) $(DEP_CPP_PROC_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_PROC_=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\dynamic.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\proc_desc.obj" : $(SOURCE) $(DEP_CPP_PROC_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\procedure.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_PROCE=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\read.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\procedure.obj" : $(SOURCE) $(DEP_CPP_PROCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_PROCE=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\os_support.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\read.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\procedure.obj" : $(SOURCE) $(DEP_CPP_PROCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\property.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_PROPE=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\property.obj" : $(SOURCE) $(DEP_CPP_PROPE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_PROPE=\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\property.obj" : $(SOURCE) $(DEP_CPP_PROPE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\read.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_READ_=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\lex.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\read.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\read.obj" : $(SOURCE) $(DEP_CPP_READ_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_READ_=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\lex.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\read.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\read.obj" : $(SOURCE) $(DEP_CPP_READ_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\section.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_SECTI=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\section.obj" : $(SOURCE) $(DEP_CPP_SECTI) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_SECTI=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\database.h"\
	"..\..\sepia\i386_nt\debug.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\gencode.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\opcode.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\section.obj" : $(SOURCE) $(DEP_CPP_SECTI) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\sepia\i386_nt\whereami_default.c

"$(INTDIR)\whereami_default.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\sepia\i386_nt\write.c

!IF  "$(CFG)" == "Eclipse - Win32 Release"

DEP_CPP_WRITE=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\lex.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\write.obj" : $(SOURCE) $(DEP_CPP_WRITE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

DEP_CPP_WRITE=\
	"..\..\sepia\i386_nt\compiler.h"\
	"..\..\sepia\i386_nt\config.h"\
	"..\..\sepia\i386_nt\dict.h"\
	"..\..\sepia\i386_nt\ec_public.h"\
	"..\..\sepia\i386_nt\embed.h"\
	"..\..\sepia\i386_nt\emu_export.h"\
	"..\..\sepia\i386_nt\lex.h"\
	"..\..\sepia\i386_nt\mem.h"\
	"..\..\sepia\i386_nt\module.h"\
	"..\..\sepia\i386_nt\property.h"\
	"..\..\sepia\i386_nt\sepia.h"\
	"..\..\Shm\i386_nt\memman.h"\
	

"$(INTDIR)\write.obj" : $(SOURCE) $(DEP_CPP_WRITE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

