# Microsoft Developer Studio Project File - Name="Eclipse" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=Eclipse - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Eclipse.mak".
!MESSAGE 
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

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Eclipse - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MT /W2 /GX /O2 /I "../../Shm/i386_nt" /I "../../Gmp/src/win32eclipse" /D "THREADED" /D "DEBUG_COMP" /D "OC" /D "DFID" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 netapi32.lib gmp33.lib shm.lib ws2_32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Shm/Release" /libpath:"../../lib/i386_nt"
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying eclipse.dll somewhere sensible
PostBuild_Cmds=copy      Release\Eclipse.dll  ..\..\lib\i386_nt\eclipse.dll\
           	copy     Release\Eclipse.lib       ..\..\lib\i386_nt\eclipse.lib
# End Special Build Tool

!ELSEIF  "$(CFG)" == "Eclipse - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MTd /W2 /Gm /GX /Zi /I "../../Shm/i386_nt" /I "../../Gmp/src/win32eclipse" /D "THREADED" /D "DEBUG_COMP" /D "OC" /D "DFID" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 netapi32.lib gmp33.lib shm.lib ws2_32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /version:5.9 /subsystem:windows /dll /incremental:no /map /debug /machine:I386 /pdbtype:sept /libpath:"../Shm/Debug" /libpath:"../../lib/i386_nt"
# SUBTRACT LINK32 /verbose /nodefaultlib
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying eclipse.dll somewhere sensible
PostBuild_Cmds=copy         Debug\Eclipse.dll  ..\..\lib\i386_nt\eclipse.dll\
            	copy Debug\Eclipse.lib          ..\..\lib\i386_nt\eclipse.lib
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "Eclipse - Win32 Release"
# Name "Eclipse - Win32 Debug"
# Begin Source File

SOURCE=..\..\sepia\i386_nt\bigrat.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\bip_arith.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\bip_array.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\bip_comp.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\bip_control.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\bip_db.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\bip_delay.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\bip_domain.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\bip_io.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\bip_load.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\bip_misc.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\bip_module.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\bip_record.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\bip_strings.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\bip_tconv.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\body.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\code.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\compiler.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\config.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\database.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\debug.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\dict.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\dict.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\dummy_mps.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\dummy_par.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\dummy_upcalls.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\dummy_wm.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\dynamic.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\dynamic.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\ec_public.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\eclipse.def
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\eclipse.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\eclipsedir.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\elipsys_fd.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\embed.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\embed.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\emu.obj
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\emu_c_env.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\emu_export.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\emu_op_addr.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\emu_regs.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\emu_util.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\error.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\error.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\external.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\external.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\fd.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\gc_stacks.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\gencode.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\handle.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\handlers.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\head.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\init.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\intervals.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\intervals.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\io.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\io.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\lex.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\lex.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\lib1.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\lib2.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\lib3.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\lib4.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\lib5.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\lib6.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\lib7.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\mem.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\mem.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\module.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\names.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\opcode.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\operator.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\os_support.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\os_support.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\pass2.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\pass3.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\pass4.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\printam.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\proc_desc.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\procedure.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\property.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\property.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\read.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\read.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\sav_res.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\section.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\sepia.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\types.h
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\whereami_default.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\write.c
# End Source File
# End Target
# End Project
