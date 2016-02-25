# Microsoft Developer Studio Project File - Name="GmpDll" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=GmpDll - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "GmpDll.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "GmpDll.mak" CFG="GmpDll - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "GmpDll - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "GmpDll - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "GmpDll - Win32 Release"

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
# ADD CPP /nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse" /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER" /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /version:3.1 /subsystem:windows /dll /machine:I386 /out:"Release/gmp33.dll"
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying gmp dll to lib/i386_nt
PostBuild_Cmds=copy Release\gmp33.dll ..\..\lib\i386_nt\gmp33.dll	copy\
   Release\gmp33.lib ..\..\lib\i386_nt\gmp33.lib
# End Special Build Tool

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

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
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse" /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER" /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /out:"Debug/gmp33.dll" /pdbtype:sept
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying gmp dll to lib/i386_nt
PostBuild_Cmds=copy Debug\gmp33.dll ..\..\lib\i386_nt\gmp33.dll	copy\
   Debug\gmp33.lib ..\..\lib\i386_nt\gmp33.lib
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "GmpDll - Win32 Release"
# Name "GmpDll - Win32 Debug"
# Begin Group "mpn"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\add.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\add_1.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\add_n.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\addmul_1.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\addsub_n.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\bdivmod.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\cmp.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\dc_divrem_n.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\dive_1.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\diveby3.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\divis.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\divrem.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\divrem_1.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\divrem_2.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\dump.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\fib2_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\gcd.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\gcd_1.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\gcdext.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\get_str.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\hamdist.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\jacbase.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\lshift.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\mod_1.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\mod_34lsub1.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\mode1o.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\mp_bases.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\mul.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\mul_1.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\mul_basecase.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\mul_fft.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\mul_n.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\perfsqr.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\popcount.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\pow_1.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\pre_divrem_1.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\pre_mod_1.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\random.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\random2.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\rootrem.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\rshift.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\sb_divrem_mn.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\scan0.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\scan1.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\set_str.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\sizeinbase.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\sqr_basecase.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\sqrtrem.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\sub.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\sub_1.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\sub_n.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\submul_1.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpn\generic\tdiv_qr.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPN"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPN"

!ENDIF 

# End Source File
# End Group
# Begin Group "mpq"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\abs.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\aors.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\canonicalize.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\clear.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\cmp.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\cmp_si.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\cmp_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\div.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\equal.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\get_d.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\get_den.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\get_num.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\get_str.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\init.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\inp_str.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\inv.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\md_2exp.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\mul.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\neg.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\out_str.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\set.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\set_d.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\set_den.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\set_f.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\set_num.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\set_si.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\set_str.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\set_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\set_z.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpq\swap.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPQ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPQ"

!ENDIF 

# End Source File
# End Group
# Begin Group "mpz"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\abs.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\add.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\add_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\and.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\aorsmul.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\aorsmul_i.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\array_init.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\bin_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\bin_uiui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cdiv_q.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cdiv_q_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cdiv_qr.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cdiv_qr_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cdiv_r.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cdiv_r_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cdiv_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cfdiv_q_2exp.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cfdiv_r_2exp.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\clear.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\clrbit.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cmp.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cmp_d.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cmp_si.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cmp_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cmpabs.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cmpabs_d.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cmpabs_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\com.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cong.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cong_2exp.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\cong_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\dive_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\divegcd.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\divexact.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\divis.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\divis_2exp.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\divis_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\dump.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\export.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fac_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fdiv_q.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fdiv_q_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fdiv_qr.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fdiv_qr_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fdiv_r.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fdiv_r_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fdiv_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fib2_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fib_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fits_sint.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fits_slong.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fits_sshort.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fits_uint.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fits_ulong.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\fits_ushort.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\gcd.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\gcd_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\gcdext.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\get_d.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\get_d_2exp.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\get_si.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\get_str.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\get_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\getlimbn.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\hamdist.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\import.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\init.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\init2.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\inp_raw.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\inp_str.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\invert.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\ior.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\iset.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\iset_d.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\iset_si.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\iset_str.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\iset_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\jacobi.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\kronsz.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\kronuz.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\kronzs.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\kronzu.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\lcm.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\lcm_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\lucnum2_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\lucnum_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\millerrabin.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\mod.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\mul.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\mul_2exp.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\mul_si.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\mul_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\n_pow_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\neg.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\nextprime.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\out_raw.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\out_str.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\perfpow.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\perfsqr.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\popcount.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\pow_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\powm.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\powm_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\pprime_p.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\random.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\random2.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\realloc.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\realloc2.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\remove.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\root.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\rrandomb.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\scan0.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\scan1.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\set.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\set_d.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\set_f.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\set_q.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\set_si.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\set_str.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\set_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\setbit.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\size.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\sizeinbase.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\sqrt.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\sqrtrem.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\sub.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\sub_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\swap.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\tdiv_q.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\tdiv_q_2exp.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\tdiv_q_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\tdiv_qr.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\tdiv_qr_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\tdiv_r.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\tdiv_r_2exp.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\tdiv_r_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\tdiv_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\tstbit.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\ui_pow_ui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\ui_sub.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\urandomb.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\urandomm.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mpz\xor.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

# PROP Intermediate_Dir "ReleaseMPZ"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

# PROP Intermediate_Dir "DebugMPZ"

!ENDIF 

# End Source File
# End Group
# Begin Source File

SOURCE=..\..\Gmp\src\ansi2knr.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\assert.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\compat.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\errno.c
# End Source File
# Begin Source File

SOURCE="..\..\Gmp\src\extract-dbl.c"
# End Source File
# Begin Source File

SOURCE="..\..\Gmp\src\insert-dbl.c"
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\memory.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mp_bpl.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mp_clz_tab.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mp_minv_tab.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\mp_set_fns.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\rand.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\randclr.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\randdef.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\randlc.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\randlc2s.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\randlc2x.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\randraw.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\rands.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\randsd.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\randsdui.c
# End Source File
# Begin Source File

SOURCE=..\..\Gmp\src\version.c
# End Source File
# End Target
# End Project
