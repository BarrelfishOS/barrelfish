# Microsoft Developer Studio Project File - Name="EplexSCplex" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=EplexSCplex - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "EplexSCplex.mak".
!MESSAGE 
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

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "EplexSCplex - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "M:/Eclipse/cplex6.0" /D CPLEX=6 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 cplex60.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"M:/Eclipse/cplex6.0"
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=copy Release\EplexSCplex.dll\
            ..\..\icparc_solvers\i386_nt\secplex60.dll	copy Release\EplexSCplex.dll\
            ..\..\lib\i386_nt\secplex60.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 Debug"

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
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "M:/Eclipse/cplex6.0" /D CPLEX=6 /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 cplex60.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"M:/Eclipse/cplex6.0"
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=copy Debug\EplexSCplex.dll\
            ..\..\icparc_solvers\i386_nt\secplex60.dll	copy Debug\EplexSCplex.dll\
            ..\..\lib\i386_nt\secplex60.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 DebugCplex75"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "EplexCpl"
# PROP BASE Intermediate_Dir "EplexCpl"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "DebugCplex75"
# PROP Intermediate_Dir "DebugCplex75"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "M:/Eclipse/cplex6.0" /D CPLEX=6 /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "M:/Eclipse/cplex75/include/ilcplex" /D CPLEX=7 /D CPLEXMINOR=5 /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 cplex60.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"M:/Eclipse/cplex6.0"
# ADD LINK32 cplex75.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"M:/Eclipse/cplex75/lib/msvc6/stat_mt"
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=copy DebugCplex75\EplexSCplex.dll\
            ..\..\icparc_solvers\i386_nt\secplex75.dll	copy DebugCplex75\EplexSCplex.dll\
            ..\..\lib\i386_nt\secplex75.dll	copy M:\Eclipse\cplex75\bin\msvc6\cplex75.dll\
            ..\..\lib\i386_nt\cplex75.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 ReleaseCplex75"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "EplexCp0"
# PROP BASE Intermediate_Dir "EplexCp0"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "ReleaseCplex75"
# PROP Intermediate_Dir "ReleaseCplex75"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "M:/Eclipse/cplex6.0" /D CPLEX=6 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "M:/Eclipse/cplex75/include/ilcplex" /D CPLEX=7 /D CPLEXMINOR=5 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 cplex60.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"M:/Eclipse/cplex6.0"
# ADD LINK32 cplex75.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"M:/Eclipse/cplex75/lib/msvc6/stat_mt"
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=copy ReleaseCplex75\EplexSCplex.dll\
            ..\..\icparc_solvers\i386_nt\secplex75.dll	copy ReleaseCplex75\EplexSCplex.dll\
            ..\..\lib\i386_nt\secplex75.dll	copy M:\Eclipse\cplex75\bin\msvc6\cplex75.dll\
            ..\..\lib\i386_nt\cplex75.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 DebugCplex90"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "EplexCpl"
# PROP BASE Intermediate_Dir "EplexCpl"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "DebugCplex90"
# PROP Intermediate_Dir "DebugCplex90"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "M:/Eclipse/cplex6.0" /D CPLEX=6 /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "M:/Eclipse/cplex90/include/ilcplex" /D CPLEX=9 /D CPLEXMINOR=0 /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 cplex60.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"M:/Eclipse/cplex6.0"
# ADD LINK32 cplex90.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"M:/Eclipse/cplex90/lib/msvc6/stat_mt"
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=copy DebugCplex90\EplexSCplex.dll\
            ..\..\icparc_solvers\i386_nt\secplex90.dll	copy DebugCplex90\EplexSCplex.dll\
            ..\..\lib\i386_nt\secplex90.dll	copy M:\Eclipse\cplex90\bin\msvc6\cplex90.dll\
            ..\..\lib\i386_nt\cplex90.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "EplexSCplex - Win32 ReleaseCplex90"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "EplexCp0"
# PROP BASE Intermediate_Dir "EplexCp0"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "ReleaseCplex90"
# PROP Intermediate_Dir "ReleaseCplex90"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "M:/Eclipse/cplex6.0" /D CPLEX=6 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "M:/Eclipse/cplex90/include/ilcplex" /D CPLEX=9 /D CPLEXMINOR=0 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 cplex60.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"M:/Eclipse/cplex6.0"
# ADD LINK32 cplex90.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"M:/Eclipse/cplex90/lib/msvc6/stat_mt"
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=copy ReleaseCplex90\EplexSCplex.dll\
            ..\..\icparc_solvers\i386_nt\secplex90.dll	copy ReleaseCplex90\EplexSCplex.dll\
            ..\..\lib\i386_nt\secplex90.dll	copy M:\Eclipse\cplex90\bin\msvc6\cplex90.dll\
            ..\..\lib\i386_nt\cplex90.dll
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "EplexSCplex - Win32 Release"
# Name "EplexSCplex - Win32 Debug"
# Name "EplexSCplex - Win32 DebugCplex75"
# Name "EplexSCplex - Win32 ReleaseCplex75"
# Name "EplexSCplex - Win32 DebugCplex90"
# Name "EplexSCplex - Win32 ReleaseCplex90"
# Begin Source File

SOURCE=..\..\icparc_solvers\seplex.c
# End Source File
# Begin Source File

SOURCE=..\..\icparc_solvers\seplex_cplex.def
# End Source File
# End Target
# End Project
