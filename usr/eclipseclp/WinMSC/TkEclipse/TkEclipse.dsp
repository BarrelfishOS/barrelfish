# Microsoft Developer Studio Project File - Name="TkEclipse" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=TkEclipse - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "TkEclipse.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "TkEclipse.mak" CFG="TkEclipse - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "TkEclipse - Win32 Release" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "TkEclipse - Win32 Debug" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "TkEclipse - Win32 Release"

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
# ADD CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "C:/Program Files/Tcl/include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "__STDC__" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 tkexdr.lib tcl83.lib tk83.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"../TkExdr/Release" /libpath:"C:/Program Files/Tcl/lib"
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll and lib to lib/i386_nt
PostBuild_Cmds=copy Release\TkEclipse.dll\
                 ..\..\lib\i386_nt\tkeclipse.dll	copy Release\TkEclipse.lib\
                 ..\..\lib\i386_nt\tkeclipse.lib
# End Special Build Tool

!ELSEIF  "$(CFG)" == "TkEclipse - Win32 Debug"

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
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "C:/Program Files/Tcl/include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "__STDC__" /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 tkexdr.lib tcl83.lib tk83.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"../TkExdr/Debug" /libpath:"M:/Eclipse/tcltk8.3/i386_nt/lib" /libpath:"../Eclipse/Release" /libpath:"C:/Program Files/Tcl/lib"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll and lib to lib/i386_nt
PostBuild_Cmds=copy Debug\TkEclipse.dll\
                 ..\..\lib\i386_nt\tkeclipse.dll	copy Debug\TkEclipse.lib\
                 ..\..\lib\i386_nt\tkeclipse.lib
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "TkEclipse - Win32 Release"
# Name "TkEclipse - Win32 Debug"
# Begin Source File

SOURCE=..\..\sepia\i386_nt\tkeclipse.c
# End Source File
# Begin Source File

SOURCE=..\..\sepia\i386_nt\tkeclipse.def
# End Source File
# End Target
# End Project
