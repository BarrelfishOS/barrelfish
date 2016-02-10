# Microsoft Developer Studio Project File - Name="EplexSXpress" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=EplexSXpress - Win32 Debug14
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "EplexSXpress.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "EplexSXpress.mak" CFG="EplexSXpress - Win32 Debug14"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "EplexSXpress - Win32 Release13" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Debug13" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Release13icp" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Debug13icp" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Debug14icp" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Debug14" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Release14icp" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Release14" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Debug15" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EplexSXpress - Win32 Release15" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "EplexSXpress - Win32 Release13"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "EplexXpr"
# PROP BASE Intermediate_Dir "EplexXpr"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release13"
# PROP Intermediate_Dir "Release13"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1250/i386_nt/include" /D "NDEBUG" /D XPRESS=12 /D XPRESSMINOR=50 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1326/i386_nt/include" /D "NDEBUG" /D XPRESS=13 /D XPRESSMINOR=26 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 xpopt321250.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"M:/Eclipse/xosl1250/i386_nt/lib"
# SUBTRACT BASE LINK32 /nodefaultlib
# ADD LINK32 xprs1326.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"M:/Eclipse/xosl1326/i386_nt/lib"
# SUBTRACT LINK32 /nodefaultlib
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=if not exist ..\..\lib\i386_nt\express1326 mkdir\
 ..\..\lib\i386_nt\express1326	copy\
          M:\Eclipse\xosl1326\i386_nt\bin\xprs.dll\
                ..\..\lib\i386_nt\express1326\xprs.dll	copy\
          M:\Eclipse\xosl13\lic_default\i386_nt\xpress.pwd\
               ..\..\lib\i386_nt\express1326\xpress.pwd	copy Release13\EplexSXpress.dll\
                ..\..\lib\i386_nt\sexpress1326.dll	copy Release13\EplexSXpress.dll\
               ..\..\icparc_solvers\i386_nt\sexpress1326.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Debug13"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "EplexXp0"
# PROP BASE Intermediate_Dir "EplexXp0"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug13"
# PROP Intermediate_Dir "Debug13"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1250/i386_nt/include" /D "_DEBUG" /D XPRESS=12 /D XPRESSMINOR=50 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1326/i386_nt/include" /D "_DEBUG" /D XPRESS=13 /D XPRESSMINOR=26 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 xpopt321250.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"M:/Eclipse/xosl1250/i386_nt/lib"
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 xprs1326.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"M:/Eclipse/xosl1326/i386_nt/lib"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=if not exist ..\..\lib\i386_nt\express1326 mkdir\
 ..\..\lib\i386_nt\express1326	copy\
          M:\Eclipse\xosl1326\i386_nt\bin\xprs.dll\
                ..\..\lib\i386_nt\express1326\xprs.dll	copy\
          M:\Eclipse\xosl13\lic_default\i386_nt\xpress.pwd\
                ..\..\lib\i386_nt\express1326\xpress.pwd	copy Debug13\EplexSXpress.dll\
                ..\..\lib\i386_nt\sexpress1326.dll	copy Debug13\EplexSXpress.dll\
               ..\..\icparc_solvers\i386_nt\sexpress1326.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Release13icp"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "EplexXpr"
# PROP BASE Intermediate_Dir "EplexXpr"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release13icp"
# PROP Intermediate_Dir "Release13icp"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1326icp/i386_nt/include" /D "NDEBUG" /D XPRESS=13 /D XPRESSMINOR=26 /D "XPRESS_OEM_ICPARC_2002" /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1326icp/i386_nt/include" /D "NDEBUG" /D XPRESS=13 /D XPRESSMINOR=26 /D "XPRESS_OEM_ICPARC_2002" /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 xprs1326icp.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"M:/Eclipse/xosl1326icp/i386_nt/lib"
# SUBTRACT BASE LINK32 /nodefaultlib
# ADD LINK32 xprs1326icp.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"M:/Eclipse/xosl1326icp/i386_nt/lib"
# SUBTRACT LINK32 /nodefaultlib
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=if not exist ..\..\lib\i386_nt\express1326icp mkdir\
 ..\..\lib\i386_nt\express1326icp	copy\
          M:\Eclipse\xosl1326icp\i386_nt\bin\xprs.dll\
                ..\..\lib\i386_nt\express1326icp\xprs.dll	copy\
          M:\Eclipse\xosl13\lic_icp\i386_nt\xpress.pwd\
               ..\..\lib\i386_nt\express1326icp\xpress.pwd	copy Release13icp\EplexSXpress.dll\
                ..\..\lib\i386_nt\sexpress1326icp.dll	copy Release13icp\EplexSXpress.dll\
               ..\..\icparc_solvers\i386_nt\sexpress1326icp.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Debug13icp"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "EplexXp0"
# PROP BASE Intermediate_Dir "EplexXp0"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug13icp"
# PROP Intermediate_Dir "Debug13icp"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1326icp/i386_nt/include" /D "_DEBUG" /D XPRESS=13 /D XPRESSMINOR=26 /D "XPRESS_OEM_ICPARC_2002" /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1326icp/i386_nt/include" /D "_DEBUG" /D XPRESS=13 /D XPRESSMINOR=26 /D "XPRESS_OEM_ICPARC_2002" /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 xprs1326icp.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"M:/Eclipse/xosl1326icp/i386_nt/lib"
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 xprs1326icp.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"M:/Eclipse/xosl1326icp/i386_nt/lib"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=if not exist ..\..\lib\i386_nt\express1326icp mkdir\
 ..\..\lib\i386_nt\express1326icp	copy\
          M:\Eclipse\xosl1326icp\i386_nt\bin\xprs.dll\
                ..\..\lib\i386_nt\express1326icp\xprs.dll	copy\
          M:\Eclipse\xosl13\lic_icp\i386_nt\xpress.pwd\
                ..\..\lib\i386_nt\express1326icp\xpress.pwd	copy Debug13icp\EplexSXpress.dll\
                ..\..\lib\i386_nt\sexpress1326icp.dll	copy Debug13icp\EplexSXpress.dll\
               ..\..\icparc_solvers\i386_nt\sexpress1326icp.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Debug14icp"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "EplexXpr"
# PROP BASE Intermediate_Dir "EplexXpr"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug14icp"
# PROP Intermediate_Dir "Debug14icp"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1326icp/i386_nt/include" /D "_DEBUG" /D XPRESS=13 /D XPRESSMINOR=26 /D "XPRESS_OEM_ICPARC_2002" /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1427/i386_nt/include" /D "_DEBUG" /D XPRESS=14 /D XPRESSMINOR=27 /D "XPRESS_OEM_ICPARC_2002" /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 xprs1326icp.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"M:/Eclipse/xosl1326icp/i386_nt/lib"
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 xprs.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"M:/Eclipse/xosl1427/i386_nt/lib"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=if not exist ..\..\lib\i386_nt\express1427icp mkdir\
 ..\..\lib\i386_nt\express1427icp	copy\
          M:\Eclipse\xosl1427\i386_nt\bin\xprs.dll\
                ..\..\lib\i386_nt\express1427icp\xprs.dll	copy\
          M:\Eclipse\xosl1427\i386_nt\bin\xprl.dll\
                ..\..\lib\i386_nt\express1427icp\xprl.dll	copy\
          M:\Eclipse\xosl14\lic_icp\xpress.lic\
                ..\..\lib\i386_nt\express1427icp\xpress.lic	copy Debug14icp\EplexSXpress.dll\
                ..\..\lib\i386_nt\sexpress1427icp.dll	copy Debug14icp\EplexSXpress.dll\
               ..\..\icparc_solvers\i386_nt\sexpress1427icp.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Debug14"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "EplexXp0"
# PROP BASE Intermediate_Dir "EplexXp0"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug14"
# PROP Intermediate_Dir "Debug14"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1326/i386_nt/include" /D "_DEBUG" /D XPRESS=13 /D XPRESSMINOR=26 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1427/i386_nt/include" /D "_DEBUG" /D XPRESS=14 /D XPRESSMINOR=27 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 xprs1326.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"M:/Eclipse/xosl1326/i386_nt/lib"
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 xprs.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"M:/Eclipse/xosl1427/i386_nt/lib"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=if not exist ..\..\lib\i386_nt\express1427 mkdir\
 ..\..\lib\i386_nt\express1427	copy\
          M:\Eclipse\xosl1427\i386_nt\bin\xprs.dll\
                ..\..\lib\i386_nt\express1427\xprs.dll	copy\
          M:\Eclipse\xosl1427\i386_nt\bin\xprl.dll\
                ..\..\lib\i386_nt\express1427\xprl.dll	copy\
          M:\Eclipse\xosl14\lic\i386_nt\xpress.lic\
                ..\..\lib\i386_nt\express1427\xpress.lic	copy Debug14\EplexSXpress.dll\
                ..\..\lib\i386_nt\sexpress1427.dll	copy Debug14\EplexSXpress.dll\
               ..\..\icparc_solvers\i386_nt\sexpress1427.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Release14icp"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "EplexXp1"
# PROP BASE Intermediate_Dir "EplexXp1"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release14icp"
# PROP Intermediate_Dir "Release14icp"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1326icp/i386_nt/include" /D "NDEBUG" /D XPRESS=13 /D XPRESSMINOR=26 /D "XPRESS_OEM_ICPARC_2002" /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1427/i386_nt/include" /D "NDEBUG" /D XPRESS=14 /D XPRESSMINOR=27 /D "XPRESS_OEM_ICPARC_2002" /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 xprs1326icp.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"M:/Eclipse/xosl1326icp/i386_nt/lib"
# SUBTRACT BASE LINK32 /nodefaultlib
# ADD LINK32 xprs.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"M:/Eclipse/xosl1427/i386_nt/lib"
# SUBTRACT LINK32 /nodefaultlib
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=if not exist ..\..\lib\i386_nt\express1427icp mkdir\
 ..\..\lib\i386_nt\express1427icp	copy\
          M:\Eclipse\xosl1427\i386_nt\bin\xprs.dll\
                ..\..\lib\i386_nt\express1427icp\xprs.dll	copy\
          M:\Eclipse\xosl1427\i386_nt\bin\xprl.dll\
                ..\..\lib\i386_nt\express1427icp\xprl.dll	copy\
          M:\Eclipse\xosl14\lic_icp\xpress.lic\
               ..\..\lib\i386_nt\express1427icp\xpress.lic	copy Release14icp\EplexSXpress.dll\
                ..\..\lib\i386_nt\sexpress1427icp.dll	copy Release14icp\EplexSXpress.dll\
               ..\..\icparc_solvers\i386_nt\sexpress1427icp.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Release14"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "EplexXp2"
# PROP BASE Intermediate_Dir "EplexXp2"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release14"
# PROP Intermediate_Dir "Release14"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1326/i386_nt/include" /D "NDEBUG" /D XPRESS=13 /D XPRESSMINOR=26 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1427/i386_nt/include" /D "NDEBUG" /D XPRESS=14 /D XPRESSMINOR=27 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 xprs1326.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"M:/Eclipse/xosl1326/i386_nt/lib"
# SUBTRACT BASE LINK32 /nodefaultlib
# ADD LINK32 xprs.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"M:/Eclipse/xosl1427/i386_nt/lib"
# SUBTRACT LINK32 /nodefaultlib
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=if not exist ..\..\lib\i386_nt\express1427 mkdir\
 ..\..\lib\i386_nt\express1427	copy\
          M:\Eclipse\xosl1427\i386_nt\bin\xprs.dll\
                ..\..\lib\i386_nt\express1427\xprs.dll	copy\
          M:\Eclipse\xosl1427\i386_nt\bin\xprl.dll\
                ..\..\lib\i386_nt\express1427\xprl.dll	copy\
          M:\Eclipse\xosl14\lic\i386_nt\xpress.lic\
               ..\..\lib\i386_nt\express1427\xpress.lic	copy Release14\EplexSXpress.dll\
                ..\..\lib\i386_nt\sexpress1427.dll	copy Release14\EplexSXpress.dll\
               ..\..\icparc_solvers\i386_nt\sexpress1427.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Debug15"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "EplexXp0"
# PROP BASE Intermediate_Dir "EplexXp0"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug15"
# PROP Intermediate_Dir "Debug15"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1326/i386_nt/include" /D "_DEBUG" /D XPRESS=13 /D XPRESSMINOR=26 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1525/i386_nt/include" /D "_DEBUG" /D XPRESS=15 /D XPRESSMINOR=25 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 xprs1326.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"M:/Eclipse/xosl1326/i386_nt/lib"
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 xprs.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"../Eclipse/Debug" /libpath:"M:/Eclipse/xosl1525/i386_nt/lib"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=if not exist ..\..\lib\i386_nt\express1525 mkdir\
 ..\..\lib\i386_nt\express1525	copy\
          M:\Eclipse\xosl1525\i386_nt\bin\xprs.dll\
                ..\..\lib\i386_nt\express1525\xprs.dll	copy\
          M:\Eclipse\xosl1525\i386_nt\bin\xprl.dll\
                ..\..\lib\i386_nt\express1525\xprl.dll	copy\
          M:\Eclipse\xosl1525\i386_nt\bin\lmgrd.exe\
                ..\..\lib\i386_nt\express1525\lmgrd.exe	copy\
          M:\Eclipse\xosl1525\i386_nt\bin\dash.exe\
                ..\..\lib\i386_nt\express1525\dash.exe	copy\
          M:\Eclipse\xosl15\lic\xpress.lic\
                ..\..\lib\i386_nt\express1525\xpress.lic	copy Debug15\EplexSXpress.dll\
                ..\..\lib\i386_nt\sexpress1525.dll	copy Debug15\EplexSXpress.dll\
               ..\..\icparc_solvers\i386_nt\sexpress1525.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "EplexSXpress - Win32 Release15"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "EplexXp2"
# PROP BASE Intermediate_Dir "EplexXp2"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release15"
# PROP Intermediate_Dir "Release15"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1326/i386_nt/include" /D "NDEBUG" /D XPRESS=13 /D XPRESSMINOR=26 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "../../sepia/i386_nt" /I "M:/Eclipse/xosl1525/i386_nt/include" /D "NDEBUG" /D XPRESS=15 /D XPRESSMINOR=25 /D "WIN32" /D "_WINDOWS" /D "DLL" /D "__STDC__" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 xprs1326.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"M:/Eclipse/xosl1326/i386_nt/lib"
# SUBTRACT BASE LINK32 /nodefaultlib
# ADD LINK32 xprs.lib eclipse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /libpath:"../Eclipse/Release" /libpath:"M:/Eclipse/xosl1525/i386_nt/lib"
# SUBTRACT LINK32 /nodefaultlib
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Copying dll to icparc_solvers, lib
PostBuild_Cmds=if not exist ..\..\lib\i386_nt\express1525 mkdir\
 ..\..\lib\i386_nt\express1525	copy\
          M:\Eclipse\xosl1525\i386_nt\bin\xprs.dll\
                ..\..\lib\i386_nt\express1525\xprs.dll	copy\
          M:\Eclipse\xosl1525\i386_nt\bin\xprl.dll\
                ..\..\lib\i386_nt\express1525\xprl.dll	copy\
          M:\Eclipse\xosl1525\i386_nt\bin\lmgrd.exe\
                ..\..\lib\i386_nt\express1525\lmgrd.exe	copy\
          M:\Eclipse\xosl1525\i386_nt\bin\dash.exe\
                ..\..\lib\i386_nt\express1525\dash.exe	copy\
          M:\Eclipse\xosl15\lic\xpress.lic\
               ..\..\lib\i386_nt\express1525\xpress.lic	copy Release15\EplexSXpress.dll\
                ..\..\lib\i386_nt\sexpress1525.dll	copy Release15\EplexSXpress.dll\
               ..\..\icparc_solvers\i386_nt\sexpress1525.dll
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "EplexSXpress - Win32 Release13"
# Name "EplexSXpress - Win32 Debug13"
# Name "EplexSXpress - Win32 Release13icp"
# Name "EplexSXpress - Win32 Debug13icp"
# Name "EplexSXpress - Win32 Debug14icp"
# Name "EplexSXpress - Win32 Debug14"
# Name "EplexSXpress - Win32 Release14icp"
# Name "EplexSXpress - Win32 Release14"
# Name "EplexSXpress - Win32 Debug15"
# Name "EplexSXpress - Win32 Release15"
# Begin Source File

SOURCE=..\..\icparc_solvers\seplex.c
# End Source File
# Begin Source File

SOURCE=..\..\icparc_solvers\seplex_xpress.def
# End Source File
# End Target
# End Project
