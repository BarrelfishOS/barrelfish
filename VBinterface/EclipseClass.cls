' BEGIN LICENSE BLOCK
' Version: CMPL 1.1
'
' The contents of this file are subject to the Cisco-style Mozilla Public
' License Version 1.1 (the "License"); you may not use this file except
' in compliance with the License.  You may obtain a copy of the License
' at www.eclipse-clp.org/license.
' 
' Software distributed under the License is distributed on an "AS IS"
' basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
' the License for the specific language governing rights and limitations
' under the License. 
' 
' The Original Code is  The ECLiPSe Constraint Logic Programming System. 
' The Initial Developer of the Original Code is  Cisco Systems, Inc. 
' Portions created by the Initial Developer are
' Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
' 
' Contributor(s): 
' 
' END LICENSE BLOCK

VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "EclipseClass"
Attribute VB_GlobalNameSpace = True
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder" ,"Yes"
Attribute VB_Ext_KEY = "Member0" ,"EclipseStreams"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

Public Enum EC_Status
    TypeError = -5
    InstantiationFault = -4
    Success = 0
    Fail = 1
    ExitBlock = 2
    Yield = 4
    Running = 5
    WaitIO = 6
    FlushIO = 7
End Enum

Private Enum EC_Option_Id
    EC_OPTION_MAPFILE = 0
    EC_OPTION_PARALLEL_WORKER = 1
    EC_OPTION_ARGC = 2
    EC_OPTION_ARGV = 3
    EC_OPTION_LOCALSIZE = 4
    EC_OPTION_GLOBALSIZE = 5
    EC_OPTION_PRIVATESIZE = 6
    EC_OPTION_SHAREDSIZE = 7
    EC_OPTION_PANIC = 8
    EC_OPTION_ALLOCATION = 9
    EC_OPTION_DEFAULT_MODULE = 10
    EC_OPTION_ECLIPSEDIR = 11
    EC_OPTION_IO = 12
End Enum

Public Enum EC_ERROR
    EC_CONVERSION_ERROR = vbObjectError + 5001
    EC_BAD_EXIT_ERROR = vbObjectError + 5002
    EC_BUSY_ERROR = vbObjectError + 5003
End Enum

Private Declare Function ec_init _
        Lib "Eclipse.dll" _
        () _
        As Long
Private Declare Function ec_cleanup _
        Lib "Eclipse.dll" _
        (ExitVal As Long) _
        As Long
Private Declare Function ec_post_string _
        Lib "Eclipse.dll" _
        (ByVal Goal As String) _
        As Long
Private Declare Function ec_post_event_string _
        Lib "Eclipse.dll" _
        (ByVal sEvent As String) _
        As Long
'Private Declare Function ec_resume _
'        Lib "Eclipse.dll" _
'        () _
'        As Long
Private Declare Function ec_resume_async _
        Lib "Eclipse.dll" _
        () _
        As Long
Private Declare Function ec_handle_events _
        Lib "Eclipse.dll" _
        (ByRef Stream As Long) _
        As Long
Private Declare Function ec_resume_status _
        Lib "Eclipse.dll" _
        () _
        As Long
Private Declare Function ec_running _
        Lib "Eclipse.dll" _
        () _
        As Long
Private Declare Function ec_resume_status_long _
        Lib "Eclipse.dll" _
        (ByRef Stream As Long) _
        As Long
Private Declare Function ec_resume_long _
        Lib "Eclipse.dll" _
        (ByRef Stream As Long) _
        As Long
Private Declare Function ec_set_option_int _
        Lib "Eclipse.dll" _
        (ByVal OptID As Long, ByVal OptVal As Long) _
        As Long
Private Declare Function ec_set_option_ptr _
        Lib "Eclipse.dll" _
        (ByVal OptID As Long, ByRef OptVal As Byte) _
        As Long
Private Declare Function ec_set_option_string _
        Lib "Eclipse.dll" _
        Alias "ec_set_option_ptr" _
        (ByVal OptID As Long, ByVal s As String) _
        As Long

Private mvarEclipseStreams As EclipseStreams
Private mActive As Boolean
Private esRpcIn As EclipseStream
Private esRpcOut As EclipseStream
Private ecTools As EclipseTools




Public Property Get Streams() As EclipseStreams
    Set Streams = mvarEclipseStreams
End Property


Public Property Let GlobalSize(size As Long)
    ec_set_option_int EC_OPTION_GLOBALSIZE, size
End Property
Public Property Let PrivateSize(size As Long)
    ec_set_option_int EC_OPTION_PRIVATESIZE, size

End Property
Public Property Let SharedSize(size As Long)
    ec_set_option_int EC_OPTION_SHAREDSIZE, size
End Property
Public Property Let LocalSize(size As Long)
    ec_set_option_int EC_OPTION_LOCALSIZE, size
End Property

'Public Property Let ArgC(num As Long)
'    ec_set_option_int EC_OPTION_ARGC, num
'End Property
'   no acesss to these
'    EC_OPTION_MAPFILE = 0
'    EC_OPTION_PARALLEL_WORKER = 1
'    EC_OPTION_ARGV = 3
'    EC_OPTION_ARGC
'    EC_OPTION_PANIC = 8
'    EC_OPTION_ALLOCATION = 9

Public Property Let EclipseDir(dir As String)
    ec_set_option_string EC_OPTION_ECLIPSEDIR, dir
End Property

Public Property Let Module(s As String)
    ec_set_option_string EC_OPTION_DEFAULT_MODULE, s
End Property



Public Function Init() As Long
    Dim status As Long
    
    mActive = False
    status = ec_set_option_int(EC_OPTION_IO, 2)
    Init = ec_init()
    Set mvarEclipseStreams = New EclipseStreams
    Set esRpcIn = mvarEclipseStreams.Add("ec_rpc_in", ToEclipse)
    Set esRpcOut = mvarEclipseStreams.Add("ec_rpc_out", FromEclipse)
End Function
        
Public Sub Send(EventName As String)
    ec_post_event_string (EventName)
End Sub

Public Function Post(Goal As String) As EC_Status
    Dim status As EC_Status
    Dim iStream As Long
    Dim Stream As EclipseStream

    Debug.Print "Posting: " & Goal
    Post = ec_post_string(Goal)
End Function

Public Sub RPC(Goal As Variant, Response As Variant)
    If ec_running() Then
        Err.Raise EC_BUSY_ERROR, TypeName(Me) & "::RPC", _
            "Attempted RPC while Eclipse busy."
    End If
    esRpcIn.WriteExdr Goal
    HandleEvents
    esRpcOut.ReadExdr Response
End Sub

Public Function ResumeAsync() As EC_Status
    Dim status As EC_Status
    Dim iStream As Long
    Dim Stream As EclipseStream

    If mActive Then
        Err.Raise EC_BUSY_ERROR, TypeName(Me) & "::ResumeFlush()", _
            "Resuming while ECLiPSe is active"
    End If
    mActive = True
    Do
        status = ec_resume_async()
        Do
            DoEvents
            status = ec_resume_status_long(iStream)
        Loop While status = Running
        Debug.Print "Status = " & status
        
        ' flush_buffers
        Select Case status
        Case FlushIO:
            Set Stream = mvarEclipseStreams.Name(iStream)
            Stream.Flush
        Case WaitIO:
            Set Stream = mvarEclipseStreams.Name(iStream)
            Stream.StreamWrite InputBox(Stream.Prompt, "ECLiPSe input")
        Case Success, Fail, Yield
            mActive = False
            Exit Do
        Case Else
            mActive = False
            Err.Raise EC_BAD_EXIT_ERROR, _
            TypeName(Me) & "::ResumeFlush", _
            "Unexpected return code from emulator (" & status & ")."
        End Select
    Loop
    ResumeAsync = status
End Function
Public Function HandleEvents() As EC_Status
    Dim status As EC_Status
    Dim iStream As Long
    Dim Stream As EclipseStream
    
    status = ec_handle_events(iStream)
    Do
        ' flush_buffers
        Select Case status
        Case FlushIO:
            Set Stream = mvarEclipseStreams.Name(iStream)
            Stream.Flush
        Case WaitIO:
            Set Stream = mvarEclipseStreams.Name(iStream)
            Stream.StreamWrite InputBox(Stream.Prompt, "ECLiPSe input")
        Case Success
            Exit Do
        Case Running
            Err.Raise EC_BUSY_ERROR, TypeName(Me) & "::HandleEvents"
        Case Else
            Err.Raise EC_BAD_EXIT_ERROR, _
            TypeName(Me) & "::HandleEvents", _
            "Unexpected return code from emulator (" & status & ")."
        End Select
        
        DoEvents
        status = ec_resume_long(iStream)
    Loop
    HandleEvents = status
End Function

'Public Function ShowTools()
'    Set ecTools = New EclipseTools
'    Set ecTools.ec = Me
'    ecTools.InitForms
'End Function

Private Sub Class_Initialize()
    Debug.Print "Creating " & TypeName(Me) & " in thread " & App.ThreadID
End Sub

Private Sub Class_Terminate()
    Set mvarEclipseStreams = Nothing
End Sub
