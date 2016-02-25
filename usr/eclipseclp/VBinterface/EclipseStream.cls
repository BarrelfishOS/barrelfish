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
Attribute VB_Name = "EclipseStream"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Description = "The streams open to eclipse"
Attribute VB_Ext_KEY = "SavedWithClassBuilder" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"No"
Option Explicit

Private Declare Function ec_queue_write _
        Lib "Eclipse.dll" _
        (ByVal StreamNr As Long, ByVal buffer As String, ByVal length As Long) _
        As Long
Private Declare Function ec_queue_read _
        Lib "Eclipse.dll" _
        (ByVal StreamNr As Long, ByVal buffer As String, ByVal length As Long) _
        As Long
Private Declare Sub ec_int32_xdr _
        Lib "Eclipse.dll" _
        (l As Long, ByVal xdrString As String)
Private Declare Sub ec_double_xdr _
        Lib "Eclipse.dll" _
        (d As Double, ByVal xdrString As String)
Private Declare Sub ec_xdr_int32 _
        Lib "Eclipse.dll" _
        (ByVal xdrString As String, l As Long)
Private Declare Sub ec_xdr_double _
        Lib "Eclipse.dll" _
        (ByVal xdrString As String, d As Double)
Public Enum EclipseStreamMode
    ToEclipse
    FromEclipse
End Enum

Const ExdrVersion = 1

'local variable(s) to hold property value(s)

Private mvarMode As EclipseStreamMode 'local copy
Private mvarStreamID As Long
Private msPrompt As String
Private mKey As String

Event Flush()

Friend Property Let Key(ByVal vData As String)
    mKey = vData
End Property
Public Property Get Key() As String
    Key = mKey
End Property

Friend Property Let Mode(ByVal vData As EclipseStreamMode)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.Mode = 5
    mvarMode = vData
End Property


Public Property Get Mode() As EclipseStreamMode
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.Mode
    Mode = mvarMode
End Property

Public Property Let Prompt(ByVal vData As String)
    msPrompt = vData
End Property
Public Property Get Prompt() As String
    Prompt = msPrompt
End Property

Friend Property Let id(ByVal vData As Long)
    mvarStreamID = vData
End Property
Friend Property Get id() As Long
    id = mvarStreamID
End Property
Public Sub StreamWrite(data As String)
    If mvarMode = FromEclipse Then
        Err.Raise 3, TypeName(Me) & "::StreamWrite", _
            "Writing to a FromEclipse stream" _
            & " (" & Key & ")."
    End If
    ec_queue_write id, data, Len(data)
End Sub

Friend Sub Flush()
    If Mode = ToEclipse Then Exit Sub
    RaiseEvent Flush
End Sub
Public Function Read(l As Long) As String
    Dim buffer As String
    Dim ret As Long

    If mvarMode = ToEclipse Then
        Err.Raise 3, TypeName(Me) & "::StreamWrite", _
            "Reading from a ToEclipse stream" _
            & " (" & Key & ")."
    End If
    
    buffer = Space(l)
    ret = ec_queue_read(mvarStreamID, buffer, l)
    If (ret < l) Then
        Read = Left(buffer, ret)
    Else
        Read = buffer
    End If
End Function
Public Function NewData() As String
    Dim buffer As String * 1000
    Dim lenbuf As Long

    lenbuf = ec_queue_read(mvarStreamID, buffer, 1000)
    If (lenbuf = 1000) Then
        NewData = buffer & NewData
    ElseIf lenbuf = -192 Then
        Err.Raise 2, TypeName(Me) & "::Flush", _
            "Trying to read from a stream that is not a queue" _
            & " (" & Key & ")."
    Else
        NewData = Left(buffer, lenbuf)
    End If
End Function



Public Sub WriteExdr(data As Variant)
    Dim o As String
    o = "V" & Chr$(ExdrVersion) & Exdr(data)
    ec_queue_write id, o, Len(o)
End Sub
Private Function Exdr(data As Variant) As String
    Dim o As String
    Dim buff As String * 8
    Dim i As Long
    Dim Item As Variant
    Dim TheType As VbVarType
    
    TheType = VarType(data)
    Select Case TheType
    Case Is >= vbArray
        ec_int32_xdr UBound(data), buff
        o = "F" & Left(buff, 4)
        ec_int32_xdr Len(data(0)), buff
        o = o & "S" & Left(buff, 4) & data(0)
        For i = 1 To UBound(data)
            o = o & Exdr(data(i))
        Next i
        Exdr = o
    Case vbObject
        If TypeName(data) = "Collection" Then
            For Each Item In data
                o = o & "[" & Exdr(Item)
            Next Item
            Exdr = o & "]"
        Else
            Err.Raise EC_CONVERSION_ERROR, TypeName(Me) & "::WriteExdr", _
                "Cannot convert object of type " & TypeName(data) & "."
        End If
    Case vbString
        ec_int32_xdr Len(data), buff
        Exdr = "S" & Left(buff, 4) & data
    Case vbDouble
        ec_double_xdr data, buff
        Exdr = "D" & buff
    Case vbLong, vbInteger
        ec_int32_xdr data, buff
        Exdr = "I" & Left(buff, 4)
    Case vbEmpty
        Exdr = "_"
    Case Else
        Err.Raise EC_CONVERSION_ERROR, TypeName(Me) & "::WriteExdr", _
            "Cannot convert data of type " & TypeName(data) & "."
    End Select
End Function

Public Sub ReadExdr(vout As Variant)
    Dim sIn As String
    Dim iLen As Long
    
    sIn = Space(2)
    iLen = ec_queue_read(mvarStreamID, sIn, 2)
    If iLen <> 2 Or sIn <> "V" & Chr$(ExdrVersion) Then
        Err.Raise _
            EC_CONVERSION_ERROR, TypeName(Me) & "::ReadExdr", _
            "Bad magic or version number in exdr data"
    End If
    ReadSubExdr vout
End Sub
Private Sub ReadSubExdr(vout As Variant)
    Dim i As Long
    Dim sIn As String
    Dim arity As Long
    Dim col As Collection
    Dim ar() As Variant
    Dim v As Variant
    
    sIn = Space(1)
    ec_queue_read mvarStreamID, sIn, 1
    Select Case sIn
    Case "I"
        sIn = Space(4)
        ec_queue_read mvarStreamID, sIn, 4
        ec_xdr_int32 sIn, i
        vout = i
    Case "D"
        sIn = Space(8)
        ec_queue_read mvarStreamID, sIn, 8
        ec_xdr_double sIn, vout
    Case "S"
        sIn = Space(4)
        ec_queue_read mvarStreamID, sIn, 4
        ec_xdr_int32 sIn, i
        sIn = Space(i)
        ec_queue_read mvarStreamID, sIn, i
        vout = sIn
    Case "F"
        sIn = Space(9)
        ec_queue_read mvarStreamID, sIn, 9
        ec_xdr_int32 Left(sIn, 4), arity
        ReDim ar(arity)
        ec_xdr_int32 Right(sIn, 4), i
        sIn = Space(i)
        ec_queue_read mvarStreamID, sIn, i
        ar(0) = sIn
        For i = 1 To arity
            ReadSubExdr v
            If TypeName(v) = "Collection" Then
                Set ar(i) = v
            Else
                ar(i) = v
            End If
        Next i
        vout = ar
    Case "]"
        Set vout = New Collection
    Case "_"
        vout = Empty
    Case "["
        Set col = New Collection
        Do
            ReadSubExdr v
            col.Add v
            sIn = Space(1)
            ec_queue_read mvarStreamID, sIn, 1
        Loop While sIn = "["
        If sIn <> "]" Then Err.Raise _
            EC_CONVERSION_ERROR, TypeName(Me) & "::ReadExdr", _
            "Missing closing bracket for list"
        Set vout = col
    Case Else
        Err.Raise EC_CONVERSION_ERROR, TypeName(Me) & "::ReadExdr", _
            "Unrecognized exdr format (" & sIn & ")."
    End Select
End Sub
