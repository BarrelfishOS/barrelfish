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
Attribute VB_Name = "EclipseStreams"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"EclipseStream"
Attribute VB_Ext_KEY = "Member0" ,"EclipseStream"
Attribute VB_Ext_KEY = "Top_Level" ,"No"
Option Explicit

Private Declare Function ec_stream_nr _
        Lib "Eclipse.dll" _
        (ByVal StreamName As String) _
        As Long

'local variable to hold collection
Private mCol As Collection
Private mMap() As String 'maps stream ids to collection keys

Public Sub Flush()
    Dim es As EclipseStream
    For Each es In mCol
        es.Flush
    Next
End Sub
Public Function Add(Key As String, Mode As EclipseStreamMode) As EclipseStream
    Dim NewID As Long
    Dim OtherStream As EclipseStream
    
    'find out steam id
    NewID = ec_stream_nr(Key)
    
    'enter into map from streamid => stream
    If NewID > UBound(mMap) Then
        ReDim Preserve mMap(NewID + 100)
    End If
    mMap(NewID) = Key
    
    'confirm this is not an already aliased stream
    If Mode = FromEclipse Then
        For Each OtherStream In mCol
            If OtherStream.id = NewID And OtherStream.Mode = FromEclipse Then
                Err.Raise 1, TypeName(Me) & "::Add", _
                    "Trying to input from a queue that is aliased to" _
                    & " another queue (" & OtherStream.Key & ")."
            End If
        Next
    End If
    'create a new object
    Dim objNewMember As EclipseStream
    Set objNewMember = New EclipseStream

    'set the properties passed into the method
    objNewMember.Key = Key
    objNewMember.Mode = Mode
    objNewMember.id = NewID

    mCol.Add objNewMember, Key

    'return the object created
    Set Add = objNewMember
    Set objNewMember = Nothing


End Function
Friend Function Name(id As Long) As EclipseStream
    Set Name = Item(mMap(id))
End Function

Public Property Get Item(vntIndexKey As Variant) As EclipseStream
Attribute Item.VB_UserMemId = 0
    'used when referencing an element in the collection
    'vntIndexKey contains either the Index or Key to the collection,
    'this is why it is declared as a Variant
    'Syntax: Set foo = x.Item(xyz) or Set foo = x.Item(5)
  Set Item = mCol(vntIndexKey)
End Property



Public Property Get Count() As Long
    'used when retrieving the number of elements in the
    'collection. Syntax: Debug.Print x.Count
    Count = mCol.Count
End Property


Public Sub Remove(vntIndexKey As Variant)
    'used when removing an element from the collection
    'vntIndexKey contains either the Index or Key, which is why
    'it is declared as a Variant
    'Syntax: x.Remove(xyz)


    mCol.Remove vntIndexKey
End Sub


Public Property Get NewEnum() As IUnknown
    'this property allows you to enumerate
    'this collection with the For...Each syntax
    Set NewEnum = mCol.[_NewEnum]
End Property


Private Sub Class_Initialize()
    Dim es As EclipseStream
    
    ' intial bounds on map array
    ReDim mMap(0 To 100)
    'creates the collection when this class is created
    Set mCol = New Collection
    Set es = Add("input", ToEclipse)
    es.id = 0
    es.Prompt = "Waiting for input on stream 'input'."
    Set es = Add("output", FromEclipse)
    es.id = 1
    Set es = Add("error", FromEclipse)
    es.id = 2
End Sub


Private Sub Class_Terminate()
    'destroys collection when this class is terminated
    Set mCol = Nothing
End Sub

