Imports System.Text
Imports TN3270Framework

Public Class SeuSession
    Private _tn3270Session As TN3270Session
    Private _document As RpgDocument
    Private _currentRow As Integer = 0
    Private _currentSpecType As Char = "H"c ' Default to Control Spec initially
    Private _currentPromptLine As String = New String(" "c, 80)
    Private _currentEditLineIndex As Integer = -1 ' -1 means appending a new line
    
    Private _inMenuMode As Boolean = False
    Private _menuErrorMessage As String = ""
    Private _menuAction As String = ""
    
    ' Dimensions of the Source View area
    Private Const SourceStartRow As Integer = 1
    Private Const SourceEndRow As Integer = 17 ' Leave bottom 7 rows for formatting

    Public Sub New(tn3270Session As TN3270Session, document As RpgDocument)
        _tn3270Session = tn3270Session
        _document = document

        ' Wire up TN3270 events
        AddHandler _tn3270Session.AidKeyReceived, AddressOf OnAidKeyReceived
        AddHandler _tn3270Session.Disconnected, AddressOf OnDisconnected
    End Sub

    Public Sub Start()
        RefreshScreen()
    End Sub

    Private Sub RefreshScreen()
        _tn3270Session.ClearFields()

        If _inMenuMode Then
            DrawFileMenu()
            _tn3270Session.ShowScreen()
            Return
        End If

        ' Draw Header
        Dim filename = _document.FilePath
        If String.IsNullOrEmpty(filename) Then filename = "UNTITLED"
        Dim header = $"RPG II SEU EDITOR - {filename}".PadRight(80)
        _tn3270Session.WriteText(1, 1, header, TN3270Color.Turquoise, TN3270Color.Neutral)

        ' Draw Source List
        Dim displayRows = SourceEndRow - SourceStartRow
        For i As Integer = 0 To displayRows - 1
            Dim docIndex = _currentRow + i
            Dim rowNum = SourceStartRow + i + 1
            
            If docIndex < _document.Lines.Count Then
                Dim lineNumStr = (docIndex + 1).ToString("D4") & " "
                Dim lineData = _document.Lines(docIndex)
                Dim isEditing = (docIndex = _currentEditLineIndex)
                
                ' Add the sequence number as an unprotected field so it's selectable via Tab/Backtab
                Dim seqField = _tn3270Session.AddField(rowNum, 1, 4, (docIndex + 1).ToString("D4"), False, If(isEditing, TN3270Color.Yellow, TN3270Color.Green), TN3270Color.Neutral)
                seqField.Name = $"SEQ_{docIndex}"
                
                ' Protected source line data
                _tn3270Session.WriteText(rowNum, 6, " " & lineData.PadRight(74), If(isEditing, TN3270Color.Yellow, TN3270Color.Green), TN3270Color.Neutral)
            Else
                ' Empty area
                _tn3270Session.WriteText(rowNum, 1, "".PadRight(80), TN3270Color.Green, TN3270Color.Neutral)
            End If
        Next

        DrawFormatPrompt()

        _tn3270Session.WriteText(24, 2, "PF2=File  PF3=Exit  PF4=Prompt  PF7=Up  PF8=Down", TN3270Color.Turquoise, TN3270Color.Neutral)

        _tn3270Session.ShowScreen()
    End Sub
    
    Private Sub DrawFileMenu()
        _tn3270Session.WriteText(1, 1, "RPG II SEU - FILE OPERATION".PadRight(80), TN3270Color.Turquoise, TN3270Color.Neutral)
        
        _tn3270Session.WriteText(4, 10, "Select Action:", TN3270Color.Green, TN3270Color.Neutral)
        _tn3270Session.WriteText(5, 12, "1. New File", TN3270Color.White, TN3270Color.Neutral)
        _tn3270Session.WriteText(6, 12, "2. Load File", TN3270Color.White, TN3270Color.Neutral)
        _tn3270Session.WriteText(7, 12, "3. Save File", TN3270Color.White, TN3270Color.Neutral)
        _tn3270Session.WriteText(8, 12, "4. Return to Editor", TN3270Color.White, TN3270Color.Neutral)
        
        _tn3270Session.WriteText(10, 10, "Action Code:", TN3270Color.Green, TN3270Color.Neutral)
        Dim actionField = _tn3270Session.AddField(10, 23, 1, _menuAction, False, TN3270Color.Yellow, TN3270Color.Neutral)
        actionField.Name = "ACTION"
        actionField.IsNumeric = True
        
        _tn3270Session.WriteText(12, 10, "Filename:", TN3270Color.Green, TN3270Color.Neutral)
        Dim fname = _document.FilePath
        If fname Is Nothing Then fname = ""
        Dim fileField = _tn3270Session.AddField(12, 20, 50, fname, False, TN3270Color.Yellow, TN3270Color.Neutral)
        fileField.Name = "FILENAME"

        If Not String.IsNullOrEmpty(_menuErrorMessage) Then
            _tn3270Session.WriteText(15, 10, _menuErrorMessage, TN3270Color.Red, TN3270Color.Neutral)
        End If

        _tn3270Session.WriteText(24, 1, "PF3=Return to Editor", TN3270Color.Turquoise, TN3270Color.Neutral)
    End Sub
    
    Private Sub DrawFormatPrompt()
        Dim promptRowStart = 19
        _tn3270Session.WriteText(promptRowStart, 1, new String("-"c, 80), TN3270Color.Blue, TN3270Color.Neutral)
        
        Dim specDef = FormatDefinitions.GetFormatDefForLine(New String(" "c, 5) & _currentSpecType & New String(" "c, 74))
        
        Dim modeStr = If(_currentEditLineIndex >= 0, $"EDITING LINE {(_currentEditLineIndex + 1).ToString("D4")}", "ADDING")
        _tn3270Session.WriteText(promptRowStart + 1, 1, $"[{modeStr}] Format {Char.ToUpper(_currentSpecType)}: {specDef.Description}".PadRight(80), TN3270Color.Yellow, TN3270Color.Neutral)
        
        ' Clear prompt rows
        _tn3270Session.WriteText(promptRowStart + 2, 1, new String(" "c, 80))
        _tn3270Session.WriteText(promptRowStart + 3, 1, new String(" "c, 80))
        _tn3270Session.WriteText(promptRowStart + 4, 1, new String(" "c, 80))
        
        ' 3270 Architecture requires 1 attribute byte before every field.
        ' It's impossible to perfectly align 80 columns of fields adjacently. Flow them instead.
        
        Dim currentRow = promptRowStart + 2
        
        ' Form Type field - always allow changing the form type explicitly here
        _tn3270Session.WriteText(currentRow, 2, "Type:", TN3270Color.Turquoise, TN3270Color.Neutral)
        Dim typeField = _tn3270Session.AddField(currentRow, 8, 1, _currentSpecType.ToString(), False, TN3270Color.Pink, TN3270Color.Neutral)
        typeField.Name = "FORM_TYPE"
        
        Dim currentCol = 10 ' skip "Type:" (2..6) + Attr(7) + Field(8) + Terminator(9)
        
        For Each field In specDef.Fields
            If field.Name = "Form Type" Then Continue For ' handled
            
            Dim labelStr = field.Name
            If labelStr.Length > 15 Then labelStr = labelStr.Substring(0, 15).Trim()
            
            ' space needed: labelStr(len) + ": "(1) + attr(1) + field(len) + gap(1) = label + field + 3 
            ' we add +4 to be safe 
            Dim spaceNeeded = labelStr.Length + field.Length + 4
            
            If currentCol + spaceNeeded >= 80 Then
                currentRow += 1
                currentCol = 2
                If currentRow > promptRowStart + 4 Then Exit For ' Failsafe
            End If
            
            _tn3270Session.WriteText(currentRow, currentCol, labelStr & ":", TN3270Color.Turquoise, TN3270Color.Neutral)
            currentCol += labelStr.Length + 1
            
            ' Provide column for the input attribute byte
            currentCol += 1 
            
            ' Extract current data for this field
            Dim currentData = ""
            If _currentPromptLine.Length >= field.StartColumn - 1 + field.Length Then
                 currentData = _currentPromptLine.Substring(field.StartColumn - 1, field.Length)
            End If
            
            ' Add unprotected input field
            Dim tField = _tn3270Session.AddField(currentRow, currentCol, field.Length, currentData, False, TN3270Color.White, TN3270Color.Neutral)
            tField.Name = field.Name
            tField.IsNumeric = field.IsNumeric
            
            ' Move past the field data and a gap
            currentCol += field.Length + 1
        Next
    End Sub
    
    Private Sub RebuildPromptLineFromFields()
        Dim sb = New StringBuilder(New String(" "c, 80))
        Dim specDef = FormatDefinitions.GetFormatDefForLine(New String(" "c, 5) & _currentSpecType & New String(" "c, 74))
        
        ' Check if Form Type was changed manually
        Dim typeField = _tn3270Session.GetFieldByName("FORM_TYPE")
        If typeField IsNot Nothing AndAlso Not String.IsNullOrEmpty(typeField.Content) Then
            _currentSpecType = Char.ToUpper(typeField.Content(0))
            specDef = FormatDefinitions.GetFormatDefForLine(New String(" "c, 5) & _currentSpecType & New String(" "c, 74))
        End If
        
        For Each field In specDef.Fields
            If field.Name = "Form Type" Then Continue For ' handled
            
            Dim tField = _tn3270Session.GetFieldByName(field.Name)
            If tField IsNot Nothing Then
                 Dim val = tField.Content.PadRight(field.Length, " "c)
                 If val.Length > field.Length Then val = val.Substring(0, field.Length)
                 For i As Integer = 0 To field.Length - 1
                     sb(field.StartColumn - 1 + i) = val(i)
                 Next
            End If
        Next
        
        _currentPromptLine = sb.ToString()
        ' Force the Form Type character to be correct
        Dim chars = _currentPromptLine.ToCharArray()
        chars(5) = Char.ToUpper(_currentSpecType)
        _currentPromptLine = New String(chars)
    End Sub

    Private Sub ProcessMenuInput()
        Dim actionField = _tn3270Session.GetFieldByName("ACTION")
        Dim fileField = _tn3270Session.GetFieldByName("FILENAME")
        _menuErrorMessage = ""
        
        Dim action = ""
        Dim filename = ""
        
        If actionField IsNot Nothing Then action = actionField.Content.Trim()
        If fileField IsNot Nothing Then filename = fileField.Content.Trim()
        
        Select Case action
            Case "1" ' New File
                _document.Lines.Clear()
                _document.Lines.Add(New String(" "c, 80))
                _document.FilePath = filename
                _currentRow = 0
                _currentPromptLine = New String(" "c, 80)
                _inMenuMode = False
                
            Case "2" ' Load File
                Try
                    If String.IsNullOrEmpty(filename) Then
                        _menuErrorMessage = "Filename is required to load."
                    Else
                        _document.Load(filename)
                        _currentRow = 0
                        _currentPromptLine = New String(" "c, 80)
                        _inMenuMode = False
                    End If
                Catch ex As Exception
                    _menuErrorMessage = $"Error loading file: {ex.Message}"
                End Try
                
            Case "3" ' Save File
                Try
                    If String.IsNullOrEmpty(filename) Then
                        _menuErrorMessage = "Filename is required to save."
                    Else
                        _document.SaveAs(filename)
                        _menuErrorMessage = "File saved successfully."
                    End If
                Catch ex As Exception
                    _menuErrorMessage = $"Error saving file: {ex.Message}"
                End Try
                
            Case "4" ' Return 
                _inMenuMode = False
        End Select
    End Sub

    Private Sub OnAidKeyReceived(sender As Object, e As AidKeyEventArgs)
        If _inMenuMode Then
            If e.AidKey = &HF3 Then
                _inMenuMode = False
                RefreshScreen()
                Return
            End If
            
            ProcessMenuInput()
            RefreshScreen()
            Return
        End If

        ' Process input from fields before handling actions
        RebuildPromptLineFromFields()
        
        Select Case e.AidKey
            Case &H7D, &H7A ' Enter 
                ' User pressed Enter to submit the line to the document
                If Not String.IsNullOrWhiteSpace(_currentPromptLine) Then
                     If _currentEditLineIndex >= 0 Then
                          ' Update existing line
                          _document.Lines(_currentEditLineIndex) = _currentPromptLine
                          ' Return to add mode
                          _currentEditLineIndex = -1
                          _currentPromptLine = New String(" "c, 80)
                     Else
                          ' Append new line
                          _document.Lines.Add(_currentPromptLine)
                          ' Keep _currentRow tracking the bottom so it auto-scrolls down
                          If _document.Lines.Count > (SourceEndRow - SourceStartRow) Then
                              _currentRow = _document.Lines.Count - (SourceEndRow - SourceStartRow)
                          End If
                          ' Clear prompt line for next entry EXCEPT form type & page/line which we might want to retain, 
                          ' but for now just clear it all.
                          _currentPromptLine = New String(" "c, 80)
                     End If
                End If
                RefreshScreen()
                
            Case &HF2 ' PF2
                _inMenuMode = True
                _menuErrorMessage = ""
                _menuAction = ""
                RefreshScreen()
                
            Case &HF4 ' PF4
                ' Cycle Form Type
                If _currentSpecType = "H"c Then 
                    _currentSpecType = "F"c
                ElseIf _currentSpecType = "F"c Then 
                    _currentSpecType = "E"c
                ElseIf _currentSpecType = "E"c Then 
                    _currentSpecType = "L"c
                ElseIf _currentSpecType = "L"c Then 
                    _currentSpecType = "I"c
                ElseIf _currentSpecType = "I"c Then 
                    _currentSpecType = "C"c
                ElseIf _currentSpecType = "C"c Then 
                    _currentSpecType = "O"c
                Else 
                    _currentSpecType = "H"c
                End If
                
                _currentPromptLine = New String(" "c, 80)
                _currentEditLineIndex = -1 ' Also clear edit mode if we explicitly cycle form type
                RefreshScreen()
            
            Case &HF5 ' PF5 - Select line at cursor for editing
                Dim cursorRow = (e.CursorAddress \ _tn3270Session.Columns) + 1
                Dim physicalRow = cursorRow
                
                If physicalRow > SourceStartRow AndAlso physicalRow <= SourceEndRow Then
                     Dim indexInView = physicalRow - SourceStartRow - 1
                     Dim docIndex = _currentRow + indexInView
                     If docIndex >= 0 AndAlso docIndex < _document.Lines.Count Then
                         ' Load this line into the prompt
                         _currentEditLineIndex = docIndex
                         _currentPromptLine = _document.Lines(docIndex)
                         ' Set proper form type character based on line
                         If _currentPromptLine.Length >= 6 AndAlso _currentPromptLine(5) <> " "c Then
                              _currentSpecType = Char.ToUpper(_currentPromptLine(5))
                         End If
                         RefreshScreen()
                     Else
                         ' Not a valid document line
                         _currentEditLineIndex = -1
                         _currentPromptLine = New String(" "c, 80)
                         RefreshScreen()
                     End If
                Else
                     ' If they just hit PF5 without cursor positioned in the source, just cancel edit mode
                     _currentEditLineIndex = -1
                     _currentPromptLine = New String(" "c, 80)
                     RefreshScreen()
                End If
                
            Case &HF6 ' PF6 - Delete line at cursor
                Dim cursorRow = (e.CursorAddress \ _tn3270Session.Columns) + 1
                Dim physicalRow = cursorRow

                If physicalRow > SourceStartRow AndAlso physicalRow <= SourceEndRow Then
                     Dim indexInView = physicalRow - SourceStartRow - 1
                     Dim docIndex = _currentRow + indexInView
                     If docIndex >= 0 AndAlso docIndex < _document.Lines.Count Then
                         _document.Lines.RemoveAt(docIndex)
                         ' If we deleted the line we were editing, cancel edit mode
                         If _currentEditLineIndex = docIndex Then
                             _currentEditLineIndex = -1
                             _currentPromptLine = New String(" "c, 80)
                         ElseIf _currentEditLineIndex > docIndex Then
                             ' Shift edit index if it was below the deleted line
                             _currentEditLineIndex -= 1
                         End If
                         RefreshScreen()
                     End If
                End If

            Case &HF3 ' PF3
                _tn3270Session.Close()
                
            Case &HF7 ' PF7 (Page Up)
                If _currentRow > 0 Then _currentRow = Math.Max(0, _currentRow - (SourceEndRow - SourceStartRow))
                RefreshScreen()
                
            Case &HF8 ' PF8 (Page Down)
                If _currentRow + (SourceEndRow - SourceStartRow) < _document.Lines.Count Then 
                    _currentRow += (SourceEndRow - SourceStartRow)
                End If
                RefreshScreen()
                
            Case Else
                RefreshScreen()
        End Select
    End Sub

    Private Sub OnDisconnected(sender As Object, e As EventArgs)
        Console.WriteLine("SEU Session Disconnected.")
    End Sub
End Class
