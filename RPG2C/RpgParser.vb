Imports System.IO

Public Class RpgParser
    Public Property FileSpecs As New List(Of FileSpec)
    Public Property LineCounterSpecs As New List(Of LineCounterSpec)
    Public Property InputSpecs As New List(Of InputSpec)
    Public Property CalcSpecs As New List(Of CalcSpec)
    Public Property OutputSpecs As New List(Of OutputSpec)
    Public Property Errors As New List(Of RpgError)

    Public Sub ParseFile(filepath As String)
        Dim lines = File.ReadAllLines(filepath)
        
        For lineNum As Integer = 1 To lines.Length
            Dim rawLine = lines(lineNum - 1)
            ' Expand tabs to spaces (standard 8-column alignment) before parsing columns
            Dim line = ExpandTabs(rawLine, 8).PadRight(80)
            
            If line.Length < 6 Then 
                Errors.Add(New RpgError(lineNum, "LINE TOO SHORT", "Line must be at least 6 characters for spec type identification."))
                Continue For
            End If

            Dim typeChars = line(5)
            
            Select Case Char.ToUpper(typeChars)
                Case "H"c
                    ' Control spec - currently ignored in generator but parsed
                Case "F"c
                    FileSpecs.Add(ParseFileSpec(line, lineNum))
                Case "L"c
                    LineCounterSpecs.Add(ParseLineCounterSpec(line, lineNum))
                Case "I"c
                    InputSpecs.Add(ParseInputSpec(line, lineNum))
                Case "C"c
                    CalcSpecs.Add(ParseCalcSpec(line, lineNum))
                Case "O"c
                    OutputSpecs.Add(ParseOutputSpec(line, lineNum))
                Case " "c, "*"c
                    ' Comment or blank
                Case Else
                    Errors.Add(New RpgError(lineNum, "INVALID SPEC TYPE", $"Spec type '{typeChars}' is not recognized (Expected H, F, L, I, C, O)."))
            End Select
        Next
    End Sub

    Private Function ParseFileSpec(line As String, lineNum As Integer) As FileSpec
        Dim spec As New FileSpec()
        spec.Filename = Extract(line, 7, 8)
        spec.FileType = Extract(line, 15, 1) ' I = Input, O = Output, U = Update, C = Combined
        spec.FileDesignation = Extract(line, 16, 1) ' P = Primary, S = Secondary, R = Record, T = Table, F = Full Procedural
        spec.EndOfFile = Extract(line, 17, 1) ' E = End of File
        spec.FileFormat = Extract(line, 18, 1) ' F = Fixed, V = Variable
        
        ' IBM RPG II: Cols 24-27 = Record Length (first part), Cols 28-31 = Record Length continuation or Key Field
        Dim recLen1 = ExtractExact(line, 24, 4)
        Dim recLen2 = ExtractExact(line, 28, 4)
        
        ' Parse record length - typically zero-filled like "00870087" means 87 bytes
        If Not String.IsNullOrWhiteSpace(recLen1) Then
            spec.RecordLength = ValidateInt(line, 24, 4, lineNum, "RECORD LENGTH")
        End If
        
        spec.KeyFieldStart = ExtractExact(line, 28, 4) ' May be key field or record length continuation
        spec.OverflowIndicator = Extract(line, 33, 2) ' OF = Overflow
        spec.Device = Extract(line, 40, 7)

        If String.IsNullOrWhiteSpace(spec.Filename) Then
            Errors.Add(New RpgError(lineNum, "MISSING FILENAME", "F-Spec must define a filename."))
        End If

        If spec.RecordLength <= 0 Then
            Dim rawVal = ExtractExact(line, 24, 4)
            Errors.Add(New RpgError(lineNum, "INVALID REC LEN", $"Record length must be > 0 (Found: '{rawVal}' in cols 24-27)"))
        End If

        Return spec
    End Function

    Private Function ParseLineCounterSpec(line As String, lineNum As Integer) As LineCounterSpec
        Dim spec As New LineCounterSpec()
        spec.Filename = Extract(line, 7, 8)
        
        ' IBM RPG II L-Spec format:
        ' Cols 7-14: Filename
        ' Cols 15-17: Lines per page (e.g., "001")
        ' Cols 18-20: Line number for overflow (e.g., "010")
        ' Cols 21-22: Line number for first detail line (e.g., "60")
        ' Cols 23-24: Line number for last detail line (e.g., "12")
        
        spec.LinesPerPage = ValidateInt(line, 15, 3, lineNum, "LINES PER PAGE")
        spec.OverflowLine = ValidateInt(line, 18, 3, lineNum, "OVERFLOW LINE")
        
        ' The remaining columns may contain line counter values
        Dim lineCounterData = ExtractExact(line, 21, 4)
        spec.LineCounterData = lineCounterData
        
        If String.IsNullOrWhiteSpace(spec.Filename) Then
            Errors.Add(New RpgError(lineNum, "MISSING FILENAME", "L-Spec must define a filename."))
        End If
        
        Return spec
    End Function

    Private Function ParseInputSpec(line As String, lineNum As Integer) As InputSpec
        Dim spec As New InputSpec()
        spec.Filename = Extract(line, 7, 8)
        
        ' If filename is present, it's a Record identification line
        If Not String.IsNullOrWhiteSpace(spec.Filename) Then
            spec.IsRecordLine = True
            
            ' IBM RPG II I-Spec Record Identification:
            ' Cols 7-14: Filename
            ' Cols 15-16: Sequence (AA, BB, etc.)
            ' Col 17: Number (N)
            ' Col 18: Option (O)
            ' Cols 19-20: Record identifying indicator (01-99)
            ' Cols 21-22: Sequence number
            
            spec.Sequence = ExtractExact(line, 15, 2)
            spec.Number = Extract(line, 17, 1)
            spec.OptionEntry = Extract(line, 18, 1)
            spec.RecordIdIndicator = ExtractExact(line, 19, 2)
            spec.SequenceNumber = ExtractExact(line, 21, 2)
            
            ' Basic Record ID codes (Pos 1) - for matching logic
            spec.IdPos1 = ValidateInt(line, 21, 4, lineNum, "ID POS 1")
            spec.IdChar1 = Extract(line, 26, 1)
        Else
            ' It's a field description line
            spec.IsRecordLine = False
            
            ' IBM RPG II I-Spec Field Description:
            ' Cols 44-47: From position (zero-filled, right-justified)
            ' Cols 48-51: To position (zero-filled, right-justified)
            ' Col 52: Decimal positions
            ' Cols 53-58: Field name
            
            spec.StartPos = ValidateInt(line, 44, 4, lineNum, "FROM POSITION")
            spec.EndPos = ValidateInt(line, 48, 4, lineNum, "TO POSITION")
            spec.DecimalPos = Extract(line, 52, 1)
            spec.IsNumeric = Not String.IsNullOrWhiteSpace(spec.DecimalPos)
            spec.DataType = Extract(line, 43, 1) ' P=Packed, B=Binary
            spec.FieldName = Extract(line, 53, 6)
            spec.ControlLevel = Extract(line, 59, 2) ' L1-L9

            If String.IsNullOrWhiteSpace(spec.FieldName) Then
                Errors.Add(New RpgError(lineNum, "MISSING FIELD", "I-Spec field description must define a field name."))
            End If

            If spec.StartPos <= 0 Or spec.EndPos <= 0 Then
                Errors.Add(New RpgError(lineNum, "POS OUT RANGE", "Positions must be greater than zero."))
            ElseIf spec.StartPos > spec.EndPos Then
                 Errors.Add(New RpgError(lineNum, "INVALID RANGE", "Start position cannot be greater than end position."))
            End If

            If spec.IsNumeric AndAlso Not String.IsNullOrEmpty(spec.DecimalPos) Then
                Dim decVal As Integer
                If Not Integer.TryParse(spec.DecimalPos, decVal) OrElse decVal < 0 OrElse decVal > 9 Then
                    Errors.Add(New RpgError(lineNum, "INVALID DEC POS", "Decimal position must be between 0 and 9."))
                End If
            End If
        End If
        
        Return spec
    End Function

    Private Function ParseCalcSpec(line As String, lineNum As Integer) As CalcSpec
        Dim spec As New CalcSpec()
        
        ' IBM RPG II Column Format:
        ' Cols 7-8: Control level (L1-L9, LR, SR) OR conditioning indicators
        ' Cols 9-17: Factor 1 (9 chars) OR conditioning indicators (if cols 7-8 have indicators)
        ' Cols 18-27: Operation (10 chars)
        ' Cols 28-32: Factor 2 (5 chars)
        ' Cols 33-42: Result Field (10 chars)
        ' Cols 43-48: Field Length (3 digits) + Decimal Positions (3 digits)
        ' Cols 49-51: Resulting Indicators (Hi/Lo/Eq)
        ' Cols 54-59: Additional Indicators
        
        spec.ControlLevel = Extract(line, 7, 2) ' L0-L9, LR, SR
        
        ' Conditioning indicators (columns 7-17) - only if not using control level
        ' For now, we'll skip conditioning indicator parsing as it's complex
        ' and overlaps with Factor1. Most programs don't use them.
        spec.Indicator1 = ""
        spec.Not1 = False
        spec.Indicator2 = ""
        spec.Not2 = False
        spec.Indicator3 = ""
        spec.Not3 = False
        
        spec.Factor1 = Extract(line, 9, 9)
        spec.Opcode = Extract(line, 18, 10)
        spec.Factor2 = Extract(line, 28, 5)
        spec.ResultField = Extract(line, 33, 10)
        spec.FieldLength = ValidateInt(line, 43, 3, lineNum, "FIELD LENGTH")
        spec.DecimalPos = Extract(line, 46, 3)
        spec.HalfAdjust = Extract(line, 49, 1)
        spec.ResultingIndicatorHi = Extract(line, 50, 2)
        spec.ResultingIndicatorLo = Extract(line, 52, 2)
        spec.ResultingIndicatorEq = Extract(line, 54, 2)

        If String.IsNullOrWhiteSpace(spec.Opcode) Then
            Errors.Add(New RpgError(lineNum, "MISSING OPCODE", "C-Spec must define an operation code."))
        End If

        Return spec
    End Function

    Private Function ParseOutputSpec(line As String, lineNum As Integer) As OutputSpec
        Dim spec As New OutputSpec()
        spec.Filename = Extract(line, 7, 8)
        
        ' Check if this is a record identification line or field line
        Dim typeChar = Extract(line, 15, 1)
        
        If Not String.IsNullOrWhiteSpace(spec.Filename) OrElse typeChar <> "" Then
            ' File/Record Identification line
            spec.IsRecordLine = True
            
            ' IBM RPG II O-Spec Record Identification:
            ' Cols 7-14: Filename
            ' Col 15: Type (H=Heading, D=Detail, T=Total, E=Exception)
            ' Col 16: Fetch overflow
            ' Cols 17-18: Space before
            ' Col 19: Space after
            ' Cols 21-22: Skip before
            ' Col 23: Skip after
            ' Cols 25-31: Output indicators (e.g., 1P, OF, 01)
            
            spec.Type = typeChar
            spec.FetchOverflow = Extract(line, 16, 1)
            spec.SpaceBefore = ExtractExact(line, 17, 2)
            spec.SpaceAfter = ExtractExact(line, 19, 1)
            spec.SkipBefore = ExtractExact(line, 21, 2)
            spec.SkipAfter = Extract(line, 23, 1)
            spec.OutputIndicator1 = ExtractExact(line, 25, 7)
            
            ' Check for OR/OF overflow line (no filename, just OR or OF in indicator position)
            If String.IsNullOrWhiteSpace(spec.Filename) AndAlso spec.OutputIndicator1.Trim() = "OR" Then
                spec.IsOverflowLine = True
                spec.OverflowType = "OR"
            ElseIf String.IsNullOrWhiteSpace(spec.Filename) AndAlso spec.OutputIndicator1.Trim() = "OF" Then
                spec.IsOverflowLine = True
                spec.OverflowType = "OF"
            End If
        Else
            ' Field description line
            ' IBM RPG II O-Spec Field Description:
            ' Cols 32-37: Field name (for variables)
            ' Col 38: Edit code
            ' Col 39: Blank after
            ' Cols 40-43: End position (zero-filled, right-justified)
            ' Cols 45-70: Constant (for literals in quotes)
            
            spec.IsRecordLine = False
            
            ' Field name is in cols 32-37 for variable output
            Dim fieldName = Extract(line, 32, 6)
            
            ' End position in cols 40-43
            spec.EndPos = ValidateInt(line, 40, 4, lineNum, "END POSITION")
            
            ' Constant in cols 45-70
            Dim constant = ExtractExact(line, 45, 26)
            
            ' Check if it's a constant (starts with quote) or field name
            If Not String.IsNullOrWhiteSpace(constant) AndAlso constant.Trim().StartsWith("'") Then
                spec.Constant = constant.Trim()
            ElseIf Not String.IsNullOrWhiteSpace(fieldName) Then
                spec.FieldName = fieldName
            End If
            
            spec.EditCode = Extract(line, 38, 1)
            spec.BlankAfter = Extract(line, 39, 1)

            If String.IsNullOrWhiteSpace(spec.FieldName) AndAlso String.IsNullOrWhiteSpace(spec.Constant) Then
                Errors.Add(New RpgError(lineNum, "SPEC UNDEFINED", "Output field line must define either a field name or a constant."))
            End If

            If spec.EndPos <= 0 Then
                Errors.Add(New RpgError(lineNum, "INVALID END POS", "Output end position must be greater than zero."))
            End If
        End If
        
        Return spec
    End Function

    Private Function Extract(line As String, startCol As Integer, length As Integer) As String
        ' Extract and trim - use for field names and values where leading/trailing spaces don't matter
        If line.Length < startCol Then Return ""
        Dim len = Math.Min(length, line.Length - startCol + 1)
        Return line.Substring(startCol - 1, len).Trim()
    End Function
    
    Private Function ExtractExact(line As String, startCol As Integer, length As Integer) As String
        ' Extract exact content without trimming - CRITICAL for IBM RPG II column positioning
        ' Spaces are significant in RPG II and must be preserved
        If line.Length < startCol Then Return ""
        Dim len = Math.Min(length, line.Length - startCol + 1)
        Return line.Substring(startCol - 1, len)
    End Function

    Private Function ExtractInt(line As String, startCol As Integer, length As Integer) As Integer
        Dim str = Extract(line, startCol, length)
        If String.IsNullOrWhiteSpace(str) Then Return 0
        Dim val As Integer
        If Integer.TryParse(str, val) Then Return val
        Return 0
    End Function

    Private Function ValidateInt(line As String, startCol As Integer, length As Integer, lineNum As Integer, fieldDesc As String) As Integer
        Dim str = Extract(line, startCol, length)
        If String.IsNullOrWhiteSpace(str) Then Return 0
        Dim val As Integer
        If Integer.TryParse(str, val) Then Return val
        
        ' If we're here, it means we have non-numeric data in a numeric column
        Errors.Add(New RpgError(lineNum, "NOT NUMERIC", $"{fieldDesc} column ({startCol}-{startCol+length-1}) contains non-numeric data: '{str}'"))
        Return 0
    End Function

    Private Function ExpandTabs(s As String, tabSize As Integer) As String
        Dim sb As New System.Text.StringBuilder()
        For Each c In s
            If c = vbTab Then
                Dim spaces = tabSize - (sb.Length Mod tabSize)
                sb.Append(New String(" "c, spaces))
            Else
                sb.Append(c)
            End If
        Next
        Return sb.ToString()
    End Function
End Class

Public Class RpgError
    Public Property LineNumber As Integer
    Public Property ErrorCode As String
    Public Property Description As String

    Public Sub New(lineNum As Integer, code As String, desc As String)
        LineNumber = lineNum
        ErrorCode = code
        Description = desc
    End Sub

    Public Overrides Function ToString() As String
        Return $"* ERR-{ErrorCode.PadRight(15)} AT LINE {LineNumber.ToString().PadLeft(4)}: {Description}"
    End Function
End Class

Public Class FileSpec
    Public Property Filename As String
    Public Property FileType As String
    Public Property FileDesignation As String
    Public Property EndOfFile As String
    Public Property FileFormat As String
    Public Property RecordLength As Integer
    Public Property KeyFieldStart As String
    Public Property OverflowIndicator As String
    Public Property Device As String
End Class

Public Class LineCounterSpec
    Public Property Filename As String
    Public Property LinesPerPage As Integer
    Public Property OverflowLine As Integer
    Public Property LineCounterData As String
End Class

Public Class InputSpec
    Public Property IsRecordLine As Boolean
    Public Property Filename As String
    Public Property Sequence As String
    Public Property Number As String
    Public Property OptionEntry As String
    Public Property RecordIdIndicator As String
    Public Property SequenceNumber As String
    Public Property IdPos1 As Integer
    Public Property IdChar1 As String
    
    Public Property StartPos As Integer
    Public Property EndPos As Integer
    Public Property DecimalPos As String
    Public Property FieldName As String
    Public Property IsNumeric As Boolean
    Public Property ControlLevel As String
    Public Property DataType As String ' P=Packed, B=Binary
End Class

Public Class CalcSpec
    Public Property ControlLevel As String
    Public Property Indicator1 As String
    Public Property Not1 As Boolean
    Public Property Indicator2 As String
    Public Property Not2 As Boolean
    Public Property Indicator3 As String
    Public Property Not3 As Boolean

    Public Property Factor1 As String
    Public Property Opcode As String
    Public Property Factor2 As String
    Public Property ResultField As String
    Public Property FieldLength As Integer
    Public Property DecimalPos As String
    Public Property HalfAdjust As String
    Public Property ResultingIndicatorHi As String
    Public Property ResultingIndicatorLo As String
    Public Property ResultingIndicatorEq As String
End Class

Public Class OutputSpec
    Public Property IsRecordLine As Boolean
    Public Property IsOverflowLine As Boolean
    Public Property OverflowType As String
    Public Property Filename As String
    Public Property Type As String
    Public Property FetchOverflow As String
    Public Property SpaceBefore As String
    Public Property SpaceAfter As String
    Public Property SkipBefore As String
    Public Property SkipAfter As String
    Public Property OutputIndicator1 As String
    
    Public Property FieldName As String
    Public Property EndPos As Integer
    Public Property Constant As String
    Public Property EditCode As String
    Public Property BlankAfter As String
    Public Property EditWord As String
End Class
