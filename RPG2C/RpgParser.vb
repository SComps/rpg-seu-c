Imports System.IO

Public Class RpgParser
    Public Property FileSpecs As New List(Of FileSpec)
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
                Case "I"c
                    InputSpecs.Add(ParseInputSpec(line, lineNum))
                Case "C"c
                    CalcSpecs.Add(ParseCalcSpec(line, lineNum))
                Case "O"c
                    OutputSpecs.Add(ParseOutputSpec(line, lineNum))
                Case " "c, "*"c
                    ' Comment or blank
                Case Else
                    Errors.Add(New RpgError(lineNum, "INVALID SPEC TYPE", $"Spec type '{typeChars}' is not recognized (Expected H, F, I, C, O)."))
            End Select
        Next
    End Sub

    Private Function ParseFileSpec(line As String, lineNum As Integer) As FileSpec
        Dim spec As New FileSpec()
        spec.Filename = Extract(line, 7, 8)
        spec.FileType = Extract(line, 15, 1) ' I = Input, O = Output, U = Update, C = Combined
        spec.FileDesignation = Extract(line, 16, 1) ' P = Primary, S = Secondary, R = Record, T = Table, F = Full Procedural
        spec.RecordLength = ValidateInt(line, 24, 4, lineNum, "RECORD LENGTH")
        spec.Device = Extract(line, 40, 7)

        If String.IsNullOrWhiteSpace(spec.Filename) Then
            Errors.Add(New RpgError(lineNum, "MISSING FILENAME", "F-Spec must define a filename."))
        End If

        If spec.RecordLength <= 0 Then
            Dim rawVal = Extract(line, 24, 4)
            Errors.Add(New RpgError(lineNum, "INVALID REC LEN", $"Record length must be > 0 (Found: '{rawVal}' in cols 24-27)"))
        End If

        Return spec
    End Function

    Private Function ParseInputSpec(line As String, lineNum As Integer) As InputSpec
        Dim spec As New InputSpec()
        spec.Filename = Extract(line, 7, 8)
        
        ' If filename is present, it's a Record identification line
        If Not String.IsNullOrWhiteSpace(spec.Filename) Then
            spec.IsRecordLine = True
            spec.Sequence = Extract(line, 15, 2)
            spec.RecordIdIndicator = Extract(line, 19, 2)
            
            ' Basic Record ID codes (Pos 1)
            spec.IdPos1 = ValidateInt(line, 21, 4, lineNum, "ID POS 1")
            spec.IdChar1 = Extract(line, 26, 1)
        Else
            ' It's a field description line
            spec.IsRecordLine = False
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
        
        If Not String.IsNullOrWhiteSpace(spec.Filename) OrElse Extract(line, 15, 1) <> "" Then
            ' File/Record Identification line
            spec.IsRecordLine = True
            spec.Type = Extract(line, 15, 1) ' H, D, T, E
            spec.SpaceAfter = Extract(line, 19, 2)
            spec.OutputIndicator1 = Extract(line, 25, 7)
        Else
            ' Field description line
            ' IBM RPG II Column Format:
            ' Cols 32-37: Output indicators
            ' Col 39: Field name/constant indicator (Y for date)
            ' Cols 40-43: End position (4 chars, right-justified, zero-filled)
            ' Cols 45-70: Field name OR constant
            ' Col 71: Edit code
            ' Col 72: Blank after
            spec.IsRecordLine = False
            spec.OutputIndicator1 = Extract(line, 32, 6)
            spec.EndPos = ValidateInt(line, 40, 4, lineNum, "END POSITION")
            Dim fieldOrConst = Extract(line, 45, 26) ' 45-70
            ' Check if it's a constant (starts with quote) or field name
            If fieldOrConst.StartsWith("'") Then
                spec.Constant = fieldOrConst
            Else
                spec.FieldName = fieldOrConst
            End If
            spec.EditCode = Extract(line, 71, 1)

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
        If line.Length < startCol Then Return ""
        Dim len = Math.Min(length, line.Length - startCol + 1)
        Return line.Substring(startCol - 1, len).Trim()
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
    Public Property RecordLength As Integer
    Public Property Device As String
End Class

Public Class InputSpec
    Public Property IsRecordLine As Boolean
    Public Property Filename As String
    Public Property Sequence As String
    Public Property RecordIdIndicator As String
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
    Public Property Filename As String
    Public Property Type As String
    Public Property SpaceAfter As String
    Public Property OutputIndicator1 As String
    
    Public Property FieldName As String
    Public Property EndPos As Integer
    Public Property Constant As String
    Public Property EditCode As String
    Public Property EditWord As String
End Class
