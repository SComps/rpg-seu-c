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
            ' Pad line to ensure fixed-column extraction works even on manually created short files
            Dim line = rawLine.PadRight(80)
            
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
        spec.FileType = Extract(line, 15, 1) ' I = Input, O = Output
        spec.FileDesignation = Extract(line, 16, 1) ' P = Primary
        spec.RecordLength = ExtractInt(line, 24, 4)
        spec.Device = Extract(line, 40, 7)

        If String.IsNullOrWhiteSpace(spec.Filename) Then
            Errors.Add(New RpgError(lineNum, "MISSING FILENAME", "F-Spec must define a filename."))
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
            spec.IdPos1 = ExtractInt(line, 21, 4)
            spec.IdChar1 = Extract(line, 26, 1)
        Else
            ' It's a field description line
            spec.IsRecordLine = False
            spec.StartPos = ExtractInt(line, 44, 4)
            spec.EndPos = ExtractInt(line, 48, 4)
            spec.DecimalPos = Extract(line, 52, 1)
            spec.FieldName = Extract(line, 53, 6)
            spec.IsNumeric = Not String.IsNullOrWhiteSpace(spec.DecimalPos)

            If String.IsNullOrWhiteSpace(spec.FieldName) Then
                Errors.Add(New RpgError(lineNum, "MISSING FIELD NAME", "I-Spec field description must define a field name."))
            End If
        End If
        
        Return spec
    End Function

    Private Function ParseCalcSpec(line As String, lineNum As Integer) As CalcSpec
        Dim spec As New CalcSpec()
        spec.Indicators = Extract(line, 9, 8)
        spec.Factor1 = Extract(line, 18, 10)
        spec.Opcode = Extract(line, 28, 5)
        spec.Factor2 = Extract(line, 33, 10)
        spec.ResultField = Extract(line, 43, 6)
        spec.FieldLength = ExtractInt(line, 49, 3)
        spec.DecimalPos = Extract(line, 52, 1)
        spec.HalfAdjust = Extract(line, 53, 1)
        spec.ResultingIndicatorHi = Extract(line, 54, 2)
        spec.ResultingIndicatorLo = Extract(line, 56, 2)
        spec.ResultingIndicatorEq = Extract(line, 58, 2)

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
            spec.SpaceAfter = Extract(line, 18, 1)
            spec.OutputIndicator1 = Extract(line, 23, 2) ' Simplification
        Else
            ' Field description line
            spec.IsRecordLine = False
            spec.FieldName = Extract(line, 32, 6)
            spec.EndPos = ExtractInt(line, 40, 4)
            spec.Constant = Extract(line, 45, 24)
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
End Class

Public Class CalcSpec
    Public Property Indicators As String
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
End Class
