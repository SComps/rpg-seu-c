Imports System.Text
Imports System.Linq

Public Class CGenerator
    Private _parser As RpgParser

    Public Sub New(parser As RpgParser)
        _parser = parser
    End Sub

    Public Function Generate() As String
        Dim sb As New StringBuilder()

        ' Headers
        sb.AppendLine("#include <stdio.h>")
        sb.AppendLine("#include <stdlib.h>")
        sb.AppendLine("#include <string.h>")
        sb.AppendLine("#include <stdbool.h>")
        sb.AppendLine()

        ' State variables
        sb.AppendLine("// --- Indicators ---")
        sb.AppendLine("bool IND[100] = {false};")
        sb.AppendLine("bool IN_LR = false;")
        sb.AppendLine("bool IN_MR = false;")
        sb.AppendLine()

        sb.AppendLine("// --- Program Variables ---")
        Dim vars = GetVariables()
        For Each v In vars.Values
            If v.IsNumeric Then
                sb.AppendLine($"double {v.Name} = 0.0;")
            Else
                sb.AppendLine($"char {v.Name}[{v.Length + 1}] = {{0}};")
            End If
        Next
        sb.AppendLine()

        ' Function Signature
        sb.AppendLine("int main(int argc, char** argv) {")
        
        ' File Setup
        Dim files = _parser.FileSpecs
        Dim expectedArgs = files.Count + 1
        
        sb.AppendLine($"    if (argc < {expectedArgs}) {{")
        sb.Append("        printf(""Usage: %s ")
        For Each f In files
            sb.Append($"<{f.Filename}> ")
        Next
        sb.AppendLine("\n"", argv[0]);")
        sb.AppendLine("        return 1;")
        sb.AppendLine("    }")
        sb.AppendLine()

        Dim fileIndex = 1
        Dim inFileData As FileSpec = Nothing
        Dim outFileData As FileSpec = Nothing
        
        For Each f In files
            If f.FileType = "I" Then
                inFileData = f
                sb.AppendLine($"    FILE* {f.Filename} = fopen(argv[{fileIndex}], ""r"");")
                sb.AppendLine($"    if (!{f.Filename}) {{ printf(""Failed to open {f.Filename}\n""); return 1; }}")
            ElseIf f.FileType = "O" Then
                outFileData = f
                sb.AppendLine($"    FILE* {f.Filename} = fopen(argv[{fileIndex}], ""w"");")
                sb.AppendLine($"    if (!{f.Filename}) {{ printf(""Failed to open {f.Filename}\n""); return 1; }}")
            End If
            fileIndex += 1
        Next
        sb.AppendLine()
        
        If inFileData IsNot Nothing Then
            Dim recLen = If(inFileData.RecordLength > 0, inFileData.RecordLength, 80)
            sb.AppendLine($"    char recordBuf[{recLen + 2}];")
        End If
        sb.AppendLine()

        ' RPG Logic Cycle Loop
        sb.AppendLine("    // --- Main Logic Cycle ---")
        If inFileData IsNot Nothing Then
            sb.AppendLine($"    while (fgets(recordBuf, sizeof(recordBuf), {inFileData.Filename})) {{")
            ' Strip newline
            sb.AppendLine("        recordBuf[strcspn(recordBuf, ""\r\n"")] = 0;")
            
            ' Determine Record ID and extract input fields
            For Each iSpec In _parser.InputSpecs
                If Not iSpec.IsRecordLine Then
                    ' Extract field
                    If vars.ContainsKey(iSpec.FieldName) Then
                        Dim v = vars(iSpec.FieldName)
                        Dim len = iSpec.EndPos - iSpec.StartPos + 1
                        sb.AppendLine($"        // Extract {iSpec.FieldName}")
                        sb.AppendLine($"        char tmp_{iSpec.FieldName}[{len + 1}];")
                        sb.AppendLine($"        strncpy(tmp_{iSpec.FieldName}, recordBuf + {iSpec.StartPos - 1}, {len});")
                        sb.AppendLine($"        tmp_{iSpec.FieldName}[{len}] = '\0';")
                        
                        If v.IsNumeric Then
                            sb.AppendLine($"        {iSpec.FieldName} = atof(tmp_{iSpec.FieldName});")
                        Else
                            sb.AppendLine($"        strcpy({iSpec.FieldName}, tmp_{iSpec.FieldName});")
                        End If
                    End If
                End If
            Next
        Else
            ' No input file, run once
            sb.AppendLine("    {")
            sb.AppendLine("        IN_LR = true;")
        End If

        sb.AppendLine()
        sb.AppendLine("        // --- Detail Calculations ---")
        For Each cSpec In _parser.CalcSpecs
            GenerateCalcOp(sb, cSpec, vars)
        Next

        sb.AppendLine()
        sb.AppendLine("        // --- Detail Output ---")
        Dim currentOutFile = ""
        For Each oSpec In _parser.OutputSpecs
            If oSpec.IsRecordLine Then
                currentOutFile = oSpec.Filename
            Else
                If Not String.IsNullOrEmpty(currentOutFile) Then
                    If Not String.IsNullOrEmpty(oSpec.FieldName) Then
                        ' Print Variable
                        If vars.ContainsKey(oSpec.FieldName) Then
                            Dim v = vars(oSpec.FieldName)
                            If v.IsNumeric Then
                                sb.AppendLine($"        fprintf({currentOutFile}, ""%-*g "", {v.Length}, {v.Name});")
                            Else
                                sb.AppendLine($"        fprintf({currentOutFile}, ""%-*s "", {v.Length}, {v.Name});")
                            End If
                        End If
                    ElseIf Not String.IsNullOrEmpty(oSpec.Constant) Then
                        ' Print Constant
                        Dim sanitized = oSpec.Constant.Replace("'", "")
                        sb.AppendLine($"        fprintf({currentOutFile}, ""%s "", ""{sanitized}"");")
                    End If
                End If
            End If
        Next
        
        If outFileData IsNot Nothing Then
            sb.AppendLine($"        fprintf({outFileData.Filename}, ""\n"");")
        End If

        sb.AppendLine("    }")
        sb.AppendLine()

        ' Cleanup
        For Each f In files
            sb.AppendLine($"    if ({f.Filename}) fclose({f.Filename});")
        Next

        sb.AppendLine("    return 0;")
        sb.AppendLine("}")

        Return sb.ToString()
    End Function

    Private Sub GenerateCalcOp(sb As StringBuilder, c As CalcSpec, vars As Dictionary(Of String, VariableDef))
        ' Handle TAG/Labels
        If c.Opcode = "TAG" Then
            sb.AppendLine($"{c.Factor1}:;")
            Return
        End If
        
        ' Handle basic condition indicators (simplified, checking positions 10-11 in cSpec)
        ' e.g., if columns 10-11 has an indicator like 01. We extract it from c.Indicators
        Dim indStr = c.Indicators.Trim()
        Dim condWrapper = False
        If indStr.Length >= 2 Then
            Dim indNum = indStr.Substring(indStr.Length - 2)
            If indNum <> "  " Then
                sb.AppendLine($"        if (IND[{indNum}]) {{")
                condWrapper = True
            End If
        End If
        
        ' Extract components
        Dim f1 = c.Factor1.Trim()
        Dim f2 = c.Factor2.Trim()
        Dim res = c.ResultField.Trim()
        
        Select Case c.Opcode
            Case "ADD"
                sb.AppendLine($"        {res} = {If(String.IsNullOrEmpty(f1), res, f1)} + {f2};")
            Case "SUB"
                sb.AppendLine($"        {res} = {If(String.IsNullOrEmpty(f1), res, f1)} - {f2};")
            Case "MULT"
                sb.AppendLine($"        {res} = {If(String.IsNullOrEmpty(f1), res, f1)} * {f2};")
            Case "DIV"
                sb.AppendLine($"        if ({f2} != 0) {res} = {If(String.IsNullOrEmpty(f1), res, f1)} / {f2};")
            Case "Z-ADD"
                sb.AppendLine($"        {res} = {f2};")
            Case "MOVE", "MOVEL"
                If vars.ContainsKey(res) AndAlso Not vars(res).IsNumeric Then
                    Dim src = If(vars.ContainsKey(f2) OrElse Not f2.StartsWith("'"), f2, f2.Replace("'", """"))
                    ' String assignment
                    If src.StartsWith("""") Then
                        If c.Opcode = "MOVEL" Then
                            sb.AppendLine($"        strncpy({res}, {src}, sizeof({res}) - 1);")
                        Else
                            sb.AppendLine($"        strcpy({res}, {src});")
                        End If
                    Else
                        If c.Opcode = "MOVEL" Then
                            sb.AppendLine($"        strncpy({res}, {f2}, sizeof({res}) - 1);")
                        Else
                            sb.AppendLine($"        strcpy({res}, {f2});")
                        End If
                    End If
                End If
            Case "COMP"
                ' Sets indicators HI, LO, EQ
                Dim hi = c.ResultingIndicatorHi.Trim()
                Dim lo = c.ResultingIndicatorLo.Trim()
                Dim eq = c.ResultingIndicatorEq.Trim()
                If hi <> "" Then sb.AppendLine($"        IND[{hi}] = ({f1} > {f2});")
                If lo <> "" Then sb.AppendLine($"        IND[{lo}] = ({f1} < {f2});")
                If eq <> "" Then sb.AppendLine($"        IND[{eq}] = ({f1} == {f2});")
            Case "SETON"
                Dim hi = c.ResultingIndicatorHi.Trim()
                Dim lo = c.ResultingIndicatorLo.Trim()
                Dim eq = c.ResultingIndicatorEq.Trim()
                If hi <> "" AndAlso hi <> "LR" Then sb.AppendLine($"        IND[{hi}] = true;")
                If lo <> "" AndAlso lo <> "LR" Then sb.AppendLine($"        IND[{lo}] = true;")
                If eq <> "" AndAlso eq <> "LR" Then sb.AppendLine($"        IND[{eq}] = true;")
                If hi = "LR" OrElse lo = "LR" OrElse eq = "LR" Then sb.AppendLine($"        IN_LR = true;")
            Case "SETOF"
                Dim hi = c.ResultingIndicatorHi.Trim()
                Dim lo = c.ResultingIndicatorLo.Trim()
                Dim eq = c.ResultingIndicatorEq.Trim()
                If hi <> "" AndAlso hi <> "LR" Then sb.AppendLine($"        IND[{hi}] = false;")
                If lo <> "" AndAlso lo <> "LR" Then sb.AppendLine($"        IND[{lo}] = false;")
                If eq <> "" AndAlso eq <> "LR" Then sb.AppendLine($"        IND[{eq}] = false;")
            Case "GOTO"
                sb.AppendLine($"        goto {f2};")
            Case "EXCPT"
                sb.AppendLine($"        // EXCPT (External Output) unsupported in simplified generator")
        End Select

        If condWrapper Then
            sb.AppendLine($"        }}")
        End If
    End Sub

    Private Function GetVariables() As Dictionary(Of String, VariableDef)
        Dim vars As New Dictionary(Of String, VariableDef)()

        ' From Input Specs
        For Each i In _parser.InputSpecs
            If Not i.IsRecordLine AndAlso Not String.IsNullOrWhiteSpace(i.FieldName) Then
                If Not vars.ContainsKey(i.FieldName) Then
                    vars(i.FieldName) = New VariableDef With {
                        .Name = i.FieldName,
                        .Length = i.EndPos - i.StartPos + 1,
                        .IsNumeric = i.IsNumeric
                    }
                End If
            End If
        Next

        ' From Calc Specs (Result fields often define new variables)
        For Each c In _parser.CalcSpecs
            If Not String.IsNullOrWhiteSpace(c.ResultField) AndAlso c.FieldLength > 0 Then
                If Not vars.ContainsKey(c.ResultField) Then
                    vars(c.ResultField) = New VariableDef With {
                        .Name = c.ResultField,
                        .Length = c.FieldLength,
                        .IsNumeric = Not String.IsNullOrWhiteSpace(c.DecimalPos)
                    }
                End If
            End If
        Next

        Return vars
    End Function
End Class

Public Class VariableDef
    Public Property Name As String
    Public Property Length As Integer
    Public Property IsNumeric As Boolean
End Class
