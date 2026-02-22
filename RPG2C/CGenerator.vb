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
        sb.AppendLine("#include <ctype.h>")
        sb.AppendLine()

        sb.AppendLine("// --- RPG Runtime Support ---")
        sb.AppendLine("double rpg_decode_packed(const unsigned char* buf, int start, int len, int decimals) {")
        sb.AppendLine("    double val = 0;")
        sb.AppendLine("    int bytes = len;")
        sb.AppendLine("    for (int i = 0; i < bytes; i++) {")
        sb.AppendLine("        unsigned char b = buf[start + i];")
        sb.AppendLine("        if (i < bytes - 1) {")
        sb.AppendLine("            val = val * 100 + ((b >> 4) * 10) + (b & 0x0F);")
        sb.AppendLine("        } else {")
        sb.AppendLine("            val = val * 10 + (b >> 4);")
        sb.AppendLine("            if ((b & 0x0F) == 0x0D || (b & 0x0F) == 0x0B) val = -val;")
        sb.AppendLine("        }")
        sb.AppendLine("    }")
        sb.AppendLine("    for (int i = 0; i < decimals; i++) val /= 10.0;")
        sb.AppendLine("    return val;")
        sb.AppendLine("}")
        sb.AppendLine()
        sb.AppendLine("void rpg_format_edit(char* dest, double val, char code, int len, int dec) {")
        sb.AppendLine("    char fmt[20], tmp[64];")
        sb.AppendLine("    if (code == 'Z') { // Zero Suppress")
        sb.AppendLine("        if (val == 0) { memset(dest, ' ', len); dest[len] = 0; return; }")
        sb.AppendLine("        sprintf(fmt, ""%%.%df"", dec);")
        sb.AppendLine("        sprintf(tmp, fmt, val);")
        sb.AppendLine("        int start = 0; while(tmp[start] == '0' || tmp[start] == ' ') start++;")
        sb.AppendLine("        sprintf(dest, ""%-*s"", len, tmp + start);")
        sb.AppendLine("    } else {")
        sb.AppendLine("        sprintf(fmt, ""%%.*f"", dec);")
        sb.AppendLine("        sprintf(dest, fmt, val);")
        sb.AppendLine("    }")
        sb.AppendLine("}")
        sb.AppendLine()

        ' State variables
        sb.AppendLine("// --- Indicators ---")
        sb.AppendLine("bool IND[100] = {false};")
        sb.AppendLine("bool IN_LR = false;")
        sb.AppendLine("bool IN_MR = false;")
        ' Level Indicators
        sb.AppendLine("bool IN_L[10] = {false};")
        sb.AppendLine("#define IN_L1 IN_L[1] ")
        sb.AppendLine("#define IN_L2 IN_L[2] ")
        sb.AppendLine("#define IN_L3 IN_L[3] ")
        sb.AppendLine("#define IN_L4 IN_L[4] ")
        sb.AppendLine("#define IN_L5 IN_L[5] ")
        sb.AppendLine("#define IN_L6 IN_L[6] ")
        sb.AppendLine("#define IN_L7 IN_L[7] ")
        sb.AppendLine("#define IN_L8 IN_L[8] ")
        sb.AppendLine("#define IN_L9 IN_L[9] ")
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
        
        ' Previous Value variables for Level Breaks
        sb.AppendLine("// --- Control Level Buffers ---")
        For Each iSpec In _parser.InputSpecs.Where(Function(is_l) Not is_l.IsRecordLine AndAlso Not String.IsNullOrEmpty(is_l.ControlLevel))
            Dim v = vars(iSpec.FieldName)
            If v.IsNumeric Then
                sb.AppendLine($"double PREV_{iSpec.FieldName} = 0.0;")
            Else
                sb.AppendLine($"char PREV_{iSpec.FieldName}[{v.Length + 1}] = {{0}};")
            End If
        Next
        sb.AppendLine()

        ' Generate Subroutines BEFORE main
        sb.AppendLine("// --- Subroutines ---")
        Dim inSR = False
        For Each c In _parser.CalcSpecs
            If c.Opcode = "BEGSR" Then
                inSR = True
                sb.AppendLine($"void {c.Factor1.Trim()}() {{")
            ElseIf c.Opcode = "ENDSR" Then
                sb.AppendLine("}")
                sb.AppendLine()
                inSR = False
            ElseIf inSR Then
                GenerateCalcOp(sb, c, vars, "    ")
            End If
        Next

        ' Function Signature
        sb.AppendLine("int main(int argc, char** argv) {")
        
        ' File Setup
        Dim files = _parser.FileSpecs.GroupBy(Function(f_l) f_l.Filename).Select(Function(g_l) g_l.First()).ToList()
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
        sb.AppendLine("    bool firstRecord = true;")
        sb.AppendLine()

        ' RPG Logic Cycle Loop
        sb.AppendLine("    // --- Main Logic Cycle ---")
        If inFileData IsNot Nothing Then
            Dim recLen = If(inFileData.RecordLength > 0, inFileData.RecordLength, 80)
            sb.AppendLine($"    while (fread(recordBuf, 1, {recLen}, {inFileData.Filename}) > 0) {{")
            
            ' 1. Check for Level Breaks
            sb.AppendLine("        // Check for Level Breaks")
            For Each iSpec In _parser.InputSpecs.Where(Function(ispec_l) Not ispec_l.IsRecordLine AndAlso Not String.IsNullOrEmpty(ispec_l.ControlLevel))
                Dim v = vars(iSpec.FieldName)
                Dim len = iSpec.EndPos - iSpec.StartPos + 1
                sb.AppendLine($"        {{")
                sb.AppendLine($"            char cur_val[{len + 1}];")
                sb.AppendLine($"            strncpy(cur_val, recordBuf + {iSpec.StartPos - 1}, {len}); cur_val[{len}] = 0;")
                If v.IsNumeric Then
                    If iSpec.DataType = "P" Then
                        sb.AppendLine($"            double d_cur = rpg_decode_packed((const unsigned char*)recordBuf, {iSpec.StartPos - 1}, {len}, {If(String.IsNullOrEmpty(iSpec.DecimalPos), 0, iSpec.DecimalPos)});")
                    Else
                        sb.AppendLine($"            double d_cur = atof(cur_val);")
                    End If
                    sb.AppendLine($"            if (!firstRecord && d_cur != PREV_{iSpec.FieldName}) IN_L[{(If(iSpec.ControlLevel.StartsWith("L"), iSpec.ControlLevel.Substring(1), "0"))}] = true;")
                Else
                    sb.AppendLine($"            if (!firstRecord && strcmp(cur_val, PREV_{iSpec.FieldName}) != 0) IN_L[{(If(iSpec.ControlLevel.StartsWith("L"), iSpec.ControlLevel.Substring(1), "0"))}] = true;")
                End If
            Next

            ' 2. Total Calcs
            sb.AppendLine()
            sb.AppendLine("        if (!firstRecord && (")
            Dim totalConds = New List(Of String)
            For i As Integer = 1 To 9
                totalConds.Add($"IN_L{i}")
            Next
            totalConds.Add("IN_LR")
            sb.AppendLine("            " & String.Join(" || ", totalConds))
            sb.AppendLine("        )) {")
            sb.AppendLine("            // --- Total Calculations ---")
            For Each c In _parser.CalcSpecs.Where(Function(cs_l) cs_l.ControlLevel.StartsWith("L") OrElse cs_l.ControlLevel = "LR")
                GenerateCalcOp(sb, c, vars, "            ")
            Next
            sb.AppendLine()
            sb.AppendLine("            // --- Total Output ---")
            GenerateOutput(sb, vars, "T")
            sb.AppendLine("        }")

            ' Reset Level Indicators
            sb.AppendLine("        for(int i=1; i<10; i++) IN_L[i] = false;")
            sb.AppendLine()

            ' 3. Extract Input Fields and Update PREV buffers
            sb.AppendLine("        // Extract Input Fields")
            For Each iSpec In _parser.InputSpecs.Where(Function(ispec_l) Not ispec_l.IsRecordLine)
                Dim v = vars(iSpec.FieldName)
                Dim len = iSpec.EndPos - iSpec.StartPos + 1
                sb.AppendLine($"        {{")
                sb.AppendLine($"            char tmp[{len+1}]; strncpy(tmp, recordBuf + {iSpec.StartPos - 1}, {len}); tmp[{len}] = 0;")
                If v.IsNumeric Then
                    If iSpec.DataType = "P" Then
                        sb.AppendLine($"            {iSpec.FieldName} = rpg_decode_packed((const unsigned char*)recordBuf, {iSpec.StartPos - 1}, {len}, {If(String.IsNullOrEmpty(iSpec.DecimalPos), 0, iSpec.DecimalPos)});")
                    Else
                        sb.AppendLine($"            {iSpec.FieldName} = atof(tmp);")
                    End If
                    If Not String.IsNullOrEmpty(iSpec.ControlLevel) Then sb.AppendLine($"            PREV_{iSpec.FieldName} = {iSpec.FieldName};")
                Else
                    sb.AppendLine($"            strcpy({iSpec.FieldName}, tmp);")
                    If Not String.IsNullOrEmpty(iSpec.ControlLevel) Then sb.AppendLine($"            strcpy(PREV_{iSpec.FieldName}, {iSpec.FieldName});")
                End If
                sb.AppendLine($"        }}")
            Next

            sb.AppendLine()
            sb.AppendLine("        // --- Detail Calculations ---")
            For Each cSpec In _parser.CalcSpecs.Where(Function(cs_l) String.IsNullOrWhiteSpace(cs_l.ControlLevel) AndAlso Not IsInSubroutine(cs_l))
                GenerateCalcOp(sb, cSpec, vars, "        ")
            Next

            sb.AppendLine()
            sb.AppendLine("        // --- Detail Output ---")
            GenerateOutput(sb, vars, "D")
            
            sb.AppendLine("        firstRecord = false;")
            sb.AppendLine("        if (IN_LR) break;")
            sb.AppendLine("    }")
        Else
            ' No input file, run once
            sb.AppendLine("    {")
            sb.AppendLine("        IN_LR = true;")
            sb.AppendLine("        // Detail Calculations")
            For Each cSpec In _parser.CalcSpecs.Where(Function(cs_l) String.IsNullOrWhiteSpace(cs_l.ControlLevel) AndAlso Not IsInSubroutine(cs_l))
                GenerateCalcOp(sb, cSpec, vars, "        ")
            Next
            GenerateOutput(sb, vars, "D", "        ")
            sb.AppendLine("    }")
        End If

        sb.AppendLine()
        sb.AppendLine("    // --- Final LR Cycle ---")
        sb.AppendLine("    IN_LR = true;")
        sb.AppendLine("    // Total Calculations")
        For Each c In _parser.CalcSpecs.Where(Function(cs_l) cs_l.ControlLevel = "LR" OrElse (cs_l.ControlLevel.StartsWith("L") AndAlso cs_l.ControlLevel <> "L0"))
            GenerateCalcOp(sb, c, vars, "    ")
        Next
        sb.AppendLine("    // Total Output")
        GenerateOutput(sb, vars, "T")

        ' Cleanup
        For Each f In files
            sb.AppendLine($"    if ({f.Filename}) fclose({f.Filename});")
        Next

        sb.AppendLine("    return 0;")
        sb.AppendLine("}")

        Return sb.ToString()
    End Function

    Private Sub GenerateOutput(sb As StringBuilder, vars As Dictionary(Of String, VariableDef), type As String, Optional indent As String = "            ")
        Dim currentOutFile = ""
        Dim currentType = ""
        For Each oSpec In _parser.OutputSpecs
            If oSpec.IsRecordLine Then
                currentOutFile = oSpec.Filename
                currentType = oSpec.Type
            Else
                If Not String.IsNullOrEmpty(currentOutFile) AndAlso (currentType = type OrElse type = "*ALL") Then
                    ' Simple conditioning (simplified)
                    If Not String.IsNullOrEmpty(oSpec.OutputIndicator1) Then
                        sb.AppendLine($"{indent}if (IND[{oSpec.OutputIndicator1}]) {{")
                        indent &= "    "
                    End If

                    If Not String.IsNullOrEmpty(oSpec.FieldName) Then
                        ' Print Variable
                        If vars.ContainsKey(oSpec.FieldName) Then
                            Dim v = vars(oSpec.FieldName)
                            If v.IsNumeric Then
                                If Not String.IsNullOrEmpty(oSpec.EditCode) Then
                                    sb.AppendLine($"{indent}{{")
                                    sb.AppendLine($"{indent}    char edit_buf[64];")
                                    sb.AppendLine($"{indent}    rpg_format_edit(edit_buf, {v.Name}, '{oSpec.EditCode}', {v.Length}, {v.DecimalPos});")
                                    sb.AppendLine($"{indent}    fprintf({currentOutFile}, ""%-*s "", {v.Length}, edit_buf);")
                                    sb.AppendLine($"{indent}}}")
                                Else
                                    sb.AppendLine($"{indent}fprintf({currentOutFile}, ""%-*.*f "", {v.Length}, {v.DecimalPos}, {v.Name});")
                                End If
                            Else
                                sb.AppendLine($"{indent}fprintf({currentOutFile}, ""%-*s "", {v.Length}, {v.Name});")
                            End If
                        End If
                    ElseIf Not String.IsNullOrEmpty(oSpec.Constant) Then
                        ' Print Constant
                        Dim sanitized = oSpec.Constant.Replace("'", "")
                        sb.AppendLine($"{indent}fprintf({currentOutFile}, ""%s "", ""{sanitized}"");")
                    End If

                    If Not String.IsNullOrEmpty(oSpec.OutputIndicator1) Then
                        indent = indent.Substring(0, indent.Length - 4)
                        sb.AppendLine($"{indent}}}")
                    End If
                End If
            End If
        Next
    End Sub

    Private Sub GenerateCalcOp(sb As StringBuilder, c As CalcSpec, vars As Dictionary(Of String, VariableDef), Optional indent As String = "        ")
        ' Handle TAG/Labels
        If c.Opcode = "TAG" Then
            sb.AppendLine($"{c.Factor1}:;")
            Return
        End If

        ' Handle conditioning indicators (Up to 3)
        Dim conditions As New List(Of String)
        If Not String.IsNullOrWhiteSpace(c.Indicator1) Then
            conditions.Add(If(c.Not1, "!", "") & $"IND[{c.Indicator1}]")
        End If
        If Not String.IsNullOrWhiteSpace(c.Indicator2) Then
            conditions.Add(If(c.Not2, "!", "") & $"IND[{c.Indicator2}]")
        End If
        If Not String.IsNullOrWhiteSpace(c.Indicator3) Then
            conditions.Add(If(c.Not3, "!", "") & $"IND[{c.Indicator3}]")
        End If

        Dim condWrapper = False
        If conditions.Count > 0 Then
            sb.AppendLine($"{indent}if ({String.Join(" && ", conditions)}) {{")
            indent &= "    "
            condWrapper = True
        End If

        ' Extract components
        Dim f1 = c.Factor1.Trim()
        Dim f2 = c.Factor2.Trim()
        Dim res = c.ResultField.Trim()

        Select Case c.Opcode
            Case "ADD"
                sb.AppendLine($"{indent}{res} = {If(String.IsNullOrEmpty(f1), res, f1)} + {f2};")
            Case "SUB"
                sb.AppendLine($"{indent}{res} = {If(String.IsNullOrEmpty(f1), res, f1)} - {f2};")
            Case "MULT"
                sb.AppendLine($"{indent}{res} = {If(String.IsNullOrEmpty(f1), res, f1)} * {f2};")
            Case "DIV"
                sb.AppendLine($"{indent}if ({f2} != 0) {res} = {If(String.IsNullOrEmpty(f1), res, f1)} / {f2};")
            Case "Z-ADD"
                sb.AppendLine($"{indent}{res} = {f2};")
            Case "MOVE", "MOVEL"
                If vars.ContainsKey(res) AndAlso Not vars(res).IsNumeric Then
                    Dim src = If(vars.ContainsKey(f2) OrElse Not f2.StartsWith("'"), f2, f2.Replace("'", """"))
                    ' String assignment
                    If src.StartsWith("""") Then
                        If c.Opcode = "MOVEL" Then
                            sb.AppendLine($"{indent}strncpy({res}, {src}, sizeof({res}) - 1);")
                        Else
                            sb.AppendLine($"{indent}strcpy({res}, {src});")
                        End If
                    Else
                        If c.Opcode = "MOVEL" Then
                            sb.AppendLine($"{indent}strncpy({res}, {f2}, sizeof({res}) - 1);")
                        Else
                            sb.AppendLine($"{indent}strcpy({res}, {f2});")
                        End If
                    End If
                End If
            Case "COMP"
                ' Sets indicators HI, LO, EQ
                Dim hi = c.ResultingIndicatorHi.Trim()
                Dim lo = c.ResultingIndicatorLo.Trim()
                Dim eq = c.ResultingIndicatorEq.Trim()
                If hi <> "" Then sb.AppendLine($"{indent}IND[{hi}] = ({f1} > {f2});")
                If lo <> "" Then sb.AppendLine($"{indent}IND[{lo}] = ({f1} < {f2});")
                If eq <> "" Then sb.AppendLine($"{indent}IND[{eq}] = ({f1} == {f2});")
            Case "SETON"
                Dim hi = c.ResultingIndicatorHi.Trim()
                Dim lo = c.ResultingIndicatorLo.Trim()
                Dim eq = c.ResultingIndicatorEq.Trim()
                If hi <> "" AndAlso hi <> "LR" Then sb.AppendLine($"{indent}IND[{hi}] = true;")
                If lo <> "" AndAlso lo <> "LR" Then sb.AppendLine($"{indent}IND[{lo}] = true;")
                If eq <> "" AndAlso eq <> "LR" Then sb.AppendLine($"{indent}IND[{eq}] = true;")
                If hi = "LR" OrElse lo = "LR" OrElse eq = "LR" Then sb.AppendLine($"{indent}IN_LR = true;")
            Case "SETOF"
                Dim hi = c.ResultingIndicatorHi.Trim()
                Dim lo = c.ResultingIndicatorLo.Trim()
                Dim eq = c.ResultingIndicatorEq.Trim()
                If hi <> "" AndAlso hi <> "LR" Then sb.AppendLine($"{indent}IND[{hi}] = false;")
                If lo <> "" AndAlso lo <> "LR" Then sb.AppendLine($"{indent}IND[{lo}] = false;")
                If eq <> "" AndAlso eq <> "LR" Then sb.AppendLine($"{indent}IND[{eq}] = false;")
            Case "GOTO"
                sb.AppendLine($"{indent}goto {f2};")
            Case "EXSR"
                sb.AppendLine($"{indent}{f2}();")
            Case "EXCPT"
                sb.AppendLine($"{indent}// EXCPT (External Output) unsupported in simplified generator")
        End Select

        If condWrapper Then
            indent = indent.Substring(0, indent.Length - 4)
            sb.AppendLine($"{indent}}}")
        End If
    End Sub

    Private Function IsInSubroutine(c As CalcSpec) As Boolean
        Dim inSR = False
        For Each cs In _parser.CalcSpecs
            If cs Is c Then Return inSR
            If cs.Opcode = "BEGSR" Then inSR = True
            If cs.Opcode = "ENDSR" Then inSR = False
        Next
        Return False
    End Function

    Private Function GetVariables() As Dictionary(Of String, VariableDef)
        Dim vars As New Dictionary(Of String, VariableDef)()

        ' From Input Specs
        For Each i In _parser.InputSpecs
            If Not i.IsRecordLine AndAlso Not String.IsNullOrWhiteSpace(i.FieldName) Then
                If Not vars.ContainsKey(i.FieldName) Then
                    vars(i.FieldName) = New VariableDef With {
                        .Name = i.FieldName,
                        .Length = i.EndPos - i.StartPos + 1,
                        .IsNumeric = i.IsNumeric,
                        .DecimalPos = If(String.IsNullOrEmpty(i.DecimalPos), 0, Integer.Parse(i.DecimalPos))
                    }
                End If
            End If
        Next

        ' From Calc Specs (Result fields often define new variables)
        Dim mathOps = {"ADD", "SUB", "MULT", "DIV", "Z-ADD"}
        For Each c In _parser.CalcSpecs
            If Not String.IsNullOrWhiteSpace(c.ResultField) Then
                Dim isNumeric = Not String.IsNullOrWhiteSpace(c.DecimalPos) OrElse mathOps.Contains(c.Opcode)
                If Not vars.ContainsKey(c.ResultField) Then
                    vars(c.ResultField) = New VariableDef With {
                        .Name = c.ResultField,
                        .Length = If(c.FieldLength > 0, c.FieldLength, 15),
                        .IsNumeric = isNumeric,
                        .DecimalPos = If(String.IsNullOrEmpty(c.DecimalPos), 0, Integer.Parse(c.DecimalPos))
                    }
                ElseIf isNumeric Then
                    ' Force existing variable to numeric if used in math
                    vars(c.ResultField).IsNumeric = True
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
    Public Property DecimalPos As Integer
End Class
