Public Class FormSpecDefinition
    Public Property Description As String
    Public Property Fields As New List(Of FormFieldDefinition)
End Class

Public Class FormFieldDefinition
    Public Property Name As String
    Public Property StartColumn As Integer ' 1-based index (e.g. col 7)
    Public Property Length As Integer
    Public Property IsNumeric As Boolean = False
    
    Public Sub New(name As String, startCol As Integer, len As Integer, Optional isNum As Boolean = False)
        Me.Name = name
        Me.StartColumn = startCol
        Me.Length = len
        Me.IsNumeric = isNum
    End Sub
End Class

Public Class FormatDefinitions
    Public Shared ReadOnly Formspecs As New Dictionary(Of Char, FormSpecDefinition)
    
    Shared Sub New()
        ' Initialize Common
        Dim hSpec As New FormSpecDefinition() With {.Description = "H - Control Specification"}
        ' Common col 1-5 is Page/Line, Col 6 is Form Type
        hSpec.Fields.Add(New FormFieldDefinition("Page/Line", 1, 5))
        hSpec.Fields.Add(New FormFieldDefinition("Form Type", 6, 1))
        hSpec.Fields.Add(New FormFieldDefinition("Core Size to Compile", 7, 1))
        hSpec.Fields.Add(New FormFieldDefinition("Core Size to Execute", 8, 1))
        hSpec.Fields.Add(New FormFieldDefinition("Debug/Object", 9, 1))
        hSpec.Fields.Add(New FormFieldDefinition("Sterling", 10, 1))
        hSpec.Fields.Add(New FormFieldDefinition("Date Format", 11, 1))
        hSpec.Fields.Add(New FormFieldDefinition("Inverted Print", 12, 1))
        hSpec.Fields.Add(New FormFieldDefinition("Alt Collating Seq", 13, 1))
        hSpec.Fields.Add(New FormFieldDefinition("Unused 14-74", 14, 61))
        hSpec.Fields.Add(New FormFieldDefinition("Program Name", 75, 6))
        Formspecs.Add("H"c, hSpec)
        
        Dim fSpec As New FormSpecDefinition() With {.Description = "F - File Description"}
        fSpec.Fields.Add(New FormFieldDefinition("Page/Line", 1, 5))
        fSpec.Fields.Add(New FormFieldDefinition("Form Type", 6, 1))
        fSpec.Fields.Add(New FormFieldDefinition("Filename", 7, 8))
        fSpec.Fields.Add(New FormFieldDefinition("File Type", 15, 1))
        fSpec.Fields.Add(New FormFieldDefinition("File Desig", 16, 1))
        fSpec.Fields.Add(New FormFieldDefinition("End of File", 17, 1))
        fSpec.Fields.Add(New FormFieldDefinition("Sequence", 18, 1))
        fSpec.Fields.Add(New FormFieldDefinition("File Format", 19, 1))
        fSpec.Fields.Add(New FormFieldDefinition("Block Length", 20, 4, True))
        fSpec.Fields.Add(New FormFieldDefinition("Record Length", 24, 4, True))
        fSpec.Fields.Add(New FormFieldDefinition("Mode of Processing", 28, 1))
        fSpec.Fields.Add(New FormFieldDefinition("Length of Key", 29, 2, True))
        fSpec.Fields.Add(New FormFieldDefinition("Record Address Type", 31, 1))
        fSpec.Fields.Add(New FormFieldDefinition("Type of Org", 32, 1))
        fSpec.Fields.Add(New FormFieldDefinition("Overflow Ind", 33, 2))
        fSpec.Fields.Add(New FormFieldDefinition("Key Start Pos", 35, 4, True))
        fSpec.Fields.Add(New FormFieldDefinition("Extension Code", 39, 1))
        fSpec.Fields.Add(New FormFieldDefinition("Device", 40, 7))
        fSpec.Fields.Add(New FormFieldDefinition("Symbolic Device", 47, 6))
        fSpec.Fields.Add(New FormFieldDefinition("Labels", 53, 1))
        fSpec.Fields.Add(New FormFieldDefinition("Name of Label Exit", 54, 6))
        fSpec.Fields.Add(New FormFieldDefinition("Core Index", 60, 2))
        fSpec.Fields.Add(New FormFieldDefinition("Unused 62-65", 62, 4))
        fSpec.Fields.Add(New FormFieldDefinition("File Addition", 66, 1))
        fSpec.Fields.Add(New FormFieldDefinition("Unused 67-70", 67, 4))
        fSpec.Fields.Add(New FormFieldDefinition("Num of Extents", 71, 2, True))
        fSpec.Fields.Add(New FormFieldDefinition("Tape Rewind", 73, 1))
        fSpec.Fields.Add(New FormFieldDefinition("File Cond", 74, 1))
        fSpec.Fields.Add(New FormFieldDefinition("Comments", 75, 6))
        Formspecs.Add("F"c, fSpec)
        
        Dim iSpec As New FormSpecDefinition() With {.Description = "I - Input Specification"}
        iSpec.Fields.Add(New FormFieldDefinition("Page/Line", 1, 5))
        iSpec.Fields.Add(New FormFieldDefinition("Form Type", 6, 1))
        iSpec.Fields.Add(New FormFieldDefinition("Filename(Rec)", 7, 8))
        iSpec.Fields.Add(New FormFieldDefinition("Sequence", 15, 2))
        iSpec.Fields.Add(New FormFieldDefinition("Number", 17, 1))
        iSpec.Fields.Add(New FormFieldDefinition("Option", 18, 1))
        iSpec.Fields.Add(New FormFieldDefinition("Rec ID Ind", 19, 2))
        iSpec.Fields.Add(New FormFieldDefinition("Pos 1", 21, 4))
        iSpec.Fields.Add(New FormFieldDefinition("Not/Char 1", 25, 3))
        iSpec.Fields.Add(New FormFieldDefinition("Pos 2", 28, 4))
        iSpec.Fields.Add(New FormFieldDefinition("Not/Char 2", 32, 3))
        iSpec.Fields.Add(New FormFieldDefinition("Pos 3", 35, 4))
        iSpec.Fields.Add(New FormFieldDefinition("Not/Char 3", 39, 3))
        iSpec.Fields.Add(New FormFieldDefinition("Stacker", 42, 1))
        iSpec.Fields.Add(New FormFieldDefinition("Packed/Bin", 43, 1))
        iSpec.Fields.Add(New FormFieldDefinition("Field Location", 44, 8)) ' Starts 44, ends 51 based on my old notes: Field Location 44-51
        iSpec.Fields.Add(New FormFieldDefinition("Decimal Pos", 52, 1))
        iSpec.Fields.Add(New FormFieldDefinition("Field Name", 53, 6))
        iSpec.Fields.Add(New FormFieldDefinition("Ctrl Level", 59, 2))
        iSpec.Fields.Add(New FormFieldDefinition("Matching Fields", 61, 2))
        iSpec.Fields.Add(New FormFieldDefinition("Field Record Rel Ind", 63, 2))
        iSpec.Fields.Add(New FormFieldDefinition("Blank After Ind", 65, 2)) ' Often plus/minus/blank indicators 65-70
        iSpec.Fields.Add(New FormFieldDefinition("Zero Ind", 67, 2))
        iSpec.Fields.Add(New FormFieldDefinition("Unused 69-74", 69, 6))
        iSpec.Fields.Add(New FormFieldDefinition("Comments", 75, 6))
        Formspecs.Add("I"c, iSpec)
        
        Dim cSpec As New FormSpecDefinition() With {.Description = "C - Calculation Specification"}
        cSpec.Fields.Add(New FormFieldDefinition("Page/Line", 1, 5))
        cSpec.Fields.Add(New FormFieldDefinition("Form Type", 6, 1))
        cSpec.Fields.Add(New FormFieldDefinition("Control Level", 7, 2))
        cSpec.Fields.Add(New FormFieldDefinition("Indicators 1", 9, 3))
        cSpec.Fields.Add(New FormFieldDefinition("Indicators 2", 12, 3))
        cSpec.Fields.Add(New FormFieldDefinition("Indicators 3", 15, 3))
        cSpec.Fields.Add(New FormFieldDefinition("Factor 1", 18, 10))
        cSpec.Fields.Add(New FormFieldDefinition("Operation", 28, 5))
        cSpec.Fields.Add(New FormFieldDefinition("Factor 2", 33, 10))
        cSpec.Fields.Add(New FormFieldDefinition("Result Field", 43, 6))
        cSpec.Fields.Add(New FormFieldDefinition("Field Length", 49, 3, True))
        cSpec.Fields.Add(New FormFieldDefinition("Decimal Pos", 52, 1))
        cSpec.Fields.Add(New FormFieldDefinition("Half Adjust", 53, 1))
        cSpec.Fields.Add(New FormFieldDefinition("Resulting Ind +/1", 54, 2))
        cSpec.Fields.Add(New FormFieldDefinition("Resulting Ind -/2", 56, 2))
        cSpec.Fields.Add(New FormFieldDefinition("Resulting Ind Z/3", 58, 2))
        cSpec.Fields.Add(New FormFieldDefinition("Comments", 60, 15))
        cSpec.Fields.Add(New FormFieldDefinition("Prog ID", 75, 6))
        Formspecs.Add("C"c, cSpec)
        
        Dim oSpec As New FormSpecDefinition() With {.Description = "O - Output Specification"}
        oSpec.Fields.Add(New FormFieldDefinition("Page/Line", 1, 5))
        oSpec.Fields.Add(New FormFieldDefinition("Form Type", 6, 1))
        oSpec.Fields.Add(New FormFieldDefinition("Filename", 7, 8))
        oSpec.Fields.Add(New FormFieldDefinition("Type", 15, 1))
        oSpec.Fields.Add(New FormFieldDefinition("Space/Skip Bef", 16, 2))
        oSpec.Fields.Add(New FormFieldDefinition("Space/Skip Aft", 18, 2))
        oSpec.Fields.Add(New FormFieldDefinition("Output Ind 1", 20, 3))
        oSpec.Fields.Add(New FormFieldDefinition("Output Ind 2", 23, 3))
        oSpec.Fields.Add(New FormFieldDefinition("Output Ind 3", 26, 3))
        oSpec.Fields.Add(New FormFieldDefinition("EXCPT Name", 29, 3)) ' wait actually I need to re-check
        oSpec.Fields.Add(New FormFieldDefinition("Field Name", 32, 6))
        oSpec.Fields.Add(New FormFieldDefinition("Edit Codes/Zer", 38, 1))
        oSpec.Fields.Add(New FormFieldDefinition("Blank After", 39, 1))
        oSpec.Fields.Add(New FormFieldDefinition("End Pos", 40, 4, True))
        oSpec.Fields.Add(New FormFieldDefinition("Data Format", 44, 1))
        oSpec.Fields.Add(New FormFieldDefinition("Constant/Edit Word", 45, 26))
        oSpec.Fields.Add(New FormFieldDefinition("Prog ID", 71, 10))
        Formspecs.Add("O"c, oSpec)
        
        Dim blankSpec As New FormSpecDefinition() With {.Description = "Unknown Form Type"}
        blankSpec.Fields.Add(New FormFieldDefinition("Page/Line", 1, 5))
        blankSpec.Fields.Add(New FormFieldDefinition("Form Type", 6, 1))
        blankSpec.Fields.Add(New FormFieldDefinition("Source Data", 7, 74))
        Formspecs.Add(" "c, blankSpec)
    End Sub
    
    Public Shared Function GetFormatDefForLine(line As String) As FormSpecDefinition
        If String.IsNullOrEmpty(line) OrElse line.Length < 6 Then Return Formspecs(" "c)
        Dim type = Char.ToUpper(line(5))
        If Formspecs.ContainsKey(type) Then
             Return Formspecs(type)
        End If
        Return Formspecs(" "c)
    End Function
End Class
