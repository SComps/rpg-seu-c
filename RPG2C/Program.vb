Imports System.IO
Imports System.Linq

Module Program
    Sub Main(args As String())
        If args.Length < 1 Then
            Console.WriteLine("Usage: RPG2C <input.rpg> [output.c]")
            Console.WriteLine("If output.c is not provided, writes to the same directory as <input.rpg> with .c extension")
            Return
        End If

        Dim inputFile = args(0)
        If Not File.Exists(inputFile) Then
            Console.WriteLine($"Error: Input file '{inputFile}' not found.")
            Return
        End If

        Dim outputFile As String
        If args.Length > 1 Then
            outputFile = args(1)
        Else
            outputFile = Path.ChangeExtension(inputFile, ".c")
        End If

        Try
            Console.WriteLine($"Parsing RPG file: {inputFile}...")
            Dim parser As New RpgParser()
            parser.ParseFile(inputFile)

            ' Generate Listing File
            Dim listingFile = Path.ChangeExtension(inputFile, ".lst")
            Using sw As New StreamWriter(listingFile)
                sw.WriteLine($"RPG II COMPILER LISTING - {DateTime.Now}")
                sw.WriteLine($"SOURCE FILE: {inputFile}")
                sw.WriteLine(New String("-"c, 80))
                
                Dim sourceLines = File.ReadAllLines(inputFile)
                For i As Integer = 0 To sourceLines.Length - 1
                    sw.WriteLine($"{ (i+1).ToString("D4") }  {sourceLines(i)}")
                    ' Check if there was an error on this line
                    Dim lineNum = i + 1
                    Dim lineErrors = parser.Errors.Where(Function(e) e.LineNumber = lineNum)
                    For Each rpgErr In lineErrors
                        sw.WriteLine(rpgErr.ToString())
                    Next
                Next
                
                If parser.Errors.Count = 0 Then
                    sw.WriteLine(New String("-"c, 80))
                    sw.WriteLine("NO ERRORS FOUND. TRANSPILATION SUCCESSFUL.")
                Else
                    sw.WriteLine(New String("-"c, 80))
                    sw.WriteLine($"{parser.Errors.Count} ERROR(S) FOUND.")
                End If
            End Using
            Console.WriteLine($"Listing file generated: {listingFile}")

            If parser.Errors.Count > 0 Then
                Console.WriteLine("Transpilation stopped due to errors. Check the listing file for details.")
                Return
            End If

            Console.WriteLine($"Generating C code...")
            Dim generator As New CGenerator(parser)
            Dim cCode = generator.Generate()

            Console.WriteLine($"Writing to C file: {outputFile}...")
            File.WriteAllText(outputFile, cCode)

            Console.WriteLine("Transpilation successful!")

        Catch ex As Exception
            Console.WriteLine($"Transpilation failed: {ex.Message}")
        End Try
    End Sub
End Module
