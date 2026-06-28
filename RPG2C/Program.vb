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
            ' Start timing
            Dim startTime = DateTime.Now
            
            Console.WriteLine()
            Console.WriteLine("RPG2C TRANSPILER - Version 1.0")
            Console.WriteLine("Using RPG Runtime Library v1.0")
            Console.WriteLine(New String("=", 80))
            Console.WriteLine($"Source File: {inputFile}")
            Console.WriteLine($"Output File: {outputFile}")
            Console.WriteLine($"Start Time:  {startTime:yyyy-MM-dd HH:mm:ss}")
            Console.WriteLine(New String("=", 80))
            Console.WriteLine()
            
            Console.WriteLine("Phase 1: Parsing RPG source...")
            Dim parseStart = DateTime.Now
            Dim parser As New RpgParser()
            parser.ParseFile(inputFile)
            Dim parseEnd = DateTime.Now
            Dim parseTime = (parseEnd - parseStart).TotalMilliseconds
            
            ' Count source lines
            Dim sourceLines = File.ReadAllLines(inputFile)
            Dim totalLines = sourceLines.Length
            Dim codeLines = sourceLines.Where(Function(l) Not String.IsNullOrWhiteSpace(l) AndAlso Not l.TrimStart().StartsWith("*")).Count()
            Dim commentLines = sourceLines.Where(Function(l) l.TrimStart().StartsWith("*")).Count()
            Dim blankLines = totalLines - codeLines - commentLines
            
            ' Count specifications
            Dim fSpecs = parser.FileSpecs.Count
            Dim iSpecs = parser.InputSpecs.Count
            Dim cSpecs = parser.CalcSpecs.Count
            Dim oSpecs = parser.OutputSpecs.Count
            
            Console.WriteLine($"  Lines parsed: {totalLines} ({codeLines} code, {commentLines} comments, {blankLines} blank)")
            Console.WriteLine($"  Specifications: {fSpecs} File, {iSpecs} Input, {cSpecs} Calc, {oSpecs} Output")
            Console.WriteLine($"  Parse time: {parseTime:F2} ms")
            Console.WriteLine()

            ' Generate Listing File
            Console.WriteLine("Phase 2: Generating listing file...")
            Dim listingFile = Path.ChangeExtension(inputFile, ".lst")
            Using sw As New StreamWriter(listingFile)
                sw.WriteLine("RPG II TRANSPILER LISTING")
                sw.WriteLine($"RPG2C Version 1.0 - {DateTime.Now:yyyy-MM-dd HH:mm:ss}")
                sw.WriteLine($"Runtime Library: v1.0")
                sw.WriteLine(New String("=", 80))
                sw.WriteLine($"SOURCE FILE: {inputFile}")
                sw.WriteLine($"OUTPUT FILE: {outputFile}")
                sw.WriteLine(New String("=", 80))
                sw.WriteLine()
                sw.WriteLine("SOURCE LISTING:")
                sw.WriteLine(New String("-", 80))
                
                For i As Integer = 0 To sourceLines.Length - 1
                    sw.WriteLine($"{(i+1).ToString("D5")}  {sourceLines(i)}")
                    ' Check if there was an error on this line
                    Dim lineNum = i + 1
                    Dim lineErrors = parser.Errors.Where(Function(e) e.LineNumber = lineNum)
                    For Each rpgErr In lineErrors
                        sw.WriteLine($"       **ERROR** {rpgErr.Description}")
                    Next
                Next
                
                sw.WriteLine(New String("-", 80))
                sw.WriteLine()
                sw.WriteLine("COMPILATION STATISTICS:")
                sw.WriteLine(New String("-", 80))
                sw.WriteLine($"Total Lines:          {totalLines,8}")
                sw.WriteLine($"  Code Lines:         {codeLines,8}")
                sw.WriteLine($"  Comment Lines:      {commentLines,8}")
                sw.WriteLine($"  Blank Lines:        {blankLines,8}")
                sw.WriteLine()
                sw.WriteLine($"Specifications:")
                sw.WriteLine($"  File Specs (F):     {fSpecs,8}")
                sw.WriteLine($"  Input Specs (I):    {iSpecs,8}")
                sw.WriteLine($"  Calc Specs (C):     {cSpecs,8}")
                sw.WriteLine($"  Output Specs (O):   {oSpecs,8}")
                sw.WriteLine()
                sw.WriteLine($"Parse Time:           {parseTime,8:F2} ms")
                sw.WriteLine()
                
                If parser.Errors.Count = 0 Then
                    sw.WriteLine(New String("=", 80))
                    sw.WriteLine("COMPILATION SUCCESSFUL - NO ERRORS")
                    sw.WriteLine(New String("=", 80))
                Else
                    sw.WriteLine(New String("=", 80))
                    sw.WriteLine($"COMPILATION FAILED - {parser.Errors.Count} ERROR(S) FOUND")
                    sw.WriteLine(New String("=", 80))
                    sw.WriteLine()
                    sw.WriteLine("ERROR SUMMARY:")
                    For Each rpgErr In parser.Errors
                        sw.WriteLine($"  Line {rpgErr.LineNumber,5}: {rpgErr.Description}")
                    Next
                End If
            End Using
            Console.WriteLine($"  Listing file: {listingFile}")
            Console.WriteLine()

            If parser.Errors.Count > 0 Then
                Console.WriteLine(New String("=", 80))
                Console.WriteLine($"TRANSPILATION FAILED - {parser.Errors.Count} ERROR(S)")
                Console.WriteLine("Check the listing file for details.")
                Console.WriteLine(New String("=", 80))
                Return
            End If

            Console.WriteLine("Phase 3: Generating C code...")
            Dim genStart = DateTime.Now
            Dim generator As New CGenerator(parser)
            Dim cCode = generator.Generate()
            Dim genEnd = DateTime.Now
            Dim genTime = (genEnd - genStart).TotalMilliseconds
            
            ' Count generated C lines
            Dim cLines = cCode.Split(vbLf).Length
            
            Console.WriteLine($"  Generated {cLines} lines of C code")
            Console.WriteLine($"  Generation time: {genTime:F2} ms")
            Console.WriteLine()

            Console.WriteLine("Phase 4: Writing output file...")
            File.WriteAllText(outputFile, cCode)
            Console.WriteLine($"  Output file: {outputFile}")
            Console.WriteLine()

            Console.WriteLine("Phase 5: Copying runtime library...")
            Dim outputDir = Path.GetDirectoryName(Path.GetFullPath(outputFile))
            Dim exeDir = AppDomain.CurrentDomain.BaseDirectory
            
            ' Try to find runtime files:
            ' 1. Same directory as executable (published/installed version)
            ' 2. Project structure (development version)
            Dim runtimeDir As String = exeDir
            Dim runtimeFiles = {"rpg_runtime.h", "rpg_file.h", "rpg_data.h", "rpg_runtime.c", "rpg_file.c", "rpg_data.c"}
            
            ' Check if runtime files exist in exe directory
            If Not File.Exists(Path.Combine(exeDir, "rpg_runtime.h")) Then
                ' Development mode: navigate to Runtime directory
                Dim projectRoot = Path.GetDirectoryName(Path.GetDirectoryName(Path.GetDirectoryName(Path.GetDirectoryName(exeDir))))
                runtimeDir = Path.Combine(projectRoot, "Runtime")
            End If
            
            ' Copy runtime files to output directory (skip if source = destination)
            Dim copiedCount = 0
            For Each runtimeFile In runtimeFiles
                Dim srcPath = Path.Combine(runtimeDir, runtimeFile)
                Dim destPath = Path.Combine(outputDir, runtimeFile)
                
                If File.Exists(srcPath) AndAlso Path.GetFullPath(srcPath) <> Path.GetFullPath(destPath) Then
                    File.Copy(srcPath, destPath, True)
                    copiedCount += 1
                End If
            Next
            
            If copiedCount > 0 Then
                Console.WriteLine($"  Copied {copiedCount} runtime files")
            Else
                Console.WriteLine($"  Runtime files already in output directory")
            End If
            
            ' Create build script
            Dim buildScriptPath = Path.Combine(outputDir, "build.sh")
            Dim baseName = Path.GetFileNameWithoutExtension(outputFile)
            Using sw As New StreamWriter(buildScriptPath)
                sw.WriteLine("#!/bin/bash")
                sw.WriteLine("# Auto-generated build script for " & baseName)
                sw.WriteLine()
                sw.WriteLine("set -e  # Exit on error")
                sw.WriteLine()
                sw.WriteLine("echo ""Building " & baseName & "...""")
                sw.WriteLine("gcc -o " & baseName & " " & Path.GetFileName(outputFile) & " rpg_runtime.c rpg_file.c rpg_data.c -lm -lsqlite3 -O2")
                sw.WriteLine("echo ""Build successful: " & baseName & """")
                sw.WriteLine("echo ""Usage: ./" & baseName & " <input_files>""")
            End Using
            
            ' Make build script executable on Unix
            If Environment.OSVersion.Platform = PlatformID.Unix OrElse Environment.OSVersion.Platform = PlatformID.MacOSX Then
                Try
                    Dim proc = New Process()
                    proc.StartInfo.FileName = "chmod"
                    proc.StartInfo.Arguments = "+x " & buildScriptPath
                    proc.StartInfo.UseShellExecute = False
                    proc.Start()
                    proc.WaitForExit()
                Catch
                    ' Ignore chmod errors
                End Try
            End If
            
            Console.WriteLine($"  Created build script: {buildScriptPath}")
            Console.WriteLine()

            ' Final statistics
            Dim endTime = DateTime.Now
            Dim totalTime = (endTime - startTime).TotalMilliseconds
            
            Console.WriteLine(New String("=", 80))
            Console.WriteLine("TRANSPILATION SUCCESSFUL")
            Console.WriteLine(New String("=", 80))
            Console.WriteLine($"Total Time:           {totalTime,8:F2} ms")
            Console.WriteLine($"  Parse Time:         {parseTime,8:F2} ms ({parseTime/totalTime*100:F1}%)")
            Console.WriteLine($"  Generation Time:    {genTime,8:F2} ms ({genTime/totalTime*100:F1}%)")
            Dim linesPerSec = If(totalTime > 0, (totalLines / totalTime * 1000), 0)
            Console.WriteLine($"Lines/Second:         {linesPerSec,8:F0}")
            Console.WriteLine()
            Console.WriteLine($"Input:  {totalLines,5} lines RPG II")
            Console.WriteLine($"Output: {cLines,5} lines C")
            Dim ratio = If(totalLines > 0, (cLines / totalLines), 0)
            Console.WriteLine($"Ratio:  {ratio,5:F2}:1")
            Console.WriteLine(New String("=", 80))

        Catch ex As Exception
            Console.WriteLine($"Transpilation failed: {ex.Message}")
        End Try
    End Sub
End Module
