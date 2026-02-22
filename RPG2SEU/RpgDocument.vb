Imports System.IO
Imports System.Text

Public Class RpgDocument
    ' Each line is strictly 80 columns wide.
    Public Lines As New List(Of String)
    Public FilePath As String

    Public Sub New()
        ' Initialize with one empty Form I (Input) or similar default?
        ' Maybe just an empty document
    End Sub

    Public Sub Load(path As String)
        FilePath = path
        Lines.Clear()
        If File.Exists(path) Then
            Dim rawLines = File.ReadAllLines(path)
            For Each line In rawLines
                Lines.Add(PadOrTruncate(line, 80))
            Next
        End If
        If Lines.Count = 0 Then
            Lines.Add(New String(" "c, 80))
        End If
    End Sub

    Public Sub Save()
        If Not String.IsNullOrEmpty(FilePath) Then
            Dim outLines As New List(Of String)
            For Each line In Lines
                outLines.Add(PadOrTruncate(line, 80))
            Next
            File.WriteAllLines(FilePath, outLines)
        End If
    End Sub

    Public Sub SaveAs(path As String)
        FilePath = path
        Save()
    End Sub

    Private Function PadOrTruncate(s As String, length As Integer) As String
        If s Is Nothing Then s = ""
        If s.Length > length Then
            Return s.Substring(0, length)
        Else
            Return s.PadRight(length, " "c)
        End If
    End Function
End Class
