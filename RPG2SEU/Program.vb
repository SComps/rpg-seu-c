Imports System.Threading
Imports TN3270Framework
Imports System.IO

Module Program
    Private _listener As TN3270Listener
    Private _currentFilePath As String = ""

    Sub Main(args As String())
        If args.Length > 0 Then
            _currentFilePath = args(0)
            Console.WriteLine($"Target File: {_currentFilePath}")
        Else
            Console.WriteLine("Warning: No file specified. Will open in-memory buffer.")
        End If
    
        Console.WriteLine("Starting RPG II SEU Editor Server on port 2323...")
        
        _listener = New TN3270Listener(2323)
        AddHandler _listener.ConnectionReceived, AddressOf OnClientConnected
        _listener.Start()

        Console.WriteLine("Server running. Connect via 3270 Emulator to localhost:2323.")
        Console.WriteLine("Press 'Q' to quit.")

        While True
            If Console.KeyAvailable Then
                Dim keyInfo = Console.ReadKey(True)
                If keyInfo.KeyChar = "q"c Or keyInfo.KeyChar = "Q"c Then
                    Exit While
                End If
            End If
            Thread.Sleep(100)
        End While

        Console.WriteLine("Shutting down...")
        _listener.StopListening()
    End Sub

    Private Sub OnClientConnected(sender As Object, e As TN3270ConnectionEventArgs)
        Console.WriteLine($"Client connected from {e.RemoteEndPoint}")
        
        Dim session = e.Session
        AddHandler session.NegotiationComplete, Sub(s, eventArgs)
                                                    Dim doc = New RpgDocument()
                                                    If Not String.IsNullOrEmpty(_currentFilePath) Then
                                                        doc.Load(_currentFilePath)
                                                    End If
                                                    
                                                    Dim seu = New SeuSession(session, doc)
                                                    seu.Start()
                                                End Sub
        session.StartNegotiation()
    End Sub
End Module
