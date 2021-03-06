#tag Class
Protected Class RLEStream
Implements Readable,Writeable
	#tag Method, Flags = &h0
		Sub Close()
		  Try
		    Me.Flush
		  Catch IOException
		    ' read-only
		  End Try
		  IOStream.Close
		  IOStream = Nil
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(Stream As BinaryStream)
		  IOStream = Stream
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(mb As MemoryBlock)
		  IOStream = New BinaryStream(mb)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		 Shared Function Create(f As FolderItem, OverWrite As Boolean = False) As RLEStream
		  Dim bs As BinaryStream = BinaryStream.Create(f, OverWrite)
		  Return New RLEStream(bs)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		 Shared Function Decode(InData As MemoryBlock) As MemoryBlock
		  Dim instream As New RLEStream(InData)
		  Dim out As String
		  While Not instream.EOF
		    out = out + instream.Read(64)
		  Wend
		  instream.Close
		  Return out
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Destructor()
		  If IOStream <> Nil Then IOStream.Close
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		 Shared Function Encode(InData As MemoryBlock) As MemoryBlock
		  Dim out As New MemoryBlock(0)
		  Dim outstream As New RLEStream(out)
		  outstream.Write(InData)
		  outstream.Close
		  Return out
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function EOF() As Boolean
		  // Part of the Readable interface.
		  Return IOStream.EOF And Runcount < 1
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Flush()
		  // Part of the Writeable interface.
		  If NeedsFlush Then
		    Dim c As String = RunChar
		    If IsNumeric(c) Then
		      c = "\" + c
		    End If
		    If Runcount > 1 Then
		      IOStream.Write(Str(Runcount) + c)
		    ElseIf RunChar <> "" Then
		      IOStream.Write(c)
		    End If
		  End If
		  IOStream.Flush
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Length() As Integer
		  Return IOStream.Length
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		 Shared Function Open(f As FolderItem, ReadWrite As Boolean = False) As RLEStream
		  Dim bs As BinaryStream = BinaryStream.Open(f, ReadWrite)
		  Return New RLEStream(bs)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Position() As Integer
		  Return IOStream.Position
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Read(Count As Integer, encoding As TextEncoding = Nil) As String
		  // Part of the Readable interface.
		  If RawIO Then Return IOStream.Read(Count, encoding)
		  Dim ret As New MemoryBlock(0)
		  Dim retstream As New BinaryStream(ret)
		  
		  If Count > 1 Then
		    #pragma BackgroundTasks Off
		    For i As Integer = 1 To Count
		      retstream.Write(Me.Read(1, encoding))
		    Next
		    retstream.Close
		    Return ret
		  End If
		  
		  #pragma BackgroundTasks Off
		  #pragma NilObjectChecking Off
		  #pragma BoundsChecking Off
		  
		  Dim rcount As String
		  While Not Me.EOF
		    If Runcount >= Count Then
		      For i As Integer = 1 To Count
		        retstream.Write(RunChar)
		        Runcount = Runcount - 1
		      Next
		      If Runcount = 0 Then RunChar = ""
		    End If
		    If retstream.Length >= Count Then Exit While
		    Dim m As String = IOStream.Read(1)
		    Select Case m
		    Case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
		      rcount = rcount + m
		    Else
		      If m = "\" Then 'escape
		        If IOStream.EOF Then 
		          Dim err As New UnsupportedFormatException
		          err.Message = "Invalid RLE data"
		          Raise err
		        End If
		        m = IOStream.Read(1)
		      End If
		      If rcount.Trim = "" Then rcount = "1"
		      Runcount = Val(rcount)
		      RunChar = DefineEncoding(m, encoding)
		      rcount = ""
		    End Select
		  Wend
		  retstream.Close
		  Return ret
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ReadError() As Boolean
		  // Part of the Readable interface.
		  Return IOStream.ReadError
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Write(text As String)
		  // Part of the Writeable interface.
		  If RawIO Then
		    IOStream.Write(text)
		    Return
		  End If
		  
		  If text.Len > 1 Then
		    Dim textstream As New BinaryStream(text)
		    While Not textstream.EOF
		      Me.Write(textstream.Read(1))
		    Wend
		    textstream.Close
		    Return
		  End If
		  #pragma BackgroundTasks Off
		  #pragma NilObjectChecking Off
		  #pragma BoundsChecking Off
		  
		  If RunChar = "" Then RunChar = text
		  If StrComp(text, RunChar, 1) = 0 Then
		    Runcount = Runcount + 1
		  Else
		    If Runcount > 1 Then
		      If IsNumeric(RunChar) Or RunChar = "\" Then
		        IOStream.Write(Str(Runcount) + "\" + RunChar)
		      Else
		        IOStream.Write(Str(Runcount) + RunChar)
		      End If
		    Else
		      If IsNumeric(RunChar) Or RunChar = "\" Then
		        IOStream.Write("\" + RunChar)
		      Else
		        IOStream.Write(RunChar)
		      End If
		    End If
		    RunChar = text
		    Runcount = 1
		  End If
		  NeedsFlush = True
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function WriteError() As Boolean
		  // Part of the Writeable interface.
		  Return IOStream.WriteError
		End Function
	#tag EndMethod


	#tag Property, Flags = &h1
		Protected IOStream As BinaryStream
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected NeedsFlush As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		RawIO As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected RunChar As String
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected Runcount As Integer = 0
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="RawIO"
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			InheritedFrom="Object"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
