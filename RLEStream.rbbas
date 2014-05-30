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
		  Dim out As New MemoryBlock(InData.Size)
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
		  Dim ret As String
		  
		  If Count > 1 Then
		    #pragma BackgroundTasks Off
		    For i As Integer = 1 To Count
		      ret = ret + Me.Read(1, encoding)
		    Next
		    Return ret
		  End If
		  
		  If encoding <> Nil Then ret = DefineEncoding(ret, encoding)
		  Dim rcount As String
		  While Not Me.EOF
		    If Runcount >= Count Then
		      For i As Integer = 1 To Count
		        ret = ret + RunChar
		        Runcount = Runcount - 1
		      Next
		      If Runcount = 0 Then RunChar = ""
		    End If
		    If ret.Len >= Count Then Return ret
		    Dim m As String = IOStream.Read(1)
		    Select Case m
		    Case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
		      rcount = rcount + m
		    Else
		      If m = "\" Then 'escape
		        If IOStream.Position < IOStream.Length Then
		          Dim n As String = IOStream.Read(1)
		          m = n
		        End If
		      End If
		      If rcount.Trim = "" Then rcount = "1"
		      Runcount = Val(rcount)
		      RunChar = m
		      rcount = ""
		    End Select
		  Wend
		  
		  Return DefineEncoding(ret, encoding)
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
		  
		  If text.LenB > 1 Then
		    #pragma BackgroundTasks Off
		    For i As Integer = 1 To text.LenB
		      Me.Write(MidB(text, i, 1))
		    Next
		    Return
		  End If
		  
		  Dim data As MemoryBlock = text
		  Dim sz As Integer = Data.Size - 1
		  If RunChar = "" Then RunChar = Data.StringValue(0, 1)
		  'If IsNumeric(RunChar) Then RunChar = "\" + RunChar
		  For i As Integer = 0 To sz
		    Dim char As String = Data.StringValue(i, 1)
		    If StrComp(char, RunChar, 1) <> 0 Then
		      If Runcount > 1 Then
		        If IsNumeric(RunChar) Then
		          IOStream.Write(Str(Runcount) + "\" + RunChar)
		        Else
		          IOStream.Write(Str(Runcount) + RunChar)
		        End If
		      Else
		        If IsNumeric(RunChar) Then
		          IOStream.Write("\" + RunChar)
		        Else
		          IOStream.Write(RunChar)
		        End If
		      End If
		      RunChar = char
		      Runcount = 1
		    Else
		      Runcount = Runcount + 1
		    End If
		  Next
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
