#Include Once "windows.bi"
#include Once "win/mmsystem.bi"
#Include Once "mynetwin.bi"
'		===========================================================================
'		===========================================================================
' Animatron.bas - a program to implement a scripting language to control an 
'						animatronic figure (i.e., Peter Penguin) via an SSC-32 
'						servo controller. The scripting language has been named a-code 
'						for animatronic code, and implements the functions defined herein.
'	v3.x	Basic functions (previous versions were development/beta releases)
'	v4.x	Added random move selection, as well as random pauses.
'	v5.x	Added the capability to be controlled from a network connection
'			plus features such as using a script as a macro in addition to a sub-process
'			(e.g. player). Started to implement a wait command so that network commands
'			could stop execution or override playing scripts.
'	v6.x	Restructured to include a next address in the script list, to simplify the
'			processing of calling/playing subscripts and to provide the structure for
'			a network command to override the execution of other scripts playing.
'  v6.2  Added "OneOnly" command; modified interpretation and processing of "Sync"
'			commands.
' v6.3	Added ServerCreate thread, to allow reconnection of network connection.
' v6.4	Corrected bug in Say function.
' v6.5	Added SAY network command
' v6.6	Added ActionLoop command, to sequentilly loop through a group of commands
' 			each time a script is called.
' v7.0	Added For..Loop like functionality called Repeat..EndRepeat. Also, implemented
'			a random repeat function that randomly calculates the loop end value, for verying
'			loops.
'
'		===========================================================================
'		===========================================================================

Declare Function FindIt (array() As String, What As String) As Integer
Declare Function DisplayStep (i As Integer, j As Integer) As String
Declare Function NextStep(myStep As Integer) As Integer
Declare Sub ServerThread(byval as any ptr)
Declare Sub ServerCreate(byval as any ptr)


ReDim MoveDescription(0) As String ' Move lookup table: description of move, e.g. "Move Eyes Left"
ReDim MoveCommand(0) As String ' actual servo move command, e.g. "#0P2000 #1P2000 T100"
Dim MoveIndex As Integer =0 ' array index of Move arrays

ReDim LabelDescription(0) As String	' Label lookup table; name of label or script
ReDim LabelScriptIndex(0) As Integer ' index to step after label in script arrays
Dim LabelIndex As Integer =0 ' array index for Label arrays

ReDim PlayerStep(0) As Integer		' Player properties: index to step in script to execute next
ReDim As Double PlayerEndWait(0)		' array of time before next command is issued, by player
PlayerEndWait(0)=0
Dim As Double SoundEndWait=0			' timer value before next check of ADC for voice/sound input, ~12ms from last test.
Dim Player As Integer =0

ReDim ScriptAction(0) As Integer	' Script properties: action to take
ReDim ScriptDescription(0) As Integer ' Index to servo move table
ReDim ScriptOption(0) As Integer ' value for action, i.e. pause length after command is issued
ReDim ScriptStack(0) As Integer ' stack for saved script addresses (could be used for other purposes)
Dim ScriptStep As Integer =0, MainScript As Integer =0, EndMain As Integer =0
Dim Shared AdjustStep As Integer =1
Dim ErrorFound As Integer =0, WaitName As Integer =0

Dim As Integer Selections ' variable to store the possible number of random moves from which to select
Dim As Integer minPause, maxPause, timePause
Randomize

ReDim SyncPoints(0) As ULongInt ' flags for sync points; bit encoded by associated players
Dim SyncPointStatus As ULongInt =0
Dim SyncPointIndex As Integer =0

Dim IndexPlayers As Integer = 0, MaxPlayers As Integer =0
Dim ScriptIndex As Integer  = 0,  NotDone    As Integer =-1 ' script player definitions

ReDim LoopStack(0) As Integer, LoopMaximum(0)As Integer
Dim As Integer LoopIndex=0, RepeatEndIndex=0, RepeatIndex=0
Const As Integer RepeatInitialIndex=0

'Speech variables
Dim As String dat, ssc32cmd, PW, lastPW="000"
Dim As Integer level
Dim soundFile As String

'Control variables
Dim As String ControlKey
Dim SingleStep As Integer = 0
Const PauseKey="\"
Const As String cr=Chr(13), lf =Chr(10),  esc=Chr(27)
Const As String crlf=cr+lf

'SSC-32 constants
Const scbase="#3P"
Const qs = "VA" + cr

' Network control variables
type CLIENT
  as FDSOCKET hClient
  as Any ptr  hThread
  as integer  blnExit
end type   
Dim As Integer i 
Dim As String netMessage
Dim As Any Pointer ServerProcess
' Shared variables for the server thread
dim shared as CLIENT  Clients
dim shared as Integer ClientsConnected
' Shared message definitions
Dim Shared As Integer MsgReady=0, serverReady=0
Dim Shared As ZString Ptr myMessage

' Prepare Show
Dim CommPort As String  ' serial port connected to servo controller (SSC-32)
Dim Animatron As String ' name of file containing mapping between move descriptions and SSC-32 commands
Dim ShowName As String  ' name of file containing a script for a given "show"
Const dfltCommPort = "com5:"
Const dfltMoves = "peter.csv"
Const dfltScript = "RepeatTest.csv"

' connection to servo controller (SSC-32)
Print "Comm port [";dfltCommPort;"]: "; 
Input CommPort
If CommPort = "" Then CommPort = dfltCommPort

' short description to servo controller (SSC-32) commands mappings
Print "Character/Move file name [";dfltMoves;"]:"; 
Input Animatron
If Animatron = "" Then Animatron = dfltMoves
Open Animatron For Input As #1

' script file
Print "Show file name [";dfltScript;"]: ";
Input ShowName
If ShowName = "" Then ShowName = dfltScript


'		===========================================================================
'		===========================================================================
' 		Define Moves (lookup table)
'		===========================================================================
'		===========================================================================

' read in short description and corresponding SSC-32 command
Dim Move As String , CtrlrCmd As String
Do While Not Eof(1)And ErrorFound =0
	Input #1, Move, CtrlrCmd
	If UCase(Move) = "END" Then Exit Do  
	MoveIndex += 1
	ReDim Preserve MoveDescription(MoveIndex)As String, MoveCommand(MoveIndex)As String
	MoveDescription(MoveIndex) = UCase(Move)
	MoveCommand(MoveIndex) = CtrlrCmd+cr		
Loop 
Close #1



'		===========================================================================
'		===========================================================================
' 		Define Script(s)
'		===========================================================================
'		===========================================================================

Open ShowName For Input As #1

Dim Action As String, Description As String, OptionValue As Integer, LocalIndex As Integer
Const PlayMove As Integer 	 =0 ' send commands to SSC-32
Const PlayScript As Integer =1 ' execute script in parallel; script must be defined in same file
Const StartScript As Integer=2 ' define new script; main script MUST be last in file
Const EndScript As Integer  =3 ' end of script routine definition
Const JumpTo As Integer 	 =4 ' "goto" command; label MUST exist (is not checked for)
Const Label As Integer 		 =5 ' definition of label used in "JumpTo" command
Const SyncPoint As Integer  =6 ' definition of scripts which will synchronize with other script(s)
Const EndSync As Integer 	 =7 ' definition of step in script at which to wait for synchronization
Const ScriptPause As Integer=8 ' "Pause" or "Delay" command; pauses execution for n milliseconds
Const Say As Integer=9 ' play external sound file; will cause servo defined in "scbase" (e.g. mouth servo) to synchronize to sound
Const RandomMove As Integer=10 ' Randomly perform on of the following 'n' actions; n is the option value
Const RandomPause As Integer=11 ' Pause some random time between the two times specified on the command
Const CallScript As Integer=12 ' Call a script rather than running it in parallel in its own player
Const EndWait As Integer=13  ' network command to clear pauses in execution at a "NetWait" point; not used in scripts
Const NetWait As Integer=14  ' Define a point in script at which a network command can cause a pause in execution
Const OneOnly As Integer=15  ' define a command that will exit a script if it is already running in a separate player.
Const ActionSeq As Integer=16 ' loop sequentially through command group in a script.
Const EndRepeat As Integer=17 ' endpoint of Repeat and EndRepeat commands (both not tokenized; only termination is)

Do While Not Eof(1) And ErrorFound =0
	Input #1, Action, Description, OptionValue
	Print Action, Description, OptionValue
	If Left(Action,1)=":" Then Action=":" ' special case of a commented line
	
	Select Case UCase(Action)
		
		Case ":"
			
		Case "ACTIONSEQ"
			ScriptStep +=1
			ReDim Preserve ScriptAction(ScriptStep) As Integer, ScriptDescription(ScriptStep) As Integer, ScriptOption(ScriptStep) As Integer, ScriptStack(ScriptStep) As Integer 
			AdjustStep=OptionValue
			ScriptAction(ScriptStep) = ActionSeq
			ScriptDescription(ScriptStep)=ScriptStep+1
			ScriptOption(ScriptStep) = ScriptStep+OptionValue
			ScriptStack(ScriptStep)=ScriptStep+1

		Case "CALLSCRIPT", "JUMPTO"
			ScriptStep+=1
			ReDim Preserve ScriptAction(ScriptStep) As Integer, ScriptDescription(ScriptStep) As Integer, ScriptOption(ScriptStep) As Integer, ScriptStack(ScriptStep) As Integer 
			If UCase(Action) = "CALLSCRIPT" Then 
				ScriptAction(ScriptStep)=CallScript
			Else 
				ScriptAction(ScriptStep)=JumpTo
			EndIf
			LocalIndex = FindIt(LabelDescription(), Description)
			If LocalIndex < 0 Then
				Print "Cannot find script or label ";Description
				ErrorFound=2
				Exit Do 
			Else 
				ScriptDescription(ScriptStep) = LabelScriptIndex(LocalIndex)
				ScriptOption(ScriptStep) = OptionValue
				ScriptStack(ScriptStep)=NextStep(ScriptStep)
			EndIf

		Case "ENDREPEAT"
			ScriptStep += 1
			ReDim Preserve ScriptAction(ScriptStep) As Integer, ScriptDescription(ScriptStep) As Integer, ScriptOption(ScriptStep) As Integer, ScriptStack(ScriptStep) As Integer 
			ScriptAction(ScriptStep)=EndRepeat
			ScriptDescription(ScriptStep)=RepeatInitialIndex
			ScriptOption(ScriptStep)=LoopMaximum(LoopIndex)
			ScriptStack(ScriptStep)=LoopStack(LoopIndex)
			LoopIndex-=1

		Case "ENDSCRIPT"
			ScriptStep +=1
			ReDim Preserve ScriptAction(ScriptStep) As Integer, ScriptDescription(ScriptStep) As Integer, ScriptOption(ScriptStep) As Integer, ScriptStack(ScriptStep) As Integer 
			ScriptAction(ScriptStep) = EndScript
			ScriptOption(ScriptStep) = 0
			ScriptStack(ScriptStep)=0
			EndMain=ScriptStep

'		Case "ENDSYNC" -- See "SYNCPOINT"

'		Case "JUMPTO" -- See "CALLSCRIPT"

'		Case "LABEL" -- See "STARTSCRIPT"

		Case "NETWAIT" 
			LocalIndex=FindIt(LabelDescription(),Description)
			If LocalIndex < 0 Then
				LabelIndex +=1
				LocalIndex=LabelIndex
				ReDim Preserve LabelDescription(LabelIndex) As String, LabelScriptIndex(LabelIndex)As Integer
 				LabelDescription(LocalIndex) = Description
				LabelScriptIndex(LocalIndex)=-ScriptStep
			EndIf
			ScriptStep+=1
			ReDim Preserve ScriptAction(ScriptStep) As Integer, ScriptDescription(ScriptStep) As Integer, ScriptOption(ScriptStep) As Integer, ScriptStack(ScriptStep) As Integer  
			ScriptAction(ScriptStep)=NetWait
			ScriptDescription(ScriptStep)=LabelScriptIndex(LocalIndex)
			ScriptOption(ScriptStep)=ScriptStep
			ScriptStack(ScriptStep)=NextStep(ScriptStep)

'		Case "ONEONLY" -- See "SYNCPOINT"

		Case "PLAYMOVE"
			ScriptStep +=1
			ReDim Preserve ScriptAction(ScriptStep) As Integer, ScriptDescription(ScriptStep) As Integer, ScriptOption(ScriptStep) As Integer, ScriptStack(ScriptStep) As Integer 
			ScriptAction(ScriptStep) = PlayMove
			LocalIndex = FindIt(MoveDescription(), Description)
			If LocalIndex < 0 Then
				Print "Cannot find move ";Description
				ErrorFound=1
				Exit Do 
			Else 
				ScriptDescription(ScriptStep) = LocalIndex
			EndIf
			ScriptOption(ScriptStep) = OptionValue
			ScriptStack(ScriptStep)=NextStep(ScriptStep)
		
		Case "PLAYSCRIPT"
			ScriptStep+=1
			ReDim Preserve ScriptAction(ScriptStep) As Integer, ScriptDescription(ScriptStep) As Integer, ScriptOption(ScriptStep) As Integer, ScriptStack(ScriptStep) As Integer 
			ScriptAction(ScriptStep)=PlayScript
			LocalIndex = FindIt(LabelDescription(), Description)
			If LocalIndex < 0 Then
				Print "Cannot find script ";Description
				ErrorFound=2
				Exit Do 
			Else 
				ScriptDescription(ScriptStep) = LabelScriptIndex(LocalIndex)
				ScriptOption(ScriptStep) = OptionValue
				ScriptStack(ScriptStep)=NextStep(ScriptStep)
			EndIf
		
'		Case "RANDOMMOVE" -- See "SCRIPTPAUSE"
			
		Case "RANDOMREPEAT"
			LoopIndex +=1
			ReDim Preserve LoopStack(LoopIndex)As Integer, LoopMaximum(LoopIndex) As Integer
			RepeatEndIndex=Int(Rnd*(Int(OptionValue)-Int(Val(Description))))+Int(Val(Description))
			LoopStack(LoopIndex)=NextStep(ScriptStep)
			LoopMaximum(LoopIndex)=RepeatEndIndex

		Case "RANDOMPAUSE"
			ScriptStep +=1
			ReDim Preserve ScriptAction(ScriptStep) As Integer, ScriptDescription(ScriptStep) As Integer, ScriptOption(ScriptStep) As Integer, ScriptStack(ScriptStep) As Integer 
			ScriptAction(ScriptStep) = RandomPause
			minPause=Val(Description)
			maxPause=OptionValue
			If minPause > maxPause Then Swap minPause, maxPause 
			ScriptDescription(ScriptStep)=minPause
			ScriptOption(ScriptStep)=maxPause
			ScriptStack(ScriptStep)=NextStep(ScriptStep)

		Case "REPEAT"
			LoopIndex +=1
			ReDim Preserve LoopStack(LoopIndex) As Integer, LoopMaximum(LoopIndex)As Integer
			RepeatEndIndex=Val(Description)
			LoopStack(LoopIndex)=NextStep(ScriptStep)
			LoopMaximum(LoopIndex)=Val(Description)

		Case "SAY"
			ScriptStep +=1
			ReDim Preserve ScriptAction(ScriptStep) As Integer, ScriptDescription(ScriptStep) As Integer, ScriptOption(ScriptStep) As Integer, ScriptStack(ScriptStep) As Integer 
			LocalIndex=FindIt(LabelDescription(),Description)
			If LocalIndex < 0 Then
				LabelIndex =UBound(LabelScriptIndex)+1
				LocalIndex=LabelIndex
				ReDim Preserve LabelDescription(LabelIndex) As String, LabelScriptIndex(LabelIndex)As Integer
				LabelDescription(LocalIndex) = Description
				LabelScriptIndex(LocalIndex)=0
			End If
			ScriptAction(ScriptStep)=Say
			ScriptDescription(ScriptStep)=LocalIndex
			ScriptOption(ScriptStep)=OptionValue
			ScriptStack(ScriptStep)=NextStep(ScriptStep)
			
		Case "SCRIPTPAUSE", "RANDOMMOVE"
			ScriptStep+=1
			ReDim Preserve ScriptAction(ScriptStep) As Integer, ScriptDescription(ScriptStep) As Integer, ScriptOption(ScriptStep) As Integer, ScriptStack(ScriptStep) As Integer  
			If  UCase(Action)="RANDOMMOVE" Then 
				ScriptAction(ScriptStep)=RandomMove
				AdjustStep=OptionValue+1
			Else
				ScriptAction(ScriptStep)=ScriptPause
				OptionValue=Val(Description)
			EndIf
			ScriptDescription(ScriptStep)=OptionValue
			ScriptOption(ScriptStep)=OptionValue
			ScriptStack(ScriptStep)=NextStep(ScriptStep)

		Case "STARTSCRIPT", "LABEL"
			LabelIndex +=1
			ReDim Preserve LabelDescription(LabelIndex) As String, LabelScriptIndex(LabelIndex)As Integer
			LabelDescription(LabelIndex) = Description
			LabelScriptIndex(LabelIndex)=ScriptStep+1
			If UCase(Action) = "STARTSCRIPT" Then MainScript=ScriptStep+1

		Case "SYNCPOINT", "ENDSYNC", "ONEONLY"
			ScriptStep+=1
			ReDim Preserve ScriptAction(ScriptStep) As Integer, ScriptDescription(ScriptStep) As Integer, ScriptOption(ScriptStep) As Integer, ScriptStack(ScriptStep) As Integer  
			If UCase(Action)="SYNCPOINT" Then
				ScriptAction(ScriptStep)=SyncPoint
			ElseIf UCase(Action)="ENDSYNC" Then
				ScriptAction(ScriptStep)=EndSync
			Else
				ScriptAction(ScriptStep)=OneOnly
			EndIf
			LocalIndex=FindIt(LabelDescription(),Description)
			If LocalIndex < 0 Then
				LabelIndex =UBound(LabelScriptIndex)+1
				LocalIndex=LabelIndex
				SyncPointIndex+=1
				ReDim Preserve LabelDescription(LabelIndex) As String, LabelScriptIndex(LabelIndex)As Integer
 				LabelDescription(LabelIndex) = Description
				LabelScriptIndex(LabelIndex)=SyncPointIndex
			EndIf
			ScriptDescription(ScriptStep)=LabelScriptIndex(LocalIndex)
			ScriptOption(ScriptStep)=0
			ScriptStack(ScriptStep)=NextStep(ScriptStep)
			
		Case Else
			Print "Unrecognized script command  ";chr(34);Action;chr(34)
			ErrorFound=4
			Sleep
			Exit Do 			

	End Select
Loop 

Close #1
If ErrorFound Then End ErrorFound 


'		===========================================================================
'		===========================================================================
' 		Execute processed Script(s)
'		===========================================================================
'		===========================================================================

	Open "ADebug.txt" For Output As #2
	For LocalIndex=0 To UBound(ScriptAction)
		Print #2, Using "###] #### #### #### ####";LocalIndex,ScriptAction(LocalIndex),ScriptDescription(LocalIndex),ScriptOption(LocalIndex),ScriptStack(LocalIndex)
	Next
	Print #2,""
	For LocalIndex=0 To UBound(LabelDescription)
		Print #2, Using "###] ####  ";LocalIndex,LabelScriptIndex(LocalIndex);
		Print #2, LabelDescription(LocalIndex)
	Next
	Print #2,""
	For i=0 To maxplayers
		Print #2, Using " ###";PlayerStep(i);
		Next 
	Print #2,""
	Close #2
	Sleep
	Print "Continuing..."
ElseIf ControlKey = cr Then
	SingleStep = Not SingleStep
ElseIf ControlKey = Esc Then
	NotDone=0
	Exit For 
EndIf 


' Speech processing ...
		If (SoundEndWait-Timer <0) Then
			dat = Input$(Loc(1), #1)

			If dat = "" Then
				' Print "No data"
			Else
				level = Asc(dat)
				' Print "Level ";level
				Select Case level
					Case 0 To 12
						PW="600"
					Case 13 To 25
						PW="700"
					Case 25 To 255
						PW="800"
				End Select
				If (PW<>lastPW)  Then
					ssc32cmd=scbase+PW+cr
					'Print "Command = ";ssc32cmd
					Put #1,, ssc32cmd 
				EndIf
				lastPW = PW
			End If 

			Put #1,,qs
			SoundEndWait=Timer+.012
		EndIf
		
		
		If ((PlayerEndWait(Player)-Timer)< 0) And (PlayerStep(Player)<>0) Then
			If SingleStep Then 
				Print "Single ";
				Sleep
			EndIf

			PlayerEndWait(Player)=0
			ScriptStep=PlayerStep(Player)
			Select Case ScriptAction(ScriptStep)

				Case ActionSeq
					LocalIndex=ScriptStack(ScriptStep)
					Print "Step ";DisplayStep(Player,ScriptStep);": Executing step ";LocalIndex;" of sequence"
					PlayerStep(Player)=ScriptStack(ScriptStep)
					LocalIndex+=1
					If LocalIndex > ScriptOption(ScriptStep) Then LocalIndex=ScriptDescription(ScriptStep)
					ScriptStack(ScriptStep)=LocalIndex

				Case CallScript
					' look for the end of the script
					LocalIndex=ScriptDescription(ScriptStep)
					While LocalIndex <= UBound(ScriptAction)
						If ScriptAction(LocalIndex) = EndScript Then Exit While 
						LocalIndex+=1
					Wend
					If LocalIndex > UBound(ScriptAction) Then
						Print "Cannot find end of script..."
						Exit Do
					EndIf
					' set the option value in the EndScript to point to the next step
					ScriptStack(LocalIndex)=ScriptStack(ScriptStep)
					PlayerStep(Player)=ScriptDescription(ScriptStep)
					PlayerEndWait(Player)=Timer+ScriptOption(ScriptStep)/1000
					Print "Step ";DisplayStep(Player,ScriptStep);":  Calling script"

				Case EndRepeat
					RepeatIndex = ScriptDescription(ScriptStep)
					RepeatIndex +=1
					If RepeatIndex >= ScriptOption(ScriptStep) Then
						ScriptDescription(ScriptStep)=RepeatInitialIndex
						PlayerStep(Player)+=1
					Else
						ScriptDescription(ScriptStep)=RepeatIndex
						PlayerStep(Player)=ScriptStack(ScriptStep)						
					EndIf
					Print "Step ";DisplayStep(Player,ScriptStep);":  End Repeat ";RepeatIndex; "of ";ScriptOption(ScriptStep)
	
				Case EndScript
					PlayerStep(Player)=ScriptStack(ScriptStep)
					ScriptStack(ScriptStep) = 0
					NotDone = (ScriptStep<>EndMain) ' main script has ended
					Print "Step ";DisplayStep(Player,ScriptStep);":  Script has completed on ";Player
					
				Case EndSync
					SyncPointIndex=ScriptDescription(ScriptStep)
					SyncPointStatus= SyncPoints(SyncPointIndex) And (Not 2^Player)
					If SyncPointStatus <> SyncPoints(SyncPointIndex) Then
						Print "Step ";DisplayStep(Player,ScriptStep);":  Sync'ing to";SyncPointIndex;"  [";SyncPoints(SyncPointIndex);"]"
					EndIf
					SyncPoints(SyncPointIndex)= SyncPointStatus
					If SyncPoints(SyncPointIndex) = 0 Then 
						PlayerStep(Player)=ScriptStack(ScriptStep)
					EndIf

				Case JumpTo
					Print "Step ";DisplayStep(Player,ScriptStep);":  Jump to";ScriptDescription(ScriptStep)
					PlayerStep(Player)=ScriptDescription(ScriptStep)
					
				Case NetWait
					PlayerStep(Player)=ScriptStack(ScriptStep)

				Case OneOnly
					Print "Step ";DisplayStep(Player,ScriptStep);":  Running single instance of script on ";Player
					' complete step processing at first execution, to set end of script index
					If ScriptOption(ScriptStep)=0 Then
						LocalIndex=ScriptStep+1
						Do
							If ScriptAction(LocalIndex)=EndSync Then Exit Do
							LocalIndex+=1
						Loop Until LocalIndex >UBound(ScriptAction)
						If LocalIndex > UBound(ScriptAction) Then
							Print "Cannot find end of script syncronization"
							Exit Do 
						Else
							ScriptOption(ScriptStep)=LocalIndex
						EndIf					
					EndIf
					' check to see if there are any other instances of this script running
					SyncPointIndex=ScriptDescription(ScriptStep)
					SyncPointStatus= SyncPoints(SyncPointIndex) And (Not 2^Player)
					If SyncPointStatus=0 Then
						PlayerStep(Player)=ScriptStack(ScriptStep) ' none, go to next step
					Else
						PlayerStep(Player)=ScriptOption(ScriptStep)' found instance, go to end of script
					EndIf

				Case PlayMove
					MoveIndex=ScriptDescription(ScriptStep)
					Print "Step ";DisplayStep(Player,ScriptStep);":  ";MoveDescription(MoveIndex),MoveCommand(MoveIndex)
					Put #1,,MoveCommand(MoveIndex)
					PlayerEndWait(Player)=Timer+ScriptOption(ScriptStep)/1000
					PlayerStep(Player)=ScriptStack(ScriptStep)

				Case PlayScript
					IndexPlayers=1
					Do While IndexPlayers <= MaxPlayers
						If PlayerStep(IndexPlayers)=0 Then Exit Do
						IndexPlayers+=1
					Loop
					If IndexPlayers>MaxPlayers Then 
						MaxPlayers+=1
						ReDim Preserve PlayerStep(MaxPlayers) As Integer, PlayerEndWait(MaxPlayers) As Double
						If Player > 63 Then
							Print "Too many simultaneous Players...";Player
							Exit Do 
						EndIf
					EndIf
					PlayerStep(IndexPlayers)=ScriptDescription(ScriptStep)
					PlayerEndWait(IndexPlayers)=0
					PlayerStep(Player)=ScriptStack(ScriptStep)
					PlayerEndWait(Player)=Timer+ScriptOption(ScriptStep)/1000
					Print "Step ";DisplayStep(Player,ScriptStep);":  Starting script on Player ";IndexPlayers

				Case RandomMove
					Selections=ScriptDescription(ScriptStep)
					MoveIndex=Int(Rnd()*Selections)+1 ' calculate offset to selected command in script
					PlayerStep(Player)=ScriptStep+MoveIndex
					Print "Step ";DisplayStep(Player,ScriptStep);":  RandomMove ";MoveIndex;" of";Selections
				
				Case RandomPause
					Print "Step ";DisplayStep(Player,ScriptStep);":  ";
					minPause=ScriptDescription(ScriptStep)
					maxPause=ScriptOption(ScriptStep)
					timePause=Int(Rnd()*(maxPause-minPause)+minPause)/1000
					Print Using "Pause ##.###";timePause
					PlayerEndWait(Player)=Timer+timePause
					PlayerStep(Player)=ScriptStack(ScriptStep)

				Case Say
					soundFile=LabelDescription(ScriptDescription(ScriptStep))
					Print "Step ";DisplayStep(Player,ScriptStep);":  Saying ";soundFile
					PlaySound(soundFile, NULL, SND_ASYNC Or SND_FILENAME)
					PlayerStep(Player)=ScriptStack(ScriptStep)

				Case ScriptPause
					Print "Step ";DisplayStep(Player,ScriptStep);":  ";
					Print Using "Pause ##.###";(ScriptOption(ScriptStep)/1000)
					PlayerEndWait(Player)=Timer+ScriptOption(ScriptStep)/1000
					PlayerStep(Player)=ScriptStack(ScriptStep)

				Case SyncPoint
					SyncPointIndex=ScriptDescription(ScriptStep)
					Print "Step ";DisplayStep(Player,ScriptStep);":  Defining synchronization point ";SyncPointIndex
					SyncPoints(SyncPointIndex)=SyncPoints(SyncPointIndex) Or (2^Player)
					PlayerStep(Player)=ScriptStack(ScriptStep)

			End Select
		EndIf
		
	Next
	Sleep 1
Loop

timeEndPeriod( 1 )
Print "Press any key to finish";
Print
Sleep

End

Function FindIt (AnArray() As String, What As String) As Integer
	Dim i As Integer, myWhat As String
	myWhat=UCase(what)
	For i=0 To UBound(AnArray)
		If UCase(AnArray(i)) = myWhat Then Exit For 
	Next
	If i > ubound(AnArray) Then i=-16384
	Return i
End Function

Function DisplayStep(i As Integer, j As Integer) As String
	Dim Display_Number As String =""
	Display_Number = Right("  " & Str(i),2) & "-"+Left(Str(j) & "   ",3)
	Return Display_Number
End Function
Function NextStep(myStep As Integer) As Integer
	Dim where As Integer
	where = myStep + AdjustStep
	If AdjustStep > 1 Then AdjustStep-=1
	Return where
End Function
Sub ServerThread(byval lpany as any ptr)
  Dim as integer localreturn,index=cast(integer,lpAny)
  Dim As String msg,RecvdMessage
'  thread started for client
  Clients.blnExit=0
  while Clients.blnExit=0
    localreturn=NetReceiveString(Clients.hClient,msg)
    if localreturn<1 then
      Clients.blnExit=1
    Else
'     thread: got a message " & msg
      if instr(msg,esc) Then
        msg="exit"&crlf
        localreturn=NetSendString(Clients.hClient,msg)
        Clients.blnExit=1
      ElseIf InStr(msg,cr) Then
'      thread: send ok
        msg="ok"&crlf
        localreturn=NetSendString(Clients.hClient,msg)
        myMessage=StrPtr(RecvdMessage)
        MsgReady=-1
        While MsgReady
        	Sleep 1
        Wend
        RecvdMessage=""
      Else 
			RecvdMessage=RecvdMessage+msg
      End If 
    End If  
  wend
' thread end for client
  NetClose Clients.hClient
  Clients.hClient=0
end sub
Sub ServerCreate(ByVal mpany As Any Ptr)

' Client data type
' Server definitions
Dim as FDSOCKET     hServer
dim as SOCKADDR_IN  Server,Client
dim as integer      ret,addrsize

Do
	hServer=NetSocket(AF_INET,SOCK_STREAM,0)
	
	If hServer<0 Then  
		Print "Network server: error create socket!"
		Beep :Sleep :end 1
	End If 
	Server.sin_family=AF_INET
	Server.sin_port  =build_port(2310)
	Server.sin_addr  =INADDR_ANY
	ret=NetBind(hServer,@Server,sizeof(SOCKADDR_IN))
	If ret<0 then 
		Print "Network server: error binding to port!"
		beep:sleep:end 1
	End If
	ret=NetListen(hServer,3)
	If ret<0 then 
		Print "Network server: error listening!"
		beep:sleep:end 1
	End if
	Print "Network server: wait on connection"
	addrsize=sizeof(SOCKADDR_IN)
	ret=NetAccept(hServer,@Client,@addrsize)
	If ret>-1 then
'	  	Print "Network server: accept"
	   Clients.hClient=ret
	   Clients.hThread=ThreadCreate(@ServerThread)
		Print "Network server: connection"
		serverReady=-1
	Else
	    Print "Network server: error accept!"
	    serverReady=0
	End If
	
	If Clients.hThread<>0 then
'		? "server: wait on thread" 
		ThreadWait(Clients.hThread)
	End if
	Sleep 100
	Print "Network server: close!"
	NetClose hServer

Loop

End Sub 
