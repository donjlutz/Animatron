#Include Once "windows.bi"
#include Once "win/mmsystem.bi"
'#Include Once "netwin.bi"
'		===========================================================================
'		===========================================================================
' SoundTrackXref.bas - a program to parse a scripting language for controlling an 
'						animatronic figure (i.e., Peter Penguin) via an SSC-32 
'						servo controller, and create a cross reference for the sound file
'						names.
'	February, 2015
'		===========================================================================
'		===========================================================================

Declare Function FindIt (array() As String, What As String) As Integer
Declare Function NextStep(myStep As Integer) As Integer

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

ReDim SyncPoints(0) As ULongInt ' flags for sync points; bit encoded by associated players
Dim SyncPointStatus As ULongInt =0
Dim SyncPointIndex As Integer =0

Dim IndexPlayers As Integer = 0, MaxPlayers As Integer =0
Dim ScriptIndex As Integer  = 0,  NotDone    As Integer =-1 ' script player definitions

ReDim LoopStack(0) As Integer, LoopMaximum(0)As Integer
Dim As Integer LoopIndex=0, RepeatEndIndex=0, RepeatIndex=0
Const As Integer RepeatInitialIndex=0

'Control variables
Dim As String ControlKey
Dim SingleStep As Integer = 0
Const PauseKey="\"
Const As String cr=Chr(13), lf =Chr(10),  esc=Chr(27)
Const As String crlf=cr+lf

' Prepare Show
Dim CommPort As String  ' serial port connected to servo controller (SSC-32)
Dim Animatron As String ' name of file containing mapping between move descriptions and SSC-32 commands
Dim ShowName As String  ' name of file containing a script for a given "show"
Dim ShowList As String 

Const dfltMoves = "peter.csv"
Const dfltScript = "PeterLive5"
Const dfltList = "soundtrack_xref.txt"

' short description to servo controller (SSC-32) commands mappings
Print "Character/Move file name [";dfltMoves;"]:"; 
Input Animatron
If Animatron = "" Then Animatron = dfltMoves
Open Animatron For Input As #1

' script file
Print "Show file name [";dfltScript;"]: ";
Input ShowName
If ShowName = "" Then ShowName = dfltScript
ShowName=ShowName+".csv"

' sound file mapping
Print "Byte code file name [";dfltList;"]: ";
Input ShowList
If ShowList = "" Then ShowList = dfltList




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

Open ShowName For Input As #1

Print "+----------------------------+"
Print "| Starting to compile script |"
Print "+----------------------------+"
Print "Press any key to start";
Sleep


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
' 		Output tokens for processed script
'		===========================================================================
'		===========================================================================


Print
Print "+----------------------------------------+"
Print "| Starting to output sound file mappings |"
Print "+----------------------------------------+"
Print "Press any key to start";
Sleep

Print
Dim i As Integer
Dim cmd As String 
Open ShowList For Output As #1
For i=1 To UBound(ScriptAction)
	If ScriptAction(i) = 9 Then
    	Print #1, "Pointer to ";LabelDescription(ScriptDescription(i));" = ";ScriptDescription(i)
		cmd="copy "+LabelDescription(ScriptDescription(i))+" track"+Str(ScriptDescription(i))+".mp3"
		Shell cmd
	EndIf
Next

Print
Print "+-------------------------------------+"
Print "| Finished output sound file mappings |"
Print "+-------------------------------------+"
Print "Press any key to finish";
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

Function NextStep(myStep As Integer) As Integer
	Dim where As Integer
	where = myStep + AdjustStep
	If AdjustStep > 1 Then AdjustStep-=1
	Return where
End Function



