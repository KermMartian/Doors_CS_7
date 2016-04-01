;-----------------------------------------------------------
;	Filename:		APGuis.asm
;	Long name:  	AP GUIs:
;						FileOpen, FileSave, and FileSaveAs
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	May 19, 2010
;	Routines Included:
;		-FileOpen
;		-FileSave
;		-FileSaveAs
;		-FolderCount
;		-AFNext
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

;========================================================
;========================================================
;#####			    FileOpen					  #####
;========================================================
;========================================================

FileOpen:
	ld (FileOSExclude),a							;0=only show files for this program
	call OpenGUIStack
	ld a,c
	push af
		xor a
		ld (ProgsDone),a
		inc a
		ld (FileOSCurFld),a
		ld hl,FOpenData_SmWin
		call PushGUIStacks

#ifdef false
		ld a,2
		ld hl,FOpenData_SmWin
		ld de,FOpenData_SmWinE-FOpenData_SmWin
		call PushGUIStack
		ld a,5
		ld hl,FOpenData_WinB1
		ld de,FOpenData_WinB1E-FOpenData_WinB1
		call PushGUIStack
		ld a,$10
		ld hl,FOpenData_UpBtn
		ld de,FOpenData_UpBtnE-FOpenData_UpBtn
		call PushGUIStack
		ld a,$0E
		ld hl,FOpenData_UpHotspot
		ld de,FOpenData_UpHotspotE-FOpenData_UpHotspot
		call PushGUIStack
#endif
	
FileOpenSoftRes:
		xor a
		ld (FileOSContext),a
		ld (FileOSCur),a
		call FolderCount
		ld a,b
		ld (FileOSTotFF),a

		ld hl,FOpenScrollData
		ld de,CmdShadow
		push de
			ld bc,FOpenScrollDataE-FOpenScrollData
			ldir
			ld hl,CmdShadow+8
			ld a,(FileOSTotFF)
			push hl
				ld l,a
				ld h,0
				ld a,6
				bcall(_divhlbya)
				or a
				jr z,DrawScrollDesk2NoRem
				inc l
DrawScrollDesk2NoRem:
				ld a,l
				or a
				jr nz,DrawScrollDesk21
				inc a
DrawScrollDesk21:
				add a,a
				ld b,a
				add a,a
				add a,b
				pop hl
			ld (hl),a
			inc hl
			inc hl
			ld a,(ProgsDone)
			ld (hl),a
			pop hl
		ld de,FOpenScrollDataE-FOpenScrollData
		ld a,$13
		call PushGUIStack
		call FOpenPutStack
		ld hl,APGUIHook_FOpen
		jp GUIMouseCall
FOpenClick1:
	ld b,0
	jr FOpenClick
FOpenClick2:
	ld b,1
	jr FOpenClick
FOpenClick3:
	ld b,2
	jr FOpenClick
FOpenClick4:
	ld b,3
	jr FOpenClick
FOpenClick5:
	ld b,4
	jr FOpenClick
FOpenClick6:
	ld b,5
FOpenClick:
	ld a,(FileOSCur)
	add a,b
	ld hl,(progptr)
FOpenClickLoop:
	or a
	jr z,FOpenClickLoopDone
	push af
		call AFNext
		pop af
	dec a
	jr FOpenClickLoop
FOpenClickLoopDone:
	call AFNext
	ld hl,APGUIHook_FOpen
	or a
	jp z,GUIMousecall
	dec a
	jp z,FOpenClickFolder
	;==== deal with opening a file here ====
	;first need to check if there's already a file open
	;if so, deal with writing it back
	;OP1 MUST BE PRESERVED THROUGH THE PROCESS
	bcall(_pushop1)
		bcall(_GetRAMNameAP)
		jr c,FOpenClickLoopDone_NoFile
		bcall(_SwapProgChain)					;swap AP file and its program
		bcall(_asmcheckwriteback)
		bcall(_PopProgChain)
FOpenClickLoopDone_NoFile:
		bcall(_PushProgChain)
		push hl
			bcall(_popop1)
		bcall(_chkfindsym)
		pop hl
	jp c,FOpenCancel_PopProgChain
	
	push bc
		ld (hl),1									;A1
		inc hl										;B1
		ld de,Op1
		ex de,hl
		ld bc,9
		ldir										;Copy original name
		pop bc										;return page, and de is now at A2
	ld a,b
	ld (de),a
	push af
		ld hl,TmpProgName2
		ld (ScratchWord),hl
		bcall(_GetArcStatus)
		or a
		bcallnz(_initTmpASMOp1)
		pop af
	or a
	bcallnz(_APGui_gui7ToTop)
	bcall(_SwapProgChain)	;put the program above the file again
	ld c,5					;base element count
	jp FOpenDone
FOpenClickFolder:
	ld a,b
	ld (FileOSCurFld),a
	ld a,(FileOSOnscreen)
	ld b,a
	add a,a
	add a,b				;a=a*3;
	ld b,a				;b=a;
	inc b				;scrollbar
	call PopGUIStacks
	jp FileOpenSoftRes
FOpenClickTable:
	.dw FOpenClick1
	.dw FOpenClick2
	.dw FOpenClick3
	.dw FOpenClick4
	.dw FOpenClick5
	.dw FOpenClick6
FOpenScrollCorrect:
	ld a,(bc)
	ld (FileOSCur),a
	ld a,(FileOSOnscreen)
	ld b,a
	add a,a
	add a,b				;a=a*3;
	ld b,a				;b=a;
	call PopGUIStacks
	call FOpenPutStack
	ret
FOpenUpFld:
	ld a,(FileOSCurFld)
	cp 1
	jr z,FOpenUpFldNo
	ld b,a
	ld a,1
	bcall(_FldSearch)
	ld a,c
	or a
FOpenUpFldNo:
	ld hl,APGUIHook_FOpen
	jp z,GUIMouseCall
	ld (FileOSCurFld),a
	ld a,(FileOSOnscreen)
	ld b,a
	add a,a
	add a,b				;a=a*3;
	ld b,a				;b=a;
	inc b				;scrollbar
	call PopGUIStacks
	jp FileOpenSoftRes
FOpenPutStack:
FOpenNameSetup:
	ld b,0
	ld hl,(progptr)
FOpenNameSetup1:
	ld a,(FileOSCur)
	cp b
	jr z,FOpenNameSetup2
	push bc
	call AFNext
	pop bc
	inc b
	jr FOpenNameSetup1
FOpenNameSetup2:
	xor a
	ld (FileOSOnscreen),a
	inc a
	ld (FileOSYCoord),a
FOpenNameSetup3:
	push bc
		call AFNext
		pop bc
	or a
	jp z,FOpenDoneDisp
	push bc
		push hl
			dec a
			jp z,FopenDispFolder
			;display file here
			call ClearFOSStage
			ld hl,IconSpace32b
			push hl
				ld a,(FileOSYCoord)
				ld (hl),7
				inc hl
				ld (hl),a
				inc hl
				ld (hl),0
				inc hl
				ld de,Op1+1
				ex de,hl
				ld bc,8
				ldir
				pop hl
			ld de,12			;extra zt for 8-wide prognames
			ld a,4
			push hl
				call PushGUIStack
				call ClearFOSStage
				pop hl
			push hl
				ld (hl),1
				inc hl
				ld a,(FileOSYCoord)
				inc a
				ld (hl),a
				inc hl
				ld (hl),5
				inc hl
				ld de,FOpenFileIcon
				ld bc,5
				ex de,hl
				ldir
				pop hl
			ld de,8
			ld a,$10
			push hl
				call PushGUIStack
				call ClearFOSStage
				pop hl
			push hl
				ld (hl),1
				inc hl
				ld a,(FileOSYCoord)
				ld (hl),a
				inc hl
				ld (hl),32
				inc hl
				ld (hl),6
				inc hl
				push hl
					ld a,(FileOSOnscreen)
					add a,a
					ld e,a
					ld d,0
					ld hl,FOpenClickTable
					ld a,(FileOSContext)
					or a
					jr z,FOpenDispSkip1
					ld hl,FSaveClickTable
FOpenDispSkip1:
					add hl,de
					bcall(_ldhlind)
					pop de
				ex de,hl
				ld (hl),e
				inc hl
				ld (hl),d
				pop hl
			ld a,$0E
			ld de,6
			call PushGUIStack
			jp FOpenDisp2
FOpenDispFolder:
			call ClearFOSStage
			ld hl,IconSpace32b
			push hl
				ld a,(FileOSYCoord)
				ld (hl),7
				inc hl
				ld (hl),a
				inc hl
				ld (hl),0
				inc hl
				ex de,hl
				ld bc,8
				ldir
				pop hl
			ld de,12			;extra zt for 8-wide prognames
			ld a,4
			push hl
				call PushGUIStack
				call ClearFOSStage
				pop hl
			push hl
				ld (hl),1
				inc hl
				ld a,(FileOSYCoord)
				inc a
				ld (hl),a
				inc hl
				ld (hl),5
				inc hl
				ld de,FOpenFldIcon
				ld bc,5
				ex de,hl
				ldir
				pop hl
			ld de,8
			ld a,$10
			push hl
				call PushGUIStack
				call ClearFOSStage
				pop hl
			push hl
				ld (hl),1
				inc hl
				ld a,(FileOSYCoord)
				ld (hl),a
				inc hl
				ld (hl),32
				inc hl
				ld (hl),6
				inc hl
				push hl
					ld a,(FileOSOnscreen)
					add a,a
					ld e,a
					ld d,0
					ld hl,FOpenClickTable
					ld a,(FileOSContext)
					or a
					jr z,FOpenDispSkip2
					ld hl,FSaveClickTable
FOpenDispSkip2:
					add hl,de
					bcall(_ldhlind)
					pop de
				ex de,hl
				ld (hl),e
				inc hl
				ld (hl),d
				pop hl
			ld a,$0E
			ld de,6
			call PushGUIStack
FOpenDisp2:			
			pop hl
		pop bc
	ld a,(FileOSYCoord)
	add a,6
	ld (FileOSYCoord),a
	ld a,(FileOSOnscreen)
	inc a
	ld (FileOSOnscreen),a
	cp 6
	jr z,FOpenDoneDisp
	push de
		ld de,(ptemp)
		call mos_cphlde
	pop de
	jp nz,FOpenNameSetup3
FOpenDoneDisp:
	ret
ClearFOSStage:
	push de
		push hl
			push bc
				ld hl,IconSpace32b			;these lines clear out IconSpace32b for the stage
				ld (hl),0
				push hl
					pop de
				inc de
				ld bc,31
				ldir
				pop bc
			pop hl
		pop de
	ret
FOpenDone:
		pop af
	or a
	jr nz,FOpenDonePops
FOpenDoneCGS:
	call CloseGUIStack
	jp FOpenDoneUpdates
FOpenDonePops:
	ld a,(FileOSOnscreen)
	ld b,a
	add a,a
	add a,b								;a=a*3;
;	ld b,5
FOpenDonePopsAdd:
	add a,c
	ld b,a								;b = number of items to remove
	call PopGUIStacks					;remove b items
FOpenDoneUpdates:
	bcall(_GetRAMNameAP)
	jr c,FOpenCancel
	rst 20h
	bcall(_chkfindsym)
	push de
		ex de,hl
		bcall(_ldhlind)
		ld bc,8
		or a
		sbc hl,bc
		pop de							;hl = size-8, de = pointer to size byte, bc = 8
	ex de,hl							;de = size-8, hl = pointer to size byte, bc = 8
	push hl								;pointer to size byte
		add hl,bc
		inc hl
		inc hl
		push de
			pop bc
		pop de
	or a								;rcf
	ret
FOpenCancel_PopProgChain:
		bcall(_PopProgChain)
FOpenDoneCancel:
		ld c,5
		jp FOpenDone
FOpenCancel:
	ld hl,0
	push hl
	push hl
	pop de
	pop bc
	scf									;scf
	ret

;========================================================
;========================================================
;#####			    FileSave					  #####
;========================================================
;========================================================
FileSave:
	;inputs:
	;hl  =>  data
	;bc  ==  size
	;de  =>  3-byte type
	;op1 ==  06,"NAME",0, eg.
	;
	;outputs:
	;zero flag: z = success, nz = fail (duplicate or lowmem)
	;registers: same as for FileOpen **only on success** (preserved input registers if fail)
	ld (RelocatablePtr1),hl
	ld (RelocatablePtr2),de
	push bc	
		bcall(_chkfindsym)				;preserves Op1
		jp nc,FileSaveFail1
		bcall(_pushop1)
			bcall(_GetRAMNameAP)
			jr c,FileSaveNoAPFile		;no AP file open, don't bother with this
			bcall(_SwapProgChain)		;move the file to the top
			bcall(_asmcheckwriteback)	;writeback, if necessary
			bcall(_PopProgChain)		;all done
FileSaveNoAPFile:
			bcall(_popop1)				;recall the desired name
		pop de								;size again
	push de								;was bc, the size
		ld hl,10+15						;2-byte size, 8-byte header, 15-byte (max) VAT entry
		add hl,de
		bcall(_enoughmem)
		jr c,FileSaveFail1
		pop hl							;recall size as hl
	push hl
		ld de,8
		add hl,de
		bcall(_createprotprog)				;create it
		inc de
		inc de
		ld hl,APHeader
		ld bc,5
		ldir
		ld hl,(RelocatablePtr2)
		ld bc,3
		ldir
		pop bc										;recall size again
	ld hl,(RelocatablePtr1)
	ldir
	bcall(_pushop1)
		bcall(_APGui_gui7ToTop)
		bcall(_PushProgChain)
		ld (hl),1
		inc hl
		push hl
			bcall(_popop1)
		ld hl,op1
		pop de
	ld bc,9
	ldir
	xor a
	ld (de),a								;not archived
	bcall(_SwapProgChain)
	jp FOpenDoneUpdates
FileSaveFail1:
		pop bc
	xor a
	ld h,a
	ld l,a												;zero out HL
	push hl
		push hl
			pop bc											;zero out BC too
		pop de
	cp 1
	ret
;========================================================
;========================================================
;#####			    FileSaveAs				  #####
;========================================================
;========================================================
	;inputs:
	;hl  =>  data
	;bc  ==  size
	;de  =>  3-byte type

FileSaveAs:
	ld (FileOSExclude),a							;0=only show files for this program
	ld (RelocatablePtr1),hl
	ld (RelocatablePtr2),de
	push bc
		call OpenGUIStack
		ld a,c
		pop bc
	push af
		push bc
			xor a
			ld (ProgsDone),a
			inc a
			ld (FileOSContext),a
			ld (FileOSCurFld),a
			ld a,2
			ld hl,FSaveData_SmWin
			ld de,FSaveData_SmWinE-FSaveData_SmWin
			call PushGUIStack
			ld a,5
			ld hl,FSaveData_WinB1
			ld de,FSaveData_WinB1E-FSaveData_WinB1
			call PushGUIStack
			ld a,$0E
			ld hl,FSaveData_UpHotspot
			ld de,FSaveData_UpHotspotE-FSaveData_UpHotspot
			call PushGUIStack
			;also need the filename text element and save button in here somewhere
			ld a,$09
			ld hl,FSaveFname
			ld de,8
			call PushGUIStack
			ld a,$07
			ld hl,FSaveSaveBtn
			ld de,FSaveSaveBtnE-FSaveSaveBtn
			call PushGUIStack
			ld a,$10
			ld hl,FOpenData_UpBtn+3
			ld de,FOpenData_UpBtnE-FOpenData_UpBtn-3
			call PushGUIStack

FileSaveSoftRes:
			xor a
			ld (FileOSCur),a
			call FolderCount
			ld a,b
			ld (FileOSTotFF),a

			ld hl,FSaveScrollData
			ld de,CmdShadow
			push de
				ld bc,FSaveScrollDataE-FSaveScrollData
				ldir
				ld hl,CmdShadow+8
				ld a,(FileOSTotFF)
				push hl
					ld l,a
					ld h,0
					ld a,6
					bcall(_divhlbya)
					or a
					jr z,DrawScrollDesk22NoRem
					inc l
DrawScrollDesk22NoRem:
					ld a,l
					or a
					jr nz,DrawScrollDesk221
					inc a
DrawScrollDesk221:
					add a,a
					ld b,a
					add a,a
					add a,b
					pop hl
				ld (hl),a
				inc hl
				inc hl
				ld a,(ProgsDone)
				ld (hl),a
				pop hl
			ld de,FSaveScrollDataE-FSaveScrollData
			ld a,$13
			call PushGUIStack
			call FOpenPutStack
FSaveMLoop:
			ld hl,APGUIHook_FSave
			jp GUIMouseCall
FSaveClick1:
			ld b,0
			jr FSaveClick
FSaveClick2:
			ld b,1
			jr FSaveClick
FSaveClick3:
			ld b,2
			jr FSaveClick
FSaveClick4:
			ld b,3
			jr FSaveClick
FSaveClick5:
			ld b,4
			jr FSaveClick
FSaveClick6:
			ld b,5
FSaveClick:
			ld a,(FileOSCur)
			add a,b
			ld hl,(progptr)
FSaveClickLoop:
			or a
			jr z,FSaveClickLoopDone
			push af
				call AFNext
				pop af
			dec a
			jr FSaveClickLoop
FSaveClickLoopDone:
			call AFNext
			dec a
			jp nz,FSaveMLoop
			ld a,b
			ld (FileOSCurFld),a
			ld a,(FileOSOnscreen)
			ld b,a
			add a,a
			add a,b				;a=a*3;
			ld b,a				;b=a;
			inc b				;scrollbar
			call PopGUIStacks
			jp FileSaveSoftRes
FSaveCancel:
			pop bc
		ld c,7
		jp FOpenDone
FileSaveAsGo:
			ld a,2
			call GUIFindThis
;			call GUIFindFirst
;			call GUIFindNext
;			call GUIFindNext
			ld de,10
			add hl,de
			ld a,(hl)
			or a
			jr z,FSaveCancel
			ld de,Op1
			ld a,$06
			ld (de),a
			inc de
			ld bc,8
			ldir
			bcall(_chkfindsym)
			jp nc,FSaveMLoop
			pop hl				;size
		push hl
			ld de,8			;5-byte header, 3-byte type
			add hl,de
			bcall(_createprotprog)
			dec hl
			ld a,(FileOSCurFld)
			ld (hl),a
			ex de,hl
;			ld (FPopsWhere),hl
			
			;set folder dirty flag if this new prgm is not in fldr 1 == "MAIN"
			push hl
				dec a
				jr z,FileSaveAsGo_FldNotDirty
				ld hl,AVOff_FolderDirty
				bcall(_DAVLCheckOffset)
				ld (hl),1
FileSaveAsGo_FldNotDirty:
				pop hl					
			inc hl
			inc hl
			ld de,APHeader
			ld bc,5
			ex de,hl
			ldir
			
			ld hl,(RelocatablePtr2)			;type bytes
			ld bc,3
			ldir
			pop bc
		ld hl,(RelocatablePtr1)				;data
		ldir
		;first need to check if there's already a file open
		;if so, deal with writing it back
		bcall(_GetRAMNameAP)
		jr c,FSaveRemove_NoFile
		bcall(_SwapProgChain)					;swap AP file and its program
		bcall(_asmcheckwriteback)
		bcall(_PopProgChain)
FSaveRemove_NoFile:
		bcall(_PushProgChain)
		ld (hl),1									;A1
		inc hl										;B1
		ld de,Op4
		ex de,hl
		ld bc,9
		ldir										;Copy original name
		xor a
		ld (de),a
		bcall(_APGui_gui7ToTop)
		bcall(_SwapProgChain)	;put the program above the file again
		ld c,7
		jp FOpenDone

FSaveUpFld:
				ld a,(FileOSCurFld)
				cp 1
				jr z,FSaveUpFldNo
				ld b,a
				ld a,1
				bcall(_FldSearch)
				ld a,c
				or a
FSaveUpFldNo:
				ld hl,APGUIHook_FSave
				jp z,GUIMouseCall
				ld (FileOSCurFld),a
				ld a,(FileOSOnscreen)
				ld b,a
				add a,a
				add a,b				;a=a*3;
				ld b,a				;b=a;
				inc b				;scrollbar
				call PopGUIStacks
				jp FileSaveSoftRes
FSaveClickTable:
	.dw FSaveClick1
	.dw FSaveClick2
	.dw FSaveClick3
	.dw FSaveClick4
	.dw FSaveClick5
	.dw FSaveClick6

;+--------------------------------------------------------+
;|     				FolderCount					   |
;| Counts the # of folders in (FileOSCurFld) as b.	   |
;+--------------------------------------------------------+
FolderCount:
	ld hl,(ProgPtr)
	ld b,0
FolderCountLoop:
	ld de,(ptemp)
	call mos_cphlde
	ret z
	ld a,(hl)
	and $1f
	cp $05
	jr z,FolderCountProg
	cp $06
	jr z,FolderCountProg
FolderContinue:
	ld de,-6
	add hl,de
	push bc
		ld b,(hl)
		inc b
FolderCountNL:
		dec hl
		djnz FolderCountNL
		pop bc
	jr FolderCountLoop
FolderCountProg:
	push hl
		dec hl
		ld d,(hl)
		ld a,(FileOSCurFld)
		cp d
		jr nz,FolderCountProg2

		dec hl
		dec hl
		ld e,(hl)
		dec hl
		ld d,(hl)
		dec hl
		ld a,(hl)
		ld (CurROMPage),a
		ex de,hl
		call SetUpROMP2
		inc hl
		inc hl
		ld c,0
		push bc
			ld de,APHeader
			ld b,5
			ex de,hl
AFCountCheckLoop:
			call GetArcProgByteDEP2
			cp (hl)
			jr z,AFCountCheckLoop2
			inc c
AFCountCheckLoop2:
			inc hl
			inc de
			djnz AFCountCheckLoop
			ld a,c
			pop bc
		or a
		jr nz,FolderCountCheckFld
		ld a,(FileOSExclude)
		or a
		jr nz,FolderCountProgInc
		push bc
			call APGUIs_VerifyType
			pop bc
		jr nz,FolderCountCheckFld
		pop hl
	push hl
		ld de,-7
		add hl,de
		ld a,(hl)
		cp 'z'+1											;is this a DCS temporary file? Need a more complex check =122=$7A
		jr c,FolderCountProgInc
FolderCountCheckFld:
		pop hl
	push hl
		ld de,-7
		add hl,de
		ld a,(hl)
		cp '%'
		jr nz,FolderCountProg2
		dec hl
		ld a,(hl)
		cp 'F'
		jr nz,FolderCountProg2
		dec hl
		ld a,(hl)
		cp 'L'
		jr nz,FolderCountProg2
		dec hl
		ld a,(hl)
		cp 'D'
		jr nz,FolderCountProg2
FolderCountProgInc:
		inc b
FolderCountProg2:
	pop hl
	jr FolderContinue
;+--------------------------------------------------------+
;|                     AFNext                             |
;| Finds next file or folder in the VAT given a ptr.      |
;+--------------------------------------------------------+
AFNext:
	;HL = ptr to current location
AFNextLoop:
	ld de,(ptemp)
	call mos_cphlde
	jr nz,AFNextLoop2
	xor a
	ret
AFNextLoop2:
	ld a,(hl)
	and $1f
	cp $05
	jr z,AFNextProg
	cp $06
	jr z,AFNextProg
AFNextContinue:
	ld de,-6
	add hl,de
	push bc
		ld b,(hl)
		inc b
AFNextNL:
		dec hl
		djnz AFNextNL
		pop bc
	jr AFNextLoop
AFNextProg:
	push hl
		dec hl
		ld d,(hl)
		ld a,(FileOSCurFld)
		cp d
		jp nz,AFNextProg2
		
		dec hl
		dec hl
		ld e,(hl)
		dec hl
		ld d,(hl)
		dec hl
		ld a,(hl)
		ld (CurROMPage),a
		ex de,hl
		call SetUpROMP2
		inc hl
		inc hl
		ld c,0
		push bc
			ld de,APHeader
			ld b,5
			ex de,hl
AFNextCheckLoop:
			call GetArcProgByteDEP2
			cp (hl)
			jr z,AFNextCheckLoop2
			inc c
AFNextCheckLoop2:
			inc hl
			inc de
			djnz AFNextCheckLoop
			ld a,c
			pop bc
		or a
		jr z,AFNextFileFound
AFNextFileNotFound_ChkFld:
		pop hl
	push hl
		ld de,-7
		add hl,de
		ld a,(hl)
		cp '%'
		jr nz,AFNextProg2
		dec hl
		ld a,(hl)
		cp 'F'
		jr nz,AFNextProg2
		dec hl
		ld a,(hl)
		cp 'L'
		jr nz,AFNextProg2
		dec hl
		ld a,(hl)
		cp 'D'
		jr nz,AFNextProg2
AFNextFolderFound:
		dec hl
		ld b,(hl)
		pop hl
	push hl
		dec hl
		dec hl
		dec hl
		ld e,(hl)
		dec hl
		ld d,(hl)
		inc de
		inc de
		ld a,1
		pop hl
	push de
		ld de,-12
		add hl,de
		pop de
	;a = 1 = folder
	;b = folder number	
	;de-> folder name
	;hl = __next__ VAT entry start
	ret
AFNextFileHide:
	ld de,7
	add hl,de
	jp AFNextContinue
AFNextFileFound:
		ld a,(FileOSExclude)
		or a
		jr nz,AFNextFileFoundSkipCheck
		call APGUIs_VerifyType
		jr nz,AFNextProg2
AFNextFileFoundSkipCheck:
		bcall(_ZeroOp1)
		pop hl
	ld de,Op1
	ld a,(hl)
	ld (de),a
	inc de
	dec hl
	dec hl
	dec hl
	dec hl
	dec hl
	dec hl
	ld b,(hl)
	dec hl
	ld a,(hl)
	cp 'z'+1											;is this a DCS temporary file? Need a more complex check =122=$7A
	jr nc,AFNextFileHide
	
AFNextFileFoundLoop:
	ld a,(hl)
	ld (de),a
	inc de
	dec hl
	djnz AFNextFileFoundLoop
	ld a,2
	;a = 2 = file
	;Op1 = type, filename
	;hl = __next__ VAT entry start
	ret
AFNextProg2:
		pop hl
	jp AFNextContinue

APGUIs_VerifyType:									;pointer to first byte of program's type in de
	ld hl,$9d95+16									;the "# of types" is 16 bytes into a prgm's header, ignoring $BB6D
	ld c,(hl)
	ld b,3
	inc hl
AFNextFileFound_CheckTypeOuter:
	push bc
		push de
			ld c,0
AFNextFileFound_CheckTypeInner:
			call GetArcProgByteDEP2
			cp (hl)
			jr z,AFNextFileFound_CheckTypeInner2
			inc c
AFNextFileFound_CheckTypeInner2:
			inc hl
			inc de
			djnz AFNextFileFound_CheckTypeInner
			ld a,c
			pop de
		pop bc
	or a
	ret z
	dec c
	jr nz,AFNextFileFound_CheckTypeOuter
	inc c			;unset z flag
	ret

;-------------------------------------------------------
SetUpROMP2:
	ld a,(CurROMPage)
	or a
	ret z
	push de
	ld de,9
	add hl,de
	call GetArcProgByteP2
	ld d,0
	inc a
	ld e,a
	add hl,de
	pop de
	ret

GetArcProgByteP2:
	push de
	push bc
	push hl
	ld a,(CurROMPage)
	or a
	jr z,GetArcProgByteRegP2
	ld b,a
	call AutoArcPtrUpdateP2
	bcall(_LoadCIndPaged)
	ld a,c
	pop hl
	pop bc
	pop de
	ret
GetArcProgByteRegP2:
	pop hl
	pop bc
	pop de
	ld a,(hl)
	ret

GetArcProgByteDEP2:
	push bc
	push hl
	push de
	ld a,(CurROMPage)
	or a
	jr z,GetArcProgByteRegDEP2
	ld b,a
	ex de,hl
	call AutoArcPtrUpdateP2
	bcall(_LoadCIndPaged)
	ld a,c
	pop de
	pop hl
	pop bc
	ret
GetArcProgByteRegDEP2:
	pop de
	pop hl
	pop bc
	ld a,(de)
	ret

AutoArcPtrUpdateP2:
	bit 7,h
	ret z
	inc b
	res 7,h
	set 6,h
	ret

APHeader:
	.db $BB,$6D,$C9,$31,$80

APGUIHook_FSave:
	ld b,1
	jr APGUIHook_All
APGUIHook_FOpen:
	ld b,0
APGUIHook_All:
	ld a,0ffh				;reset
	out (1),a
	ld a,0fdh				;Clear and more
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	cp 251d					;[-]
	jr z,APGUIScrollUp
	cp 253d					;[+]
	ret nz
APGUIScrollDown:
	ld hl,(83*256)+49
	jr APGUIScrollFinish
APGUIScrollUp:
	ld hl,(83*256)+19
APGUIScrollFinish:
	pop de
	ld a,b
	or a
	jr z,APGUIScrollFinish2
	ld h,50
APGUIScrollFinish2:
	ld (MseY),hl				;MseX in h, MseY in l
	ret