;-----------------------------------------------------------
;	Filename:		dcspropp.asm
;	Long name:  	Doors CS 6 Properties Subsection for 83+/84+/SE
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	April 25, 2010
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

PropMenu:
	;b is zero for an empty space, 1 if there's a program underneath
	;this is set in runprog.asm
	push bc																			;added when ld (iy+dcsProgFlags),a removed
		push ix
			ld hl,PlotsScreen+(12*2)+6
#ifdef enableeditor
			ld b,53
#endif
#ifndef enableeditor
			ld b,47
#endif
			push bc
PropClearLoop:
				ld de,PropMask
				ld c,6
PropClearLoopInner:
				ld a,(de)
				and (hl)
				ld (hl),a
				inc hl
				inc de
				dec c
				jr nz,PropClearLoopInner
				push de
					ld de,6
					add hl,de
					pop de
				djnz PropClearLoop
				ld l,2
				ld ix,PropSprite
				pop bc
			ld c,5
			ld a,52
			call iLargeSprite
			call iFastCopy
			pop de
		ld hl,SaveIX
		ld (hl),e
		inc hl
		ld (hl),d

		bcall(_OpenGUIStack)
		xor a
		ld hl,GUIDesktop_NullR					;No reason not to reuse it
		ld de,1
		bcall(_PushGUIStack)
		pop af									;was b
	or a
	push af
		jr z,PropSetupEmptySpace
		ld hl,GUIPropP_HS_1
		bcall(_PushGUIStacks)	
PropSetupEmptySpace:
		ld hl,GUIPropP_HS_6
		bcall(_PushGUIStacks)
		pop af
	jr z,PropSetupFinish		;no need to "or a" - we're popping the flags

	ld hl,GUIPropP_HS_8
	bcall(_PushGUIStacks)
PropSetupFinish:
	ld a,$0e
	ld de,6
	push de
		ld hl,GUIDesktop_HS_Quit+3		;this one is a bit risky, but should work	;PLUS THREE
		bcall(_PushGUIStack)
		pop de
	ld a,$0e
	ld hl,GUIPropP_HS_11					;catchall
	bcall(_PushGUIStack)
	ld a,1
	ld (MouseMode),a
	ld a,0FFh
	ld (TabFuncMode),a
	ld hl,MainMouseHook
	bcall(_GUIMouse)

PropMenuMouseReturn:
	pop hl \ pop hl \ pop hl
	push bc
		bcall(_CloseGUIStack)
		pop bc

	ld hl,SaveIX							;restore ix
	ld e,(hl)
	inc hl
	ld d,(hl)
	push de
		pop ix

	ld a,b
	or a									;0
	jp z,PropDelete
	dec a									;1
	jp z,PropUnlock
	dec a									;2
	jp z,PropUnarc
	dec a									;3
	jp z,PropCopy
	dec a									;4
	jp z,MoveToFldSelect
	dec a									;5
	jp z,MoveToFld
	dec a									;6
	jp z,CreateFldr
	dec a									;7
	jp z,PropRename
	dec a									;8
	jp z,PropHide
	ld a,(ix+5)
	and %00011100
	or a					;cp 0
	jp nz,RealStartNoReset
	ld a,b
	sub 9									;9
	jp nz,RealStartNoReset
	
#ifdef enableeditor
	call EditorBASIC
	jp ASMStart

#include "editor.asm"
#else
	jp ASMStart
#endif
PropHide:
	ld l,(ix+0)
	ld h,(ix+1)
	ld de,-7
	add hl,de
	push hl
		ld de,BaseFld+1
		ld b,4
PropHideFldChkLoop:
		ld a,(de)
		cp (hl)
		jr nz,PropHideContinue
		inc de
		dec hl
		djnz PropHideFldChkLoop
		pop hl
	jp ASMStart
PropHideContinue:
		pop hl
	push ix
		push ix
			pop hl
		call RunNameCopy
		pop ix
	ld a,(ix+5)				;type
	and %10000000		;archived
	push af
		jr z,PropHideContinueRAM
		push ix
			push ix
				call Arc_Unarc
				pop hl
			call RunNameCopy
			pop ix
PropHideContinueRAM:
		ld de,-7
		add hl,de
		ld a,(hl)
		cp 27
		jr nc,PropHideGo
		add a,$40
		jr PropHideFinish
PropHideGo:
		sub $40
		cp 27
		jr c,PropHideFinish
		ld a,1
PropHideFinish:
		ld (op1+1),a
		push af
			push hl
				bcall(_chkfindsym)
				pop hl
			jr nc,PropHideFinish_NoRename
			pop af
		ld (hl),a
		pop af
	jr z,PropHideFinishRAM
	push ix
		pop hl
	call RunNameCopy
	call Arc_Unarc
PropHideFinishRAM:
	jp ASMStart
PropHideFinish_NoRename:
			pop af
		pop af
	jr PropHideFinishRAM
PropUnarc:
;	push ix
;		pop hl
;	ld de,5
;	add hl,de
;	ld a,(hl)
	ld a,(ix+5)
	and $fe							;mask off lock flag!
	;ld a,(iy+dcsProgFlags)
	cp %01001000
	jr z,PropUnarcFld
	push ix
	pop hl
	call RunNameCopy
	call Arc_Unarc
PropUnarcFld:
	jp ASMStart
PropUnlock:
	ld h,(ix+1)
	ld l,(ix+0)
;	push ix
;		inc ix
;		inc ix
;		inc ix
;		inc ix
;		inc ix
;		pop hl
;	bcall(_ldhlind)
	ld a,$05
	bit 0,(ix+5)
;	jr z,PropLock
;	jr PropLock2
	jr nz,PropLock2
PropLock:
	inc a
PropLock2:
	ld (hl),a
	jp ASMStart
PropDelete:
	push ix
	pop hl
#ifdef Folders
	bcall(_ldhlind)
	ld de,-7
	add hl,de
	ld a,(hl)
	cp '%'
	jr nz,PropDelNoFld
	ld de,-4
	add hl,de
	ld b,(hl)
	ld a,4
	call FldSearch
	push ix
	pop hl
	call RunNameCopy
	push hl
	push de
	push bc
	bcall(_pushop1)
	pop bc
	pop de
	pop hl
	ld a,b
	or a
	call nz,Arc_Unarc
	bcall(_popop1)
	bcall(_chkfindsym)
	bcall(_delvar)
	jp PropDone2			;set folder save dirty flag and then go to AsmStart
PropDelNoFld:
	push ix
	pop hl
#endif
	push hl
		inc hl
		inc hl
		inc hl
		inc hl
		ld a,(hl)
		pop hl
	push af
		call RunNameCopy
		pop af
	ld b,a
	bcall(_delvararc)
	jp PropDone2
PropCopy:
	ld a,(ix+5)
	and %01001000
	sub %01001000
	jp z,ASMStart
	xor a
	jr PropRenCopy
PropRename:
	ld a,1
PropRenCopy:
	ld (ScratchVar),a
	push ix
		push ix
			call DAVLCheck
			pop hl
		call RunNameCopy
		pop ix
	ld a,(ScratchVar)
	ld b,a
	ld a,(ix+5)
	ld (iy+dcsProgFlags),a
	and $7e						;mask off archived/lock bits
	or b
	cp %01001000			;this will never jump for rename
	jp z,PropUnarcFld		;don't copy folders, but renaming is ok
	push ix
		push ix
			pop hl
		call RunNameCopy
		bcall(_pushop1)
		pop hl
	ld a,(ScratchVar)
	or a
	jr z,PropCopyNotR
	ld a,(iy+dcsProgFlags)
	and $7e						;mask off archived/lock bits
	cp %01001000				;Folder?
	jr nz,PropCopyNotR
	push ix						;THIS PART IS FOR **RENAMING FOLDERS**
		push ix
			pop hl
		call RunNameCopy
		ld a,b
		ld (CurROMPage),a
		or a
		call nz,Arc_Unarc
		pop hl
	call RunNameCopy
	push de
		ld ix,Op1+1
		ld a,8
		bcall(_PropString)
		pop hl
	or a
	jp z,PropDonePopOp1
	inc hl
	inc hl
	ld de,Op1+1
	ex de,hl
	ld bc,8
	ldir
PropDonePopOp1:
	bcall(_popop1)				;from _pushop1 above
	jp PropDone
PropCopyNotR:
	ld a,(ScratchVar)
	push af
		push ix
			push ix
				pop hl
			call RunNameCopy
			ld a,b
			ld (CurROMPage),a
			or a
			call nz,Arc_Unarc
			pop hl
		push hl
			call RunNameCopy
			ex de,hl
			push hl
				bcall(_ldhlind)
				push hl
					ex de,hl
					bcall(_FreeRAM)
					sbc hl,de
					ld de,-64
					add hl,de
					ld de,0
					bcall(_cphlde)
					jp m,PropDoneCopyNotR_Cleanup
					ld ix,Op1+1
					ld a,8
					bcall(_PropString)
					or a
					jp z,PropDoneCopyNotR_Cleanup
					ld hl,Op1
					ld (hl),6
					bcall(_chkfindsym)
					jp nc,PropDoneCopyNotR_Cleanup
					pop hl
				push hl
					bcall(_createprotprog)
					dec hl
					ld a,(CurFldr)
					ld (hl),a						;folder!
					pop bc
				pop hl
			inc bc
			inc bc
			ldir
			bcall(_op4toop1)
			bcall(_chkfindsym)
			ld a,(CurROMPage)
			push af
				or a
				call nz,Arc_Unarc
				pop af
			ld (CurROMPage),a
			pop hl
			bcall(_popop1)				;from _pushop1 above
		pop af
	or a
	jr z,PropDone
	bcall(_ChkFindSym)
	bcallnc(_delvar)
	jr PropDone2
PropDone:
;	bcall(_popop1)
	bcall(_ChkFindSym)
	jr c,PropDone2
	ld a,(CurROMPage)
	or a
	call nz,Arc_Unarc
PropDone2:

	ld hl,AVOff_FolderDirty
	call DAVLCheckOffset
	ld (hl),1

	jp ASMStart
PropDoneCopyNotR_Cleanup:
	bcall(_popop1)
PropDone1:
	pop af
	pop af
	pop af
	pop af
	jr PropDone
;-----------------------------------------------
#ifdef false
String:
	im 1
	ld	(IconSpace8b+7),a 			; store max length
	bcall(_clrlcdfull)
	bcall(_homeup)
	bcall(_getk)
	ld hl,EnterNameTxt
	call putsApp
	ld      b,0
string_loop:
	push 	bc
	bcall(_getk)
	pop	bc
	or	a
	jr	z,string_loop
	cp	56
	jr	z,backup			; [DEL] pressed
	cp	9
	jr	z,userdone			; [ENTER] pressed
	ld	c,a
	ld	a,(IconSpace8b+7)			; max length
	cp	b
	jr	z,string_loop
	ld hl,char_strtable+7
	ld a,c
	sub 9
	cp 6
	jp p,LoopTxt1
	sub 7
	jr DoneLoop
Looptxt1:
	ld de,7
	add hl,de
	sub 8
	cp 6
	jp p,LoopTxt2
	sub 7
	jr DoneLoop
LoopTxt2:
	ld de,7
	add hl,de
	sub 8
	cp 7
	jp p,LoopTxt3
	sub 7
	jr DoneLoop
LoopTxt3:
	ld de,8
	add hl,de
	sub 8
	cp 7
	jp p,LoopTxt4
	sub 8
	jr DoneLoop
LoopTxt4:
	sub 9
	ld d,0
	ld e,a
	jr DoneLoop1
DoneLoop:
	ld d,0
	ld e,a
	or a
	jr z,DoneLoop1
	jp p,DoneLoop1
	ld d,$ff
DoneLoop1:
	add hl,de
	ld a,(hl)
	ld (ix),a
	inc ix
	bcall(_putc)
	inc      b
	jr	string_loop
backup: 
	xor	a
	cp	b
	jr	z,string_loop
	dec	b
	dec	ix
	ld	(ix),32
	ld	hl,curcol
	dec	(hl)
	ld	a,32
	bcall(_putc)
	dec	(hl)
	jp	string_loop
userdone:
	ld	a,(IconSpace8b+7)
	push bc
	sub	b			; string less than max
	jr	z,ending
	ld	b,a
spaceloop:
	ld	(ix),0
	inc	ix
	djnz	spaceloop
ending:
#ifdef enableCn2
	ld hl,35
	call DAVLCheckOffset
	ld a,(hl)
	or a
	jr z,endingnocn2
	im 2
endingnocn2:
#endif
	pop af
	ret
char_strtable:
	.db "AAWRMHA"
	.db "BEVQLGA"
	.db "CZUPKFC"
	.db "DYTOJEBA"
	.db "XSNIDA"
#endif

CreateFldr:
	xor a
	call FldSearch
	push bc
		bcall(_zeroop1)
		ld de,Op1
		ld hl,BaseFld
		ld bc,5
		ldir
		ex de,hl
		pop bc
	inc c
	ld (hl),c
	ld a,c
	or a
	jp z,RealStartNoReset
	bcall(_pushop1)
	bcall(_chkfindsym)
	jp nc,RealStartNoReset
	ld hl,8
	bcall(_createprog)
	push hl
		inc de
		inc de
		push de
			pop ix
		xor a
		ld (ix),a
		push ix
			bcall(_PropString)
			pop hl
		or a
		jr nz,CreateFldFinalize
		bcall(_popop1)
		bcall(_chkfindsym)
		bcall(_delvar)
		jp RealStart
CreateFldFinalize:
		bcall(_popop1)
		pop hl
	dec hl
	ld a,(CurFldr)
	ld (hl),a
	
	ld hl,AVOff_FolderDirty						;set FldSv dirty flag
	call DAVLCheckOffset
	ld (hl),1
	
	jp RealStart
MoveToFld:
	ld hl,(PasteWord)
	ld a,h
	or l
	jr z,MoveToFldEnd
	
;	inc hl
;	ld a,(hl)
;	and $1f
;	dec hl
;	cp $05
;	jr z,MoveToFld_IsPrgm
;	cp $06
;	jr nz,MoveToFldEnd
;MoveToFld_IsPrgm:
	;hl is at T2 byte of program/file/folder
	push hl
		ld de,-5								;back to NL byte
		add hl,de
		push hl
			ld de,Op1
			push de
				ld hl,BaseFld
				ld bc,5
				ldir
				ld a,(CurFldr)
				ld (de),a
				pop hl
			ld (hl),5
			pop de
		ld b,6
MoveToFldCpLoop:
		ld a,(de)
		cp (hl)
		jr nz,MoveToFldGo
		inc hl
		dec de
		djnz MoveToFldCpLoop
		pop hl
	jr MoveToFldEnd
MoveToFldGo:
		pop hl
	ld a,(CurFldr)
	ld (hl),a
	ld hl,0
	ld (PasteWord),hl

	ld hl,AVOff_FolderDirty						;set FldSv dirty flag
	call DAVLCheckOffset
	ld (hl),1

	jr MoveToFldEnd	
MoveToFldSelect:
	ld h,(ix+1)
	ld l,(ix+0)
	dec hl
	ld (PasteWord),hl
MoveToFldEnd:
;	call FldSave								;unfortunately necessary
	jp RealStartNoReset
	
	;	** FLD SEARCH FUNCTIONS **
	;	0= find current folder's parent
	;	1=
	;	2=
	;	3= init all prgms into fldrs
	;	4=
	;	** FLD SEARCH FUNCTIONS **
	
FldSearch:
	ld d,a
	ld e,b
	xor a
	ld c,a
	ld hl,(progptr)
FldSearchMain:
	push de				;save VFAT loc
		push hl
			ld de,(ptemp)			;first byte after VAT
			or a
			sbc hl,de			;are we there?
			pop hl
		pop de				;recall VFAT loc
	ret z				;end of VAT
	ld a,(hl)
	and $1f
	dec a
	jp z,FldSearchNext
	sub 4
	jr z,FldSearchProg
	dec a
	jr z,FldSearchProg
	jp FldSearchNext
FldSearchProg:
	push hl
	ld a,d
	cp 2
	jr nc,FldSearchProgAny
	push de
	ld de,-7
	add hl,de
	pop de
	ld a,(hl)
	cp '%'
	jr nz,FldSearchProgDone
	dec hl
	ld a,(hl)
	cp 'F'
	jr nz,FldSearchProgDone
FldSearchProgAny:
	pop hl
	push hl
	push de
	ld de,-11
	add hl,de
	pop de
	ld a,(hl)
	pop hl
	push hl
	push af
	ld a,d
	or a
	jr nz,FldSearchProg2
	pop af
	cp c
	jp m,FldSearchProgDone
	ld c,a
	jr FldSearchProgDone
FldSearchProg2:
	dec a
	jr nz,FldSearchProg3
	pop af
	cp e
	jr nz,FldSearchProgDone
	dec hl
	ld c,(hl)
	jr FldSearchProgDone
FldSearchProg3:
	dec a
	jr nz,FldSearchProg4
	pop af
	dec a
	jr z,FldSearchProgDone
	dec hl
	ld (hl),1
	jr FldSearchProgDone
FldSearchProg4:
	dec a
	jr nz,FldSearchProg5
	pop af
	dec a	
	jr z,FldSearchProg4Fld
FldSearchProg4FldR:
	dec hl
	ld a,(hl)
	or a
	jr nz,FldSearchProgDone
	inc a
	ld (hl),a
	jr FldSearchProgDone
FldSearchProg4Fld:
	push hl
		push de
			ld de,-6
			add hl,de
			ld a,(hl)
			pop de
		pop hl
	cp 5
	jr nz,FldSearchProg4FldR
	jr FldSearchProgDone
FldSearchProg5:
	pop af
	dec hl
	ld a,(hl)
FldSearch5L:
	or a
	jr z,FldSearchProgDone
;	push bc
;	ld b,a
;	ld a,(CurFldr)
;	cp b
;	pop bc
	cp e
	jr nz,FldSearchProgDone
	pop hl
	push hl
	dec hl
	ld a,(CurFldr)
	ld (hl),a
FldSearchProgDone:
	pop hl
FldSearchNext:
	dec hl
	dec hl
	dec hl
	dec hl
	dec hl
	dec hl
	ld b,(hl)
	dec hl
FldSearchNextLoop:
	dec hl
	djnz FldSearchNextLoop
	jp FldSearchMain
;----------------------
PropMask:
	.db $f0,$00,$00,$00,$00,$7f