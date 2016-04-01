;-----------------------------------------------------------
;	Filename:		editor.asm
;	Long name:  	Enhanced TI-BASIC Editor
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	Unknown
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

.nolist
kEnter			equ 005h
kPrgmCr			equ 047h 			;PROGRAM CREATE
StrngObj		equ 4
_Mon			equ 401Eh 			;system monitor, customized through the context vectors
_newContext		equ 4030h 			;(kbdKey)=0, loads context, restores page in 4000h-7fffh
_cxMain			equ 4045h
_BufRight		equ 4906h
_BufLeft		equ 4903h
_IsAtBtm		equ 4933h
_scrollUp		equ 4534h
_BufToTop		equ 4927h
_DispEOW		equ 4957h
hookBackup2      equ AppBackupScreen
;codeLocation    equ AppBackupScreen+5
basic_start		equ 965Bh
basic_pc		equ 965Dh 
basic_end		equ 965Fh

;TI-83 Plus Context Equates
;---------------------------------------------
kPrgmEd			equ 046h ;PROGRAM EDIT
kMode			equ 045h
kFormat			equ 057h ;FORMAT

cxPrgmEdit		equ kPrgmEd 		;program editor
cxMode			equ kMode 			;mode settings screen
cxFormat		equ kFormat 		;FORMAT CONTEXT
.list

EditorBASIC:
	push ix
		pop hl
	call RunNameCopy
	xor a
	ld (EditorMode),a
EditorJumpIn:					;assume we just chkfindsym'ed either from RunNameCopy or before calling EditorJumpIn
	ld a,(hl)					;VAT type byte
	ld (PrevProgType),a			;save previous progtype
	ld (hl),ProgObj				;Make it unlocked
	ld a,b						;side effect of chkfindsym
	ld (CurROMPage),a
	or a
	call nz,Arc_Unarc			
	ld a,(iy+36h)				;TODO
	ld (hookBackup2),a
	ld hl,9BB0h					;TODO
	ld de,hookBackup2+1
	ld bc,4
	ldir
	ld hl,appChangeHook
	ld (9BB0h),hl				;TODO
	in a,(6)
	ld (9BB2h),a				;TODO
contextLoader:
	ld a,5
	ld (Op1),a
	bcall(_pushop1)				;have to push BEFORE opening the edit buffer
	ld hl,Op1+1
	ld de,progToEdit
	ld bc,8
	ldir
	xor a
	ld (de),a
	ld a,kPrgmEd
	bcall(_newContext)
	set 2,(iy+36h)				;TODO
	xor a
	ld (winTop),a
	bcall(_scrollUp)
	bcall(_homeup)
	ld a,':'
	bcall(_putc)
	ld a,(EditorMode)
	or a
	jr z,contextLoaderNoGoto
contextGoto:
	ld hl,(editTop)
	ld de,(editCursor)
	call cphlde
	jr nz,contextLoaderGoto3_1		;Abort, because we don't have the room to handle this
	
	ld bc,(errOffset)				;Found through a LOT of reverse-engineering (was ScratchWord)
	ld a,b
	or c
	jr z,contextLoaderGoto3_1
	ld hl,(editTail)
	;copy bc bytes from (editTail) to (editCursor)
	ldir
	ld (editTail),hl
	ld (editCursor),de
	call editorMovement_BackupToNewline
contextLoaderGoto3_1:
	bcall(_DispEOW)
	ld hl,0100h
	ld (curRow),hl
	jr contextLoaderJumpout
contextLoaderNoGoto:
	bcall(_DispEOW)
	ld hl,0100h
	ld (curRow),hl
	bcall(_BufToTop)
contextLoaderJumpout:
	xor a
	ld (MenuCurrent),a
	call SPSave

	set 7,(iy+28h)					;TODO
	bjump(_Mon)

appChangeHook:
	add a,e
	cp cxPrgmEdit
	ret z
	cp cxMode
	ret z
	cp cxFormat
	ret z

	ld a,3						;TODO: Do we really need this?
	out (10h),a
	ld a,11
	out (3),a
	
	bcall(_CursorOff)
	bcall(_CloseEditEqu)
	bcall(_popop1)				;can't pop until edit buffer is closed
	bcall(_chkfindsym)
	jr c,appChangeHookNoRearc	;in fact, doesn't exist :P
	ld a,(PrevProgType)
	ld (hl),a					;make it protected again, if it was
	ld a,(CurROMPage)
	or a
	jr z,appChangeHookNoRearc
	call Arc_Unarc
appChangeHookNoRearc:
	ld a,(hookBackup2)
	ld (iy+36h),a			;TODO FIX
	ld hl,hookBackup2+1
	ld de,appChangeHookPtr
	ld bc,4
	ldir
	ld hl,$0000				;restore the stack here
	add hl,sp
	push hl
		ld hl,AVOff_SPSave
		call DAVLCheckOffset
		bcall(_ldhlind)
		ex de,hl
		pop hl
	ex de,hl
	or a
	sbc hl,de
EditorRestoreSPLoop:
		pop de
	dec hl
	dec hl					;each stack entry is *TWO* bytes!!!!
	ld a,h
	or l
	jr nz,EditorRestoreSPLoop
	ret

editorMovement_BackupToNewline:
	ld hl,(editCursor)
	ld a,(hl)
	cp $3F
	jr z,editorMovement_BackupToNewline_GoBack
editorMovement_BackupToNewline_Loop:
	ld a,(hl)
	ld de,(editTop)
	or a
	sbc hl,de
	ret z					;we reached the beginning of the program
	add hl,de
	dec hl
	push af
		ld a,(hl)
		bcall(_IsA2ByteTok)
		;call IsA2ByteTok
		pop de
	jr z,editorMovement_BackupToNewline_GoBack
	ld a,d
	cp $3F
	jr z,editorMovement_BackupToNewline_ForwardOne
editorMovement_BackupToNewline_GoBack:
	bcall(_BufLeft)
	ld hl,(editCursor)
	jr editorMovement_BackupToNewline_Loop
editorMovement_BackupToNewline_ForwardOne:
	bcall(_BufRight)
	ret
