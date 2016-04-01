;-----------------------------------------------------------
;	Filename:		textinr.asm
;	Long name:  	Text Input Routines
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	July 12, 2010
;	Last Update:	July 9, 2006
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------
; bug list
; D6 & scroll down misalignment
; in-between clicking causes crash/ unexpected cursor positioning.


TextIn_Cursor_Start:
	ld a,0ffh				;reset
	out (1),a
	bcall(_Cn2GetK)
	or a
	jr nz,TextIn_Cursor_Start
	;call TextInDebounce
	res AlphaUpFlag,(iy+dcsGUIFlags)
TextIn_Cursor_GetKey:
	res ReDrawnFlag,(iy+dcsGUIFlags)
	call DrawCursorText
	bcall(_Cn2GetK)
	push af
		call DrawCursorText
		pop af
	or a
	jr z,TextIn_Cursor_GetKey
	bit AlphaUpFlag,(iy+dcsGUIFlags)
	call nz,TextInHideAlphaMode
	dec a								;cp $01
	jp z,TextIn_Cursor_Down
	dec a								;cp $02
	jp z,TextIn_Cursor_Left
	dec a								;cp $03
	jp z,TextIn_Cursor_Right
	dec a								;cp $04
	jp z,TextIn_Cursor_Up
	cp $0F-4
	jp z,TextIn_BkspChar
	cp $30-4
	jp z,ToggleAlphaMode
	cp $32-4
	jr z,TextInReturn
	cp $36-4
	jr z,TextInReturn
	cp $38-4
	jp z,TextIn_DelChar
	add a,4
KeyToChar:
	ld c,a
	push bc
		call GetAlphaMode		;puts it into A
		pop bc
	ld hl,UpperCaseTable+7
	or a
	jr z,KeyToCharDetect
	dec a
	jr z,LowerCaseModeSet
	ld hl,NumberCaseTable+7
	jr KeyToCharDetect
TextInDebounce:
	ld b,16
TextInDeDebounce:
	ld a,0ffh				;reset
	out (1),a
	nop \ nop
	ld a,0bfh				;[CLEAR] key set
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	inc a
	jr nz,TextInDeDebounce
	djnz TextInDeDebounce
	ld b,16
TextInDeDebounce3:			;for alpha key
	ld a,0ffh				;reset
	out (1),a
	nop \ nop
	ld a,0dfh				;alpha group
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	inc a
	jr nz,TextInDeDebounce3
	djnz TextInDeDebounce3
	ld a,0ffh				;reset
	out (1),a
	nop \ nop
	ret
TextInHideAlphaMode:
	res AlphaUpFlag,(iy+dcsGUIFlags)
	push af
		ld hl,gbuf+11
		ld de,FileOSSaveAlpha
		ld b,5
HideAlphaModeL:
		ld a,(de)
		ld (hl),a
		push de
			ld de,12
			add hl,de
			pop de
		inc de
		djnz HideAlphaModeL
	pop af
	ret
TextInReturn:
	call TextInDebounce
	bcall(_CloseProg)
	;Need to update the cursor offset!
	ld hl,(GUITextCurOffset)
	ld (Op6),hl
	jp GUIMouseRun
LowerCaseModeSet:
	ld hl,LowerCaseTable+7
KeyToCharDetect:
	ld a,c
	sub 9
	cp 6
	jp p,CharLoopTxt1
	sub 7
	jr CharDoneLoop
CharLooptxt1:
	ld de,7
	add hl,de
	sub 8
	cp 7
	jp p,CharLoopTxt2
	sub 7
	jr CharDoneLoop
CharLoopTxt2:
	ld de,7
	add hl,de
	sub 8
	cp 7
	jp p,CharLoopTxt3
	sub 7
	jr CharDoneLoop
CharLoopTxt3:
	ld de,8
	add hl,de
	sub 8
	cp 8
	jp p,CharLoopTxt4
	sub 8
	jr CharDoneLoop
CharLoopTxt4:
	sub 9
	ld d,0
	ld e,a
	jr CharDoneLoop1
CharDoneLoop:
	ld d,0
	ld e,a
	or a
	jr z,CharDoneLoop1
	jp p,CharDoneLoop1
	ld d,$ff
CharDoneLoop1:
	add hl,de
	ld a,(hl)
	or a
	jr z,InvalidChar
	cp $E1
	jr c,CharDoneLoop1Valid
	ld a,$5B
CharDoneLoop1Valid:
	ld b,a
	ld a,(linein_render_type)
	dec a
	jr z,CharDoneLoopNoCRCheck
	ld a,b
	cp $d6
	jp z,TextIn_Cursor_GetKey
CharDoneLoopNoCRCheck:
	ld a,b
	call GetOffset
	push af
		ex de,hl
		ld hl,(TxtEntrySave)
		ex de,hl				;stack entry start = de; insert location = hl
		push de
			push hl
			
				;call InsertGUIStack
				call InsertGUIStackText
				
				pop hl
			pop de					;stack entry start
		pop af
	ld (hl),a
	inc hl
	cp $d6
	
	;call z,InsertGUIStack
	call z,InsertGUIStackText
	
CharDoneLoopNoCR:
	call TxtRedraw
	call Cursor_IncPos
InvalidChar:
	jp TextIn_Cursor_GetKey
GetOffset:
	ld hl,(GUITextCurOffset)
	ld de,(GUITextScreenOffset)
	add hl,de
	ld de,(GUITextCurStart)
	add hl,de
	ret
ToggleAlphaMode:
	call GetAlphaMode
	inc a
	cp $3
	jr nz,ToggleAlphaModeStore
	xor a
ToggleAlphaModeStore:
	call SetAlphaMode
	call ShowAlphaMode
	set AlphaUpFlag,(iy+dcsGUIFlags)
	jp TextIn_Cursor_GetKey
TxtRedraw:
	;+----------------------------------------------------------------------------------------------
	;| Step 1: Erase the area - perhaps add this to the two rendering routines? Might be a good idea.
	;| Step 2: Draw the single or multiline, perhaps based upon the value of linein_render_type
	;|		  linein_render_type should follow the same conventions here as in guitools.inc
	;+----------------------------------------------------------------------------------------------
	in a,(20h)				;calc speed port
	push af
		in a, (2)
		rla
		sbc a, a
		out (20h), a		;set fast speed
		set reDrawnFlag,(iy+dcsGUIFlags)
		ld hl,(TxtEntrySave)
		push hl
			pop bc
		ld a,(linein_render_type)
		cp 1
		jp z,TxtRedrawMulti
		push bc
			ld hl,(GUITextScreenOffset)
			push hl
				pop de
			ld hl,5
			add hl,bc
			ld (hl),e
			inc hl
			ld (hl),d
		pop bc
		or a
		jr z,TxtRedrawPwd
		ld a,(linein_render_type)
		push af
			call GUIRTextLineIn
			pop af
		ld (linein_render_type),a
		jr TxtRedrawFinish
TxtRedrawPwd:
		ld a,(linein_render_type)
		push af
			call GUIRPassIn
			pop af
		ld (linein_render_type),a
		jr TxtRedrawFinish
TxtRedrawMulti:
		ld a,(linein_render_type)
		push af
			call GUIRTextMultiline
			pop af
		ld (linein_render_type),a
TxtRedrawFinish:
		pop af
	out (20h),a
	ret
TextIn_Cursor_Up:
	ld a,(linein_render_type)
	dec a
	jr nz,TextIn_Cursor_GetKey_UpRet
	ld a,(GUITextCurX)
	ld d,a
	ld a,(GUITextCurY)
	ld b,a
Cursor_Up_Loop:
	ld a,(GUITextCurX)
	ld e,a
	push de
		push bc
			call Cursor_DecPos
			pop bc
		pop de			;d=start, e=last
	ld a,(GUITextCurX)
	cp e			;if same then SOF
	jr z,TextIn_Cursor_GetKey_UpRet
	ld e,a
	cp d
	jr z,TextIn_Cursor_GetKey_UpRet
	jr nc,Cursor_Up_Loop
	bit ReDrawnFlag,(iy+dcsGUIFlags)
	jr nz,TextIn_Cursor_GetKey_UpRet
	ld a,(GUITextCurY)
	cp b
	jr z,Cursor_Up_Loop
TextIn_Cursor_GetKey_UpRet:
	jp TextIn_Cursor_GetKey
TextIn_Cursor_Down:
	ld a,(linein_render_type)
	dec a
	call z,Cursor_IncLine
	jp TextIn_Cursor_GetKey
Cursor_IncLine:
	ld a,(GUITextCurX)
	ld d,a
	ld a,(GUITextCurY)
	ld b,a
Cursor_Down_Loop:
	ld a,(GUITextCurX)
	ld e,a
	push de
		push bc
			call Cursor_IncPos
			pop bc
		pop de
	ld a,(GUITextCurX)
	cp e
	ret z
	ld e,a
	cp d
	ret z
	jr c,Cursor_Down_Loop
	bit ReDrawnFlag,(iy+dcsGUIFlags)											;<--------------------------------------
	ret nz
	ld a,(GUITextCurY)
	cp b
	jr z,Cursor_Down_Loop
	ret
TextIn_Cursor_Left:
	call Cursor_DecPos
	jp TextIn_Cursor_GetKey
Cursor_DecPos:
Cursor_DecPos_StyleRet:
	ld hl,GUITextCurX
	ld a,(hl)
	push hl
		push af
			call GetOffset
			call mos_cphlde
			jp z,EndofFile			;actually at the start, but w/e
			dec hl			;**important** (letter width)
			call mos_cphlde
			jr z,Cursor_DecPos_SkipCRCheck
			dec hl
			ld a,(hl)
			cp $d6
			jr z,Cursor_DecPos_CR
			inc hl
Cursor_DecPos_SkipCRCheck:
			ld a,(hl)
			inc hl
			push hl
				call GetVarWidthP
				pop bc
			ld hl,(GUITextCurOffset)
			dec hl
			ld (GUITextCurOffset),hl
			pop af
		pop hl
	sub d
	cp 1
	jp m,PrevRow
	ld (hl),a
	;dec ix												;<<<<-----------------------------------????????????????????
	ret
Cursor_DecPos_CR:
			pop af
		pop bc
	inc hl
	ld a,(hl)
	ld (GUITextCurX),a
	dec hl
	ld bc,0
	cp 1
	jr z,Cursor_DecPos_CRLoop2
Cursor_DecPos_CRLoop:
	dec hl
	push af
		ld a,(hl)
;		push hl
;			push bc
				call GetVarWidthP
;				pop bc
;			pop hl
		pop af
	dec bc
	sub d
	cp 1
	jr nz,Cursor_DecPos_CRLoop
Cursor_DecPos_CRLoop2:
	ld hl,(GUITextCurOffset)
	dec hl
	dec hl
;	dec hl
	ld (GUITextCurOffset),hl
	
	ld a,(linein_render_type)
	dec a
	jp nz,TxtRedraw						;forget moving up a line unless we're in multiline mode
	ld a,(GUITextCurY)
	cp 1						;changed from 17
	jr z,PageUpCursorDecPosA
	sub 6
	ld (GUITextCurY),a
	ret
PageUpCursorDecPosA:
	ld hl,0
	or a
	sbc hl,bc
	ld (GUITextCurOffset),hl
	ld hl,(GUITextScreenOffset)					;????????????????????????????????
	add hl,bc
	dec hl
	dec hl
	jp PageUpCursorDecPos2

StartofFile:
		pop de
	ld (hl),a
	ret
PrevRow:
	ld a,(linein_render_type)
	dec a
	jr z,PrevRowMultiline
	ld hl,(GUITextCurOffset)
	inc hl
	ld (GUITextCurOffset),hl
PrevRowRepeat:
	call GetOffset
	ld a,(hl)
;	or a
;	ret z
	call GetVarWidthP
	ld a,(GUITextCurX)
	sub d
	push af
		ld hl,(GUITextCurStart)
		ld de,(GUITextScreenOffset)
		add hl,de
		ld a,(hl)
		call GetVarWidthP
		pop af
	add a,d
	ld (GUITextCurX),a
	ld hl,(GUITextScreenOffset)
	dec hl
	ld (GUITextScreenOffset),hl
	cp 1
	jp m,PrevRowRepeat
	jp TxtRedraw
GetVarWidthP:
	push hl
		push bc
			call GetVarWidth
			pop bc
		pop hl
	ld a,(linein_render_type)
	or a
	ret nz
	ld d,4
	ret
PrevRowMultiline:
	push de
		push hl
			push bc
				pop hl
;			dec hl						;hl gets trashed in GetOffset; this doesn't make sense
			call GetOffset
			call mos_cphlde
			pop hl
		jr z,StartofFile
		push bc
			pop hl
		dec hl			;[X]? already done above (see **..**)
		call FindLineEnd
		pop de
	ld (GUITextCurX),a
Cursor_DecPosRet:
	ld a,(linein_render_type)
	dec a
	jp nz,TxtRedraw						;forget moving up a line unless we're in multiline mode
	ld a,(GUITextCurY)
	cp 1						;changed from 17
	jr z,PageUpCursorDecPos
	sub 6
	ld (GUITextCurY),a
	ret
PageUpCursorDecPos:
	push ix
		pop de
	or a
	sbc hl,de
	ld (GUITextCurOffset),hl
	ld hl,(GUITextCurStart)
	ex de,hl
	or a
	sbc hl,de
PageUpCursorDecPos2:
	ld (GUITextScreenOffset),hl
	ex de,hl
	ld hl,(TxtEntrySave)
	ld bc,4
	add hl,bc
	ld (hl),e
	inc hl
	ld (hl),d
	jp TxtReDraw
TextIn_Cursor_Right:
	call Cursor_IncPos
	jp TextIn_Cursor_GetKey
Cursor_IncPos:
Cursor_IncPos_StyleRet:
	ld hl,GUITextCurX
	ld a,(hl)
	push hl
		push af
			call GetOffset
			ld a,(hl)
			or a
			jr z,EndofFile
			ld hl,(GUITextCurOffset)
			inc hl
			ld (GUITextCurOffset),hl
			cp $d6
			jr z,Cursor_IncPos_CR
			call GetVarWidthP
			ld a,(GUITextWidthPx)
			ld b,a
			pop af
		pop hl
	add a,d
	inc b
	inc b
	inc b
	cp b
	jr nc,NextRow
	ld (hl),a
	;inc ix
	ret
EndofFile:
	pop hl
	pop af
	ret
Cursor_IncPos_CR:
	inc hl
	ld (GUITextCurOffset),hl
	pop af
	pop hl
	ld d,0
NextRow:
	ld a,(linein_render_type)
	dec a
	jr z,NextRowMultiline

	call GetOffset
	dec hl
	ld a,(hl)
	call GetVarWidthP
	ld a,(GUITextCurX)
	add a,d
	ld (GUITextCurX),a
	ld hl,(GUITextCurStart)
	ld de,(GUITextScreenOffset)
	add hl,de
NextRowRepeat:
	;do this until GUITextCurX is less than the width
	ld a,(GUITextCurX)
	ld b,a
	ld a,(GUITextWidthPx)
	cp b
	jr nc,NextRowDone
	ld a,(hl)
	inc hl
	push hl
		call GetVarWidthP
		ld a,(GUITextCurX)
		sub d
		ld (GUITextCurX),a
		ld hl,(GUITextScreenOffset)
		inc hl
		ld (GUITextScreenOffset),hl
		ld hl,(GUITextCurOffset)
		dec hl
		ld (GUITextCurOffset),hl
		pop hl
	jr NextRowRepeat
NextRowDone:
	jp TxtRedraw
NextRowMultiline:
	ld a,1
	add a,d
	ld (hl),a
	inc hl
	ld a,(GUITextRows)
	add a,a
	ld b,a
	add a,a
	add a,b
	sub 5
	ld b,a
	ld a,(hl)
	cp b
	jr nc,LineDownCursorIncPos
	add a,6
	ld (hl),a
	ret
LineDownCursorIncPos:
	ld hl,(GUITextCurStart)
	ld de,(GUITextScreenOffset)
	add hl,de
	ld a,1
LineDownCursorIncPos1:
	push af
		push hl
			ld a,(hl)
			cp $d6
			jr z,LineDownCIP_CR
			call GetVarWidthP
			ld a,(GUITextWidthPx)
			ld b,a
			pop hl
		pop af
	add a,d
	inc b
	inc b
	inc b
	cp b
	jr nc,FoundLineDownNext
	inc hl
	jr LineDownCursorIncPos1
LineDownCIP_CR:
	pop bc
	pop bc
	inc hl
	inc hl
;	ld a,(hl)
;	dec hl
FoundLineDownNext:
	ex de,hl
	ld hl,(GUITextScreenOffset)
	push hl
		ex de,hl
		ld de,(GUITextCurStart)
		or a
		sbc hl,de
		ld (GUITextScreenOffset),hl
		push hl
			ld hl,(TxtEntrySave)
			ld de,4
			add hl,de
			pop de
		ld (hl),e
		inc hl
		ld (hl),d
		ex de,hl
		pop de
	or a
	sbc hl,de
	ld de,(GUITextCurOffset)
	ex de,hl
	sbc hl,de
	ld (GUITextCurOffset),hl
	jp TxtReDraw
TextIn_BkspChar:
	call GetOffset
	call mos_cphlde
	jp z,TextIn_Cursor_Getkey
	call Cursor_DecPos
	;jp TextIn_DelChar
TextIn_DelChar:
	call GetOffset
	ld a,(hl)
	or a
	jr z,TextIn_DelChar1
	ex de,hl
	ld hl,(TxtEntrySave)
	ex de,hl					;de = stack entry start; hl = insert location
	push hl
		push de
			push af

				;call DeleteGUIStack
				call DeleteGUIStackText

				pop af
			pop de
		pop hl
	cp $d6

	;call z,DeleteGUIStack
	call z,DeleteGUIStackText
	
;	call GetOffset
;	ld a,(hl)
;	or a
;	jr nz,TextIn_DelChar_Done
;	ld a,(GUITextCurX)
;	dec a
;	call z,Cursor_DecPos
;TextIn_DelChar_Done:
	call TxtRedraw
TextIn_DelChar1:
	jp TextIn_Cursor_GetKey
ShowAlphaMode:
	ld hl,gbuf+11
	ld de,FileOSSaveAlpha
	ld b,5
ShowAlphaModeL:
	ld a,(hl)
	ld (de),a
	and %11000001
	ld (hl),a
	push de
		ld de,12
		add hl,de
		pop de
	inc de
	djnz ShowAlphaModeL

	call GetAlphaMode
	add a,a
	add a,a
	ld e,a
	ld d,0
	ld hl,AlphaModeUpper
	add hl,de
	push hl
	pop ix
	ld b,4
	ld a,91
	ld l,0
	call imPutSprite
	jp imFastCopy
GetAlphaMode:
	push bc
		push de
			bcall(_DAVLCheck)
			ld de,32
			add hl,de
			ld a,(hl)
			pop de
		pop bc
	ret
SetAlphaMode:
	push af
		call GetAlphaMode
		pop af
	ld (hl),a
	ret

FindLineEnd:
;	ld a,(GUITextWidthPx)
;	ld (FileOSSaveAlpha),a
	push hl
		ld de,(GUITextCurStart)
		dec hl
		call mos_cphlde
		jr z,FoundParaBegin
		ld a,(hl)
;		inc hl
		cp $d6
		jr nz,FindingParaBegin
		pop hl
	dec hl
	push hl
FindingParaBegin:
		call mos_cphlde
		jr z,FoundParaBegin
		dec hl
		ld a,(hl)
		cp $d6
		jr nz,FindingParaBegin
FoundParaBeginCR:
		inc hl
		inc hl
FoundParaBegin:
		push hl
			pop ix
		;hl = beginning of paragraph
		;(top stack entry) = char to step back to
		ld a,(GUITextWidthPx)
		add a,3								;was inc b \ inc b \ inc b
		ld b,a
		ld a,1
FindingLineEnd:
		pop de
	call mos_cphlde
	jr z,FoundLineEnd
	push de
		push af
			ld a,(hl)
			call GetVarWidthP
			inc hl
			pop af
		add a,d
		cp b
		jr c,FindingLineEnd
FindingLineEnd_NextRow:
		ld a,d
		inc a
		push hl
			pop ix
		dec ix
		jr FindingLineEnd
FoundLineEnd:
;	ld b,a
;	ld a,(hl)
;	call GetVarWidthP
;	ld a,b
;	sub d
	ret

AlphaModeUpper:
	.db %01000000
	.db %10100000
	.db %11100000
	.db %10100000
AlphaModeLower:
	.db %01100000
	.db %10100000
	.db %10100000
	.db %01100000
AlphaModeNumber:
	.db %01000000
	.db %11000000
	.db %01000000
	.db %11100000
;------Uppercase-Table-----------
UppercaseTable:
	.db $D6,$22,"WRMH",0
	.db "?",$5B,"VQLGA"		;theta
	.db ":ZUPKFC"
	.db " YTOJEBA"
	.db  "XSNIDA"
;------Lowercase-Table-----------
LowercaseTable:
	.db $D6,"'wrmh",0
	.db    "-",$18,"vqlga"
	.db    ".zupkfc"
	.db    " ytojeba"
	.db     "xsnida"
;------Number-Table-----------
NumbercaseTable:
	.db $D6,$2b,$2d,$09,$2F,$5E,0
	.db $1A,"369",$29,$26,$5F
	.db    ".258",$28,$24,$25
	.db    "0147",$2C,$21,$23,"="
	.db $05,$C1,$5D,$B7,$B6,$D0
