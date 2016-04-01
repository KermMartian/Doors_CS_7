;-----------------------------------------------------------
;	Filename:		cedit.asm
;	Long name:  	Cursor Editor
;	Author:			Chipmaster, with Kerm Martian aka Christopher Mitchell
;	Last Update:	Unknown
;
; Tab functions, mouse hook for GUI, InfoPop and MemoryPop-related
; code, and similar.
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

CursorEdit:
   xor a
   ld (CX),a
   ld (CY),a
   ld (CGStimer),a
   ld hl,40
   bcall(_DAVLCheckOffset)
   ld de,cMouseMask
   ld bc,16
   ldir
   call displayOuterEdge
   
cmainloop:

   ld hl,CGStimer
   inc (hl)
   inc hl
   inc (hl)
   call displayGrid
   call displayCursor
   call imfastcopy
   
   bcall(_Cn2GetK)
   cp 4
   jr nz,notMoveUp
   ld a,(CY)
   or a
   jr z,cmainloop
   dec a
   ld b,1
   jp CCursorMove
notMoveUp:
   cp 1
   jr nz,notMoveDown
   ld a,(CY)
   cp 7
   jr z,cmainloop
   inc a
   ld b,1
   jp CCursorMove
notMoveDown:
   cp 3
   jr nz,notMoveRight
   ld a,(CX)
   cp 7
   jr z,cmainloop
   inc a
   ld b,0
   jp CCursorMove
notMoveRight:
   cp 2
   jr nz,notMoveLeft
   ld a,(CX)
   or a
   jp z,cmainloop
   dec a
   ld b,0
   jp CCursorMove
notMoveLeft:
	ld b,0
	cp 36h						;[2nd]
	jr z,modPixelStatus
	inc b
	cp 30h						;[ALPHA]
	jr z,modPixelStatus
	inc b
	cp 38h						;[DEL]
	jr z,modPixelStatus
notChangePixel:
   cp 9
   ret z
   jp cmainloop
;Calls
   
   
modPixelStatus:
;White Opaque, Black Opaque, White Trans
;If White Opaque:
;-Change cMouse

;If Black Opaque
;-Change cMouse
;-Change cMouseMask

;If White Trans
;-Change cMouseMask
	push bc
;		ld a,b
;		cp 2						;deleting (switch to trans)
;		jr z,modPixelStatusMask
		ld a,(CY)
		ld hl,cMouse
		ld e,a
		ld d,0
		add hl,de
		ld c,%10000000
		ld a,(CX)
		or a
		jr z,cgetpixelLoopFinish
		ld b,a
cgetpixelLoop:
		srl c
		djnz cgetpixelLoop
cgetpixelLoopFinish:			;c is mask, (hl) is current value
		pop af					;a is type
	push af
		or a					;2nd
		jr z,cgetpixelLoopFinish_Black
cgetpixelLoopFinish_White:
		ld a,c
		cpl						;xor $ff
		and (hl)
		ld (hl),a
		jr modPixelStatusMask
cgetpixelLoopFinish_Black:
		ld a,(hl)
		or c
		ld (hl),a		
modPixelStatusMask:
		ld a,(CY)
		ld hl,cMouseMask
		ld e,a
		;d = 0, never got a chance to change
		add hl,de
		ld c,%10000000
		ld a,(CX)
		or a
		jr z,cgetpixelLoop2Finish
		ld b,a
cgetpixelLoop2:
		srl c
		djnz cgetpixelLoop2
cgetpixelLoop2Finish:			;c is mask, (hl) is current value
		pop af					;a is type
	cp 2
	jr z,cgetpixelLoop2Finish_Trans
	ld a,c
	cpl						;xor $ff
	and (hl)
	ld (hl),a
	jr modPixelStatusEnd
cgetpixelLoop2Finish_Trans:
	ld a,(hl)
	or c
	ld (hl),a		
modPixelStatusEnd:
	jp cmainLoop

;=====================================================================
displayOuterEdge:
;displays 4 black lines surrounding our grid.
   ld hl,39*256+23   ;(23,39)   (23,56)
   push hl         ;-------------
   ld d,56         ;|           |
   ld e,l         ;|           |
   push de         ;|           |
   ld a,1         ;|           |
   call mos_fastline   ;-------------
   pop de         ;(40,39)   (40,56)
   ld hl,56*256+40
   push hl
   ld a,1
   call mos_fastline   ;doesn't say what's destroyed.  Assume all.
   pop hl
   ld de,39*256+40
   push de
   ld a,1
   call mos_fastline
   pop de
   pop hl
   ld a,1
   call mos_fastline
   ret
   
;=====================================================================
displayGrid:
;Displays the current state of pixels
   ld b,8
   ld hl,cMouse-1
   ld e,22
GridOuterLoop:
   push bc
   inc hl
   ld a,38            ;2 less than what it will display first at
   inc e
   inc e
   ld b,%10000000
GridInnerLoop:
   push bc
   push hl
   push de
   push af
   ld a,(hl)
   and b
   jr z,GridDisplayBlank
GridBlack:
   pop af
   pop de
   inc a
   inc a
   push de
   push af
   ld l,e
   ld b,2
   ld ix,Block
   call imPutSpriteMask
   jr DisplayGridJoin
GridDisplayBlank:
   ld de,8
   or a
   sbc hl,de
   ld a,(hl)
   and b
   jr nz,GridDisplayGrey
GridDisplayBlank2:
   pop af
   pop de
   inc a
   inc a
   push de
   push af
   ld l,e
   ld b,2
   ld ix,Blank
   call imPutSpriteMask
DisplayGridJoin:
   pop af
   pop de
   pop hl
   pop bc
   srl b
   jr nz,GridInnerLoop
   pop bc
   djnz GridOuterLoop
   ret
GridDisplayGrey:
   ld a,(CGStimer)
   srl a
   jr c,GridDisplayBlank2
   jr GridBlack
  
Block:
   .db %00111111
   .db %00111111
Blank:
   .db %00111111
   .db %00111111
Dummy:
   .db 0,0,0,0,      ;4 bytes of crap
BlockMask:
   .db %11000000
   .db %11000000
BlankMask:
   .db %00000000
   .db %00000000
   
   
displayCursor:
;flashes cursor at X,Y
   ld a,(cblinktmr)
   cp 20
   jr nc,CursorcblinkOff
CursorOn:
   ld ix,Block
displayCursorBack:
   ld a,(CY)
   add a,a
   add a,24
   ld l,a
   ld a,(CX)
   add a,a
   add a,40
   ld b,2
   call imPutSpriteMask
   ret
CursorcblinkOff:
   cp 40
   call nc,resetcblinktmr
CursorOff:
   ld ix,Blank
   jr displayCursorBack
resetcblinktmr:
   xor a
   ld (cblinktmr),a
   ret
   
   
CCursorMove:
;Makes it blank at last spot
;Makes it solid at next spot
;b = 0 lr, b = 1 ud
   push af
   push bc
   xor a
   ld (cblinktmr),a
   call CursorOff
   pop bc
   ld a,b
   or a
   jr z,CCursorMoveLeftRight
   pop af
   ld (CY),a
   call CursorOn
   jp cmainloop
CCursorMoveLeftRight:
   pop af
   ld (CX),a
   call CursorOn
   jp cmainloop