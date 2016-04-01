;-----------------------------------------------------------
;	Filename:		ap.asm
;	Long name:  	Associated program type detection
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	Unknown
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

;
;	bcd = type
;	returns z set for failure or nz set for sucess
;
DCSAPGetType:
	ld e,b
	;now it's ecd - that frees a, b, hl, ix
	ld hl,(ProgPtr)
DCSAPGetTypeLoop:
	push de						;the routine itself push/pop's hl
		ld de,(ptemp)
		bcall(_mos_cphlde)
		pop de
	ret z
	ld a,(hl)
	and $1f
	cp 05
	jr z,DCSAPGetTypeProg
	cp 06
	jr z,DCSAPGetTypeProg
DCSAPGetTypeCont:
	dec hl
	dec hl
	dec hl
	dec hl
	dec hl
	dec hl
	ld b,(hl)
	dec hl
DCSAPGetTypeCLoop:
	dec hl
	djnz DCSAPGetTypeCLoop
	jr DCSAPGetTypeLoop
DCSAPGetTypeProg:
	push hl
	push de
	dec hl
	dec hl
	dec hl
	ld e,(hl)
	dec hl
	ld d,(hl)
	dec hl
	ld a,(hl)
	ld (CurROMPage),a
	ex de,hl
	call SetUpROM
	ld de,18
	add hl,de
	call GetArcProgByte			; <--ready to load the 31,7F,n
	cp $31
	jr nz,DCSAPGetTypeProgDone
	inc hl
	call GetArcProgByte			; <--ready to load the 31,7F,n
	cp $7F
	jr nz,DCSAPGetTypeProgDone
	;here=masterprog
	inc hl
	call GetArcProgByte
	ld b,a
	inc hl
DCSAPGetTypeProgCheck:
	pop de
	push de
	push hl
	call GetArcProgByte
	inc hl
	cp e
	jr nz,DCSAPGetTypeProgCheck1
	call GetArcProgByte
	inc hl
	cp c
	jr nz,DCSAPGetTypeProgCheck1
	call GetArcProgByte
	cp d
	jr z,DCSAPGetTypeFound
DCSAPGetTypeProgCheck1:
	pop hl
	inc hl
	inc hl
	inc hl
	djnz DCSAPGetTypeProgCheck
DCSAPGetTypeProgDone:
	pop de
	pop hl
	jr DCSAPGetTypeCont
DCSAPGetTypeFound:
	ld hl,Op1
	ld (hl),0
	push hl
	pop de
	inc de
	ld bc,8
	ldir
	pop hl
	pop de
	pop hl
	ld de,-6
	add hl,de
	ld b,(hl)
	ld de,Op1
	ex de,hl
	ld (hl),5
	;inc de
DCSAPGetTypeFound1:
	inc hl
	dec de
	ld a,(de)
	ld (hl),a
	djnz DCSAPGetTypeFound1
	xor a
	cp 1
	ret

APGui_gui7ToTop:
	;NEED TO move gui7 AppVar to high RAM
	ld hl,GUIavNamep0
	rst 20h
	bcall(_ChkFindSym)
	ret c
	push de
		ld de,-8
		add hl,de								;this puts it at the "u" in "gui7"
		inc (hl)
		;ld hl,1
		ld hl,0
		bcall(_CreateAppVar)
		pop hl
	;ex de,hl
	
	push hl
		push de
			ld c,(hl)									;move the size bytes from hl (gvi7) to de (gui7)
			inc hl
			inc de
			ld b,(hl)
			inc hl
			inc de
			push bc
				push hl										;hl = from, de = to, bc = size
					ld hl,300h
					push hl				;1
						sbc hl,bc			;1					;more than 300 bytes to move?
						jr c,gui7_relocate_gt768			;2
						pop hl				;1
					push bc				;1
gui7_relocate_gt768:											;now it's either 768 or 768-size
						pop bc
					pop hl
				ex de,hl
				or a
				sbc hl,bc									;move hl (which is really de) down by this much
				ex de,hl
				pop bc
			push bc
			;	dec bc
				ld a,2										;swap from hl to de using 768 bytes at ix
				ld ix,gbuf									;(a=1 is swap from de to hl, but high ram to low)
				bcall(_hook1_1)								;(we need to swap from low ram to high ram)
				bcall(_grbufclr)
				pop bc
			pop hl
		pop de
	or a
	sbc hl,bc
	ex de,hl												;de is now bc bytes lower in memory
	xor a
	ld (hl),a
	inc hl
	ld (hl),a
	ex de,hl
	ld (hl),c
	inc hl
	ld (hl),b
	ld hl,GUIavNamep0
	rst 20h
	ld hl,Op1+2
	inc (hl)
	bcall(_ChkFindSym)
	bcallnc(_Delvar)
	ret
	
