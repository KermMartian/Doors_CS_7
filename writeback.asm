;-----------------------------------------------------------
;	Filename:		Writeback.asm
;	Long name:  	Intelligent Writeback routines
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	May 30, 2007
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

asmcheckwriteback:
	call GetArcStatus
	or a
	ret z			;if it takes the jump original was unarchived
							;otherwise, continue with the comparison
	call GetProgChainTop		;A1
	ld de,10+1
	add hl,de					;B2
	rst 20h
	bcall(_chkfindsym)
	ret c						;eet ees no more, sire!
	ex de,hl
	push hl						;location in ROM on page CurROMPage, start of size word
	
		bcall(_ldhlind)
		push hl						;total size

			bcall(_zeroop1)
			call GetProgChainTop		;A1
			inc hl						;B1
			rst 20h
			bcall(_chkfindsym)
			jp c,asmcheckwritebackfail
			ld a,b
			ld (CurROMPage),a
			ex de,hl
			call SetUpROM
			inc hl
			inc hl
			pop bc
		pop de
	inc de
	inc de
ASMWriteBackCPLoop:
	push bc
		call GetArcProgByte
		ld b,a
		ld a,(de)
		cp b
		pop bc
	jr nz,ASMWriteback
	inc hl
	inc de
	dec bc
	ld a,(CurROMPage)
	or a
	jr z,WriteBackNoPageInc
	bit 7,h
	jr z,WriteBackNoPageInc
	inc a
	res 7,h
	set 6,h
	ld (CurROMPage),a
WriteBackNoPageInc:
	ld a,b
	or c
	jr nz,ASMWriteBackCPLoop
	jr ASMNoWriteback
ASMWriteback:
	call GetProgChainTop		;A1
	inc hl						;B1
	rst 20h
	bcall(_chkfindsym)
	bcall(_delvararc)
	call GetProgChainTop		;A1
	inc hl						;B1
	push hl
		ld de,10
		add hl,de				;B2
		rst 20h
		bcall(_chkfindsym)
		ld de,-6
		add hl,de
		ld b,(hl)				;name length
		dec hl
		pop de
	push de
		inc de
ASMWriteBackNLoop:
		ld a,(de)
		ld (hl),a
		inc de
		dec hl
		djnz ASMWriteBackNLoop

		pop hl
	rst 20h
	bcall(_chkfindsym)
	call Arc_Unarc

ASMNoWriteback:				;no writeback necessary zomg yay!
	call GetProgChainTop
	ld de,10+1
	add hl,de				;A1->B1->B2
	rst 20h
	bcall(_chkfindsym)
	bcallnc(_delvar)
	ret
asmcheckwritebackfail:
			pop de
		pop bc
	ret
