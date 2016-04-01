;-----------------------------------------------------------
;	Filename:		easteregg.asm
;	Long name:  	Self-explanatory.
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	Unknown
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

easteregg:
	ld hl,QuadPrgmStr
	rst 20h
	bcall(_chkfindsym)
	ret c
	ld hl,gbuf
	push hl
	pop de
	inc de
	ld (hl),$ff
	ld bc,767
	ldir
	ld a,44
	ld l,6
	ld b,35
	ld c,1
	ld ix,ee_em
	call iLargeSprite

	ld hl,(256*43)+1
	ld (pencol),hl
	ld hl,ee_txt1
	call vputsapp
	ld hl,(256*49)+1
	ld (pencol),hl
	ld hl,ee_txt2
	call vputsapp
	ld hl,(256*55)+1
	ld (pencol),hl
	ld hl,ee_txt3
	call vputsapp
#ifdef false
	ld a,0
	ld l,46
	ld b,14
	ld c,12
	ld ix,ee_txt
	call iLargeSprite
#endif
	call iFastcopy
	
wait:
	ld a,$fd
	out (1),a
	nop
	nop
	in a,(1)
	cp $fe
	ret z
	cp $bf
	jr nz,wait
	
	bcall(_grbufclr)
	ld hl,$0000
	ld de,gbuf
	ld bc,$00A6											;playToneEnd-playTone
	push hl
		push bc
			ldir
			pop bc
		pop hl
	ld de,gbuf+768-$00A6								;(playToneEnd-playTone)
	ldir

	ld hl,(20*256)+2
	ld (pencol),hl
	ld hl,creditstring1
	call vputsapp
	ld hl,(26*256)+2
	ld (pencol),hl
	ld hl,creditstring2
	call vputsapp
	ld hl,creditstring3
songloopout:
;	ld ix,eastereggsong
songloop:
	push hl
		ld hl,gbuf+36*12
		push hl
			pop de
		inc de
		ld bc,12*5-1
		ld (hl),$00
		ldir
	pop hl
	xor a
	ld (pencol),a
	ld a,35
	ld (penrow),a
	push hl
		call vputsapp
		call ifastcopy
		;now the actual easter egg itself!!
;		ld h,(ix)
;		inc ix
;		ld l,(ix)
;		inc ix
;		ld d,(ix)
;		inc ix
;		ld e,(ix)
;		inc ix
;		ld c,(ix)
;		inc ix
;		ld b,(ix)
;		inc ix
;		push ix
;			call gbuf
;			pop ix

		ld hl,$a000
EEWasteLoop:
		dec hl
		ld a,h
		or l
		jr nz,EEWasteLoop

		ld a,$BF
		out (1),a
		nop \ nop
		in a,(1)
		cp $df
		jr z,eequit
;		push ix
;			pop hl
;		ld de,eastereggsongend
;		bcall(_cphlde)
	pop hl
;	jr nz,songloopctd
;	ld ix,eastereggsongrestart
;songloopctd:
	inc hl
	ld a,(hl)
	or a
	jr nz,songloop
	ld hl,creditstring3
	jr songloop
eequit:
	pop hl
	ret
ee_em:
	.db $F8,$F8,$F8,$F8,$F8,$F8,$F8,$F8,$F8,$F8,$F8,$F8,$F8,$F8,$F0,$78
	.db $F8,$F0,$78,$F0,$70,$78,$F0,$70,$70,$00,$00,$00,$00,$00,$F8,$F8
	.db $F8,$F8,$F8

ee_txt1:
	.db "Doors CS 7 has encountered",0
ee_txt2:
	.db "a fatal error. ",$C1,"ENTER] to",0
ee_txt3:
	.db "resume or ",$C1,"CLEAR] to crash.",0
#ifdef false
ee_txt:
	.db $18,$00,$00,$E6,$71,$00,$00,$00,$00,$08,$00,$00,$15,$DD,$98,$84
	.db $11,$8C,$CE,$CE,$EA,$CD,$DB,$B0,$15,$55,$10,$84,$11,$54,$8C,$A8
	.db $AA,$A9,$93,$28,$19,$DD,$30,$EC,$11,$4D,$8E,$AE,$EE,$A5,$D3,$B0
	.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$41,$02,$00
	.db $00,$06,$E0,$80,$32,$00,$00,$00,$0C,$8D,$9A,$76,$DD,$84,$CC,$DD
	.db $93,$70,$00,$00,$14,$D5,$2A,$64,$95,$04,$8A,$99,$12,$50,$00,$00
	.db $0C,$8C,$9A,$74,$9D,$26,$EA,$5D,$31,$70,$00,$00,$00,$00,$00,$00
	.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1B,$A0,$01,$90
	.db $00,$00,$20,$00,$1B,$9A,$BB,$9D,$92,$2E,$6C,$9B,$9D,$99,$B0,$00
	.db $13,$12,$BB,$15,$12,$2C,$A8,$92,$91,$29,$28,$00,$13,$B3,$AB,$9D
	.db $1B,$AE,$69,$8B,$9D,$1B,$2A,$00
#endif
creditstring1:
	.db "Christopher Mitchell would",0
creditstring2:
	.db "like to thank:",0
creditstring3:
	.db $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
	.db "BrandonW",$0c
	.db "TI-Freak8x",$0c
	.db "Ben Ryves",$0c
	.db "Iambian",$0c
	.db "Jim e",$0c
	.db "Joe W",$0c
	.db "DWedit",$0c
	.db "Timendus",$0c
	.db "Tr1p1ea",$0c
	.db "Many others! See http://dcs.cemetech.net"
	.db 0
creditstring4:

QuadPrgmStr:
.db 5,"QUAD",0
;tempo = 175
;eastereggsong:
;	note(bb2,rest,bb2,rest,6000/tempo)
;eastereggsongrestart:
;	note(rest,d2,rest,d2,6000/tempo)
;eastereggsongend: