;-----------------------------------------------------------
;	Filename:		intro.asm
;	Long name:  	Intro Screen Routine
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	June 2, 2006
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

minContrast = 10d
Intro:
	ld a,($8447)
	push af
	ld a,minContrast
	ld ($8447),a
	add a,23d
	or $c0
	bcall(_lcd_busy)
	out (LCDINSTPORT),a
	bcall(_grbufclr)
	ld hl,IntroImageData
	ld de,gbuf+(12*16)
	ld bc,12*21
	ldir
	call iFastCopy
	pop bc
	ld c,minContrast
IntroContrast:
	ld a,c
	ld ($8447),a
	add a,23d
	or $c0
	bcall(_lcd_busy)
	out (LCDINSTPORT),a
	ld hl,$3000
	call IntroPause
	ld a,c
	inc c
	cp b
	jr nz,IntroContrast
	push bc
	ld b,3
IntroWait:
	ld hl,$0000
	call IntroPause
	djnz IntroWait
	pop bc
IntroContrastOut:
	ld a,c
	ld ($8447),a
	add a,23d
	or $c0
	bcall(_lcd_busy)
	out (LCDINSTPORT),a
	ld hl,$3000
	call IntroPause
	ld a,c
	dec c
	cp minContrast
	jr nz,IntroContrastOut
	push bc
	bcall(_clrlcdfull)
	pop bc
	ld a,b
	ld ($8447),a
	add a,23d
	or $c0
	bcall(_lcd_busy)
	out (LCDINSTPORT),a
	ret
IntroPause:
	dec hl
	ld a,h
	or l
	jr nz,IntroPause
	ret
IntroImageData:
 .db $00,$00,$00,$00,$00
 .db $00,$00,$00,$00,$00,$00,$00,$00,$1C,$00,$3F,$FE
 .db $00,$03,$FF,$03,$FE,$00,$78,$00,$3E,$00,$3F,$FF
 .db $C0,$0F,$FF,$8F,$FE,$03,$F8,$00,$7F,$00,$07,$01
 .db $F0,$3C,$03,$9E,$0E,$07,$C0,$00,$FF,$80,$07,$00
 .db $F0,$38,$03,$9C,$0E,$0F,$00,$01,$FF,$C0,$07,$00
 .db $78,$70,$03,$9C,$0E,$1E,$00,$03,$F3,$E0,$07,$00
 .db $3C,$F0,$03,$9C,$06,$1C,$00,$07,$E1,$F0,$07,$00
 .db $3C,$E0,$00,$1E,$00,$3C,$00,$0F,$C1,$F8,$07,$00
 .db $3D,$E0,$00,$1F,$00,$38,$00,$1F,$80,$FC,$07,$00
 .db $1D,$E0,$00,$1F,$E0,$7F,$F0,$3F,$1C,$7E,$07,$00
 .db $1F,$E0,$00,$0F,$F0,$7F,$F8,$3E,$3E,$3E,$07,$00
 .db $1F,$E0,$00,$07,$FC,$78,$7C,$1C,$7F,$1C,$07,$00
 .db $1F,$E0,$00,$01,$FE,$78,$3C,$00,$FF,$80,$07,$00
 .db $1D,$E0,$00,$00,$3E,$70,$1E,$01,$FF,$C0,$07,$00
 .db $1D,$E0,$00,$00,$1F,$70,$1E,$03,$F7,$E0,$07,$00
 .db $1C,$F0,$00,$00,$0F,$70,$1E,$07,$E3,$F0,$07,$00
 .db $3C,$F0,$03,$98,$0F,$78,$1E,$0F,$C1,$F8,$07,$00
 .db $78,$78,$03,$98,$0F,$78,$1C,$1F,$80,$FC,$07,$00
 .db $F0,$7C,$03,$98,$0E,$78,$3C,$3F,$00,$7E,$07,$03
 .db $F0,$3F,$03,$98,$1E,$3C,$3C,$3E,$00,$3E,$3F,$FF
 .db $C0,$0F,$FF,$9F,$FC,$1F,$F8,$1C,$00,$1C,$3F,$FE
 .db $00,$03,$FF,$1F,$F0,$0F,$E0,$00,$00,$00,$00,$00
 .db $00,$00,$00,$00,$00,$00,$00
 