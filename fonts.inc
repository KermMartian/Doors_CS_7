;-----------------------------------------------------------
;	Filename:		fonts.inc
;	Long name:  	Font routines
;	Authors:		Kerm Martian aka Christopher Mitchell
;	Last Update:	Unknown
;
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

DGetCharWidth:
	;inputs: a=char
	;destroyed: c
	;outputs: c=width
	push af
		push hl
			push de
				call DGetCharWidthLocFromA
				ld c,a
				pop de
			pop hl
		pop af
	ret

;--------------------------------------------
;input:		a=char
;outputs:	a=width
;			c=raw first byte
;			d = page of entry
;			hl->first byte of entry (c=(hl))
;--------------------------------------------
DGetCharWidthLocFromA:
	ld l,a
	ld h,0
	ld e,l
	ld d,h
	add hl,hl
	add hl,de
	ld de,DCSF_GraphScreen
	add hl,de
	ld c,(hl)
	
	in a,($06)
	ld d,a

	ld a,$f0
	and c
	cp $f0
	jr z,DGetCharWidthLarge
	srl a
	srl a
	srl a
	srl a
	ret

DGetCharWidthLarge:
	ld a,$0f
	and c
	ret

DPutMap:
	or a
	ret z
	push hl
		push bc
			;a = character
			;e = fontset
#ifdef multiplefonts
			ld l,a
			ld h,0
			push hl
				ld hl,FontSetTable
				ld d,0
				add hl,de
				ld c,(hl)
				inc hl
				ld b,(hl)
				ld a,(bc)
				pop de
			push af
				inc a							;account for width byte
				push bc
					bcall(_MultADE)
					pop de
				add hl,de
				inc hl							;the height byte!!!
				pop bc

#else
			call DGetCharWidthLocFromA			;returns hl, c, and a.
#endif
			ld de,TempCharRAM
			push af
				ld b,a
				ld a,(pencol)
				add a,b
				sub 97
				jr c,DPutMap_NotOffEdge
				pop bc
			pop bc
		pop bc
	scf											;it failed
	ret
DPutMap_NotOffEdge:
				ld a,$f0
				and c
				cp $f0
				jr z,DPutMapStraightCopy
				ld b,3
				jr DPutMapInsertCopyUnpack
DPutMapCopyUnpack:
				ld c,(hl)
				ld a,$f0
				and c
				ld (de),a
				inc de
DPutMapInsertCopyUnpack:
				ld a,$0f
				and c
				sla a
				sla a
				sla a
				sla a
				ld (de),a
				inc de
				inc hl
				djnz DPutMapCopyUnpack
				jr DPutMap_CheckInv
DPutMapStraightCopy:
				inc hl
				ld a,(hl)
				inc hl
				ld h,(hl)
				ld l,a
				ld bc,5
				ldir
DPutMap_CheckInv:
				pop af
			push af
				bit TextInverse,(iy+textflags)
				jr z,DPutMapNoInv
				ld de,FontInv
				ld h,0
				ld l,a
				add hl,de
				ld c,(hl)
				ld b,5
				ld hl,TempCharRAM
FInvLoop:
				ld a,(hl)
				xor c
				ld (hl),a
				inc hl
				djnz FInvLoop
DPutMapNoInv:
				pop af
			;a = width
			;b = height
			;hl = character's first byte
			ld ix,TempCharRAM
			push af
				ld a,(penrow)
				ld l,a
				inc l
				ld a,(pencol)
				ld b,5
				call iPutSprite
				pop bc
			ld a,(pencol)
			add a,b
			ld (pencol),a
			pop bc
		pop hl
	ret

FontInv:
	.db %00000000
	.db %10000000
	.db %11000000
	.db %11100000
	.db %11110000
	.db %11111000
	.db %11111100
	.db %11111110
	.db %11111111
#ifdef multiplefonts
FontSetTable:
	.dw	DCSF_GraphScreen
	.dw DCSF_3x3
#endif
DCSF_GraphScreen:
#ifdef multiplefonts
	.db 3					;Base char height (IE, width in BYTES!! NOT NIBBLES!!)
#endif
	
	.db $40,$EE,$E0				;square
	.db $f6 \ .dw DCSF_GS_ItalN		;$06,$00,$B0,$48,$48,$90	;italN
	.db $40,$AA,$AE				;u
	.db $40,$AA,$44				;v
	.db $40,$AA,$EA				;w
	.db $48,$CE,$C8				;>
	.db $40,$00,$00				;3-space
	.db $f6 \ .dw DCSF_GS_DownArrow	;$06,$00,$70,$F8,$70,$20	;downarrow
	.db $42,$44,$48				;integral
	.db $40,$A4,$A0				;passX
	.db $40,$0E,$AE				;square
	.db $40,$04,$E4				;cross
	.db $40,$00,$40				;dot
	.db $40,$0E,$44				;littleT
	.db $4E,$42,$C0				;cubed
	.db $5F,$CE,$CC				;boldF
	
	.db $46,$44,$C4				;sqrt
	.db $f5 \ .dw DCSF_GS_Inv			;$05,$30,$10,$D0,$10,$00	;^-1
	.db $4C,$24,$E0				;^2
	.db $40,$24,$8E				;anglesign
	.db $f5 \ .dw DCSF_GS_Deg			;$05,$60,$90,$90,$60,$00	;degreemark
	.db $f5 \ .dw DCSF_GS_Rad			;$05,$B0,$C0,$80,$80,$00	;radianmark
	.db $4E,$44,$00				;upT
	.db $53,$C3,$0F				;lessthanequal
	.db $f6 \ .dw DCSF_GS_Equ			;$06,$10,$F8,$20,$F8,$40	;equalsign
	.db $f5 \ .dw DCSF_GS_Gte			;$05,$C0,$30,$C0,$00,$F0	;greaterthanequal
	.db $40,$60,$00				;upMinus
	.db $4E,$8C,$8E				;bigE
	.db $54,$2F,$24				;rightArrow
	.db $f6 \ .dw DCSF_GS_L10			;$06,$00,$B8,$C8,$C8,$B8	;little10
	.db $44,$E4,$44				;uparrow
	.db $44,$44,$E4				;downarrow
	
	.db $10,$00,$00				;spacebar
	.db $28,$88,$08				;exclMark
	.db $4A,$AA,$00				;double-quote
	.db $f6 \ .dw DCSF_GS_nsg			;$06,$50,$F8,$50,$F8,$50	;numberSign
	.db $f6 \ .dw DCSF_GS_dsg			;$06,$78,$A0,$70,$28,$F0	;dollarsign
	.db $4A,$24,$8A				;percentSign
	.db $f5 \ .dw DCSF_GS_amp			;$05,$40,$A0,$40,$A0,$50	;ampersand
	.db $28,$88,$00				;single-quote
	.db $34,$88,$84				;openParen
	.db $38,$44,$48				;closeParen
	.db $f6 \ .dw DCSF_GS_ast			;asterisk
	.db $40,$4E,$40				;plusSign
	.db $30,$04,$48				;comma
	.db $40,$0E,$00				;minusSign
	.db $20,$00,$08				;period
	.db $42,$24,$88				;forwardSlash
	
	.db $44,$AA,$A4				;zero
	.db $44,$C4,$4E				;one
	.db $4C,$24,$8E				;two
	.db $4C,$24,$2C				;three
	.db $48,$AE,$22				;four
	.db $4E,$8C,$2C				;five
	.db $46,$8E,$AE				;six
	.db $4E,$24,$88				;seven
	.db $4E,$AE,$AE				;eight
	.db $4E,$AE,$2C				;nine
	.db $20,$80,$80				;colon
	.db $30,$40,$48				;semicolon
	.db $42,$48,$42				;lessthan
	.db $40,$E0,$E0				;equalto
	.db $48,$42,$48				;greaterthan
	.db $4C,$24,$04				;qmark
	
	.db $f6 \ .dw DCSF_GS_asg			;$06,$70,$08,$68,$A8,$70	;@sign
	.db $44,$AE,$AA				;A
	.db $4C,$AC,$AC				;B
	.db $46,$88,$86				;C
	.db $4C,$AA,$AC				;D
	.db $4E,$8C,$8E				;E
	.db $4E,$8C,$88				;F
	.db $46,$8A,$A6				;G
	.db $4A,$AE,$AA				;H
	.db $4E,$44,$4E				;I
	.db $42,$22,$AE				;J
	.db $4A,$AC,$AA				;K
	.db $48,$88,$8E				;L
	.db $4A,$EE,$AA				;M
	.db $4C,$AA,$AA				;N
	.db $4E,$AA,$AE				;O
	
	.db $4C,$AC,$88				;P
	.db $4E,$AA,$E6				;Q
	.db $4C,$AC,$AA				;R
	.db $46,$84,$2C				;S
	.db $4E,$44,$44				;T
	.db $4A,$AA,$AE				;U
	.db $4A,$AA,$44				;V
	.db $4A,$AE,$EA				;W
	.db $4A,$A4,$AA				;X
	.db $4A,$A4,$44				;Y
	.db $4E,$24,$8E				;Z
	.db $44,$AE,$A4				;theta
	.db $48,$84,$22				;backslash
	.db $3C,$44,$4C				;closeBracket
	.db $44,$A0,$00				;tothePower
	.db $40,$00,$0E				;underscore
	
	.db $38,$40,$00				;backquote
	.db $40,$6A,$A6				;a
	.db $48,$CA,$AC				;b
	.db $40,$68,$86				;c
	.db $42,$6A,$A6				;d
	.db $40,$4A,$C6				;e
	.db $34,$8C,$88				;f
	.db $46,$A6,$2C				;g
	.db $48,$CA,$AA				;h
	.db $28,$08,$88				;i
	.db $34,$04,$4C				;j
	.db $48,$8A,$CA				;k
	.db $38,$88,$84				;l
	.db $f6 \ .dw DCSF_GS_m			;$06,$00,$D0,$A8,$A8,$88	;m
	.db $40,$CA,$AA				;n
	.db $40,$4A,$A4				;o
	
	.db $40,$CA,$C8				;p
	.db $40,$6A,$62				;q
	.db $40,$AC,$88				;r
	.db $30,$C8,$4C				;s
	.db $38,$C8,$84				;t
	.db $40,$AA,$AE				;u
	.db $40,$AA,$44				;v
	.db $f6 \ .dw DCSF_GS_wl			;$06,$00,$88,$A8,$A8,$50	;w
	.db $40,$A4,$4A				;x
	.db $40,$AA,$48				;y
	.db $f5 \ .dw DCSF_GS_zl			;$05,$00,$F0,$20,$40,$F0	;z
	.db $46,$48,$46				;{
	.db $28,$88,$88				;|
	.db $4C,$42,$4C				;}
	.db $$f5 \ .dw DCSF_GS_tilde		;$05,$00,$50,$A0,$00,$00	;~
	.db 30,$0E,$0E				;-= 

	.db $4E,$AA,$AE				;zero
	.db $44,$C4,$4E				;one
	.db $4C,$24,$8E				;two
	.db $4C,$24,$2C				;three
	.db $48,$AE,$22				;four
	.db $4E,$8C,$2C				;five
	.db $45,$8E,$AE				;six
	.db $4E,$24,$88				;seven
	.db $4E,$AE,$AE				;eight
	.db $4E,$AE,$2C				;nine
	.db $f5 \ .dw DCSF_GS_Aprime		;$05,$20,$60,$90,$F0,$90	;A'
	.db $f5 \ .dw DCSF_GS_Aacute		;$05,$40,$60,$90,$F0,$90	;A`
	.db $f5 \ .dw DCSF_GS_Astar		;$05,$60,$60,$90,$F0,$90	;A*
	.db $f5 \ .dw DCSF_GS_Aumlaut		;$05,$90,$60,$90,$F0,$90	;A:
	.db $f5 \ .dw DCSF_GS_alprime		;$05,$20,$00,$60,$A0,$50	;a'
	.db $f5 \ .dw DCSF_GS_alacute		;$05,$40,$00,$60,$A0,$50	;a`
	
	.db $f5 \ .dw DCSF_GS_alhat		;$05,$40,$A0,$60,$A0,$50	;a^
	.db $f5 \ .dw DCSF_GS_alumlaut		;$05,$A0,$00,$60,$A0,$50	;a:
	.db $f5 \ .dw DCSF_GS_Eprime		;$05,$80,$70,$60,$40,$70	;E'
	.db $f5 \ .dw DCSF_GS_Eacute		;$05,$10,$E0,$C0,$80,$E0	;E`
	.db $44,$0E,$CE   ;E^
	.db $f5 \ .dw DCSF_GS_Eumlaut		;$05,$A0,$E0,$C0,$80,$E0	;E:
	.db $42,$4C,$A6   ;e'
	.db $48,$4C,$A6   ;e`
	.db $44,$4C,$A6   ;e^
	.db $4A,$4C,$A6   ;e:
	.db $42,$E4,$4E   ;I'
	.db $48,$E4,$4E   ;I`
	.db $44,$E4,$4E   ;I^
	.db $4A,$E4,$4E   ;I:
	.db $42,$0C,$4E   ;i'
	.db $48,$0C,$4E   ;i`
 	
	.db $44,$0C,$4E				;i^
	.db $4A,$0C,$4E				;i:
	.db $f6 \ .dw DCSF_GS_Oprime		;$06,$10,$70,$88,$88,$70	;O'
	.db $f6 \ .dw DCSF_GS_Oacute		;$06,$40,$70,$88,$88,$70	;O`
	.db $f6 \ .dw DCSF_GS_Ohat		;$06,$20,$70,$88,$88,$70	;O^
	.db $f6 \ .dw DCSF_GS_Oumlaut		;$06,$50,$70,$88,$88,$70	;O:
	.db $f6 \ .dw DCSF_GS_olprime		;$06,$10,$00,$70,$88,$70	;o'
	.db $f6 \ .dw DCSF_GS_olacute		;$06,$40,$00,$70,$88,$70	;o`
	.db $f6 \ .dw DCSF_GS_olhat		;$06,$20,$00,$70,$88,$70	;o^
	.db $f6 \ .dw DCSF_GS_olumlaut	;$06,$50,$00,$70,$88,$70	;o:
	.db $42,$4A,$AA				;U'
	.db $48,$4A,$AA				;U`
	.db $44,$A0,$AA				;U^
	.db $4A,$0A,$AA				;U:
	.db $f5 \ .dw DCSF_GS_ulprime		;$05,$20,$00,$A0,$A0,$50	;u'
	.db $f5 \ .dw DCSF_GS_ulacute		;$05,$80,$00,$A0,$A0,$50	;u`

	.db $f5 \ .dw DCSF_GS_ulhat		;$05,$40,$00,$A0,$A0,$50	;u^
	.db $f5 \ .dw DCSF_GS_ulumlaut		;$05,$A0,$00,$A0,$A0,$50	;u:
	.db $46,$88,$6C				;C5
	.db $40,$68,$6C				;c5
	.db $f5 \ .dw DCSF_GS_5blank		;$05,$00,$00,$00,$00,$00	;N~
	.db $f5 \ .dw DCSF_GS_5blank		;$05,$00,$00,$00,$00,$00	;n~
	.db $34,$80,$00				;'
	.db $38,$40,$00				;`
	.db $4A,$00,$00				;^:
	.db $44,$04,$86				;^?
	.db $28,$08,$88				;^!
	.db $f5 \ .dw DCSF_GS_alpha		;$05,$00,$50,$A0,$A0,$50	;alpha
	.db $44,$AC,$AC				;beta
	.db $f5 \ .dw DCSF_GS_gamma		;$05,$10,$50,$A0,$20,$20	;gamma
	.db $f6 \ .dw DCSF_GS_delta		;$06,$00,$20,$50,$88,$F8	;delta
	.db $44,$84,$A4   			;?? 
	
	.db $46,$8F,$86				;epsilon
	.db $3C,$88,$8C				;[
	.db $48,$44,$AA				;sigma
	.db $f5 \ .dw DCSF_GS_lmicro		;$05,$00,$A0,$A0,$F0,$80	;micro
	.db $f6 \ .dw DCSF_GS_lpi			;$06,$00,$F8,$50,$50,$50	;lpi
	.db $52,$56,$48				;psi
	.db $5F,$42,$4F				;Sigma
	.db $f5 \ .dw DCSF_GS_lhmmm		;$05,$00,$70,$A0,$A0,$40	;hmmm
	.db $f5 \ .dw DCSF_GS_ltau		;$05,$00,$E0,$40,$50,$20	;tau
	.db $f6 \ .dw DCSF_GS_Psi			;$06,$20,$70,$A8,$70,$20	;Psi
	.db $f6 \ .dw DCSF_GS_Omega		;$06,$70,$88,$88,$50,$D8	;Omega
	.db $4E,$0A,$4A				;x-bar
	.db $4E,$0A,$48				;y-bar
	.db $4A,$4A,$00				;^x
	.db $40,$00,$0E				;underscore
	.db $42,$6E,$62				;<| 
	
	.db $30,$CC,$00
	.db $42,$24,88
	.db $30,$0C,$00
	.db $4C,$24,$E0
	.db $44,$A4,$00
	.db $4E,$42,$C0
	.db $f5 \ .dw DCSF_GS_5blank		;CR
	.db $50,$00,$00
	.db $48,$08,$AC
	.db $44,$C8,$CA
	.db $48,$A4,$A2
	.db $4E,$8E,$88
	.db $40,$4A,$C6
	.db $30,$88,$8E
	.db $3C,$AA,$AA
	.db $4A,$55,$55				;scriptcrampedN
	.db $3C,$8C,$8C				;crampedE
	
	.db $34,$EE,$E4				;formulaDiamond

DCSF_GS_ItalN:		.db $00,$B0,$48,$48,$90	;italN
DCSF_GS_DownArrow:	.db $00,$70,$F8,$70,$20	;downarrow
DCSF_GS_Inv			.db $30,$10,$D0,$10,$00	;^-1
DCSF_GS_Deg			.db $60,$90,$90,$60,$00	;degreemark
DCSF_GS_Rad			.db $B0,$C0,$80,$80,$00	;radianmark
DCSF_GS_Equ			.db $10,$F8,$20,$F8,$40	;equalsign
DCSF_GS_Gte			.db $C0,$30,$C0,$00,$F0	;greaterthanequal
DCSF_GS_L10			.db $00,$B8,$C8,$C8,$B8	;little10
DCSF_GS_nsg			.db $50,$F8,$50,$F8,$50	;numberSign
DCSF_GS_dsg			.db $78,$A0,$70,$28,$F0	;dollarsign
DCSF_GS_amp			.db $40,$A0,$40,$A0,$50	;ampersand
DCSF_GS_ast			.db $20,$A8,$70,$A8,$20	;asterisk
DCSF_GS_asg			.db $70,$08,$68,$A8,$70	;@sign
DCSF_GS_m			.db $00,$D0,$A8,$A8,$88	;m
DCSF_GS_wl			.db $00,$88,$A8,$A8,$50	;w
DCSF_GS_zl			.db $00,$F0,$20,$40,$F0	;z
DCSF_GS_tilde		.db $00,$50,$A0,$00,$00	;~
DCSF_GS_Aprime		.db $20,$60,$90,$F0,$90	;A'
DCSF_GS_Aacute		.db $40,$60,$90,$F0,$90	;A`
DCSF_GS_Astar		.db $60,$60,$90,$F0,$90	;A*
DCSF_GS_Aumlaut		.db $90,$60,$90,$F0,$90	;A:
DCSF_GS_alprime		.db $20,$00,$60,$A0,$50	;a'
DCSF_GS_alacute		.db $40,$00,$60,$A0,$50	;a`
DCSF_GS_alhat		.db $40,$A0,$60,$A0,$50	;a^
DCSF_GS_alumlaut		.db $A0,$00,$60,$A0,$50	;a:
DCSF_GS_Eprime		.db $80,$70,$60,$40,$70	;E'
DCSF_GS_Eacute		.db $10,$E0,$C0,$80,$E0	;E`
DCSF_GS_Eumlaut		.db $A0,$E0,$C0,$80,$E0	;E:
DCSF_GS_Oprime		.db $10,$70,$88,$88,$70	;O'
DCSF_GS_Oacute		.db $40,$70,$88,$88,$70	;O`
DCSF_GS_Ohat		.db $20,$70,$88,$88,$70	;O^
DCSF_GS_Oumlaut		.db $50,$70,$88,$88,$70	;O:
DCSF_GS_olprime		.db $10,$00,$70,$88,$70	;o'
DCSF_GS_olacute		.db $40,$00,$70,$88,$70	;o`
DCSF_GS_olhat		.db $20,$00,$70,$88,$70	;o^
DCSF_GS_olumlaut	.db $50,$00,$70,$88,$70	;o:
DCSF_GS_ulprime		.db $20,$00,$A0,$A0,$50	;u'
DCSF_GS_ulacute		.db $80,$00,$A0,$A0,$50	;u`
DCSF_GS_ulhat		.db $40,$00,$A0,$A0,$50	;u^
DCSF_GS_ulumlaut	.db $A0,$00,$A0,$A0,$50	;u:
DCSF_GS_5blank		.db $00,$00,$00,$00,$00	;N~
DCSF_GS_alpha		.db $00,$50,$A0,$A0,$50	;alpha
DCSF_GS_gamma		.db $10,$50,$A0,$20,$20	;gamma
DCSF_GS_delta		.db $00,$20,$50,$88,$F8	;delta
DCSF_GS_lmicro		.db $00,$A0,$A0,$F0,$80	;micro
DCSF_GS_lpi			.db $00,$F8,$50,$50,$50	;lpi
DCSF_GS_lhmmm		.db $00,$70,$A0,$A0,$40	;hmmm
DCSF_GS_ltau		.db $00,$E0,$40,$50,$20	;tau
DCSF_GS_Psi			.db $20,$70,$A8,$70,$20	;Psi
DCSF_GS_Omega		.db $70,$88,$88,$50,$D8	;Omega

DCSF_3x3:
