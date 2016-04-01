;-----------------------------------------------------------
;	Filename:		moslibs.asm
;	Long name:  	MirageOS Compatibility Library
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	Unknown
;
; Partial support for MOS libs. Code used with permission
; from Dan Weiss, author of Crunchy.
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------


mos_getnext:
	push de
	ld de,-6
	add hl,de
	ld b,(hl)
	inc b
getnextloop:
	dec hl
	djnz getnextloop
	ld de,(ptemp)
	call mos_cphlde
	pop de
	ret

;INPUT:
;HL = pointer to entry
;DE = pointer to entry
;mos_vatswap:
;   call	mos_cphlde
;   jr nc,entryOrderOK
;   ex de,hl ;I want a consistent order, unless I can assume one (VAT1 > VAT2)
;entryOrderOK:
;
 ;  push de
;
;	   ld (vat1begin),hl
;	   push hl
;		   ld de,-6
;		   add hl,de ;(hl) = Name Length
;		   ld a,(hl)
;		   neg
;		   ld e,a
;		   add hl,de ;hl - NL
;		   ld (vat1end),hl
;		   ex de,hl
;		   pop hl
;	   sbc hl,de
;	   ld a,l
;	   inc a
;	   inc a
;	   ld (vat1size),a
;
;	   ld hl,(vat1begin)
;	   ld de,Tvat1begin
;	   ld b,0 \ ld c,a
;	   lddr ;Copy vat1 entry
;	   
 ;  pop hl
;
 ;  ld (vat2begin),hl
  ; push hl
	;   ld de,-6
	 ;  add hl,de ;(hl) = Name Length
;	   ld a,(hl)
;	   neg
;	   ld e,a
;	   add hl,de ;hl - NL
;	   pop de
 ;  ex de,hl
  ; sbc hl,de
   ;ld a,l
;   inc a
 ;  inc a
  ; ld (vat2size),a
;
;   ld hl,(vat2begin)
 ;  ld de,Tvat2begin
  ; ld b,0 \ ld c,a
   ;lddr ;Copy vat2 entry

;ENTRIES PRESERVED, LENGTH ACQUIRED

;   ld a,(vat2size)
 ;  ld b,a
  ; ld a,(vat1size)
   ;cp b
;   jr z,equalSwap				;if a > b
 ;  jr nc,leftToRightShiftSwap	;if a < b
;
 ;  sub b ;negative value
  ; neg
   ;push af
;
;	   ld hl,(vat2begin)
;	   inc hl
;	   push hl
;		   ld d,$ff \ neg \ ld e,a
;		   add hl,de         ;HL = NEW LEFT BOUNDARY
;		   pop de
;	   push de
;		   push hl
;
;	;de = (vat2begin)
;			   ld hl,(vat1end)
;			   or a ;clear carry
;			   sbc hl,de
;			   ld b,h \ ld c,l   ;BC = SIZE OF CHUNK
;			   pop de
;		   pop hl
;	   ld a,b
;	   or c
;	   jr z,swapSkipldir
;	   inc bc
;;hl = src
;de = dest
;bc = count (distance)
;	   ldir ;Our fancy shifting opcode :>
;swapSkipldir:

;MEMORY INBETWEEN SHIFTED
;	   ld hl,(vat1begin)
;	   ex de,hl
;	   ld hl,Tvat2begin
;	   ld a,(vat2size)
;	   ld b,0 \ ld c,a
;	   lddr
;
;	   ld hl,(vat2begin)
;	   pop af ;a = vatdiff
 ;  neg
  ; ld b,$ff \ ld c,a
   ;add hl,bc ;CORRECT FOR SHIFTING OF ENTRY
;   ld de,Tvat1begin
 ;  ex de,hl
  ; ld a,(vat1size)
;   ld b,0 \ ld c,a
 ;  lddr   
;
;   ret

;equalSwap:
 ;  ld hl,(vat1begin)
  ; ex de,hl
   ;ld hl,Tvat2begin
;   ld a,(vat1size)
 ;  ld b,0 \ ld c,a
  ; lddr
;
 ;  ld hl,(vat2begin)
  ; ex de,hl
   ;ld hl,Tvat1begin
;   ld a,(vat1size)
 ;  ld b,0 \ ld c,a
  ; lddr
;
 ;  ret ;EQUAL SIZED ENTRIES SWAPPED!

;leftToRightShiftSwap
 ;  sub b
  ; push af
;
;	   ld hl,(vat1end)
;	   dec hl
;	   push hl
;		   ld d,0 \ ld e,a
;		   add hl,de        ;HL = NEW RIGHT BOUNDARY
;		   pop de
;	   push de
;		   push hl
;
;de = (vat1end)
;			   ld hl,(vat2begin)
;			   or a ;clear carry
;				ex	de,hl
;			   sbc hl,de
;			   ld b,h \ ld c,l   ;BC = SIZE OF CHUNK
;			   pop de
;		   pop hl
;	   ld a,b
;	   or c
;	   jr z,swapSkiplddr
;	   inc bc
;;hl = src
;d;e = dest
;bc; = count (distance)
;	   lddr ;Our fancy shifting opcode :>
;swapSkiplddr:
;
;MEMORY INBETWEEN SHIFTED
;	   ld hl,(vat1begin)
;	   ex de,hl
;	   ld hl,Tvat2begin
;	   ld a,(vat2size)
;	   ld b,0 \ ld c,a
;	   lddr
;
;	   ld hl,(vat2begin)
;	   pop af ;a = vatdiff
 ;  ld b,0 \ ld c,a
  ; add hl,bc ;CORRECT FOR SHIFTING OF ENTRY
;   ex de,hl
 ;  ld hl,Tvat1begin
  ; ld a,(vat1size)
   ;ld c,a
;   lddr   
 ;  ret
   
;Routines by Kroot (Kristopher Root) www.greenfire.tk
;DirectIn	- Direct input routine
;		- Input  a = key group
;		- Output z = no key was pressed
;		-        a = key code
mos_DirectIn:
 ld b,a
 ld a,0FFh
 out (1),a
 ld a,b
 out (1),a
 in a,(1)
 cp $ff
 ret

MirageLinkErr:
;l4616:
 xor a
 inc a
 ret

mos_sendbytetios:
 AppOnErr(MirageLinkErr)
 bcall(_sendabyte)
 AppOffErr
 cp a
 ret

mos_getbytetios:
 AppOnErr(MirageLinkErr)
 bcall(_RecAByteIO)
; ld (appBackUpScreen+200),a  ;old code
; AppOffErr
; ld a,(appBackUpScreen+200)
	ld e,a
	AppOffErr
	ld a,e
 cp a
 ret

mos_getbytetiosw:
 AppOnErr(MirageLinkErr)
 bcall(_Rec1stByteNC)
 ld e,a
 AppOffErr
 ld a,e
; ld (appBackUpScreen+200),a
; AppOffErr
; ld a,(appBackUpScreen+200)
 cp a
 ret

mos_version:
 ld a,2
 ld hl,0101h
 ret

;setvputs	- Sets graph cursor and displays text
;		- Input  de = cursor location
;		-        hl -> string
;		- Ouput  String displayed

mos_setvputs:
 ld (pencol),de
 bcall(_vputs)
 ret

mos_pointonc:
 cp $60
 ret nc
 bit 6,e
 ret nz
 jr mos_setpixel
mos_pixelonhl:
 ld a,h
 ld e,l
mos_setpixel:
	push hl
		call imGetPixel
		or (hl)
		ld (hl),a
		pop hl
	ret

mos_pointoffc:
 cp $60
 ret nc
 bit 6,e
 ret nz
 jr mos_pixeloff
mos_pixeloffhl:
 ld a,h
 ld e,l
mos_pixeloff:
 push hl
	 call imGetPixel
	 cpl
	 and (hl)
	 ld (hl),a
	 pop hl
 ret

mos_pointxorc:
 cp 96
 ret nc
 bit 6,e
 ret nz
 jr mos_pixelxor
mos_pixelxorhl:
 ld a,h
 ld e,l
mos_pixelxor:
 push hl
	 call imGetPixel
	 xor (hl)
	 ld (hl),a
	 pop hl
 ret

mos_pixeltesthl:
 ld a,h
 ld e,l
mos_pixeltest_:
 push hl
	 call imGetPixel
	 and (hl)
	 pop hl
 ret
 
;fastcopys	- Runs fastcopy, but preserves all registers
mos_fastcopys:
 push af
	 push hl
		 push de
			 push bc
				 call imFastCopy
				 pop bc
			 pop de
		 pop hl
	 pop af
 ret

;delayb		- Delays using halt loop
;		- Input  b = delay amount
;		- Output delay
mos_delayb:
 ei
delayb_loop:
 halt
 djnz delayb_loop
 ret

mos_multhl:
 ld e,l

;multhe		- Multiplies h*e (fast!)
;		- Input  h = first operand
;		-        e = second operand
;		- Output hl = h*e
mos_MultHE:
 ld     l,0
 ld     d,l
 ld     b,8
MultHELoop:
 add    hl,hl
 jr     nc,MultHELoop2
 add    hl,de
MultHELoop2:
 djnz   MultHELoop
 ret

mos_quittoshell:
	ld sp,(cmdshadow+78)		;<<< That is a saved stack pointer
	ld hl,(cmdshadow+78)
	inc hl
	inc hl
	bcall(_ldhlind)
	ld de,cmdshadow
	ex de,hl
	ld (hl),$ED
	inc hl
	ld (hl),$79
	inc hl
	ld (hl),$c9
	; dec hl
	; dec hl
	; ld sp,hl
	ld a,d
	ld c,6
	jp CmdShadow


;centertext	- Centers text (horizontally) on the screen and displays
;		- Input  a = penrow to display at
;		-        hl -> text
;		- Output displays centered text
;		-        destroys op registers
mos_centertext:
     ld   (penrow),a
     ld   de,op1+1
     push de
		 bcall(_strcopy)
		 pop  hl
     bcall(_strlength)
     dec  hl
     ld   (hl),c
     bcall(_sstringlength)
     sra  b
     ld   a,48
     sub  b
     ld   (pencol),a
     inc  hl
     bcall(_vputs)
     ret

;cphlbc		- Compares hl and bc
mos_cphlbc:
   push hl
	   or   a
	   sbc  hl, bc
	   pop  hl
   ret

;putsprite8	- Calls the Ion putsprite routine, but sets size to 8
mos_putsprite8:
 ld b,8
 jp imputsprite

;rand127		- Returns a number between 0 and 127 in register a
mos_rand127:
 ld b,128
 jp imrandom

;cphlde		- Compares hl and de
mos_cphlde:
  or a
  sbc hl,de
  add hl,de
  ret

mos_compstrs:
	ld a,(de)
	cp (hl)
	jr nz,mos_nextstr
	inc hl
	inc de
	or a
	ret z
	jr mos_compstrs

;nextstr		- Find next string
;		- Input  hl -> string
;		- Output hl -> byte after string's null terminator
mos_nextstr:
	ld a,(hl)
	inc hl
	or a
	jr nz,mos_nextstr
	cp 1				;set nz flag
	ret

;compstrsn	- Compares two null terminated strings for 'b' letters
;		- Input  hl -> string 1
;		-        de -> string 2
;		-				 b  -> number of bytes to compare
;		- Output z = strings are the same
;		-        hl -> byte after string1's null terminator


mos_compstrsn:
	ld a,(de)
	cp (hl)
	inc hl
	inc de
	ret nz
	djnz mos_compstrsn
	ret

;displays character A at pencol=hl, moves the cursor back, flips the MSB of arcinfo+22,
;displays character A AGAIN, flips the MSB of arcinfo+22 again.  WTF
;
mos_vputa:
 push af
	 push hl
		 push af
			 ld hl,(pencol)
			 push hl
				 bcall(_vputmap)
				pop hl
			 ld (penCol),hl
			 ld hl,arcInfo+22  ;arcInfo+22
			 ld a,80h
			 xor (hl)
			 ld (hl),a
		 pop af
		 bcall(_vputmap)
		 ld hl,arcInfo+22
		 ld a,80h
		 xor (hl)
		 ld (hl),a
	 pop hl
 pop af
 ret


;largespritehl	- Same as Ion largesprite routine, but h = x coordinate
mos_largespritehl:
 ld a,h
 jp imlargesprite

;END routines by kroot, back to code by Dwedit

NotImplemented:
;Displays @xxxx: CALL yyyy
;         Not Implemented!
	ex (sp),hl
	push af
		push bc
			ld bc,(currow)
			push bc
				push de
					push hl
						dec hl
						push hl
							ld de,0
							ld (currow),de
							dec hl
							dec hl
							ld a,'@'
							bcall(_putc)
							bcall(_disphl)
							ld a,':'
							bcall(_putc)
						pop hl
						;ld hl,(hl-2)
						ld a,(hl)
						dec hl
						ld l,(hl)
						ld h,a
						bcall(_disphl)
						ld de,1
						ld (currow),de
						;ld hl,tNotImplemented
						;call Appputs
NotImPause:
						bcall(_getcsc)
						or a
						jr z,NotImPause
;						cp 09d						;kEnter
;						jp z,mos_quittoshell
						jp mos_quittoshell
						
					pop hl
				pop de
			pop bc
			ld (currow),bc
		pop bc
	pop af
	ex (sp),hl
	ret

mos_fastcopyb:
; ld de,$2F3
; add hl,de
; ld a,$80
; out ($10),a
; call slowdown_silver
mos_fastcopyentry:
	jp imFastCopy

mos_vputsc:
 push hl
 ld de,(penCol)
 push de
 bcall(_VPutS)
 pop de
 ld (penCol),de
 ld a,$80
 ld hl,flags+20
 xor (hl)
 ld (hl),a
 pop hl
 bcall(_VPutS)
 ld a,$80
 ld hl,flags+20
 xor (hl)
 ld (hl),a
 ret

mos_scrolld7:  ;only mos_vnewline actually uses this
 ld de,plotSScreen
 ld hl,plotSScreen+84
 ld bc,$2AC
 ldir
 ld hl,plotSScreen+684
; ld bc,$54
; bcall(_MemClear)
 ld bc,$53
 ld de,plotsscreen+685
 ld (hl),0
 ldir
 jp imfastcopy

mos_vnewline:
 xor a
 ld (penCol),a
 ld a,(penRow)
 add a,$07
 cp $3F
 jr z,mos_scrolld7
 ld (penRow),a
 ret

mos_disprle:
 ld bc,$300
mos_disprlel:
 ld a,(hl)
 cp $91
 jr z,l428A
 ldi
l4287:
 ret po
 jr mos_disprlel
l428A:
 inc hl
 inc hl
 ld a,(hl)
l428D:
 dec hl
 dec a
 ldi
 jr nz,l428D
 inc hl
 jr l4287

mos_fastrectangle:
	ld c,a
	push de
	ld e,l
	call mos_fastrectangle_line
	pop de
	push de
	ld d,h
	call mos_fastrectangle_line
	pop de
	push hl
	ld h,d
	call mos_fastrectangle_line
	pop hl
	ld l,e
mos_fastrectangle_line:
	push bc
	push hl
	push de
	ld a,c
	call mos_fastline
	pop de
	pop hl
	pop bc
	ret

l44DD:
 push hl
 exx
 ex af,af'
 jp z,l44FF
 jp c,l4509
 push af
 ex af,af'
 pop af
 pop de
 cp $02
 jr nz,l44F6
 ld a,d
 call imGetPixel
 xor (hl)
 ld (hl),a
 exx
 ret
l44F6:
 ld hl,stack_bottom
 rlc (hl)
 jr c,l4501
 jr l450B
l44FF:
 ex af,af'
 pop de
l4501:
 ld a,d
 call imGetPixel
 or (hl)
 ld (hl),a
 exx
 ret
l4509:
 ex af,af'
 pop de
l450B:
 ld a,d
 call imGetPixel
 cpl
 and (hl)
 ld (hl),a
 exx
 ret

mos_fastlined:
 ld a,$AA
 ld (stack_bottom),a
 jr mos_fastline
mos_fastlinew:
 xor a
 jr mos_fastline
mos_fastlinex:
 ld a,$02
 jr mos_fastline
mos_fastlineb:
 ld a,$01
mos_fastline:
	ld b,a
	call iCheckInts1
	push af
		di
		ld a,b
		cp $01
		ex af,af'
		ld a,$01
		ld (stack_bottom+1),a
		ld (stack_bottom+1+1),a
		ld a,d
		sub h
		ld b,a
		jp nc,l4542
		neg
		ld b,a
		ld a,$FF
		ld (stack_bottom+1),a
l4542:
		ld a,e
		sub l
		ld c,a
		jp nc,l4550
		neg
		ld c,a
		ld a,$FF
		ld (stack_bottom+1+1),a
l4550:
		ld e,$00
		ld a,b
		cp c
		jp c,l4573
		ld d,b
		inc b
l4559:
		call l44DD
		ld a,(stack_bottom+1)
		add a,h
		ld h,a
		ld a,e
		add a,c
		ld e,a
		cp d
		jp c,l456F
		sub d
		ld e,a
		ld a,(stack_bottom+1+1)
		add a,l
		ld l,a
l456F:
		djnz l4559
		pop af
	ret po
;#ifdef enablecn2eis
	ei
;#endif
	ret

l4573:
		ld a,b
		ld b,c
		ld c,a
		ld d,b
		inc b
l4578:
		call l44DD
		ld a,(stack_bottom+1+1)
		add a,l
		ld l,a
		ld a,e
		add a,c
		ld e,a
		cp d
		jp c,l458E
		sub d
		ld e,a
		ld a,(stack_bottom+1)
		add a,h
		ld h,a
l458E:
		djnz l4578
		pop af
	ret po
;#ifdef enablecn2eis
	ei
;#endif
	ret 

l7EA2:
	pop af
	push de
	push hl
	ld c,a
	ld a,d
	sub h
	inc a
	ld b,a
	ld d,h
l7EAB:
	push bc
	push hl
	push de
	ld a,c
	call mos_fastline
	pop de
	pop hl
	inc d
	inc h
	pop bc
	djnz l7EAB
	pop hl
	pop de
	ret

mos_filledrectangle:
	push af				;a  = mode: 0, 1, or 2 (2=invert)
		ld a,e					;e	= y1
		sub l					;l	= y0
		inc a
		ld b,a
		ld a,d					;d	= x1
		sub h					;h	= x0
		inc a
		cp $0A
		jr c,l7EA2
		cp $11
		jr nc,l7ED5
		ld c,a
		ld a,h
		and $07
		add a,c
		cp 17
		jr nc,l7ED5
		
l7ED4:
		set 7,c
;		ld a,d
;		and $07
;		cp $07
;		jr nz,l7EE3
;		ld c,%10000000
		jr l7EE3
l7ED5:
		ld a,h
		and $F8
		ld c,a
		ld a,d
		sub c
		srl a
		srl a
		srl a
		dec a
		ld c,a
l7EE3:
		push de
			ld a,h
			ld e,l
			ld h,$00
			ld d,h
			add hl,de
			add hl,de
			add hl,hl
			add hl,hl
			ld e,a
			srl e
			srl e
			srl e
			add hl,de
			ld de,plotSScreen
			add hl,de
			pop de
		push hl
			push bc
				ld c,d
				and $07
				ld l,a
				ld h,$00
				ld de,data4678
				add hl,de
				ld a,(hl)
				cpl
				ld b,a
				ld a,c
				and $07
				ld l,a
				ld h,$00
				ld de,data4678+1
				add hl,de
				ld a,(hl)
				ld c,a
				push bc
					pop de
				pop bc
			pop hl
		pop af
	ld (stack_bottom),a			;pattern
	or a
	jr z,l7F40					;white
	dec a
	jr z,l7F64					;black
	dec a
	jr z,l7F20					;invert
mos_fastline_start_pattern:
	ld a,(stack_bottom)
	cpl
	ld (stack_bottom),a
	push bc
		push hl

			push bc
				ld a,(stack_bottom)
				cpl
				and d
				ld b,a
				ld a,(hl)
				or d
				xor b
				ld (hl),a
				pop bc

			inc hl
			bit 7,c
			jr nz,mos_fastline_end_pattern
			ld b,c
mos_fastline_inner_pattern:
		;	ld a,(hl)
		;	or $FF
		;	cpl			;xor $FF
			ld a,(stack_bottom)
			ld (hl),a
			inc hl
			djnz mos_fastline_inner_pattern
mos_fastline_end_pattern:

;	push bc
			ld a,(stack_bottom)
			cpl
			and e
			ld b,a
			ld a,(hl)
			or e
			xor b
			ld (hl),a
		;	pop bc

			pop hl
		push de
			ld de,$C
			add hl,de
			pop de
		pop bc
	djnz mos_fastline_start_pattern
	ret

l7F20:
	push bc
	push hl
	ld a,(hl)
	xor d
	ld (hl),a
	inc hl
	bit 7,c
	jr nz,l7F32
	ld b,c
l7F2B:
	ld a,(hl)
	cpl			;xor $FF
	ld (hl),a
	inc hl
	djnz l7F2B
l7F32:
	ld a,(hl)
	xor e
	ld (hl),a
	pop hl
	push de
	ld de,$C
	add hl,de
	pop de
	pop bc
	djnz l7F20
	ret
l7F40:
	push bc
	push hl
	ld a,(hl)
	or d
	xor d
	ld (hl),a
	inc hl
	bit 7,c
	jr nz,l7F55
	ld b,c
l7F4C:
	ld a,(hl)
	or $FF
	cpl			;xor $FF
	ld (hl),a
	inc hl
	djnz l7F4C
l7F55:
	ld a,(hl)
	or e
	xor e
	ld (hl),a
	pop hl
	push de
	ld de,$C
	add hl,de
	pop de
	pop bc
	djnz l7F40
	ret
l7F64:
	push bc
	push hl
	ld a,(hl)
	or d
	ld (hl),a
	inc hl
	bit 7,c
	jr nz,l7F76
	ld b,c
l7F6F:
	ld a,(hl)
	or $FF
	ld (hl),a
	inc hl
	djnz l7F6F
l7F76:
	ld a,(hl)
	or e
	ld (hl),a
	pop hl
	push de
	ld de,$C
	add hl,de
	pop de
	pop bc
	djnz l7F64
	ret

mos_filledrectangle_save:
	push af
		push de
			push hl
				push bc
					call mos_filledrectangle
					jr mos_FastRectangle_Save_Finish
;					pop bc
;				pop hl
;			pop de
;		pop af
;	ret

mos_FastRectangle_Save:
	push af
		push de
			push hl
				push bc
					call mos_fastrectangle
mos_FastRectangle_Save_Finish:
					pop bc
				pop hl
			pop de
		pop af
	ret

data4678:
	.db %100000000
	.db %110000000
	.db %111000000
	.db %111100000
	.db %111110000
	.db %111111000
	.db %111111100
	.db %111111110
	.db %111111111

timer1                = $8a3a
timer1max             = $8a3b
timer2                = $8a3c
timer2max             = $8a3d
timer3                = $8a3e

The_Interrupt:
	di
	exx
	ex af,af'
	ld a,(The_Interrupt_Flags)
	bit 0,a
	jr z,notimer

	ld hl,(timer1)
	inc hl
	ld (timer1),hl
	ld hl,(timer2)
	inc hl
	ld (timer2),hl
	ld hl,timer3
	inc (hl)
notimer:
	exx
	bit 5,a
	jr z,noInterrupt
	ex af,af'
Int_Modify = $ - The_interrupt + 1 + $8A8A
	call 0 ;modify this...
	rst 38h
	ret
noInterrupt:
	ex af,af'
	rst 38h
	ret
The_Interrupt_Flags = $ - The_interrupt + $8A8A
	nop
The_Interrupt_end:

;		-          bit 0 = timer interrupt
;		-          bit 1 = getkey interrupt   (ignored)
;		-          bit 2 = apd interrupt      (apd timer is initalized to 72)
;		-          bit 3 = task interrupt     (ignored)
;		-          bit 4 = keydelay interrupt (ignored)
;		-          bit 5 = custom interrupt

mos_setupint:
;hl = address of interrupt
;NOT the same as the mirageos version
;interrupt gets installed at 8B00-8C00, points to 8A8A (all inside statvars)
	di

	res statsvalid,(iy+statflags)
	push hl
		ld hl,The_Interrupt
		ld de,$8A8A
		ld bc,The_Interrupt_end-The_interrupt
		ldir
		ld (The_Interrupt_Flags),a
		push af
			ld hl,$8B00
			ld de,$8B01
			ld bc,256
			ld a,h
			ld i,a
			dec a
			ld (hl),a
			ldir
		pop af
		bit 0,a
		jr z,setupnotimer
		ld hl,timer1
		ld b,5
setupint_clearbytes:
		ld (hl),0
		djnz setupint_clearbytes
setupnotimer:
		bit 2,a
		jr z,setupnoapd
		set apdable,(iy+apdflags)
		ld hl,apdtimer
		ld (hl),72
setupnoapd:
		pop hl
	ld (Int_Modify),hl
	im 2
	ei
	ret
		

;#IF 0
mos_getText:
	ld c,0
;		-           bit 0 set = don't display cursor
;		-           bit 1 set = don't display to screen or graphbuffer, just save to (hl)
;		-           bit 2 set = don't allow numbers
;		-           bit 3 set = don't allow backspace or clear

mos_getTextv:

	ld a,(flags+asm_flag1)
	ld (iy+asm_flag1),c
	push af
		call gt_main
		pop af
	ld (flags+asm_flag1),a
	ret

gt_backspace:
	bit 3,(iy+asm_flag1)
	jr nz,gettextloop

	ld a,c
	or a
	jr z,gettextstart
	dec c
	push bc
		ld b,0
		push hl
			add hl,bc
			ld (hl),b		;=0
		pop hl
	pop bc
	jr getTextStart
gt_clear:
	bit 3,(iy+asm_flag1)
	jr nz,gettextloop

	ld c,0
	ld (hl),c				;=0
	jr getTextStart

gt_main:	
	;also not the same as MirageOS version
	set textwrite,(iy+sgrflags)
	ld c,0
	ld (hl),c				;=0
	
	ld ix,(pencol)
	
	
	;b = total characters, c = on character
	;e = alpha state, d = blink counter, hl = pointer to text, (sp) = pencol
gettextstart:
	ld de,$FF00

gettextloop:
	inc d
	push bc
		push hl
			push de
				bit 1,(iy+asm_flag1)
				call z,gt_drawstring
			pop de
			ld a,d
			and %00010000
			jr nz,gt_nocursor
			bit 0,(iy+asm_flag1)
			jr nz,gt_nocursor
			call gt_toggleinverse
			push ix
				call gt_drawcursor
			pop ix
			call gt_toggleinverse
gt_nocursor:
			call mos_fastcopys
		pop hl
		push hl
			push de
				call gt_erase
				ei
				halt
				bcall(_getcsc)
			pop de
			call gt_scancodetoascii
		pop hl
	pop bc
	bit 2,(iy+asm_flag1)
	call nz,gt_numbertest
	or a
	
	jr z,gettextloop
	dec a ;1
	jr z,gt_done
	dec a ;2
	jr z,gt_backspace
	dec a ;3
	jr z,gt_clear
	dec a ;4
	jr z,gt_alpha
	add a,4 ;back
	;insert character
	ld d,a
	;enough space?
	ld a,c
	cp b
	jr z,gettextstart
	push hl

		push bc

			ld b,0
			add hl,bc
			;insert char
			ld (hl),d
			inc hl
			ld (hl),b				;=0
		pop bc
	pop hl
	inc c
	jr gettextstart
gt_done:
	jp gt_drawstring
gt_alpha:
	ld a,e
	xor 1
	ld e,a
	ld d,0
	jr getTextLoop

gt_erase:
	ld bc,(pencol)
	push ix
	pop hl
	

	ld a,l
	ld l,h
	ld h,a
	
	cp c
	ret z
	dec c
	
	ld d,c
	ld a,5
	bit texterasebelow,(iy+textflags)
	jr z,gt_noextrarow
	inc a
gt_noextrarow:
	
	add a,b
	ld e,a

	ld a,d
	sub h
	cp 15
	jr nz,gt_mirbugworkaround
	inc d
gt_mirbugworkaround:
	xor a
	bit textinverse,(iy+textflags)
	jr z,gt_notwhite
	ld a,1
gt_notwhite:


	call mos_filledrectangle
	ret
	
gt_drawstring:
	ld (pencol),ix
	bcall(_vputs)
	ret
gt_toggleinverse:
	ld hl,flags+textflags
	ld a,(hl)
	xor 1 << textinverse
	ld (hl),a
	ret
gt_drawcursor:
	;display A or a depending on alpha state
	ld a,e
	or a
	jr nz,gt_capitala
	ld a,'a'
gt_putmap:
	push de
		bcall(_vputmap)
	pop de
	ret
gt_capitala:
	ld a,'A'
	jr gt_putmap

gt_scancodetoascii:
	or a
	ret z
	ld c,a
	ld b,0
	ld hl,asciitable-1
	ld a,e
	or a
	jr z,gt_dontadd56
	ld a,c
	add a,56
	ld c,a
gt_dontadd56
	add hl,bc
	ld a,(hl)
	or a
	ret

gt_numbertest:
	ret z
	cp '0'
	ret c
	cp '9'+1
	jr c,gt_itsanumber
	or a
	ret
gt_itsanumber:
	xor a
	ret

AsciiTable:
	.db 0,2,' ',0,0,0,0,0  ;dn,lf,rt,up,__,__,__,__
	.db 1,34,"wrmh",3,0    ;en,+ ,- ,* ,/ ,^ ,cl,__
	.db "?[vqlg!",0       ; -,3 ,6 ,9 ,) ,ta,va,__
	.db ":zupkfc."        ;. ,2 ,5 ,8 ,( ,co,pr,st
	.db " ytojebX"         ;0 ,1 ,4 ,7 ,, ,si,mx,xt
	.db 0,"xsnida",4       ;__,> ,ln,lg,x2,x1,mt,AL
	.db "54321",0,0,2      ;gr,tr,zo,wi,Y=,2n,mo,de

	.db 0,2,' ',0,0,0,0,0  ;dn,lf,rt,up,__,__,__,__
	.db 1,34,"WRMH",3,0    ;en,+ ,- ,* ,/ ,^ ,cl,__
	.db "?[VQLG!",0       ; -,3 ,6 ,9 ,) ,ta,va,__
	.db ":ZUPKFC."        ;. ,2 ,5 ,8 ,( ,co,pr,st
	.db " YTOJEBX"         ;0 ,1 ,4 ,7 ,, ,si,mx,xt
	.db 0,"XSNIDA",4       ;__,> ,ln,lg,x2,x1,mt,AL
	.db "09876",0,0,2    ;gr,tr,zo,wi,Y=,2n,mo,de
;	
;	.db 0,2,' ',0,0,0,0,0  ;dn,lf,rt,up,__,__,__,__
;	.db 1,"+-*/^",3,0      ;en,+ ,- ,* ,/ ,^ ,cl,__
;	.db $D2,"369)t",0,0    ; -,3 ,6 ,9 ,) ,ta,va,__
;	.db ".258(c",0,0       ;. ,2 ,5 ,8 ,( ,co,pr,st
;	.db "0147,saX"         ;0 ,1 ,4 ,7 ,, ,si,mx,xt
;	.db 0,$1C,"ll",$12,$11,'!',4       ;__,> ,ln,lg,x2,x1,mt,AL
;	.db 0,0,0,0,0,5,0,2    ;gr,tr,zo,wi,Y=,2n,mo,de
;
;	.db 0,2,' ',0,0,0,0,0    ;dn,lf,rt,up,__,__,__,__
;	.db 1,0,"][",$DB,$C4,3,0 ;en,+ ,- ,* ,/ ,^ ,cl,__
;	.db 0,$DC,$DC,"w}t",0,0      ; -,3 ,6 ,9 ,) ,ta,va,__
;	.db $D6,".258(c",0,0         ;. ,2 ,5 ,8 ,( ,co,pr,st
;	.db "0147,saX"           ;0 ,1 ,4 ,7 ,, ,si,mx,xt
;	.db 0,$1C,"ll",$12,$11,'!',4       ;__,> ,ln,lg,x2,x1,mt,AL
;	.db 0,0,0,0,0,5,0,2      ;gr,tr,zo,wi,Y=,2n,mo,de

;#endif