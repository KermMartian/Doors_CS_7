;-----------------------------------------------------------
;	Filename:		sortalph.asm
;	Long name:  	Alpha-sort VAT
;	Authors:		Tari (Peter Marheine) with Christopher Mitchell
;	Last Update:	12-1-06-0749
;
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

;int start, stored, location, temp, VAT[VAT_end]
;for ( start=0; start<VAT_end; start++; ) {
;   stored=start;
;   for ( location=start; location<VAT_end; location++; ) {
;      if ( VAT[location]>VAT[stored] ) {
;         stored=location;
;         }
;      }
;   temp=VAT[stored];
;   VAT[stored]=VAT[start];
;   VAT[start]=temp;
;   }
SavedPtr = IconSpace32b+27	;word

sort:
	bcall(_grbufclr)
	xor a
	bcall(_GUIDrawHourglass)
	ld hl,(ProgPtr)
for1:
   push hl
	   ld (SavedPtr),hl
	   call local_getnext
	   push hl
		   pop ix
	   jr z,end1
	   pop hl
   ld de,OP1
   call getname
   push hl
for2:
	   push ix
			pop hl
	   ld de,OP2
	   push de
		   call getname
		   ld hl,OP1
		   pop de
	   ld b,8
	   call strcomp_local
	   jr nc,for2_2
	   ld (SavedPtr),ix
	   push ix
		pop hl
	   ld de,OP1
	   call getname
for2_2:
	   push ix
			pop hl
	   call local_getnext
	   push hl
			pop ix
	   jr nz,for2
for1_more:
	   pop hl
   push hl
	   ld de,(SavedPtr)
	   call local_vatswap
	   pop hl
   call local_getnext
   jr for1
   
strcomp_local:
;   ld a,(de)
;   cp (hl)
	ld a,(hl)
	or a
	jr z,strcomp_local_normalA
	cp 27
	jr nc,strcomp_local_normalA
	add a,$40
strcomp_local_normalA:
	ld c,a
	ld a,(de)
	or a
	jr z,strcomp_local_normalB
	cp 27
	jr nc,strcomp_local_normalB
	add a,$40
strcomp_local_normalB:
	cp c
	ret nz
	or a
	ret z
	inc hl
	inc de
	djnz strcomp_local
	ret

getname:
	push hl
		ld bc,-6
		add hl,bc
		ld b,(hl)
		ld a,9			;at least 1 zero at the end
		sub b
		ld c,a
getname_L1:
		dec hl
		ld a,(hl)
		ld (de),a
		inc de
		djnz getname_L1
		ld b,c
		xor a
getname_L2:
		ld (de),a
		inc de
		djnz getname_L2
end1:
		pop hl
	ret 

local_getnext:
	push de
	ld de,-6
	add hl,de
	ld b,(hl)
	inc b
local_getnextloop:
	dec hl
	djnz local_getnextloop
	ld de,(ptemp)
	bcall(_cphlde)
	pop de
	ret

Tvat1begin .equ IconSpace32b
Tvat2begin .equ IconSpace32b+17 ;word
vat1size   .equ IconSpace32b+19 ;byte
vat2size   .equ IconSpace32b+20 ;byte
vat1begin  .equ IconSpace32b+21 ;word
vat1end    .equ IconSpace32b+23 ;word
vat2begin  .equ IconSpace32b+25 ;word

;INPUT:
;HL = pointer to entry
;DE = pointer to entry
local_vatswap:
   call mos_cphlde
   ret z									;duh
   jr nc,l_entryOrderOK
   ex de,hl ;I want a consistent order, unless I can assume one (VAT1 > VAT2)
l_entryOrderOK:

   push de

   ld (vat1begin),hl
   push hl
   ld de,-6
   add hl,de ;(hl) = Name Length
   ld a,(hl)
   neg
   ld e,a
   add hl,de ;hl - NL
   ld (vat1end),hl
   ex de,hl
   pop hl
   sbc hl,de
   ld a,l
   inc a
   inc a
   ld (vat1size),a

   ld hl,(vat1begin)
   ld de,Tvat1begin
   ld b,0 \ ld c,a
   lddr ;Copy vat1 entry
   
   pop hl

   ld (vat2begin),hl
   push hl
   ld de,-6
   add hl,de ;(hl) = Name Length
   ld a,(hl)
   neg
   ld e,a
   add hl,de ;hl - NL
   pop de
   ex de,hl
   sbc hl,de
   ld a,l
   inc a
   inc a
   ld (vat2size),a

   ld hl,(vat2begin)
   ld de,Tvat2begin
   ld b,0 \ ld c,a
   lddr ;Copy vat2 entry

;ENTRIES PRESERVED, LENGTH ACQUIRED

   ld a,(vat2size)
   ld b,a
   ld a,(vat1size)
   cp b
   jr z,l_equalSwap
;if a > b
   jr nc,l_leftToRightShiftSwap
;if a < b

   sub b ;negative value
   neg
   push af

   ld hl,(vat2begin)
   inc hl
   push hl
   ld d,$ff \ neg \ ld e,a
   add hl,de         ;HL = NEW LEFT BOUNDARY
   pop de
   push de
   push hl

;de = (vat2begin)
   ld hl,(vat1end)
   or a ;clear carry
   sbc hl,de
   ld b,h \ ld c,l   ;BC = SIZE OF CHUNK
   pop de
   pop hl
   ld a,b
   or c
   jr z,l_swapSkipldir
   inc bc
;hl = src
;de = dest
;bc = count (distance)
   ldir ;Our fancy shifting opcode :>
l_swapSkipldir:

;MEMORY INBETWEEN SHIFTED
   ;ld hl,(vat1begin)
   ;ex de,hl
   ld de,(vat1begin)
   ld hl,Tvat2begin
   ld a,(vat2size)
   ld b,0 \ ld c,a
   lddr

   ld hl,(vat2begin)
   pop af ;a = vatdiff
   neg
   ld b,$ff \ ld c,a
   add hl,bc ;CORRECT FOR SHIFTING OF ENTRY
   ld de,Tvat1begin
   ex de,hl
   ld a,(vat1size)
   ld b,0 \ ld c,a
   lddr   

   ret

l_equalSwap:
   ;ld hl,(vat1begin)
   ;ex de,hl
   ld de,(vat1begin)
   ld hl,Tvat2begin
   ld a,(vat1size)
   ld b,0 \ ld c,a
   lddr

   ld hl,(vat2begin)
   ex de,hl
   ld hl,Tvat1begin
   ld a,(vat1size)
   ld b,0 \ ld c,a
   lddr

   ret ;EQUAL SIZED ENTRIES SWAPPED!

l_leftToRightShiftSwap
   sub b
   push af

   ld hl,(vat1end)
   dec hl
   push hl
   ld d,0 \ ld e,a
   add hl,de        ;HL = NEW RIGHT BOUNDARY
   pop de
   push de
   push hl

;de = (vat1end)
   ld hl,(vat2begin)
   or a ;clear carry
	ex	de,hl
   sbc hl,de
   ld b,h \ ld c,l   ;BC = SIZE OF CHUNK
   pop de
   pop hl
   ld a,b
   or c
   jr z,l_swapSkiplddr
   inc bc
;hl = src
;de = dest
;bc = count (distance)
   lddr ;Our fancy shifting opcode :>
l_swapSkiplddr:

;MEMORY INBETWEEN SHIFTED
   ;ld hl,(vat1begin)
   ;ex de,hl
   ld de,(vat1begin)
   ld hl,Tvat2begin
   ld a,(vat2size)
   ld b,0 \ ld c,a
   lddr

   ld hl,(vat2begin)
   pop af ;a = vatdiff
   ld b,0 \ ld c,a
   add hl,bc ;CORRECT FOR SHIFTING OF ENTRY
   ex de,hl
   ld hl,Tvat1begin
   ld a,(vat1size)
   ld c,a
   lddr   
   ret