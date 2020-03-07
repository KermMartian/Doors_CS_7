;-----------------------------------------------------------
;	Filename:		iolibsp.inc
;	Long name:  	Ion Extended Libs for 83+/84+/SE
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	June 2, 2006
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

; The Built-in Ion Library Routines by Joe Wingbermuehle

#define TI83P_USE_FLASH_iDetect

iCheckInts2:
;Push a zero byte to the stack and pop it
	xor a
	push af
		pop af
	ld a,i
	ret pe
	;See if an interrupt triggered. If so, the byte on the stack will not be 0 anymore
	dec sp
	dec sp
	pop af
	add a,a
	ret z
	xor a
	ret

;-----> Return Ion iVersion information
; hl=Ion iVersion,a=Ion compatibility number
; d=routine compatability number
; e=number of routines
immVersion:
	ld	a,ION_COMPATIBILITY
	ld	hl,iVersion_MAJOR*256+iVersion_MINOR
	ld	de,LIBRARY_COMPATIBILITY*256+NUMBER_OF_LIBRARIES
	ret

;-----> Generate a iRandom number
; input b=upper bound
; ouput a=answer 0<=a<b
; all registers are preserved except: af and bc
immRandom:
	push	hl
	push	de
	ld	hl,(randData)
	ld	a,r
	ld	d,a
	ld	e,(hl)
	add	hl,de
	add	a,l
	xor	h
	ld	(randData),hl
	sbc	hl,hl
	ld	e,a
	ld	d,h
immRandomLoop:
	add	hl,de
	djnz	immRandomLoop
	ld	a,h
	pop	de
	pop	hl
	ret

;-----> Draw a sprite
; b=size of sprite
; l=yc
; a=xc
; ix holds pointer
immPutSprite:
	ld	e,l
	ld	h,$00
	ld	d,h
	add	hl,de
	add	hl,de
	add	hl,hl
	add	hl,hl
	ld	e,a
	and	$07
	ld	c,a
	srl	e
	srl	e
	srl	e
	add	hl,de
	ld	de,gbuf
	add	hl,de
immPutSpriteLoop1:
mmsl1:	ld	d,(ix)
	ld	e,$00
	ld	a,c
	or	a
	jr	z,immPutSpriteSkip1
immPutSpriteLoop2:
	srl	d
	rr	e
	dec	a
	jr	nz,immPutSpriteLoop2
immPutSpriteSkip1:
	ld	a,(hl)
	xor	d
	ld	(hl),a
	inc	hl
	ld	a,(hl)
	xor	e
	ld	(hl),a
	ld	de,$0B
	add	hl,de
	inc	ix
	djnz	immPutSpriteLoop1
	ret

;-----> Draw a picture
;Input:	ix->sprite
;	a=x
;	l=y
;	b=height	(in pixels)
;	c=width		(in bytes, e.g. 2 would be 16)
;Output: nothing
; All registers are destroyed except bc', de', hl'
immLargeSprite:
	ld d,a
	call iCheckInts2
	push af
		ld a,d
		di
		ex	af,af'
		ld	a,c
			push	af
			ex	af,af'
			ld	e,l
			ld	h,$00
			ld	d,h
			add	hl,de
			add	hl,de
			add	hl,hl
			add	hl,hl
			ld	e,a
			and	$07
			ld	c,a
			srl	e
			srl	e
			srl	e
			add	hl,de
			ld	de,gbuf
			add	hl,de
immLargeSpriteLoop1:
			push	hl
immLargeSpriteLoop2:
				ld	d,(ix)
				ld	e,$00
				ld	a,c
				or	a
				jr	z,immLargeSpriteSkip1
immLargeSpriteLoop3:
				srl	d
				rr	e
				dec	a
				jr	nz,immLargeSpriteLoop3
immLargeSpriteSkip1:
				ld	a,(hl)
				xor	d
				ld	(hl),a
				inc	hl
				ld	a,(hl)
				xor	e
				ld	(hl),a
				inc	ix
				ex	af,af'
				dec	a
				push	af
					ex	af,af'
					pop	af
				jr	nz,immLargeSpriteLoop2
				pop	hl
			pop	af
		push	af
			ex	af,af'
			ld	de,$0C
			add	hl,de
			djnz	immLargeSpriteLoop1
			pop	af
		pop af
	ret po
;#ifdef enablecn2eis
	ei
;#endif
	ret

;-----> Get pixel information
; input:	e=y coordinate
;		a=x coordinate
; output:	a holds data for pixel (e.g. %00100000)
;		hl->byte where pixel is on the gbuf
immGetPixel:
	ld	d,$00
	ld	h,d
	ld	l,e
	add	hl,de
	add	hl,de
	add	hl,hl
	add	hl,hl
	ld	de,gbuf
	add	hl,de
	ld	b,$00
	ld	c,a
	and	%00000111
	srl	c
	srl	c
	srl	c
	add	hl,bc
	ld	b,a
	inc	b
	ld	a,%00000001
immGetPixelLoop:
	rrca
	djnz	immGetPixelLoop
	ret

;-----> Copy the gbuf to the screen, guaranteed 
;Courtesy of Runer112/Axe.
;Input: nothing
;Output:graph buffer is copied to the screen, no matter the speed settings
immFastCopy:
	ld	hl,gbuf
	ld	a,$80
	out	($10),a
	call	mmSafety
	push	af
	ld	a,$1F
__immFastCopyAgain:
	inc	a
	out	($10),a		;many cc into, 72cc loop
	cp	$2B
	ld	bc,(64<<9)+$11
	push	hl
	inc	de		;waste
__immFastCopyLoop:
	push	af		;semi waste
	ld	de,11
	outi			;71cc into, 71cc loop
	add	hl,de
	pop	af		;semi waste
	djnz	__immFastCopyLoop
	pop	hl
	inc	hl
	jr	c,__immFastCopyAgain
__immFastCopyRestore:
	pop	af
	out	($20),a
	ret	c
	ei
	ret

;Safety stuff (Stupid TI/z80!)
mmSafety:			;Output: af holds state to-be-pushed
				;DESTROYS B, all other registers okay.
	in	a,($02)		;Save model settings
	rla			;BE flag into carry
	sbc	a,a		;00 if BE else FF
	jr	z,__mmSafetyBE	;Check If model is base edition
	in	a,($20)		;Get current speed setting
__mmSafetyBE:
	ld	b,a		;Save speed setting
	xor	a		;Set nc (interrupts)
	out	($20),a		;Set speed to slow
mmSafetyIntOnly:
	push	af		;Check interrupts
	pop	af
	ld	a,i		;pe if interrupt
	di
	ld	a,b		;Restore speed setting
	ret	pe		;Return if interrupts on
	dec	sp		;Otherwise, robust test
	dec	sp
	pop	af
	or	a		;Set nc (interrupts)
	ld	a,b
	ret	nz		;Return if interrupts on
	scf			;Set c (no interrupts)
	ret			;Return

;-----> iDetect a file
; input:
;	hl=place to start looking
;	ix->first line of data (0 terminated)
; output:
;	de=place stopped + 1
;	hl->program data (after the string)
;	z=0 if found, z=1 if not found.
;	All registers destroyed
immDetect: ld	de,(ptemp)
	bcall(_cphlde)
	ld	a,(hl)
	jr	nz,immDetectContinue
	inc	a
	ret
immDetectContinue:
	push	hl
	and	$01
	jr	nz,immDetectSkip

	dec	hl
	dec	hl
	dec	hl	; hl->lsb ptr
	ld	e,(hl)
	dec	hl
	ld	d,(hl)
	dec	hl	; hl->page
	ld	a,(hl)
	or	a
	push	af
	ld	h,d
	ld	l,e	; hl & de->program
	jr	z,immDetectNoMove
	push	hl
	bcall(_memfree)
	ld	bc,64
	sbc	hl,bc
	pop	hl
	jr	c,immDetectNotEnough
	ld	de,($9820)
	push	ix
	push	hl
	push	de
	bcall(_flashToRam)
	pop	hl
	push	hl
	pop	ix
	ld	a,10
	add	a,(ix+9)
	ld	e,a
	ld	d,0	; de=flash offset
	add	hl,de
	ex	(sp),hl
	add	hl,de
	pop	de
	ex	de,hl	; hl-temp, de-perm
	pop	ix
immDetectNoMove:
	inc	de
	inc	de
	ld	c,(hl)
	inc	hl
	ld	b,(hl)
	inc	hl	; hl->data in ram
	push	bc
	push	ix
	pop	bc
immDetectCheck:
	ld	a,(bc)
	or	a
	jr	z,immDetectFound
	cp	(hl)
	inc	bc
	inc	de
	inc	hl
	jr	z,immDetectCheck
immDetectBad:
	pop	bc
immDetectNotEnough:
	pop	af
immDetectSkip:
	pop	hl
	ld	bc,-6
	add	hl,bc
	ld	b,(hl)
	dec	hl
immDetectNameLoop2:
	dec	hl
	djnz	immDetectNameLoop2
	jr	immDetect
immDetectFound:
	pop	hl
	; hl=size, de->data
	pop	af	; a=page, f=(or a)
	jr	z,immDetectInRam
	push	de	; data
	push	af
	push	hl
	bcall(_enoughRam)
	pop	bc
	jr	c,immDetectBad
	pop	af
	pop	hl
	ld	de,($9820)	; tempMem
	push	de
	bcall(_flashToRam)
	pop	de
immDetectInRam:	; de->data in RAM
	pop	hl	; hl->vat location
	ld	bc,-6
	add	hl,bc
	ld	b,(hl)
	inc	b
immDetectNameLoop1:
	dec	hl
	djnz	immDetectNameLoop1
	ex	de,hl
	xor	a
	ret

;-----> iDecompress data
; input:
;	hl->compressed data
;	de->place to load data
;	b=length of compressed data
;	c=compression factor (1, 3, or 15)
; output:
;	level is iDecompressed
;	hl->next byte of compressed data
immDecompress:
	call iCheckInts2
	push af
		di
immDecompressLoop:
		push	bc
		ld	a,(hl)
		ex	af,af'
		ld	a,c
		ld	b,8
		cp	1
		jr	z,dmmcmp1
		ld	b,4
		cp	3
		jr	z,dmmcmp1
		ld	b,2
dmmcmp1:	push	bc
		ld	a,c
		ld	b,1
		cp	1
		jr	z,dmmcmp2
		inc	b
		cp	3
		jr	z,dmmcmp2
		ld	b,4
dmmcmp2:	ex	af,af'
dmmcmp3:	rlca
		djnz	dmmcmp3
		ld	b,a
		ex	af,af'
		ld	a,b
		and	c
		ld	(de),a
		inc	de
		pop	bc
		djnz	dmmcmp1
		inc	hl
		pop	bc
		djnz	immDecompressLoop
		pop af
	ret po
;#ifdef enablecn2eis
	ei
;#endif
	ret

; End of Ion Library Routines by Joe Wingbermuehle
; Ion Extended Routines by Joe Wingbermuehle and Kerm Martian

immPutSpriteMask:
	ld	e,l
	ld	h,$00
	ld	d,h
	add	hl,de
	add	hl,de
	add	hl,hl
	add	hl,hl
	ld	e,a
	and	$07
	ld	c,a
	srl	e
	srl	e
	srl	e
	add	hl,de
	ld	de,gbuf
	add	hl,de
immPutSpriteLoop1Mask:
	ld	d,(ix)
	ld	e,$FF
	ld	a,c
	or	a
	jr	z,immPutSpriteSkip1Mask
immPutSpriteLoop2Mask:
	srl	d
	rr	e
	set 7,d
	dec	a
	jr	nz,immPutSpriteLoop2Mask
immPutSpriteSkip1Mask:
	ld	a,(hl)
	and	d
	ld	(hl),a
	inc	hl
	ld	a,(hl)
	and	e
	ld	(hl),a
	push ix
	dec hl
	ld de,8
	add ix,de
immPutSpriteLoop1AND:
	ld	d,(ix)
	ld	e,$00
	ld	a,c
	or	a
	jr	z,immPutSpriteSkip1AND
immPutSpriteLoop2AND:
	srl	d
	rr	e
	dec	a
	jr	nz,immPutSpriteLoop2AND
immPutSpriteSkip1AND:
	ld	a,(hl)
	xor	d
	ld	(hl),a
	inc	hl
	ld	a,(hl)
	xor	e
	ld	(hl),a

	pop ix
	ld	de,$0B
	add	hl,de
	inc	ix
	djnz	immPutSpriteLoop1Mask
	ret
	
; End of Ion Library Routines by Kerm Martian