;-----------------------------------------------------------
;	Filename:		mouse_p0.asm
;	Long name:  	Mouse routines for app page 0
;	Author:			Kerm Martian aka Christopher Mitchell
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

InfoPop:
	push bc
		ld hl,gbuf
		ld de,savesscreen
		ld bc,768
		ldir
		ld hl,(2*256)+39
		ld de,(92*256)+53					;ld de,(93*256)+54
;		ld a,$ff
		ld a,1
;		bcall(_DrawCustomRectangle)
		bcall(_mos_filledrectangle)
		ld hl,(3*256)+40
		ld de,(91*256)+52					;ld de,(92*256)+53
		ld a,$00
;		bcall(_DrawCustomRectangle)
		bcall(_mos_filledrectangle)
		ld hl,(40*256)+4
		ld (pencol),hl
		ld hl,InfoPopSize
		call vputsapp
		pop bc
	push bc
		ld a,b
		add a,a
		add a,a
		add a,b
		add a,b
		ld e,a
		ld d,0
		ld hl,VFAT
		add hl,de
		push hl
			bcall(_ldhlind)
			ld de,-5
			add hl,de
			ld a,(hl)
			ld (CurROMPage),a
			pop hl
		push hl
			inc hl
			inc hl
			bcall(_ldhlind)
			call SetUpROM
			push hl
				call GetArcProgWord
				call vdisphl
				ld hl,InfoPopSize2
				call vputsapp
				ld hl,(46*256)+4
				ld (pencol),hl
				ld hl,InfoPopSize3
				call vputsapp
				pop hl
			inc hl
			inc hl
			call GetArcProgByte
			cp $BB
			jp nz,MouseShowBASICCheckMOS
			inc hl
			call GetArcProgByte
			cp $6D
			jp nz,MouseShowBASIC
			inc hl
			call GetArcProgByte
			cp $aa
			jr z,MouseShowDCSASM
			cp $c9
			jp nz,MouseShowBASIC
			inc hl
			call GetArcProgByte
			inc hl
			inc hl
			cp $31
			jp z,MouseShowBASIC
			cp 1
			jr nz,MouseShowSkipMOS
			ld de,29
			add hl,de
MouseShowSkipMOS:
			;hl = Description
			;de = Destination
			ld de,Op1
			ld b,22
MouseShowASMCopy:
			call GetArcProgByte
			call DGetCharWidth
			push af
				ld a,c
				add a,b
				ld b,a
				pop af
			ld c,a
			ld a,91
			sub b
			jr c,MouseShowASMSkipEnd
			ld a,c
			ld (de),a
			inc hl
			inc de
			or a
			jr nz,MouseShowASMCopy
MouseShowASMSkipEnd:
			xor a
			ld (de),a
			ld hl,Op1
			jp MouseShowFinish
MouseShowDCSASM:
			inc hl
			call GetArcProgByte
			cp $c9
			jr nz,MouseShowBASIC
			inc hl
			inc hl
			inc hl
			push hl
				call GetArcProgWord
				ld a,h
				or l
				jr z,MouseShowDCSNoDesc
				ld de,$9d99
				or a
				sbc hl,de
				pop de
			add hl,de
			jr MouseShowSkipMOS
MouseShowBASICCheckMOS:
			cp $3e
			jr nz,MouseShowBASIC
			inc hl
			call GetArcProgByte
			cp $2a
			jr nz,MouseShowBASIC
			inc hl
			ld de,Op1
			push de
MouseShowBASICMOSLoop:
				push hl
					call GetArcProgWord
					ld a,l
					ld (ScratchWord),hl
					pop hl
				cp $3f
				jr z,MouseShowBASICMOSDone
				push af
					push hl
						push de
							ld hl,ScratchWord
							bcall(_Get_Tok_Strng)
							ld hl,Op3
							pop de
						ldir
						pop hl
					inc hl
					pop af
				bcall(_IsA2ByteTok)
				jr nz,MouseShowBASICMOSLoop
				inc hl
				jr MouseShowBASICMOSLoop
MouseShowBASICMOSDone:
				xor a
				ld (de),a
				pop hl
			jr MouseShowFinish
MouseShowDCSNoDesc:
				pop hl
MouseShowBASIC:
			ld hl,Op1
			ld (hl),0
			push hl
			pop de
			inc de
			ld bc,10
			ldir
			pop hl
		push hl
			call RunNameCopy
			xor a
			ld (Op1+9),a
			ld hl,Op1+1
			ld a,(hl)
			cp $25
			jr nz,MouseShowFinish
			inc hl
			ld a,(hl)
			dec hl
			cp 'F'
			jr nz,MouseShowFinish
			ld hl,FolderTextString
MouseShowFinish:
			ld a,(hl)
			cp $27
			jr nc,MouseShowFinish_NotHidden
			add a,40
			ld (hl),a
MouseShowFinish_NotHidden:
			call vputsapp
			call ifastcopy
			;call MOUSE_CLRMSEForce
			pop hl
		pop bc
MouseShowWait:
	call Cn2GetK_NBD
	or a
	jr nz,MouseShowWaitFinish
	ld hl,(APDnext)
	inc hl
	ld (APDnext),hl
	ld a,h
	and h
	sub $a0
	jr nz,MouseShowWait
MouseShowWaitFinish:
	xor a
	ld (SETable+4+2),a		;SE SHELL EXPANSIONS: Indicate that SECache is CLEARED!!
	ld hl,savesscreen
	ld de,gbuf
	ld bc,768
	ld a,b				;b = 768/256 = 3
	ld (dAPDtimer),a
	ldir
	jp iFastCopy

MemoryPop:
	ld hl,gbuf
	ld de,savesscreen
	ld bc,768
	ldir

	ld hl,(31*256)+43
	ld de,(91*256)+57					;ld de,(92*256)+58
;	ld a,$ff
	ld a,1
;	bcall(_DrawCustomRectangle)
	bcall(_mos_filledrectangle)

	ld hl,(32*256)+44
	ld de,(90*256)+56					;ld de,(91*256)+57
;	ld a,$00
	xor a
;	bcall(_DrawCustomRectangle)
	bcall(_mos_filledrectangle)

	ld hl,(44*256)+33	;y = 44, x = 33
	ld (pencol),hl
	ld hl,MemoryPop_RAM
	bcall(_vputsapp)
	bcall(_memfree)
	bcall(_vdisphl)
	ld hl,MemoryPop_Slash
	bcall(_vputsapp)
	ld hl,MemoryPop_24576
	bcall(_vputsapp)
	ld hl,(50*256)+33	;y = 50, x = 33
	ld (pencol),hl
	ld hl,MemoryPop_ARC
	bcall(_vputsapp)
	bcall(_ChkFreeArc)
	ld hl,0839Fh
	ld d,(hl)
	inc hl
	ld e,(hl)
	inc hl
	ld a,(hl)	;don't care about low byte @$83A2 though
	srl d
	rr e
	rra			;divide hla by two	(=256*2 = 512)
	srl d
	rr e
	rra			;divide hla by four		(=256*4 = 1024)
	ld h,e
	ld l,a
	bcall(_vdisphl)
	ld hl,MemoryPop_KSlash
	bcall(_vputsapp)
	bcall(_getHardwareVersion)
	or a
	ld hl,160		; version 0 -> 83+, 512 kB (160kB user-accessible)
	jr z,MouseShowMemoryROMFound
	cp 2
	ld hl,480		; version 2 -> 84+, 1024 kB (480kB user-accessible)
	jr z,MouseShowMemoryROMFound
	ld hl,1536		; version 1/3 -> 83+/84+ SE, 2048 kB (1.5mB user-accessible)
MouseShowMemoryROMFound:
	bcall(_vdisphl)
	ld hl,MemoryPop_K
	bcall(_vputsapp)
	call ifastcopy
	;call MOUSE_CLRMSEForce
	jp MouseShowWait

;------------------------------------------------------------------------------------------------
; TabFunc Routines

TabFunc_GetCoords:					;puts (x,y,x,y) in (h,l,d,e)
	ld hl,TabFuncAreas_0			;for a given offset a=[0-10]
	ld a,(MouseMode)
	or a
	jr z,TabFunc_GetCoords_1
	ld hl,TabFuncAreas_1
TabFunc_GetCoords_1:
	ld a,(TabFuncMode)
	add a,a
	add a,a							;In case it ever gets that big (but shouldn't),
	ld e,a							;note that a must be <= 63.
	ld d,0
	add hl,de
	ld b,(hl)
	inc hl
	ld c,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld e,(hl)
	push bc
		pop hl
	ret

TabFunc_Tab:
	ld a,(TabFuncMode)
	push af
		inc a
		jr z,TabFunc_Tab_NoErase
		call TabFunc_GetCoords
		ld a,2
		bcall(_mos_filledrectangle)
TabFunc_Tab_NoErase:
		pop af							;b = (TabFuncMode)
	push af
		ld a,(MouseMode)
		add a,11
		ld b,a
		pop af
	inc a
	cp b
	jr nz,TabFunc_Tab_Set
	xor a
TabFunc_Tab_Set:
	ld (TabFuncMode),a
	call TabFunc_GetCoords
	ld a,2
	bcall(_mos_filledrectangle)
	call iFastCopy
	ret

TabFunc_ShiftTab:
	ld a,(TabFuncMode)
	push af
		inc a
		jr z,TabFunc_ShiftTab_NoErase
		call TabFunc_GetCoords
		ld a,2
		bcall(_mos_filledrectangle)
TabFunc_ShiftTab_NoErase:
		pop af							;b = (TabFuncMode)
	dec a
	bit 7,a						;0xff (was at 0) or 0xfe (tabfuncs was disabled)
	jr z,TabFunc_ShiftTab_Set
	ld a,(MouseMode)
	add a,10
TabFunc_ShiftTab_Set:
	ld (TabFuncMode),a
	call TabFunc_GetCoords
	ld a,2
	bcall(_mos_filledrectangle)
	call iFastCopy
	ret
