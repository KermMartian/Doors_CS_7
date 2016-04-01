;-----------------------------------------------------------
;	Filename:		detecttype.inc
;	Long name:  	Variable type detection
;	Authors:		Kerm Martian aka Christopher Mitchell
;	Last Update:	Unknown
;
; DetectType Routine
; Inputs:
;		hl = VAT entry pointer
; Outputs:
;		a  = ProgFlag (a=$ff means invalid)
;		ix = Program icon
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

DetectType:
	ld a,(hl)
	cp $06
	jr nz,DetectTypeNoLockSet
	set 0,(iy+dcsProgFlags)
DetectTypeNoLockSet:
	dec hl
#ifdef Folders
	ld a,(hl)
	ld (LastClick),a			;store folder of this program in LastClick
#endif
	dec hl
	dec hl
	push hl						;save point to DAL
		dec hl
		dec hl
		ld a,(hl)					;get page
		ld (CurROMPage),a			;save the ROM Page (0=in RAM)
		or a
		jr z,DetectTypeNoArcSet
		set 7,(iy+dcsProgFlags)
DetectTypeNoArcSet:
		pop hl						;recall pointer to DAL
	push hl						;save pointer to DAL again
		dec hl						
		dec hl
		dec hl
		dec hl
		ld a,(hl)					;first prog name byte
		cp $23
		jp z,VATInvalid
		cp $21
		jp z,VATInvalid
		pop hl
	push hl
#ifdef Folders
		push af					;save possible '%' name char
#endif
			ld a,(hl)
			dec hl
			ld h,(hl)
			ld l,a
			call SetUpROM
			call GetArcProgByte					;ldhlind->de for ram or rom
			ld e,a						;e...
			inc hl
			call GetArcProgByte
			ld d,a						;...d = prog length
			inc hl
			call GetArcProgByte				;Get type
			ld b,a
;-------------------------------------
#ifdef Folders
			ld a,(CurFldr)
			or a
			jr z,DetectTypeIgnoreFlds	;if running from a hook or runprog, ignore this
			push bc
				ld b,a
				ld a,(LastClick)
				cp b
				pop bc
			jp nz,VATIgnorePop
DetectTypeIgnoreFlds:
#endif
;--------------------------------------
			ld a,b
			cp $AB
			jp z,VATIgnorePop
#ifdef AnsHide
			cp $72
			jp z,VATIgnorePop
#endif
#ifdef Folders
			pop af
		cp '%'
		jp z,VATValidFld
#endif
;---------------------------------------------
		push hl
			call GetArcProgByte
			cp $BB
			jp nz,VATValid6
			inc hl
			call GetArcProgByte
			cp $6D
			jp nz,VATValid6
			inc hl
			call GetArcProgByte
			pop hl
VATValid2:
		cp $C9						;Ion Proprietary
		jp nz,VATValid3
VATValid2b:
		push hl
			inc hl
			inc hl
			inc hl
			call GetArcProgByte
			pop hl
		cp $30
		jr nz,VATValidMOS
VATValidIon:
		ld a,(iy+dcsProgFlags)
		or %00100100
		ld ix,PIconIon
		jp DetectTypeFinish
VATValidMOS:
		cp $31
		jr z,VATValidDCSAP
		ld de,IconSpace32b
		push de
			pop ix
		ld bc,32-2
		inc hl
		inc hl
		inc hl
		inc hl
		call ldirROMRAM					;fixed for ROM
		ld hl,IconSpace32b+32-2
		ld (hl),0
		inc hl
		ld (hl),0
		ld a,(iy+dcsProgFlags)
		or %00111110
		jp DetectTypeFinish
VATValidDCSAP:
		inc hl
		inc hl
		inc hl
		inc hl
		call GetArcProgByte
		cp $80
		jr nz,VATValidIon
		inc hl
		call GetArcProgByte
		ld b,a
		push bc
			inc hl
			call GetArcProgByte
			pop bc
		ld c,a
		push bc
			inc hl
			call GetArcProgByte
			ld d,a
			pop bc
		ld a,(CurROMPage)
		push af
			call DCSAPGetType				;should return name in Op1
			jr z,VATValidAPUnknown
			bcall(_chkfindsym)
			ld a,b
			ld (CurROMPage),a
			ex de,hl
			call SetUpROM
			push hl
				ld de,12
				add hl,de
				call GetArcProgWord
				or a
				ld de,$9d91
				sbc hl,de
				pop de
			ex de,hl
			add hl,de
			ld de,32
			add hl,de
			ld de,IconSpace32b
			push de
				ld bc,32
				call ldirROMRAM
				pop ix
VATValidAPUnknown1:
			pop af
		ld (CurROMPage),a
		ld a,(iy+dcsProgFlags)
		or %00001010
		jp DetectTypeFinish
VATValidAPUnknown:
		ld ix,CrashIcon
		jr VATValidAPUnknown1
VATValid3:
		cp $AF
		jp z,VATValid2b					;Ion non-p
VATValid4:
#ifdef AnsHide
		cp $72
		jr z,VatIgnore
#endif
		cp $AB						;ignored prog
		jr nz,VATValid5
		jr VatIgnore
VATIgnorePop:
#ifdef Folders
			pop af
#endif
VatIgnore:
		; previously pop'ed de and dec'ed it 6 bytes
VATInvalid:
		ld a,(ProgsToDo)
		inc a
		ld (ProgsToDo),a
		ld a,(ScratchVar)
		dec a
		ld (ScratchVar),a
		pop hl
	ld a,$ff						;invalid mask
	ret								;in VATProcessEntry, this will drop to VATValidSkip
VATValid5:
		cp $aa						;=xor d (dcsASM)
		jr nz,VATValid5b
		push de
			push hl
				pop bc
			ld de,8
			add hl,de
			push hl
				call GetArcProgWord				;get dcs version intended
				ld a,l							;stored BIG ENDIAN! High byte in l!
				pop hl
			pop de
		cp 8
		jr nc,VATIgnore
		inc hl
		inc hl
		call GetArcProgWord				;start icon init
		ld a,h
		or l
		jr z,VATValid5NoIcon
		ld de,-$9D95+2
		add hl,de
		add hl,bc
		ld de,IconSpace32b
		push de
			pop ix
		ld bc,32
		call ldirROMRAM					;fixed for ROM
		jr VATValid5Icon
VATValid5NoIcon:
		ld ix,PIconDCSasm
VATValid5Icon:
		ld a,(iy+dcsProgFlags)
		or %00101010
		jp DetectTypeFinish
VATValid5b:
		cp $AB
		jr z,VATIgnore
#ifdef AnsHide
		cp $72
		jr z,VATIgnore
#endif
VATValid5NoSkip:
		ld ix,PIconASMos
		ld a,(iy+dcsProgFlags)
		or %00001000
		jp DetectTypeFinish
VATValid6:
			pop hl
		ex de,hl
VATValid6Loopback:
		ld hl,dcsBASICtxt
		ld b,4
VATValid6Loop:
		call GetArcProgByteDE
		cp (hl)
		jr nz,VATValid7
		inc hl
		inc de
		djnz VATValid6Loop
		ex de,hl
		call GetArcProgByte
		inc hl
		cp '6'
		jr z,VATValid6HighRes
		cp $3F
		jr nz,VATValid7
		;hl=16 byte icon code
		call GetArcProgByte
		cp $2a
		call nz,DCS5BASICAutoUpgrade
		inc hl
		ld bc,8
		ld de,IconSpace8b
		call dcsSquish
		ld hl,IconSpace8b
		ld de,IconSpace32b
		push de
			call dcsEnlarge
			pop ix
		jr VATValid6Continue
VATValid6HighRes:
		inc hl
		call GetArcProgByte
		cp $2a
		call nz,DCS5BASICAutoUpgrade
		inc hl
		ld de,IconSpace32b
		push de
			ld bc,32
			call dcsSquish
			pop ix
VATValid6Continue:
		ld a,(iy+dcsProgFlags)
		or %00000010
		jr DetectTypeFinish
#ifdef Folders
VATValidFld:
		ld ix,FolderIcon
		ld a,(iy+dcsProgFlags)
		or %01001000
		jr DetectTypeFinish
#endif
VATValid7:
		ld a,(hl)
		cp 'D'
		jr nz,VATValid7_AxeSrcCheck			;check if it's an Axe source file
VATValid7Search:
		inc de
		call GetArcProgByteDE
		cp $3f
		jr nz,VATValid7Search
		inc de
		call GetArcProgByteDE
		cp $3e
		jr nz,VATValid71
		inc de
		call GetArcProgByteDE
		cp 'D'
		dec de
		jp z,VATValid6Loopback
		inc de
		cp $2a
		jr nz,VATValid71
		inc de
		ld hl,IconSpace32b
		push hl
			ex de,hl
			ld bc,32
			call dcsSquish
			pop ix
		jr VATValid72
VATValid71:
		ld ix,PIconBASICos
VATValid72:
		ld a,(iy+dcsProgFlags)
		jr DetectTypeFinish
VATValid7_AxeSrcCheck:
		call GetArcProgByteDE
		cp $3A						;the '.' symbol in TI tokens
		jr nz,VATValid71
		inc de
		call GetArcProgByteDE
		cp '0'
		jr c,VATValid7_IsAxeSrc
		cp '9'+1
		jr c,VATValid71
VATValid7_IsAxeSrc:
		ld ix,PIconBASICaxesrc
		jr VATValid72
DetectTypeFinishPopHL:
			pop hl
DetectTypeFinish:
		pop hl
	ret
	