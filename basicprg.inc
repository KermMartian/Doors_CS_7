;-----------------------------------------------------------
;	Filename:		basicprg.inc
;	Long name:  	Execute TI-BASIC Programs
;	Authors:		Kerm Martian aka Christopher Mitchell
;	Last Update:	Unknown
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

RunBASICProgram:
	
	in a,(2)
	rla
	sbc a,a
	out (20h),a
	ld hl,Op1
	ld de,parseVar
	ld bc,9
	ldir											;because DAVLCheck hoses Op1
	call DAVLCheck									;returns AppVarLoc

	xor a
	ld (hl),a
	bit 5, (iy + 34h)								;RawKeyHook
	jr z,NoHookToSave
	ld a,($9B86)
	ld (hl),a
	inc hl
	ld de,($9B84)
	ld (hl),e
	inc hl
	ld (hl),d
	dec hl
	dec hl
NoHookToSave:
	inc hl
	inc hl
	inc hl
	push hl
		in a,(6)
		ld hl,BASICKeyHook
		bcall(_EnableRawKeyHook)
		pop hl

	xor a											;hl is at offset +3 in appvar
	ld (hl),a
	bit 2, (iy + 36h)								;AppChangeHook	TODO FIX
	jr z,NoHookToSave2
	ld a,(appChangeHookPtr+2)
	ld (hl),a
	inc hl
	ld de,(appChangeHookPtr)
	ld (hl),e
	inc hl
	ld (hl),d
	dec hl
	dec hl
NoHookToSave2:
	inc hl
	inc hl
	inc hl
	push hl
		in a,(6)
		ld hl,BASICAppChangeHook
		bcall(_EnableAppChangeHook)
		pop hl

	xor a											;hl is at offset +6 in appvar
	ld (hl),a
	bit 1, (iy + 36h)								;ParserHook
	jr z,NoHookToSave3
	ld a,($9BAE)
	ld (hl),a
	inc hl
	ld de,($9BAC)
	ld (hl),e
	inc hl
	ld (hl),d
	res 1, (iy + 36h)	
NoHookToSave3:
#ifdef celtic3support
	ld de,AVOff_XLC3Libs
	ld hl,(AppVarLoc)
	add hl,de
	ld a,(hl)
	or a
	jr z,NoXLibCelticIIIHook
	ld hl,BASICParserHook
	bcall(_EnableParserHook)
	jr XLibCelticIIIHookFinish
#endif
NoXLibCelticIIIHook:
	in a,(6) \ dec a \ dec a						;WE ARE ON PAGE 0.  HOOK IS ON PAGE 2.
	ld hl,BASICParserHookSimple
	bcall(_EnableParserHook)						;Simple hook just catches Stop
XLibCelticIIIHookFinish:

	ld hl,parseVar
	rst 20h
	set progExecuting,(iy+newDispF)
	set cmdExec,(iy+cmdFlags)
	AppOnErr(ErrorHandler)
	set 7,(iy+28h)									;getKey on [2nd][on] (although TI-OS seems to reset at parseinp)
	bcall(_ParseInp)
	ld a,(CurFldr)
	or a
	jr z,BasicExecChkPause2
	ld hl,TextShadow
	ld b,128
BasicExecChkPause:
	ld a,(hl)
	cp ' '
	jp nz,BasicExecChkPause1
	inc hl
	djnz BasicExecChkPause
BasicExecChkPause2:
	AppOffErr
	xor a
ErrorHandler:
	and %01111111									;disable bit 7
	push af
;		bcall(_ReloadAppEntryVecs)
		ld a,1										;clear them all
		bcall(_dbfClearAnsStack_OffPage)			;clear the AnsStack, if open
		res progExecuting,(iy+newDispF)
		res cmdExec,(iy+cmdFlags)
		res grfSplit,(iy+sgrFlags)
		res TextWrite,(iy+sgrflags)					;just in case
		res TextInverse,(iy+textflags)
		pop bc
;FinishCont:
	push bc
	    set 3,(iy+33h)           ; ignoreBPlink          
        res 4,(iy+33h)           ; BPLinkOn      Disable link Assist for sure
        res 0,(iy+3Eh)           ;                Disables Silent linking supposedly  
        ld a,%00000001 
        out (08h),a 
		res 5, (iy + 34h)							;RawKeyHook
		res 2, (iy + 36h)							;AppChangeHook
		res 1, (iy + 36h)							;ParserHook
		res 7,(iy+28h)									;Off turns it OFF
		bcall(_ForceFullScreen)
		pop bc
RestoreHookStuff:
	push bc
		call DAVLCheck									;returns AppVarLoc
		ld     a,(hl)
		or     a
		pop bc
	jr z,RestoreHookStuff2
	push bc
		ld     a,(hl)
		push hl
			inc hl
			ld e,(hl)
			inc hl
			ld d,(hl)
			ex de,hl
			bcall(_EnableRawKeyHook)
			pop hl
		set 5, (iy + 34h)
		pop bc
RestoreHookStuff2:
	push bc
		inc hl
		inc hl
		inc hl							;offset +3
		ld     a,(hl)
		or     a
		pop bc
	jr z,RestoreHookStuff3
	push bc
		ld     a,(hl)
		push hl
			inc hl
			ld e,(hl)
			inc hl
			ld d,(hl)
			ex de,hl
			bcall(_EnableAppChangeHook)
			pop hl
		set 2, (iy + 36h)
		pop bc
RestoreHookStuff3:
#ifdef celtic3support
	push bc
		inc hl
		inc hl
		inc hl							;offset +6
		ld     a,(hl)
		or     a
		pop bc
	ret z
	push bc
		ld     a,(hl)
		push hl
			inc hl
			ld e,(hl)
			inc hl
			ld d,(hl)
			ex de,hl
			bcall(_EnableParserHook)
			pop hl
		set 1, (iy + 36h)
		pop bc
#endif
	ret

BasicExecChkPause1:
	call Pause
	jp BasicExecChkPause2

;--------------------------------------------------------------
BASICAppChangeHook:
	add a,e
	set 7,(iy+28h)						;getKey on [2nd][on]
	ret

BASICKeyHook:
	add    a,e
	set 7,(iy+28h)						;getKey on [2nd][on]
	push af
		cp kOff
		jr z,BASICKeyHookRetZ
		ld a,(cxCurApp)
		cp 50h
		jr nz,BASICKeyHookYield
		pop af
	push af
		cp kQuit
		jr nz,BASICKeyHookYield
BASICKeyHookRetZ:
		pop af
	xor a
	ret
BASICKeyHookYield:
		pop af
	ld b,a
	inc b
	cp b								;set nz
	ret

;--------------------------------------------------------------s
DCS5BASICAutoUpgrade:
	ld a,(CurROMPage)
	or a
	jr nz,DCS5BASICAutoUpgradeNo
	push hl
		dec hl
		dec hl
		ld a,(hl)
		cp '6'
		jr nz,DCS5BASICAutoUpgrade1
		dec hl
DCS5BASICAutoUpgrade1:
		dec hl
		dec hl
		dec hl
		dec hl
		push hl
			ld a,(hl)
			dec hl
			ld l,(hl)
			ld h,a
			ld de,8
			bcall(_mos_cphlde)
			jr c,DCSBASICAutoUpgrade_PrgmTooSmall
;			ex de,hl
			pop bc			;recall DAH of prgm size
		pop de				;recall current pointer in prgm
	push de					;save current pointer in prgm
		push bc				;save DAH of program size
			push hl			;save size of program
				ld hl,1
;				ex de,hl
				bcall(_insertmem)
				ld a,$2a
				ld (de),a
				pop de
			inc de			;increment size
			pop hl			;recall DAH of program size
		ld (hl),d
		dec hl
		ld (hl),e
		pop hl				;recall previous location
	ret
DCSBASICAutoUpgrade_PrgmTooSmall:			;less than 8 bytes long
			pop hl
		pop hl
DCS5BASICAutoUpgradeNo:						;in archive
	dec hl
	ret

;==============================================================
; SetErrOffset:
;    This function sets (ErrOffset) even if we are deep into
;    possible temporary variables. It is based on the OS code
;    that performs the same function for the OS 2:Goto option.
;--------------------------------------------------------------
SetErrOffset:
	ld a,(basic_prog+1)
	or a
	ret z
	bcall(_Op1ToOp3)
	ld hl,basic_prog
	rst 20h
	ld hl,(nextParseByte)
	ld de,(basic_start)
	or a
	sbc hl,de
SetErrOffset_FindLoop:
	ld (errOffset),hl
	ld a,(Op1+1)
	cp $24
	jr nz,SetErrOffset_ActualProgramName
	rst 10h
	ex de,hl
	inc hl
	inc hl
	ld a,(hl)
	or a
	jr z,SetErrOffset_EmptyProgramName
	push af
		inc hl
		push hl
			bcall(_ZeroOp1)
			pop ix
		ld hl,Op1+1
		ld b,(ix+0)
SetErrOffset_IXHLCopyLoop:
		inc ix
		ld a,(ix+0)
		ld (hl),a
		inc hl
		djnz SetErrOffset_IXHLCopyLoop
		inc ix
		pop af
	ld (Op1),a
	ld e,(ix+0)
	ld d,(ix+1)
	ld hl,(errOffset)
	add hl,de
	jr SetErrOffset_FindLoop
SetErrOffset_EmptyProgramName:
	bcall(_Op3toOp1)
	xor a
	ld (basic_prog+1),a
	ret
SetErrOffset_ActualProgramName:
	push hl
		ld de,basic_prog
		ld hl,Op1
		ld bc,10
		ldir
		pop hl
	ld (errOffset),hl
	bcall(_Op3toOp1)
	sub a
	inc a
	ret
