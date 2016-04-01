;-----------------------------------------------------------
;	Filename:		DCSlibsP.inc
;	Long name:  	Doors CS-specific libs for the TI-83+/84+
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	May 30, 2006
;	Routines Included:
;		-VDispA
;		-ClrDialogFull
;		-ClrWinFull
;		-SmallWindow
;		-LargeWindow
;		-hdetect
;		-DAVLCheck
;		-PutsApp
;		-VputsApp
;		-ReadAppVarWord
;		-DrawCustomRectangle
;		-MultADE
;		-DivHDE
;		-VertLine
;		-DispLongInt
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

VDispA:
	bcall(_setxxop1)
	ld a,3
	bcall(_dispop1a)
	jr VDispFinish
VDispHL:
	bcall(_setxxxxop2)
	bcall(_op2toop1)
	ld a,5
	bcall(_dispop1a)
VDispFinish:
	push hl
		ld hl,pencol
		dec (hl)
		dec (hl)
		pop hl
	ret
;--------------------------------------------------
ClrDialogFull:
	ld hl,(9*256)+16
	ld de,(86*256)+54						;was ld de,(87*256)+55
	bcall(_mos_filledrectangle)
;	bcall(_DrawCustomRectangle)
	jp iFastCopy
;--------------------------------------------------
ClrWinFull:
	ld hl,(1*256)+7
	ld de,(93*256)+62						;was ld de,(94*256)+63
	xor a
	bcall(_mos_filledrectangle)
	jp iFastCopy
;--------------------------------------------------
SMALL_WINDOW:					;draws a small window
	ld hl,(8*256)+8
	ld de,(87*256)+55						;was ld de,(94*256)+63
	xor a
	bcall(_mos_filledrectangle)
SMALL_WINDOW_NOERASE:				;Now draw the window
	ld b,8					;saved 22 bytes here
	ld c,b
	ld d,88
	ld e,b
	ld h,1
	bcall(_ILine)				;first line
	ld c,48
	ld e,c
	bcall(_ILine)				;second line
	ld c,56
	ld e,c					;ld e,54 : save 1 byte and 3 clocks [°]
	bcall(_ILine)				;third line
	ld c,b
	ld d,b					;ld d,16 : save 1 byte and 3 clocks [°]
	bcall(_ILine)				;fourth line
	ld b,88
	ld c,8
	ld d,b					;ld d,80 : save 1 byte and 3 clocks [°]
	bcall(_ILine)				;fifth line
	ld ix,XBox
	ld a,81
	ld l,8
	ld b,7
	jp iPutSprite
;--------------------------------------------------
LargeWindow:
	bcall(_ClrLCDFull)
	ld hl,PlotsScreen
	ld b,64
	push hl
		ld de,11
LargeWindowLoop1:
		ld (hl),$80
		add hl,de
		ld (hl),$01
		inc hl
		djnz LargeWindowLoop1
		pop hl
	ld b,12
LargeWindowLoop2:
	ld (hl),$FF
	inc hl
	djnz LargeWindowLoop2
	ld b,12
	ld hl,PlotsScreen+(8*12)
LargeWindowLoop3:
	ld (hl),$FF
	inc hl
	djnz LargeWindowLoop3
	ld b,12
	ld hl,PlotsScreen+(63*12)
LargeWindowLoop4:
	ld (hl),$FF
	inc hl
	djnz LargeWindowLoop4
	ld ix,XBox
	ld a,88
	ld l,1
	ld b,7
	jp iPutSprite
XBox:
	.db %11111110
	.db %10000010
	.db %10101010
	.db %10010010
	.db %10101010
	.db %10000010
	.db %11111110
;--------------------------------------------------
;;;;;;;;
; Sound-producing loops
; 26 * HL cycles with the DLoop
; 80 cycles with the rest of the soundloop

PlaySound:
        PUSH HL                 ; 11
DLoop:
        DEC  HL         ;  6
        LD   A, H       ;  4
        OR   L          ;  4
        JR   NZ, DLoop  ; 12    ;  7
        POP  HL                 ; 10
Play:
        LD   A, B               ;  4
        XOR  00000011b          ;  7
        LD   B, A               ;  4
        OUT  (0), A             ; 11
        DEC  DE                 ;  6
        LD   A, D               ;  4
        OR   E                  ;  4
        JR   NZ, PlaySound      ; 12
DonePlay:
	ret
;----------Hot spot detection-----------
;inputs: b,c (first x and y cor)
;        d,e (last x and y cor)
;        h,l (current x,y)
;output: "z" flag [either true (set) or false (reset)]

hdetect:
	ld a,h
	cp b
	jr c,hno_detect
	cp d
	jr nc,hno_detect
	ld a,l
	cp c
	jr c,hno_detect
	cp e
	jr nc,hno_detect
hyes_detect:
	xor a
	or a
	ret
hno_detect:
	xor a
	cp 1
	ret
Pause:
	;bcall(_Cn2GetK)
	call Cn2GetK
	or a
	jr z,Pause
	ret
;--------------------------------------------------
DAVLCheck:
	ld hl,AppVarName
	rst 20h
	bcall(_chkfindsym)
	jr nc,DAVLExists
	ld hl,AVBackupNameP1
	rst 20h
	bcall(_chkfindsym)
	jr nc,DAVLBackupFound
	ld hl,AppVarName
	rst 20h
	ld hl,AppVarSize
	bcall(_createappvar)
	inc de
	inc de
	push de									;the following indent does an initial clean
		ld bc,AppVarSize-1					;@start+0
		xor a
		ld (de),a
		push de
		pop hl
		inc de
		ldir
		pop de								;@start+0
	ld hl,9+17
	add hl,de								;@start+26
	ld a,1
	ld (hl),a
	inc hl									;@start+27
	ld (hl),0
	ld de,5
	add hl,de								;@start+32
	ld bc,DAVLData_32p_End-DAVLData_32p
	ld de,DAVLData_32p
	ex de,hl
	ldir
	ld hl,MOUSE_SPRITE						;@start+40
	ld bc,16
	ldir
	ld hl,10								;@start+56
	add hl,de
	ld (hl),15								;@start+66	(acceleration)
	inc hl
	inc hl
	ld (hl),a								;@start+68	;a=1
	inc hl
	inc hl
	ld (hl),a								;@start+70	;a=1
	ld de,5
	add hl,de
	ld (hl),a								;@start+75	;a=1: Parserbook ENABLED
;	inc hl
;	inc hl									;think nothing was here before
	bcall(_OpenGUIStack)
	bcall(_grbufclr)
	bcall(_AboutDisplay)
	bcall(_SMHelpOffpage)
	bcall(_CloseGUIStack)
	bcall(_BackupAppVar)
	call FldSave
	jr DAVLCheck
DAVLExists:
	ld a,b
	or a
	jp nz,DAVLArced
	ex de,hl
	push hl
		bcall(_ldhlind)
		ld de,AppVarSize
		bcall(_cphlde)
		jp nz,DAVLSizeWrong
		pop hl
	inc hl
	inc hl
	ld (AppVarLoc),hl
	ret
DAVLBackupFound:
	ld a,(CurROMPage)
	push af
		ld a,b
		ld (CurROMPage),a
		ex de,hl
		call SetUpROM
		push hl
			ld hl,AppVarName
			rst 20h
			ld hl,AppVarSize
			bcall(_createappvar)
			pop hl
		ld bc,AppVarSize+2
		call ldirROMRAM					;copy to buffer
		pop af
	ld (CurROMPage),a
	;check crashing here
	bcall(_GUIDrawHourGlass)
	bcall(_grbufclr)
	ld ix,CrashIcon
	ld a,40
	ld bc,(16*256)+2
	ld l,c				;l=2
	call iLargeSprite
	ld hl,(25*256)+1
	ld (pencol),hl
	ld hl,CrashTxt1
	call vPutsApp
	ld hl,(31*256)+1
	ld (pencol),hl
	ld hl,CrashTxt2
	call vPutsApp
	ld hl,(43*256)+1
	ld (pencol),hl
	ld hl,CrashTxt3
	call vPutsApp
	ld hl,(56*256)+2
	ld (pencol),hl
	ld hl,CrashTxt4
	call vPutsApp
	call FldRestore
	ld hl,(37*256)+1
	ld (pencol),hl
	ld a,(ScratchVar)
	or a
	jr nz,DAVLBackupFldrs0
	ld hl,CrashTxt_Fld0
	jr DAVLBackupFldrsFinish
DAVLBackupFldrs0:
	dec a
	jr nz,DAVLBackupFldrs1
	ld hl,CrashTxt_Fld1
	jr DAVLBackupFldrsFinish
DAVLBackupFldrs1:
	ld hl,CrashTxt_Fld2
DAVLBackupFldrsFinish:
	call vPutsApp
	ld hl,CrashTxt_FldFinish
	call vPutsApp
	call iFastCopy
	call Pause
	jp DAVLCheck
DAVLSizeWrong:
		ld hl,AppVarName
		rst 20h
		bcall(_chkfindsym)
		bcallnc(_delvararc)
		ld hl,AVBackupNameP1
		rst 20h
		bcall(_chkfindsym)
		bcallnc(_delvararc)
		pop hl
	jp DAVLCheck
DAVLArced:
	call Arc_Unarc
	jp DAVLCheck
DAVLCheckOffset:
	push hl
	call DAVLCheck
	pop de
	add hl,de
	ret
DAVLData_32p:
	.db 0,1,1,1,0,180		;180 -> 9
	.db 0,0			;these are dummy values
DAVLData_32p_End:
AVBackupNameP1:
	.db 15h,"DCS7b",0
;-----------------------------------------------
#ifdef false
SetCrashContext:
	push af
	push hl
	push de
	push bc
	ld hl,67
	call DAVLCheckOffset
	ld (hl),1
	pop bc
	pop de
	pop hl
	pop af
	ret
ResetCrashContext:
	push af
	push hl
	push de
	push bc
	ld hl,67
	call DAVLCheckOffset
	ld (hl),0
	pop bc
	pop de
	pop hl
	pop af
	ret
#endif
;-----------------------------------------------
putsApp:
	PUSH BC
	PUSH AF
	LD A,(winBtm)
	LD B,A ; B = bottom line of window
PutS10:
	LD A,(HL) ; get a character of string name
	INC HL
	OR A ; end of string?
	SCF ; indicate entire string was displayed
	JR Z, PutS20 ; yes --->
	bcall(_PutC) ; display one character of string
	LD A,(curRow) ; check cursor position
	CP B ; off end of window?
	JR C,PutS10 ; no, display rest of string
PutS20:
	POP BC ; restore A (but not F)
	LD A,B
	POP BC ; restore BC
	RET
;-----------------------------------------------
vputsApp:
	push af
		push de
			push ix
VPutS10:
				ld a,(hl) ; get a character of string name
				inc hl
				or a ; end of string?
				jr z, VputS20 ; yes --->
				push hl
					ld e,0
					call DPutMap ; display one character of string
					pop hl
				jr nc, VPutS10 ; display rest of string IF FITS
VputS20:
				pop ix
			pop de
		pop af
	ret
;-----------------------------------------------
#ifdef false
ReadAppVarWord:
	push de
		push hl
			;a=offset from start
			ld d,0
			ld e,a
			ld hl,(AppVarLoc)
			add hl,de
			ld a,(hl)
			inc hl
			ld h,(hl)
			ld l,a
			pop hl
		pop de
	ret
#endif
ReadAppVarByte:
	push de
		push hl
			;a=offset from start
			ld d,0
			ld e,a
			ld hl,(AppVarLoc)
			add hl,de
			ld a,(hl)
			pop hl
		pop de
	ret

;-----------------------------------------------
ArcUnarcASM:
	push ix
	pop hl
	call RunNameCopy
	ld a,b
	ld (CurROMPage),a
	push bc
		ex de,hl
		push hl
			call SetUpROM
			call GetArcProgWord
			push hl
				ld hl,TmpProgName
				rst 20h
				bcall(_chkfindsym)
				jr c,RunASMProgNoneToDel
				bcall(_delvararc)
RunASMProgNoneToDel:
				ld hl,TmpProgName
				rst 20h
			pop hl
			push hl
				bcall(_createprog)
				inc de
				inc de
			pop bc
		pop hl
		call SetUpROM
		inc hl
		inc hl
	pop af
	or a
	jr z,RunASMProgNoPageInc
	bit 7,h
	jr z,RunASMProgNoPageInc
	inc a
	res 7,h
	set 6,h
RunASMProgNoPageInc:
	bcall(_flashtoram)
	ret
	
DispLongInt:
	;hl => 3-byte integer
	ld e,(hl)
	inc hl
	push de
		bcall(_ldhlind)
		push hl
			bcall(_ZeroOp1)
			ld hl,Op1+8
			ld (hl),0
			ld b,h
			ld c,l
			pop hl
		pop de
	ld d,10
	;ehl = X
	;d = 10
	;bc = where to put it
DispLongIntLoop:
	dec bc
	push bc
	call DivEHLbyD
	ld b,'0'
	add a,b
	pop bc
	ld (bc),a			;store the digit
	ld a,e
	or h
	or l
	jr nz,DispLongIntLoop
	push bc
	pop hl
	jp VPutsApp
DivEHLbyD:
	;Inputs:
	;	EHL = X
	;	D = Y
	;	A = 0
	;Outputs:
	;	EHL = X/Y
	;	A = Remainder
	ld b,24
	xor a
DivEHLbyDLoop:
	add	hl,hl		; unroll 24 times
	rl	e		; ...
	rla			; ...
	cp	d		; ...
	jr	c,$+4		; ...
	sub	d		; ...
	inc	l		; ...
	djnz DivEHLbyDLoop
	ret
SetSpeedFast:
	in a, (2)
	and 80h
	ret z															;%10000000 or %00000000
	rlca ;Move the 1 bit in bit 7 to bit 0 (80h -> 01h)				;%00000001 or %00000000
	out (20h), a
	ret
SetSpeedSlow:
	in a, (2)
	and 80h
	ret z
	xor a
	out (20h), a
	ret
	
	
; Play the tone dur=bc, period = h,l,d,e (4 channel sound - h,l = left speaker, d,e = right speaker)
#ifdef false
playTone:
.relocate gbuf
	di
	xor a
	ld (toneMask1+1),a
	ld (toneMask2+1),a
	inc b 
; INIT CHANNEL A

	ld a,h
	ld (toneAPitch+1),a
	or a
	jr z,isRestA
	ld a,1
	jr notRestA
isRestA:
	xor a
notRestA:
	ld (toneAChange+1),a

; INIT CHANNEL B

	ld a,l
	ld (toneBPitch+1),a
	or a
	jr z,isRestB
	ld a,1
	jr notRestB
isRestB:
	xor a
notRestB:
	ld (toneBChange+1),a


; INIT CHANNEL C

	ld a,d
	ld (toneCPitch+1),a
	or a
	jr z,isRestC
	ld a,2
	jr notRestC
isRestC:
	xor a
notRestC:
	ld (toneCChange+1),a

; INIT CHANNEL D

	ld a,e
	ld (toneDPitch+1),a
	or a
	jr z,isRestD
	ld a,2
	jr notRestD
isRestD:
	xor a
notRestD:
	ld (toneDChange+1),a

toneMaskPreserve:
	push bc
toneMask:
	and 1
	jr z,playPart2
	ld a,(toneMask1+1)
	jr playPart1
playPart2:
	ld a,(toneMask2+1)
playPart1:
	out (bPort),a
pitchLoop:
	dec h
	jr nz,noPitchA
toneMask1:
	ld a,0
toneAChange:
	xor 0
	ld (toneMask1+1),a
toneAPitch:
	ld h,0
noPitchA:
	dec l
	jr nz,noPitchB
toneMask2:
	ld a,0
toneBChange:
	xor 0
	ld (toneMask2+1),a
toneBPitch:
	ld l,0
noPitchB:
	dec d
	jr nz,noPitchC
	ld a,(toneMask1+1)
toneCChange:
	xor 0
	ld (toneMask1+1),a
toneCPitch:
	ld d,0
noPitchC:
	dec e
	jr nz,noPitchD
	ld a,(toneMask2+1)
toneDChange:
	xor 0
	ld (toneMask2+1),a
toneDPitch:
	ld e,0
noPitchD:

extendDuration:
	ld a,0
	dec a
	ld (extendDuration+1),a
	jr nz,toneMask

	ld a,$BF
	out (1),a
	in a,(1)
	cp $00									;basically disables this
	jr nz,notTimeToQuit

	pop hl
	pop hl
	pop hl
	ret
notTimeToQuit:
	pop bc
	dec c
	jp nz,toneMaskPreserve
	dec b
	jp nz,toneMaskPreserve
	ret
.endrelocate
playToneEnd:
#endif
;-----------------------------------------------

Cn2GetK_NBD:		;GetK with no debouncing
	push hl
		push de
			push bc
				call Cn2GetK_Sub
				pop bc
			pop de
		pop hl
	ret

Cn2GetK:
Cn2GetCSC:
	push hl
		push de
			push bc
				call Cn2GetK_Sub
				push af
					ld b,6
GetKDebounceL:
					ld a,0ffh				;reset
					out (c),a
					nop
					nop
					out (c),e				;enable
					nop \ nop
					in a,(c)				;get the value
					inc a
					jr nz,GetKDebounceL
					djnz GetKDebounceL
					pop af
				pop bc
			pop de
		pop hl
	ret

Cn2GetK_Sub:
	ld a,$ff		;
	out (1),a		;reset keyport
	ld e,$fe		;frist group
	ld c,$01		;key port
	ld l,0		;l holds key pressed
cscloop:
	out (c),e		;set keygroup
	ld b,8		;loop, Delay needed when work with key driver
	nop \ nop
	in a,(c)		;read key
cscbit:
	inc l			;inc to get key pressed
	rra 			; if key pressed done
	jr nc,donecsc
	djnz cscbit 	;loop 8
	rlc e			;next key group
	jp m,cscloop	;if bit 7 set loop
	ld l,0		;if no key pressed 0
donecsc:
	ld a,l		;
	or a
	ret

DisableLaunchKeyhook:					;don't want Keyhook running inside DCS!
	ld hl,AVOff_OnPrgmHook
	call DAVLCheckOffset
	ld a,(hl)
	or a
	ret z
	bit 5,(iy+34h)
	ret z								;nothing doing :)
	ld a,(9B86h)						;the page
	inc a
	ld b,a
	in a,(6)
	cp b
	ret nz		;it wasn't our hook
	res 5,(iy+34h)						;disable the hook
	inc hl
	push hl
		bcall(_ldhlind)
		ld a,h
		or l
		pop hl
	ret z	;wasn't anything there before
	ld de,9B84h
	ld bc,4
	ldir
	set 5,(iy+34h)
	ret

DisableMyParserhook:					;don't want Parserhook running inside DCS either!
	ld hl,AVOff_ParserHook
	call DAVLCheckOffset
	ld a,(hl)
	or a
	ret z
	bit 1,(iy+36h)
	ret z								;nothing doing :)
	ld a,(9BACh+2)						;the page is AFTER the address! [dal dah page] = [+0 +1 +2]
	inc a
	inc a								;it's on page [0 1 2] 2 of the app
	ld b,a
	in a,(6)
	cp b
	ret nz		;it wasn't our hook
	res 1,(iy+36h)						;disable the hook
	inc hl
	push hl
		bcall(_ldhlind)
		ld a,h
		or l
		pop hl
	ret z	;wasn't anything there before
	ld de,9BACh
	ld bc,4
	ldir
	res 1,(iy+36h)
	ret

SPSave:
	push ix
		push hl
			ld hl,AVOff_SPSave			;Save SP
			call DAVLCheckOffset
			ex de,hl
			ld hl,0
			add hl,sp
			push de
				ld de,0+6				;+4 are for ix and hl, +2 are for the call
				add hl,de
				pop de
			ex de,hl					;switch hl and de
			ld (hl),e					;put first byte
			inc hl						;and then
			ld (hl),d					;second byte in
			pop hl
		pop ix
	ret

DisableLaunchOffscrpt:
	ld hl,OffScriptName
	rst 20h
	bcall(_chkfindsym)
	bcallnc(_delvararc)
	ret

EnableLaunchOffscrpt:
	ld hl,AVOff_Offscript
	call DAVLCheckOffset
	ld a,(hl)
	or a
	ret z
	ld hl,OffScriptName
	rst 20h
	bcall(_chkfindsym)
	ret nc
	ld hl,OffScrptCodeEnd-OffScrptCode
	bcall(_enoughmem)
	ret c
	ex de,hl
	push hl
		bcall(_createappvar)
		inc de
		inc de
		ld hl,OffScrptCode
		pop bc
	ldir
	set 1,(iy+33h)
	ret

OffScrptCode:
.relocate $8001
  bit 4,(IY+apdFlags)
  ret nz
  ld hl,MyAppNameOffScrpt
  rst 20h
  bcall(_FindApp)
  ret c
  dec a		;this is on page 2
  ld hl,MyGetKeyHook
  bcall(_EnablegetKeyHook)
  ;set 0,(iy+34h)
  ret
MyAppNameOffScrpt:
  .db AppObj,"DoorsCS7"
  ret
.endrelocate
OffScrptCodeEnd:

OffScriptName:
	.db 15h,"OFFSCRPT"


calcAndStore_OSChecksum:
	ld ix, userMem
	ld bc, 60DBh
	ld hl, 11h
	ld d, h
loc_C31:                                ; CODE XREF: calculateOSChecksum+16
	ld e, (ix+0)
	inc ix
	add hl, de
	dec bc
	ld a, c
	or b
	jr nz, loc_C31
	ld e, d
	ex de, hl
	sbc hl, de
	ld (RAMChecksum),hl
	ret
