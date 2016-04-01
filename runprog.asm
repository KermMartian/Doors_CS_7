;-----------------------------------------------------------
;	Filename:		runprog.asm
;	Long name:  	Run ASM and BASIC programs
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	Unknown
;
; Code necessary to set up, execute, and clean up after
; assembly and BASIC programs. Also handles some things with
; HomeRun and the properties menu.
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

RunProgDeny:
	ld a,(LastClick)
	sub 2
	jp nz,MainMouseLoop
	push ix
		pop hl
	ld bc,5
	add hl,bc
	xor a
	ld (hl),a
	ld b,a
	jp PropMenu

mj_RunProg:
RunProgFromHook:
	;takes VAT location in hl
	res graphDraw,(iy+graphFlags)		;make BASIC programs *NOT* clear the graphscreen
	push hl
		call DAVLCheck
		pop hl
	ld de,VFAT
	push de
		push hl
			ld a,l
			ld (de),a
			inc de
			ld a,h
			ld (de),a
			inc de
			dec hl
			ld a,(hl)
			dec hl
			dec hl
			ld a,(hl)
			ld (de),a
			inc de
			dec hl
			ld a,(hl)
			ld (de),a
			inc de
			dec hl
			ld a,(hl)
			ld (de),a
			inc de
			pop hl
		push de
			xor a
			ld (iy+dcsProgFlags),a
			ld (CurFldr),a			;used for the fldr of current prog, 0 means don't check it
			call DetectType
			pop de
		ld (de),a
		xor a
		ld (LastClick),a			;don't trigger the prop menu
		pop ix
	cp $ff
	ret z
	ld (iy+dcsProgFlags),a
	jr RunProgImmediate

RunProgFromDesktop:
	set graphDraw,(iy+graphFlags)		;make BASIC programs clear the graphscreen
RunProg:
	ld a,(ProgsDone)
	ld c,a
	ld a,(TotalProgs)
	sub c
	bit 7,a
	jr nz,RunProgContinue
	inc b
	sub b
	jp c,RunProgDeny
	inc a
RunProgContinue:
	push af
		push bc								;save screen index
			push ix
				push ix
					push ix
						pop hl
					call RunNameCopy
					bcall(_PushOp1)
						call FldSave
						bcall(_PopOp1)
					bcall(_ChkFindSym)
					ex de,hl
					push hl
						pop bc
					pop ix
				pop hl
			ld (hl),e
			inc hl
			ld (hl),d
			inc hl
;			push bc
;				pop de
			ld (hl),c
			inc hl
			ld (hl),b
			pop bc
		pop af								;recall run offset!
RunProgFromDesktop_NoFldBack:
	call RunProgImmediate
	;jp c,RenderDesktop
	jp RealStart
	
RunProgImmediate:
	call SPSave
	
	ld (IconSpace8b),hl			;save VFAT entry for future use
	ld b,1
	ld a,(LastClick)
	sub 2
	jp z,PropMenu					;deal with rightclick
	push ix					;put ix...
	
		call PushProgChain		;push the previous program run upwards
								;in case we are running nested programs
	
		pop hl					;...into hl (==VFAT+6(N-1))
	ld b,1
	push hl
		bcall(_ldhlind)
		dec hl
		dec hl
		dec hl
		ld d,(hl)
		dec hl
		ld e,(hl)					;de = VAR entry
		ex de,hl
		ld (ScratchWord),hl			;save it
		ex de,hl
		pop hl
	push hl
		ld de,4
		add hl,de
		ld a,(hl)
		ld (CurROMPage),a			;should be correct for PropDelete routine
		inc hl
		ld a,(hl)					;get prog type flags
		ld (iy+dcsProgFlags),a
		
		ld hl,(45*256)+32			;initialize mouse coordinates
		ld (MseY),hl				;x=45,y=32
		
#ifdef false
		ld hl,textshadow;cmdshadow
		ld (hl),' '
		ld e,l
		ld d,h
		inc de
		ld bc,128-1
		ldir
#endif

		pop hl
	and $7e						;mask out lock/unlock bit
	or a
	jr z,RunBasicProg			;Regular BASIC program
	cp 2
	jr z,RunBasicProgDCS		;Doors CS BASIC program
	cp $30
	jp z,RunTIOSASM				;TI-OS program
	cp $08
	jp z,RunTIOSASM				;Compressed TI-OS ASM program
	cp $24
	jp z,RunIonProg				;either Ion type
	cp $2A
	jp z,RunDCSasmProg
	cp $3E
	jp z,RunMOSasmProg
	cp $0A
	jp z,RunAssociatedProg
#ifdef Folders
	cp %01001000
	jp z,RunFldr
#endif
	ld hl,501
	call DCSError
	jp PopProgChainOrAAndRet
;	call PopProgChain
;	or a			;set carry flag
;	ret

ArcUnarcProgA:
	call RunNameCopy
ArcUnarcProgA_Op1:					;Op1 is still intact after the chkfindsym bcall
	push hl
		push de
			push bc
				bcall(_pushop1)				;all destroyed
				call GetProgChainTop
				pop bc
			ret c							;I hope this doesn't happen; I don't think it would be graceful
			ld de,10
			add hl,de
			ld a,b
			ld (hl),a						;store the page at A2
			ld (CurROMPage),a
			pop de
		pop hl
RuninRAM:
	call GetProgChainTop
	push hl
		bcall(_popOp1)					;_ all destroyed
		pop de
	inc de							;Past A1
	ld hl,Op1
;	ex de,hl						;Copy from Op1 to B1,C1
	ld bc,9
	ldir
	ret
RunBasicProgDCS:
RunBASICProg:
	call ArcUnarcProgA				;this sets up the appvar
	call GetArcstatus
	or a
	jr z,RunBASICProgUseOrig		;it's not archived!!
	call GetProgChainTop
	inc hl							;B1
	rst 20h
	bcall(_chkfindsym)
	ret c
	push bc
		ex de,hl
		push hl
			call SetUpROM				;CurROMPage is set by ArcUnarcProgA
			call GetArcProgWord
			ld a,h
			or l
			jp z,RunBASICEmptyProg
			push hl
				ld hl,TmpProgName
				push hl
					push hl
						call GetProgChainTop
						ld de,11
						add hl,de
						pop de
					ex de,hl
					ld bc,9
					ldir
					pop hl
				rst 20h
				bcall(_chkfindsym)
				bcallnc(_delvararc)
				ld hl,TmpProgName
				rst 20h
			pop hl
			push hl
				bcall(_enoughmem)
				jp c,TmpMemErr
				ex de,hl
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
	jr z,RunBASICProgNoPageInc
	bit 7,h
	jr z,RunBASICProgNoPageInc
	inc a
	res 7,h
	set 6,h
RunBASICProgNoPageInc:
	bcall(_flashtoram)
RunBASICProgUseOrig:

	call GetRAMName					;this chunk of code deals with Axe source editing
	rst 20h
	bcall(_ChkFindSym)
	inc de
	inc de
	ld a,(de)
	cp $3A							;the '.' symbol in TI tokens
	jr nz,RunBASICProg_NotAxeSrc
	inc de
	ld a,(de)
	cp '0'
	jr c,RunBASICProg_IsAxeSrc
	cp '9'+1
	jr c,RunBASICProg_NotAxeSrc
RunBASICProg_IsAxeSrc:
	ld hl,TmpProgName
	rst 20h
	bcall(_chkfindsym)
	bcallnc(_delvar)

	xor a
	ld (EditorMode),a				;offset into program - no Goto mode
	call GetProgChainTop
	inc hl
	rst 20h
	bcall(_ChkFindSym)
	call EditorJumpIn
	call PopProgChain
	scf							;force Homerun to reset context from the editor
	ret
RunBASICProg_NotAxeSrc:

	xor a
	call ArcUnarcDCSBASIC
	or a							;rcf
	ret z							;z is an error from ArcUnarcDCSBASIC
	bcall(_getK)					;this eliminates any residual keypresses
	bit graphDraw,(iy+graphFlags)
	jr z,RunBASICProg_SkipClean

	bcall(_grbufclr)
	bcall(_clrlcdfull)
	ld hl,TextShadow
	push hl
		pop de
	ld (hl),' '
	inc de
	ld bc,127
	ldir
	xor a
	ld h,a \ ld l,a
	ld (currow),hl									;currow = curcol = 0 (much better than homeup!)

RunBASICProg_SkipClean:
	bcall(_runindicon)				;turn off the run indicator
	bcall(_cleanall)				;clear out tempvars
	;set apptextsave,(iy+appflags)	;text goes to textshadow
	res TextWrite,(iy+sgrFlags)			;Make text go to screen
	call GetArcStatus
	push af
		call GetProgChainTop
		inc hl						;at B1
		pop af
	ld de,0							;jump to B2?
	or a
	jr z,RunBASICCopy
RunBASICCopyTmp:
	ld de,10
RunBASICCopy:
	add hl,de						;add 0 (B1) or 10 (B2)
	rst 20h
	bcall(_ChkFindSym)
#ifdef enablecn2
	di
	im 1
	ei
#endif
	call RunBASICProgram			;<---- FOR TEST
	res 4,(iy+9)					;reset [ON] register
	push bc
		ld a,b
		or a
		jr z,RunBASICPost_Valid
		call SetErrOffset
		jr nz,RunBASICPost_Valid
		ld hl,0
		ld (errOffset),hl
RunBASICPost_Valid:
;		ld hl,($965D)				;nextParse
;		ld de,(basic_start)
;		or a
;		sbc hl,de
;		ld (ScratchWord),hl			;No longer used.
RunBASICDebounce:
		bcall(_getk)
		jr nz,RunBASICDebounce			;if there was a keypress, ignore it and continue debouncing
		res IndicRun,(iy+IndicFlags)	;turn off the run indicator
		xor a
		out (20h),a						;Reset execution speed?

		ld a,$ff
		call ArcUnarcDCSBASIC
		ld hl,TmpProgName
		rst 20h
		bcall(_chkfindsym)
		bcallnc(_delvar)
		pop bc
#ifdef showbasicerrors				;otherwise residual keys were cleared
	ld a,b
	cp %01111111
	scf
	jr z,RunBASICFinishErrorSilent
	or a
	jr nz,RunBASICFinishError
#endif
RunBASICFinished:
PopProgChainOrAAndRet:
	call PopProgChain				;remove the BASIC program that was just run
	or a								;cleared carry -> RealStartNoReset
	ret

#ifdef showbasicerrors
RunBASICFinishError:
	call BASICErrorParse
	set graphDraw,(iy+graphFlags)
RunBASICFinishErrorSilent:
	push af
		call PopProgChain
		pop af
	ret
#endif


GetArcStatus:
	push hl
		push de
			call GetProgChainTop
			ld de,10
			add hl,de
			ld a,(hl)				;ld the A2 byte
			pop de
		pop hl
	ret

GetRAMName:
	call GetArcStatus
	push af
		call GetProgChainTop
		inc hl
		pop af
	or a
	ret z
	ld de,10
	add hl,de
	ret

GetRAMNameAP:
	call GetProgChainSize
	cp 2
	ret c						;Don't bother if less than 2
	call GetProgChainTop
	ld de,-20
	add hl,de
	ld a,(hl)					;at A1 byte of PREVIOUS stack entry
	or a
	scf
	ret z						;Don't bother if not an AP file
	ld de,10					;at A2 byte of PREVIOUS stack entry
	add hl,de
	ld a,(hl)
	inc hl						;at B2 byte of PREVIOUS stack entry
	or a
	scf
	ccf
	ret nz
	ld de,-10
	add hl,de					;at B1 byte of PREVIOUS stack entry
	or a						;rcf
	ret

RunBASICEmptyProg:
			pop hl
		pop hl
	jr RunBASICFinished
BASICErrorParse:
	and $7F
	ld l,a
	ld h,0
	ld de,506
	add hl,de
	jp DCSError
;	or a						;reset carry flag
	;ret

ErrUndefined:
	ld hl,503
	call DCSError
	or a						;rcf
	ret

#ifdef Folders
RunFldr:
	push ix
		pop hl
	bcall(_ldhlind)
	ld de,-11
	add hl,de
	ld a,(hl)
	ld (CurFldr),a
	call PopProgChain
	jp RealStartNoReset
#endif
;-----------------------------------------------
ArcUnarcDCSBASIC:
	ld (ScratchVar),a
	call GetArcStatus
	push af
		call GetProgChainTop
		inc hl						;B1
		pop af
	or a
	jr z,ArcUnarcDCSBASIC2
	ld de,10
	add hl,de						;B2
ArcUnarcDCSBASIC2:
	rst 20h
	bcall(_ChkFindSym)
	jr c,ErrUndefined
	inc de
	inc de
	inc de
	ld a,(de)						;get second byte of program
	cp $22
	jr nz,ArcUnarcDCSBASIC3
ArcUnarcDCSBASIC2L:
	inc de
	ld a,(de)
	cp $3f
	jr nz,ArcUnarcDCSBASIC2L
	ld hl,5
	jr ArcUnarcDCSBASIC4
ArcUnarcDCSBASIC3:
	ld hl,3
ArcUnarcDCSBASIC4:
	add hl,de
	ld a,(hl)
	cp '6'
	push af
		jr nz,ArcUnarcDBLR0
		inc hl
ArcUnarcDBLR0:
		inc hl
		ld a,(hl)
		cp $2a						;double-quote
		jr nz,ArcUnarcDBLR1
		inc hl
ArcUnarcDBLR1:
		pop af
	jr nz,ArcUnarcDBLR
	ld de,48d
	add hl,de
ArcUnarcDBLR:
	ld de,17d
	add hl,de
	ld a,(hl)
	cp $3E
	jr nz,ArcUnarcRetNZ				;should never be zero, exit status=OK
	inc hl
	ld a,(hl)
	cp 'A'
	jr nz,ArcUnarcRetNZ				;should never be zero, exit status=OK
	inc hl
	ld a,(hl)
	cp $3F							;line return
	jr nz,ArcUnarcRetNZ				;should never be zero, exit status=OK
	inc hl
ArcUnarcDCSBASICloop:
	push hl
		bcall(_ZeroOp1)
		pop hl
	push hl
		ld bc,0
ArcUnarcDBL1:
		inc bc
		inc hl
		ld a,(hl)
		cp $3F
		jr nz,ArcUnarcDBL1
		pop de
	ld hl,Op1
	ld (hl),5
	inc hl
	ex de,hl
	ldir
	inc hl
	push hl
		bcall(_ChkFindSym)
		jr c,ArcUnarcDBErr
		ld de,-5
		add hl,de
		call GetArcStatus
		or a
		jr nz,ArcUnarcDB2
		ld a,(hl)
		or a
		jr z,ArcUnarcDB3
ArcUnarcDB2:
		bcall(_ChkFindSym)
		ld a,b
		or a
		jr z,ArcUnarcDB22
		ld a,$ff
ArcUnarcDB22:
		ld c,a
		ld a,(ScratchVar)
		xor c
		bcallnz(_arc_unarc)
ArcUnarcDB3:
		pop hl
	ld a,(hl)
;	inc hl								;wouldn't this be important???
	cp $3E
	jr nz,ArcUnarcDCSBASICloop
ArcUnarcRetNZ:
	ld a,1
	ret									;exit status==OK
ArcUnarcDBErr:
	;		pop hl
		pop hl
	ld hl,503d
	call DCSError
	ld hl,TmpProgName				;ld hl,ProgNameSpace
	rst 20h
	bcall(_ChkFindSym)
	bcallnc(_delvar)
	call PopProgChain				;all done
	;jp RenderDesktop
	xor a								;says this FAILED
	ret
	
;-----------------------------------------------
RunAssociatedProg:
	push ix
		ld hl,TmpProgname2
		ld (ScratchWord),hl
;		call PushProgChain			;insert new entry for AP before its viewer
		pop hl
;	ret c							;die if failure
	call ArcUnarcProgA
	call GetProgChainTop
	ld (hl),1						;A1
	ld de,10
	add hl,de
	ld a,(hl)						;A2
	or a
	call nz,initTmpASM
	
	call PushProgChain				;Push the program entry on top of the AP file	
	call GetRAMNameAP
	rst 20h
	bcall(_chkfindsym)				;get the AP name in RAM
	ld hl,7
	add hl,de
	ld b,(hl)
	inc hl
	ld c,(hl)
	inc hl
	ld d,(hl)
	call DCSAPGetType
	jr z,RAP_Fail
	bcall(_chkfindsym)
	jr c,RAP_Fail
	dec hl
	ld a,(hl)
	ld (Op6),a
	inc hl
	call ArcUnarcProgA_Op1
	jp RunDCSasmProgAP
RAP_Fail:
	bcall(_grbufclr)
	ld hl,(2*256)+2
	ld (pencol),hl
	ld hl,RAPFailTxt
	call VPutsApp
	ld hl,(8*256)+2
	ld (pencol),hl
	ld hl,RAPFailTxt2
	call VPutsApp
	call iFastCopy
	call Pause
	call PopProgChain			;the main program
	call asmcheckwriteback		;the AP file
	jp PopProgChainOrAAndRet
	;call PopProgChain
	;or a						;rcf
	;ret
RAPFailTxt:
	.db "No handler for this file.",0
RAPFailTxt2:
	.db "See dcs.cemetech.net",0
;-----------------------------------------------
RunTIOSASM:
	push ix
		ld hl,TmpProgname
		ld (ScratchWord),hl
		pop hl
	call ArcUnarcProgA
	call GetArcStatus
	or a
	call nz,initTmpASM
	jp ASMContinueAll
RunDCSasmProg:
	push ix
		pop hl
	call ArcUnarcProgA
RunDCSasmProgAP:
	ld hl,TmpProgname
	ld (ScratchWord),hl
	call GetArcStatus
	or a
	call nz,initTmpASMOp1
	;call FixLibs				;redundant
	call RunDCSIon
	ld hl,ScratchWord
	push hl
		pop ix
	push hl
		call GetRAMName
RunDCSasmOrig:
		rst 20h
		bcall(_ChkFindSym)
		pop hl
;	push hl
	inc hl
	inc hl						;de points to size byte of program
	ld (hl),e					;hl == FreeRAM (?)
	inc hl
	ld (hl),d					;hl == FreeRAM+1
	ex de,hl
	push hl
		ld de,14				;bypass extra 'ret' & $BB,$6D
		add hl,de
		bcall(_ldhlind)
		pop de
	ld a,l
	or h						;check if we have any ALEs
	jp z,ASMContinueAll
	add hl,de				;at lib table
	ld de,-$9D95+2
	add hl,de
	ld ix,ALEVectorStart			;blank vector entry
DCSLibLoop:
	dec hl
	rst 20h
	ld a,5
	ld (op1),a
	push hl
		bcall(_chkfindsym)
		inc de
		inc de
		inc de
		inc de
		ld (ScratchWord),de
		inc de
		inc de
		pop hl
	jp c,LibErr				;fix pop(s)?
	push hl
		ex de,hl
		ld b,(hl)
DCSLibLoadLoop:
		inc hl
		push hl
			bcall(_ldhlind)
			push de
				ld de,(scratchWord)
				add hl,de
				pop de
			ld a,$C3
			ld (ix),a
			inc ix
			ld (ix),l
			inc ix
			ld (ix),h
			inc ix
			pop hl
		inc hl
		djnz DCSLibLoadLoop
		pop hl
	ld a,(hl)
	cp $ff
	jr nz,DCSLibLoop
	jp ASMContinueAll
RunIonProg:
	push ix
		ld hl,TmpProgname
		ld (ScratchWord),hl
		pop hl
	call ArcUnarcProgA
	call GetArcStatus
	or a
	call nz,initTmpASM
	;call FixLibs				;redundant
	call RunDCSIon
	jp ASMContinueAll
RunDcsIon:
FixLibs:
	ld de,ionVectors
	ld hl,IonJumpmap
	ld bc,8*3
	push bc
		ldir
		ld hl,dcsLibAdditions
		pop bc
	ldir
	ret
RunMOSasmProg:
	;first we need to do any pointer correction necessary...
	;...which is none
	jp RunIonProg

initTmpASMOp1:
	bcall(_chkfindsym)
	dec hl
	ld a,(hl)
	ld (Op6),a
	inc hl
	jr initTmpASMOp12
initTmpASM:
	push ix
		pop hl
	call RunNameCopy
initTmpASMOp12:
	ld a,b
	push hl
		push de
			push bc
				call GetProgChainTop		;A1
				ld de,10
				add hl,de					;A2
				pop bc
			ld a,b
			ld (hl),a
			ld (CurROMPage),a
			pop de
		pop hl							;this should properly set up SafeArcFlags for the DCS BASIC routine
	push bc
		ex de,hl
		push hl
			call SetUpROM
			call GetArcProgWord
			;ld a,h
			;or l
			;jp z,RunBASICEmptyProg
			push hl
				call GetProgChainTop	;A1
				inc hl					;B1
				ld d,(hl)
				ld b,0					;was 0 with dec bc later
initTmpAsmNLL:
				ld a,(hl)				;\- get name length
				or a					;|
				jr z,initTmpAsmNLLDone	;/
				inc hl					; 
				inc b
				ld a,b
				cp 9
				jr nz,initTmpAsmNLL
initTmpAsmNLLDone:
				
				ld c,b
				ld b,0
				push bc
					ld a,d					;prog type
					ld de,Op1
					ld (de),a				;store it as first byte of Op1
					ld hl,(ScratchWord)
					push de
						inc de
						inc hl
						ldir				;copy from scratchword to op1
						call GetProgChainTop
						ld de,10+1			;to A2, then B2
						add hl,de
						pop de
					pop bc
				push hl						;hl = progchain, de = op1
					ld a,c
					cp 9
					jr z,initTmpAsm_NoZTerm
					add hl,bc
					ld (hl),0
initTmpAsm_NoZTerm:
					pop hl
				ex de,hl
				push de
					ldir					;copy RAM name
					call GetProgChainSize
					pop de
				inc de						;pointing to Op1+1
				ld b,a
				ld a,(de)
				add a,b
				ld (de),a					;add size of progchain to this to make it unique
				bcall(_chkfindsym)
				jr c,RunAProgNoneToDel
				bcall(_delvararc)
RunAProgNoneToDel:
				call GetProgChainTop
				ld de,10+1			;to A2, then B2
				add hl,de				
				rst 20h
				pop hl				;recall and resave size
			push hl
				bcall(_enoughmem)
				jr c,TmpMemErr
				ex de,hl
				ld a,(op1)
				sub 5
				jr nz,InitTmpASMMakeProt
				bcall(_createprog)
				jr InitTmpASMMake
InitTmpASMMakeProt:
				bcall(_createprotprog)
InitTmpASMMake:
				dec hl
				ld a,(Op6)
				ld (hl),a
				inc de
				inc de
				pop bc
			pop hl
			call SetUpROM
			inc hl
			inc hl
		pop af
	or a
	jr z,RunAProgNoPageInc
	bit 7,h
	jr z,RunAProgNoPageInc
	inc a
	res 7,h
	set 6,h
RunAProgNoPageInc:
	bcall(_flashtoram)	
	ret
TmpMemErr:
				pop af
			pop af
		pop af
		
	ld hl,$0000				;restore stack
	add hl,sp
	push hl
		ld hl,(AppVarLoc)
		ld de,AVOff_SPSave
		add hl,de
		bcall(_ldhlind)
		ex de,hl
		pop hl
	ex de,hl
	or a
	sbc hl,de
TmpMemErrL:
		pop de
	dec hl
	dec hl					;each stack entry is *TWO* bytes!!!!
	ld a,h
	or l
	jr nz,TmpMemErrL

MemError:
	ld hl,504
	call DCSError
	jp PopProgChainOrAAndRet
	;call PopProgChain
	;or a						;reset carry flag
	;ret
;--------------------------------------------------------------
ASMContinueAll:
	ld hl,AVOff_ASMExecActive
	call DAVLCheckOffset
	ld (hl),1
	
	call GetRAMName
	rst 20h
	bcall(_chkfindsym)
	ex de,hl
	
	res TextWrite,(iy+sgrFlags)
	push hl
		;bcall(_ldhlind)
		;push hl
		;	pop bc
		ld c,(hl)
		inc hl
		ld b,(hl)
		dec bc
		dec bc				;the $BB,$6D
		pop de
	push de				;save progstart for unarc of pendfile main
		inc de
		inc de
		inc de
		inc de				;size & ASM token (4 bytes)
		ld a,(de)
		push af
			inc de
			ld a,(de)
			dec de
			push af
				ld a,(iy+dcsProgFlags)
				and %00101110
				cp %00001000
				jr z,ASMSwap_TIOS
				ld a,(iy+dcsProgFlags)
				and $7e
				cp $3e
				jr nz,ASMSwap_DCSIon
ASMSwap_MOS:
				ld a,$18
				ld (de),a
				inc de
				push de
					ld a,(de)
					ld h,0
					ld l,a
					add hl,de
					ld de,30				;NOT 16!!!
					add hl,de
#ifdef mossupport
					bcall(_mos_nextstr)
#endif
					pop de
				or a
				sbc hl,de
				dec hl
				ld a,l
				ld (de),a
				dec de						;ooops :D
				jr ASMSwap_TIOS
ASMSwap_DCSIon:
				ld a,$b7			;or a (to clear carry flag)
				ld (de),a
				ld a,(iy+dcsProgFlags)
				and $02
				sub 2
				jr nz,ASMSwap_TIOS
				inc de
				xor a
				ld (de),a
				dec de
ASMSwap_TIOS:
				ld hl,$9D95
#ifdef hook3
				ld ix,gbuf
			;	call $8C4D			;anova vars
				call hook1			;GOGOGO RUN THE PROGRAM!
#endif
				bcall(_grbufclr)	;since it's used to swap
				;call DAVLCheck
				call GetRAMName
				rst 20h
				bcall(_chkfindsym)
				inc de
				inc de
				inc de
				inc de
				inc de
				pop af
			ld (de),a
			dec de
			pop af
		ld (de),a
		pop hl
	call asmcheckwriteback
	call GetRAMNameAP				;carry reset if AP prog & file, carry set if not
									;and leaves HL at B1 byte of AP file
	jr c,ASMSwap_Cleanup			;jump if it IS NOT an AP file
ASMSwap_TIOSContinue:
	call PopProgChain			;collapse to just the AP file
	call asmcheckwriteback
ASMSwap_Cleanup:
	bcall(_grbufclr)
	bcall(_clrlcdFull)
	bcall(_ClrTxtShd)
	bcall(_homeup)
	set graphDraw,(iy+graphFlags)

	call PopProgChain			;remove the program
	or a						;reset carry flag
	ret
#include "writeback.asm"
LibErr:
		pop ix
	ld hl,502d
	call DCSError
	;ccf
	or a						;reset carry flag
	ret
#ifdef hook3
;Load:
;|%%%%%%%%%%%%%|\\\\\\\\\\\\\|--------------|
; DCS5???       Prog          Free
;|\\\\\\|%%%%%%%%%%%%%|\\\\\\|--------------|
;
;|\\\\\\\\\\\\\|%%%%%%%%%%%%%|--------------|
;
;hl should inc by bc; de should also inc
;
;Unload:
;|\\\\\\\\\\\\|%%%%%%%%%%%%%|--------------|
; Prog          DCS5???      Free
;|\\\\\\|%%%%%%%%%%%%%|\\\\\|--------------|
;
;|%%%%%%%%%%%%%|\\\\\\\\\\\\|--------------|
;
;hl should stet; de should also stet

;first, de to hl via ix
;next, execute
;finally, hl to de via ix

hook1:					;202 bytes max	;this no longer applies
	ld a,1				;2
	push hl				;1
		push bc				;1
			push ix				;1
				push de				;1
					call hook1_1	;3
					bcall(_grbufclr)
					
					in a,(6)
					push af					;save DCS ROM page

						ld hl,AVOff_SPSave
						call DAVLCheckOffset
						ex de,hl
						ld hl,0
						add hl,sp
						dec hl
						dec hl
						ld (cmdshadow+78),hl
						ex de,hl					;switch hl and de
						ld (hl),e					;put first byte
						inc hl						;and then
						ld (hl),d					;second byte in
						AppOnErr(hook1reterror)
						ld hl,hook1ret
						push hl
							ld hl,$9d95
							push hl
								;ld hl,19
								;call DAVLCheckOffset
								call GetRAMNameAP		;returns carry set for non-AP, carry reset for AP
								jr c,hook1normal
;								ld a,(hl)
;								inc a
;								jr z,hook1normal
								pop hl
							ld hl,($9dA1)
							push hl
								ld hl,$9DA3
								ld a,(hl)
								push af
									inc hl
									ld d,(hl)
									inc hl
									ld e,(hl)
									push de
										;need to get size and loc and type
										call GetRAMNameAP
										rst 20h
										bcall(_chkfindsym)
										ex de,hl
										push hl
											bcall(_ldhlind)
											ld de,-8
											add hl,de
											pop de
										push hl
											ld hl,10
											add hl,de
											push hl
												pop ix
											pop bc
										pop de
									pop af
hook1normal:
#ifdef enableCn2eis
								di
								im 1
								ei
#endif
								pop hl
							jp (hl)			;3
hook1retfromp2:
						pop hl \ pop hl \ pop hl \ pop hl
hook1ret:
						AppOffErr
hook1reterror:
#ifndef enableCn2eis
						di
#endif
						bcall(_fillBasePageTable)
						xor a
						bcall(_GUIDrawHourglass)
						pop af				;the ROM page we're on now - no need to save it
					pop hl				;1	THIS is where we need to copy back to
				call GetRAMName
				rst 20h
				bcall(_chkfindsym)
				inc de
				inc de
				inc de
				inc de					;hl points to first byte after $BB,$6D
				ex de,hl				;de points to highest byte of VAT entry
				pop ix				;1		;restore the gbuf save
			pop bc							;restore the program swap size
;		add hl,bc
		push bc								;save the size again
			ld de,300h
			ex de,hl
			push hl				;1
				sbc hl,bc			;1					;more than 300 bytes to move?
				jr c,hook0_2			;2
				pop hl				;1
			push bc				;1
hook0_2:												;now it's either 768 or 768-size
				pop bc
			ex de,hl				;1
			or a				;reset carry flag		;now hl = VAT entry - 768 or 768-size (?)
			sbc hl,bc
			pop bc				;1
		pop de				;1							;restore hl = 9d95 = source
	ex de,hl
	ld a,2				;2
;	jp hook1_1	;3
hook1_1:
	push bc				;1
		push hl				;1
			push ix				;1
				push de				;1
					push af				;1
						push hl				;1
							ld hl,768			;3
							push hl				;1
								sbc hl,bc			;1
								jr c,hook1_2			;2
								pop hl				;1
							push bc				;1
hook1_2:
								pop bc				;1
							pop hl				;1
						pop af				;1
					push af				;1
						push bc
							dec a				;1
							jr nz,hook1_2_1			;3
							call swap1
							pop bc
						pop af				;1
					pop de				;1
				pop ix				;1
			pop hl
		push hl
			push ix				;1
				push de				;1
					push af				;1
						push bc
							call swap2
							jr hook1_3
hook1_2_1:
							call swap3
							pop bc
						pop af				;1
					pop de				;1
				pop ix				;1
			pop hl
		push hl
			push ix				;1
				push de				;1
					push af				;1
						push bc				;1
							call swap4
hook1_3:
							pop bc				;1
						pop af
					pop de				;1
				pop ix				;1
			pop hl
		pop bc
	push hl
		ld hl,768			;3
		sbc hl,bc			;1
		pop hl				;1
	ret nc				;<<>>Check this<<>>
	cp 1
	jr nz,hook1_4
	push bc
		ld bc,768
		add hl,bc
		ex de,hl
		add hl,bc
		pop bc
	ex de,hl
	jr hook1_5
hook1_4:
	push hl
		push de
			push bc
				push bc
					pop hl
				or a
				ld de,768d*2
				sbc hl,de
				jr nc,hook1_4_1			;jumps if greater than 768d (doesn't need change)
				pop hl
			push hl
				ex de,hl
				or a
				sbc hl,de
				pop bc
			pop de				;hl is $600-bc
		add hl,de			;<-- this m4kes the ch4nge
		pop de
	ex de,hl
	jr hook1_5
hook1_4_1:
				pop bc
			pop de
		pop hl
hook1_5:
	ex de,hl
	push hl
		push de
			push bc
				pop hl
			ld de,768
			or a
			sbc hl,de
			push hl
				pop bc
			pop hl
		pop de
	jp hook1_1
					;----
					;72 bytes
hookend1:
hookv3:					;80 bytes max
swap1:
	;bc = size
	;hl=$9d95[+]
	;de=prog
	;ix=gbuf
	push ix
		pop hl
	ex de,hl
	push hl
		push bc
			ldir
			pop de
		pop hl
	bcall(_delmem)
	ret				;12 bytes total
swap1end:
swap2:
	;bc = size
	;hl=$9d95[+]
	;de=prog
	;ix=gbuf
	push ix
		push bc
			push hl
				push bc
					pop de
				ex de,hl
				bcall(_insertmem)
				pop de
			pop bc
		pop hl
	ldir
	ret				;14 bytes total
swap2end:
swap3:
	;bc = size
	;hl=$9d95[+]
	;de=prog
	;ix=gbuf
	ex de,hl
	push ix
		pop hl
	ex de,hl
	push hl
		push bc
			ldir
			pop de
		pop hl
	bcall(_delmem)
	ret				;13 bytes total
swap3end:
swap4:
	;bc = size
	;hl=$9d95[+]
	;de=prog
	;ix=gbuf
	push ix
		push bc
			push de
				push bc
					pop hl
				bcall(_insertmem)
				pop de
			pop bc
		pop hl
	ldir
	ret				;13 bytes total
swap4end:
hookend:
					;-----
					;52 bytes total - 28 left
#endif
;4 routines:
;
;-1- Copy himem to gbuf (prog to gbuf)
;-2- Copy gbuf to lowmem (gbuf to $9d95)
;-3- Copy lowmem to gbuf ($9d95 to gbuf)
;-4- Copy gbuf to himem (gbuf to prog)
;--------------------------------------------------------------