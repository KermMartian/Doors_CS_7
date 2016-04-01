;DetectType: Used in both VATFind->VATValid and in RunProg
;	Inputs:	(VATMode)
;				0=count programs
;				1=display desktop
;				2=count SEs
;				3=run SEs
;				4=RunProg active
;			hl
;				pointer to data type of VAT entry (beginning)
VATProcessEntry:
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
		pop hl						;recall pointer to DAL
	push hl						;save pointer to DAL again
		ld a,(VATMode)
		cp 2
		jp nc,VATMainLoop2			;if not displaying the desktop, jump out

		inc hl
		inc hl
		inc hl
		or a
		jp z,VatProcessEntryCount
	;new DetectType changes start here V----
		push hl
			ld a,l
			ld (de),a					;VAT location LSB...
			inc de
			ld a,h
			ld (de),a					;...MSB
			inc de
			dec hl
			dec hl
			dec hl
			ld a,(hl)
			ld (de),a					;Prog location LSB...
			inc de
			dec hl
			ld a,(hl)
			ld (de),a					;...MSB
			inc de
			dec hl
			ld a,(hl)
			ld (de),a					;ROM page
			ld (CurROMPage),a				;Save it for byte routine
			inc de
			pop hl
		push de
			call DetectType				;if we get here, we're displaying the desktop
			pop de
		cp $ff						;was it invalid?
		jp nz,VATMainContinue
		ld hl,-5
		add hl,de
		pop de
	ex de,hl						;de = VFAT pointer, hl=VAT pointer
	push de
		dec hl
		jp VATValidSkipPoundBang
	;new DetectType changes end here   ^----
		
VATMainContinue:
		ld (de),a
		inc de
		push de
			ld (iy+dcsProgFlags),a		;the flag mask returned from DetectType
		
;#ifdef false
			ld hl,-6
			add hl,de
			ld e,(hl)
			inc hl
			ld d,(hl)
			ld hl,(PasteWord)
			inc hl
			bcall(_mos_cphlde)
			jr nz,VATMainContinue_NotCut
			push ix
				pop hl
			ld de,IconSpace32b
			push de
				ld bc,32
				ldir
				pop hl
			push hl
				ld b,16
				ld c,$aa
VATMainContinue_CutLoop:
				ld a,(hl)
				and c
				ld (hl),a
				inc hl
				ld a,(hl)
				and c
				ld (hl),a
				inc hl
				ld a,c
				cpl
				ld c,a
				djnz VATMainContinue_CutLoop
				pop ix
;#endif
				
VATMainContinue_NotCut:			
			ld a,(SpriteY)
			ld l,a
			ld a,(SpriteX)
			push af
				ld c,2
				ld b,16
				call iLargeSprite		;uses the ix returned from DetectType
				pop af
			pop de
		push de
			push af
				ld a,(iy+dcsProgFlags)
				bit 6,a
				jr nz,VATMainNoHide
				ld hl,-6
				add hl,de
				ld e,(hl)
				inc hl
				ld d,(hl)
				ld hl,-7
				add hl,de
				ld a,(hl)
				cp 27
				jr nc,VATMainNoHide
				ld a,(SpriteY)
				ld b,6
				add a,b
				ld l,a
				pop af
			push af
				ld b,-3
				add a,b
				ld ix,HideIcon
				ld b,3
				call iPutSprite
VATMainNoHide:
				pop af
			push af
				bit 7,(iy+dcsProgFlags)
				jr z,VATMainNoArc
				ld a,(SpriteY)
				ld b,12
				add a,b
				ld l,a
				pop af
			push af
				ld b,-3
				add a,b
				ld ix,ArcIcon
				ld b,4
				call iPutSprite
VATMainNoArc:
				ld a,(SpriteY)
				ld l,a
				pop af
			push af
				ld b,4
				bit 0,(iy+dcsProgFlags)
				jr z,VATNoLock
				ld ix,LockIcon
				sub 3
				call iPutSprite
VATNoLock:
				pop af
			add a,29
			ld (SpriteX),a
			cp 90
			jr nz,VATSpriteContinue
			ld a,3
			ld (SpriteX),a
			ld a,29
			ld (SpriteY),a
VATSpriteContinue:
			pop de
		pop hl
	push hl
		push de
			ld a,(iy+dcsProgFlags)
			bit 6,a
			jp nz,VATFindDispFldName
			dec hl
			dec hl
			dec hl
			ld b,(hl)
			ld a,(NameY)
			ld (PENROW),a
			ld a,(NameX)
			ld (PENCOL),a
			add a,29
			ld (NameX),a
			cp 90
			jr nz,VATNameContinue
			ld a,3
			ld (NameX),a
			ld a,47
			ld (NameY),a
VATNameContinue:
			xor a
			ld (ScratchWord+1),a
VATNameWriteLoop:
			dec hl
			ld a,(hl)
			cp 27
			jr nc,VATNameWriteLoopNoFix
			add a,$40
VATNameWriteLoopNoFix:
			ld (ScratchWord),a
			push hl
				push bc
					ld hl,ScratchWord
					call vputsApp
					ld hl,PENCOL
					dec (hl)
					pop bc
				pop hl
			djnz VATNameWriteLoop
			pop de
		pop hl
	dec hl
	jr VATValidSkip3
;		jr VATMainLoop2
;VATMainLoop3:								;if we get here, DetectType said the file was INVALID
;		ld hl,-5
;		add hl,de
;		ex de,hl
VATMainLoop2:								;end of name
		pop hl									;recall DAL pointer
VATValidSkip:
	push de									;saves ???
		ld e,(hl)
		dec hl
		ld d,(hl)								;get program's address...
		ex de,hl								;move program address into hl
		call SetUpROM							;set up for archive reads (if necessary)
		ex de,hl
		inc de
		inc de
;		call GetArcProgByteDE					;get first byte of the program
;		cp $AB									;should we ignore this program?
;		jr z,VATCountSkipSE						;if so, don't count it
;#ifdef AnsHide
;		cp $72									;also check for $72 if Ans is a valid hide token
;		jr z,VATCountSkipSE
;#endif
		inc de									;if we get here, we should show this, so move to the third byte
		inc de
;		call GetArcProgByteDE					;what is it?
;#ifdef AnsHide
;		cp $72									;Could be Ans in 3rd byte to hide
;		jr z,VATCountSkipSE
;#endif
;		cp $AB									;or Rand, for some reason
;		jr nz,VATValidSkip2						;jump if it's something we should definitely deal with
;VATCountSkipSE:								;if we get here, it should be hiiden (might be an SE?)
		ld a,(VATMode)
		sub 2									;are we counting SEs?
		jr z,VATSECheck
		dec a									;or maybe running SEs?
		jr z,VATSECheck							;either way, go check if this hidden prog is an SE
;VATCountDec:								;otherwise don't count it and go skip
;		ld a,(ScratchVar)
;		dec a
;		ld (ScratchVar),a
		jr VATValidSkipPoundBang
VATSECheck:
		inc de
		call GetArcProgByteDE
		cp $C9
		jr nz,VATVAlidSkipPoundBang
		inc de
		call GetArcProgByteDE
		cp $31
		jr nz,VATVAlidSkipPoundBang
		inc de
		call GetArcProgByteDE
		cp $87
		jr nz,VATVAlidSkipPoundBang
		inc de
		call GetArcProgByteDE
		and $f0
		sra a
		sra a
		sra a
		sra a
		sub 5
		jr nz,VATVAlidSkipPoundBang
		ld a,(VATMode)
		sub 3
		jr z,VATRunSECheck
		ld a,(ScratchVar)
		inc a
		ld (ScratchVar),a
		jr VATVAlidSkipPoundBang
VatProcessEntryCount:
		call DetectType				;if we get here, we're counting the desktop
		pop hl
	push hl
		dec hl
		cp $ff						;was it invalid?
		jp z,VATValidSkipPoundBang
VATValidSkip2:
;		ld a,(VATMode)
;		or a
;		jr nz,VATVAlidSkipPoundBang
;		push hl
;			dec hl
;			dec hl
;			dec hl
;			ld a,(hl)
;			cp $21
;			jr z,VATValidPoundBang
;			cp $23
;			jr z,VATValidPoundBang
;			ld a,(LastClick)
;			ld b,a
;			ld a,(CurFldr)
;			cp b
;			jr z,VATValidNoPoundBang
VATValidPoundBang:
;			ld a,(ScratchVar)
;			dec a
;			ld (ScratchVar),a
VATValidNoPoundBang:
;			pop hl
VATVAlidSkipPoundBang:
		pop de
VATValidSkip3:
	dec hl					;past page
	dec hl					;to name length
	ld b,(hl)				;get name length in counter
VATNameLoop:
	dec hl					;skip a name char
	djnz VATNameLoop			;if not done, repeat
	dec hl					;go to first of next
	ld a,(VATMode)				;check VAT mode
	dec a					;is it 1 (disp progs)?
	jr z,VATCheckDoneDisp			;if so, check if done
VATMainReLoop:
	ret						;RETURN TO VATMAINLOOP
VATRunSECheck:
		ld a,(ScratchVar)
		dec a
		ld (ScratchVar),a
		or a
		jr nz,VATValidSkip2
		ex de,hl
;		ld de,-2
;		add hl,de
		dec hl
		dec hl
		pop de
	pop bc					;get rid of the call from VATFind!
 ld a,(CurROMPage)
 or a
 ret z
 ld hl,0
 ret
VATCheckDoneDisp:
	ld a,(ProgsToDo)
	dec a
	ld (ProgsToDo),a
	or a
	jr nz,VATMainReLoop
	pop bc					;get rid of the call from VATFind!
 ret
VATFindDispFldName:
;---------------------
			ld e,(hl)
			dec hl
			ld d,(hl)
			dec hl
			ld a,(hl)
			ld (CurROMPage),a
			ex de,hl
			call SetUpROM
			inc hl
			inc hl
			ld a,(NameY)
			ld (PENROW),a
			ld a,(NameX)
			ld (PENCOL),a
			add a,29
			ld (NameX),a
			cp 90
			jr nz,VATNameContinueFld
			ld a,3
			ld (NameX),a
			ld a,46
			ld (NameY),a
VATNameContinueFld:
			ld b,8
			xor a
			ld (ScratchWord+1),a
VATNameWriteLoopFld:
			call GetArcProgByte
			ld (ScratchWord),a
			push hl
				push bc
					ld hl,ScratchWord
					call vputsApp
					ld hl,PENCOL
					dec (hl)
					pop bc
				pop hl
			inc hl
			dec b
			ld a,b
			or a
			jr z,VATNameDoneFld
			ld a,(hl)
			or a
			jr nz,VATNameWriteLoopFld
VATNameDoneFld:
			pop de
		pop hl
	dec hl
	jp VATValidSkip3
;-------------------------
;		jp VATMainLoop2 
		