;-----------------------------------------------------------
;	Filename:		main.asm
;	Long name:  	Main shell driver code
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	Unknown
;
; Launch and clean up after shell, render desktop,
; handle switching to menus and running programs.
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

RealStart:
#ifdef intro
	call Intro
#endif

	ld hl,(45*256)+32			;initialize mouse coordinates
	ld (MseY),hl				;x=45,y=32
	call DisableLaunchKeyhook	;Disable [ON][PRGM] keyhook
	call DisableMyParserhook	;Disable parserhook for now
	call DisableLaunchOffscrpt
	bcall(_CloseGUIStack)
	ld hl,sHomescreen			;Delete the homescreen var
	rst 20h
	bcall(_ChkFindSym)
	push af
		bcallnc(_DelVar)
		pop af
	ld hl,0
	push hl
		bcallnc(_CreateProg)		;Recreate it as blank
		pop hl
	ld (cmdshadcur),hl			;zero out cursor positions
	ld (cmdcursor),hl
	ld a,(randData)					;save randomness of data
	ld b,255					;255 bytes to clear
	ld hl,MainSafeRAM				;clear our saferam areas
InitClearLoop:
	ld (hl),0					;put zero in it
	inc hl						;go to next byte
	djnz InitClearLoop				;done yet?
	ld (randData),a					;recall contents of random data
	ld hl,ProgChainAppVar
	rst 20h
	bcall(_chkfindsym)
	bcallnc(_delvararc)			;remove CHAIN7 appvar
#ifdef Folders
	ld hl,AVOff_CurrFolder
	call DAVLCheckOffset
	ld a,(hl)
	push af
		ld hl,BaseFld
		rst 20h
		pop af
	push af
		ld hl,Op1+5
		ld (hl),a
		bcall(_chkfindsym)
		pop bc
	jr nc,InitFolderSet		;jump over if the currfldr exists
	ld b,1
InitFolderSet:
	ld a,b
	ld (CurFldr),a
#endif
#ifdef alphasort
	di
	call SetSpeedFast
	ld hl,68
	call DAVLCheckOffset
	ld a,(hl)
	or a
;	push af
;		call nz,sort
;		pop af
	bcallnz(_sort)
	call SetSpeedSlow
#endif
	di						;disable all interrupts
	xor a
	ld (APDNext),a
	ld (APDNext+1),a
	ld (dAPDTimer),a
	ld hl,69
	call DAVLCheckOffset
	ld (hl),0
#ifdef adcsswap
	call ADCSSwap
#endif
	ld hl,(45*256)+32			;initialize mouse coordinates
	ld (MseY),hl				;x=45,y=32
RealStartNoReset:
#ifdef enableCn2
	ld hl,35
	call DAVLCheckOffset
	ld a,(hl)
	or a
	call nz,Cn2_Setup
#endif

	ld hl,AVOff_CurrFolder
	call DAVLCheckOffset
	ld a,(CurFldr)
	ld (hl),a
	
#ifdef easteregg
	ld a,$f7
	out (1),a
	nop \ nop
	in a,(1)
	cp $fd
	call z,easteregg
#endif
	
	call SPSave
	res lwrCaseActive,(iy+appLwrCaseFlag)
	ld hl,33
	call DAVLCheckOffset
	ld a,(hl)
	or a
	jr z,NoLowerAtStart
	set lwrCaseActive,(iy+appLwrCaseFlag)
NoLowerAtStart:
#ifdef Folders					;folder startup checks
	ld hl,BaseFld
	rst 20h
	bcall(_chkfindsym)
	jr nc,BaseFldSet
	ld hl,2
	bcall(_createprotprog)
	inc de
	inc de
	ld a,'M'
	ld (de),a
	inc de
	xor a
	ld (de),a
	ld a,2
	call FldSearch
BaseFldSet:
	ld a,3
	call FldSearch
#endif

;ClearVFAT:
;	ld hl,VFAT
;	push hl
;		pop de
;	inc de
;	ld bc,35
;	ld (hl),0
;	ldir

	ld hl,(progptr)					;get start of VAT
	ld (CurVATStart),hl				;Set first program to check
	xor a						;a=0
	ld (ProgsDone),a
	ld (VATMode),a					;VAT mode 0 (count progs)
	ld (ScratchVar),a				;Set init no. of progs (-1)
	call VATFind					;get no. of progs
	ld a,(ScratchVar)				;get total number
	ld (TotalProgs),a				;put it in its var
	push af
		ld hl,AVOff_CurrScreen
		call DAVLCheckOffset
		ld a,(hl)
		pop bc
	cp b
	jr c,RealStartNoTop
	xor a
RealStartNoTop:
RealStartFindScreen:
	or a
	jr z,RealStartFoundScreen
	push af
		call DrawProgsToScreen
		call ScrollDownSub
		pop af
	sub 6
	jr RealStartFindScreen
RealStartFoundScreen:

#ifndef Folders
	or a						;=cp 0
	jr nz,ProgsToLoad				;if progs =/=0, continue
	ld hl,505d					;Error 505: No Progs
	call DCSError					;display the error
	jp ExitDoorsCS					;if no progs, exit
#endif

ProgsToLoad:						;Continue here if programs exist
	ld a,2						;VATMode 2 = count SEs
	ld (VATMode),a					;put it in...
	xor a						;a=0
	ld (ScratchVar),a				;so far no SEs
	call VATFind					;get quantity
	ld a,(ScratchVar)				;save it to...
	cp 4
	jr c,ProgsToLoadSELess
	ld a,4
ProgsToLoadSELess:
	ld (SETotal),a					;...total SEs
	or a						;are there any?
	jr z,NoSEAtStart				;if not, forget this

	ld b,a
	ld a,3
	ld (VATMode),a
	ld hl,SETable
	push bc
		push hl
			ld (hl),0
			push hl
				pop de
			inc de
			ld bc,15
			ldir
			pop hl
		pop bc
SEInitTable:
	push hl					;save 
		push bc
			ld a,b
			ld (ScratchVar),a
			call VATFind
			pop bc
		pop de
	ex de,hl
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	inc hl
	inc hl
	djnz SEInitTable
	
	ld hl,3						;3 bytes to startup ptrs
	call RunSEs

NoSEAtStart:						;Clear out the VFAT

RenderDesktop:						;Main rendering routines
	res IndicRun,(iy+IndicFlags)	;turn off the run indicator
	res CurAble,(iy+curFlags)		;don't flash the cursor, even if interrupts get enabled
	bcall(_freeRAM)					;bytes of free RAM
	ld (FreeRAM),hl					;Save for later use
	res 4,(iy+9)					;reset [ON] register
	set TextWrite,(iy+sgrFlags)			;Make text go to buffer
	bcall(_grbufclr)				;clear the buffer

	ld hl,AVOff_CurrScreen
	call DAVLCheckOffset
	ld a,(ProgsDone)
	ld (hl),a
	ld hl,BGProgName				;"prgmZDCSBG"	;always allowing it now
	rst 20h						;copy to Op1
	bcall(_chkfindsym)				;is there a background?
	jp c,RenderDesktopNoBG				;if not, don't copy
	ld a,b
	ld (CurROMPage),a
	ex de,hl
	call SetUpROM
	ld de,4						;skip 2 size bytes, $AB, $C9 (ignore,ret)
	add hl,de					;do the skip
	ld de,PlotsScreen				;copy to buffer
	ld bc,768-(12*8)				;bytes to copy
	call ldirROMRAM					;copy to buffer
RenderDesktopNoBG:					;continue here

;---Put programs onscreen---

	call DrawProgsToScreen
	ld hl,PlotsScreen+(12d*63d)			;top line of toolbar
	call HLine					;draw
	ld hl,PlotsScreen+(12d*55d)			;bottom line of toolbar
	call HLine					;draw it
SideLines:						;draw the sides of the toolbar
	ld hl,PlotsScreen+(12*56)			;top of the left line
	ld de,11					;add 11 after first
	ld b,7						;7 rows to do
SideLinesLoop:						;draw loop
	ld (hl),$80					;left side
	add hl,de					;go to right
	ld (hl),$01					;right side
	inc hl						;on to next row
	djnz SideLinesLoop				;if >7 done, repeat
	ld a,(FreeRAM+1)				;get MSB of freeRAM
	cp 06						;is it low?
	jr nc,MouseBatt					;if not, no warning
	ld ix,ErrDialogMask				;get the icon
	ld b,8						;8 rows
	ld l,46						;ycoord = 46
	ld a,68						;xcoord = 68
	call iPutSpriteMask					;draw it
MouseBatt:						;continue here
	in a, (2)					;poll batt port
	and 01h 					;is low bit set?
	jr nz,MouseNoBatt				;if ok, skip this
	ld ix,ErrDialogMask				;get icon
	ld b,8						;8 rows
	ld l,46						;y=46
	ld a,78						;x=78
	call iPutSpriteMask			;put it up
MouseNoBatt:					;here starts mem meter disp
	ld hl,(FreeRAM)				;get free RAM in bytes
	ld de,-(TotalRAMSize/8)		;int(totalmem/7)=3048
	ld bc,$f800					;initial value for no free
MemDivLoop:						;see how much to draw
	bit 0,b						;check if b is full
	push af						;save the z flag
		sra b						;shift b over
		set 4,b						;set the leftmost pixel
		pop af						;recall flags
	jr z,MemDivLoopSkip			;if b is full, add to c
	sra c						;shift c over
	set 7,c						;set left pixel of c
MemDivLoopSkip:					;all times come here
	add hl,de					;subtract hl-3048
;	push hl						;save hl					;no need to save hl for cphlde
	push de						;save de
		ld de,TotalRAMSize/8		;check if hl=<3048
		bcall(_cphlde)				;compare
		pop de						;recall de...
;	pop hl						;...and hl
	jp p,MemDivLoop				;if hl>3429, do it again
	ld a,b						;get left side
	and %00001111				;mask first 4 pixels
	ld b,a						;save it
	ld a,c						;get right side
	and $e0						;last 3 pixels only
	ld c,a						;save that too
	ld hl,PlotsScreen+(12*59)+8	;get left side
	ld a,(hl)					;recall screen image
	or b						;or with left side
	ld (hl),a					;save it
	inc hl						;next 8 pixels
	ld a,(hl)					;get cur value
	or c						;add right mask
	ld (hl),a					;put it back
	ld a,(8447h)					;actually checks contrast, here was ld a,(battlevel)
	ld de,$01fc					;init batt mask (for full)
BattDivLoop:						;shift loop here
	sub 2						;take battlevel-5
	sla e						;shift it to the left
	jr nc,BattDivLoop2				;if didn't overflow, skip over
	sla d						;otherwise work with d
	inc d						;inc it
	jr BattDivLoop3					;continue
BattDivLoop2:						;if none to add
	sla d						;shift over
BattDivLoop3:						;all continue here
	cp 22						;check if level is now>0
	jp p,BattDivLoop				;if so, loop up
	ld a,d						;get d value
	and $03						;mask it
	ld d,a						;save it
	ld hl,PlotsScreen+9d+(12*58)			;screen location
	ld b,3						;3 rows to do
BattDivLoop4:						;disp loop
	ld a,(hl)
	or d
	ld (hl),a
	inc hl
	ld a,(hl)
	or e
	ld (hl),a
	push de
		ld de,11
		add hl,de
		pop de
	djnz BattDivLoop4
	ld ix,TaskbarIcons
	;ld b,5
	;ld c,4
	ld bc,(5*256)+4
	ld a,64
	ld l,57
	call iLargeSprite
	ld ix,StartMenuSprite
	;ld b,9
	;ld c,2
	ld bc,(9*256)+2
	ld a,c					;was ld a,2
	ld l,55
	call iLargeSprite
	ld a,(CurFldr)
	cp 1
	jr z,SkipFldUpIcon
	push af
		ld ix,UpFldIcon
		ld a,18
		ld l,57
		ld b,5
		call iPutSprite
		ld hl,BaseFld
		ld de,Op1
		ld bc,7
		ldir
		dec de
		dec de
		pop af
	ld (de),a
	bcall(_chkfindsym)
	ld a,b
	ld (CurROMPage),a
	ex de,hl
	call SetUpROM
	inc hl
	inc hl
	ld de,Op1
	push de
		ld bc,8
		call ldirROMRAM
		xor a
		ld (de),a
		ld hl,(56*256)+26
		ld (pencol),hl
		pop hl
	call vputsapp
SkipFldUpIcon:
	ld a,(TotalProgs)
#ifdef Folders
	or a
	jr z,DesktopContinue2
#endif
	dec a
DesktopContinue2:
	ld hl,ScrollBarData
	ld de,CmdShadow
	push de
	ld bc,ScrollBarDataEnd-ScrollBarData
	ldir
	ld hl,CmdShadow+8
	ld a,(TotalProgs)
	push hl
	ld l,a
	ld h,0
	ld a,6
	bcall(_divhlbya)
	or a
	jr z,DrawScrollDeskNoRem
	inc l
DrawScrollDeskNoRem:
	ld a,l
	or a
	jr nz,DrawScrollDesk1
	inc a
DrawScrollDesk1:
	add a,a
	ld b,a
	add a,a
	add a,b
	pop hl
	ld (hl),a
	inc hl
	inc hl
	ld a,(ProgsDone)
	ld (hl),a
	pop bc
	ld a,(bc)
	ld h,a
	ld d,a
	inc bc
	ld a,(bc)
	ld l,a
	inc bc
	bcall(_GUIRScrollVertDesk)
#ifdef enablecn2
#ifdef Debug
	ld hl,(56*256)+25
	ld (pencol),hl
	ld a,(SIDSave+4)
	call vdispa
	ld hl,gbuf+715
	ld a,(hl)
	or %01001000
	ld (hl),a
#endif
#endif
	call iFastCopy
MainMouseLoop:					;Jump back here when hotspot falls through for all checks
;---Call mouse---

	ld a,$c7					;performance optimizations
	bcall(_OpenGUIStack)
	ld hl,GUIDesktop_NullR
	bcall(_PushGUIStacks)

	ld a,1								;a=1
	ld (LastClick),a					;needed in case of folder switch
	dec a								;a=0
	ld (MouseMode),a
	dec a								;a=255
	ld (TabFuncMode),a
	ld hl,MainMouseHook
	bcall(_GUIMouse)

DesktopMouseReturn:
	;among other things, the GUIMouse routine already sets LastClick
	pop hl \ pop hl \ pop hl	;the bcall stuff
	
	push bc
		bcall(_CloseGUIStack)
		pop bc
	ld a,b
	cp 6
	jr nc,DesktopMouseReturn2
	add a,a
	ld h,a
	add a,a
	add a,h
	ld l,a
	ld h,0
	ld de,VFAT
	add hl,de
	push hl
		pop ix
	jp RunProgFromDesktop
DesktopMouseReturn2:
	sub 6
	jr z,UpFldGo
	dec a
	jr z,DCSMenuOpen
	dec a
	jr z,ScrollUp
	dec a
	jr z,ScrollDown
	dec a
	jp z,ExitDoorsCS
	dec a
	jp z,RenderDesktop
	dec a
	jp z,ScrollWayUp
	dec a
	jp z,ScrollWayDown
	jp MainMouseLoop		; //?????// this should never happen (b>10)

UpFldGo:
	jp UpFld
DCSMenuOpen:
	bcall(_OpenStartMenu)
	ret

ScrollUp:
	call ScrollUpSub
	jp nz,RenderDesktop
	xor a
	bcall(_GUIDrawHourglass)
ScrollUpLoopToEnd:
	call ScrollDownSub
	push af
		call DrawProgsToScreen
		pop af					;populate NextVATStart
	jr nc,ScrollUpLoopToEnd
	jp RenderDesktop

ScrollUpSub:
	ld a,(ProgsDone)
	or a
	ret z
	sub 6
	ld (ProgsDone),a
	ld hl,(CurVATStart)
	ld (NextVATStart),hl
	ld hl,(CurVATPtr)
	push hl
	ld de,PrevVATArray-2
	add hl,de
	bcall(_ldhlind)
	ld (CurVATStart),hl
	pop hl
	dec hl
	dec hl
	ld (CurVATPtr),hl
	or h					;unset z
	ret

ScrollDown:
	call ScrollDownSub
	jp nc,RenderDesktop
	ld hl,(progptr)					;get start of VAT
	ld (CurVATStart),hl				;Set first program to check
	xor a
	ld (ProgsDone),a
	jr ScrollJpRenderDesktop
	
ScrollDownSub
	ld a,(ProgsDone)
	ld b,a
	ld a,(TotalProgs)
	sub b
	bit 7,a
	jr nz,ScrollDown2
	cp 6+1
	ret c
ScrollDown2:
	ld a,6
	add a,b
	ld (ProgsDone),a
	ld hl,(CurVATStart)
	ld (PrevVATStart),hl
	push hl
	ld hl,(CurVATPtr)
	ld de,PrevVATArray
	add hl,de
	pop de
	ld (hl),e
	inc hl
	ld (hl),d
	ld hl,(CurVATPtr)
	inc hl
	inc hl
	ld (CurVATPtr),hl
	ld hl,(NextVATStart)
	ld (CurVATStart),hl
	or a							;rcf
	ret
	
ScrollWayValue:
	ld b,4
	ld a,(TotalProgs)
	cp 60
	ret nc
	dec b
	cp 36
	ret nc
	dec b
	ret

ScrollWayUp:
	call ScrollWayValue
ScrollWayUpLoop:
	push bc
		call ScrollUpSub
		pop bc
	jr c,ScrollJpRenderDesktop
	push bc
		call DrawProgsToScreen
		pop bc
	djnz ScrollWayUpLoop
ScrollJpRenderDesktop:
	jp RenderDesktop

ScrollWayDown:
	call ScrollWayValue
ScrollWayDownLoop:
	push bc
		call ScrollDownSub
		pop bc
	jr c,ScrollJpRenderDesktop
	push bc
		call DrawProgsToScreen
		pop bc
	djnz ScrollWayDownLoop
	jr ScrollJpRenderDesktop

#ifdef Folders
UpFld:
	ld a,(CurFldr)
	cp 1
	jr z,UpFldNo
	ld b,a
	ld a,1
	call FldSearch
	ld a,c
	or a
UpFldNo:
	jp z,MainMouseLoop
	ld (CurFldr),a
	jp RealStartNoReset
#endif

;--------------------------------------------------------------
#ifdef StandardEdition
PropMenu:
	push ix
		inc ix
		inc ix
		inc ix
		inc ix
		inc ix
		pop hl
	bcall(_ldhlind)
	ld a,$05
	bit 0,(ix)
	jr z,PropLock
	ld (hl),a
	jr PropLock2
PropLock:
	inc a
	ld (hl),a
PropLock2:
	jp RenderDesktop
#endif
#ifdef ProEdition
#include "dcspropp.asm"
#endif
;---------------------------------
Off:
	ld a,2
	out ($10),a ;turn off screen
	res OnInterrupt,(iy+OnFlags)
;	res curAble,(iy+curFlags)
	call debounceon
	call calcAndStore_OSChecksum
;	in a,(3)
;	push af
;		im 1
		ei								;enable interrupts
		ld	a,1						;enable ONLY [ON] key, no linkport wake
		out	(3),a
		halt
		di
		res OnInterrupt,(iy+OnFlags)
;		pop af
;	or %00001000					;don't go into low-power mode on halt
	ld a,%00001011
	out (3),a
	ld a,3
	out ($10),a ;turn on screen
;	call debounceon
;	ret

debounceon:
; ld a,%00001000			;only on, no [ON], no hardware timer
; out (3),a
; nop \ nop
 ld b,16
debounceoninner:
 in a,(4)
 bit 3,a
 jr z,debounceon
 djnz debounceoninner
 ret
;-------------------------------
HLine:
	ld b,12
HLine1:
	ld (hl),$ff
	inc hl
	djnz HLine1
	ret
;-------------------------------
VATFind:
	ld hl,(CurVATStart)
	ld de,VFAT
	ld a,(VATMode)
	dec a
	jr z,VATMainLoop
	ld hl,(progptr)
VATMainLoop:
	push de				;save VFAT loc
		push hl
			ld de,(ptemp)			;first byte after VAT
			or a
			sbc hl,de			;are we there?
			pop hl
		pop de				;recall VFAT loc
	ret z				;end of VAT
	ld a,(VATMode)
	or a
	jr nz,VATMainLoopNoCount
	ld a,(scratchvar)
	inc a
	ld (ScratchVar),a
VATMainLoopNoCount:
	xor a				;a=0
	ld (iy+dcsProgFlags),a		;clear the flags
	ld a,(hl)			;get current byte in VAT
	and 1fh				;mask out lowest 5 bits
	sub 5		;cp 05h
	jr z,VATValid
	set 0,(iy+dcsProgFlags)
	dec a		;cp 06h
	jr z,VATValid
	push af
		ld a,(VATMode)
		or a
		jr nz,VATNoList1
		ld a,(ScratchVar)
		dec a
		ld (ScratchVar),a
		jr VATNoList2
VATNoList1:
		ld a,(ProgsToDo)
		inc a
		ld (ProgsToDo),a
VATNoList2:
		pop af
	dec hl
	dec hl
	dec hl
	dec hl
	call VATValidSkip3
	jr VATValid+3
VATValid:
	call VATProcessEntry
	jp VATMainLoop
;---------------------------------------
ldirROMRAM:
	ld a,(CurROMPage)
ldirROMRAM_A:
	or a
	jr z,ldirRAM
ldirROM:
	call GetArcProgByte
	ld (de),a
	inc hl
	inc de
	dec bc
	ld a,b
	or c
	jr nz,ldirROM
	ret
ldirRAM:
	ldir
	ret

GetArcProgByte:
	push de
		push bc
			ld a,(CurROMPage)
			or a
			jr z,GetArcProgByteReg
			ld b,a
			call AutoArcPtrUpdate
			push hl
				ld a,b
				ld (CurROMPage),a
				bcall(_LoadCIndPaged)
				ld a,c
				pop hl
GetArcProgByteFinish:
			pop bc
		pop de
	ret
GetArcProgByteReg:
			ld a,(hl)
			jr GetArcProgByteFinish

GetArcProgByteDE:
	ex de,hl
	call GetArcProgByte
	ex de,hl
	ret

GetArcProgWord:
	push de
	push bc
	ld a,(CurROMPage)
	or a
	jr z,GetArcProgWordReg
	ld b,a
	call AutoArcPtrUpdate
	bcall(_LoadDEIndPaged)
	ex de,hl
	pop bc
	pop de
	ret
GetArcProgWordReg:
	pop bc
	pop de

	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a					;_ldhlind

	ret
AutoArcPtrUpdate:
	bit 7,h
	ret z
	inc b
	res 7,h
	set 6,h
	ret
SetUpROM:
	ld a,(CurROMPage)
	or a
	ret z
	push de
		ld de,9
		add hl,de
		call GetArcProgByte
		ld d,0
		inc a
		ld e,a
		add hl,de
		pop de
	ret
#ifdef enableCn2
DataReceived:
	ld hl,0
	ld (pencol),hl
	ld hl,Cn2_Int_RecBuf+7
	bcall(_vputs)
	call Cn2_Clear_Recbuf
	jp ifastcopy
#endif
;--------------------------
MouseAccelReset:
	ld hl,(AppVarLoc)
	ld de,37
	add hl,de
	ld b,(hl)
	ld hl,MouseAccelAct
	push hl
	ld a,(SETotal)
	or a
	jr z,MouseAccelSet
	ld hl,(AppVarLoc)
	ld de,66
	add hl,de
	ld b,(hl)
MouseAccelSet:
	pop hl
	ld (hl),b
	inc hl
	ld (hl),b
	ret
RunNameCopy:
	bcall(_ldhlind)
RunNameCopy2:
	push hl
		bcall(_ZeroOp1)
		pop hl
	ld a,(hl)
	ld de,Op1
	ld (de),a
	inc de
	dec hl
#ifdef folders
	ld a,(hl)
	ld (Op6),a
#endif
	dec hl
	dec hl
	dec hl	
	dec hl
	dec hl
	ld b,(hl)
	dec hl
RunBasicNameCopy:
	ld a,(hl)
	ld (de),a
	inc de
	dec hl
	djnz RunBasicNameCopy
	xor a
	ld (de),a
	bcall(_chkfindsym)
	ret nc
	ld hl,503
	jp DCSError

dcsSquish:				;optimized - should still work
	push bc
		;ld c,2
		;ld b,0
		ld bc,(0*256)+2
dcsSquishLoop:
		call GetArcProgByte		;A=ascii 65d
		push hl				;0=ascii 48d
			push de
				ld e,48d
				sub e
				cp 10h
				jr c,dcsSquish_Num
				sub 7				;save 5 bytes
dcsSquish_Num:
				or b
				pop de
			pop hl
		inc hl
		dec c
		jr z,dcsSquishNext
		sla a
		sla a
		sla a
		sla a
		ld b,a
		jr dcsSquishLoop
dcsSquishNext:
		ld (de),a
		inc de
		pop bc
	dec bc
	ld a,b
	or c
	jr nz,dcsSquish
	ret

dcsEnlarge:			;turns 8x8 sprite at hl into 16x16 at de
	ld c,8 
ByteLoop: 
	ld b,4 
OuterLoop: 
	push bc 
		ld b,4 
InnerLoop: 
		rlc (hl) 
		push af
			rl c 
			pop af 
		rl c 
		djnz InnerLoop 
		ld a,c 
		ld (de),a
		inc de 
		pop bc 
	djnz OuterLoop 
	inc hl 
	dec c 
	jr nz,ByteLoop 
	ret
	
DCSError:
	push hl
		bcall(_PushOp1)
		bcall(_grbufclr)
		bcall(_clrlcdfull)
;		bcall(_homeup)
		xor a
		ld l,a
		ld b,8
		ld ix,ErrDialogIcon
		call iPutSprite
		call iFastCopy
		ld hl,(2*256)+0
		ld (CurRow),hl				;CurRow = 0, Curcol = 2
		ld hl,ErrTxt
		call putsApp
		pop hl
	push hl
		bcall(_disphl)
		bcall(_PopOp1)
		pop hl
	push hl
		ld de,502d
		bcall(_mos_cphlde)			;was ld de,502d \ or a \ sbc hl,de
		jr z,DCSError502503
		inc de
		bcall(_mos_cphlde)
		jr nz,DCSErrorPause
DCSError502503:
		bcall(_newline)
		ld hl,prgmTxt
		call PutsApp
		xor a
		ld (Op1+9),a				;touched Op2! If we care, that is.
		ld hl,Op1+1
		call putsApp
DCSErrorPause:
		bcall(_newline)
		pop hl
	ld de,-501d
	add hl,de						;was ld de,501d \ or a \ sbc hl,de
	ld a,l
	cp 6
	jr nc,DCSErrorBASIC
	add hl,hl
	ld de,ErrorTxtList
	add hl,de
	bcall(_ldhlind)
DCSErrorPauseDisp:
	call PutsApp
	jp Pause
DCSErrorBASIC:
	;hl is already hl-501=[501-556]-->[0-55]
	push hl
		add hl,hl
		pop de
	add hl,de
	ld de,ErrCodes
	add hl,de
	push hl
		ld hl,BASICErrorTxt
		call PutsApp
		pop hl
	ld b,3
DCSErrorCodeLoop:
	ld a,(hl)
	bcall(_putc)
	inc hl
	djnz DCSErrorCodeLoop
	ld hl,GotoErrL1
	call PutsApp
	bcall(_newline)
	ld hl,GotoErrL2
	call PutsApp
DCSErrorKeyWait:
	call Pause
	cp $1a				;[2]
	jr z,DCSErrorMenu_Opt2
	cp $22				;[1]
	jp z,DCSErrorBASICDone
	jr DCSErrorKeyWait
DCSErrorMenu_Opt2:
	bcall(_grbufclr)
	call DAVLCheck
	ld a,1
	ld (EditorMode),a
	ld hl,parseVar
	inc hl
	ld a,(hl)
	dec hl
	cp $24
	jr nz,DCSErrorBASIC_UseParseVar
	rst 20h
	rst 10h							;bcall(_FindSym)
	ret c							;it's missing??
	inc de
	inc de
	ld hl,Op1
	ex de,hl

	ldi
	;ld a,(hl)
	;ld (de),a
	;inc hl
	;inc de

	;ld c,(hl)
	;ld a,c
	ld a,(hl)
	or a
	jr z,DCSErrorBASIC_UseProgChain
	inc hl
	ld b,0
	ldir
	xor a
	ld (de),a				;should be zero now
	jr DCSErrorBASIC_CheckVar
DCSErrorBASICGotoArc:
	call GetArcStatus
	or a
	jr z,DCSErrorBASICDone	;if it was unarchived, failures occurred
	call GetProgChainTop
	ret c					;return if no chain
	inc hl					;past the type flag (prog/AP file)
	rst 20h
	bcall(_chkfindsym)
	ret c
	jr DCSErrorBASIC_Edit
DCSErrorBASIC_UseProgChain:
	call GetProgChainTop
	inc hl
DCSErrorBASIC_UseParseVar:
	rst 20h
DCSErrorBASIC_CheckVar:
	bcall(_chkfindsym)
	jr c,DCSErrorBASICGotoArc
DCSErrorBASIC_Edit:
	ld a,(hl)
	and $1f
	sub 5
	jr z,DCSErrorBASIC_EditGo
	dec a
	jr nz,DCSErrorBASICDone
DCSErrorBASIC_EditGo:
	call EditorJumpIn
DCSErrorBASICDone:
	scf						;SET carry flag!
	ret

RealStartNoResetRetFrom2:
	ld hl,RealStartNoReset
	jr ResetFrom2
RenderDesktopRetFrom2:
	ld hl,RenderDesktop
ResetFrom2:
	pop de
	pop de
	pop de
	pop de
	pop de
	pop de
	jp (hl)
ExitDoorsCS:
	bcall(_CloseGUIStack)
	ld a,(SETotal)
	or a
	jr z,NoSEAtEnd

	ld hl,7
	call RunSEs

NoSEAtEnd:
	call FldSave						;see if we need to back up folders
	res TextWrite,(iy+sgrFlags)
	set IndicRun,(iy+IndicFlags)	;turn off the run indicator
	ld hl,$966e
	ld de,$966e+1
	ld (hl),' '
	ld bc,127
	ldir
	res lwrCaseActive,(iy+appLwrCaseFlag)
	ld hl,33
	call DAVLCheckOffset
	ld a,(hl)
	or a
	jr z,NoLowerAtEnd
	set lwrCaseActive,(iy+appLwrCaseFlag)
NoLowerAtEnd:
;	ld hl,DoorsCSVersion
;	call putsApp
;	ld hl,AuthorTxt
;	call putsApp
;	bcall(_newline)
	ld a,%11010000
	out (0),a
	im 1
	ei
	bcall(_getcsc)
	bcall(_ReloadAppEntryVecs)
	bcall(_grbufclr)
	bcall(_clrlcdFull)
	bcall(_ClrTxtShd)
	bcall(_homeup)
	set graphDraw,(iy+graphFlags)
	bcall(_EnableLaunchKeyhook)
	bcall(_EnableMyParserhook)
	call EnableLaunchOffscrpt
	ld a,kClear
	bjump(_jforcecmd)

Arc_Unarc:
Arc_UnarcP2:
	push bc
		push hl
			push de
				bcall(_pushop1)
				xor a
				bcall(_GUIDrawHourglass)
				pop de
			pop hl
		pop bc
	ld a,b
	ld (CurROMPage),a
	or a
	jr z,Arc_UnarcROM
	ex de,hl
	call SetUpROM
	call GetArcProgWord
	ex de,hl
	bcall(_FreeRAM)
	or a
	sbc hl,de
	jp c,Arc_UnarcErr
;	pop bc
;	pop de
;	pop hl
	jr Arc_Unarc1
Arc_UnarcROM:
	push hl
		push de
			bcall(_chk_batt_low)
			pop de
		pop hl
	jr z,Arc_UnarcErr
	ex de,hl
	bcall(_Ldhlind)
	ex de,hl
	push de
		bcall(5014h)
		ld de,839Fh
		ld a,(de)
		ld h,a
		inc de
		ld a,(de)
		or h
		jr nz,Arc_Unarc_ROMFine
		inc de
		ld a,(de)
		ld h,a
		inc de
		ld a,(de)
		ld l,a
		pop de
	or a
	sbc hl,de
	jr c,Arc_UnarcErr
	push de							;was jr Arc_Unarc_ROMFine1
Arc_Unarc_ROMFine:
		pop de
Arc_Unarc_ROMFine1:
Arc_Unarc1:
	push hl
		push de
			push bc
				ld a,(iy+34h)
				ld (hookBackup),a
				ld hl,9B88h
				ld de,hookBackup+1
				ld bc,4
				ldir
				ld a,1
				ld (de),a								;hookbackup+5
				ld hl,GarbageCollectHook
				in a,(6)
				bcall(_EnableGetCSCHook)				;enable GarbageCollect thingie
				bcall(_popop1)
				pop bc
			pop de
		pop hl
	call iCheckInts0
	push af
		bcall(_Arc_Unarc)
		pop af
	jp pe,Arc_Unarc_NoDI
	di
Arc_Unarc_NoDI:
	push hl
		push de
			push bc
				ld hl,hookBackup+5
				ld a,(hl)
				or a
				jr z,Arc_Unarc_NoHookRestore
				ld hl,hookBackup
				ld a,(hl)
				ld (iy+34h),a
				inc hl
				ld de,9B88h
				ld bc,4
				ldir
Arc_Unarc_NoHookRestore:
				pop bc
			pop de
		pop hl
	ld a,1
	bcall(_GUIDrawHourglass)
	ret
Arc_UnarcErr:
	bcall(_popop1)
;	ld hl,$0000
;	add hl,sp
;	push hl
;;		ld hl,(AppVarLoc)
	;	ld de,AVOff_SPSave
	;	add hl,de
;		bcall(_ldhlind)
;		ex de,hl
;		pop hl
;	ex de,hl
;	or a
;	sbc hl,de
;Arc_UnarcErrL:
;		pop de
;	dec hl
;	dec hl					;each stack entry is *TWO* bytes!!!!
;	ld a,h
;	or l
;	jr nz,Arc_UnarcErrL
	ld hl,506d
	jp DCSError
	;ret
;	jp RealStartNoReset

GarbageCollectHook:
	add a,e						;standard $83 header
	cp $1A
	jr nz,GarbageCollectHookNo_1B
	ld a,(MenuCurrent)
	cp $40						;GarbageCollect under 2.53MP+
	jr z,GarbageCollectHookYes
	cp $3F						;GarbageCollect
	jr nz,GarbageCollectHookNo
	
GarbageCollectHookYes:
	;destroy the GetCSCHook
	bcall(_DisableGetCSCHook)				;enable GarbageCollect thingie
	ld a,(hookBackup)
	ld (iy+34h),a
	ld hl,hookBackup+1
	ld de,9B88h
	ld bc,4
	ldir
	ld (hl),0								;zero out the was-it-removed flag

	ld a,(iy+35h)
	ld (hookBackup),a
	ld hl,9BCCh
	ld de,hookBackup+1
	ld bc,4
	ldir
	ld hl,GarbageCollectWinHook
	in a,(6)
	bcall(_EnableLocalizeHook)				;enable GarbageCollect thingie
	ld a,k2
	cp a						;set z
	ret
GarbageCollectHookNo:
	ld a,$1A
	ret
GarbageCollectHookNo_1B:
	ld a,b
	or a						;set z if a=0, nz otherwise
	ret
GarbageCollectWinHook:
	add a,e
	cp $73
	jr z,GarbageCollectWinHook2
;	cp $72
;	ret nz
	cp a									;set z flag
	ret
GarbageCollectWinHook2:
	bcall(_grbufclr)
	xor a									;do _not_ try to swap the gui7 appvar
	bcall(_OpenGUIStack)
	push bc									;remember if it was open
		ld hl,GCSMWin
;		ld de,GCSMWin1-GCSMWin
;		ld a,$2
;		bcall(_PushGUIStack)
;		ld hl,GCSMWinB
;		ld de,GCSMWinB1-GCSMWinB
;		ld a,$5
;		bcall(_PushGUIStack)
;		ld hl,GCIcon
;		ld de,GCIcon1-GCIcon
;		ld a,$11
;		bcall(_PushGUIStack)
;		ld hl,GCText
;		ld de,GCText1-GCText
;		ld a,$4
		bcall(_PushGUIStacks)
		bcall(_RenderGUI)
		ld b,4
		bcall(_PopGUIStacks)
		pop bc
	ld a,c
	or a
	bcallz(_CloseGUIStack)
	;destroy this hook
	ld a,(hookBackup)						;restore previous localize hook
	ld (iy+35h),a
	ld hl,hookBackup+1
	ld de,9BCCh
;	push hl
		ld bc,4
		ldir
;		pop hl
;	ld (hl),0								;tell the main code after arc_unarc not to bother destroying this or the previous hook
	cp a									;set z flag
	ret							

#ifdef adcsswap
ADCSSwap:						;swap prgmZDCS to end of VAT
	ld hl,ADCStxt					;prgmADCS
	rst 20h						;copy to Op1
	bcall(_chkfindsym)				;find mem locs
	push hl						;save VAT entry...
		push de						;...and mem location
			bcall(_ldhlind)					;get size
			push hl						;put into...
				pop bc						;...bc
			pop de						;recall mem loc and
		pop hl						;vat entry
	push bc						;Save size
		ld b,0						;Not archived
		bcall(_delvar)					;delete prgmADCS
		pop hl						;recall prog size
	push hl						;save prog size
		bcall(_createprotprog)				;program of nnnn bytes
		ld (hl),$06					;lock the program
		inc de						;get past size
		inc de						;to first prog byte
		ld a,$BB
		ld (de),a
		inc de
		ld a,$6D
		ld (de),a
		inc de
		pop bc
	dec bc
	dec bc						;recall size
	ld hl,$9D95					;copy from usermem
	ldir						;to new prog
	ex de,hl					;hl=end of prog
	dec hl						;hl=last byte of prog
	ld ($965F),hl					;put in end prog BASIC int
	inc hl						;increment ptr
	ld ($965D),hl					;cur byte > last, so BASIC parser quits.
	ret						;done w/ the load
#endif

RunSEs:
	ld a,(SETotal)
	ld b,a
	ld de,SETable
	ex de,hl				;table offset (3/5/7) in de now
RunSELoop:
	push de
		push hl
			push bc
				ld a,(hl)
				inc hl
				ld b,(hl)
				push hl
					pop ix
				inc ix
				ld l,a
				ld h,b
				or b
				jr z,RunSEContinue
				push hl
					add hl,de
					ld a,(hl)
					inc hl
					ld h,(hl)
					ld l,a
					or h
					pop bc
				jr z,RunSEContinue
				add hl,bc				;table offset still in de
				call RunSENow
RunSEContinue:
				pop bc
			pop hl
		ld a,d
		ld de,4
		add hl,de
		pop de
	djnz RunSELoop
	ld d,a
	ret
RunSENow:
	ld a,e
	cp 5
	jr nz,RunSENowCopy
	ld a,(SETotal)
	dec a
	jr nz,RunSENowCopy
	ld a,(SETable+4+2)
	or a
	jr nz,RunSENowGo
	inc a
	ld (SETable+4+2),a
RunSENowCopy:
	ld de,SEram
	ld bc,768
	ldir
RunSENowGo:
	ld e,0
	jp SEram					;remove SE running from here

;------------------------------
DrawProgsToScreen:
	ld c,6
	ld a,(ProgsDone)
	ld b,a
	ld a,(TotalProgs)
	sub b
	bit 7,a
	jr z,DesktopContinue1
	cp 6
	jr nc,DesktopContinue1
	ld c,a
DesktopContinue1:
	ld a,c
	ld (ProgsToDo),a
	
	ld hl,(3*256)+3
	ld (SpriteX),hl
	ld hl,(21*256)+3
	ld (NameX),hl
	ld a,1						;was xor a \ inc a
	ld (VATMode),a
	call VATFind				;Shows icons and names
	ld (NextVATStart),hl
	ret
;------------------------------

#ifdef false
Restart:
	;call StackStrip
	pop hl
	pop hl
	pop hl
	jp RealStart

Shutdown:
	pop hl
	pop hl
	pop hl
	bcall(_grbufclr)
	bcall(_clrlcdfull)
;	call StackStrip
	call Off
	jp RenderDesktop
#endif
QuitToOS:
	pop hl
	pop hl
	pop hl
;	call StackStrip
	jp ExitDoorsCS
;---------------------------------------------		
#include "datap0.inc"
#ifdef false
LibTable:
	jp iVersion
	jp iRandom
	jp iPutSprite
	jp iLargeSprite
	jp iGetPixel
	jp iFastCopy
	jp iDetect
	jp iDecompress

	jp Small_Window
	jp ClrDialogFull
	jp LargeWindow
	jp ClrWinFull
	jp PlaySound
	jp VDispHL
	jp Pause
	jp hDetect
LibTableEndRAM:
#endif