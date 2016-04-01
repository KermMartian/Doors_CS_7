;-----------------------------------------------------------
;	Filename:		dcsblibs.asm
;	Long name:  	DCSB hybrid libraries for TI-BASIC programs
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

; Doors CS BASIC Libraries
;================================
;dbfStringWidth:			COMPLETE
;dbfPxScan:					COMPLETE
;dbfHometoGraph:			COMPLETE
;dbfUsedPicList:			COMPLETE
;dbfArcUnarcPic:			COMPLETE
;dbfDCSLibVersion:			COMPLETE
;dbfSimpleGUIMouse:			COMPLETE
;dbfPushGUIStack:			COMPLETE
;dbfPopGUIStack:			COMPLETE
;dbfOpenGUIStack:			COMPLETE
;dbfCloseGUIStack:			COMPLETE
;dbfRenderGUIStack:			COMPLETE
;dbfGUIMouse:				COMPLETE
;dbfMenu:					COMPLETE
;dbfCn2BASICSend:			INCOMPLETE
;dbfCn2BASICGet:			INCOMPLETE
;dbfCn2BASICStop:           Mostly Complete
;================================
#define bjump(xxxx) call 50h \ .dw xxxx
dbfStringWidth:
	ld hl,sc1
	call getsourcestring
	ld de,0
	jp z,dbfStringWidth_Finish  ;zero-length string
dbfStringWidthLoopOuter:
	push hl
		push bc
			push de
				bcall(_Get_Tok_Strng)
				ld hl,Op3
				pop de
dbfStringWidthLoopInner:
			push hl
				push bc
					ld a,(hl)
					push de
						bcall(_GetVarWidth)
						ld l,d
						ld h,0
						pop de
					add hl,de
					ex de,hl
					pop bc
				pop hl
			inc hl
			dec bc
			ld a,b
			or c
			jr nz,dbfStringWidthLoopInner
			pop bc
		pop hl
	ld a,(hl)
	inc hl
	dec bc
	bcall(_IsA2ByteTok)
	jr nz,dbfStringWidthLoopOuter2
	inc hl
	dec bc
dbfStringWidthLoopOuter2:
	ld a,b
	or c
	jr nz,dbfStringWidthLoopOuter
dbfStringWidth_Finish:
	ex de,hl
	bcall(_setxxxxop2)
	bcall(_op2toop1)
	ret
;================================
dbfPxScan:
	ld a,(var2)
	ld e,a
	ld a,(var1)
	ld d,a
	push de
		call immGetPixel
		pop de
	ld c,a
	ld a,(var3)
	dec a
	jr z,dbfPxScanUp
	dec a
	jr z,dbfPxScanDown
	dec a
	jr z,dbfPxScanLeft
dbfPxScanRight:
	ld a,96
	sub d
	ld b,a
	ld a,(hl)
	and c
	ld d,a
;-------------------------------
; b: number of iterations
; d: pixel to xor with
; hl: address
; c: current mask
dbfPxScanRightLoop:
	ld a,b
	or a
	jr z,dbfPxScanRightEnd
	dec b
	rrc d
	rrc c
	jr nc,dbfPxScanRightNoInc
	inc hl
dbfPxScanRightNoInc:
	ld a,(hl)
	and c
	xor d
	jr z,dbfPxScanRightLoop
dbfPxScanRightEnd:
	ld a,96
	sub b
dbfPxScanRightEndSet:
	bit 7,a
	jr nz,dbfPxScanRightEndSetNeg
	bcall(_SetXXOp1)
	ret
dbfPxScanRightEndSetNeg:
	neg
	bcall(_SetXXOp1)
	bcall(_InvOp1S)
	ret
dbfPxScanLeft:
	ld b,d
	inc b
	ld a,(hl)
	and c
	ld d,a
;-------------------------------
; b: number of iterations
; d: pixel to xor with
; hl: address
; c: current mask
dbfPxScanLeftLoop:
	ld a,b
	or a
	jr z,dbfPxScanLeftEnd
	dec b
	rlc d
	rlc c
	jr nc,dbfPxScanLeftNoInc
	dec hl
dbfPxScanLeftNoInc:
	ld a,(hl)
	and c
	xor d
	jr z,dbfPxScanLeftLoop
dbfPxScanLeftEnd:
	dec b
	ld a,b
	jr dbfPxScanRightEndSet
dbfPxScanDown:
	ld a,64
	sub e
	ld b,a
	ld a,(hl)
	and c
	ld d,a
;-------------------------------
; b: number of iterations
; d: pixel to xor with
; hl: address
; c: current mask
dbfPxScanDownLoop:
	ld a,b
	or a
	jr z,dbfPxScanDownEnd
	dec b
	push de
		ld de,12
		add hl,de
		pop de
	ld a,(hl)
	and c
	xor d
	jr z,dbfPxScanDownLoop
dbfPxScanDownEnd:
	ld a,64
	sub b
	jr dbfPxScanRightEndSet
dbfPxScanUp:
	ld b,e
	inc b
	ld a,(hl)
	and c
	ld d,a
;-------------------------------
; b: number of iterations
; d: pixel to xor with
; hl: address
; c: current mask
dbfPxScanUpLoop:
	ld a,b
	or a
	jr z,dbfPxScanUpEnd
	dec b
	push de
		ld de,-12
		add hl,de
		pop de
	ld a,(hl)
	and c
	xor d
	jr z,dbfPxScanUpLoop
dbfPxScanUpEnd:
	dec b
	ld a,b
	jr dbfPxScanRightEndSet
	
;================================
dbfHometoGraph:
	ld hl,textShadow
	ld a,(iy+FontFlags)
	push af
		ld a,(iy+sGrFlags)
		push af
			set textWrite,(iy+sGrFlags)
			set fracDrawLFont,(iy+fontFlags)
			set fullScrnDraw,(iy+apiflg4)
			set 4,(iy+24h)
			ld b,8
			xor a
			bit grfSplit,(iy+SGrFlags)
			jr z,dbfHometoGraphBegin
			ld b,4
			ld hl,textShadow+4*16
			ld a,32
dbfHometoGraphBegin:
			ld (penrow),a
dbfHometoGraphOuter:
			xor a
			ld (pencol),a
			ld c,16
dbfHometoGraphInner:
			ld a,(hl)					;vputsmap destroys neither HL nor BC
			bcall(_VPutMap)
			inc hl
			dec c
			jr nz,dbfHometoGraphInner
			push hl
				ld hl,penrow
				ld a,8
				add a,(hl)
				ld (hl),a
				pop hl
			djnz dbfHometoGraphOuter
			ld a,(var1)
			or a
			call nz,fastcopy
			pop af
		ld (iy+sGrFlags),a
		pop af
	ld (iy+FontFlags),a
	jp xLIBEndNoOut
;================================
dbfUsedPicList:
	ld bc,0
	ld de,-9
	ld hl,SymTable
dbfUsedPicList_Enumerate:
	push de
		ld de,(progptr)
		bcall(_mos_cphlde)
		pop de
	jr z,dbfUsedPicList_Store
	ld a,(hl)
	and $1f
	cp 7								;pic obj
	jr nz,dbfUsedPicList_EnumNotPic
	inc bc
dbfUsedPicList_EnumNotPic:
	add hl,de
	jr dbfUsedPicList_Enumerate
dbfUsedPicList_Store:
	push bc
		push bc
			pop hl
		ld a,h
		or l
		jr z,dbfUsedPicList_Empty
		bcall(_CreateTRList)			;create an hl-element list
		inc de
		inc de
		pop bc
	ld hl,SymTable
dbfUsedPicList_StoreLoop:
	ld a,b
	or c
	jr z,dbfUsedPicList_Finish
	push de
		ld de,(progptr)
		bcall(_mos_cphlde)
		pop de
	jr z,dbfUsedPicList_Finish
	ld a,(hl)
	push de
		ld de,-9
		add hl,de
		pop de
	and $1f
	cp 7								;pic obj
	jr nz,dbfUsedPicList_StoreLoop
	push hl
		push bc
			push de
				inc hl
				inc hl
				ld a,(hl)
				inc hl
				inc hl
				ld b,(hl)
				push bc
					inc a
					ld l,a
					ld h,0
					bcall(_SetXXXXOp2)
					pop af
				or a
				jr z,dbfUsedPicList_StoreLoopNoNegate
				bcall(_InvOP2S)
dbfUsedPicList_StoreLoopNoNegate:
				ld hl,Op2
				pop de
			ld bc,9
			ldir
			pop bc
		pop hl
	dec bc
	jr dbfUsedPicList_StoreLoop
dbfUsedPicList_Empty:
		inc hl
		bcall(_CreateTRList)			;create an hl-element list
		inc de
		inc de
		pop bc
	push de
		bcall(_Op1Set0)
		ld hl,Op1
		pop de
	ld bc,9
	ldir
dbfUsedPicList_Finish:
	bcall(_Op4ToOp1)
	ret
;================================
dbfArcUnarcPic:
	ld hl,Op1
	ld (hl),7					;tPictObj
	inc hl
	ld (hl),60h					;tVarPict
	inc hl
	ld a,(var1)
	dec a
	ld (hl),a
	inc hl
	ld (hl),0
	bcall(_FindSym)
	jr c,dbfArcUnarcPic_End
	push bc
		ld a,b
		or a
		jr z,dbfArcUnarcPicRAM
		ld b,1
dbfArcUnarcPicRAM
		ld a,(var2)
		cp b
		pop bc
	jr z,dbfArcUnarcPic_End
	bcall(_Arc_Unarc)
dbfArcUnarcPic_End:
	jp xLIBEndNoOut
;================================
dbfDCSLibVersion:
	ld a,5
	bcall(_setxxop1)
	ret
;================================
dbfMouseCoordSet:
	ld a,(var1)				;X
	bit 7,a
	jr z,dbfSimpleGUIMouse_Chk1
	xor a
dbfSimpleGUIMouse_Chk1:
	cp 96
	jr c,dbfSimpleGUIMouse_Chk2
	ld a,95
dbfSimpleGUIMouse_Chk2:
	ld h,a
	ld a,(var2)				;Y
	bit 7,a
	jr z,dbfSimpleGUIMouse_Chk3
	xor a
dbfSimpleGUIMouse_Chk3:
	cp 64
	jr c,dbfSimpleGUIMouse_Chk4
	ld a,63
dbfSimpleGUIMouse_Chk4:
	ld l,a
	ld (MseY),hl
	ret
;================================
dbfSimpleGUIMouse:
	call dbfMouseCoordSet
	bcall(_OpenGUIStack)
	push bc
		ld hl,SimpleGUIMouseData
		bcall(_PushGUIStacks)
		ld hl,(stack)
		push hl
			ld hl,$0000
			bcall(_GUIMouse)
dbfSimpleGUIMouse_Process:
			pop hl
		ld (stack),hl
		ld hl,3
		bcall(_CreateTRList)
		ex de,hl
		inc hl
		inc hl
		ld a,(MseX)
		call AtoRealAtHL
		ld a,(MseY)
		call AtoRealAtHL
		ld a,(lastclick)
		call AtoRealAtHL
		ld b,2
		bcall(_PopGUIStacks)
		pop bc
	ld a,c
	or a
	jr nz,dbfSimpleGUIMouse_Finish
	bcall(_CloseGUIStack)
dbfSimpleGUIMouse_Finish:
	bcall(_Op4ToOp1)
	ret
;================================
dbfPushGUIStack:
	ld hl,xLIBEndNoOut
	push hl
	ld a,(var1)						;get type int
	cp 23d
	ret z
	cp 25d
	ret nc							;24 is the highest allowed
	ld h,0
	ld l,a
	add hl,hl
	ld de,dbfPushGUIStack_EntryTable
	add hl,de
	push af							;save type
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		pop af
	jp (hl)

dbfPushGUIStack_EntryTable:
	.dw dbfPGS_GUIRNull
	.dw dbfPGS_GUIRLargeWin
	.dw dbfPGS_GUIRSmallWin
	.dw dbfPGS_GUIRFullScreenImg
	.dw dbfPGS_GUIRText
	.dw dbfPGS_GUIRWinButtons
	.dw dbfPGS_GUIRWrappedText
	.dw dbfPGS_GUIRButtonText
	.dw dbfPGS_GUIRButtonImg
	.dw dbfPGS_GUIRTextLineIn
	.dw dbfPGS_GUIRRadio
	.dw dbfPGS_GUIRCheckBox
	.dw dbfPGS_GUIRByteInt
	.dw dbfPGS_GUIRWordInt
	.dw dbfPGS_GUIRHotspot
	.dw dbfPGS_GUIRTextMultiline
	.dw dbfPGS_GUIRSprite
	.dw dbfPGS_GUIRLargeSprite
	.dw dbfPGS_GUIRPassIn
	.dw dbfPGS_GUIRScrollVert
	.dw dbfPGS_GUIRScrollHoriz
	.dw dbfPGS_GUIRBorder
	.dw dbfPGS_GUIRRect
	.dw 0000
	.dw dbfPGS_GUIRMouseCursor
;================================
dbfPGS_GUIRNull:
	ld de,1
	bcall(_PushGUIStack)				;a is set up, hl doesn't matter
	ld a,(var2)
	ld (hl),a
	ret
dbfPGS_GUIRLargeWin:
	ld hl,sc2
	ld de,5+1							;one of these is the zero term
	call dbfPGS_GetStrlenToDE_SaveA
dbfPGS_GUIRWinFinish:
	push hl
		ld hl,sc1
		call getsourcestring
		pop de
	ex de,hl
	ld bc,5
	push hl
		push bc
			call HexStringToBin
			pop bc
		pop hl
	add hl,bc					;bc=5 is already set up here
	ld de,sc2
	jp copysourcestring_plain_zt

dbfPGS_GUIRSmallWin:
	ld hl,sc2
	ld de,5+1+1+1				;one of these is the zero term
	call dbfPGS_GetStrlenToDE_SaveA
	ld b,2
	call dbfPGS_LoadVarsToHL
	jr dbfPGS_GUIRWinFinish
dbfPGS_GUIRFullScreenImg:
	ld hl,var2
	dec (hl)							;Pic1 = 0, Pic9=8, Pic0=9, etc
	ld a,(hl)
	call SearchForPic
	ret c
	ld a,b
	or a
	ret nz
	ld a,03
	ld de,768
	bcall(_PushGUIStack)
	ld a,(var2)
	push hl
		call SearchForPic
		pop hl
	inc de
	inc de
	ex de,hl
	ld bc,768-12
	ldir
	push de
		pop hl
	ld (hl),0
	inc de
	ld bc,11
	ldir
	ret
	
dbfPGS_GUIRText:
	ld hl,sc1
	ld de,1+1+1+1							;one of these is the zero term
	call dbfPGS_GetStrlenToDE_SaveA
	ld b,3
	call dbfPGS_LoadVarsToHL
	ld de,sc1
	jp copysourcestring_plain_zt

dbfPGS_GUIRWinButtons:
	ld de,7
	ld hl,dbfPGS_Template_GUIRWinButtons
	bcall(_PushGUIStack)
	ld a,(var2)
	ld (hl),a
	inc hl
	ld b,3
dbfPGS_GUIRWinButtons_ClearLoop:
	push af
		and %10000000
		jr nz,dbfPGS_GUIRWinButtons_ClearLoopEnd
		;a is already zero!
		ld (hl),a
		inc hl
		ld (hl),a
		dec hl
dbfPGS_GUIRWinButtons_ClearLoopEnd:
		inc hl
		inc hl
		pop af
	sla a
	djnz dbfPGS_GUIRWinButtons_ClearLoop
	ret

dbfPGS_GUIRWrappedText:
	ld hl,sc1
	ld de,4+1							;one of these is the zero term
	call dbfPGS_GetStrlenToDE_SaveA
	ld b,4
	call dbfPGS_LoadVarsToHL
	ld de,sc1
	jp copysourcestring_plain_zt

dbfPGS_GUIRButtonText:
	ld hl,sc1
	ld de,4+1							;one of these is the zero term
	call dbfPGS_GetStrlenToDE_SaveA
	ld b,2
	call dbfPGS_LoadVarsToHL
	call dbfPGS_InsGMRPtr
	ld de,sc1
	jp copysourcestring_plain_zt

dbfPGS_GUIRButtonImg:
	ld a,(var4)
	push af
		add a,a
		add a,a
		pop bc
	add a,b
	ld l,a
	ld h,0
	push hl
		ld de,6
		add hl,de
		ld a,8
		ex de,hl
		bcall(_PushGUIStack)
		ld b,2
		call dbfPGS_LoadVarsToHL
		call dbfPGS_InsGMRPtr
		ld a,(var4)
		ld (hl),a
		inc hl
		ld a,(var5)
		ld (hl),a
		inc hl
		push hl
			ld hl,sc1
			call getsourcestring
			pop de
		ex de,hl
		pop bc
	jp HexStringToBin			;de->hl for bc target bytes

dbfPGS_GUIRTextLineIn:
	ld hl,sc1
	ld de,7+1							;one of these is the zero term
	call dbfPGS_GetStrlenToDE_SaveA
	ld b,3
	call dbfPGS_LoadVarsToHL
	ld de,(var5)
	ld (hl),e
	inc hl
	ld (hl),d
	xor a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld de,sc1
	jp copysourcestring_plain_zt

dbfPGS_GUIRRadio:
dbfPGS_GUIRCheckBox:
	ld hl,sc1
	ld de,7+1							;one of these is the zero term
	call dbfPGS_GetStrlenToDE_SaveA
	ld b,4
	call dbfPGS_LoadVarsToHL
	ld de,sc1
	jp copysourcestring_plain_zt

dbfPGS_GUIRByteInt:
	ld de,5
	bcall(_PushGUIStack)
	ld b,5
	jp dbfPGS_LoadVarsToHL

dbfPGS_GUIRWordInt:
	ld de,10
	bcall(_PushGUIStack)
	ld b,2
	call dbfPGS_LoadVarsToHL
	ld de,var4
	ld bc,3*2
	ex de,hl
	ldir
	ret

dbfPGS_GUIRHotspot:
	ld de,6
	bcall(_PushGUIStack)
	ld b,4
	call dbfPGS_LoadVarsToHL
	jp dbfPGS_InsGMRPtr

dbfPGS_GUIRTextMultiline:
	ld hl,sc1
	ld de,6+1							;one of these is the zero term
	call dbfPGS_GetStrlenToDE_SaveA
	ld b,4
	call dbfPGS_LoadVarsToHL
	xor a
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld de,sc1
	jp copysourcestring_plain_zt

dbfPGS_GUIRSprite:
	push af
		ld a,(var4)
		ld e,a
		ld d,0
		ld hl,3
		add hl,de
		pop af
	ex de,hl
	bcall(_PushGUIStack)
	ld b,3
	call dbfPGS_LoadVarsToHL
	push hl
		ld hl,sc1
		call getsourcestring
		pop de
	ex de,hl
	ld a,(var4)
	ld c,a
	ld b,0
	jp HexStringToBin			;de->hl for bc target bytes
	
dbfPGS_GUIRLargeSprite:
	push af
		ld a,(var4)
		ld e,a
		ld d,0
		ld a,(var5)
		bcall(_MultADE)
		pop af
	push hl
		ld de,4
		add hl,de
		ex de,hl
		bcall(_PushGUIStack)
		ld b,4
		call dbfPGS_LoadVarsToHL
		push hl
			ld hl,sc1
			call getsourcestring
			pop de
		ex de,hl
		pop bc
	jp HexStringToBin			;de->hl for bc target bytes

dbfPGS_GUIRPassIn:
	ld hl,sc1
	ld de,7+1							;one of these is the zero term
	call dbfPGS_GetStrlenToDE_SaveA
	ld b,3
	call dbfPGS_LoadVarsToHL
	ld de,(var5)
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	xor a
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld de,sc1
	jp copysourcestring_plain_zt

dbfPGS_GUIRScrollVert:
dbfPGS_GUIRScrollHoriz:
	ld de,16
	bcall(_PushGUIStack)
	ld b,4
	call dbfPGS_LoadVarsToHL
	ld de,var6
	ld bc,8
	ex de,hl
	ldir
	ex de,hl
	call dbfPGS_InsGMRPtr
	jp dbfPGS_InsGMRPtr
	
dbfPGS_GUIRBorder:
dbfPGS_GUIRRect:
	ld de,5
	bcall(_PushGUIStack)
	ld b,5
	jp dbfPGS_LoadVarsToHL

dbfPGS_GUIRMouseCursor:
	ld de,2+2+8+8
	bcall(_PushGUIStack)
	ld de,var2
	ld bc,4
	ex de,hl
	ldir
	push de
		ld hl,sc1
		call getsourcestring
		pop de
	ex de,hl
	ld bc,8
	push bc
		call HexStringToBin			;de->hl for bc target bytes
		push hl
			ld hl,sc2
			call getsourcestring
			pop de
		ex de,hl
		pop bc
	jp HexStringToBin			;de->hl for bc target bytes

;================================

dbfPGS_LoadVarsToHL:		;13-byte routine
	push de
		ld de,var2
dbfPGS_LoadVarsLoop:
		ld a,(de)
		ld (hl),a
		inc de \ inc de
		inc hl
		djnz dbfPGS_LoadVarsLoop
		pop de
	ret
dbfPGS_InsGMRPtr:
	ld de,dbfGUIMouse_Return
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	ret
dbfPGS_GetStrlenToDE_SaveA:
	push de
		push af
			call getsourcestring_plain
;			ld a,d
;			or e
			pop bc
		pop hl
;	jr z,dbfPGS_GetStrlenToDE_SaveA_Ret
	ld a,b
	add hl,de
	ex de,hl
	bcall(_PushGUIStack)
	ret
;dbfPGS_GetStrlenToDE_SaveA_Ret:
;	pop hl				;the call from the function
;	ret

;================================
dbfPopGUIStack:
	ld a,(var1)
	or a
	jr z,dbfPopGUIStackFinish
	ld b,a
	bcall(_PopGUIStacks)
dbfPopGUIStackFinish:
	jp xLIBEndNoOut
;================================
dbfOpenGUIStack:
	bcall(_OpenGUIStack)
	jp xLIBEndNoOut
;================================
dbfCloseGUIStack:
	bcall(_CloseGUIStack)
	jp xLIBEndNoOut
;================================
dbfRenderGUIStack:
	ld a,(nargs)
	sub 2
	ld b,a
	ld a,(var1)
	or b
	push af
		bcall(_RenderGUISub)
		pop af
	call nz,immFastCopy
	jp xLIBEndNoOut
;================================
dbfGUIMouse:
	call dbfMouseCoordSet
	ld hl,(stack)
	push hl
		ld hl,(var3)
		push hl
			ld hl,sum12MouseHookHotspot
			ld de,6
			ld a,$0E
			bcall(_PushGUIStack)
			ld hl,sum12MouseHook
			bcall(_GUIMouse)

dbfGUIMouse_ReturnP2:
			bcall(_PopGUIStack)
			pop hl
		ld (var3),hl
		pop hl
	ld (stack),hl
	xor a
	ld (ScratchVar),a
	push ix								;save trigger item for the end of this routine
	
	;create output string for data
	;first pass: size
	bcall(_GUIFindFirst)
	ld bc,0
dbfGUIMouse_StringSizeLoop:
	bcall(_mos_cphlde)
	jp z,dbfGUIMouse_StringSave
	push hl
		push hl
			pop ix
		push de
			push bc
				inc hl
				inc hl
				ld a,(hl)
				sub 9
				jr c,dbfGUIMouse_StringSizeContinue
				cp 18+1-9
				jr nc,dbfGUIMouse_StringSizeContinue
				ld l,a
				ld h,0
				add hl,hl
				ld de,dbfGUIMouseRet_StringDataVecT
				add hl,de
				ld a,(hl)
				inc hl
				ld h,(hl)
				ld l,a
				or h
				jr z,dbfGUIMouse_StringSizeContinue
				ld a,(ScratchVar)
				or a
				jr z,dbfGUIMouse_StringSize_NoDelimitYet
				pop bc
			ld a,$80			;tCrossIcon
			ld (bc),a
			inc bc
			push bc
dbfGUIMouse_StringSize_NoDelimitYet:
				ld de,dbfGUIMouse_StringReturn
				push de
					jp (hl)
dbfGUIMouse_StringReturn:			;hl = data loc, de=data size
				ld a,(ScratchVar)
				or a
				jr z,dbfGUIMouse_StringReturn_Count
				ld a,e
				or d
				jr z,dbfGUIMouse_StringSizeContinue
				ex de,hl
				push hl
					pop bc			;size in bc, location in de
				pop hl				;destination location in hl
			ex de,hl			;size in bc, source in hl, destination in de
dbfGUIMouse_StringReturnStoreLoop:
			ld a,(hl)
			cp $D6
			jr nz,dbfGUIMouse_StringReturnStoreLoop1
			inc hl						;the extra byte after the CRLF byte
			dec bc
dbfGUIMouse_StringReturnStoreLoop1:
			push hl
										;the following routine needs to preserve de, bc
				call Get_Strng_Tok		;token in h,l; z set if one-byte tok (in l only)
				ex de,hl
				jr z,dbfGUIMouse_StringReturnStoreLoop_Single
				ld (hl),d
				inc hl
dbfGUIMouse_StringReturnStoreLoop_Single:
				ld (hl),e
				ex de,hl
				inc de
				pop hl
			inc hl
			dec bc
			ld a,b
			or c
			jr nz,dbfGUIMouse_StringReturnStoreLoop
			push de
				jr dbfGUIMouse_StringSizeContinue
dbfGUIMouse_StringReturn_Count:
				ld a,e
				or d
				jr z,dbfGUIMouse_StringSizeContinueIncBC
				ld bc,0
dbfGUIMouse_StringReturnLenLoop:
				ld a,(hl)
				cp $D6
				jr nz,dbfGUIMouse_StringReturnLenLoop1
				inc hl						;the extra byte after the CRLF byte
				dec de
dbfGUIMouse_StringReturnLenLoop1:
				push hl
					inc bc					;the following routine needs to preserve de, bc
					call Get_Strng_Tok		;token in h,l; z set if one-byte tok (in l only)
					jr z,dbfGUIMouse_StringReturnLenLoop_Single
					inc bc
dbfGUIMouse_StringReturnLenLoop_Single:
					pop hl
				inc hl
				dec de
				ld a,d
				or e
				jr nz,dbfGUIMouse_StringReturnLenLoop
				;inc bc						;for &#x253C; (128d = 0x80 token)
				pop hl			;recall old size
			add hl,bc
			push hl
dbfGUIMouse_StringSizeContinueIncBC:
				pop bc
			inc bc
			push bc
dbfGUIMouse_StringSizeContinue:
				pop bc
			pop de
		pop hl
	push bc
		bcall(_GUIFindNext)
		pop bc
	jp dbfGUIMouse_StringSizeLoop

	;second pass: contents
dbfGUIMouse_StringSave:
	ld a,(ScratchVar)
	inc a
	ld (ScratchVar),a
	dec a
	jr nz,dbfGUIMouse_ListOutput
	;size is in bc, string number is in var3
	ld a,(var3)
	or a
	jr nz,dbfGUIMouse_StringSave_Not0
	ld a,10
dbfGUIMouse_StringSave_Not0:
	push bc
		dec a
		ld hl,Op1
		ld (hl),4
		inc hl
		ld (hl),$AA
		inc hl
		ld (hl),a
		inc hl
		ld (hl),0
		rst 10h					;findsym
		bcallnc(_DelVarArc)
		pop hl
	bcall(_EnoughMem)
	jr c,dbfGUIMouse_ListOutput			;Insufficient memory
	ex de,hl
	bcall(_CreateStrng)
	inc de
	inc de
	push de
		bcall(_GUIFindFirst)
		pop bc							;variable storage in bc
	jp dbfGUIMouse_StringSizeLoop
	
dbfGUIMouse_ListOutput:	
	;save common info in list
		ld hl,4
		bcall(_CreateTRList)
		ex de,hl
		inc hl
		inc hl
		ld a,(MseX)
		call AtoRealAtHL
		ld a,(MseY)
		call AtoRealAtHL
		ld a,(lastclick)
		call AtoRealAtHL
		pop bc
	ld a,c
	call AtoRealAtHL
	
	bcall(_Op4ToOp1)
	ret

dbfGUIMouseRet_StringDataVecT:
;NOTE**: STARTS AT ITEM 9, NOT ZERO
;NOTE**: Each of these takes the pointer to the current item @ ix
;NOTE**: Each of these should return data @ hl, length in de
	.dw dbfGUIMouseRet_Sum9
	.dw dbfGUIMouseRet_Sum10
	.dw dbfGUIMouseRet_Sum11
	.dw dbfGUIMouseRet_Sum12
	.dw dbfGUIMouseRet_Sum13
	.dw 0
	.dw dbfGUIMouseRet_Sum15
	.dw 0
	.dw 0
	.dw dbfGUIMouseRet_Sum18
;NOTE**: ENDS AT ITEM 18, NOT 24
;================================
dbfGUIMouseRet_Sum9:
dbfGUIMouseRet_Sum18:
	push ix
		pop hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld bc,9
	add hl,bc
	push hl
		ld hl,-10-1			;NO ZERO TERM!
		add hl,de
		pop de
	ex de,hl				;size in de, location in hl
	ret
dbfGUIMouseRet_Sum10:
dbfGUIMouseRet_Sum11:
	ld a,(ix+6)
	add a,'0'
	ld hl,Op1
	ld (hl),a
	ld de,1
	ret
dbfGUIMouseRet_Sum12:
	ld h,0
	jr dbfGUIMouseRet_Sum13_Finish
dbfGUIMouseRet_Sum13:
	ld h,(ix+6)
dbfGUIMouseRet_Sum13_Finish:
	ld l,(ix+5)
	xor a
	ld (Op1+5),a
	ld de,Op1
	push de
		call convHLtoDec
		pop hl
	ld de,5			;NO ZERO TERM!
	ret
dbfGUIMouseRet_Sum15:
	push ix
		pop hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld bc,8
	add hl,bc
	push hl
		ld hl,-9-1			;NO ZERO TERM!
		add hl,de
		pop de
	ex de,hl				;size in de, location in hl
	ret
;================================
temp1b		.equ	temp1
temp2b		.equ	temp1+1
temp3b		.equ	temp2
temp4b		.equ	temp2+1				;at +4, IconSpace8b (scroll data saferam) begins

dbfMenu:
	ld a,(sargs)
	push af
		bcall(_APGui_gui7ToTop)
		bcall(_OpenGUIStack)
		push bc								;save whether the GUI was open previously
			;add initial items onto GUI stack
			ld hl,sc1
			call getsourcestring			;returns string size in chars in bc
			ld hl,1+1+5+1
			add hl,bc						;string size + 0 term + 5-byte icon + coord-x + coord-y
			ld de,dbfMenuTemp_SMW
			ex de,hl
			ld a,2							;GUIRSmallWin
			bcall(_PushGUIStack)			;returns coordinate in HL
			ld b,2
			ld de,var1
dbfMenu_LoadCoordsLoop:
			ld a,(de)
			ld (hl),a
			inc de \ inc de
			inc hl
			djnz dbfMenu_LoadCoordsLoop
			push hl
				ld hl,sc2
				call getsourcestring
				pop de
			ex de,hl
			ld bc,5
			call HexStringToBin			;de->hl for bc target bytes, returns final HL
			ld de,sc1
			call copysourcestring_plain_zt
			;completed constructing the GUIRSmallWin now
			ld hl,dbfMenuTemp_Scroll
			ld de,dbfMenuTemp_ScrollEnd-dbfMenuTemp_Scroll
			ld a,$13
			bcall(_PushGUIStack)
			ld de,4
			add hl,de					;at the "max" value
			push hl
				ld hl,sc3				;time to find the length
				call getsourcestring
				ld d,0
dbfMenu_ItemCountLoop:
				ld a,b
				or c
				jr z,dbfMenu_ItemCountFinish
				ld a,(hl)
				cp $80					;tCrossIcon
				jr nz,$+5
				ld (hl),0				;divide them with zero terms for simplicity
				inc d
				inc hl
				dec bc
				bcall(_IsA2ByteTok)
				jr nz,dbfMenu_ItemCountLoop
				inc hl
				dec bc
				jr dbfMenu_ItemCountLoop
dbfMenu_ItemCountFinish:
				ld a,d
				pop hl					;item count returned in a
			ld (temp1b),a				;temp1 is the total item count
			push af
				cp 6
				jr c,$+4
				ld a,6
				ld (hl),a
				inc hl
				ld (hl),0
				pop af
			inc hl
			inc hl
			inc hl
			ld (hl),a
			inc hl
			ld (hl),0					;two-byte word value
			ld hl,dbfMenuTemp_WinB
			ld de,7
			ld a,5
			bcall(_PushGUIStack)
			xor a
			ld (temp2b),a				;current scroll value
			ld (temp4b),a				;current item (0-5)
dbfMenu_MainOuter:
			bcall(_GUIFindFirst)
			ld de,10+3
			add hl,de
			ld a,(temp2b)
			ld (hl),a
			ld a,1
			ld (temp3b),a				;y-coord of current item
			ld hl,sc3
			call getsourcestring
			ld a,(temp2b)
dbfMenu_MainFindFirst:
			or a
			jr z,dbfMenu_MainDisp
			push af
				call dbfMenu_GetNextItem	;moves hl from start of this item to start of next
				pop af
			dec a
			jr dbfMenu_MainFindFirst
dbfMenu_MainDisp:
			ld a,(temp2b)
			ld b,a
dbfMenu_MainDispLoop:
			ld a,(temp1b)
			cp b
			jr z,dbfMenu_KeyStart
			ld a,(temp2b)
			add a,6
			cp b
			jr z,dbfMenu_KeyStart
			push bc
				ld (RelocatablePtr2),hl
					push hl
						call dbfMenu_GetNextItem
						pop hl
					push hl
						pop ix
					dec de					;zero term
					push de
						push de
							pop bc
						ld de,0
						call getsourcestring_loop
						inc de				;for the zterm
						ld hl,3
						add hl,de
						ex de,hl
						ld a,4
						bcall(_PushGUIStack)
						ld (hl),1
						inc hl
						ld a,(temp3b)
						ld (hl),a
						add a,6
						ld (temp3b),a
						inc hl
						ld (hl),0
						inc hl
						pop bc							;size to copy
					ld de,(RelocatablePtr2)
				push de
					ex de,hl							;de is where to put it, hl is source
					call copysourcestring_plain_zt_raw
					pop hl
				call dbfMenu_GetNextItem
				pop bc
			inc b
			jr dbfMenu_MainDispLoop
dbfMenu_KeyStart:
			bcall(_RenderGUI)
			ld b,6
			ld a,(temp1b)
			cp b
			jr nc,dbfMenu_KeyStart_Pops
			ld b,a
dbfMenu_KeyStart_Pops:
			bcall(_PopGUIStacks)
dbfMenu_KeyRectDraw:
			call dbfMenu_DrawEraseRect
			call fastcopy
dbfMenu_KeyLoop:
			bcall(_Cn2GetK)				;a is key, e is group
			or a
			jr z,dbfMenu_KeyLoop
			dec a
			jp z,dbfMenu_KeyDown
			cp 4-1
			jp z,dbfMenu_KeyUp
			cp 9h-1
			jr z,dbfMenu_KeyGo
			cp 36h-1
			jr nz,dbfMenu_KeyLoop
dbfMenu_KeyGo:		
			pop bc
		ld a,c
		or a
		jr nz,dbfMenu_DonePops				;if jump, it was already open
		bcall(_CloseGUIStack)
		jr dbfMenuGoto
dbfMenu_DonePops:
		ld b,3
		bcall(_PopGUIStacks)
dbfMenuGoto:
		pop af
	cp 4
	jr nz,dbfMenuGoto_RetAns
	ld hl,sc4
	call getsourcestring
	jr z,dbfMenuGoto_RetAns					;trigger a lbl error
	ld a,(temp2b)
	ld e,a
	ld a,(temp4b)
	add a,e
	ld e,a
	ld d,0
	add hl,de
	add hl,de							;hl is now first of two chars
	ld de,(basic_start)
	ex de,hl
dbfMenuGotoLoop:
	push de
		ld de,(basic_end)
		bcall(_mos_cphlde)
		pop de
	jr z,dbfMenuGotoErr
	ld a,(hl)
	cp $D6								;tLbl
	jr nz,dbfMenuGotoContinue
	inc hl
	ld a,(de)
	cp $29								;space ' '
	jr z,dbfMenuGotoCheckOneChar
	cp (hl)
	jr nz,dbfMenuGotoContinue
	inc hl
	inc de
	ld a,(de)
	cp (hl)
	jr z,dbfMenuGotoFound
	dec hl
	dec de
	jr dbfMenuGotoContinue
dbfMenuGotoCheckOneChar:
	inc de
	ld a,(de)
	dec de
	cp (hl)
	jr nz,dbfMenuGotoContinue
	inc hl
	ld a,(hl)
	dec hl
	cp $3E
	jr z,dbfMenuGotoFound
	cp $3F
	jr z,dbfMenuGotoFound
dbfMenuGotoContinue:
	ld a,(hl)
	inc hl
	bcall(_IsA2ByteTok)
	jr nz,dbfMenuGotoLoop
	inc hl
	jr dbfMenuGotoLoop
	;LOOP TO FIND Lbl
	;CHECK THE Lbl tag
	;JUMP OUT OR CONTINUE
	;IF REACH THE END, TRIGGER ERROR
	;NEED TO CHANGE BASIC_PC HERE!!!
dbfMenuGoto_RetAns:
	ld a,(temp2b)
	ld e,a
	ld a,(temp4b)
	add a,e
	bcall(_SetXXOp1)
	ret
dbfMenuGotoFound:
	inc hl
	ld (basic_pc),hl
	jp xLIBEndNoOut
dbfMenuGotoErr:
	ld a,20+$80
	b_jump(_JError)

dbfMenu_KeyUp:
	call dbfMenu_DrawEraseRect
	ld a,(temp4b)
	or a
	jr nz,dbfMenu_KeyUpSimple
	ld a,(temp2b)
	or a
	jr z,dbfMenu_KeyRectDrawGo
	dec a
	ld (temp2b),a
	jp dbfMenu_MainOuter
dbfMenu_KeyUpSimple:
	dec a
	ld (temp4b),a
	jr dbfMenu_KeyRectDrawGo
dbfMenu_KeyDown:
	call dbfMenu_DrawEraseRect
	ld a,(temp1b)
	ld b,a
	ld a,(temp4b)
	cp 5
	jr z,dbfMenu_KeyDownPage
	dec b
	cp b
	jr z,dbfMenu_KeyRectDrawGo
	inc a
	ld (temp4b),a
dbfMenu_KeyRectDrawGo:
	jp dbfMenu_KeyRectDraw
dbfMenu_KeyDownPage:
	ld a,(temp2b)
	add a,6
	cp b
	jp z,dbfMenu_KeyRectDrawGo
	sub 5				;sub 6 \ inc a
	ld (temp2b),a
	jp dbfMenu_MainOuter
dbfMenu_DrawEraseRect:
	ld a,(temp4b)
	add a,a
	ld b,a
	add a,a
	add a,b					;a=a*6
	inc a					;a=1+a*6
	ld e,a
	ld d,0
	push de
		bcall(_RenderGUIGetWinX)
		pop de
	ld a,(hl)
	inc hl
	ld l,(hl)
	ld h,a
	add hl,de
	ex de,hl
	ld hl,(71*256)+6
	add hl,de
	ex de,hl
	ld a,2
	bcall(_mos_filledrectangle)
	ret
;---------------------------------
; dbfMenu_GetNextItem
; Inputs:
;      hl=first byte of substring
; Outputs:
;      hl=first byte of next substr
;      de=length of substr, incl
;         tCrossIcon delimiter
dbfMenu_GetNextItem:
	ld de,0
dbfMenu_GetNextLoop:
	ld a,(hl)
	inc hl
	inc de
	or a								;zero term
	ret z
	cp $80
	ret z								;tCrossIcon
	bcall(_IsA2ByteTok)
	jr nz,$+4
	inc hl
	inc de
	jr dbfMenu_GetNextLoop
;================================
dbfPushAns:
	bcall(_RclAns)
	ld hl,Op1
	ld a,(hl)
	and $1f
	push af
		or a
		jr z,dbfPushAnsReal
		dec a
		jr z,dbfPushAnsRList
		dec a
		jr z,dbfPushAnsRMat
		sub 2
		jr z,dbfPushAnsString
		sub 8
		jr z,dbfPushAnsComplex
		dec a
		jp z,dbfPushAnsCList
		pop af
	jp xLibEndNoOut
dbfPushAnsReal:
;		ld hl,Op1						;not necessary
		ld de,9
		jr dbfPushAnsContinue
dbfPushAnsCList:
		ex de,hl
		push hl
			ld h,b \ ld l,c
			add hl,hl				;*2, so in the end it will be *18 + 2
			ld b,h \ ld c,l
			jr dbfPushAnsAllLists
dbfPushAnsRMat:
		ex de,hl
		push hl
			ld e,(hl)
			inc hl
			ld h,(hl)
;			ld h,b \ ld e,c
			bcall(_mos_MultHE)		;result in hl
			ld b,h \ ld c,l			;hl->bc
			jr dbfPushAnsAllLists	;and Mats!
dbfPushAnsRList:
		ex de,hl
		push hl
			ld h,b \ ld l,c			;element count was in bc
dbfPushAnsAllLists:					;REAL	;CPLX
			add hl,hl				;*2		;*4
			add hl,hl				;*4		;*8
			add hl,hl				;*8		;*16
			add hl,bc				;*9		;*18
			inc hl
			inc hl					;*9+2	;*18+2		(because of element number count, 1 word)
			pop de
		ex de,hl					;hl=where, de=size
		jr dbfPushAnsContinue
dbfPushAnsString:
		ex de,hl
		ld e,(hl)
		inc hl
		ld d,(hl)
		inc hl
		jr dbfPushAnsContinue
dbfPushAnsComplex:
;		ld hl,Op1						;not necessary
		ld de,18
dbfPushAnsContinue:
		push hl
			push de
				inc de
				inc de
				inc de
				push de
					bcall(_Op1ToOp3)
					call dbfPushPopAnsStack_AnsNameOrDefault
					bcall(_Chkfindsym)
					jr nc,dbfPushAnsExists
					bcall(_Op2ToOp5)
					pop hl
				bcall(_CreateAppVar)
				push de
					bcall(_Op5ToOp2)
					pop de
				inc de
				inc de
				jr dbfPushAnsContinue2
dbfPushAnsExists:
					pop hl
				push hl
					push de
						ex de,hl
						ld a,(hl)
						inc hl
						ld h,(hl)
						ld l,a
						add hl,de
						pop de
					ld a,l
					ld (de),a
					inc de
					ld a,h
					ld (de),a
					inc de
					pop hl
				bcall(_InsertMem)
dbfPushAnsContinue2:
				push de
					bcall(_Op3ToOp1)
					pop de
				pop bc
			pop hl
		pop af
	ld (de),a
	inc de
	ld a,c
	ld (de),a
	inc de
	ld a,b
	ld (de),a
	inc de
	ld a,b
	or c
	jr z,dbfPushAnsFinishNoLDIR		;added for the zero-length string case
	ldir
dbfPushAnsFinishNoLDIR:
	jp xLibEndNoOut
;================================
dbfPopAns:
	call dbfPushPopAnsStack_AnsNameOrDefault
	bcall(_chkfindsym)
	jp c,xLibEndNoOut					;don't do anything if the appvar doesn't exist
	ex de,hl
	push hl
		inc hl
		inc hl
		ld a,(hl)
		inc hl
		ld e,(hl)
		inc hl
		ld d,(hl)
		push de
			ld bc,dbfPopAnsFinish
			push bc
				inc hl							;item starting at hl, size in de, type in a
				and $1f
				or a
				jr z,dbfPopAnsReal
				dec a
				jr z,dbfPopAnsRList
				dec a
				jr z,dbfPopAnsRMat
				sub 2
				jr z,dbfPopAnsString
				sub 8
				jr z,dbfPopAnsComplex
				dec a
				jr z,dbfPopAnsCList
				pop bc
dbfPopAnsFinish:
			pop de
		pop hl
	;need to remove de+3 bytes at hl
;	push hl
;		dec hl
		ld c,(hl)
		inc hl
;		dec hl
		ld b,(hl)
		inc hl
;		pop hl
	ex de,hl		;de=remove from here, hl=size of entry-3, bc=size of appvar
	inc hl
	inc hl
	inc hl
	push hl
		or a
		sbc hl,bc
		pop hl
	jr nz,dbfPopAnsFinishRemove
	call dbfPushPopAnsStack_AnsNameOrDefault
	bcall(_ChkFindSym)
	bcall(_DelVar)
	jr dbfPopAnsFinishEnd
dbfPopAnsFinishRemove:
	ex de,hl							;hl=remove from here, de=size
	push bc
		push hl
			push de
				bcall(_DelMem)
				pop bc
			pop de
		pop hl
	or a
	sbc hl,bc
	ex de,hl
	dec hl
	ld (hl),d
	dec hl
	ld (hl),e
dbfPopAnsFinishEnd:
	bcall(_Op4ToOp1)					;goes into Ans
	ret
;================================
dbfPopAnsReal:
	ld bc,9
	jr dbfPopAnsComplexGo
dbfPopAnsComplex:
	ld bc,18
dbfPopAnsComplexGo:
	ld de,Op1
	ldir
	bcall(_Op1ToOp4)					;in case ChkFindSym \ DelVar runs
	ret
dbfPopAnsRMat:
	push de
		ld e,(hl)
		inc hl
		ld d,(hl)
		inc hl
		push hl
			ex de,hl					;h=rows, l=columns
			bcall(_CreateTempRMat)
			jr dbfPopAnsListFinish
dbfPopAnsCList:
	push de
		ld e,(hl)
		inc hl
		ld d,(hl)
		inc hl
		push hl
			ex de,hl
			bcall(_CreateTempCList)
			jr dbfPopAnsListFinish
dbfPopAnsRList:
	push de
		ld e,(hl)
		inc hl
		ld d,(hl)
		inc hl
		push hl
			ex de,hl
			bcall(_CreateTempRList)
dbfPopAnsListFinish:
			pop hl
		inc de
		inc de
		pop bc
	dec bc
	dec bc
	ldir
	ret									;name is already in Op4
dbfPopAnsString:
	push hl
		push de
			ex de,hl
			bcall(_CreateTempString)
			inc de
			inc de
			pop bc
		pop hl
	ld a,b
	or c
	ret z								;account for the zero-length case
	ldir
	ret									;name is already in Op4
;================================
dbfPushPopAnsStack_AnsNameOrDefault:
	ld a,(nargs)
	dec a
	jr z,dbfPushPopAnsStack_AnsDefault
	ld a,(var1)
	jp dbfAnsStackNameToOp1
dbfPushPopAnsStack_AnsDefault:
	xor a
	jp dbfAnsStackNameToOp1
;================================
dbfClearAnsStack:
	ld hl,xLibEndNoOut
	push hl
		ld a,(nargs)
dbfClearAnsStack_OffPage:
		dec a
		jr z,dbfClearAnsStackAll
		ld a,(var1)
		call dbfAnsStackNameToOp1
		bcall(_chkfindsym)
		ret c
		bcall(_delvararc)
		ret
dbfClearAnsStackAll:
		xor a
dbfClearAnsStackLoop:
		push af
			call dbfAnsStackNameToOp1
			bcall(_chkfindsym)
			bcallnc(_delvararc)
			pop af
			inc a
		cp 10d					;0-9 cleared
		ret z
		jr dbfClearAnsStackLoop
dbfAnsStackNameToOp1:
		push af
			ld hl,AnsAppVar
			rst 20h
			pop af
		add a,'0'
		ld (Op1+4),a				;'15h' 'A' 'n' 's' 'N' '\0'
		ret
;================================
AtoRealAtHL:
	push de
		push bc
			push hl
				bcall(_setxxop1)
				pop de
			ld hl,Op1
			ld bc,9
			ldir
			pop bc
		pop hl
	ex de,hl
	ret
;================================
Get_Strng_Tok:
	push bc
		ld h,0
		ld l,a
		cp $30							;'0'
		jr c,Get_Strng_Tok_NotNum
		cp $39+1
		jr c,Get_Strng_Tok_Fin			;'0' to '9'
Get_Strng_Tok_NotNum:
		cp $41							;'A'
		jr c,Get_Strng_Tok_NotLetUpr
		cp $5A+1
		jr c,Get_Strng_Tok_Fin			;'A' to 'Z'
Get_Strng_Tok_NotLetUpr:
		cp $61
		jr c,Get_Strng_Tok_NotLetLwr
		cp $7A+1
		jr nc,Get_Strng_Tok_NotLetLwr
		ld h,$BB
		add a,$B0-$61
		ld l,a
		cp $BB
		jr c,Get_Strng_Tok_Fin
		inc l
		jr Get_Strng_Tok_Fin
Get_Strng_Tok_NotLetLwr:
		ld b,(Get_Strng_Tok_TblTwoByteEnd-Get_Strng_Tok_TblTwoByte)/2
		ld hl,Get_Strng_Tok_TblTwoByte
Get_Strng_Tok_TwoByteLoop:
		ld c,(hl)
		inc hl
		cp c
		jr z,Get_Strng_Tok_TwoByteFinish
		inc hl
		djnz Get_Strng_Tok_TwoByteLoop
		ld b,(Get_Strng_Tok_TblOneByteEnd-Get_Strng_Tok_TblOneByte)/2
		ld hl,Get_Strng_Tok_TblOneByte
Get_Strng_Tok_OneByteLoop:
		ld c,(hl)
		inc hl
		cp c
		jr z,Get_Strng_Tok_OneByteFinish
		inc hl
		djnz Get_Strng_Tok_OneByteLoop
		ld hl,$000E
Get_Strng_Tok_Fin:
		ld a,h
		or a
		pop bc
	ret
Get_Strng_Tok_TwoByteFinish:
		ld l,(hl)
		ld h,$BB
		jr Get_Strng_Tok_Fin
Get_Strng_Tok_OneByteFinish:
		ld l,(hl)
		ld h,0
		jr Get_Strng_Tok_Fin

Get_Strng_Tok_TblTwoByte:
	.db $05,$EC
	.db $09,$F1
	.db $23,$D2
	.db $24,$D3
	.db $25,$DA
	.db $26,$D4
	.db $27,$D0
	.db $B6,$9A
	.db $B7,$9B
Get_Strng_Tok_TblTwoByteEnd:

Get_Strng_Tok_TblOneByte:
	.db $1A,$B0
	.db $20,$29
	.db $21,$2D
	.db $22,$2A
	.db $28,$10
	.db $29,$11
	.db $2A,$82
	.db $2B,$70
	.db $2C,$2B
	.db $2E,$3A
	.db $2F,$83
	.db $3a,$3E
	.db $3F,$D6
	.db $5D,$07
	.db $5E,$F0
	.db $5F,$D9
	.db $C1,$06
	.db $D6,$81
Get_Strng_Tok_TblOneByteEnd:

;==============================================

#define  Cn2_Setup	        $4209
#define  Cn2_Clear_SendBuf  $420C
#define  Cn2_Clear_RecBuf   $420F
#define Cn2_Setdown	        $4212
#define Cn2_GetK		    $4221
#define SWord               $87FF
#define Rword               $86F8
#define Sdat                $8801
#define Rdat                $86FA
#define rvarlength          $9872+176	; put at the end of appbackupscreen right after iambians variable only because idk what he is doing there so best safe then sorry
dbfCn2BASICSend:			;INCOMPLETE
	;Check arguments
	; order is
	; ID
	; Data
	; ID is any real variable preferably theta for simplicities sake
	; Data can be a string of a list. gonna have to parse the vat for just the type and put a 1/0 in for type
	; after thats parsed its a simple copy of the real to the ip part
	; the size is copied from the data pointer
	; for lists its (multx9)+1
	; for strings its length+1

	;   Order of stuff
	; Check for pending frame if one quit early (send status code?)
	; Set up buffers
	; set up sizes
	; mark pending
	; set ans to 1 if frame was set to 0 if not
	; quit
	; jp $
	ld a,(sword+1) ; load a with the frame pending flag byte
	bit 7,a             ; if the msb is set we have a frame 
	jp nz,QUITBAD   
	; Assumign theta for The real var removing from wiki now 2/7/13
	ld hl,IDRealName
	rst 20h
	bcall(_chkfindsym)  
	jp c,QUITBAD        ; make sure theta exists
	xor a
	cp b 
	jp nz,QUITBAD     ; if file is in atchive quit cause im to lazy to unarchive it. Who archives realvars anyway? delvar that shit
	inc de \ inc de ; Load to the actual data we want not the realvar exponent/flagbytes
	ex de,hl         ; move data pointer to hl for Ldir
	ld de,$87fa   
	ld bc,5
	ldir            ; copy bytes into Id

	ld hl,sc1       ; load Op1 value of passed data
	rst 20h
	bcall(_chkfindsym)
	jp c,QUITBAD
	ld c,a
	xor a
	cp b
	jp nz,QUITBAD
	ex de,hl
	ld a,c
	and 1fh        ; A contains the element type in the lower 5 bits mask out the upper bits
	ld de,sdat    ; overwrite vat pointer (lol who uses those) with sdat
	ld (de),a     ; load first byte of frame with 1
	cp 1           ; if its a list
	jr z,SList     ; ...
	cp 4           ; if its a string
	jp nz,QuitBad   ; ... 
;	jr SString     ; if none of the above skedadle
 
SString:       ;Set (frame) to 0 ; Also get size from list and +3 to it for frame length
	inc de
	ld c,(hl)
	inc hl
	ld b,(hl)    ; load hl with the size of the string.
	dec hl       ; return hl to start of the string
	inc bc
	inc bc
	inc bc      ; increase it to match size+info byte length
	ld (sword),bc ; save to length proper
	dec bc  ; restore size after we increase it to match size+ length bytes
	ldir
	xor a
	bcall(_setxxop1)
	ld a,(sword+1)
	or 80h
	ld (sword+1),a
	ret
  
SList:        ; Set (frame) to 1
	inc de
	ld c,(hl)  ; dont need the whole thing as this can not be over 28               
	ld a,c    ; and while yes this can leave a bug if there is over 100 elements in the list not my problem
	cp 29      
	jr nc,QUITBAD   ; quit if over 28 
	add a,a
	add a,a 
	add a,a
	add a,c   ; multiply by nine
	inc a     
	inc a     ; increase a by 3 for when we 
	inc a     ; to accomodate for info byte and size bytes of variable
	ld (sword),a
	dec a     ; return to var size
	ld b,0    ; make bc 00xx where xx will be C
	ld c,a    ; save to C so we have 00xx proper
	; hl = var data
	; bc = length of var.
	; de = cn2 buffer
	ldir       ;
	xor a
	bcall(_setxxop1) 
	ld a,(sword+1)
	or 80h
	ld (sword+1),a
	ret
	
dbfCn2BASICGet:				;INCOMPLETE
	; Check to see if there is a frame.
	; if there is parse the frame and write out 
	; id gets copied into the String 
	; List gets copied into the list


	; we are sending raw variable data in the frames 
	; So if it is a list add it to a list if it is a string add it to a string
	; MAKE SURE WE HAVE ENOUGH Ram for this 

	;Order of routine.
	;   Check for pending frame  [x]
	;   MTROC ID string8    
	;   Copy ID over
	;   Check Data Type
	;   MTROC Appropriate variable  (LCn2,Str9)
	;   Copy Data over
	;   Clear Frame Pending
	;   Ret
	; set ans to 0 if frame was recieved

	ld a,(rword+1) ; load a with the incoming flag byte
	bit 7,a             ; if the msb is set we have a frame 
	jp z,QUITBAD               ; if its not quit
	ld hl,IDrealName
	ld a,9                  ;delete and create theta. this shouldent matter
	ld (RVarLength),a
	call DeleteAndCreate
	; here i handle the real this should be rather easy
	; we are interested in just the first 5 values in the mantessa
	; the mantessa is de+2
	; so lets initialise our variable
	ld a,$00     ;standard exponent and no negative? i think we will see 
	ld (de),a
	inc de 
	ld a,$80
	ld (de),a
	inc de
	;Now the next 5 bytes are our id.
	ld bc,5
	ld hl,$86F3   ; recieve id
	ldir          

	ld a,(rdat)          ; load a with frame id
	cp 1 ; ListObj
	jp z,ldlist
	cp StrngObj    ;if StrngOBj we have a string if one we have a list. 
	jp nz,quitbad          ; if list div length by nine
;	jp ldStrng


ldStrng:
	ld a,(Rword)
	dec a                 ; if string calculate length
	ld (RVarLength),a
	ld hl,DataStringName
	jr Copydata
	; kinda just shoved this here. Makes following it a pain but do I really care?
	; nothing traverses here it all jumps over it. Woo dead space 
QUITBAD:
	;returns 1 if we failed. reusable routine.
	ld a,1
	bcall(_setxxop1)
	ret  
 
ldlist:
	ld a,(rdat+1)  ; Assuming the list is <28 elements.
	;dec a             ;Changing to bcall createlist it takes elements. not length
	;This may just need to be
	;ld a,(rdat+1)
	ld (RVarLEngth),a
	ld hl,DataListName
   
CopyData:
	Call DeleteAndCreate
	;get data length -1 -> bc
	ld a,(rword)      ; get length from frame and store into bc
	dec a
	ld c,a
	ld b,0
	ld hl,rdat+1    ; load hl with pointer to frame+1 for variable data
	;jp $
	ldir ;theta now has the user id in it

	ld hl,RWord+1
	res 7,(hl)
	; acknowledge we got a frame and quit
	xor a
	bcall(_setxxop1)
	ret
	;according to iambian and some of kerms code this is supposed to return the value of op1 in ans ¯\(°_o)/¯
	; jp XlibEndNoOut
DeleteAndCreate
	;expects pointer to variable name in hl
	;destroys all
	;returns pointer to variable data in de
	;jp $
	push hl
		;bcall(_zeroop1)
		rst 20h  ;load op1 with variable name
		rst 10h
		; call(_findsym)      ;GO FUCK A ROCK YOU PIECE OF SHIT WORK RIGHT DAMNIT
		;  push af
		jr c,Create
		bcall(_DelVarArc)   ;delete if since it exists
Create: 
		;pop af   
		pop hl   ; restore hl
	;push af ; save A again...
	rst 20h ;fix op1
	;pop af   ; restore A which has the element type in it.
	ld a,(op1)
	;and 1fh
	cp 1
	ld hl,(RVarLength)
	jr z,CreateRList
	jr c,CreateRVar
CreateString:   ;4
	bcall(4327h)  ;Create string
	ret
CreateRlist:
	bcall(_CreateRList)
	ret
CreateRVar:
	bcall(_CreateReal)
	ret

DataStringName:
	.db strngOBJ,tVarStrng,tstr9,$0
IDRealName:
	.db $0,tTheta,$0,$0
DataListName:
	.db $01,tVarLst,tC,tN,t2,$0
 
dbfCn2BASICStop:                                ;Main Functions Complete
	ld a,(nargs)
	cp 2
	jp nz,xLIBEndNoOut
	ld A,(var1)
	or a
	jr z,disablecn2    ; If enabled, skip over enabling
 
EnableCn2:
	;Disable APD; Set up Cn2.2 hook; set up TI-OS intterupt.

	bcall(4C84h)            ;Disable APD  ( DO we even use ti83+.inc???)
	;jp $
	di
	bcall(_Cn3_Setup)
	ld hl,Cn2BasicOnhook ;calls a routine in the calcnet22.asm file does what i need it to 
	ld (Cn2_Hook_Pre),HL   ; Hopefully chain The tios intterupt before the calcnet intterupt.
						  ; Personally wondering if this should be at the end of the calcnet intterupt.
												  ; only because of how the tios intterupt is unpredicable in length.
												  ; Do we really have to worry about that?? According to kerm I do not

	set 3,(iy+33h)           ; ignoreBPlink          
	res 4,(iy+33h)           ; BPLinkOn      Disable link Assist for sure
	res 0,(iy+3Eh)           ;                Disables Silent linking supposedly  
	ld a,%10000000 
	out (08h),a               ;Disable link assist via ports want as much cushion as possible here.
	ei
	jp xLIBEndNoOut

 
DisableCn2: 
	bcall(_Cn3_Setdown)
	di
	res 3,(iy+33h)           ; ignoreBPlink          
	set 4,(iy+33h)           ; BPLinkOn      Enable Silent linking and link assist
	set 0,(iy+3Eh)  
	ld a,0
	out (08h),a               ; Reenable link assist disable intterupts
	bcall($4C87)              ; Enable Apd again
	ei    
	jp xLibEndNoOut

dbfCn2BasicStatus:
	ld a,(nargs)
	cp 2
	jp nz,xlibEndNoOut
FlagRet:
	ld a,(var1)
	or a
	jr z,SendFlagCheck
	ld a,(rword+1) 
	jr FlagCheckFinish
SendFlagCheck:
	ld a,(sword+1)
FlagCheckFinish:
	and %10000000
	rlc a
	bcall(_setxxop1)
	ret  ;hurr duurrr 
