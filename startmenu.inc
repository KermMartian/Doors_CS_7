;-----------------------------------------------------------
;	Filename:		StartMenu.inc
;	Long name:  	DCS Menu Rendering and Functions
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	June 22, 2010
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------
.nolist
_GetBytePaged	.equ	4F54h
_OffPageJump	.equ	44F2h
.list

OpenStartMenu:
	ld ix,StartMenuSprite_1
	ld b,9
	ld c,2
	ld a,2
	ld l,55
	call imLargeSprite
	ld ix,StartMenuSprite2_1
	ld b,9
	ld c,2
	ld a,2
	ld l,55
	call imLargeSprite
	call imFastCopy	
OpenStartMenuShadow:					;back up the background for restoration
	ld hl,gbuf+((14+8)*12)
	ld b,41-8							;removed link and power options = 41-8-8 = 25 pixels high
	ld de,StartMenuShadow
OpenStartMenuShadowOuter:
#ifdef lang_english
		ld c,5
#endif
#ifndef lang_english
		ld c,6
#endif
OpenStartMenuShadowInner:
	ld a,(hl)
	ld (de),a
	inc hl
	call ShadowIncDE
	dec c
	jr nz,OpenStartMenuShadowInner
	push de
#ifdef lang_english
		ld de,12-5
#endif
#ifndef lang_english
		ld de,12-6
#endif
		add hl,de
		pop de
	djnz OpenStartMenuShadowOuter
	ld b,1
OpenStartMenuLoop:
	push bc
	ld hl,gbuf+(54*12)
#ifdef lang_english
	ld de,-12-5
#endif
#ifndef lang_english
	ld de,-12-6
#endif
OpenSMLClear:
#ifdef lang_english
	ld c,5
#endif
#ifndef lang_english
	ld c,6
#endif
OpenSMLCInner:
	ld (hl),0
	inc hl
	dec c
	jr nz,OpenSMLCInner
	add hl,de
	djnz OpenSMLClear
	pop bc
	push bc
#ifdef lang_english
	ld c,5
#endif
#ifndef lang_english
	ld c,6
#endif
	ld a,55
	sub b
	ld l,a
	xor a
	ld ix,StartMenuImg
	call imLargeSprite
	call imFastCopy
	pop bc
	inc b
	ld a,b
	cp 41-8
	jr nz,OpenStartMenuLoop
	
	call OpenGUIStack
	xor a
	ld de,1
	call PushGUIStack
	ld a,3
	ld de,768
	ld hl,gbuf
	call PushGUIStack
	ld hl,SMHS1
	call PushGUIStacks

#ifdef false
	ld a,$0E
	ld hl,SMHS5
	ld de,6
	call PushGUIStack
#endif

#ifdef enablecn2eis
	ei
#endif
SMMainMouseCall:
	ld hl,DCSMenuHook
	jp GUIMouseCall

#ifdef false
SMPower:
	bcall(_Off)
	ld hl,DCSMenuHook
	jp GUIMouseCall
#endif

SMHelp:
	ld hl,SMMainMouseCall
	push hl
SMHelpOffpage:
	ld hl,SMHelp_Window1
SMHelpP2:
	call PushGUIStacks
	ld hl,0
	jp GUIMouseCall

SMHelpCloseP1:
	ld b,4
	call PopGUIStacks
	ld hl,SMHelp_Window2
	jr SMHelpP2

SMHelpCloseP2:
	ld b,4
	call PopGUIStacks
	ret

SMOptions:
	ld hl,SmallWinS2
	call PushGUIStacks
	
	call GUIFindFirst
	call GUIFindNext
	;Set Acceleration Level
	push de
		push hl
		ld hl,37
			bcall(_DAVLCheckOffset)
			ld a,(hl)
			ld b,1
			cp $ff
			jr nz,SMOptions_AccelEnabled
			dec b
			ld a,180						;180/20 = 9, the default
SMOptions_AccelEnabled:
			pop hl
		push hl
			ld de,6
			add hl,de
			ld (hl),b
			pop hl
		pop de
	push af
		call GUIFindNext
		pop af
	push de
		push hl
			ld l,a
			ld h,0
			ld a,20
			bcall(_divhlbya)
			ld a,l
			pop hl
		push hl
			ld de,5
			add hl,de
			ld (hl),a
			pop hl
		pop de
	call GUIFindNext				;mouse start
	;set mouse start
	push de
		push hl
			ld hl,66
			bcall(_DAVLCheckOffset)
			pop de
		push de
			ex de,hl
			ld bc,5
			add hl,bc
			ld a,(de)
			push hl
				ld l,a
				ld h,0
				ld a,5
				bcall(_divhlbya)
				ld a,l
				pop hl
			ld (hl),a
			pop hl
		pop de
	ld bc,35								;set cn2 force
	call SMOptions_NextCheckFromAV
	ld bc,36								;run on start force checkbox
	call SMOptions_NextCheckFromAV
	ld bc,33								;lowercase checkbox
	call SMOptions_NextCheckFromAV
	ld bc,34								;background checkbox
	call SMOptions_NextCheckFromAV
	ld bc,AVOff_OnPrgmHook					;on-prgm hook
	call SMOptions_NextCheckFromAV
	ld bc,AVOff_ParserHook					;parser hook
	call SMOptions_NextCheckFromAV

	;call RenderGUI
SMOptionsMouse:
	ld hl,$0000
	jp GUIMouseCall

SMDisplay:
	ld hl,SmallWinS4
	call PushGUIStacks
	in a,(2)
	and %10100000
	jr z,SMDisplayNotSE
	ld hl,SMDisplayTuneButton
	ld de,SMDisplayTuneButtonE-SMDisplayTuneButton
	ld a,$07
	call PushGUIStack
SMDisplayNotSE:
	call GUIFindFirst
	call GUIFindNext
	call GUIFindNext
	push de
		push hl
			ld de,13
			add hl,de
			ld a,($8447)
			bit 0,a
			jr nz,SMDisplayOdd
			dec a
SMDisplayOdd:
			ld (hl),a
			pop hl
		pop de
	call GUIFindNext
	ld de,6
	add hl,de
	push hl
		ld hl,68
		bcall(_Davlcheckoffset)
		pop de
	ld a,(hl)
	ld (de),a
SMDisplayMouse:
	;call RenderGUI
	ld hl,$0000
	jp GUIMouseCall

SMDisplayTune:
	xor a
	out ($20),a
	ld a,$2E
	out ($10),a
	ld c, $0C-$04
SMDisplaydelayTest:
	ld d,100
	ld b,255
	ld a,4
	add a,c
	ld c,a
	out ($29), a
SMDisplaytestLoop:
	inc	hl
	dec	hl
	inc	hl
	dec	hl
	nop
	ld a,(hl) 							;59 tstates to here
	in a,($10)							;59 tstates total
	or a
	jp m,SMDisplaydelayTest
;	ld a,$01
;	out	($10),a
	out ($11),a
	djnz SMDisplaytestLoop
	ld b,255
	dec	d
	jr nz,SMDisplaytestLoop				;25500 trials
	ld a,c
	jr SMDisplayMouse

#ifdef false
SMPowerShutdownGo:
	bcall(_CloseGUIStack)
	bcall(_Shutdown)
SMPowerRestartGo:
	bcall(_CloseGUIStack)
	bcall(_Restart)
SMPowerQuitGo:
	bcall(_CloseGUIStack)
	bcall(_QuitToOS)
#endif

SMOptionParse:
	call GUIFindFirst 				;window buttons
	call GUIFindNext				;acceleration yes/no
	;set yes/no
	push hl
		ld bc,6
		add hl,bc
		ld a,(hl)
		ld b,a
		pop hl
	push bc
		call GUIFindNext				;acceleration meter
		pop bc
	;Set Acceleration Level
	push de
		push bc
			push hl
				ld hl,37
				bcall(_DAVLCheckOffset)
				pop de
			push de
				ex de,hl
				ld bc,5
				add hl,bc
				ld a,(hl)
				add a,a
				ld b,a
				add a,a
				add a,a
				add a,b
				add a,a							;a = a*20
				pop hl
			ld c,a
			pop af
		or a
		jr nz,SMOptionParse_AccelEnabled
		ld c,$ff
SMOptionParse_AccelEnabled:
		ld a,c
		ld (de),a
		pop de
	call GUIFindNext				;mouse start
	;set mouse start
	push de
		push hl
			ld hl,66
			bcall(_DAVLCheckOffset)
			pop de
		push de
			ex de,hl
			ld bc,5
			add hl,bc
			ld a,(hl)
			ld b,a
			add a,a
			add a,a
			add a,b							;a = a*5
			ld (de),a
			pop hl
		pop de
	ld bc,AVOff_FldSave
	call SMOptions_NextAVFromCheck
	push af
		ld bc,AVOff_Offscript
		call SMOptions_NextAVFromCheck
		ld bc,AVOff_Lowercase
		call SMOptions_NextAVFromCheck
		ld bc,AVOff_XLC3Libs
		call SMOptions_NextAVFromCheck
		ld bc,AVOff_OnPrgmHook
		call SMOptions_NextAVFromCheck
		ld bc,AVOff_ParserHook
		call SMOptions_NextAVFromCheck
		call BackupAppVar
SMOptionsClose:
		ld b,16							;was 14, +parser hook checkbox ;was 15, +Backup Now button
		call PopGUIStacks
		;call RenderGUI
		pop af
	ld hl,DCSMenuHook
	or a
	jp z,GUIMouseCall
	call CloseGUIStack
	bcall(_RealStartNoResetRetFrom2)
SMDisplayParse:
	call GUIFindFirst
	call GUIFindNext
	call GUIFindNext				;now at contrast bar
	push hl
		push de
			ld de,13
			add hl,de
			ld a,(hl)
			ld ($8447),a
			add a,23d
			or $c0
			bcall(_lcd_busy)
			out (LCDINSTPORT),a
			;set contrast to variables, including out to port
			pop de
		pop hl
	call GUIFindNext
	ld de,6
	add hl,de
	ld a,(hl)
	push af
		ld hl,68
		bcall(_DAVLCheckOffset)
		pop af
	ld (hl),a
SMDisplayClose:
	ld b,7
	in a,(2)
	and %10100000
	jr z,SMDisplayCloseNotSE
	inc b							;retune button
SMDisplayCloseNotSE:
	call PopGUIStacks
	;recall the contrast setting from the memory location and set it to the port
	ld a,($8447)
	add a,23d
	or $c0
	bcall(_lcd_busy)
	out (LCDINSTPORT),a
	;this is in case the user pressed cancel.
	;call RenderGUI
	ld hl,DCSMenuHook
	jp GUIMouseCall				;avoid bcall pop
SMAbout:
	call AboutDisplay
	;call RenderGUI
	jp SMMainMouseCall

SMBackupFoldersNow:
	xor a
	call GUIDrawHourglass
	bcall(_FldSave_NoAVCheck)
	jp SMOptionsMouse

#ifdef false
SMLink:
	ld hl,LinkSMWin
	ld de,LinkSMWin1-LinkSMWin
	ld a,$2
	call PushGUIStack
	ld hl,LinkSMWinB
	ld de,LinkSMWinB1-LinkSMWinB
	ld a,$5
	call PushGUIStack
	ld hl,LinkIcon
	ld de,LinkIcon1-LinkIcon
	ld a,$11
	call PushGUIStack
	ld hl,LinkCustomVector
	ld de,2
	ld a,$17
	call PushGUIStack
	ld hl,LinkCustomVector2
	ld de,2
	ld a,$17
	call PushGUIStack
#ifdef enableCn2
	ld hl,35
	bcall(_DAVLCheckOffset)
	ld a,(hl)
	or a
	push af
	jr nz,SMLinkCn2Yes
#endif
	ld hl,LinkDisabledTxt
	ld de,LinkDisabledTxt1-LinkDisabledTxt
	jr SMLinkCn2
#ifdef enableCn2
SMLinkCn2Yes:
	ld hl,LinkEnabledTxt
	ld de,LinkEnabledTxt1-LinkEnabledTxt
#endif
SMLinkCn2:
	ld a,$4
	call PushGUIStack
#ifdef enableCn2
	pop af
	jr nz,SMLinkgCnHuh
#endif
SMLinkCn2_NoDeal:
	ld hl,LinkgcnnoTxt
	ld de,LinkgcnnoTxt1-LinkgcnnoTxt
	ld a,$4
	call PushGUIStack
#ifdef enableCn2
	jr LinkDoneUp
SMLinkgCnHuh:
	ld hl,LinkgcnwaitTxt
	ld de,LinkgcnwaitTxt1-LinkgcnwaitTxt
	ld a,$4
	call PushGUIStack
	;call RenderGUI
	bcall(_Cn2_Clear_SendBuf)
	ld hl,SMLinkTestgCn
	ld de,Cn2_Int_SendBuf
	ld bc,SMLinkTestgCn1-SMLinkTestgCn
	ldir
	ld hl,0

	im 2
	ei
SMLinkIdle:
	ld b,8
SMLinkIdleLoop:
	inc hl
	inc hl
	dec hl
	dec hl
	djnz SMLinkIdleLoop
	dec hl
	ld a,h
	or l
	jr nz,SMLinkIdle
	call PopGUIStack
	ld a,(Cn2_Int_SendBuf+5)
	or a
	jr nz,SMLinkCn2_NoDeal
	ld hl,LinkgcnyesTxt
	ld de,LinkgcnyesTxt1-LinkgcnyesTxt
	ld a,$4
	call PushGUIStack
LinkDoneUp:
	bcall(_Cn2_Clear_SendBuf)
#endif
	ld hl,$0000
	jp GUIMouseCall				;avoid bcall pop
SMLinkExit:
	ld b,7
	call PopGUIStacks
	ld hl,DCSMenuHook
	jp GUIMouseCall
#endif
SMClose:
	call CloseGUIStack
	ld b,40-8
CloseStartMenuLoop:
	push bc
		ld a,41-8
		sub b
		ld b,a
		ld de,StartMenuShadow
		ld hl,gbuf+((14+8)*12)
CloseSMLShad:
#ifdef lang_english
		ld c,5
#endif
#ifndef lang_english
		ld c,6
#endif
CloseSMLShadL:
		ld a,(de)
		ld (hl),a
		inc hl
		call ShadowIncDE
		dec c
		jr nz,CloseSMLShadL
		push de
#ifdef lang_english
			ld de,12-5
#endif
#ifndef lang_english
			ld de,12-6
#endif
			add hl,de
			pop de
		djnz CloseSMLShad
		pop bc
	push bc
		inc b
		ld hl,gbuf+(54*12)
CloseSMLClear:
#ifdef lang_english
		ld c,5
#endif
#ifndef lang_english
		ld c,6
#endif
CloseSMLCInner:
		ld (hl),0
		inc hl
		dec c
		jr nz,CloseSMLCInner
		push de
#ifdef lang_english
			ld de,-12-5
#endif
#ifndef lang_english
			ld de,-12-6
#endif
			add hl,de
			pop de
		djnz CloseSMLClear
		pop bc
	push bc
#ifdef lang_english
		ld c,5
#endif
#ifndef lang_english
		ld c,6
#endif
		ld a,55
		sub b
		ld l,a
		xor a
		ld ix,StartMenuImg
		call imLargeSprite
		call imFastCopy
		pop bc
	djnz CloseStartMenuLoop

	bcall(_RealStartNoResetRetFrom2)

SM_ContrastUDUp:		;Used in Display menu
;	ld hl,10
;	add hl,bc
	ld a,(bc)
	or a
	ret z
	add a,23d
	or $c0
	bcall(_lcd_busy)
	out (LCDINSTPORT),a
	ret
SM_ContrastUDDown:		;Used in Display menu
;	ld hl,10
;	add hl,bc
	ld a,(bc)
	cp 27h
	ret z
	add a,23d
	or $c0
	bcall(_lcd_busy)
	out (LCDINSTPORT),a
	ret
AboutDisplay:
	ld hl,SmallWinAbout7
	call PushGUIStacks
;	call RenderGUI
	ld hl,$0000
	jp GUIMouseCall
SMAbout7Close:
	ld b,3
	call PopGUIStacks
	ret
SMEditCursor:
	call CursorEdit
	ld hl,40
	bcall(_DAVLCheckOffset)
	ld de,CMouseMask
	ex de,hl
	ld bc,16
	ldir
	ld hl,$0000
	call GUIMouseCall
;	bcall(_RenderDesktopRetFrom2)

IntroPause:
	dec hl
	ld a,h
	or l
	jr nz,IntroPause
	ret

SMDefaultCursor:
	ld hl,40
	bcall(_DAVLCheckOffset)			;was bcall \ ld de, 40 \ add hl,de
	ld de,MOUSE_SPRITE2
	ex de,hl
	ld bc,16
	ldir
	ld hl,$0000
	call GUIMouseCall
	ret
	
BackupAppVar:	
	;first let's verify that it's actually changed
	ld hl,AVBackupName
	rst 20h
	bcall(_chkfindsym)
	jr c,BackupAppVar_FirstTime
	ld a,b
	ld (CurROMPage),a
	call SetupROMP2
	inc hl
	inc hl
	push hl
	bcall(_DAVLCheck)
	pop de
	ld b,AppVarSize
BackupAppVarCheckLoop:
	call GetArcProgByteDEP2
	cp (hl)
	jr nz,BackupAppVarGo
	inc hl
	inc de
	djnz BackupAppVarCheckLoop
	;if we get here, the backup is clean.
	ret
BackupAppVarGo:
	ld hl,AAVSMWin
	ld de,AAVSMWin1-AAVSMWin
	ld a,$2
	call PushGUIStack
	ld hl,AAVSMWinB
	ld de,AAVSMWinB1-AAVSMWinB
	ld a,$5
	call PushGUIStack
	ld hl,AAVIcon
	ld de,AAVIcon1-AAVIcon
	ld a,$11
	call PushGUIStack
	ld hl,AAVText
	ld de,AAVText1-AAVText
	ld a,$4
	call PushGUIStack
	call RenderGUI
	ld hl,AVBackupName
	rst 20h
	bcall(_chkfindsym)
	bcallnc(_delvararc)
BackupAppVar_FirstTime:
	ld hl,AVBackupName
	rst 20h
	bcall(_chkfindsym)
	ld hl,AppVarSize
	push hl
	bcall(_createappvar)
	inc de
	inc de
	push de
	bcall(_DAVLCheck)
	pop de
	pop bc
	ldir
	ld hl,AVBackupName
	rst 20h
	bcall(_chkfindsym)
	bcall(_arc_unarcP2)

	ld b,4
	call PopGUIStacks
	ret

LinkCountIn:
#ifdef enableCn2
	ld hl,(20*256)+33
	ld de,Cn2_Count_Rec
	jr LinkCount1
#endif
LinkCountOut:
#ifdef enableCn2
	ld hl,(28*256)+33
	ld de,Cn2_Count_Send
LinkCount1:
	ld (pencol),hl
	push de
	ld hl,35
	bcall(_DAVLCheckOffset)
	ld a,(hl)
	pop hl
	or a
	ret z
	bcall(_DispLongInt)
#endif
	ret

AVBackupName:
	.db 15h,"DCS7b",0

EnableLaunchKeyhook:
	ld hl,AVOff_OnPrgmHook
	bcall(_DAVLCheckOffset)
	ld a,(hl)
	or a
	ret z									;don't install if it's not enabled.
	inc hl
	bit 5,(iy+34h)
	jr z,installLaunchKeyHook
	ex de,hl
	ld hl,9B86h
	ld b,(hl)
	dec hl
	dec hl
	in a,(6)
	cp b
	jr z,installLaunchKeyHookSaved
	ld bc,4
	ldir
	jr installLaunchKeyHookSaved
installLaunchKeyHook:
	;ld hl,AVOff_OnPrgmHookSave
	;bcall(_DAVLCheckOffset)
	xor a
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a					;believe it or not, saves 2 bytes over ldir method
installLaunchKeyHookSaved:
	ld hl,LaunchKeyHook
	in a,(6)
	bcall(4F66h)				;KeyhookEnable bcall
	ret

EnableMyParserhook:
	ld hl,AVOff_ParserHook
	bcall(_DAVLCheckOffset)
	ld a,(hl)
	or a
	ret z									;don't install if it's not enabled.
	inc hl
	bit 1,(iy+36h)
	jr z,installParserhook
	ex de,hl
	ld hl,9BACh+2
	ld b,(hl)
	dec hl
	dec hl
	in a,(6)
	dec a					;parserhook on page 3
	cp b
	jr z,installParserhookSaved
	ld bc,4
	ldir
	jr installParserhookSaved
installParserhook:
	;ld hl,AVOff_ParserHookSave
	;bcall(_DAVLCheckOffset)
	xor a
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a					;again, this is shorter than the ldir. Go figure.
installParserhookSaved:
	ld hl,ParserHook
	in a,(6)
	dec a						;ParserHook is on page THREE! :D
	bcall(5026h)				;ParserhookEnable bcall
	ret
	
LaunchKeyHook:
	add a,e
	push af
		push de
			push bc
				push hl
					ld hl,AppVarName2
					rst 20h
					bcall(_chkfindsym)
					jr c,myRawKeyHookContinue
					ld hl,71+2
					add hl,de
					ld e,(hl)
					inc hl
					ld d,(hl)
					ld a,d
					or e
					jr z,myRawKeyHookContinue
					inc hl
					ld a,(hl)
					ld h,d
					ld l,e
					inc de
					ld (Op1),de
					ld (Op1+2),a
					B_CALL(_GetBytePaged)
					ld a,b
					cp 83h
					jr nz,myRawKeyHookContinue
					pop hl
				pop bc
			pop de
		pop af
	ld ix,returnPoint
	push ix
		ld ix,Op1
		push ix
			B_JUMP(_OffPageJump)
returnPoint:
	ret z ;other hook said to ignore keypress
	jr myRawKeyHookContinue2
myRawKeyHookContinue:
				pop hl
			pop bc
		pop de
	pop af
myRawKeyHookContinue2:   
	push af
		in a,(4)
		and %0001000
		or a
		jr z,LaunchGo
		pop af
myRawKeyHookCancel:
	or a
	ret
LaunchGo:
		pop af
	cp $2D
	jr z,MyGetKeyHook
    cp $31					;kXT0n aka kVarX aka kGraphVars
	jr nz,myRawKeyHookCancel
    bit apdAble,(iy+apdFlags)
    jr z,myRawKeyHookCancel
    set apdRunning,(iy+apdFlags)
    ;jr z,myRawKeyHookCancel
    ld a,$01
    ld ($8449),a
    ld ($8448),a
    xor a
    ret
MyGetKeyHook:
	add a,e
	res curable,(iy+CurFlags)
	bcall(_disablegetkeyhook)
	xor a
	ld (menuCurrent),a
	ld hl,MyAppName3+1
	ld de,progToEdit
	ld bc,8
	ldir
	bcall(_CloseEditEqu) ;to start the app
	res 2,(iy+68)
	res 5,(iy+69)
	set 7,(iy+73)
	;ld a,cxExtAp
	;bcall(_NewContext)
	bcall(_ExecuteApp)
	;xor a
	;or a
	;ret
MyAppName3:
  .db AppObj,"DoorsCS7"
	
#ifdef false
SMLinkTestgCn:
	.db $AA,$AA,$AA,$AA,$AA
	.dw 2
	.db $FF,$00
SMLinkTestgCn1:
LinkCustomVector:
	.dw LinkCountIn
LinkCustomVector2:
	.dw LinkCountOut
#endif
	
ShadowIncDE:
	inc de
	push hl
		ld hl,StartMenuShadow1End
		or a
		sbc hl,de
		add hl,de
		jr nz,ShadowIncDE2
		ld de,StartMenuShadow2
ShadowIncDE2:
		pop hl
	ret

SMOptions_NextCheckFromAV:
	push bc
		call GUIFindNext
		pop bc
	push de
		push hl
			push bc
				pop hl
			bcall(_DAVLCheckOffset)
			pop de
		push de
			ex de,hl
			ld bc,6
			add hl,bc
			ld a,(de)
			ld (hl),a
			pop hl
		pop de
	ret

SMOptions_NextAVFromCheck:
	push bc
		call GUIFindNext				;run on start force checkbox
		pop bc
	push de
		push hl
			push bc
				pop hl
			bcall(_DAVLCheckOffset)
			pop de
		push de
			ex de,hl
			ld bc,6
			add hl,bc
			ld a,(hl)
			ld (de),a
			pop hl
		pop de
	ret
