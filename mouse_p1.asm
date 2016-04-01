;-----------------------------------------------------------
;	Filename:		mouse_p1.asm
;	Long name:  	Mouse routines for app page 1
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

;------------------------------
;Special Return Labels (page 1 of [0 1 2])
GUIDesktop_Click_P1
	ld b,0
	jr DesktopMouseReturn_P1
GUIDesktop_Click_P2
	ld b,1
	jr DesktopMouseReturn_P1
GUIDesktop_Click_P3
	ld b,2
	jr DesktopMouseReturn_P1
GUIDesktop_Click_P4
	ld b,3
	jr DesktopMouseReturn_P1
GUIDesktop_Click_P5
	ld b,4
	jr DesktopMouseReturn_P1
GUIDesktop_Click_P6
	ld b,5
	jr DesktopMouseReturn_P1
GUIDesktop_Click_UpFld:
	ld b,6
	jr DesktopMouseReturn_P1
GUIDesktop_Click_SM:
	ld b,7
	jr DesktopMouseReturn_P1
GUIDesktop_Click_ScrollUp:
	ld b,8
	jr DesktopMouseReturn_P1
GUIDesktop_Click_ScrollDown:
	ld b,9
	jr DesktopMouseReturn_P1
GUIDesktop_Click_Quit:
	ld b,10
	jr DesktopMouseReturn_P1
GUIDesktop_Click_Rerender:
	ld b,11
	jr DesktopMouseReturn_P1
GUIDesktop_Click_ScrollWayUp:
	ld b,12
	jr DesktopMouseReturn_P1
GUIDesktop_Click_ScrollWayDown:
	ld b,13
;	jr DesktopMouseReturn_P1
DesktopMouseReturn_P1:
	bcall(_DesktopMouseReturn)
;------------------------------
GUIPropP_Click_1:
	ld b,0
	jr PropMenuMouseReturn_P1
GUIPropP_Click_2:
	ld b,1
	jr PropMenuMouseReturn_P1
GUIPropP_Click_3:
	ld b,2
	jr PropMenuMouseReturn_P1
GUIPropP_Click_4:
	ld b,3
	jr PropMenuMouseReturn_P1
GUIPropP_Click_5:
	ld b,4
	jr PropMenuMouseReturn_P1
GUIPropP_Click_6:
	ld b,5
	jr PropMenuMouseReturn_P1
GUIPropP_Click_7:
	ld b,6
	jr PropMenuMouseReturn_P1
GUIPropP_Click_8:
	ld b,7
	jr PropMenuMouseReturn_P1
GUIPropP_Click_9:
	ld b,8
	jr PropMenuMouseReturn_P1
GUIPropP_Click_10:
	ld b,9
	jr PropMenuMouseReturn_P1
GUIPropP_Click_11:
	ld b,10
;	jr PropMenuMouseReturn_P1
PropMenuMouseReturn_P1:
	bcall(_PropMenuMouseReturn)
;------------------------------
MainMouseHook:				;called by GUIMouse
	and a					;.db $a7, signals to check output of this hook for SE info
	;Shell Extensions
	call RunMouseSEs
	ret nz

NoSEAtMouse:						;---Batt Check---

	;tabfunc key checks
	ld a,(TabFuncMode)
	inc a
	jr z,TabFuncCheckClick
	ld a,0feh				;check for the arrow keys, disable tabfuncs
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	cpl
	and %00001111
	jr z,TabFuncCheckClick
	;erase and turn off if the arrows were pressed
	bcall(_TabFunc_GetCoords)
	ld a,2
	call mos_filledrectangle
	ld a,0ffh
	ld (TabFuncMode),a
	jr PostTabFuncs
	
TabFuncCheckClick:
	ld a,0f7h				;graph keys
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	cp 07Fh
	bcallz(_TabFunc_Tab)
	ld a,0efh				;graph keys
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	cp 07Fh
	bcallz(_TabFunc_ShiftTab)
	ld a,(TabFuncMode)
	inc a					;0xff = inactive
	jr z,PostTabFuncs
	ld a,0dfh				;alpha key
	out (1),a
	nop \ nop
	in a,(1)
	cp 07fh
	jr z,TabFunc_RightClickGo
	ld a,0bfh				;graph keys
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	cpl						;1s to 0s, 0s to 1s, == xor $ff
	and %10100011			;[2nd], [trace], [graph]
	jr z,FinishTabFunc
	and %00000001
	ld a,1
	jr z,TabFunc_LeftClickGo	;z here means it *wasn't* %fe = [GRAPH]
TabFunc_RightClickGo:
	ld a,2
TabFunc_LeftClickGo:
	ld (LastClick),a
	bcall(_TabFunc_GetCoords);fills in (b,c,d,e)
	push hl
		ld a,2
		call mos_filledrectangle
		call GUIMouse_ClrMse2
		pop hl
	inc h
	inc l
	ld (MseY),hl
	pop hl					;force click
	ret						;do it!
	
FinishTabFunc:				;the following performs debouncing
	ld b,4
FinishTabFuncLoop:
	ld a,0f7h				;graph keys
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	push af
		ld a,0efh				;graph keys
		out (1),a				;enable
		nop \ nop
		in a,(1)				;get the value
		pop de
	and d
	inc a
	jr nz,FinishTabFunc		;loop if not debounced
	djnz FinishTabFuncLoop
	
PostTabFuncs:
	ld a,(MouseMode)
	or a
	jr z,MouseP1_Hotkeys_Desktop
MouseP1_Hotkeys_PropMenu:
	ld a,0fdh				;Clear and more
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	cp 191
	jr nz,MouseP1_Hotkeys_PropMenuRetZ
	call GUIMouse_ClrMse2
	ld hl,(2*256)+2				;click outside to cancel
	ld (MseY),hl				;MseX in h, MseY in l
	pop hl
MouseP1_Hotkeys_PropMenuRetZ:
	xor a
	ret							;should force the click

MouseP1_Hotkeys_Desktop:
;Code to look for shortcut keys
	ld a,0bfh				;graph keys
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	cp 239
	jp z,DCSMenuDirect
	cp 247
	jp z,UpFldDirect
	ld a,0ffh				;reset
	out (1),a
	ld a,0fdh				;Clear and more
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	cp 191					;[clear]
	jp z,ExitNow
	cp $ef					;[/]
	jp z,MScrollWayUp
	cp $f7					;[*]
	jp z,MScrollWayDown
	cp 251d					;[-]
	jp z,MScrollUp
	cp 253d					;[+]
	jp z,MScrollDown
;	ld a,0ffh				;reset
;	out (1),a
	ld a,0efh				;1 and 4
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	ld b,0
	cp 251
	jp z,MRunProgGo
	ld b,3
	cp 253
	jp z,MRunProgGo
;	ld a,0ffh				;reset
;	out (1),a
	ld a,0f7h				;2 and 5
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	ld b,1
	cp 251
	jp z,MRunProgGo
	ld b,4
	cp 253
	jp z,MRunProgGo
;	ld a,0ffh				;reset
;	out (1),a
	ld a,0fbh				;3 and 6
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	ld b,2
	cp 251
	jp z,MRunProgGo
	ld b,5
	cp 253
	jp z,MRunProgGo
		
	ld a,(TabFuncMode)
	inc a
	jr z,InfoPopCheck		;don't do MemoryPop/InfoPop unless TabFunc is inactive
InfoPopRetZ:
	xor a
	ret						;we need to return A==0 FOR SEs HERE!!
	;MemoryPop, InfoPop
InfoPopCheck:
	ld hl,(APDnext)
	ld a,h
	and l
	cp $10
	jr nz,InfoPopRetZ		;we need to return A!=0 FOR SEs HERE!!
	ld a,(dAPDtimer)
	or a
	jr nz,InfoPopRetZ		;we need to return A!=0 FOR SEs HERE!!
	
	ld hl,(MseY)
	ld bc,(3*256)+3
	ld de,(82*256)+54
	bcall(_hdetect)
	jr nz,MemoryPopCheck
;Do the InfoPop
	ld b,0
	ld a,h
	cp 32
	jr c,InfoPopSetY
	inc b
	cp 61
	jr c,InfoPopSetY
	inc b
InfoPopSetY:
	ld a,l
	cp 29
	jr c,InfoPopGo
	inc b
	inc b
	inc b
InfoPopGo:
	ld a,(ProgsDone)
	ld c,a
	ld a,(TotalProgs)
	sub c
	bit 7,a
	jr nz,InfoPopGoImmediate
	inc b
	sub b
	jr c,InfoPopRetZ
	dec b
InfoPopGoImmediate	
	push bc
		call GUIMouse_ClrMse2
		pop bc
	bcall(_InfoPop)
	jr MemoryPopFinish
MemoryPopCheck:
	ld bc,(66*256)+56
	ld de,(76*256)+62
	bcall(_hdetect)
	jr nz,InfoPopRetZ
;Do the MemoryPop
	call GUIMouse_ClrMse2
	bcall(_MemoryPop)
MemoryPopFinish:
	call GUIMouse_PutMse_NoTimerReset
MemoryPopFinishRetZ:
	xor a
	ret

;------------------------------
MRunProgGo:
	ld a,b
	cp 3
	jr c,MRunProgGo2
	sub 3
MRunProgGo2:
	add a,a
	add a,a
	add a,a
	add a,a
	add a,a
	add a,4						;4, 36, or 70
	ld h,a
	ld l,10
	ld a,b
	cp 3
	jr c,MRunProgGo3
	ld l,40
MRunProgGo3:
	push hl
		call GUIMouse_ClrMse2
		pop hl
	ld (MseY),hl				;MseX in h, MseY in l
	pop hl
	ret							;should force the click
MScrollUp:
	ld hl,(92*256)+3
	jr MRunProgGo3
MScrollDown:
	ld hl,(92*256)+53
	jr MRunProgGo3
MScrollWayUp:
	ld hl,(98*256)+4
	jr MRunProgGo3
MScrollWayDown:
	ld hl,(98*256)+7
	jr MRunProgGo3
UpFldDirect:
	ld a,(CurFldr)
	dec a
	ret z
	ld hl,(20*256)+60
	jr MRunProgGo3
DCSMenuDirect:
	ld hl,(9*256)+60
	jr MRunProgGo3
ExitNow:
	ld hl,(92*256)+60
	jr MRunProgGo3
;------------------------------
DCSMenuHook:
	and a					;.db $a7, signals to check output of this hook for SE info
	call RunMouseSEs
	ret nz
	ld a,0bfh				;graph keys
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	cp 239
	jr z,DCSMH_DCSMenuClose
	ld a,0ffh				;reset
	out (1),a
	ld a,0fdh				;Clear and more
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	cp 191					;[clear] key
	jr nz,MemoryPopFinishRetZ
DCSMH_DCSQuit:
	bcall(_ExitDoorsCS)
	;ret					;no need
DCSMH_DCSMenuClose:
	ld hl,(9*256)+60
	jr MRunProgGo3
;------------------------------
SMMouseHook:
	and a					;.db $a7, signals to check output of this hook for SE info
	call RunMouseSEs
	ret						;returns z or nz

RunMouseSEs:
	ld a,(SETotal)
	or a
	ret z
	ld hl,5
	push bc
		bcall(_RunSEs)
		pop bc
	ld a,d
	or a
	ret

;------------------------------
sum12MouseHook:
	ld a,0dfh				;graph keys
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	cp 0fdh
	ret nz
	call GUIMouse_ClrMse2
	call RenderGUIGetWinX
	ld d,a
	call RenderGUIGetWinY
	ld e,a
	ld hl,(96*256)+0
	add hl,de
	ld (MseY),hl				;MseX in h, MseY in l
	pop hl
	ret							;should force the click
