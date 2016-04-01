;-----------------------------------------------------------
;	Filename:		GUIMouse.asm
;	Long name:  	GUI Mouse Setup and Run Routines
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	May 1, 2010
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;----------------------------------------------------------
;	.dw GUIRMouseCursor				;$18							[xy,xy,8-byte mask,8-byte sprite]
;	.dw GUIRMouseClick				;$19							[entry type, entry # in top group, onclick ptr,precleanstack(0/1),upperleft [word], lowerright [word]]
;----------------------------------------------------------
	
GUIMouseRAM:
	pop de
GUIMouse:
	;| First, we need to find the beginning of the top stack entry
	;| and save it for future reference.  That will be the virtual
	;| base, since we only need to deal with the stuff at the top
	;| of the stack.  Next, we will cycle through the top group
	;| until we reach the very top of the GUIStack, pushing both
	;| cursor definitions and hotspot definitions onto the stack.
	;| After that, we just jump to the GUIMouseRun routine and
	;| wait for a valid click of some sort.
	;
	;Basic schematic format:
	; >start searching stack
	; >>is it a group item? if so, save the ptr
	; >>>loop until we find the end or another group item
	; >>is it another group? pop and jump back 2
	; >pop and save
	;
	;Interesting output note: ix has the index of the hotspot that
	;triggered the cessation of mouse activity.
	;
				pop de											;this is to deal with clearing out the bcall since bjump fails
			pop de
		pop de
GUIMouseSetup:										;skip the extra bcall suppression pops
GUIMouseCall:
	;hl = location of mouse hook
	ld (GUIMouseHookLoc),hl
GUIMouseCallReRender:
	ld hl,(GUIMouseHookLoc)
	push hl
		call RenderGUI
		call GUIStackFindTop
		;hl=start, de=end
		bcall(_cphlde)			;zero if no items in the top group
		ld c,1
		pop ix
	ret z					;this needs fixing
	push ix
		push de
			xor a
			ld (ScratchVar),a
GUIMouseSetupLoopM:
			push de
				push hl
					inc hl
					inc hl
					ld a,(hl)
					inc hl
					push hl
						pop bc
					add a,a
					ld hl,GUIMVectorTable
					ld e,a
					ld d,0
					add hl,de
					bcall(_ldhlind)
					ld a,h
					or l
					jr z,GUIMouseSetupLoopMRet
					ld ix,GUIMouseSetupLoopMRet
					push ix
						jp (hl)
GUIMouseSetupLoopMRet:
					pop hl
				push hl
					ld e,(hl)					;saved one byte next three lines 9/28/2010
					inc hl
					ld d,(hl)
;					bcall(_ldhlind)
;					ex de,hl
					ld hl,ScratchVar
					inc (hl)
					pop hl
				add hl,de
				pop de
;			ld a,(ScratchVar)					;moved five lines up, save 3 bytes
;			inc a
;			ld (ScratchVar),a
			call mos_cphlde
			jr nz,GUIMouseSetupLoopM
			call OpenGUIStack
			push hl
				dec hl
				dec hl
				ld e,(hl)
				inc hl
				ld d,(hl)
				pop hl						;9/28/2010 replaced ldhlind/pop de; speed improvement
			add hl,de
			ld (GUIStackMouseEnd),hl
			pop hl				;hl="end" of mouse GUIStack entries
		ld (GUIStackMouse),hl	;hl="start" of mouse GUIStack entries
		pop hl
	ld (GUIMouseHookLoc),hl
GUIMouseRun:
	ld hl,40
	bcall(_DAVLCheckOffset)
	ld de,MouseSpriteLoc
	ex de,hl
	ld (hl),e
	inc hl
	ld (hl),d
	;now we need to display the mouse, wait for a keypress, move or act.
	call GUIMouseSub
GUIMouseRunDone:
;	pop hl						;this is the base start
	ld hl,(GUIStackMouse)
	ld de,(GUIStackMouseEnd)
	ld ix,0						;which item triggered it, useful for DCSB Libs
GUIMouse_ClickLoop:
	bcall(_cphlde)
	jr z,GUIMouseRun
	push de
		push hl
			inc hl
			inc hl
			ld a,(hl)
			cp $19
			jr nz,GUIMouse_ClickLoopNo
			ld de,6
			add hl,de
			ld c,(hl)
			inc hl
			ld b,(hl)
			inc hl
			ld e,(hl)
			inc hl
			ld d,(hl)
			push hl
				ld hl,(MseWord)
				bcall(_hdetect)				;doesn't touch ix
				pop hl
			jr z,GUIMouse_ClickLoopFound
GUIMouse_ClickLoopNo:
			inc ix
			pop hl
		push hl
			bcall(_ldhlind)
			pop de
		add hl,de
		pop de
	jr GUIMouse_ClickLoop
GUIMouse_ClickLoopFound:
			pop hl
		pop bc			;hl = entry start, bc = end of stack
	push hl
		ld de,5
		add hl,de
		bcall(_ldhlind)
		pop bc
	push hl
		inc bc
		inc bc
		push bc
			pop de
		inc bc
		inc bc
		ld a,(bc)
		push bc
			ld b,a
			push bc
				push ix
					call GUIStackFindTop
					pop ix
				pop bc
GUIMouse_FoundLoop:
			ld a,b
			or a
			jr z,GUIMouse_FoundDone
			push hl
				bcall(_ldhlind)
				pop de
			add hl,de
			dec b
			jr GUIMouse_FoundLoop
GUIMouse_FoundDone:
			ld b,h
			ld c,l
			pop de
		inc bc
		inc bc
		inc bc
		push bc
			inc de
			inc de
			inc de
			ld a,(de)
			or a
			jr z,GUIMouse_FoundDone_NoPreClean
			push ix
				call GUISRemoveMouseEntries
				pop ix
GUIMouse_FoundDone_NoPreClean:
			pop bc
			
		;Added to make shortcut key use invisible
;		ld hl,(GUIMouseHookLoc)
;		ld a,h
;		or l
;		jr z,GUIMouse_FoundDone_NoRestore
		ld hl,(GUIMseSave)
		ld (MseY),hl
GUIMouse_FoundDone_NoRestore:
			
		pop hl
	jp (hl)
	
	;the following two routines must update:
	;>> entry size  <---done
	;>> av size	<--done
	;>> insert memory/delete memory <--done
InsertGUIStackText:			;hl = where.  Assumed 1 byte
						;de = stack entry start
	push de
		push hl
			ld hl,(iMathPtr2)
			ld de,(iMathPtr3)
			call mos_cphlde
			pop hl
		pop de
	ret z

	push hl
		dec de
		dec de
		dec de
		push de
			ex de,hl
			bcall(_ldhlind)
			inc hl
			pop de
		ex de,hl
		ld (hl),e
		inc hl
		ld (hl),d
		pop hl						;increment the item's size word

;	hl = where, 1 byte, iMathPtr2 current byte after last
	ld de,(iMathPtr2)
	push de
		push de
			ex de,hl			;de=where, hl=first byte after
			or a
			sbc hl,de
			push hl
				pop bc			;bc=bytes to move
			pop hl
		push hl
			pop de
		dec hl				;from where to where+1
		lddr		

		pop hl
	inc hl
	ld (iMathPtr2),hl
	ld hl,(iMathPtr1)
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc de
	ld (hl),d
	dec hl
	ld (hl),e				;increment size word of program
	
	ld hl,(GUIStackMouse)
	inc hl
	ld (GUIStackMouse),hl
	ld hl,(GUIStackMouseEnd)
	inc hl
	ld (GUIStackMouseEnd),hl
	or h				;to reset zero flag
	ret

DeleteGUIStackText:			;hl=where.  Assumed 1 byte
						;de = stack entry start
	push hl
		dec de
		dec de
		dec de
		push de
			ex de,hl
			bcall(_ldhlind)
			dec hl			;decrement size of entry
			pop de
		ex de,hl
		ld (hl),e
		inc hl
		ld (hl),d
		pop hl

;	hl = where, 1 byte, iMathPtr2 current byte after last
	ld de,(iMathPtr2)
	push de
		ex de,hl			;de=where, hl=first byte after
		or a
		sbc hl,de
		push hl
			pop bc			;bc=bytes to move
		push de
			pop hl
		inc hl				;from where to where+1
		dec bc
		ldir		

		pop hl
	dec hl
	ld (iMathPtr2),hl
	ld hl,(iMathPtr1)
	ld e,(hl)
	inc hl
	ld d,(hl)
	dec de
	ld (hl),d
	dec hl
	ld (hl),e				;decrement size word of program

	ld hl,(GUIStackMouse)
	dec hl
	ld (GUIStackMouse),hl
	ld hl,(GUIStackMouseEnd)
	dec hl
	ld (GUIStackMouseEnd),hl
	or h						;resets zero flag
	ret
	
GUISRemoveMouseEntries:
	call GUIStackFindTop
;	ld a,c
;	or a
;	ret nz					;return if c!=0 : this indicates an error.
	push bc					;the final total end storage variable
GUISRemoveMouseEntriesLoop:
		push hl
			inc hl
			inc hl
			ld a,(hl)
			sub $19
			jr z,GUISRemoveMouseEntriesFirstFound
			dec a
			jr z,GUISRemoveMouseEntriesFirstFound
			pop hl
		push hl
			bcall(_ldhlind)
			pop de
		ex de,hl
		add hl,de
		pop de
	push de
		call mos_cphlde
;		jr z,GUISRemoveMouseEntriesLoopNone
		jr nz,GUISRemoveMouseEntriesLoop			;saved two bytes
GUISRemoveMouseEntriesLoopNone:
		pop de
	ld c,1
	ret
GUISRemoveMouseEntriesFirstFound:
			pop hl
		pop de
GUISRemoveMouseEntriesDelLoop:
	push hl
		call PopGUIStack
		call GUIStackFindTop
		pop hl
	call mos_cphlde					;not sure why it's DE, not BC, but w/e
	jr nz,GUISRemoveMouseEntriesDelLoop
	ld c,0
	ret
GUIFindFirst:
	call OpenGUIStack
	ld a,c
	inc c
	or a
	ret z							;if c was 0, then this was the first time the stack had been opened.  c=1 if this has happened
	jp GUIStackFindTop
	;ret
GUIFindNext:						;requires hl and de.
	call mos_cphlde
	ld c,1
	ret z							;c=1 means that we're already at the end
	push de
		push hl
			bcall(_ldhlind)
			pop de
		add hl,de
		pop de
	ld c,0							;c=0 means success.  DE=end still, HL=beginning of next entry
	ret
GUIFindThis:
	push af
		call GUIFindFirst
		pop bc
GUIFindThisLoop:
	ld a,b
	or a
	jr z,GUIFindThisDone
	call GUIFindNext
	dec b
	jr GUIFindThisLoop
GUIFindThisDone:
   ld c,(hl)
   inc hl
   ld b,(hl)
   dec hl
   ret
  
GUIMouseAccelReset:
	ld hl,(AppVarLoc)
	ld de,37
	add hl,de
	ld b,(hl)
	ld hl,MouseAccelAct
	ld (hl),b
	inc hl
	ld (hl),b
	ret
	
GUIMouseSub:						;main mouse routine
	bcall(_DAVLCheck)
	call GUIMouseAccelReset
GUIMOUSE_MOVELOOP:
	xor a
	ld (dAPDtimer),a
	ld h,a							;\ 
	ld l,a							; |- why was this previously omitted?
	ld (APDnext),hl					;/ 
	call GUIMOUSE_PUTMSE			;put it up
GUIMOUSE_GETKEY:
	in a,(4)
	bit 3,a
	jr z,GUIMOUSE_OFF
	ld hl,(APDnext)
	inc hl
	ld (APDnext),hl
	ld a,h
	or l
	jr nz,GUIMOUSE_GETKEY2
	ld a,(dAPDtimer)
	inc a
	ld (dAPDtimer),a
	cp 00ch
	jr nz,GUIMOUSE_GETKEY2
GUIMOUSE_OFF:
	bcall(_Off)
	jp GUIMouse_Getkey
GUIMOUSE_GETKEY2:

	ld a,(MseY)
	ld l,a
	ld a,(MseX)
	ld h,a
	ld (GUIMseSave),hl							;Added to make shortcut key use invisible
	;push hl
		ld de,(GUIMouseHookLoc)
		ld a,d
		or e
	;	pop de
	ex de,hl									;9/28/2010 saved two stack accesses
	jr z,GUIMOUSE_GETKEYNOCALL
	ex de,hl
	ld bc,GUIMOUSE_HOOKRETURN
	push bc			;return loc
		push de			;hook loc
			ld a,h									;for backwards compatiblity, (h,l) and (a,l) aliases for (x,y)
			ret										;call the hook, if there is one.
GUIMOUSE_HOOKRETURN:
	ld d,a
	
	ld hl,(GUIMouseHookLoc)
	ld a,(hl)
	cp $A7										;$A7 = and a, signals SE checkingness
	jr nz,GUIMOUSE_GETKEYNOCALL

	ld a,d
	or a
	jr z,GUIMOUSE_GETKEYNOCALL
	ld b,$ff										;for GUIMouse_ClrMse checks (??why??)
	dec a
	jp z,GUIMOUSE_LCLICK
	dec a
	jp z,GUIMOUSE_RCLICK
	dec a
	jp z,GUIMOUSE_DOWN
	dec a
	jp z,GUIMOUSE_UP
	dec a
	jp z,GUIMOUSE_LEFT
	dec a
	jp z,GUIMOUSE_RIGHT
	dec a
	cp 94
	jr nc,MouseSECheck2
	push af
		call GUIMouse_ClrMse2
		pop af
	ld (MseX),a
	jr GUIMOUSESE_REDRAW
MouseSECheck2:
	sub 96
	cp 62
	jr nc,GUIMOUSE_GETKEYNOCALL
	push af
		call GUIMouse_ClrMse2
		pop af
	ld (MseY),a
GUIMOUSESE_REDRAW:
	call GUIMOUSE_PUTMSE
GUIMOUSE_GETKEYNOCALL:

	ld hl,MouseAccelNxCt
	ld a,(hl)
	or a
	jr nz,GUIMouseSpeedNoChange
	ld (hl),1
	dec hl
	push hl
		ld hl,(AppVarLoc)
		ld de,66
		add hl,de
		ld b,(hl)
		pop hl
	ld a,(hl)
	sub b
	or a
	jr z,GUIMouseSpeedNoChange
	ld (hl),a
	inc hl
	ld (hl),a
GUIMouseSpeedNoChange:

	ld hl,MouseAccelNxCt
	ld a,(hl)
	cp $ff
	jr z,GUIMouseSpeedNoChg_NoAccel
	dec a
	ld (hl),a
GUIMouseSpeedNoChg_NoAccel:
	ld b,a

	ld a,0ffh				;reset
	out (1),a
	ld a,0fdh				;Clear and more
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	cp 191
	call z,GUIMOUSE_KEYCLEAR
	cp $fe
	ld a,0ffh				;reset
	out (1),a
	ld a,0feh				;arrow keys
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the direct input
	cp 0f7h
	jp z,GUIMOUSE_down			 	;up key
	cp 0f5h
	jp z,GUIMOUSE_UPLEFT
	cp 0f3h
	jp z,GUIMOUSE_UPRIGHT
	sub 0fah
	jp z,GUIMOUSE_DOWNRIGHT
	dec a						;$fb
	jp z,GUIMOUSE_RIGHT			;right key
	dec a						;$fc
	jp z,GUIMOUSE_DOWNLEFT
	dec a				   		;$fd
	jp z,GUIMOUSE_LEFT
	dec a						;$fe
	jp z,GUIMOUSE_up				;down key
	ld a,0ffh				;reset
	out (1),a

;I really don't like this hack at all
	ld hl,(GUIMouseHookLoc)
	ld de,MainMouseHook
	call mos_cphlde
	jr nz,GUIMouse_Clickkeys
	ld a,(TabFuncMode)
	inc a
	jr nz,GUIMouse_SkipClickkeys

GUIMouse_Clickkeys:
	ld a,0bfh				;graph keys
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	cp 253d
	jp z,GUIMOUSE_LCLICK		   	;TRACE key
	cp 254d
	jp z,GUIMOUSE_RCLICK		   	;GRAPH key
	cp 223					;2nd key
	jp z,GUIMOUSE_LCLICK
	ld a,0ffh				;reset
	out (1),a
	ld a,0dfh				;alpha group
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	cp 127					;alpha
	jp z,GUIMOUSE_RCLICK
	ld a,0ffh				;reset
	out (1),a
	ld a,0fdh				;Clear and more
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	cp $fe
	jp z,GUIMOUSE_LCLICK
GUIMouse_SkipClickkeys:

	call GUIMouseAccelReset

	jp GUIMOUSE_GETKEY				;invalid key - repeat
GUIMOUSE_UPLEFT:					;Saved 36 bytes with the dec/inc (hl)
	call GUIMOUSE_CLRMSE
	ld hl,MseY
	dec (hl)
	inc hl
	dec (hl)						;9/28/10 saved 2 bytes
	jp GUIMOUSE_MOVELOOP
GUIMOUSE_UPRIGHT:
	call GUIMOUSE_CLRMSE
	ld hl,MseX
	inc (hl)
	dec hl							;.db MseY, MseX
	dec (hl)
	jp GUIMOUSE_MOVELOOP
GUIMOUSE_DOWNLEFT:
	call GUIMOUSE_CLRMSE
	ld hl,MseX
	dec (hl)
	dec hl
	inc (hl)
	jp GUIMOUSE_MOVELOOP
GUIMOUSE_DOWNRIGHT:
	call GUIMOUSE_CLRMSE
	ld hl,MseX
	inc (hl)
	dec hl
	inc (hl)
	jp GUIMOUSE_MOVELOOP
GUIMOUSE_LEFT:
	call GUIMOUSE_CLRMSE
	ld hl,MseX
	dec (hl)
	jp GUIMOUSE_MOVELOOP
GUIMOUSE_RIGHT:
	call GUIMOUSE_CLRMSE
	ld hl,MseX
	inc (hl)
	jp GUIMOUSE_MOVELOOP
GUIMOUSE_DOWN:
	call GUIMOUSE_CLRMSE
	ld hl,MseY
	dec (hl)
	jp GUIMOUSE_MOVELOOP
GUIMOUSE_UP:
	call GUIMOUSE_CLRMSE
	ld hl,MseY
	inc (hl)
	jp GUIMOUSE_MOVELOOP
GUIMOUSE_KEYCLEAR:
	ld de,AVOff_GUIWinType
	ld hl,(AppVarLoc)				;this should already be set up
	add hl,de
	ld a,(hl)
	cp 2
	ret nz							;don't do it if not a GUIRSmallWin
	push hl
		call GUIMouse_ClrMse2
		pop hl
	inc hl
	ld a,(hl)
	inc hl
	ld l,(hl)
	ld h,a
	ld de,(74*256)+(256-5)
	add hl,de
	ld (MseY),hl
	pop bc							;don't return
	ret
GUIMOUSE_CLRMSE:
	push hl
		ld hl,(GUIMouseHookLoc)
		ld a,(hl)
		pop hl
	cp $3f
	jp nz,GUIMOUSE_CLRMSE12 ;acceleration disabled!
	ld a,$ff
	out (1),a
	nop
	ld a,$bf
	out (1),a
	nop \ nop
	in a,(1)
	inc a
	jr z,GUIMOUSE_CLRMSE12
	ld a,b
	cp $ff
	jr z,GUIMouse_ClrMse2
	and %00001111
	jr z,GUIMouse_ClrMse2
	jr GUIMOUSE_CLRMSE12_DENY
GUIMOUSE_CLRMSE12:
	ld a,b
	cp $ff
	jr z,GUIMouse_ClrMse2
	or a
	jr z,GUIMouse_ClrMse2
GUIMOUSE_CLRMSE12_DENY:
	pop hl
	jp GUIMOUSE_GETKEY
GUIMouse_ClrMse2:
	ld a,(MseY)
	ld e,a
	ld a,(MseX)
	call imGetPixel
	ld de,IconSpace32b+16
	ld bc,(8*256)+255		;the 'c' components get consumed in the ldi's
	ex de,hl
GUIMousePutMseSaveLoop:
	ldi
	ldi
;	ld a,(de)
;	ld (hl),a
;	inc hl
;	inc de
;	ld a,(de)
;	ld (hl),a
;	inc de
	push hl
;		ld de,11
		ld hl,10
		add hl,de
		pop de
	ex de,hl
	djnz GUIMousePutMseSaveLoop
	ret
GUIMOUSE_PUTMSE:
	xor a
	ld (dAPDtimer),a
GUIMouse_PutMse_NoTimerReset:
	ld ix,(MouseSpriteLoc)
	ld hl,(GUIStackMouse)
	ld de,(GUIStackMouseEnd)
GUIMOUSE_PUTMSE_Loop:
	call mos_cphlde
	jr z,GUIMOUSE_PUTMSE_CursorDone
	push de
		push hl
			inc hl
			inc hl
			ld a,(hl)
			cp $1a
			jr nz,GUIMOUSE_PUTMSE_CursorNo
			inc hl
			ld c,(hl)
			inc hl
			ld b,(hl)
			inc hl
			ld e,(hl)
			inc hl
			ld d,(hl)
			push hl
				ld hl,(MseWord)
				bcall(_hdetect)
				pop hl
			jr nz,GUIMOUSE_PUTMSE_CursorNo
			inc hl
			push hl
				pop ix
GUIMOUSE_PUTMSE_CursorNo:
			pop hl
		push hl
			bcall(_ldhlind)
			pop de
		add hl,de
		pop de
	jr GUIMOUSE_PUTMSE_Loop
GUIMOUSE_PUTMSE_CursorDone:
	ld hl,MseY
	ld a,(hl)
	inc a
	jr nz,GUIMOUSE_PUTMSEa
	inc (hl)
GUIMOUSE_PUTMSEa:
	sub 65
	jr nz,GUIMOUSE_PUTMSEb
	dec (hl)
GUIMOUSE_PUTMSEb:
	ld a,(hl)
	ld c,a
	ld b,8
	cp 64-8
	jr c,GUIMOUSE_PUTMSE2
	ld a,64
	sub c
	ld b,a
GUIMOUSE_PUTMSE2:
	ld hl,MseX
	ld a,(hl)
	inc a
	jr nz,GUIMOUSE_PUTMSEc
	inc (hl)
GUIMOUSE_PUTMSEc:
	sub 97
	jr nz,GUIMOUSE_PUTMSEd
	dec (hl)
GUIMOUSE_PUTMSEd:
	ld a,(hl)
	push af
		push bc
			push hl
				cp 89d
				call nc,GUIMouse_Clip
				pop hl
			pop bc
		ld l,c
		pop af
	push af
		push bc
			push hl
				ld e,l
				call imGetPixel
				ld de,IconSpace32b+16
				ld bc,(8*256)+255		;the 'c' components get consumed in the ldi's
GUIMousePutMseSaveLp:
				ldi
				ldi
				;ld a,(hl)
				;ld (de),a
				;inc hl
				;inc de
				;ld a,(hl)
				;ld (de),a
				;inc de
				push de
					;ld de,11
					ld de,10
					add hl,de
					pop de
				djnz GUIMousePutMseSaveLp
				pop hl
			pop bc
		pop af
	call imPutSpriteMask
	jp imFastCopy
GUIMOUSE_LCLICK:
	call GUIMOUSE_CLRMSE2
	ld a,1
	jr GUIMouseEnd
GUIMOUSE_RCLICK:
	call GUIMOUSE_CLRMSE2
	ld a,2
GUIMouseEnd:
	ld (LastClick),a
	ld a,(MseX)
	ld h,a
	ld a,(MseY)
	ld l,a
	push hl
		ld hl,(GUIMouseHookLoc)
		ld a,(hl)
		pop hl
	cp $3f
	jp z,GUIMOUSE_GETKEY			;debounce disabled!
	ld b,16
GUIMouseEndDebounce:
	ld a,0ffh				;reset
	out (1),a
	nop \ nop
	ld a,0bfh				;[CLEAR] key set
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	inc a
	jr nz,GUIMouseEndDebounce
	djnz GUIMouseEndDebounce
	ld b,16
GUIMouseEndDebounce3:			;for alpha key
	ld a,0ffh				;reset
	out (1),a
	nop \ nop
	ld a,0dfh				;alpha group
	out (1),a				;enable
	nop \ nop
	in a,(1)				;get the value
	inc a
	jr nz,GUIMouseEndDebounce3
	djnz GUIMouseEndDebounce3
	ld a,0ffh				;reset
	out (1),a
	nop \ nop
	ret
GUIMouse_Clip:
	push af
		ld hl,(MouseSpriteLoc)
		ld de,IconSpace32b
		push de
			ld bc,16
			ldir
			pop ix
		pop af
	sub 88
	ld b,a
	ld e,$ff
GUIMouse_Clip1:
	sla e
	djnz GUIMouse_Clip1
	ld b,8
	push ix
	pop hl
	push de
		ld a,%11111111
		xor e
		ld e,a
GUIMouse_Clip2:
		ld a,(hl)
		or e
		ld (hl),a
		inc hl
		djnz GUIMouse_Clip2
	pop de
	ld b,8
GUIMouse_Clip3:
	ld a,(hl)
	and e
	ld (hl),a
	inc hl
	djnz GUIMouse_Clip3
	ret

;----------------------------------------------------------------

GUISWinButtonClose:
GUISWinButtonMinimize:
GUISWinButtonMaximize:
GUISWinButtonRestore:
GUISTextLineInClick:
	;	First we need to figure out the x,y coordinates --> col/row offset & byte offset
	;it seems to me that we first need to subtract the box top left x,y from the mouse
	;x,y, then remember to deal with the 1-pixel offset on the text cursor, then set
	;up variables that we need.  What might we need?
	;	>topleft x
	;	>topleft y
	;	>cur x
	;	>cur y
	;	>curoffset (word)
	;	>curtxtstart	(word)
	;I don't think we need to bother with something to show total text length or loc-
	;ation of the first byte after the data, because the text is just a zero-terminated
	;string.
	;	Anyway, once we have that all set up, we should switch to typing mode.  The routine
	;should take the alpha mode from something stored in the appvar - I suppose I'll need to
	;write GUISetAlpha and GUIGetAlpha.  Do typing and such until [2nd] or [F4]/[TRACE] is
	;clicked.  When that occurs, just jp back to the mouse routine.  We might need to pop
	;the $18 and $19 items off the GUIStack, but on second thought, why bother.  Just jump
	;to GUIMouseRun.
	;	Things we need for typing:
	;	>insert char
	;	>switch alpha modes
	;	>delete char - DEL = delete, NOT backspace (7/1/06)
	;	>insert/remove CRLF character
	;	>render current textbox.  Should rely upon the setup of the GUIStack item for this
	;	 textbox somewhere.  Don't forget - no CRLF in this item, but we'll need it for the
	;	 quite similar GUISMultilineClick routine.
	;Oh, and don't forget to scratch the GUISTextLineInType and GUISMultilineType routines.
	;		-11:36 pm, Friday, 6/9/2006.  Kerm Martian, esq.
	;I know, let's use SafeRAM5 for those vars - that gives us 10 bytes total, so 2 spare bytes
	;		-1:12 pm, Saturday, 6/10/2006.  Kerm Martian, esq.
	;[10:51pm - I realized that I need two CurOffset items: one to keep track of the first
	;displayed character's offser. and a second to keep track of the cursor's bytewise offset
	;from the first character onscreen. Since both are words, and I used my extra 2 bytes of that
	;safeRAM for the width and height, I'll have to use another two bytes borrowed from
	;IconSpace32b to make up for it
	;I guess this routine will actually be for singleline, multilinee, and password input.
	;one of the variables will have to be used to keep track of it, just as in the rendering
	;routine. Indeed, it should in fact be the exeact same variable to make it easier to call 
	;into the rendering routine when something is tyoed.
	;		-6:54 am, Friday, 8/4/2006
	;$12 & $09 = singleline %00010010 %00001001 (bit 2 reset)
	;$0F = multiline %00001111 (bit 2 set)
	push bc
	push bc
		
;		ld hl,MseY				;increment mouse X and Y for some reason
;		inc (hl)
;		inc hl
;		inc (hl)
		
		pop hl
	ld (TxtEntrySave),bc
	ld a,1
	ld (GUITextRows),a
	dec bc
	ld a,(bc)
	cp $0F
	jr z,GUISTextInMulti
	cp $12
	jr z,GUISTextInPass
	ld a,2
	ld (linein_render_type),a
	jr GUISTextInCont
GUISTextInMulti:
	ld a,1
	ld (linein_render_type),a
	push bc
		inc bc
		inc bc
		inc bc
		ld a,(bc)
		pop bc
	ld (GUITextRows),a
	inc bc
	jr GUISTextInCont
GUISTextInPass:
	xor a
	ld (linein_render_type),a
GUISTextInCont:			;now it's time to set up the edit buffer
	push bc
		ld hl,GUIavName
		rst 20h				;copy appvar to op1
		bcall(_ChkFindSym)	;data location in de
		pop hl				;de = size byte DAL, hl=offset+start
	or a
	sbc hl,de				;hl = offset
	push hl					;save the offset
		bcall(_EditProg)	;no outputs
		pop de
	ld hl,(iMathPtr1)
	add hl,de
	push hl
		pop bc				;bc = new location of it
	inc bc
	inc bc
	inc bc				;whoops, forgot that :)
	ld a,(bc)
	sub 3
	ld (GUITextWidthPx),a
	pop bc
	call GUIRxy
	ld a,h
	ld (GUITextRX),a
	ld a,l
	ld (GUITextRY),a
	push hl
	pop de
	ld a,1						;not 0: account for border itself
	ld (GUITextCurX),a
	ld (GUITextCurY),a
	inc bc
	inc bc
	ld a,(linein_render_type)
	dec a
	jr z,GUISTextInCont1
	inc bc						;covers all three types since maxchar is a word
GUISTextInCont1:
	ld a,(bc)
	ld l,a
	inc bc
	ld a,(bc)
	ld h,a
	;hl now  = offset from text start
	inc bc
	ld (GUITextScreenOffset),hl
	push bc
	pop hl
	ld (GUITextCurStart),hl
	;now the start is set up too....
	;don't forget that d,e is the top x,y	;done with setup type-specific stuff; now proceed to the type stuff
GUITextInReady:						;the Multiline routine should jump here after setting up the variables.
	ld hl,0
	ld (GUITextCurOffset),hl
	ld hl,(GUITextCurStart)
	ld de,(GUITextScreenOffset)
	add hl,de
GUISTextLineInClickloop:
	ld a,(hl)
	cp $D6
	jp z,GUISTextLineInClickloopCR
	push hl
	or a
	jr z,GUISTextLineInClickloopDone
	call GetVarWidthP

;	pop hl
	;removed the X increment portion of the routine and moved it below,
	;then added the following
	ld a,(GUITextCurX)
	ld b,a
;	push hl

	;do the check now
	;4 checks to do here, 2 x and two y.
	;b already = cur x, d=width
	ld hl,MseX
	ld a,(GUITextRX)
	add a,b
	cp (hl)
	jr c,GUISTextLineInClickloop1
	jr GUISTextLineInClickloopNo
GUISTextLineInClickloop1:
	add a,d
	cp (hl)
	jr c,GUISTextLineInClickloopNo
	ld a,(GUITextRY)
	ld b,a
	ld a,(GUITextCurY)
	add a,b
	dec hl
	cp (hl)
	jr nc,GUISTextLineInClickloopNo
;	jr GUISTextLineInClickloop2
;GUISTextLineInClickloop2:
	add a,6
	cp (hl)
	jr nc,GUISTextLineInClickloopDone
GUISTextLineInClickloopNo:
;VVV moved the following section from above
	ld a,(GUITextCurX)
	add a,d
	ld (GUITextCurX),a
	ld c,a
	ld a,(GUITextWidthPx)
	ld b,a
	ld a,c
	inc b
	inc b
	inc b
	sub b
	call nc,GUISTextLineInClickloopIncLine
	pop hl
	inc hl
	jr GUISTextLineInClickloop
GUISTextLineInClickloopIncLine:
	ld a,(GUITextCurY)
	add a,6
	ld (GUITextCurY),a
	ld a,1
	add a,d
	ld (GUITextCurX),a
	ret
GUISTextLineInClickloopDone:
	pop hl
	ld de,(GUITextScreenOffset)
	or a
	sbc hl,de
	ld de,(GUITextCurStart)
	or a
	sbc hl,de
	ld (GUITextCurOffset),hl
GUISTextLineInType:
	;now type!
	;let's assume that the alpha mode was already set up when the GUI Stack is opened
	;call DrawCursorText
#include "textinr.asm"
GUISTextLineInClickloopCR:
	ld a,(MseY)
	ld c,a
	ld a,(GUITextRY)
	ld b,a
	ld a,(GUITextCurY)
	add a,b
	add a,6
	cp c
	jr nc,GUISTextLineInClickloopCR_Found
	sub b
	ld (GUITextCurY),a
	inc hl									;shouldn't this be a double increment??? 
	inc hl									;<-------- Decision 40 seconds later: yes (08/05/06 9:20pm)
	ld a,1
	ld (GUITextCurX),a
	jp GUISTextLineInClickLoop
GUISTextLineInClickloopCR_Found:
	push hl
	jp GUISTextLineInClickloopDone

DrawCursorText:
	ld a,(GUITextCurY)
	ld b,a
	ld a,(GUITextRY)
	add a,b
	ld l,a
	ld a,(GUITextCurX)
	ld b,a
	ld a,(GUITextRX)
	add a,b
	ld b,5
	ld ix,TextBarCursor
	call imPutSprite
	jp imFastCopy

GUISRadioClick:
	push bc
	inc bc
	inc bc
	ld a,(bc)
	push af
;	call GUISRemoveMouseEntries
;	pop de
;	pop bc
;	push bc
;	push de
	call GUIStackFindTop				;hl = top
	push de
	pop bc
	pop de
GUISRadioClickOCL:
	push hl
	push de
	inc hl
	inc hl
	ld a,(hl)
	cp $0a
	jr nz,GUISRadioClickOCLC
	pop de
	push de
	inc hl
	inc hl
	inc hl
	ld a,(hl)
	cp d
	jr nz,GUISRadioClickOCLC
	inc hl
	ld (hl),0
GUISRadioClickOCLC:
	pop de
	pop hl
	push de
	push hl
	bcall(_ldhlind)
	pop de
	add hl,de
	pop de
	call mos_cphlbc
	jr nz,GUISRadioClickOCL
	pop hl
	inc hl
	inc hl
	inc hl
	ld (hl),1
	jp GUIMouseCallReRender
GUISCheckClick:
	push bc
	;call GUISRemoveMouseEntries
	pop bc
	;bc is set up as the first data byte of the entry
	inc bc
	inc bc
	inc bc
	ld a,(bc)
	or a
	jr z,GUISCheckClick2
	ld a,$ff
GUISCheckClick2:
	inc a
	ld (bc),a
	jp GUIMouseCallReRender
GUISByteInc:
	inc bc
	inc bc
	ld a,(bc)
	push bc
	pop hl
	inc hl
	inc hl
	cp (hl)
	jr z,GUISByteIncDone
	dec hl
	dec hl
	inc a
	ld (hl),a
GUISByteIncDone:
	jp GUIMouseCallReRender
GUISByteDec:
	inc bc
	inc bc
	ld a,(bc)
	push bc
	pop hl
	inc hl
	cp (hl)
	jr z,GUISByteIncDone
	dec hl
	dec a
	ld (hl),a
	jr GUISByteIncDone
GUISWordInc:
	push bc
	pop hl
	inc hl
	inc hl
	push hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	inc hl
	inc hl
	bcall(_ldhlind)
	call mos_cphlde
	pop hl
	jr z,GUISByteIncDone
	inc de
	ld (hl),e
	inc hl
	ld (hl),d
	jr GUISByteIncDone
GUISWordDec:
	push bc
	pop hl
	inc hl
	inc hl
	push hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	bcall(_ldhlind)
	call mos_cphlde
	pop hl
	jr z,GUISByteIncDone
	dec de
	ld (hl),e
	inc hl
	ld (hl),d
	jr GUISByteIncDone
GUISScrollVertUp:
	ld hl,(GUIMouseHookLoc)
	push hl
		push ix
			;bc points to the entry
			push bc
			;	call GUISRemoveMouseEntries
				pop hl
			push hl
			ld de,12
			add hl,de
			bcall(_ldhlind)
			push hl
				pop ix
			pop hl
			ld de,4
			add hl,de
			ld de,ScrollPer
			ld bc,8
			ldir					;now they're all stored
			push hl
				pop bc
			dec bc
			dec bc
			ld hl,(ScrollCur)
			ld de,(ScrollMin)
			call mos_cphlde
			jr z,GUISScrollVertUpRet
			ld de,(ScrollPer)
			or a
			sbc hl,de
			ld a,l
			ld (bc),a
			inc bc
			ld a,h
			ld (bc),a
			dec bc
GUISScrollVertUpRet:
			push ix
				pop hl
			pop ix
		ld de,GUISScrollReturn
		push de
			ld a,h
			or l
			ret z
Callhlscroll:
			jp (hl)
GUISScrollReturn:
		pop hl
	ld (GUIMouseHookLoc),hl
	jp GUIMouseCallRerender
GUISScrollVertDown:
	ld hl,(GUIMouseHookLoc)
	push hl
		push ix
			;bc points to the entry
			push bc
				;call GUISRemoveMouseEntries
				pop hl
			push hl
				ld de,14
				add hl,de
				bcall(_ldhlind)
				push hl
					pop ix
				pop hl
			ld de,4
			add hl,de
			ld de,ScrollPer
			ld bc,8
			ldir					;now they're all stored
			push hl
				pop bc
			dec bc
			dec bc
			ld hl,(ScrollCur)
			ld de,(ScrollPer)
			add hl,de
			ld de,(ScrollMax)
			call mos_cphlde
			jr z,GUISScrollVertDownRet
			ld a,l
			ld (bc),a
			inc bc
			ld a,h
			ld (bc),a
			dec bc
GUISScrollVertDownRet:
			jr GUISScrollVertUpRet

GUIMVectorTable:
	.dw 0
	.dw 0
	.dw 0
	.dw 0
	.dw 0
	.dw GUIMSWinButtons							;COMPLETE	<-- don't even know what I'm going to do about this :/
	.dw 0										;COMPLETE [nothing to click on for this one]
	.dw GUIMSButtonText							;COMPLETE
	.dw GUIMSButtonImg							;COMPLETE
	.dw GUIMSTextLineIn							;COMPLETE
	.dw GUIMSRadio								;COMPLETE
	.dw GUIMSCheckbox							;COMPLETE
	.dw GUIMSByteInt							;COMPLETE	<-- need two hotspots, dec and inc		;x+14/x+18 y+0/y+3/y+6
	.dw GUIMSWordInt							;COMPLETE	<-- ditto for this					;x+22/x+26 y+0/y+3/y+6 
	.dw GUIMSHotspot							;COMPLETE	<-- this one should be really easy
	.dw GUIMSTextMultiLine						;			<-- ARGH this one will be hard			OOOPS!!! forgot this one :D .... STILL 9/12/05
	.dw 0										;COMPLETE [nothing to click on for this one]
	.dw 0										;COMPLETE [nothing to click on for this one]
	.dw GUIMSPassIn								;COMPLETE	<-- should be similar to GUIMSTextLineIn
	.dw GUIMSScrollVert							;COMPLETE	<-- two hotspots for this, dec and inc
	.dw GUIMSScrollHoriz						;COMPLETE	<-- two hotspots for this, dec and inc
	.dw 0										;COMPLETE [nothing to click on for this one]
	.dw 0										;COMPLETE [nothing to click on for this one]
	.dw 0										;COMPLETE [nothing to click on for this one]
	.dw GUIMSMouseCursor									;COMPLETE [nothing to click on for this one]
	.dw 0
	.dw 0

;+-------------------------------------------------------------------------------------------
;| These are just for reference while programming	
;| 	.dw GUIRMouseCursor				;$18							[xy,xy,8-byte mask,8-byte sprite]
;| 	.dw GUIRMouseClick				;$19							[entry type, entry # in top group, onclick ptr,precleanstack(0/1),upperleft [word], lowerright [word]]
;+-------------------------------------------------------------------------------------------
GUIMSMouseCursor:
	ld a,$1a
	push bc
		pop hl
	ld de,20
	call PushGUIStack
	ret
GUIMSWinbuttons:
	push bc
		call RenderGUIGetWinX
		add a,86
		push af
			call RenderGUIGetWinType
			dec a
			add a,a
			add a,a
			add a,a
			add a,a
			ld d,a
			or a
			jr z,GUIMSWinButtons1
			dec d
GUIMSWinButtons1:
			pop af
		sub d
		push af
			call RenderGUIGetWinY
			sub 8
			ld e,a
		;	ld b,7
			pop af
		ld d,a						;de = topleft x,y of first button
		ld hl,GUIStackStage
		ld (hl),$05
		inc hl
		ld a,(ScratchVar)
		ld (hl),a
		inc hl
		pop bc
	inc bc
	inc bc
	inc bc
	inc bc
	inc bc
	inc bc
	ld a,3
GUIMSWinbuttonsLoop:
	push af
		push de
			ld a,(bc)
			ld d,a					;de = onclick
			dec bc
			ld a,(bc)
			dec bc
			ld e,a
			or d
			jr z,GUIMSWinbuttonsLoopPop
			ld hl,GUIStackStage+2
			ld (hl),e
			inc hl
			ld (hl),d
			pop de
		inc hl
		ld (hl),1
		inc hl
		ld (hl),e
		inc hl
		ld (hl),d
		ld a,-7
		add a,d
		ld d,a
		push de
			inc hl
			ld a,e
			add a,6
			ld (hl),a
			inc hl
			ld a,d
			add a,6+8
			ld (hl),a
			ld hl,GUIStackStage
			ld de,9
			ld a,$19
			push bc
				call PushGUIStack
				pop bc
GUIMSWinbuttonsLoopPop:
			pop de
		pop af
	dec a
	jr nz,GUIMSWinbuttonsLoop
	ret

GUIMSButtonText:				;[x,y,onclick {word},zt text]
	call GUIRxy

	ld de,GUIStackStage
	push de
	ex de,hl
	ld (hl),$07
	call GUIMS_ScratchVarBC1ED_ToHL
	push hl
GUIMSButtonTextWidthLoop:
	ld a,(bc)
	or a
	jr z,GUIMSButtonTextWidthDone
	push de
	push bc
	call getVarWidth
	ld a,d
	pop bc
	inc bc
	pop de
	add a,d
	ld d,a
	jr GUIMSButtonTextWidthLoop
GUIMSButtonTextWidthDone:
	pop hl
	ld a,e
	add a,6
	ld (hl),a
	ld a,d
	add a,4
	inc hl
	ld (hl),a
	pop hl
	ld de,9
	ld a,$19
	jp PushGUIStack
	;ret

GUIMS_ScratchVarBC1ED_ToHL:
	inc hl
	ld a,(ScratchVar)
	ld (hl),a
	inc hl
	ld a,(bc)
	ld (hl),a
	inc hl
	inc bc
	ld a,(bc)
	ld (hl),a
	inc hl
	ld (hl),1						;precleanstack = yes plz
	inc hl
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	inc bc							;one place doesn't need this, but it pops bc, so no worries
	ret

GUIMSButtonImg:					;[x,y,onclick {word},img width [BYTES],button width [PX], img data (5 rows high)]
	call GUIRxy
	ld de,GUIStackStage
	push de
	ex de,hl
	ld (hl),$08
	call GUIMS_ScratchVarBC1ED_ToHL
	ld a,e
	add a,6
	ld (hl),a
	ld a,(bc)
	add a,4
	add a,d			;ERM.....
	inc hl
	ld (hl),a
	pop hl
	ld de,9
	ld a,$19
	jp PushGUIStack
	;ret
GUIMSTextLineIn:			;[x,y,width,maxchar,curdatapos (0 to datalength) [word],data (z.t.)]
	call GUIRxy
	push hl
	ld de,GUIStackStage
	push de
	push de
	ex de,hl
	ld (hl),$09
GUIMSSLI:
	inc hl
	ld a,(ScratchVar)
	ld (hl),a
	inc hl
	push de
	ld de,GUISTextLineInClick
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	pop de
	ld (hl),0							;preclean stack = no
	inc hl
	inc e
	ld (hl),e
	dec e
	inc hl
	ld (hl),d
	inc hl
	ld a,e
	add a,6
	ld (hl),a
	ld e,a
	ld a,(bc)
	add a,d
	inc hl
	ld (hl),a
	ld d,a
	pop hl
	push de
	ld de,9
	ld a,$19
	call PushGUIStack
	pop de
	pop hl
	pop bc
	push hl
	ld (hl),c
	inc hl
	ld (hl),b
	inc hl
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	ld de,TextCursor
	ld bc,16
	ex de,hl
	ldir
	pop hl
	ld de,4+8+8
	ld a,$18
	jp PushGUIStack
	;ret
GUIMSRadio:
	call GUIRxy			;this sets up hl
	ld de,GUIStackStage
	push de
	ex de,hl
	ld (hl),$0A
	inc hl
	ld a,(ScratchVar)		;what do we have stored here???
	ld (hl),a
	push de					;to save top x,y
	ld de,GUISRadioClick
	jr GUIMSCheckRadioSet
GUIMSCheckbox:
	call GUIRxy			;this sets up hl
	ld de,GUIStackStage
	push de
	ex de,hl
	ld (hl),$0B
	inc hl
	ld a,(ScratchVar)		;what do we have stored here???
	ld (hl),a
	push de					;to save top x,y
	ld de,GUISCheckClick
GUIMSCheckRadioSet:
	inc hl					;hehe.... oops. :D
	ld (hl),e
	inc hl
	ld (hl),d				;this should work here
	pop de					;recall top x,y
	inc hl
	ld (hl),1				;preclean = yes
	inc hl
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	ld a,e
	add a,5
	ld (hl),a
	ld a,d
	add a,5
	inc hl
	ld (hl),a
	pop hl
	ld de,9
	ld a,$19
	jp PushGUIStack
	;ret
GUIMSByteInt:				;<-- need two hotspots, dec and inc		;x+14/x+18 y+0/y+3/y+6
	call GUIRxy
	ld de,GUIStackStage
	push de
	ex de,hl
	ld (hl),$0C
	inc hl
	ld a,(ScratchVar)
	ld (hl),a
	inc hl
	push de
	ld de,GUISByteInc
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	ld (hl),1
	inc hl
	pop de
	ld (hl),e
	inc hl
	ld a,14
	add a,d
	ld (hl),a
	inc hl
	inc e
	inc e
	inc e
	ld (hl),e
	inc hl
	add a,4
	ld (hl),a
	pop hl
	push hl
	push de
	ld de,9
	ld a,$19
	call PushGUIStack
	ld hl,GUIStackStage+2
	ld de,GUISByteDec
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	inc hl
	pop de
	ld (hl),e
	inc hl
	inc hl
	inc e
	inc e
	inc e
	ld (hl),e
	pop hl
	ld de,9
	ld a,$19
	jp PushGUIStack
GUIMSWordInt:
	call GUIRxy
	ld de,GUIStackStage
	push de
	ex de,hl
	ld (hl),$0D
	inc hl
	ld a,(ScratchVar)
	ld (hl),a
	inc hl
	push de
	ld de,GUISWordInc
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	ld (hl),1
	inc hl
	pop de
	ld (hl),e
	inc hl
	ld a,22
	add a,d
	ld (hl),a
	inc hl
	inc e
	inc e
	inc e
	ld (hl),e
	inc hl
	add a,4
	ld (hl),a
	pop hl
	push hl
	push de
	ld de,9
	ld a,$19
	call PushGUIStack
	ld hl,GUIStackStage+2
	ld de,GUISWordDec
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	inc hl
	pop de
	ld (hl),e
	inc hl
	inc hl
	inc e
	inc e
	inc e
	ld (hl),e
	pop hl
	ld de,9
	ld a,$19
	jp PushGUIStack
GUIMSHotspot:
	call GUIRxy
	ld de,GUIStackStage
	push de
	push bc
	ex de,hl
	ld (hl),$0E
	inc bc
	inc bc
	call GUIMS_ScratchVarBC1ED_ToHL
	pop bc
	ld a,(bc)
	add a,d
	ld d,a
	inc bc
	ld a,(bc)
	add a,e
	ld (hl),a
	inc hl
	ld (hl),d
	pop hl
	ld de,9
	ld a,$19
	jp PushGUIStack
GUIMSTextMultiLine:
;	[x,y,rows,width,curdatapos (0 to datalength) [word],data]
	call GUIRxy
	push hl
		ld de,GUIStackStage
		push de
			push de
				ex de,hl
				ld (hl),$0F
				inc hl
				ld a,(ScratchVar)		;what do we have stored here???
				ld (hl),a
				push de					;to save top x,y
					ld de,GUISTextLineInClick
					inc hl					;hehe.... oops. :D
					ld (hl),e
					inc hl
					ld (hl),d				;this should work here
					pop de					;recall top x,y
				inc hl
				ld (hl),0							;preclean stack = no
				inc hl
				inc e
				ld (hl),e
				dec e
				inc hl
				ld (hl),d
				inc hl
				ld a,(bc)
				push bc
					ld b,a
					ld a,e
GUIMSTextMultiLineR:
					add a,6
					djnz GUIMSTextMultiLineR
					pop bc
				inc bc
				ld (hl),a
				ld e,a
				ld a,(bc)
				add a,d
				inc hl
				ld (hl),a
				ld d,a
				pop hl
			push de
				ld de,9
				ld a,$19
				call PushGUIStack
				pop de
			pop hl
		pop bc
	push hl
		ld (hl),c
		inc hl
		ld (hl),b
		inc hl
		ld (hl),e
		inc hl
		ld (hl),d
		inc hl
		ld de,TextCursor
		ld bc,16
		ex de,hl
		ldir
		pop hl
	ld de,4+8+8
	ld a,$18
	jp PushGUIStack
	;ret
GUIMSPassIn:
	call GUIRxy
	push hl
	ld de,GUIStackStage
	push de
	push de
	ex de,hl
	ld (hl),$12
	jp GUIMSSLI
GUIMSScrollVert:								;$13			COMPLETE	XY	[x,y,height,ID,per,min,max,cur,onScrollUp,onScrollDown]
	call GUIRxy									;[entry type, top group entry #, onclick ptr,precleanstack(0/1),upperleft[w], lowerright [w]]
	push bc										;at height
	ld de,GUIStackStage
	push de
	ex de,hl
	ld (hl),$13
	inc hl
	ld a,(ScratchVar)		;what do we have stored here???
	ld (hl),a
	push de					;to save top x,y
	ld de,GUISScrollVertUp
	inc hl					;hehe.... oops. :D
	ld (hl),e
	inc hl
	ld (hl),d				;this should work here
	pop de					;recall top x,y
	push de
	inc hl
	ld (hl),1				;preclean=yes
	inc hl
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	ld a,e
	add a,6
	ld (hl),a				;y-val, need to EXPAND
	ld a,d
	add a,5
	inc hl
	ld (hl),a
	pop de
	pop hl
	push de
	ld de,9
	ld a,$19
	call PushGUIStack
	ld hl,GUIStackStage+2
	ld de,GUISScrollVertDown
	ld (hl),e
	inc hl
	ld (hl),d
	pop de
	pop hl
	ld a,(hl)
	add a,e
	add a,5					;y-val, need to EXPAND (lower)
	ld hl,GUIStackStage+5
	ld (hl),a
	inc hl
	inc hl
	add a,5
	ld (hl),a
	ld hl,GUIStackStage
	ld de,9
	ld a,$19
	jp PushGUIStack
GUIMSScrollHoriz:
	call GUIRxy									;[entry type, top group entry #, onclick ptr,precleanstack(0/1),upperleft[w], lowerright [w]]
	push bc										;at height
	ld de,GUIStackStage
	push de
	ex de,hl
	ld (hl),$13
	inc hl
	ld a,(ScratchVar)		;what do we have stored here???
	ld (hl),a
	push de					;to save top x,y
	ld de,GUISScrollVertUp
	inc hl					;hehe.... oops. :D
	ld (hl),e
	inc hl
	ld (hl),d				;this should work here
	pop de					;recall top x,y
	push de
	inc hl
	ld (hl),1				;preclean = yes
	inc hl
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	ld a,e
	add a,5
	ld (hl),a
	ld a,d
	add a,6
	inc hl
	ld (hl),a
	pop de
	pop hl
	push de
	ld de,9
	ld a,$19
	call PushGUIStack
	ld hl,GUIStackStage+2
	ld de,GUISScrollVertDown
	ld (hl),e
	inc hl
	ld (hl),d
	pop de
	pop hl
	ld a,(hl)
	add a,d
	add a,6
	ld hl,GUIStackStage+6
	ld (hl),a
	inc hl
	inc hl
	add a,6
	ld (hl),a
	ld hl,GUIStackStage
	ld de,9
	ld a,$19
	jp PushGUIStack
TextBarCursor:
	.db $80,$80,$80,$80,$80
