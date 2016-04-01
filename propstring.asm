;-----------------------------------------------------------
;	Filename:		propstring.asm
;	Long name:  	Properties string input
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	Unknown
;
; This file contains the String input segment
; There's also a set of cross-page hacks to make
; the rest of the Properties menu work
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

#ifdef false
PropGUIClick1:
	ld b,1
	jr PropGUIFinish
PropGUIClick2:
	ld b,2
	jr PropGUIFinish
PropGUIClick3:
	ld b,3
	jr PropGUIFinish
PropGUIClick4:
	ld b,4
	jr PropGUIFinish
PropGUIClick5:
	ld b,5
	jr PropGUIFinish
PropGUIClick6:
	ld b,6
	jr PropGUIFinish
PropGUIClick7:
	ld b,7
	jr PropGUIFinish
PropGUIClick8:
	ld b,8
	jr PropGUIFinish
PropGUIClick9:
	ld b,9
	jr PropGUIFinish
PropGUIClick10:
	ld b,10
	jr PropGUIFinish
PropGUIClose:
	ld b,0
PropGUIFinish:
	bcall(_PropMenuReturn)
#endif

PropString:
	push ix
	call OpenGUIStack
	ld hl,Prop_SmWin
	call PushGUIStacks
	ld hl,0
	jp GUIMouseCall
PropStringCancel:
	pop ix
	call CloseGUIStack
	xor a
	ret
PropStringGo:
	call GUIFindFirst
	call GUIFindNext
	ld de,10
	add hl,de
	ld a,(hl)
	or a
	jr z,PropStringCancel
	pop de
	ld b,8
	ld c,0
PropStringGoLoop:
	ld a,(hl)
	ld (de),a
	inc hl
	inc de
	or a
	jr z,PropStringGoDone
	inc c
	djnz PropStringGoLoop
PropStringGoDone:
	ld a,c
	push af
	bcall(_PushOP1)
	call CloseGUIStack
	bcall(_PopOP1)
	pop af
	ret