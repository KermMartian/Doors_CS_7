;-----------------------------------------------------------
;	Filename:		GUItools.inc
;	Long name:  	GUI Rendering Routines
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	June 3, 2006
;	Routines Included:
;		-RenderGUI
;		-PushGUIStack
;		-PushGUIStacks
;		-PopGUIStack
;		-PopGUIStacks
;		-OpenGUIStack
;		-CloseGUIStack
;		-GUIRnull
;		-GUIRLargeWin
;		-GUIRSmallWin
;		-GUIRFullScreenImg
;		-GUIRText
;		-GUIRWinButtons
;		-GUIRWrappedText
;		-GUIRButtonText
;		-GUIRButtonImg
;		-GUIRTextLineIn
;		-GUIRRadio
;		-GUIRCheckbox
;		-GUIRByteInt
;		-GUIRWordInt
;		-GUIRHotspot
;		-GUIRTextMultiline
;		-GUIRSprite
;		-GUIRLargeSprite
;		-GUIRPassIn
;		-GUIRScrollVert
;		-GUIRScrollHoriz
;		-GUIRBorder
;		-GUIRRect
;		-GUIRCustom
;		-GUIDrawHourglass
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

;+-----------------------------------------
;| GUI Stack Format:
;| _every_ element has this format:
;|   --size (including 2 bytes for size) - 2 bytes
;|   --type 							 - 1 byte
;|   --data								 - [size-3] bytes
;| structural format:
;|  --group definition
;|   -- group item
;|   -- group item
;|   -- group item
;|   -- group item
;|  --group definition
;|   -- group item
;|  --group definition
;|   -- group item
;|   -- group item
;|
;+------------------------------------------
;
RenderGUI:
	call RenderGUISub
	push bc
		call imFastCopy
		pop bc
	ret
RenderGUISub:
	;This routine starts from the bottom of the GUI stack and goes from low to high memory.
	;It essentially just ignores the groupings of the stack and renders each item individually.
	;This is different from the setup of the "mouse" routine, which only uses the items in
	;the top group on the stack.
	;What we need to do here is take elements off the stack, then pass it to any of a few routines based on the type definition.
	;set things up with flags and clearing the buffer

	in a,(20h)				;calc speed port
	push af
		in a, (2)
		rla
		sbc a, a
		out (20h), a		;set fast speed

		set textWrite,(iy+textflags)
		;First, initialize a pointer to the the AppVar
		call OpenGUIStack
		ld a,c
		ld c,1				;therefore error (c) will be set if it is an empty stack
		or a
		jr z,RenderGUIExitFast			;quit if for some reason this is the first time the AppVar was created - this indicates an empty stack
		push hl
			dec hl
			dec hl
			bcall(_ldhlind)
			pop de
		add hl,de
		ex de,hl
RenderGUILoop:
		push de
			push hl
				bcall(_ldhlind)
				pop de
			ex de,hl				;hl=where,de=entry size
			push hl					;current_start; first_byte_after_end
				push de					;entry_size; current_start; first_byte_after_end
					dec de
					dec de
					dec de
					inc hl
					inc hl
					ld a,(hl)
					inc hl					;now hl is first byte, de is data length, and a is type
					push hl
						pop bc
					ld hl,GUIVectorTable
					ld e,a
					ld d,0
					add hl,de
					add hl,de				;each entry is _two_ bytes (1 word)
					push af
						bcall(_ldhlind)
						pop af
					ld ix,RenderGUILoopReturn
					push ix
						jp (hl)					;bc is first byte, de is data length, a is type
RenderGUILoopReturn:
					pop de
				pop hl
			add hl,de
			pop de
		bcall(_cphlde)
		jr nz,RenderGUILoop
		res TextWrite,(iy+textflags)
		ld c,0
RenderGUIExitFast:
		pop af
	out (20h),a				;restore "speed" (if this is an 84)
	ret
RenderGUIAVSet:
	ld hl,AppVarName2
	rst 20h
	bcall(_chkfindsym)
	jr nc,RGAVExists
	ld hl,32
	bcall(_createappvar)
	jr RenderGUIAVSet
RGAVExists:
	inc de
	inc de
	ex de,hl
	ld de,AVOff_GUIWinType						;ProgNameSpace, ProgNameSpace2, SafeArc[null]/2/3, CurGUIType
	add hl,de
	ret
RenderGUISetWinX:
	ld de,1
	jr RenderGUISetWin
RenderGUISetWinY:
	ld de,2
RenderGUISetWin:
	push de
	push af
	call RenderGUIAVSet
	pop af
	pop de
	add hl,de
	ld (hl),a
	ret
RenderGUIGetWinX:
	push bc
	push de
	ld de,1
	jr RenderGUIGetWin
RenderGUIGetWinY:
	push bc
	push de
	ld de,2
RenderGUIGetWin:
	push de
	call RenderGUIAVSet
	pop de
	add hl,de
	ld a,(hl)
	pop de
	pop bc
	ret
RenderGUISetWinType:
	push hl
	push de
	push af
	call RenderGUIAVSet
	pop af
	ld (hl),a
	pop de
	pop hl
	ret
RenderGUIGetWinType:
	push hl
	push de
	push af
	call RenderGUIAVSet
	pop af
	ld a,(hl)
	pop de
	pop hl
	ret	
OpenGUIStack:
	push af
		ld hl,GUIavName
		rst 20h
		bcall(_chkfindsym)
		pop bc
	ld c,1
	jr nc,GSExists
	ld hl,0
	bcall(_createappvar)				;hl = VAT, de = data
#ifdef guiswap_VATtop
	ld a,b
	cp $C7
	jr nz,OpenGUIStack_SkipSwap
	push de
		ld de,Op1+10				;11 bytes to load
		push de
			push hl
				ld bc,11
				lddr						;will leave hl at ^'7iug'4etc and de at Op1-1
				pop de
			inc hl
			push hl
				ld hl,(progptr)
				or a
				sbc hl,de
				push hl
					pop bc
				pop hl
			inc de
			ex de,hl
			ldir							;hl ends at (progptr)+1
			dec hl
			pop de
		ld bc,11
		ex de,hl
		lddr
		pop de
OpenGUIStack_SkipSwap:
#endif
	ld c,0
GSExists:
	push de
		ex de,hl
		bcall(_ldhlind)
		pop de
	inc de
	inc de
	ex de,hl
	ret
CloseGUIStack:
	ld hl,GUIavName
	rst 20h
	bcall(_chkfindsym)
	jr c,GSDNExists
	push de
		ex de,hl
		bcall(_ldhlind)
		push hl
			bcall(_chkfindsym)
			bcall(_delvar)
			pop hl
		pop de
	ld c,0
	ret
GSDNExists:
	ld c,1
	ret
PGS_NMRAM:
	pop de
	pop hl
	pop af
	bcall(_grbufclr)
	ld hl,0
	ld (pencol),hl
	ld hl,NMRAM_Message
	bcall(_vputsapp)
	call imfastcopy
	bcall(_Pause)

	ld hl,$0000
	add hl,sp
	push hl
		bcall(_ldhlind)
		ex de,hl
		ld hl,AVOff_SPSave
		bcall(_DAVLCheckOffset)
		bcall(_ldhlind)
;		ex de,hl
;		pop hl
;	ex de,hl
		pop de
	or a
	sbc hl,de
StackStripLPGS:
	pop de
	dec hl
	dec hl					;each stack entry is *TWO* bytes!!!!
	ld a,h
	or l
	jr nz,StackStripLPGS
	ld hl,guiavname
	rst 20h
	bcall(_chkfindsym)
	bcallnc(_delvar)

	ld hl,69
	bcall(_DAVLCheckOffset)
	ld a,(hl)
	or a
	jr nz,PGS_NMRAM_PR

	bcall(_QuitToOS)
	
PGS_NMRAM_PR:
	bcall(_hook1retfromp2)


PushGUIStack:
;---------------------------------------
;What we want to do here is three-fold:
; 1. we want to init a handle into the AppVar and read to the end of it
; 2. we want to get the size of the data to add and add it to the end +3 (size and type)
; 3. We want to update the AppVar size in the file and in the VAT
;Inputs:
; --a  = type
; --hl = data ptr
; --de = data length
;---------------------------------------
	push af								;a = type
		push hl								;save read-from location
			push de								;(de = size) - remember to size-check DE with _enoughMem!
				ex de,hl
				inc hl
				inc hl
				inc hl
				push hl
					bcall(_freeRAM)					;bytes of free RAM
					pop de
				call mos_cphlde
				jp c,PGS_NMRAM
				call OpenGUIStack					;check for creation & get pointers
				pop bc								;bc now = size
			push bc								;save size again
				inc bc								;account for two size bytes...
				inc bc
				inc bc								;...and one type byte
				push hl								;save datastart of AppVar
					dec hl
					dec hl								;go down to start of AppVar size
					push hl
						ld a,(hl)
						inc hl
						ld h,(hl)
						ld l,a							;was bcall(_ldhlind)
						push hl
							add hl,bc							;hl = first byte after end of AppVar
							;push hl
							;	pop de
							pop bc
						pop de							;de is LSB of size byte, hl is new size
					ex de,hl							;switch it
					ld (hl),e
					inc hl
					ld (hl),d							;appvar size has now been updated
					pop hl								;recall AppVar data start
				add hl,bc
				pop de								;these three are the initial function arguments
			push de
				inc de
				inc de
				inc de
				ex de,hl
				bcall(_insertmem)
				pop bc								;this is now the size
			inc bc
			inc bc
			inc bc
			ex de,hl
			ld (hl),c
			inc hl
			ld (hl),b
			inc hl
			pop de
		pop af
	ld (hl),a
	inc hl
	push hl
	ex de,hl
		dec bc
		dec bc
		dec bc
		ldir
		pop hl
	ld c,0
	ret												;returns hl = first non-size byte of new element

PushGUIStacks:
;1. First, figure out total size of this chunk
	push hl
		ld bc,0
PushGUIStacks_CountLoop:
		ld e,(hl)
		inc hl				;past SizeBL to SizeBH
		ld d,(hl)
		ld a,d
		inc a
		jr z,PushGUIStacks_Finish
		inc hl
		push hl
			push bc
				pop hl
			add hl,de
			push hl
				pop bc
			pop hl
		add hl,de
		dec hl
		dec hl
		jr PushGUIStacks_CountLoop
;2. Now bc is the size, so we'll do a trick
PushGUIStacks_Finish:
;		pop hl
		push bc
			pop de
	;	push hl
		push de
			ex de,hl
			push bc
				push hl
					bcall(_freeRAM)
					pop de
				pop bc
			call mos_cphlde
			jp c,PGS_NMRAM
			call OpenGUIStack					;check for creation & get pointers
			pop bc								;bc now = size
		push bc								;save size again
			push hl								;save datastart of AppVar
				dec hl
				dec hl								;go down to start of AppVar size
				push hl
					ld a,(hl)
					inc hl
					ld h,(hl)
					ld l,a							;was bcall(_ldhlind)
					push hl
						add hl,bc							;hl = first byte after end of AppVar
						;push hl
						;	pop de
						pop bc
					pop de							;de is LSB of size byte, hl is new size
				ex de,hl							;switch it
				ld (hl),e
				inc hl
				ld (hl),d							;appvar size has now been updated
				pop hl								;recall AppVar data start
			add hl,bc
			pop de								;these three are the initial function arguments
		push de
			ex de,hl
			bcall(_insertmem)					;returns de=insertion address
			pop bc								;this is now the size
		pop hl									;recall source pointer
	ldir
	ld c,0
	ret

PopGUIStackFast:						;take hl = appvar
	push hl
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		pop de
	inc de
	inc de
	jr PopGUIStack_NoOpenGUIStack
PopGUIStack:
	call OpenGUIStack
	ex de,hl							;de=start, hl=length
PopGUIStack_NoOpenGUIStack:
	add hl,de							;de=start, hl=end
	ex de,hl							;hl=start, de=end
	push de
PopGUIStackRLoop:
		push hl
			push hl
				bcall(_ldhlind)
				pop de
			add hl,de
			pop bc							;next-to-last value
		pop de							;start + size
	push de
		call mos_cphlde
		jr nz,PopGUIStackRLoop
		pop hl
	push bc
		pop hl
	push hl
		bcall(_ldhlind)
		pop de
	push hl
		push de
			push hl
				ex de,hl							;hl=where,de=how much to remove
				bcall(_delmem)
				call OpenGUIStack
				dec hl
				dec hl
				pop de
			push hl
				push de
					bcall(_ldhlind)
					or a
					pop de
				sbc hl,de
				pop de
			ex de,hl
			ld (hl),e
			inc hl
			ld (hl),d
			pop hl
		pop bc
	ret
	
PopGUIStacks:
	push bc
		call OpenGUIStack
		dec hl
		dec hl
		pop bc
PopGUIStacks_L:
	push bc
		push hl
			call PopGUIStackFast
			pop hl
		pop bc
	djnz PopGUIStacks_L
	ret

GUIStackFindTop:			;inputs: none
	call OpenGUIStack
	ld a,c
	inc c					;changed from ld c,1 --> if c=0, then there's an error
	or a
	ret z				;quit if for some reason this is the first time the AppVar was created - this indicates an empty stack
	push hl
		dec hl
		dec hl
		bcall(_ldhlind)
		pop de
	add hl,de
	ex de,hl
GUIStackFindTopLoop:
	push de
	push hl
		bcall(_ldhlind)
		pop de
	ex de,hl				;hl=where,de=entry size
	push hl					;current_start; first_byte_after_end
	push de					;entry_size; current_start; first_byte_after_end
	dec de
	dec de
	dec de
	inc hl
	inc hl
	ld a,(hl)
	cp 3
	jr nc,GUIStackFindNotG
	ld (scratchword),hl
GUIStackFindNotG:
	pop de
	pop hl
	add hl,de
	pop de
	call mos_cphlde
	jr nz,GUIStackFindTopLoop
	; now (ScratchWord) is the group start
	ld hl,(scratchword)
	dec hl
	dec hl
	push de
	push hl
	bcall(_ldhlind)
	ex de,hl
	pop hl
	add hl,de
	pop de
	push de
	pop bc
	inc bc
	inc bc
	ret
	;outputs:
	;c = error.  0=none, 1=error
	;hl=top first entry
	;de=final end
GUIavName:
	.db 15h,"gui7",0
AppVarName2:
	.db 15h,"DCS7",0
GUIVectorTable:
	.dw GUIRnull					;$00			COMPLETE	XY	[none]
	.dw GUIRLargeWin				;$01			COMPLETE	--	[5 byte icon, header text]
	.dw GUIRSmallWin				;$02			COMPLETE	XY	[x,y, 5 byte icon, header text]
	.dw GUIRFullScreenImg			;$03			COMPLETE	XY	[768 bytes]
	.dw GUIRText					;$04			COMPLETE	XY	[x,y,font,z.t. text]
	.dw GUIRWinButtons				;$05			COMPLETE	--	[which {-,[],X,00000}, onclick[-] {word}, onclick[] {word}, onclick[X] {word}]
	.dw GUIRWrappedText				;$06			COMPLETE	XY	[x,y,width,font,z.t. text]
	.dw GUIRButtonText				;$07			COMPLETE	XY	[x,y,onclick {word},zt text]
	.dw GUIRButtonImg				;$08			COMPLETE	XY	[x,y,onclick {word},img width [BYTES],button width [PX], img data (5 rows high)]
	.dw GUIRTextLineIn				;$09			COMPLETE	XY	[x,y,width,maxchar [word],curdatapos (0 to datalength) [word],data (z.t.)]
	.dw GUIRRadio					;$0A			COMPLETE	XY	[x,y,group,state {bool},zttext]  1 per group
	.dw GUIRCheckbox				;$0B			COMPLETE	XY	[x,y,group,state {bool},zttext] ++ per group
	.dw GUIRByteInt					;$0C			COMPLETE	XY	[x,y,value,min,max]
	.dw GUIRWordInt					;$0D			COMPLETE	XY	[x,y,value,min,max]
	.dw GUIRHotspot					;$0E			COMPLETE	XY	[x,y,width,height,ptr]
	.dw GUIRTextMultiline			;$0F			COMPLETE	XY	[x,y,rows,width,curdatapos (0 to datalength) [word],data]
	.dw GUIRSprite					;$10			COMPLETE	XY	[x,y,b,data]
	.dw GUIRLargeSprite				;$11			COMPLETE	XY	[x,y,a,b,data]
	.dw GUIRPassIn					;$12			COMPLETE	XY	[x,y,width,maxchar [word],curdatapos (0 to datalength) [word],data (z.t.)]
	.dw GUIRScrollVert				;$13			COMPLETE	XY	[x,y,height,ID,per,min,max,cur,onScrollUp,onScrollDown]
	.dw GUIRScrollHoriz				;$14			COMPLETE	XY	[x,y,width,ID,per,min,max,cur,onScrollUp,onScrollDown]
	.dw GUIRBorder					;$15			COMPLETE	XY	[x,y,width,height,color]
	.dw GUIRRect					;$16			COMPLETE	XY	[x,y,width,height,fill]
	.dw GUIRCustom					;$17			COMPLETE	--	[ptr_to_routine]
	.dw GUIRMouseCursor				;$18			COMPLETE	--  [xy,xy,8-byte mask,8-byte sprite]
	.dw GUIRMouseClick				;$19			COMPLETE	--  [entry type, top group entry #, onclick ptr,precleanstack(0/1),upperleft[w], lowerright [w]]
	;.dw GUIRHotKey					;$1A						--  [count,group,key,where,[group....]]
GUIRNull:
	push bc
		call RenderGUISetWinType
		xor a
		call RenderGUISetWinX
		xor a
		call RenderGUISetWinY
		pop hl
	ld a,(hl)
	inc a
	ret nz
	bcall(_grbufclr)		;clear if it's an $FF
	ret
GUIRLargeWin:
	push bc
	call RenderGUISetWinType
	ld a,1
	call RenderGUISetWinX
	ld a,9
	call RenderGUISetWinY
	ld hl,gbuf
	ld (hl),0
	push hl
		push hl
			pop de
		inc de
		ld bc,768-1
		ldir
		ld b,8*12
		pop hl
	ld b,64-8
	push hl
		ld de,8*12
		add hl,de
		ld de,11
GUIRLargeWinLoop1:
		ld (hl),$80
		add hl,de
		ld (hl),$01
		inc hl
		djnz GUIRLargeWinLoop1
		pop hl
	ld b,10
	ld (hl),$7f
	inc hl
GUIRLargeWinLoop2:
	ld (hl),$FF
	inc hl
	djnz GUIRLargeWinLoop2
	ld (hl),$fe
	ld b,12
	ld hl,PlotsScreen+(8*12)
GUIRLargeWinLoop3:
	ld (hl),$FF
	inc hl
	djnz GUIRLargeWinLoop3
	ld b,10
	ld hl,PlotsScreen+(63*12)
	ld (hl),$7f
	inc hl
GUIRLargeWinLoop4:
	ld (hl),$FF
	inc hl
	djnz GUIRLargeWinLoop4
	ld (hl),$fe
	pop ix
	push ix
	ld a,2
	;ld b,5
	;ld c,1
	ld bc,(5*256)+1
	ld l,a
	call imPutSprite
	ld hl,(1*256)+8
	ld (pencol),hl
	pop hl
	ld de,5
	add hl,de
	;ld a,8
	;ld (pencol),a
	;ld a,1
	;ld (penrow),a
	bcall(_vPutsApp)
	ld hl,gbuf+12
	ld b,12*7
GUIRLargeWinInvLoop:
	ld a,(hl)
	cpl
	ld (hl),a
	inc hl
	djnz GUIRLargeWinInvLoop
	ret
GUIRSmallWin:
	push bc
	call RenderGUISetWinType
	pop bc
	;inc bc
	ld a,(bc)
	push af
	push bc
	inc a
	call RenderGUISetWinX
	pop bc
	inc bc
	ld a,(bc)
	pop hl
	ld l,a
	push bc
	push hl
	add a,9
	call RenderGUISetWinY
	pop hl
	;now erase the window area
	;push hl
		ld a,79				;80
		add a,h
		ld d,a
		ld a,47				;48
		add a,l
		ld e,a
		xor a	;ld a,0
		call mos_filledrectangle_save
	;	call DrawCustomRectangle
		;pop hl
	push hl
	ld b,47
	inc l
	call VertLine
	pop hl
	push hl
		ld b,47
		inc l
		ld a,80
		add a,h
		ld h,a
		call VertLine
		pop hl
	push hl
		inc h
		ld a,l
		add a,8					;was 9
		ld e,a
		ld a,h
		add a,78				;was 79
		ld d,a
		ld a,1
		call mos_filledrectangle_save
		pop hl
	push hl
		ld a,l
		add a,48
		ld l,a
		ld e,a
		inc h
		ld a,h
		add a,79
		ld d,a
		;ld a,1
		call api_drawline_set_ld1
		pop de
	pop hl
	inc hl
	push hl
		push de
			push hl
				pop ix
			ld a,d
			inc a
			inc a
			ld l,e
			inc l
			inc l
			ld b,5
			call imPutSprite
			pop hl
		
		ld a,h
		add a,8
		ld (pencol),a
		ld a,l
		inc a
		ld (penrow),a
		
		pop bc
	ld hl,5
	add hl,bc
	bcall(_vPutsApp)
	ret
GUIRFullScreenImg:
	push bc
		call RenderGUIGetWinType
		pop hl
	or a
	ret nz					;quit if it's not a fullscreened window
	ld de,gbuf
	ld bc,768
	ldir
	ret
GUIRText:					;x,y,big/small,data
	ld a,(bc)
	ld d,a
	call RenderGUIGetWinX
	add a,d
	ld (pencol),a
;	ld (curcol),a
	inc bc
	ld a,(bc)
	ld d,a
	call RenderGUIGetWinY
	add a,d
	ld (penrow),a
;	ld (currow),a
	inc bc
	ld a,(bc)
	inc bc
	push bc
	pop hl
	or a
	jr nz,GUIRTextBig
	bcall(_vPutsApp)
	ret
GUIRTextBig:
	ld a,(pencol)
	srl a \ srl a \ srl a
	ld (curcol),a
	ld a,(penrow)
	srl a \ srl a \ srl a
	ld (currow),a
	bcall(_PutsApp)
	ret
GUIRWinButtons:
	push bc
	call RenderGUIGetWinX
	add a,93
	push af
	call RenderGUIGetWinType
	dec a
	add a,a
	add a,a
	add a,a
	add a,a
	ld d,a
	or a
	jr z,GUIRWinButtons1
	dec d
GUIRWinButtons1:
	pop af
	sub d
	push af
	call RenderGUIGetWinY
	sub 8
	ld l,a
	ld b,7
	ld ix,WinButtonRight
	pop af
	push af
	push hl
	call imPutSprite
	pop hl
	pop af
	sub 6
	ld h,a
	pop bc
	ld a,(bc)
	;hl now = (h,l) = (x,y)
	ld ix,WinButtonX
	ld b,3
GUIRWinButtonsDo:
	push bc
	push af
	push ix
	push hl
	bit 5,a
	jr z,GUIRWinButtons2
	ld b,7
	ld a,h
	call imPutSprite
	pop hl
	ld a,h
	sub 6
	ld h,a
	push hl
GUIRWinButtons2:
	pop hl
	pop ix
	ld de,7
	add ix,de
	pop af
	pop bc
	rr a
	djnz GUIRWinButtonsDo
	ret

GUIRWrappedText:
	call GUIRxy
	ld a,(bc)
	ld e,a					;e is the width to look for
	inc bc
	ld a,(bc)
	;will eventually deal with different font faces here
	inc bc					;bc now points to the first char of the zero-term. string
GUIRWrappedTLO:				;bc = first char, e=width, hl = xy
	push de					;save the width
		push hl					;save xy
			ld a,h
			ld (pencol),a
			ld a,l
			ld (penrow),a
			push bc
GUIRWrappedTLI:
				ld a,(bc)				;get a char
				or a
				jr z,GUIRWrappedTLI2
				cp $D6
				jr z,GUIRWrappedTLI2
				push bc
					call getVarWidth
					pop bc
				ld a,e
				sub d
				or a
				jp m,GUIRWrappedTLI2
				ld e,a
				inc bc
				jr GUIRWrappedTLI
GUIRWrappedTLI2:
				pop hl
			push bc
				push hl
					or a
					sbc hl,bc
					ld b,l
					xor a
					sub b
					ld b,a
					pop hl
				call VPutSN
				pop bc
			pop hl
		ld a,l
		add a,6
		ld l,a
		pop de
	ld a,(bc)
	or a
	ret z
	cp $D6
	jr nz,GUIRWrappedTLO
	inc bc
	jr GUIRWrappedTLO
GUIRTextLineIn:
	ld hl,linein_render_type
	ld (hl),2
GUIRLineIn:
	call GUIRxy
	push hl
;	inc bc
	ld a,(bc)
	add a,h
	inc a
	ld d,a
	ld a,6
	add a,l
	ld e,a
	pop hl
	push hl
		push de
			;push hl
				;push bc
					ld a,(linein_render_type)
					push af
						xor a							;ld a,0
						dec d
						dec e
						call mos_filledrectangle_save
						pop af
					ld (linein_render_type),a
					;pop bc
				;pop hl
			ld a,(bc)
			push af
				ld a,h
				inc a
				inc a
				ld (pencol),a
				ld a,l
				ld (penrow),a
				;now disp the text
				inc bc
				inc bc		;maxchar is a WORD, not a byte.  Now at curpos
				inc bc		;forgot about width.  Hehe... >_> :P
				push bc
					pop hl
				bcall(_ldhlind)
				inc bc
				inc bc
				add hl,bc					;hl now at txt to disp.  de=end xy
				ld b,h
				ld c,l
				pop af
GUIRLineIn_NoSet:
			;a=width
			;bc=ptr2txt
			;de=end xy
			;penrow/pencol set
			ld e,a
			ld h,a
			push bc
GUIRWrappedTLII:
				ld a,(bc)				;get a char
				or a
				jr z,GUIRWrappedTLII2
				cp $d6
				jr z,GUIRWrappedTLIICR
				push bc
					push hl
						call getVarWidthP
						pop hl
GUIRWrappedTLII3:
					pop bc
				ld a,e
				dec d
				sub d
				or a
				jp m,GUIRWrappedTLII2
				dec a
				ld e,a
				inc bc
				jr GUIRWrappedTLII
GUIRWrappedTLIICR:
				inc bc
				ld a,h
				sub e
				inc a
				ld (bc),a				;this should set the end-of-line x
				dec bc					;this may or may not be correct....
GUIRWrappedTLII2:
		pop hl
;	push bc
	push hl
		or a
		sbc hl,bc
		ld b,l
		xor a
		sub b
		ld b,a
		pop hl
	ld a,(linein_render_type)
	or a
	jr z,GUIRWrappedTLII4
	ld a,(hl)
	or a
	call nz,VPutSN
	push hl
		pop bc
	jr GUIRWrappedTLII5
GUIRWrappedTLII4:
	ld a,(hl)
	or a
	jr z,GUIRWrappedTLII5
	ld a,$09						;the [] character
	push bc
		push hl
			ld e,0
			bcall(_DPutMap)
			pop hl
		pop bc
	djnz GUIRWrappedTLII4
GUIRWrappedTLII5:
	ld a,(linein_render_type)
	dec a
	ret z
			pop de						;???? WTF YO
		pop hl
	ld a,255
	jp GUIRBorder_NoSet
GUIRHotSpot:
GUIRMouseCursor:
GUIRMouseClick:
	ret
GUIRxy:						;returns d=x, l=y
	ld a,(bc)
	ld d,a
	call RenderGUIGetWinX
	add a,d
	ld d,a
	inc bc
	ld a,(bc)
	ld e,a
	call RenderGUIGetWinY
	add a,e
	ld l,a
	inc bc
	ld h,d
	ret
GUIRSprite:
	call GUIRxy
	ld a,(bc)
	inc bc
	push bc
	pop ix
	ld b,a
	ld a,d
	jp imPutSprite
	;ret
GUIRLargeSprite: 
	call GUIRxy
	ld a,(bc)
	ld e,a
	inc bc
	ld a,(bc)
	ld d,a
	inc bc
	push bc
	pop ix
	push de
	pop bc
	ld a,h
	jp imLargeSprite
	;ret
GUIRScrollVert:									;[x,y,height,ID,per,min,max,cur] (12 bytes)
	call GUIRxy
GUIRScrollVertDesk:
	push bc
	push de
	push hl
	ld b,6
	ld a,d
	ld ix,ScrollVertTop
	call imPutSprite
	pop hl
	pop de
	pop bc
	ld a,(bc)
	add a,l
	push bc
	push de
	push hl
	ld b,6
	add a,b
	ld l,a
	ld a,d
	ld ix,ScrollVertBottom
	call imPutSprite
	pop hl
	pop de
	pop bc
	push bc
	push de
	push hl
	ld a,(bc)
	ld b,a
	ld a,l
	add a,6
	ld l,a
	ld a,d
	ld ix,ScrollVertFillSprite
	call imPutSprite
	pop hl	;l=y
	pop de	;d=x
	pop bc	;bc/GUIStack pointer
	call GUIRScrollHorizVert_ScrollSetup
	add a,l
	add a,6
	ld l,a
	inc h
	push hl
		ld de,(scrollPer)
		ld a,b
		call MultADE
		ld de,(ScrollMax)
		call DivHLDE
		pop de
	ld h,d
	ld a,l
	cp 3
	jr nc,ScrollVertNoInc
	ld a,3
ScrollVertNoInc:
	ld l,e
	add a,e
	ld e,a
	inc d
	inc d
	inc d
	inc d
	inc d
	xor a			;ld a,0
	push de
		dec d
		dec e
		call mos_filledrectangle_save
		ld ix,ScrollBarVertSE
		ld b,1
		ld a,h
		push hl
			call imPutSprite
			pop hl
		pop de
	ld a,e
	sub l
	dec a
	dec a
	ld b,a
	ld a,h
	inc l
GUIRSVLoop:
	push bc
		push af
			push hl
				ld b,1
				ld ix,ScrollBarVertM
				call imPutSprite
				pop hl
			pop af
		pop bc
	inc l
	djnz GUIRSVLoop
	ld b,1
	ld ix,ScrollBarVertSE
	jp imPutSprite
	;ret
GUIRScrollHoriz:									;[x,y,height,ID,per,min,max,cur]
	call GUIRxy
	push bc
		push de
			push hl
				ld b,7
				ld a,d
				ld ix,ScrollHorizLeft
				call imPutSprite
				pop hl
			pop de
		pop bc
	ld a,(bc)					;get the width
	add a,d						;add it to x, not y (d=x,l=y)
	push bc
		push de
			push hl
				add a,6						;take into account the width of the left arrow
			;	ld l,a
			;	ld a,d
				ld b,7
				ld ix,ScrollHorizRight
				call imPutSprite
				pop hl
			pop de
		pop bc
	ld a,5
	add a,d
	ld d,a
	ld h,d
	ld a,(bc)
	add a,d
	ld d,a
	ld e,l
;	inc d
	inc d
	ld a,6
	add a,e
	ld e,a
	push bc
		push de
			ld a,d
			add a,5
			ld d,a
			push hl
				ld a,$ff
				call GUIRBorder_NoSet
				pop hl
			pop de
		inc h
		inc l
		push de
;			push hl
				ld a,%01010101
				dec d
				dec e
				call mos_filledrectangle_save
;				call drawcustomrectangle
;				pop hl
			pop de
		pop bc
	inc e
	call GUIRScrollHorizVert_ScrollSetup
	add a,h
;	add a,6		;<--
	ld h,a
;	inc h		;<--
	push hl
	ld de,(scrollPer)
	ld a,b
	call MultADE
	ld de,(ScrollMax)
	call DivHLDE
	pop de
	ld h,d
	ld a,l
	cp 3
	jr nc,ScrollHorizNoInc
	ld a,3
ScrollHorizNoInc:
	ld l,e
	add a,d
	ld d,a
	inc e
	inc d
	inc e
	inc e
	inc e
	inc e
	xor a								;ld a,0
	push de

		dec d
		dec e
		call mos_filledrectangle_save

		ld ix,ScrollBarHorizSE
		ld b,5
		ld a,h
		push hl
			call imPutSprite
			pop hl
		pop de
	ld a,d
	sub h
	dec a
	dec a
	ld b,a
	;ld a,h
	inc h
GUIRSHLoop:
	push bc
	push hl
	ld b,5
	ld a,h
	ld ix,ScrollBarHorizM
	call imPutSprite
	pop hl
	pop bc
	inc h
	djnz GUIRSHLoop
	ld b,5
	ld a,h
	ld ix,ScrollBarHorizSE
	jp imPutSprite

GUIRBorder:
	call GUIRxy
	ld a,(bc)
	add a,h
	ld d,a
	inc bc
	ld a,(bc)
	add a,l
	ld e,a	;start=hl;end=de
	inc bc
	ld a,(bc)
GUIRBorder_NoSet:					;hl=start, de=end, a=color
	and %00000001
	jp mos_fastrectangle

GUIRScrollHorizVert_ScrollSetup:
	ld a,(bc)				;common with horiz starts here
	inc bc	;now at ID
	inc bc	;now at per
	push hl
		push bc
			pop hl
		push af
			ld de,ScrollPer
			ld bc,8
			ldir					;now they are all stored
			pop bc
		ld hl,(ScrollMax)
		ld de,(ScrollMin)
		or a
		sbc hl,de
	;	or a
	;	ld de,(ScrollPer)
	;	sbc hl,de
		ld (ScrollMax),hl
		ld hl,(ScrollCur)
		ld de,(ScrollMin)
		or a
		sbc hl,de
		ex de,hl
		ld a,b
		push bc
			call MultADE
			ld de,(ScrollMax)
			call DivHLDE
			pop bc
		ld a,l
		pop hl
	ret

GUIRRect:
	call GUIRxy					;h=x,l=y
	ld a,(bc)
	add a,h
	ld d,a
	inc bc
	ld a,(bc)
	add a,l
	ld e,a
	inc bc
	ld a,(bc)
	cpl
	dec d
	dec e
	jp mos_filledrectangle

GUIRCustom:
	push bc
	pop hl
	bcall(_ldhlind)
	jp (hl)

GUIRButtonImg:
	call GUIRxy
	inc bc
	inc bc
	push bc
	ld a,d
	ld h,a
	push hl
	ld ix,ButtonStart
	ld b,7
	call imPutSprite
	pop hl
	pop bc
	inc h
	inc h
	push hl
	inc l
	ld a,(bc)
	inc bc
	push bc
	inc bc
	push bc
	pop ix
	ld c,a
	ld b,5
	ld a,h
	call imLargeSprite
	pop bc
	ld a,(bc)
	pop hl
	push af
	push hl
	add a,h
	ld d,a
	ld e,l
	push de
	;ld a,1
	call api_drawline_set_ld1
	pop de
	pop hl
	push hl
	ld a,e
	add a,6
	ld e,a
	ld l,a
	;ld a,1
	call api_drawline_set_ld1
	pop hl
	pop af
	add a,h
	ld b,7
	ld ix,ButtonEnd
	jp imPutSprite
GUIRButtonText:
	call GUIRxy
	inc bc
	inc bc
	push bc
	ld a,d
	ld h,a
	push hl
	ld ix,ButtonStart
	ld b,7
	call imPutSprite
	pop hl
	pop bc
	inc h
	inc h
	push hl
		ld a,l
		ld (penrow),a
		ld a,h
		ld (pencol),a
		push bc
			pop hl
		set textinverse,(iy+textflags)
		bcall(_vPutsApp)
		res textinverse,(iy+textflags)
		;got up to here
		pop hl
	ld a,(pencol)
	sub h
	push af
	push hl
	add a,h
	ld d,a
	ld e,l
	push de
	;ld a,1
	call api_drawline_set_ld1
	pop de
	pop hl
	push hl
	ld a,e
	add a,6
	ld e,a
	ld l,a
	;ld a,1
	call api_drawline_set_ld1
	pop hl
	pop af
	add a,h
	ld b,7
	ld ix,ButtonEnd
	jp imPutSprite
	;ret						;<------removed: call/ret changed to jp.
GUIRRadio:
	call GUIRxy
	ld a,d
	add a,6
	ld (pencol),a
	ld a,l
	dec a
	ld (penrow),a
	inc bc
	push bc
		;push hl
			;push de
				ld ix,RadioButtonOff
				ld a,(bc)
				or a
				jr z,GUIRCheckboxOff
				ld ix,RadioButtonOn
				jr GUIRCheckboxOff
GUIRCheckbox:
	call GUIRxy
	ld a,d
	add a,6
	ld (pencol),a
	ld a,l
	dec a
	ld (penrow),a
	inc bc
	push bc
		;push hl
			;push de
				ld ix,CheckboxOff
				ld a,(bc)
				or a
				jr z,GUIRCheckboxOff
				ld ix,CheckboxOn
GUIRCheckboxOff:
				;pop de
			;pop hl
		ld b,5
		ld a,d
		call imPutSprite
		pop hl
	inc hl
	bcall(_vPutsApp)
	ret
GUIRByteInt:
	call GUIRxy
	ld a,l
	ld (penrow),a
	ld a,h
	inc a
	inc a
	ld (pencol),a
	push hl
	ld a,(iy+sgrFlags)
	push af
		set textWrite,(iy+sGrFlags)
		ld a,(bc)
		bcall(_VDispA)
		pop af
	ld (iy+sGrFlags),a
	pop hl
	ld a,h
	ld ix,ByteIntBox
	ld b,7
	ld c,3
	jp imLargeSprite
GUIRWordInt:
	call GUIRxy
	ld a,l
	ld (penrow),a
	ld a,h
	inc a
	inc a
	ld (pencol),a
	push hl
	push bc
	pop hl
	bcall(_ldhlind)
	ld a,(iy+sgrFlags)
	push af
		set textWrite,(iy+sGrFlags)
		bcall(_vdisphl)
		pop af
	ld (iy+sGrFlags),a
	pop hl
	ld a,h
	ld ix,WordIntBox
	ld bc,(7*256)+4
;	ld b,7
;	ld c,4
	jp imLargeSprite
GUIRTextMultiline:			;[x,y,rows,width,curdatapos (0 to datalength) [word],data]
	call GUIRxy
	push bc
		push hl
			push bc
				push hl
					ld a,l
					ld (penrow),a
					ld hl,linein_render_type
					ld (hl),1
					pop hl
				pop bc
			push hl
				ld a,(bc)			;rows
				ld (SpriteX),a
				inc bc
				ld a,(bc)			;width
				push af
					inc bc
					ld a,(bc)
					ld l,a
					inc bc
					ld a,(bc)
					ld h,a
					inc bc
					add hl,bc
					push hl
						pop bc
					pop af
				sub 3
				pop hl
			push hl
				push af
					add a,3
					add a,h
					ld d,a
					ld a,(SpriteX)
					add a,a
					ld e,a
					add a,a
					add a,e
					add a,l
					ld e,a
					push af
;						push bc
;							push hl
								push de
;									ld a,(linein_render_type)
;									push af
										;ld a,$00
										xor a
										dec d
										dec e
;										call DrawCustomRectangle
										call mos_filledrectangle_save
;										pop af
;									ld (linein_render_type),a
									pop de
;								pop hl
;							pop bc
						pop af
GUIRTextMultilineLoop:
					;a=width
					;bc=ptr2txt
					;de=end xy
					;penrow/pencol set
					pop af
				pop hl
			push af
				ld a,h
				inc a
				inc a
				ld (pencol),a
				pop af
			push hl
				push af
					call GUIRLineIn_NoSet		;returns bc pos
					ld a,(bc)
					or a
					jr z,GUIRTextMultilineEnd
					cp $d6
					jr z,GUIRTextMultilineLoopCR
					dec bc
					ld a,(bc)
					cp $d6
					jr nz,GUIRTextMultilineLoopNoCR
GUIRTextMultilineLoopCR:
					inc bc
GUIRTextMultilineLoopNoCR:
					inc bc
					ld a,(penrow)
					add a,6
					ld (penrow),a
					ld a,(SpriteX)
					dec a
					ld (SpriteX),a
					jr nz,GUIRTextMultilineLoop
GUIRTextMultilineEnd:
					pop hl
				pop af
			pop hl
		pop bc
	ld a,(bc)
	add a,a
	ld d,a
	add a,a
	add a,d
	add a,l
	ld e,a
	inc bc
	ld a,(bc)
	add a,h
	ld d,a
	ld a,255
	jp GUIRBorder_NoSet
GUIRPassIn:
	ld hl,linein_render_type
	ld (hl),0
	jp GUIRLineIn
GetVarWidth:
	ld d,1
	ld hl,width1
	ld bc,width2-width1
	cpir
	ret z
	inc d
	ld bc,width3-width2
	cpir
	ret z
	inc d
	ld bc,width4-width3
	cpir
	ret z
	inc d
	ld bc,Width5-Width4
	cpir
	ret z
	inc d
	ld bc,width6-width5
	cpir
	ret z
	inc d
	ld bc,widthend-width6
	cpir
	ret z
VarWidthNotFound:
	bcall(_DGetCharWidth)
	ld d,c
	ret

VPutSN:
	PUSH AF
		PUSH DE
			PUSH IX
				ld a,b
				or a
				jr z,PP11
PP10:
				LD A,(HL) ; get a character of string name
				or a
				jr z,PP11
				INC HL
				cp $d6
				jr z,PP20
				push hl
					ld e,0
					bcall(_DPutMap) ; display one character of string
					pop hl
				DJNZ PP10 ; display rest of string
PP11:
				POP IX
			POP DE
		POP AF
	RET
PP20:
	inc hl
	jr PP11
;-----------------------------------------------
; (no longer used) The following routine borrowed from the TI Calculator API by Timendus
;-----------------------------------------------
api_drawline_set_ld1:
	ld a,1
api_drawline_set:
	dec d
;	dec e
	jp mos_filledrectangle

#ifdef false
DrawCustomRectangle:
api_draw_square:
   ; hl = ul
   ; de = lr
   ; draw block from (h,l) to (d,e)
   ; a:  0   = white
   ;     $FF = black

   ld (api_drawline_color),a   ; store color
api_draw_square_noset:
   ld a,h            ; ~~\
   cp 96
   jr c,api_drawblock_skip1
   ld h,96
api_drawblock_skip1:
   ld a,d
   cp 96
   jr c,api_drawblock_skip2
   ld d,96         ;       -- Check for off screen coords
api_drawblock_skip2:
   ld a,l
   cp 64
   jr c,api_drawblock_skip3
   ld l,64
api_drawblock_skip3:
   ld a,e
   cp 64
   jr c,api_drawblock_loop
   ld e,64         ; __/
api_drawblock_loop:
   ld a,l
   cp e
   ret nc            ; check if coord1 is still higher than coord2
   push hl
   push de
   ld e,l            ; if so; draw uppermost line
   call api_drawline_dontset
   pop de
   pop hl
   inc l            ; and go down one pixel
   ld a,(api_drawline_color)
   cp %10101010
   jr z,api_drawblock_dither
   cp %01010101
   jr nz,api_drawblock_loop
api_drawblock_dither:
   cpl
   ld (api_drawline_color),a
   jp api_drawblock_loop
api_drawline_set:
   ld (api_drawline_color),a
api_drawline_dontset:
;   ret
   
   push hl
	   ld a,h
	   call api_drawline_getmask         ; get left mask
	   ld b,a
	   ld a,d
	   call api_drawline_getmask         ; get right mask
	   cpl
	   ld c,a
	   pop hl
   push bc         ; store masks
   call api_drawline_getbyte         ; get left bytepointer
   push hl
   ld h,d
   ld l,e
   call api_drawline_getbyte         ; get right bytepointer
   pop de
   push de
   push hl
   sbc hl,de         ; calculate distance between bytepointers
   jp z,api_drawline_onebyte   ; if same byte, skip to onebyte
   ld a,l
   cp 1
   jp z,api_drawline_skip_inner   ; if two succeeding bytes, skip filling inner bytes
   ld b,l
   dec b
   ex de,hl
   inc hl
   ld a,(api_drawline_color)
api_drawline_loop_inner:
   ld (hl),a         ; Fill inner bytes up with color
   inc hl
   djnz api_drawline_loop_inner
api_drawline_skip_inner:
   pop de            ; get right bytepointer
   pop hl            ; get left bytepointer
   pop bc            ; get masks
   call api_drawline_modify
   ex de,hl
   ld b,c
   jp api_drawline_modify
api_drawline_onebyte:
   pop de            ; get right bytepointer
   pop hl            ; get left bytepointer
   pop bc            ; get masks
   ld a,b
   and c            ; merge masks
   ld b,a
api_drawline_modify:         ; make the changes to the address in hl
   push bc
   ld a,(api_drawline_color)
   ;b = mask
   ;a = color
   ;(hl) = screen
   push bc
   ld c,a
   ld a,b
   ;cpl
   and c
   pop bc
   push af
   ld a,b
   cpl
   and (hl)
   ld b,a
   pop af
   or b
   ld (hl),a
   pop bc
   ret
api_drawline_getbyte:
   ld a,h            ; calculate pointer to byte in plotsscreen
   ld h,0            ; h = x, l = y
   ld b,h
   ld c,l
   add hl,hl         ; l times 12
   add hl,bc
   add hl,hl
   add hl,hl
   ld c,a
   srl c            ; x devided by 8
   srl c
   srl c
   add hl,bc
   ld bc,plotsscreen
   add hl,bc
   ret
api_drawline_getmask:
   and 7            ; look mask up in table
   ld hl,api_drawline_lut
   push de
;   push af
;   push de
;   ld a,(api_drawline_color)
;   cp %10101010
;   jr z,api_drawline_getmask1
;   cp %01010101
;   jr nz,api_drawline_getmask3
;   ld de,16
;   add hl,de
;   jr api_drawline_getmask3
;api_drawline_getmask1:
;   ld de,8
;   add hl,de
;api_drawline_getmask3:
;   pop de
;   pop af
   ld e,a
   ld d,0
   add hl,de
   ld a,(hl)
   pop de
   ret
api_drawline_lut:
   .db %11111111
   .db %01111111
   .db %00111111
   .db %00011111
   .db %00001111
   .db %00000111
   .db %00000011
   .db %00000001
;api_drawline_lut1:
;	.db %10101010
;	.db %00101010
;	.db %00101010
;	.db %00001010
;	.db %00001010
;	.db %00000010
;	.db %00000010
;	.db %00000000
;api_drawline_lut2:
;	.db %01010101
;	.db %01010101
;	.db %00010101
;	.db %00010101
;	.db %00000101
;	.db %00000101
;	.db %00000001
;	.db %00000000
#endif
;--------------------------------------------------
;Inputs: HL/DE
;Outputs: HL remainder DE
DivHLDE:
	push bc
	ld a,h
	ld c,l
	ld hl,0
	ld b,16
DivHLDE2:
	scf \ rl c		; unroll 16 times
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr	nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	djnz DivHLDE2
	ld d,a
	ld e,c
	pop bc
	ex de,hl
	ret
;----------------------------------------------------
;inputs: a*de
;outputs: ahl
MultADE:
	ld hl,0
	ld c,l		;was ld c,0
	add	a,a		; optimised 1st iteration
	jr	nc,$+4
	ld	h,d
	ld	l,e
	ld b,7
MultADE2:
	add	hl,hl		; unroll 7 times
	rla			; ...
	jr	nc,$+4		; ...
	add	hl,de		; ...
	adc	a,c		; ...
	djnz MultADE2
	ret

;--------------------------------------------------------
VertLine:
	;h,l=start
	;b=height
	push bc
	ld a,h
	ld e,l
	call imGetPixel
	pop bc
	ld c,a
	ld de,12
VertLineLoop:
	ld a,(hl)
	or c
	ld (hl),a
	add hl,de
	djnz VertLineLoop
	ret
;--------------------------------------------------------
GUIDrawHourglass:
	ld de,gbuf+28*12+5				;40 is closest to 44
	ld hl,IconSpace32b
	ld b,8
	or a
	jr nz,GUIDrawHourglassCopyBack
	ex de,hl
GUIDrawHourglassCopySave:
	ldi
	ldi
	push de
		ld de,10
		add hl,de
		pop de
	djnz GUIDrawHourglassCopySave
	ld a,44
	ld l,28
	ld b,7
	ld ix,HourglassCursor
	call imPutSpriteMask
	jr GUIDrawHourglassCopy
GUIDrawHourglassCopyBack:
	ldi
	ldi
	push hl
		ld hl,10
		add hl,de
		pop de
	ex de,hl
	djnz GUIDrawHourglassCopyBack
GUIDrawHourglassCopy:
	jp imFastCopy
;--------------------------------------------------------
dbfSimpleGUIMouseClick:
	ld hl,dbfSimpleGUIMouse_PageSwapCode
dbf_ReturnToP2FromP1:
	ld de,Op1
	ld bc,5
	ldir
	in a,(6)
	dec a
	jp Op1
dbfGUIMouse_Return:
	pop hl
	ld de,GUIMouseCallRerender
	call mos_cphlde
	jr z,dbfGUIMouse_Return_ReRenderRemoval
	ld de,GUISScrollReturn
	call mos_cphlde
	jr nz,dbfGUIMouse_Return_NoScrollFix
	pop hl
	jr dbfGUIMouse_Return_ReRenderRemoval
dbfGUIMouse_Return_NoScrollFix
	push hl
dbfGUIMouse_Return_ReRenderRemoval:
	ld hl,dbfGUIMouseReturn_PageSwapCode
	jr dbf_ReturnToP2FromP1
dbfSimpleGUIMouse_PageSwapCode:
	out (6),a
	jp dbfSimpleGUIMouse_Process
dbfGUIMouseReturn_PageSwapCode:
	out (6),a
	jp dbfGUIMouse_ReturnP2
dbfPGS_Template_GUIRWinButtons:
	.db 0
	.dw dbfGUIMouse_Return
	.dw dbfGUIMouse_Return
	.dw dbfGUIMouse_Return
dbfMenuTemp_SMW:
	;NO CONTENTS NECESSARY
dbfMenuTemp_Scroll:
	.db 72,0,27,0
	.dw 1,0,1,0			;at (50,0), height 22, ID 0, per=1, min=0, max=1, cur=0
	.dw $0000,$0000					;onclick
dbfMenuTemp_ScrollEnd:
dbfMenuTemp_WinB:
	.db 32
	.dw 0,0,0						;not clickable!
sum12MouseHookHotspot:
	.db 96,0,2,2
	.dw dbfGUIMouse_Return
