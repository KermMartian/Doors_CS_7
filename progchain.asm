;-----------------------------------------------------------
;	Filename:		ProgChain.asm
;	Long name:  	Program chaining for RunProg
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	May 15, 2010
;	Routines Included:
;		-PushProgChain
;		-PopProgChain
;		-GetProgChainSize
;		-GetProgChainTop
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

PushProgChain:
	ld hl,ProgChainAppVar
	rst 20h
	bcall(_chkfindsym)
	jr c,PushProgChainCreate
	ex de,hl
	push hl
		ld a,(hl)					;get size byte low
		inc hl
		ld h,(hl)					;and size byte high
		ld l,a
		ld de,20
		add hl,de					;add one slot size
		pop de
	ex de,hl						;hl = size byte in prgm, de = new size
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl							;at the S byte now
	ld a,(hl)						;a = S'
	inc (hl)						;S = S' + 1
	inc hl
	push hl							;save the first A1 byte
		ld l,a
		ld h,0
		add hl,hl					;hl = S'*2
		add hl,hl					;hl = S'*4
		push hl
			add hl,hl				;hl = S'*8
			add hl,hl				;hl = S'*16
			pop bc
		add hl,bc					;hl = S'*20
		pop de
	add hl,de						;hl = &S + 20S'
	push hl
		ld de,20
		ex de,hl
		bcall(_InsertMem)
		pop hl
	jr PushProgChainFinish
PushProgChainCreate:
	ld hl,21						;one 20-byte entry and one size byte
	bcall(_CreateAppVar)
	ex de,hl
	inc hl
	inc hl
	ld (hl),1
	inc hl
PushProgChainFinish:
	push hl
		ld (hl),0
		push hl
			pop de
		inc de
		ld bc,20-1
		ldir						;clean the entry
		pop hl
	scf
	ccf
	ret
	
PopProgChain:
	ld hl,ProgChainAppVar
	rst 20h
	bcall(_chkfindsym)
	ret c
	inc de
	inc de
	ld a,(de)
	dec a
	ld (de),a
	jr z,PopProgChainDelete
	push de							;update the size!
		ex de,hl
		dec hl
		ld d,(hl)
		dec hl
		ld e,(hl)
		push hl
			ld hl,-20
			add hl,de
			pop de
		ex de,hl
		ld (hl),e
		inc hl
		ld (hl),d
		pop de
	inc de
	ld l,a
	ld h,0
	add hl,hl						;hl = S*2
	add hl,hl						;hl = S*4
	push hl
		add hl,hl					;hl = S*8
		add hl,hl					;hl = S*16
		pop bc
	add hl,bc						;hl = S*20
	add hl,de						;hl = &S+1+S*20
	ld de,20						;20 bytes to delete
	bcall(_delmem)					;remove that top entry on the stack
	ret
PopProgChainDelete:
	dec de
	dec de
	bcall(_delvar)
	ret

GetProgChainSize:
	bcall(_PushOp1)
	ld hl,ProgChainAppVar
	rst 20h
	bcall(_chkfindsym)
	xor a
	jr c,GetProgChainSizeFail
	inc de
	inc de
	ld a,(de)
GetProgChainSizeFail:				;carry is preserved
	push af
		push de
			bcall(_PopOp1)
			pop de
		pop af
	ret								;nc is already set

GetProgChainTop:
	bcall(_pushop1)
	call GetProgChainSize
	ret c
	or a
	ret z
	dec a
	ld l,a
	ld h,0
	add hl,hl						;hl = S*2
	add hl,hl						;hl = S*4
	push hl
		add hl,hl					;hl = S*8
		add hl,hl					;hl = S*16
		pop bc
	add hl,bc						;hl = S*20
	add hl,de						;hl = &S+S*20
	inc hl							;hl = &S+1+S*20
	push hl
		bcall(_popop1)
		pop hl
	or a						;clear carry flag
	ret

SwapProgChain:						;swaps top two progchain entries
	call GetProgChainSize
	cp 2
	ret c							;don't bother if fewer than 2 entries
	call GetProgChainTop
	push hl
		ld de,-20
		add hl,de
		pop de
	ld b,20
SwapProgChainLoop:
	ld c,(hl)
	ld a,(de)
	ld (hl),a
	ld a,c
	ld (de),a
	inc hl
	inc de
	djnz SwapProgChainLoop
	ret

ProgChainAppVar:
	.db 15h,"CHAIN7",0
