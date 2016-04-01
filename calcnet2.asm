;CALCnet2 Core Routines
;(c) 2001-2006 Kerm Martian aka Christopher Mitchell
;All rights reserved
;please see the associated license.txt for license details
;
;This file contains two routines:
;		+ Cn2_Setup
;		+ Cn2_Int_Start
;What is Implemented:
; rec frame
; Int exit on unmatched SID
; rec frame wrapper
; send frame wrapper
; byte-receive routine
; byte-send ; byte-get routine
; Collision detection
; Cn2_Int_SendByte_OuterLoop masking H/L check
; get and store SID
; doublecheck rrc c and rlc c opcode functions -> decided on sla and sra instead
; Doublecheck send defines H/L in header.asm
; Reset setbuffer status flag on successful send
; Set getbuffer status flag on successful receive
; check linestate for send xmitbyte start
; collide receive if receive buffer contains data
; ack read on send frame
; ack write on read frame
; send frame

;What Needs to be Implemented:

;;;Stuff to Define;;;
;[-] ClockMask
;[-] ClockLow11
;[-] ClockHigh
;[-] DataMask
;[-] DataLow
;[-] DataHigh
;[-] ld a,(linkstate)				--> in a,(0)
;[-] ld (linkstate),a				--> out (0),a
;[-] Time_for_15,000_Clock_Cycles
;[-] Time_For_6,000_cycles
;[-] Time_For_5,000_cycles
;[-] StackSaveWord

;SavesScreen Data:
Cn2StackSaveWord .equ SavesScreen+0						;86ECh
;+2: current calc's SID
SIDSave		.equ	SavesScreen+2						;86EEh
;+7: cn2_int_recbuf
Cn2_Int_RecBuf .equ SavesScreen+5+2						;86F3h
;+270: cn2_int_sendbuf
Cn2_Int_SendBuf .equ SavesScreen+256+5+2+7				;87FAh
;+533: 7 bytes of saferam
Cn2_SafeRAM_7b	.equ SavesScreen+512+5+2+14				;8901h
;+540: 3 bytes of Recbytecount
Cn2_Count_Rec	.equ SavesScreen+512+7+14+7				;8908h
;+547: 3 bytes of Sendbytecount
Cn2_Count_Send	.equ SavesScreen+512+28+3				;890Bh
_GetSerial .equ	$807E
;Cn2_Int_RecBuf:
;0->1->2->3->4->5->6->7->8->|9-10-11->>
;| Sender SID   |Size |Data---->>
;
;Cn2_Int_SendBuf:
;0->1->2->3->4->5->6->7->8->|9-10-11->>
;| Receiver SID |Size |Data---->>
;

Cn2_Clear_RecBuf:
	ld hl,Cn2_Int_RecBuf
	push hl
	pop de
	inc de
	ld (hl),0
	ld bc,256+5+2
	ldir
	ret
Cn2_Clear_SendBuf:
	ld hl,Cn2_Int_SendBuf
	push hl
	pop de
	inc de
	ld (hl),0
	ld bc,256+5+2
	ldir
	ret

Cn2_Setdown:
	di
	call Cn2_Clear_RecBuf
	call Cn2_Clear_SendBuf
	im 1
	ei
	ret
Cn2_Setup:
	di
	ld hl,SavesScreen
	push hl
	pop de
	push de
	inc de
	ld bc,767
	ld (hl),0
	ldir
#ifndef cn2fakeserial
#ifndef cn2fakeserial_gcnhost
	bcall(_GetSerial)
	ld hl,Op4
	pop de
	inc de
	inc de
	ld bc,5
	ldir
#endif
#endif
#ifdef cn2fakeserial_gcnhost
	pop hl
	inc hl
	inc hl
	ld a,$aa
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
#endif
#ifdef cn2fakeserial
	pop de
	ld hl,6
	add hl,de
	ld b,2
	call iRandom
	ld (hl),a
#ifdef cn2debug	
	or a
	jr nz,cn2fakeserialnoset
	ld hl,cn2Message
	ld de,Cn2_Int_SendBuf
	ld bc,cn2MessageEnd-cn2Message
	ldir
cn2fakeserialnoset:
#endif
#endif
	di
	ld hl,Cn2_Caller_Routine
	ld de,$9999
	ld bc,Cn2_Caller_RoutineDone-Cn2_Caller_Routine
	ldir
	in a,(6)
	ld ($9999+Cn2_caller_HomePage-Cn2_Caller_Routine+1),a
	ld a,$9a
	ld i,a
	ld h,a
	ld l,0
	ld (hl),$99
	push hl
	pop de
	inc de
	ld bc,256
	ldir
	ld a,%00000110
	out (4),a
    ld a,11
    out (3),a
	im 2
	ld a,sendCLDL
	out (0),a
	ei
	ret

Cn2_Caller_Routine:
	ex af,af'
	exx
	in a,(6)
	push af
Cn2_Caller_HomePage:
	ld a,0						;dummy for page load - DO NOT optimize to xor a
	out (6),a
	call Cn2_Int_Start
	pop af
	out (6),a
;	ld a,%00000110
;	out (4),a
;    ld a,10     ; Enable hardware
;    out (3),a
	exx
	ex af,af'
;	ei
	ret			;18 bytes total now
Cn2_Caller_RoutineDone:

Cn2_Int_Start:
;	ld hl,cn2MessageTxt
;	call mputsapp

#ifdef cn2debug
	ld hl,gbuf+726-12
	ld a,(hl)
	rlc a
	xor $01
	ld (hl),a
	ld hl,gbuf+727
	ld (hl),0
	ld hl,gbuf+727-12
	ld (hl),0
	ld hl,gbuf+727+12
	ld (hl),0
	ld hl,gbuf+727-24
	ld (hl),0
	ld hl,gbuf+727+24
	ld (hl),0
#endif

	ld hl,0
	add hl,sp
	ld (Cn2StackSaveWord),hl
	
	;start with Off stuff
	in a,(4)
	bit 0,a
	jr z,Cn2_NoOff
	ld a,2
	out ($10),a ;turn off screen
	res OnInterrupt,(iy+OnFlags)
	bcall(_debounceon)
	im 1
    eI			; disable interrupts
	ld	a,1
	out	(3),a
	halt
	di
	im 1
	res OnInterrupt,(iy+OnFlags)
	ld a,3
	out ($10),a ;turn off screen
	bcall(_debounceon)
	ld a,%00000110
	out (4),a
    ld a,11     ; Enable hardware
    out (3),a
	ei
	ret
Cn2_NoOff:
	ld hl,6000/58
Cn2_Int_CheckPendRec:
	dec hl													; 6
	in a,(0)												;11

	push hl
#ifdef cn2debug
	ld hl,gbuf+727-12
	ld (hl),a
#endif
	pop hl

	and ClockMask											; 7
	cp ClockLow								;1 = low			; 7
	jp z,Cn2_Int_CheckPendSend								; 7
	ld a,h													; 4
	or l													; 4
	jr nz,Cn2_Int_CheckPendRec								;12	;total here: 58 cycles
	;if we get here, then a device is indeed preparing to send
	ld hl,6000/58
Cn2_Int_ReceiveFrame:
	dec hl													; 6
	in a,(0)												;11
	and ClockMask											; 7
	cp ClockLow								;1 = low			; 7
	jr z,Cn2_Int_ReceiveFrameStarting						; 7
	ld a,h													; 4
	or l													; 4
	jr nz,Cn2_Int_ReceiveFrame								;12	;total here: 58 cycles
Cn2_Int_ReceiveFrameStarting:
	;INSERT RECEIVE ROUTINE HERE
	;first, get 5 bytes for the receiving SID
#ifdef cn2debug
	ld hl,gbuf+726
	ld a,(hl)
	rlc a
	xor $01
	ld (hl),a

	ld hl,gbuf+727
	ld a,(hl)
	or %01000000
	ld (hl),a
#endif
	ld hl,Cn2_Int_RecBuf+5
	ld b,(hl)
	inc hl
	ld a,(hl)
	and %01111111
	or b

	;jp nz,Cn2_Int_GetByte_PopFail						;this forces the collision
	ret nz
	
#ifdef cn2debug
	ld hl,gbuf+727
	ld a,(hl)
	or %00100000
	ld (hl),a
#endif
	ld b,5
	ld hl,Cn2_SafeRAM_7b
	push hl
Cn2_Int_ReceiveFrame_SIDRec
	push bc
	push hl
	call Cn2_Int_GetByte
	pop hl
	pop bc
	ld (hl),a
	inc hl
	djnz Cn2_Int_ReceiveFrame_SIDRec
	pop hl
	ld de,SIDSave
	ld b,5
Cn2_Int_ReceiveFrame_SIDVerify:
	ld a,(de)
	cp (hl)
	jr nz,Cn2_Int_CheckPendSend
	inc hl
	inc de
	djnz Cn2_Int_ReceiveFrame_SIDVerify				;if we get past here, it's for us
	ld b,5
	ld hl,Cn2_Int_RecBuf
Cn2_Int_ReceiveFrame_SIDSend
	push bc
	push hl
	call Cn2_Int_GetByte
	pop hl
	pop bc
	ld (hl),a
	inc hl
	djnz Cn2_Int_ReceiveFrame_SIDSend
	push hl										;now in Cn2_SafeRAM_7b we have the SID of the sender
	call Cn2_Int_GetByte
	pop hl
	ld (hl),a
	inc hl
	ld c,a
	push hl
	push bc
	call Cn2_Int_GetByte
	pop bc
	pop hl
	and %01111111						;reset bit 7
	ld (hl),a
	inc hl
	ld b,a								;now bc = datalength
	;ld hl,Cn2_Int_RecBuf+7+2
	push bc
Cn2_Int_ReceiveFrame_Data:
	push hl
	push bc
	call Cn2_Int_GetByte
	pop bc
	pop hl
	ld (hl),a
	inc hl
	dec bc
	ld a,b
	or c
	jr nz,Cn2_Int_ReceiveFrame_Data
	
	call Cn2_Int_Calc_Checksum_Rec			;returns hl
	ld a,l
	push hl
	call Cn2_Int_SendByte
	pop hl
	ld a,h
	call Cn2_Int_SendByte
	call Cn2_Int_GetByte
	pop hl
	cp $AA
	jr nz,Cn2_Int_CheckPendSend
	;ld (Cn2_Int_RecBuf+7),hl
	ld hl,Cn2_Int_RecBuf+6
	
	ld a,(hl)
	push af
		dec hl
		bcall(_ldhlind)
		pop af
	push af
		ld e,a
		ld d,0
		add hl,de
		ld (Cn2_Count_Rec+1),hl
		jr nc,Cn2_Int_RecCountUp
		ld hl,Cn2_Count_Rec
		inc (hl)
Cn2_Int_RecCountUp:
	pop af
	or %10000000
	ld (hl),a

Cn2_Int_CheckPendSend:
	ld hl,(Cn2_Int_SendBuf+5)
	ld a,h
	bit 7,a
	jp z,Cn2_Int_Done_QuitOK
	and %01111111
	or l
	jp z,Cn2_Int_Done_QuitOK
	ld hl,15000/125
Cn2_Int_SendFrame_Reset_DE:
	ld de,0					;used to check if 5,000 cycles have passed yet		;10
Cn2_Int_SendFrame_WaitLoop:													;--
	dec hl																	; 6
	ld a,h																	; 4
	or l																	; 4
	jr z,Cn2_Int_SendFrame_FailSendBusyNet											; 7
	in a,(0)																;11
	and ClockMask															; 7
	cp ClockLow																; 7
	jr nz,Cn2_Int_SendFrame_Reset_DE										; 7
	inc de																	; 6
	push hl			;next 5 lines is uberfast cphde for hl=5000					;11
	ld hl,5000/125															;10
	or a																	; 4
	sbc hl, de																;15
	pop hl																	;10
	jr z,Cn2_Int_SendFrame_ReadyToSend										; 7
	jr Cn2_Int_SendFrame_WaitLoop											;12 Total: 125
Cn2_Int_SendFrame_FailSendBusyNet											
	ld e,1
	ret
Cn2_Int_SendFrame_ReadyToSend:
	;INSERT SEND ROUTINE HERE
	;first the 100,000-cycle hold of the clock high
#ifdef cn2debug
	ld hl,gbuf+726+12
	ld a,(hl)
	rlc a
	xor $01
	ld (hl),a

	ld hl,gbuf+727
	ld a,(hl)
	or %00001000
	ld (hl),a
#endif
	ld hl,2273
Cn2_Int_SendFrame_ReadyToSendAlert:
	dec hl													; 6
	ld a,SendCHDL											; 7
	out (0),a												;11
	ld a,h													; 4
	or l													; 4
	jr nz,Cn2_Int_SendFrame_ReadyToSendAlert				;12 Total: 44
	;44x2273 = 100,012 cycles

#ifdef cn2debug
	ld hl,gbuf+727
	ld a,(hl)
	or %00000100
	ld (hl),a
#endif

	ld b,5
	ld hl,Cn2_Int_SendBuf
Cn2_Int_SendFrameAddr1:
	push hl
	ld a,(hl)
	push bc
	call Cn2_Int_SendByte
	pop bc
	pop hl
	inc hl
	djnz Cn2_Int_SendFrameAddr1
	ld b,5
	ld hl,SIDSave
Cn2_Int_SendFrameAddr2:
	push hl
	ld a,(hl)
	push bc
	call Cn2_Int_SendByte
	pop bc
	pop hl
	inc hl
	djnz Cn2_Int_SendFrameAddr2
	ld hl,Cn2_Int_SendBuf+5
	ld c,(hl)
	inc hl
	ld a,(hl)
	and %01111111
	ld b,a
	dec hl
	inc bc
	inc bc
Cn2_Int_SendFrameAddr3:
	push hl
	ld a,(hl)
	push bc
	call Cn2_Int_SendByte
	pop bc
	pop hl
	inc hl
	dec bc
	ld a,b
	or c
	jr nz,Cn2_Int_SendFrameAddr3

	call Cn2_Int_Calc_Checksum_Send			;returns hl
	push hl
	call Cn2_Int_GetByte
	ld l,a
	push hl
	call Cn2_Int_GetByte
	pop hl
	ld h,a
	pop de
	push hl
	or   a
	sbc  hl, de
	pop  hl
	ret nz

	ld a,$AA
	call Cn2_Int_Sendbyte

	;add to Cn2_Count_Rec
	ld hl,Cn2_Int_Sendbuf+6
	ld a,(hl)
	push af
		dec hl
		bcall(_ldhlind)
		pop af
	push af
		ld e,a
		ld d,0
		add hl,de
		ld (Cn2_Count_Send+1),hl
		jr nc,Cn2_Int_SendCountUp
		ld hl,Cn2_Count_Send
		inc (hl)
Cn2_Int_SendCountUp:
	pop af

	ld hl,Cn2_Int_Sendbuf
	ld (hl),0
	push hl
	pop de
	inc de
	ld bc,5+2+256-1
	ldir
	ld e,0
	ret
Cn2_Int_Done_QuitOK:
	ld e,0
	ret
Cn2_Int_GetByte:

#ifdef cn2debug
	ld hl,gbuf+727+24
	ld a,(hl)
	or $10
	ld (hl),a
#endif

	ld hl,40								;10
Cn2_Int_GetByte1:
	dec hl									;6
	in a,(0)								;11
	and LinkMask							;7
	cp GetCHDH								;7
	jr z,Cn2_Int_GetByte2					;7
	ld a,h									;4
	or l									;4
	jr z,Cn2_Int_GetByte_PopFail			;7
	jr Cn2_Int_GetByte1						;12	-->total: 65 cycles + 10 cycles for the initial set
Cn2_Int_GetByte2:							;use 300 cycles in the send routine

#ifdef cn2debug
	ld hl,gbuf+727+24
	ld a,(hl)
	or $30
	ld (hl),a
#endif

	ld hl,40
Cn2_Int_GetByte3:
	dec hl
	in a,(0)
	and LinkMask
	cp GetCLDL
	jr z,Cn2_Int_GetByte4
	ld a,h
	or l
	jr z,Cn2_Int_GetByte_PopFail
	jr Cn2_Int_GetByte3
	ret
Cn2_Int_GetByte4:

#ifdef cn2debug
	ld hl,gbuf+727+24
	ld a,(hl)
	or $70
	ld (hl),a
#endif

	ld c,0					;this is where the data found will be stored
	ld b,8					;how many bits to get
	ld hl,40
Cn2_Int_GetByte5:
	dec hl
	in a,(0)
	and ClockMask
	cp ClockHigh
	jr z,Cn2_Int_GetByte6
	ld a,h
	or l
	jr z,Cn2_Int_GetByte_PopFail
	jr Cn2_Int_GetByte5
Cn2_Int_GetByte6:
	sla c
	in a,(0)
	and DataMask
	;sra a
	sra a						;it's shifted -t-w-o- one bits too far left to start with
	or c
	ld c,a
	ld hl,40

#ifdef cn2debug
	ld hl,gbuf+727+24
	ld a,(hl)
	or $f0
	ld (hl),a
#endif

Cn2_Int_GetByte7:
	dec hl
	in a,(0)
	and ClockMask
	cp ClockLow
	jr z,Cn2_Int_GetByte8
	ld a,h
	or l
	jr z,Cn2_Int_GetByte_PopFail
	jr Cn2_Int_GetByte7
Cn2_Int_GetByte8:
	djnz Cn2_Int_Getbyte5
	;now we have the data in c
	ld a,c
	ret
Cn2_Int_GetByte_PopFail:
	ld sp,(Cn2StackSaveWord)
Cn2_Int_GetByte_ForceCollision:
	
#ifdef cn2debug
	ld hl,gbuf+727-24
	ld (hl),1
#endif

	ld e,sendCLDL
	ld c,0
	ld b,256
Cn2_Int_GetByte_ForceCollisionLoop:
	out (c),e
	nop
	nop
	nop
	and ClockMask
	ld d,a
	in a,(0)
	and ClockMask
	cp d
	jr nz,Cn2_Int_GetByte_ForceCollisionDone
	djnz Cn2_Int_GetByte_ForceCollisionLoop
Cn2_Int_GetByte_ForceCollisionDone:
	ld e,SendCLDL
	out (c),e
	ret
Cn2_Int_SendByte:
	;inputs: a is data to send
	cpl				;xor $ff
	ld b,8
Cn2_Int_SendByteRev:
	srl a
	rl c
	djnz Cn2_Int_SendByteRev
	
#ifdef cn2debug
	ld hl,gbuf+727+24
	ld a,(hl)
	or $01
	ld (hl),a
#endif

	ld b,8
	ld hl,12							;MATCHED to receive routine part 1
	call Cn2_Int_SendByte_PauseHL
	ld a,SendCLDL
	out (0),a
	ld hl,10								;10
Cn2_Int_SendByte1:
	dec hl									;6
	in a,(0)								;11
	and LinkMask							;7
	cp GetCLDL								;7
	jr nz,Cn2_Int_SendByte_DetectCollision	;7
	ld a,h									;4
	or l									;4
	jr z,Cn2_Int_SendByte2					;7
	jr Cn2_Int_SendByte1					;12	-->total: 65 cycles + 10 cycles for the initial set
Cn2_Int_SendByte2:
	ld a,SendCHDH
	out (0),a
	ld hl,12							;MATCHED to receive routine part 2
	call Cn2_Int_SendByte_PauseHL
	ld a,SendCLDL
	out (0),a

#ifdef cn2debug
	ld hl,gbuf+727+24
	ld a,(hl)
	or $03
	ld (hl),a
#endif

	ld hl,10							;MATCHED to receive routine part 3
	call Cn2_Int_SendByte_PauseHL
Cn2_Int_SendByte_OuterLoop:
	ld a,%00000001
	and c
	sla a								;move this into the data position
;	xor $02
;	xor %11010011						;XOR it with $D1 - now it's CLDn		Clock low = 0 for sending, so 0*1+(data_bit)*2 = out
	out (0),a
	push af
	ld hl,4
	call Cn2_Int_SendByte_PauseHL
	pop af
	or %00000001						;sets clock high - now it's CHDn
	out (0),a
	xor $03
	and %00000011
	push af
	ld hl,8
	call Cn2_Int_SendByte_PauseHL
	pop de
	in a,(0)
	and %00000011
	cp d
	jr nz,Cn2_Int_SendByte_DetectCollision
	ld a,SendCLDL
	out (0),a

#ifdef cn2debug
	ld hl,gbuf+727+24
	ld a,(hl)
	or $07
	ld (hl),a
#endif

	ld hl,12
	call Cn2_Int_SendByte_PauseHL
	sra c
	djnz Cn2_Int_SendByte_OuterLoop
	ret
Cn2_Int_SendByte_DetectCollision:
#ifdef cn2debug
	in a,(0)
	ld (gbuf+727-12),a
#endif
	ld sp,(Cn2StackSaveWord)

	ld hl,gbuf+727-24
	ld (hl),$80

	ld e,SendCHDL
	ld c,0
	ld hl,105
Cn2_Int_SendByte_DetectCollisionLoop:
	out (c),e													;12
	dec hl														;6
	ld a,h														;4
	or l														;4
	jr nz,Cn2_Int_SendByte_DetectCollisionLoop					;12 Total: 38 cycles
	ld e,SendCLDL
	out (c),e
	ret										;all set

Cn2_Int_SendByte_PauseHL:
	dec hl								;6
	ld a,h								;4
	or l								;4
	jr nz,Cn2_Int_SendByte_PauseHL		;12 ; THEREFORE 26 cycles/iteration --> 12 iterations needed
	ret

Cn2_Int_Calc_Checksum_Rec:
	ld hl,Cn2_Int_RecBuf+5				;
	jr Cn2_Int_Calc_Checksum
Cn2_Int_Calc_Checksum_Send:
	ld hl,Cn2_Int_SendBuf+5
Cn2_Int_Calc_Checksum:
	ld c,(hl)
	inc hl
	ld a,(hl)
	and %01111111
	ld b,a
	inc hl
	ld de,0
Cn2_Int_Calc_ChecksumLoop:
	ld a,(hl)							; 7
	push hl								;11
	ld h,0								; 7
	ld l,a								; 4
	add hl,de							;11
	ex de,hl							; 4
	pop hl								;10
	dec bc								; 6
	ld a,b								; 4
	or c								; 4
	jr nz,Cn2_Int_Calc_ChecksumLoop		;12 ; TOTAL: 68
	ex de,hl							;4
	ret
Cn2_Int_End:

#ifdef cn2fakeserial
cn2Message:
	.db $00,$00,$00,$00,$01
	.dw (cn2MessageEnd-cn2MessageTxt)|$8000
cn2MessageTxt:
	.db "Cemetech shall rule the world!",0
cn2MessageEnd:
#endif

