;CALCnet2 Core Routines
;(c) 2001-2006 Kerm Martian aka Christopher Mitchell
;Project refresh (c) 2010 Kerm Martian aka Christopher Mitchell
;USB work and debugging (c) 2011-2012 Kerm Martian aka Christopher Mitchell
;Assistance from Tim "geekboy" Keller
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
;[ ] Track down synchronization problem
;	[ ] Note: what's the point of the loop under Cn2_Rec_Frame? Isn't it going to fall
;		through regardless of the count.  And on the other hand, won't it freeze if the
;		clock never goes low? No, of course not. :P Let's keep reading
;	[ ] I think the value of the hl that's stored under Cn2_NoOff is too low.  Don't we
;		need to do the following?
;			(1) check if clock stays high for 6000 cycles
;		 >> (2) if it does, THEN WAIT until it goes low again
;			(3) _then_ try to receive
;	[ ] It looks like step 2 is missing, which could cause premature timeouts
;	[ ] OK, actually, step 2 is there, but both steps 1 and 2 are 6000 cycles.  Too low???
;	[ ] Might only work at all due to cycle slip :( bad for 84s
;[ ] Sync fix:
;	[X] Change step (2) pause to 55K-6K cycles = (55K-6K)/58 iterations
;	[X] Change send clock hold to 55K instead of 100K cycles

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

;SavesScreen Data: 61 9b d3 ca a6
Cn2StackSaveWord .equ SavesScreen+0						;86ECh
;+2: current calc's SID
SIDSave		.equ	SavesScreen+2						;86EEh
;+7: cn2_int_recbuf
Cn2_Int_RecBuf .equ SavesScreen+5+2						;86F5h
;+270: cn2_int_sendbuf
Cn2_Int_SendBuf .equ SavesScreen+256+5+2+7				;87FCh
;+533: 7 bytes of saferam
Cn2_SafeRAM_7b	.equ SavesScreen+512+5+2+14				;8901h
;+540: 3 bytes of Recbytecount
Cn2_Count_Rec	.equ SavesScreen+512+7+14+7				;890Ah
;+543: 3 bytes of Sendbytecount
Cn2_Count_Send	.equ SavesScreen+512+28+3				;890Dh
;+546: 1 byte of non-monopolization for sending
Cn2_Last_Send	.equ SavesScreen+512+28+3+3				;890Eh	;bit 0	;LSB
Cn2_Int_Running	.equ Cn2_Last_Send						;890Eh	;bit 1
Cn2_USB_Plugged	.equ Cn2_Last_Send						;890Eh	;bit 6 and 4
;+547: 1 byte of debug on/off flagging
Cn2_Show_Debug	.equ SavesScreen+512+28+3+3+1			;890Fh
;+548: 2 bytes of pre-interrupt hook
Cn2_Hook_Pre	.equ SavesScreen+512+28+3+3+1+1			;8910h
;;+550: 2 bytes of post-interrupt hook
;Cn2_Hook_Post	.equ SavesScreen+512+28+3+3+1+1+2		;8912h


_GetSerial 						.equ	$807E
Cn2_RAMPage3_Buffer				.equ	$7000
Cn2_RAMPage3_Buffer_HighAddr	.equ	$8000+Cn2_RAMPage3_Buffer
E_EDITF			equ 7 ;allow re-entering application
E_EDIT			equ 1<<E_EDITF
E_Break			equ 6+E_EDIT
_KillUSBDevice		equ 5257h ;this actually recycles the USB connection and re-inits it (I think)
_RecycleUSB		equ 5311h ;identical to 5257h		;Thanks BrandonW
_InitUSB			equ 8108h ;initializes USB hardware as peripheral, sets 5,(iy+1Bh), C set if problems
;810Bh set 1,(81h) and wait (has something to do with USB peripheral kill, but it doesn't actually kill it)

;Cn2_Int_RecBuf:
;0->1->2->3->4->5->6->7->8->|9-10-11->>
;| Sender SID   |Size |Data---->>
;
;Cn2_Int_SendBuf:
;0->1->2->3->4->5->6->7->8->|9-10-11->>
;| Receiver SID |Size |Data---->>
;

.nolist

.list
Cn22_Start_Code:

Cn3_Clear_RecBuf:
	ld hl,Cn2_Int_RecBuf
	jr Cn3_Clear_Buf
Cn3_Clear_SendBuf:
	ld hl,Cn2_Int_SendBuf
Cn3_Clear_Buf:
	push hl
	pop de
	inc de
	ld (hl),0
	ld bc,256+5+2
	ldir
	ret

Cn3_Setdown:
	di
	;call Cn3_Clear_RecBuf
	;call Cn3_Clear_SendBuf
	im 1
	ei
	ret

Cn3_Setup:
	di
	ld hl,SavesScreen
	push hl
		pop de
	push de
		inc de
		ld bc,550-1		;552
		ld (hl),0					;this covers everything incl Cn2_Last_Send
		ldir
		bcall(_ZeroOp1)
		bcall(_Op1ToOp4)
		bcall(_GetSerial)
		ld hl,Op4
		pop de
	inc de
	inc de
	push de
		ld c,0
		ld b,5
Cn3_IDCopyLoop:
		ld a,(hl)
		ld (de),a
		or c
		ld c,a
		inc hl
		inc de
		djnz Cn3_IDCopyLoop
		pop hl
	ld a,c
	or a
	jr nz,Cn3_IDSet
	ld b,5
cn2_ptiserialsetloop:
	push bc
		ld b,250
		call iRandom
		pop bc
	ld (hl),a								;base fake-address is $00,$00,$00,$01,$00
	inc hl
	djnz cn2_ptiserialsetloop
Cn3_IDSet:
	di
	ld hl,Cn2_Caller_Routine
	ld de,$9999
	ld bc,Cn2_Caller_RoutineDone-Cn2_Caller_Routine
	ldir
#ifdef cn2_installerinflash
	in a,(6)
	ld ($9999+Cn2_caller_HomePage-Cn2_Caller_Routine+1),a				;SMC if this code is on a Flash page
#endif cn2_installerinflash
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
	ld a,%00001001		;Acknowledge and disable
	out (3),a
	ld a,%00001011		;Set 1st timer active
	out (3),a
	ld a,%00000110		;Slowest frequency, ~110hz
	out (4),a
	im 2
	ld a,sendCLDL
	out (0),a
	ei
	ret

Cn2_Caller_Routine:                     ;
	di
	ex af,af'							;switch to shadow registers					;1
	exx																				;1
#ifdef cn2_installerinflash
	in a, (2)							;											;2
	and 80h								;%10000000 for fast, %00000000 for slow		;2
	or a
	jr z,Cn2_Caller_Routine_6MHzCalc
	in a,(20h)
Cn2_Caller_Routine_6MHzCalc:
;	rlca								;%00000001 for fast, %00000000 for slow		;1
	push af																			;1
		xor a							;set speed to slow							;1
		out (20h), a					;set the speed								;2

		in a,(6)																	;2
		push af																		;1
Cn2_Caller_HomePage:
			ld a,0						;SMC field									;2
			out (6),a																;2
#endif ;cn2_installerinflash

			call Cn2_Int_Start														;3
#ifdef cn2_installerinflash
			pop af																	;1
		out (6),a																		;2
		pop af																		;1
	out (20h),a							;restore the speed!							;2

#endif ;cn2_installerinflash
#ifdef cn2_detectionworkaround			;THIS CODE IS DISABLED (not defined)
	ld a,%00001001						;Acknowledge and disable					;2
	out (3),a																		;2
	ld a,%00001011						;Set 1st timer active						;2
	out (3),a																		;2
	ld a,%00000000						;Fastest frequency, ~560hz					;2
	out (4),a																		;2
Cn2_NextTickLoop:
	in a,(4)																		;2
	and %00000010																	;2
	jr z,Cn2_NextTickLoop															;2
#endif ;cn2_detectionworkaround
	ld a,%00001001						;Acknowledge and disable					;2
	out (3),a																		;2
;	ld a,e
;	cp 2
;	jr z,Cn2_Caller_Off
	ld a,%00001011						;Set 1st timer active						;2
	out (3),a																		;2
	ld a,%00000110						;Slowest frequency, ~110hz					;2
	out (4),a																		;2
	exx																				;1
	ex af,af'																		;1
	im 2
	ei																				;1
	ret								;-1-8- -1-9- -9- -4-2- 55 bytes total now	;1
Cn2_Caller_RoutineDone:

Cn2_Int_Start:

;	ld hl,Cn2_Int_Running							;3
;	ld a,(hl)										;1
;	and $02											;2
;	ret nz											;2	;interrupt already running!
;	ld a,$02										;2
;	or (hl)											;1
;	ld (hl),a										;1

;	ld hl,cn2MessageTxt
;	call mputsapp

HookPre:
	ld hl,(Cn2_Hook_Pre)
	ld a,h
	or l
	jr z,NoPreHook
	ld de,NoPreHook
	push de
		push hl
			ret
NoPreHook:
	in a, (2)
	rlca 							;Roll bit 7 into carry.
	jr nc,Cn2_Int_USBCableNot84
	and 40h 						;Test bit 5, now rotated into bit 6.
	jr z,Cn2_Int_USBCableNot84
	ld hl,Cn2_USB_Plugged
	ld a,(hl)
	ld c,a
	and %10101111
	ld b,a
	in a,($4D)
	and %01010000
	or b
	ld (hl),a
	
	; Check if we need to reset things (on cable pull)
	xor c
	and %01010000
	jr z,Cn2_Int_USBCableNot84			;Jump over if the USB cable did not just plugged or unplugged
	xor c
	and %01010000
	jr nz,Cn2_Int_USBCableNot84			;Jump over if the USB cable did not just get unplugged
	
	;a is automatically zero here from above
	out ($4C),a
Cn2_Int_ResetUSBWaitReady:
	nop
	in a,($4C)
	and 2
	jr z,Cn2_Int_ResetUSBWaitReady
	ld a,8
	out ($4C),a

Cn2_Int_USBCableNot84:
	
#ifdef cn2debug
	ld a,(Cn2_Show_Debug)
	or a
	jr z,Cn2_Int_Start_NoDebug
	ld hl,gbuf+768-12
	ld a,(hl)
	rlc a
	xor $01
	ld (hl),a
	ld a,(Cn2_Last_Send)
	ld (gbuf+768-2),a
;;	xor a
;	inc hl
;	ld (hl),a
;	inc hl
;	ld (hl),a
Cn2_Int_Start_NoDebug:
#endif

	ld hl,0
	add hl,sp
	ld (Cn2StackSaveWord),hl

	;start with Off stuff
	in a,(4)
	bit 3,a
	jr nz,Cn2_NoOff
#ifdef ONKEYJARGON	
;	ld a,2
;	out ($10),a 						;turn off screen
;   res OnInterrupt,(iy+OnFlags)
;	call Cn2_debounceon
	ld a,(Cn2_USB_Plugged)
	and %01010000
	jr z,Cn2_Off_NoKillUSBPower
	ld a,2
	out ($54),a
Cn2_Off_NoKillUSBPower:
	ld	a,%00000001
	out	(3),a							;interrupt only on ON key
	set 3,(iy+$12)						;[2nd] pressed (fake it)
	exx
	ex af,af'
	im 1
	call $0038							;call TI-OS interrupt to handle poweroff
	di
	exx
	ex af,af'
;	res OnInterrupt,(iy+OnFlags)
;	ld a,3
;	out ($10),a ;turn on screen
	ret
;	jp Cn2_debounceon		
#endif									;interrupt caller will fix the port 3/4 values
Cn2_NoOff:

	ld a,(Cn2_Int_RecBuf+6)										;see if there's already data the user program didn't retrieve
	and %10000000
	jp nz,Cn2_Int_CheckPendSend

	ld a,(Cn2_USB_Plugged)
	and %01010000
	jp z,Cn2_Int_ReceiveIO										;check if we're in USB or I/O mode
Cn2_Int_ReceiveFrame_USB:
	bit 5,(iy+41h)
	jp z,Cn2_Int_CheckPendSend
	ex af,af'
	exx
	push hl
		push de
			push bc
				push af
					im 1
					ld hl,Cn2_Int_RecBuf
					ld de,0
					jr Cn2_Int_ReceiveFrame_USBDataWaiting
Cn2_Int_ReceiveFrame_USBInner:
					ei
					halt
					di
					in a,(4)
					bit 3,a
					jr z,Cn2_Int_ReceiveFrame_USBFail
					bit 5,(iy+41h)
					jr z,Cn2_Int_ReceiveFrame_USBInner
Cn2_Int_ReceiveFrame_USBDataWaiting:
					ld a,2
					out (8Eh),a
					in a,(94h)
					bit 6,a
					jr z,Cn2_Int_ReceiveFrame_USBReceive1
					and 0DFh
					out (94h),a
					pop af
					bcall(_KillUSBDevice)
					jr Cn2_Int_ReceiveFrame_USBFail
Cn2_Int_ReceiveFrame_USBReceive1:
					in a,(96h)
					ld b,a
Cn2_Int_ReceiveFrame_USBLoop2:
					in a,(0A2h)
					ld (hl),a
					inc hl
					ld a,d
					or a
					jr nz,Cn2_Int_ReceiveFrame_USBLoop2_NotHeader
					ld a,e
					cp 5
					jr nc,Cn2_Int_ReceiveFrame_USBLoop2_NotHeader
					ld hl,Cn2_Int_RecBuf
Cn2_Int_ReceiveFrame_USBLoop2_NotHeader:
					inc de
					djnz Cn2_Int_ReceiveFrame_USBLoop2

					ld a,2
					out (8Eh),a
					in a,(94h)
					and 0FEh
					out (94h),a
					res 5,(iy+41h)
					ld a,0A1h
					ld (8Bh),a
					push hl
						ld hl,(Cn2_Int_RecBuf+5)
						ld a,%01111111
						and h
						ld h,a
						ld bc,12
						add hl,bc
						or a
						sbc hl,de
						ld a,h
						or l
						pop hl

					ei						;this code MUST preserve z flag
					ld a,1
					out (5Bh),a
					res 0,(iy+41h)

					jr nz,Cn2_Int_ReceiveFrame_USBInner
Cn2_Int_ReceiveFrame_USBFail:
					di
					pop af
				pop bc
			pop de
		pop hl
	ex af,af'
	exx
	jp Cn2_Int_RecCountUp

Cn2_Int_ReceiveIO:
	ld hl,6000/58
Cn2_Int_CheckPendRec:
	dec hl													; 6

	in a,(0)												;11
;#ifdef cn2debug											;messing up my timing.  commented out.
;		push hl
;			ld hl,gbuf+727-12
;			ld (hl),a
;			pop hl
;#endif

	and ClockMask											; 7
	cp ClockLow								;1 = low			; 7
	jp z,Cn2_Int_CheckPendSend								; 7
	ld a,h													; 4
	or l													; 4
	jr nz,Cn2_Int_CheckPendRec								;12	;total here: 58 cycles
	;if we get here, then a device is indeed preparing to send
	ld hl,(55000-6000)/58									;I believe this should be...
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
	ld a,(Cn2_Show_Debug)
	or a
	jr z,Cn2_Int_Receive_NoDebug
	ld hl,gbuf+768-11
	ld a,(hl)
	rlc a
	xor $01
	ld (hl),a

;	ld hl,gbuf+727
;	ld a,(hl)
;	or %01000000
;	ld (hl),a
Cn2_Int_Receive_NoDebug:
#endif
	ld hl,Cn2_Int_RecBuf+5+1
;	ld b,(hl)
;	inc hl
	ld a,(hl)
	and %10000000
;	and %01111111
;	or b

	;jp nz,Cn2_Int_GetByte_PopFail						;this forces the collision
	ld b,0
	ret nz												;why no collision? Don't want to stop others' packets
	
;#ifdef cn2debug
;	ld hl,gbuf+727
;	ld a,(hl)
;	or %00100000
;	ld (hl),a
;#endif

	ld b,5
	ld c,0											;for checking for broadcast
	ld hl,Cn2_SafeRAM_7b
	push hl
Cn2_Int_ReceiveFrame_SIDRec
		push bc
			push hl
				call Cn2_Int_GetByte
				pop hl
			pop bc
		ld (hl),a
		or c
		ld c,a											;together these add 8 total cycles per iteration
		inc hl
		djnz Cn2_Int_ReceiveFrame_SIDRec
		pop hl											;10
	ld a,c
	or a
	jr z,Cn2_Int_ReceiveFrame_SIDCleared			;if SID[0] | SID[1] | SID[2] | SID[3] | SID[4] == 0, it's broadcast
	ld de,SIDSave									;10
	ld b,5											;7
Cn2_Int_ReceiveFrame_SIDVerify:
	ld a,(de)										;7
	cp (hl)											;7
	jr nz,Cn2_Int_CheckPendSend						;7
	inc hl											;6			;total is 27+5*(46) = 257 cycles for comparison
	inc de											;6
	djnz Cn2_Int_ReceiveFrame_SIDVerify				;13			;if we get past here, it's for us
	ld a,1
Cn2_Int_ReceiveFrame_SIDCleared:
	push af											;save whether it's broadcast
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
;		pop af
;	push bc
;		push af
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
		pop af
	or a
	jr z,Cn2_Int_ReceiveSuccess_Broadcast
		
			
	call Cn2_Int_Calc_Checksum_Rec			;returns hl
	ld a,l
	push hl
		call Cn2_Int_SendByte
		pop hl
	ld a,h
	call Cn2_Int_SendByte
	call Cn2_Int_GetByte
;	pop hl
	cp $AA										;if not AA, wasn't ack'd
	jr nz,Cn2_Int_CheckPendSend

Cn2_Int_ReceiveSuccess_Broadcast:
Cn2_Int_RecCountUp:
	ld hl,Cn2_Int_RecBuf+6
	ld a,(hl)
	or %10000000
	ld (hl),a

Cn2_Int_CheckPendSend:
	ld hl,Cn2_Last_Send					;\ 
	ld a,(hl)							;|
	xor 1
	ld (hl),a							;|- don't send two cycles in a row
	and 1								;|
	jp nz,Cn2_Int_Done_QuitOK			;/
	;ld (hl),1
	ld hl,(Cn2_Int_SendBuf+5)
	ld a,h
	bit 7,a
	jp z,Cn2_Int_Done_QuitOK
	and %01111111
	or l
	jp z,Cn2_Int_Done_QuitOK
	ld a,(Cn2_USB_Plugged)
	and %01010000
	jp nz,Cn2_Int_SendFrame_USB
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
Cn2_Int_SendFrame_FailSendBusyNet:
	ld e,1
	ret
Cn2_Int_SendFrame_ReadyToSend:
	;INSERT SEND ROUTINE HERE
	;first the 55000-cycle hold of the clock high
#ifdef cn2debug
	ld a,(Cn2_Show_Debug)
	or a
	jr z,Cn2_Int_Send_NoDebug
	ld hl,gbuf+768-10
	ld a,(hl)
	rlc a
	xor $01
	ld (hl),a

;	ld hl,gbuf+727
;	ld a,(hl)
;	or %00001000
;	ld (hl),a
Cn2_Int_Send_NoDebug:
#endif
	ld hl,55000/44										;2273*44 = 100012 - but only 6m/110 = 55K necessary
Cn2_Int_SendFrame_ReadyToSendAlert:
	dec hl													; 6
	ld a,SendCHDL											; 7
	out (0),a												;11
	ld a,h													; 4
	or l													; 4
	jr nz,Cn2_Int_SendFrame_ReadyToSendAlert				;12 Total: 44
	;44x2273 = 100,012 cycles

;#ifdef cn2debug
;	ld hl,gbuf+727
;	ld a,(hl)
;	or %00000100
;	ld (hl),a
;#endif

	ld b,5
	ld c,0								;for broadcast check
	ld hl,Cn2_Int_SendBuf
Cn2_Int_SendFrameAddr1:
	push hl
		ld a,(hl)
		or c
		ld c,a
		ld a,(hl)
		push bc
			call Cn2_Int_SendByte
			pop bc
		pop hl
	inc hl
	djnz Cn2_Int_SendFrameAddr1
	
	;NEED TO PAUSE!
	ld b,20
Cn2_Int_SendFrameAddr1Pause:				;give the receiver at least 257 cycles to compare
	djnz Cn2_Int_SendFrameAddr1Pause		;this gives 13*19 + 7 + 7 = 261 cycles, good enough

	ld a,c									;a = c = 0 for broadcast
	push af
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
		pop af
	or a
	jr z,Cn2_Int_SendSuccess				;Broadcast means no checksum

	call Cn2_Int_Calc_Checksum_Send			;returns hl
	push hl
		call Cn2_Int_GetByte
		ld l,a
		push hl
			call Cn2_Int_GetByte
			pop hl
		ld h,a
		pop de
	;push hl				;4/6/12
		or   a
		sbc  hl, de
	;	pop  hl				;4/6/12
	;ld e,0
	ret nz

	ld a,$AA
	call Cn2_Int_Sendbyte

	;add to Cn2_Count_Rec
	ld hl,Cn2_Int_Sendbuf+6
	ld a,(hl)
	push af
		dec hl
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
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

Cn2_Int_SendSuccess:
	ld hl,0
	ld (Cn2_Int_Sendbuf+5),hl
;	ld (hl),0
;	push hl
;		pop de
;	inc de
;	ld bc,5+2+256-1
;	ldir
Cn2_Int_Done_QuitOK:
	ld e,0
	
;	ld a,(Cn2_USB_Plugged)
;	and %01010000
;	jr z,Cn2_Int_Done_TIOSInterrupt

	in a,($55)
	cpl \ and $1f
	jr nz,Cn2_Int_Done_TIOSInterrupt	
	in a,($56)
	or a
	jr z,Cn2_Int_Done_NoTIOSInterrupt
	
	;The following code lets the TI-OS interrupt run when there's USB data to deal with
Cn2_Int_Done_TIOSInterrupt:
	ex af,af'							;switch to non-shadow registers				;1
	exx																				;1	
	im 1
	call $0038							;call TI-OS interrupt
	di
	ex af,af'							;switch to shadow registers					;1
	exx																				;1	
Cn2_Int_Done_NoTIOSInterrupt:
;	ld hl,Cn2_Int_Running
;	ld a,%11111101
;	and (hl)
;	ld (hl),a							;this interrupt no longer running
	ret	

Cn2_Int_SendFrame_USB:

	ex af, af'
	exx								;swap to normal registers
	push af							;save them
		push bc
			push de
				push hl
					push ix
						in a,($05)							;$C000 port
						ld b,a
						ld a,3
						out ($05),a
						push bc								;will pop into af, and this is on RAM PAGE 3!!
							ld hl,(Cn2_Int_SendBuf+5)
							ld a,%01111111
							and h
							ld b,a
							ld c,l
							push bc
								ld hl,Cn2_Int_SendBuf
								ld de,Cn2_RAMPage3_Buffer_HighAddr
								ld bc,5
								ldir
								push hl
									ld hl,SIDSave
									ld bc,5			;de = Cn2_RAMPage3_Buffer_HighAddr + 5
									ldir
									pop hl
								pop bc
							push bc
								inc hl
								ld a,(hl)
								and %01111111
								ld (hl),a
								dec hl
								inc bc			;hl = Cn2_Int_Sendbuf + 5
								inc bc			;de = Cn2_RAMPage3_Buffer_HighAddr + 10
								ldir
								pop de
							ld hl,12			;Receiver | Sender | Size
							add hl,de
							ex de,hl
							pop af
						out ($05),a
						xor a
						out ($33),a
						ld a,$43
						out ($33),a
						ld a,$3
						out ($34),a
						ld hl,$0014
						ld ($90AA),hl						;timeout counter
						ld l,a
						out ($35),a
						res 0,(iy+43h)						;timeout flag
						res 3,(iy+41h) ;set 3,(iy+41h)						;force it not to kill the USB connection??
						ld hl,Cn2_RAMPage3_Buffer+$4000
						im 1
						ei
						bcall(50F2h)		;SendUSBData
						di
						rlc a				;rotate carry into bit 0
						ld (Cn2_SafeRAM_7b),a
						xor a
						out ($33),a   ;Turn off Timer 2
						out ($34),a
						res 3,(iy+41h)						;let it resolve issues with the host again
						pop ix
					pop hl				;restore normal registers
				pop de
			pop bc
		pop af
	ex af, af'							;save to shadow registers
	exx
	res 0,(iy+41h)
	res 5,(iy+41h)
	ld a,(Cn2_SafeRAM_7b)
	rr a								;roll bit 0 back to carry
	jp c,Cn2_Int_Done_QuitOK
	jp Cn2_Int_SendSuccess
	
Cn2_Int_GetByte:
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
	ld hl,40
Cn2_Int_GetByte3:
	dec hl									;6
	in a,(0)								;11
	and LinkMask							;7
	cp GetCLDL								;7
	jr z,Cn2_Int_GetByte4					;7
	ld a,h									;4
	or l									;4
	jr z,Cn2_Int_GetByte_PopFail			;7
	jr Cn2_Int_GetByte3						;12 -->total: 65 cycles + 10 for initial set; max = 2610
;	ret
Cn2_Int_GetByte4:
;	ld c,0					;this is where the data found will be stored
;	ld b,8					;how many bits to get
	ld bc,(8*256)+0
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
	ld e,sendCLDL
	;ld c,0
	;ld b,256
	ld bc,(256*256)+0
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
	cpl							;was xor $ff
	ld b,8
Cn2_Int_SendByteRev:
	srl a
	rl c
	djnz Cn2_Int_SendByteRev
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
	ld hl,12
	call Cn2_Int_SendByte_PauseHL
	sra c
	djnz Cn2_Int_SendByte_OuterLoop
	ret
Cn2_Int_SendByte_DetectCollision:
	ld sp,(Cn2StackSaveWord)

#ifdef cn2debug
	ld hl,gbuf+768-1
	ld a,(hl)
	cpl								;was xor $ff
	ld (hl),a
#endif

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
	ld hl,Cn2_Int_RecBuf+5				;10
	jr Cn2_Int_Calc_Checksum			;10
Cn2_Int_Calc_Checksum_Send:
	ld hl,Cn2_Int_SendBuf+5
Cn2_Int_Calc_Checksum:
	ld c,(hl)							;7
	inc hl								;6
	ld a,(hl)							;7
	and %01111111						;7
	ld b,a								;4
	inc hl								;6
	ld de,0								;10		;max is 67 to here
Cn2_Int_Calc_ChecksumLoop:
	ld a,(hl)							; 7
	push hl								;11
		ld h,0								; 7
		ld l,a								; 4
		add hl,de							;11
		ex de,hl							; 4
		pop hl								;10
	inc hl								; 6
	dec bc								; 6
	ld a,b								; 4
	or c								; 4
	jr nz,Cn2_Int_Calc_ChecksumLoop		;12 ; TOTAL: 74	(7 for final nonjump)
	ex de,hl							;4
	ret									;10	;14+67+7 = 88 base

Cn2_debounceon:
	ld b,16
Cn2_debounceoninner:
	in a,(4)
	bit 3,a
	jr z,Cn2_debounceon
	djnz Cn2_debounceoninner
	ret

Cn2_Int_End:
;.echoln "CALCnet2.2 code is ",$-Cn22_Start_Code," bytes long"
