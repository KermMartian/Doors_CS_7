;CELTIC major revision 3 by Rodger Weisman
;This software is licensed in accordance to the attached
;ReadMe file, however perverted it may be.
;Subroutines "standard" set
;Read and write all inputs and outputs from Op6
;ConvertTtoH:    Converts text to hex
;                Four digit hex number starting at Op6
;                HL=number found at Op6
;ConvertSumSub:  Subroutine.
;                Hex digits at (de)
;                A=Number found at (de), DE points at next set of digits.
;ConvertHtoT1b:  Converts a number in a register to hex digits
;                A=number to convert
;                (Op6) represents A in ASCII hex. Not zero-terminated.
;ConvertHtoT2b:  Converts a (larger) number to hex digits.
;                HL=number to convert
;                (Op6) represents HL in ASCII hex. Not zero-terminated.
;NumTextInA:     Number to text by nibble. No bounds checking made.
;                A=low nibble of binary number
;                A=ASCII hex digit of that low nibble
;TextNumInA:     Text to number by nibble.
;                A=ASCII hex digit to convert back.
;                A=low nibble of that digit in binary.
;fastcopy:       Duh.
;                Uh.
;                Fail.
;cleardisplay:   Sets cursor to top-left position and clears the buffer.
;                No.
;                But it does destroy all registers. Temp1 is gone.
;clearbuf:       Just clears the buffer.
;                Maybe.
;                It also destroys. Everything. temp1 is also destroyed.
;getKBD:         Retrieves a keycode.
;                Your finger on a button.
;                A=keycode pressed.
;pressanykey:    Waits for a keypress. Any keypress.
;                A button or two mashed.
;                The continuation of the program. And maybe A=keycode.
;PutSN:          B-delimited display of string in smallfont. Guess.
;                B=#ofcharstodisplay;HL=pointertostring.
;                B=0;HL=theaddressafterthatend.
;PutMapper:      Displays a character in jacked-up small font.
;                A=character to display, along with a memory pointer at... Hm.
;                Buffer updated. Update the screen on your own time.
;PutC:           Displays a zero-terminated string with a few extras.
;                HL=pointer to zero-terminated string. See string codes.
;                HL=address after that zero.
;Mul72:          Cheaply multiplies A by 72. Results in HL.
;                A=number
;                HL=A*72. Good for lineskipping.
;ION routines other than FastCopy has been put into this source
;
;
;ix - Sprite
;b  - height
;c  - width in bytes
;d  - x
;e  - y

;...
;And your standard ION libs except for the already-mentioned fastcopy.
;Enjoy.
ConvertTtoH:  ;text to hexadecimal HL
	push de
		push bc
			ld de,Op6
ConvertTtoHLoop:
			call ConvertSumSub
			ld h,a
			call ConvertSumSub
			ld l,a
			pop bc
		pop de
	ret

ConvertSumSub:
	call convertSub
	and 00001111b
	rlca \ rlca \ rlca \ rlca
	ld b,a
	call convertSub
	and 00001111b
	or b
	ret

ConvertSub:
	ld a,(de)
	inc de

TextNumInA:
	cp 'A'
	jr c,$+4
	sub 7
	sub '0'
	ret

ConvertHtoT1b:  ;hexadecimal to text in Op6 from A. Preserves ALL.
	push af \ push hl \ push de
	ld hl,Op6
	ld (hl),a
	xor a \ rld \ ld e,a \ rld \ ld d,a  \ rld
	; ld hl,Op6
	ld a,e
	call ConvertHtoTSub
	ld a,d
	call ConvertHtoTSub
	pop de \ pop hl \ pop af
	ret

ConvertHtoT2b:  ;hexadecimal to text in Op6 from HL. Preserves ALL. Call to convert 2 bytes of Hexadecimal
	push af \ push hl \ push de
	ld a,L
	call ConvertHtoT1b
	ld a,h
	ld hl,(Op6)
	ld (Op6+2),hl
	call ConvertHtoT1b
	xor a
	ld (Op6+4),a
	pop de \ pop hl \ pop af
	ret

ConvertHtoTSub:
	call NumTextInA
	ld (hl),a
	inc hl
	ret

NumTextInA:
	add a,'0'
	cp '9'+1
	ret c
	add a,7
	ret

;bit 7 is set in ($10) if the LCD is busy
fastCopy:
	jp immFastCopy									;Use on-page DCS ion routine

cleardisplay:
	ld hl,0
	ld (currow),hl
clearbuf:
	di
	push hl
	push bc
	ld (temp1),SP
	ld SP,plotsscreen+$300
	ld b,64
clearbufcommon:
	ld hl,0
	push hl
	push hl
	push hl
	push hl
	push hl
	push hl
	djnz $-6
	ld SP,(temp1)
poptwothenret:    ;Yeah. Using these chunks of code over again for default position routines
	pop bc
pophlthenret:
	pop hl
donothing:
	ret

getKBD:  ;destroys registers AF and BC
	xor a
	call getKBDSC1
	xor $FF
	ret z
	ld bc,$7F00
getKBDSC2:
	rlc b
	ld a,b
	bit 7,a
	jr z,getKBDSCE
	call getKBDSC1
	cp $FF
	jr nz,getKBDSC3
	ld a,8
	add a,c
	ld c,a
	jr getKBDSC2
getKBDSC3:
	bit 0,b
	jr z,getKBDSC6   ;autotest for multiple arrow keypress
	inc c
	rrca
	jr c,$-2
	set 7,a
	cp $FF
	jr nz,getKBDSCE  ;Checking all keys in this group.
getKBDSC4:        ;Checking all other groups for keypress.
	rlc b
	ld a,b
	jr nc,getKBDSC5
	call getKBDSC1
	cp $FF
	jr nz,getKBDSCE
	jr getKBDSC4
getKBDSC5
	ld a,c
	ret
getKBDSCE:
	xor a
	scf
	ret
getKBDSC1:
	out (1),a
	push af   ;delay for keyboard to respond
		pop af
	in a,(1)
	push af
		ld a,$FF
		out (1),a
		pop af
	ret
getKBDSC6:
	push hl
		cpl
		and $0F
		ld l,a
		ld h,0
		ld bc,getKBDSC7
		add hl,bc
		ld a,(hl)
		pop hl
	ret
getKBDSC7:
	;down--------\
	;left-------\|
	;right-----\||
	;up-------\|||
	.db  00  ;0000 0
	.db  01  ;0001 1
	.db  02  ;0010 2
	.db  05  ;0011 3
	.db  03  ;0100 4
	.db  06  ;0101 5
	.db  57  ;0110 6
	.db  58  ;0111 7
	.db  04  ;1000 8
	.db  59  ;1001 9
	.db  07  ;1010 A
	.db  60  ;1011 B
	.db  08  ;1100 C
	.db  61  ;1101 D
	.db  62  ;1110 E
	.db  63  ;1111 F
;For additional keycodes,
;$96=150d
;150 = left/right
;151 = left/right/down
;152 = up/down
;153 = up/left/down
;154 = up/right/down
;155 = up/right/left
;156 = all keys mashed


;textFlags  EQU    5    ;Text output flags
;textEraseBelow EQU    1    ; 1=erase line below small char
;textScrolled   EQU    2    ; 1=screen scrolled
;textInverse    EQU    3    ; 1=display inverse bit-map
;textInsMode    EQU    4    ; 0=overstrike, 1=insert mode
textEraseAbove EQU    7    ; 1=erase line above small char (custom-made for routine)

mul72:
	ld l,a \ add a,a \ add a,a \ add a,a \ add a,l \ add a,a
	ld l,a \ ld h,0 \ add hl,hl \ add hl,hl
	ret
;=================================================================================================
;=================================================================================================
;=================================================================================================

;===================================================
;ION LIBRARY and possible installer material.
#define randData    cmdshadow+(9*3)
