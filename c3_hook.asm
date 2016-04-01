;CELTIC major revision 3 by Rodger Weisman
;This software is licensed in accordance to the attached
;ReadMe file, however perverted it may be.
;Subroutines hook handler and installer

;Version O drops the hook chaining ability, seeing as it is no longer needed.
;Besides not being needed, it slows down the TI-OS. A LOT.

;=========================================================
CIIISpriteMask:   ;guaranteed to not require MSB changing here.
.db 00000001b
.db 00000011b
.db 00000111b
.db 00001111b
.db 00011111b
.db 00111111b
.db 01111111b
.db 11111111b
CIIILogicSeries:
 inc b            ;entry to survive instr inc to something equally useless.
 and d
 or  d
 xor d
CIIIDrawLineType:
.db 1
.db 0
.db 2
;=========================================================

myflag equ asm_flag1
HitEOF equ 0  ;EOF during getnextline reached
HitSOF equ 1  ;SOF during getprevline reached
HitEOL equ 2  ;End of line encountered
ConEOF equ 3  ;Set if trying to read after HitEOL is set.
gList  equ 4  ;set to enable get list names
DoTest equ 5  ;Run routine without writing data
Hidden equ 6  ;is set if variable is "hidden"
gGroup equ 7  ;set to enable get group name
nflags equ asm_flag2
nexpon equ 0  ;set exponential write
xlcomp equ 1  ;set xlib compatibility status for error system
mode16 equ 2  ;set 16-bit mode for applicable functions
isneg  equ 3  ;set if input number is negative for final conversion
noimg  equ 4  ;set if image is in savesscreen already. Used for passthrough
overw  equ 5  ;set if files should be overwritten if it already exists
xsprt  equ 6  ;if set, do not use xLIB-only sprite buffering routine
strn16 equ 7  ;if set, treat hex strings as words instead of bytes
;appbackupscreen equ $9872 ;used here for debugging w/o ti83plus.inc
temp1  equ appbackupscreen+00
temp2  equ appbackupscreen+02
temp3  equ appbackupscreen+04
temp4  equ appbackupscreen+06
temp5  equ appbackupscreen+08
temp6  equ appbackupscreen+10
temp7  equ appbackupscreen+12
temp8  equ appbackupscreen+14
temp9  equ appbackupscreen+16
tempA  equ appbackupscreen+18
tempB  equ appbackupscreen+20
tempC  equ appbackupscreen+22
SOF    equ appbackupscreen+24  ;2adr,1pg,1buf. start of file
EOF    equ appbackupscreen+28  ;ditto above. end of file
curpos equ appbackupscreen+32  ;... current cursor position. +3=0 if in RAM
page   equ appbackupscreen+36  ;Page number for when exiting or for var in arc
stack  equ appbackupscreen+37  ;2b stack address for restoring at end of prog
pline  equ appbackupscreen+39  ;2adr,1pg,1buf. Position of previously read line
memdel equ appbackupscreen+43  ;2bytes. Amount of data deleted
memins equ appbackupscreen+45  ;2bytes. Amount of data inserted. To be compared.
gSOF   equ appbackupscreen+49  ;4bytes. Address of grouped start of file + page
gEOF   equ appbackupscreen+53  ;4bytes. Address of grouped end of file + page
gSIZE  equ appbackupscreen+57  ;2bytes. Size of the grouped entry
nsize  equ appbackupscreen+59  ;1byte. Size of name entry in OP1. Excludes type
ipart  equ appbackupscreen+60  ;2bytes. Imaginary part to any complex part input
tstrn  equ appbackupscreen+62  ;5bytes. Temporary storage of 5-digit number
cpage  equ appbackupscreen+67  ;1byte. Refers to the currently accessed page
txtind equ appbackupscreen+68  ;2bytes. Holds offset in bytes to start of buffer
txtbuf equ appbackupscreen+70  ;12bytes. Holds data for characters while processing
sc1    equ appbackupscreen+82  ;9bytes, first entry from OP stack
sc2    equ appbackupscreen+91  ;9bytes, second entry from OP stack
sc3    equ appbackupscreen+100 ;9bytes, third entry from OP stack
sc4    equ appbackupscreen+109 ;9bytes, fourth entry from OP stack
var0   equ appbackupscreen+118 ;2bytes, first number popped off of the FPS
var1   equ appbackupscreen+120 ;2bytes, second number popped off of the FPS
var2   equ appbackupscreen+122 ;2bytes, third number popped off of the FPS
var3   equ appbackupscreen+124 ;2bytes, fourth number popped off of the FPS
var4   equ appbackupscreen+126 ;2bytes, fifth number popped off of the FPS
var5   equ appbackupscreen+128 ;2bytes, sixth number popped off of the FPS
var6   equ appbackupscreen+130 ;2bytes, seventh number popped off of the FPS
var7   equ appbackupscreen+132 ;2bytes, eigth number popped off of the FPS
var8   equ appbackupscreen+134 ;2bytes, ninth number popped off of the FPS
var9   equ appbackupscreen+136 ;2bytes, tenth number popped off of the FPS
varA   equ appbackupscreen+138 ;2bytes, eleventh number popped off of the FPS
varB   equ appbackupscreen+140 ;2bytes, twelfth number popped off of the FPS
varC   equ appbackupscreen+142 ;2bytes, thirteenth number popped off of the FPS
varD   equ appbackupscreen+144 ;2bytes, fourteenth number popped off of the FPS
args   equ appbackupscreen+146 ;2bytes, number of arguments placed on the stack
nargs  equ appbackupscreen+148 ;1byte, number of number arguments on the FPS
sargs  equ appbackupscreen+149 ;1byte, number of string arguments on the FPS
rpart  equ appbackupscreen+150 ;2bytes, real part to the number thingie. *sigh*
ostrng equ appbackupscreen+152 ;9bytes, name of the output string
flgadr equ appbackupscreen+161 ;2bytes, address of myflag flag.
cmask  equ appbackupscreen+163 ;1byte, mask for compression stuffs.
tSOF   equ appbackupscreen+164 ;2bytes, temporary spot for SOF juggling
ltype  equ appbackupscreen+166 ;1byte, type of last argument (first OP1)
strlen equ appbackupscreen+167 ;1byte, 0 to 255 length of temporary string
gSOFp  equ appbackupscreen+168 ;4bytes. Address of grouped file neglecting size.
defend equ appbackupscreen+168 ;defines end here for next set of defines.
;WARNING: An interrupt table is located at $9A00, allowing for a maximum
;offset from appbackupscreen to end at $9872 + $018E (+398)

blow  equ defend+1   ;lowest byte
bhigh equ blow+1     ;highest byte
bnull equ bhigh+1    ;byte not used
bttl  equ bnull+1    ;# of bytes on table. 00 = 256
cmeth equ bttl+1     ;compression method #. Whenever it's supported...
size2 equ cmeth+1    ;temporary storage of size bytes
ctbl  equ size2+1    ;start of compression stats table
;
;overlapping addresses for xLIB compat only (few CIII functions running)
;xlib01 equ $98CD   ;for direct debugging purposes (emulator memory map)
xlib01 equ sc2      ;argument memory location for this
xlib02 equ xlib01+2
xlib03 equ xlib02+2
xlib04 equ xlib03+2
xlib05 equ xlib04+2
xlib06 equ xlib05+2
xlib07 equ xlib06+2
xlib08 equ xlib07+2
xlib09 equ xlib08+2
xlib10 equ xlib09+2
xlib11 equ xlib10+2
xlib12 equ xlib11+2
xlib13 equ xlib12+2
xlib14 equ xlib13+2
xlib15 equ xlib14+2

;========================================

BASICParserHook:
	.db $83      ;byte required for TIOS to recognize hook location
	;$8A8A=xLIBHookStart ; $B3B3=HandleMyHookDet ; $B4B4=HandleMyHookId
	or a
	ret z
	cp 2
	jr z,HookHandlerStopCheck
	ld a,$8A
	cp b \ jr z,HookHandlerCont
	ld a,$B3
	cp b \ jr z,HookHandlerCont
	inc a								;$B4
	cp b \ jr z,HookHandlerCont
#ifdef dcsblibs
	inc a \ inc a						;$B6
	cp b \ jr z,HookHandlerCont
#endif
	xor a
	ret
BASICParserHookSimple:
	.db $83      ;byte required for TIOS to recognize hook location
	or a
	ret z
	cp 1
	ret z
HookHandlerStopCheck:
	ld a,$D9-$CE
	cp b \ jr z,HookHandlerStop
	xor a
	ret
HookHandlerStop:
	ld a,%01111111							;ErrStop
	b_jump(_JError)

HookHandlerCont:
	push hl        ;storing arguments to stack
		ld a,$8A
		cp c \ jp z,xLIBHookStart
		ld a,$B3
		cp c \ jr z,HandleMyHookDet
		inc a								;$B4
		cp c \ jp z,HandleMyHookId
#ifdef dcsblibs
		inc a \ inc a						;$B6
		cp c \ jp z,HandleMyHookMenu
#endif
		pop hl         ;if reached here, then the args are not needed.
	xor a
	ret

HandleMyHookDet:
;res numOP1,(iy+ParsFlag2) if need to preserve Ans and return no value
		pop hl
	xor a
	ld (IY+myflag),a
	ld (IY+nflags),a
	ld a,(Op1)
	ld (ltype),a
	cp $02   ;matrix type
	jr nz,ContinueHandleDet
	dec hl
	ld a,l
	or h
	inc hl
	jp z,SkipHandleDet  ;one argument matrix indicates legit function.
ContinueHandleDet:
	push hl
		call C3_ChkDirtyFlag
		pop hl
	ld de,CelFuncTable
	ld bc,(CelFuncTableEnd-CelFuncTable)/2
	di
	jp popallargs
 
SkipHandleDet:
	push hl
		push bc
			rst 10h
			ex de,hl
			ld a,(hl)
			dec a
			jr nz,SHDet2
			inc hl
			ld a,(hl)
			dec a
			jr nz,SHDet2
			inc hl
			inc hl
			inc hl
			ld de,0
			ld a,(hl)
			cp $42
			jr nz,SkipHandleDetZero
			ld de,1337d
SkipHandleDetZero:
			ex de,hl
			bcall(_SetXXXXOp2)
			bcall(_Op2ToOp1)
			pop bc
		pop hl
	ld a,1
	or a
	ret
SHDet2:
		pop bc
	pop hl
	xor a
	ret

CelFuncTable:
	.dw fToggleSet    ;.db 1 ;00  condensed command
	.dw fNumToString  ;.db 1 ;01
	.dw fGetListElem  ;.db 2 ;02
	.dw fGetArgType   ;.db 1 ;03
	.dw fChkstats     ;.db 1 ;04
	.dw fLineRead     ;.db 2 ;05
	.dw fLineWrite    ;.db 3 ;06
	.dw fLineErase    ;.db 2 ;07
	.dw fLineReplace  ;.db 4 ;08
	.dw fFindProg     ;.db 1 ;09
	.dw fUngroupFile  ;.db 1 ;10
	.dw fGetGroup     ;.db 1 ;11
	.dw fExtGroup     ;.db 2 ;12
	.dw fGroupMem     ;.db 2 ;13
	.dw fBinRead      ;.db 3 ;14
	.dw fBinWrite     ;.db 3 ;15
	.dw fBinDelete    ;.db 3 ;16
	.dw fStringToBin  ;.db 1 ;17
	.dw fBinToString  ;.db 1 ;18
	.dw fastcopy      ;.db 1 ;19  ;cheap trick to access FastCopy routine
	.dw fExecHex      ;.db 1 ;20
	.dw fEdit1Byte    ;.db 2 ;21
	.dw fModeChange   ;.db 3 ;22
	.dw fIndexFile    ;.db ? ;23
	.dw fLookupIndex  ;.db ? ;24
	.dw fErrorHandle  ;.db ? ;25
	.dw fMatToStr     ;.db ? ;26
	.dw fStringRead   ;.db ? ;27
	.dw fHexToDec     ;.db ? ;28
	.dw fDecToHex     ;.db ? ;29
	.dw fEditWord     ;.db ? ;30
	.dw fBitOperate   ;.db ? ;31
	.dw fGetProgList  ;.db ? ;32
	.dw fDoLink       ;.db ? ;33
	.dw fOnBlock      ;.db ? ;34
CelFuncTableEnd:

;======================================
#ifdef dcsblibs
HandleMyHookMenu:
;res numOP1,(iy+ParsFlag2) if need to preserve Ans and return no value
		pop hl
	xor a
	ld (IY+myflag),a
	ld (IY+nflags),a
	ld de,Op1
	ld a,(de)
	ld (ltype),a
	ld a,l
	cp 4
	jr nc,ContinueHandleMenu
	cp 1
	ld a,(de)
	jr z,HandleMyHookMenuCheckType
	push hl
		dec hl
		add hl,hl
		add hl,hl
		add hl,hl
		pop de
	push de
		dec de
		add hl,de
		ld de,(FPS)
		ex de,hl
		or a \ sbc hl,de
		ld a,(hl)
		pop hl
HandleMyHookMenuCheckType:
	cp $01   ;list type		-> it means it's a list, so it's a real sum()
	jr nz,ContinueHandleMenu
	xor a
	ret		;process it like a real -m-e-n-u- sum()
ContinueHandleMenu:
	push hl
		call C3_ChkDirtyFlag
		pop hl
	ld de,DCSBLib_FuncTable
	ld bc,(DCSBLib_FuncTableEnd-DCSBLib_FuncTable)/2
	di
	jp popallargs

DCSBLib_FuncTable:
	.dw dbfStringWidth
	.dw dbfPxScan
	.dw dbfHometoGraph
	.dw dbfUsedPicList
	.dw dbfArcUnarcPic
	.dw dbfDCSLibVersion
	.dw dbfSimpleGUIMouse
	.dw dbfPushGUIStack
	.dw dbfPopGUIStack
	.dw dbfOpenGUIStack
	.dw dbfCloseGUIStack
	.dw dbfRenderGUIStack
	.dw dbfGUIMouse
	.dw dbfMenu
	.dw dbfPushAns
	.dw dbfPopAns
	.dw dbfClearAnsStack
	.dw dbfCn2BASICSend
	.dw dbfCn2BASICGet
	.dw dbfCn2BASICStop
	.dw dbfCn2BasicStatus
DCSBLib_FuncTableEnd:
#endif

;======================================
HandleMyHookId:
		call C3_ChkDirtyFlag
		xor a
		ld (IY+myflag),a
		ld (IY+nflags),a
		pop hl              ;3
	dec l               ;1
	inc hl              ;1
	ret z               ;1 set zero flag on return does default routines 
	ld de,PicArcTable   ;3
	ld bc,(PicArcTableEnd-PicArcTable)/2 ;3
	di                  ;1
	jp popallargs       ;3

PicArcTable:
	.dw pDBQUERY1      ;00
	.dw pTogglePic     ;01
	.dw pGetGroup      ;02
	.dw pExtGroup      ;03
	.dw pStringTile    ;04
	.dw pPutSprite     ;05
	.dw pShiftScreen   ;06
	.dw pDrawBox       ;07
	.dw pFillMap       ;08
	.dw pBoxShift      ;09  ;not really done yet.
	.dw pDrawText      ;10
PicArcTableEnd:

;======================================
;HandleMyHookSub:
;HandleMyHookIn:
;		pop hl
;	xor a
;	ret
;======================================
;		pop hl
;	xor a
;	ret
;======================================

ClearArgs:
	push hl
		ld hl,sc1
		ld (hl),0
		push hl
			pop de
		inc de
		ld bc,sargs-sc1+1
		ldir					;now clears out sc1-4, args0-D, nargs, sargs
		pop hl
 ret 

popallargs:  ;input: HL=#ofArgs DE=start_of_table, BC=items_on_table
	push de
		push bc
			push hl
				call clearargs
				bcall(_PushRealO1)
				pop bc           ;BC=arg counter
			ld ix,nargs      ;sargs is ix+1
			ld hl,(FPS)
			;
			push bc
popallargsL1:
				ld de,-9
				add hl,de
				ld a,(hl)
				and $1F
				jr nz,$+7
				inc (IX+0)
				jr $+5
				inc (IX+1)
				dec bc
				ld a,b
				or c
				jr nz,popallargsL1
				ld a,(ix+0) ;nargs
				cp 15
				jp nc,Err_TOOMANYARGS
				ld a,(ix+1) ;sargs
				cp 5
				jp nc,Err_TOOMANYARGS
				ld e,(ix+1)
				ld d,$00
				ld l,e
				ld h,d
				add hl,hl \ add hl,hl
				add hl,hl \ add hl,de
				ld de,sc1
				dec hl
				add hl,de
				push hl
					ld e,(ix+0)
					ld d,0
					sla e
					rl d
					ld hl,var0
					add hl,de
					push hl
						pop ix
					pop de
				pop bc  ;DE=sargsEnd,IX=nargsEnd,BC=counter
			ld hl,(FPS)
			dec hl
popallargsL2:
			push bc
				push de
					ld de,Op1+8
					ldd \ ldd \ ldd \ ldd
					ldd \ ldd \ ldd \ ldd
					ld a,(hl) \ ldd \ and $1F
					pop de
				jr nz,popallargsL2s  ;jump if not a numeric argument.
				push hl
					push de
						call ConvOp1C
						dec ix \ ld (IX+0),d
						dec ix \ ld (IX+0),e
						pop de
					pop hl
				jr popallargsL2c
popallargsL2s:
				push hl
					ld hl,Op1+8
					ldd \ ldd \ ldd \ ldd
					ldd \ ldd \ ldd \ ldd \ ldd 
					pop hl
popallargsL2c:
				pop bc
			dec bc
			ld a,c
			cp 1
			jr nz,$+19   ;second to last argument is also written into Op2 verbatim
			push hl
				push de
					push bc
						ld bc,9
						ld hl,Op1
						ld de,Op2
						ldir
						pop bc
					pop de
				pop hl
			or b
			jr nz,popallargsL2
			inc hl
			ld (FPS),hl
			pop bc          ;TBLEntries
		pop de           ;SoT
	ld (stack),SP    ;store stack
	ld hl,(var0)     ;getting first entry off of FPS
	or a             ;killing carry
	sbc hl,bc        ;subtracting table entries with this
	jp nc,Err_NOSUPPORT ;and killing routine if there is an error
	add hl,bc           ;restore HL.
	add hl,hl           ;double this number for addressing stuffs
	add hl,de           ;then add the entry # to the address for jump
	ld a,(hl)           ;obtain LSB then
	inc hl              ;.
	ld h,(hl)           ;MSB of the address.
	ld l,a              ;reload LSB.
	ld bc,programend
	push bc
		jp (hl)
 
RefreshHook:
	di
	exx
	ex af,af'
	call findappvar
	ld de,$FFFA
	ld bc,3
	ldir
	ex af,af'
	exx
	ei
	ret

xLIBHookStart:
		push hl
			call C3_ChkDirtyFlag
			pop hl
		pop bc
	xor a
	ld (IY+myflag),a
	ld (IY+nflags),a
	; res xlcomp,(iy+nflags) ;debugging mode that disables error suppression
	set xlcomp,(iy+nflags) ;normal mode supresses all error conditions
	ld a,h
	or a
	jp nz,Err_TOOMANYARGS
	ld a,l
	cp 18
	jp nc,Err_TOOMANYARGS
	di
	ld (stack),sp
	ld hl,$2020
	ld sp,xlib15
	ld a,16
	push hl
		dec a
		jr nz,$-2
		ld l,c
		ld h,b
		add hl,hl
		ld sp,xlib01
		add hl,sp
		ld sp,hl
		push bc
			call ConvOp1C
			pop bc
		push de
			dec bc
			ld a,b
			or c
			jr z,xLIBNextArg
xLIBArgLoop:
			push bc
				ld hl,(FPS)
				ld de,-9
				add hl,de
				ld (FPS),hl
				call ConvOp1C+4  ;read directly from FPS
				pop bc
			push de
			dec bc
			ld a,b
			or c
			jr nz,xLIBArgLoop
xLIBNextArg:
			ld hl,-(xLIBEndNoOut-xLIBArgTbl+2)/2
			add hl,de
			jr c,xLIBEndNoOut
			ld hl,xLIBArgTbl
			add hl,de
			add hl,de
			ld sp,hl
			pop hl
			ld sp,(stack)
			ld bc,xLIBEndNoOut
			push bc
				jp (hl)
xLIBArgTbl:
	.dw xClearScreen    ;00*
	.dw xDrawSprite     ;01*
	.dw xDrawTileMap    ;02*
	.dw xRecallPic      ;03*
	.dw xScrollScreen   ;04*
	.dw xChangeContrast ;05*
	.dw fastcopy        ;06*
	.dw xRunIndicator   ;07*
	.dw xGetKey         ;08*
	.dw xCreatePic      ;09*
	.dw xExecArcdProg   ;10*
	.dw xGetCalcVersion ;11*
	.dw xDrawShape      ;12  test line mode
	.dw xTextMode       ;13*
	.dw fChkRAMRoutine  ;14*
	.dw xLIBEndNoOut    ;15 not in set
	.dw xLIBEndNoOut    ;16 not in set
	.dw xLIBEndNoOut    ;17 not in set
	.dw xLIBEndNoOut    ;18 not in set
	.dw xLIBEndNoOut    ;19 not in set
	.dw xOmnicalcSprite ;20 Omnicalc sprite() routine
	.dw xLIBEndNoOut    ;21 not in set
	.dw xLIBEndNoOut    ;22 not in set
	.dw xLIBEndNoOut    ;23 not in set
	.dw xLIBEndNoOut    ;24 not in set
	.dw xLIBEndNoOut    ;25 not in set
	.dw xLIBEndNoOut    ;26 not in set
	.dw xLIBEndNoOut    ;27 not in set
	.dw xLIBEndNoOut    ;28 not in set
	.dw xLIBEndNoOut    ;29 not in set
	.dw xLIBEndNoOut    ;30 not in set
	.dw xLIBEndNoOut    ;31 not in set
	.dw xLIBEndNoOut    ;32 not in set
	.dw fExecHex        ;33 Omnicalc ExecHex() routine. Just linked to EXECHEX
xLIBEndNoOut:
	bcall(_RclAns)					;for 2.53MP (And possibly others)
	res numOp1,(iy+parsFlag2)
;	ld hl,Op1
;	ld (hl),$00
;	inc hl
;	ld (hl),0
;	push hl
;		pop de
;	inc de
;	ld bc,7				;clean out +2,+3,+4,+5,+6,+7,+8
;	ldir				;saved 4 bytes over previous approach
	jp ProgramEnd
 
;======================
 
#ifdef c3_errsys
#define PRGMERR(xx,yy,zz) call ProgramErr \ .db xx
#define PRGMELS(xx,yy,zz) call ProgramErr \ .db xx
#ELSE
#define PRGMERR(xx,yy,zz) call ProgramErr \ .db yy
#define PRGMELS(xx,yy,zz) call ProgramErr \ .db yy
#ENDIF

;tripping error state, 12 bytes for string
;=============================================0123456789AB
Err_PRGMFOUND:       PRGMERR(":P>IS>FN",":1","NO OVERWRITE")
Err_NULLSTRING:      PRGMERR(":NULLSTR",":2","NULL STRING ")
Err_LINENOTFOUND:    PRGMERR(":L>NT>FN",":3","LN UNDEFINED")
Err_STRNOTFOUND:     PRGMERR(":S>NT>FN",":4","SN UNDEFINED")
Err_ENTRYTOOLONG:    PRGMERR(":E>2>LNG",":5","ENTRY 2 LONG")
Err_NULLVAR:         PRGMERR(":NULLVAR",":6","EMPTY VAR   ")
Err_PRGMNOTFOUND:    PRGMERR(":P>NT>FN",":7","PG UNDEFINED")
Err_PRGMARCHIVED:    PRGMERR(":PGM>ARC",":8","PGM ARCHIVED")
Err_NULLLINE:        PRGMERR(":NULLINE",":9","EMPTY LINE  ")
Err_STRARCHIVED:     PRGMERR(":STR>ARC",":A","STR ARCHIVED")
Err_GROUPNOTFOUND:   PRGMERR(":G>NT>FN",":B","GP UNDEFINED")
Err_NOMEM:           PRGMERR(":NT>EN>M",":C","MEM TOO PUNY")
Err_NOTASTRING:      PRGMERR(":NT>A>ST",":D","NOT A STRING")
Err_NOTREAL:         PRGMERR(":NT>REAL",":E","SYNTAX -REAL")
Err_NOTAGROUP:       PRGMERR(":NT>A>GP",":F","NOT A GROUP ")
Err_TOOMANYARGS:     PRGMERR(":2>M>ARG",":G","TOO MANY ARG")
Err_PICNOTFOUND:     PRGMERR(":I>NT>FN",":H","PC UNDEFINED")
Err_DBINVALID:       PRGMERR(":D>INVAL",":I","DBSE INVALID")
Err_DBENOTFOUND:     PRGMERR(":E>NT>FN",":J","ENTRY UNDEF ")
Err_INVALIDSTRING:   PRGMERR(":INVAL:S",":K","INVALID STRN")
Err_ARGUMENT:        PRGMERR(":INVAL:A",":L","INVALID ARGS")
Err_NOTALIST:        PRGMERR(":NT>A>LS",":M","NOT A LIST  ")
Err_ENTRYNOTFOUND:   PRGMERR(":N>NT>FN",":N","ENTRY UNDEF ")
Err_BOUNDS1:         PRGMERR(":BOUNDS1",":O","INVAL BOUNDS")
Err_BOUNDS2:         PRGMERR(":BOUNDS2",":P","INVAL BOUNDS")
Err_BOUNDS3:         PRGMERR(":BOUNDS3",":Q","INVAL BOUNDS")
Err_BOUNDS4:         PRGMERR(":BOUNDS4",":R","INVAL BOUNDS")
Err_BOUNDS5:         PRGMERR(":BOUNDS5",":S","INVAL BOUNDS")
Err_BOUNDS6:         PRGMERR(":BOUNDS6",":T","INVAL BOUNDS")
Err_INVALIDFILE      PRGMERR(":INVAL:P",":U","INVALID FILE")
Err_NOSUPPORT:       PRGMELS(":SUPPORT","NS","NO SUPPORT  ")

Set1ThenEnd:
	bcall(_Op1Set1)
	jr programend
ReturnString:
	ld hl,1
	call makestring+3
	ld a,'1'
	ld (de),a
	jr programend
ProgramErr:  ;HL already pushed by the call
	bit xlcomp,(iy+nflags)
	jr nz,xlcmpexit
	call makestring
	pop hl
	ldir
programend:
xLIBEnd:
	ld SP,(stack)
	ei
	ld a,1
	or a          ;set hook system to disable.
	ret
xlcmpexit:
	res numOp1,(iy+parsFlag2)
	jr programend

C3_ChkDirtyFlag:
	bit 0,(iy+3)						;graphdirty
	call nz,clearbuf
	res 0,(iy+3)
	ret

AppVarName3:
	.db 15h,"DCS7",0
