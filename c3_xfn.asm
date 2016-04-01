;CELTIC major revision 3 by Rodger Weisman
;This software is licensed in accordance to the attached
;ReadMe file, however perverted it may be.
;subroutines xLIB compatibility file
;Provides commandset for xLIB compatibility.
;==============================
;cmd 20
xOmnicalcSprite:
;     1  2     3     4     5  6     7       8      9       10   11
;real(1, x,    y,    w,    h, pic_n ,pic_x, pic_y, logic,  flip,update_lcd)
;real20, picn, picx, picy, w, h     ,x,     y,     method
#define nodiv8() nop
#define divby8() sra h \ rr l \ sra h \ rr l \ sra h \ rr l
;Mapping FROM real20 TO real1. Get args from real20 in order for real1

 ld ix,0													;added for Real3_Check
 ld hl,(xlib07) \ nodiv8() \ push hl ;dispx location
 push hl \ pop de \ add ix,de													;added for Real3_Check
 ld hl,(xlib08) \ nodiv8() \ push hl ;dispy location
 push hl \ pop de \ add ix,de													;added for Real3_Check
 ld hl,(xlib05) \ ld a,l \ divby8() \ push hl ;width location, ld a,l added for Real3_Check
 sub 96
 ld d,a \ ld e,a
 add ix,de													;added for Real3_Check
 ld hl,(xlib06) \ nodiv8() \ push hl ;height location
 ld a,l														;\ 
 sub 62														;|- added for Real3_Check
 ld d,a \ ld e,a
 add ix,de													;added for Real3_Check
 ld hl,(xlib02) \ nodiv8() \ push hl ;picn  location
 ld hl,(xlib03) \ push hl \ pop de							;save raw DE for check
 divby8() \ push hl ;Pic_X location
 add ix,de													;added for Real3_Check
 ld hl,(xlib04) \ nodiv8() \ push hl ;Pic_Y location
 push hl \ pop de \ add ix,de													;added for Real3_Check
  
	ld hl,xlib09
	ld b,7
	pop de
	dec hl
	ld (hl),d
	dec hl
	ld (hl),e
	djnz $-5				;HERE is where the magic happens to disassemble the stack
	
	
 ld c,2   ;for or logic
 ld a,(xlib09)
 and 00000011b
 bit 0,a   ;if zero, update buffer?
 jr nz,$+3
 inc b
 bit 1,a
 jr nz,$+3
 inc c
 ld a,c
 ld (xlib09),a
 ld a,b
 ld (xlib11),a
 xor a
 ld (xlib10),a
	push ix						;check the sum for real(3) compatibility
		pop hl
	ld a,h
	or l
	jp nz,xDrawSprite
xOmnicalcSpriteFullscreen:
	ld a,(xlib06)
	ld (xlib02),a
	ld a,(xlib09)
	ld (xlib03),a
	ld a,(xlib11)
	ld (xlib04),a
	jp xRecallPic
;==============================
;cmd 13
xTextMode:
 ld a,(xlib02)
 or a  \ jr z,xTextModeA
 dec a \ jr z,xTextModeB
 dec a \ jr z,xTextModeC
 dec a \ jr z,xTextModeD
 ld a,(xlib03)
 bcall(_PutC)
 ret
xTextModeA: res textinverse,(iy+textflags) \ ret
xTextModeB: set textinverse,(iy+textflags) \ ret
xTextmodeC: set shiftLwrAlph,(iy+shiftFlags) \ ret
xTextmodeD: res shiftLwrAlph,(iy+shiftFlags) \ ret
;==============================
;cmd 12
xDrawShape:
;
;complete line drawing routine to finish off this command
;
;usage: real(12,Shape_Type,x1,y1,x2,y2,DrawShape_UpdateLCD
;Shape_Type = Tpye of Shape you want to draw:
;0 = DrawSingleLineBlack
;1 = DrawSingleLineWhite
;2 = DrawSingleLineInvert

;3 = DrawEmptyRectangleBlack			-> mos_fastrectangle								0
;4 = DrawEmptyRectangleWhite			-> mos_fastrectangle								1
;5 = DrawEmptyRectangleInvert			-> mos_fastrectangle								2
;6 = DrawFilledRectangleBlack			-> 							mos_filledrectangle		3
;7 = DrawFilledRectangleWhite			-> 							mos_filledrectangle		4
;8 = DrawFilledRectangleInvert			-> 							mos_filledrectangle		5
;9 = DrawRectOutlineBlackFillWhite		-> mos_fastrectangle		mos_filledrectangle		6
;10 = DrawRectOutlineWhiteFillBlack		-> mos_fastrectangle		mos_filledrectangle		7
	ld a,(xlib02)			;3-10 inclusive
	cp 3
	jr c,xDrawShapeLine
	sub 3					;now it's 0 to 7, inclusive
	ld c,a
	ld a,(xlib03)  ;x1
	ld h,a
	ld a,(xlib04)  ;y1
	ld l,a
	ld a,(xlib05)  ;x2
	cp h
	ret c			;forget it if x2<=x1
	cp 96
	jr c,$+4
	ld a,95
	ld d,a
	ld a,(xlib06)  ;y2
	cp l
	ret c			;forget it if y2<=y1
	cp 64
	jr c,$+4
	ld a,63
	ld e,a
xDrawShape_hl_de_c:
	;at this point the [ (x1,y1)=(h,l), (x2,y2)=(d,e), c=type ] is set up
	push hl
		ld b,0
		ld hl,xDrawShape_FilledRectangleTable
		add hl,bc
		ld a,(hl)				;a is fill type
		ld bc,xDrawShape_FastRectangleTable-xDrawShape_FilledRectangleTable
		add hl,bc
		ld b,(hl)				;b is line type
		pop hl
xDrawShape_FilledRectangle:
	cp 3
	jr z,xDrawShape_FastRectangle
	push hl
		push bc
			push de
				bcall(_mos_filledrectangle)
				pop de
			pop bc
		pop hl
xDrawShape_FastRectangle:
	ld a,b
	cp 3
	jr z,xDrawShape_ScreenCopy
	bcall(_mos_fastrectangle)					;no need to save registers
xDrawShape_ScreenCopy:	
	ld a,(xlib07)  ;update LCD code
	or a
	ret z
	jp FastCopy
xDrawShape_FilledRectangleTable:
	.db 3,3,3,1,0,2,0,1
xDrawShape_FastRectangleTable:
	.db 1,0,2,3,3,3,1,0
xDrawShapeLine:
;_ILine
;(B,C) (D,E) H= 0(clear) 1(dark) 2(xor)
;set fullScrnDraw,(iy+apiFlg4)
;set bufferOnly,(iy+plotFlag3)
;Use Fastcopy at end to draw to screen.
;Rotate Y coordinate about axis.
;x1=b   y1=c   x2=d   y2=e
;xlib03 xlib04 xlib05 xlib06
 ld hl,CIIIDrawLineType
 ld e,a
 ld d,0
 add hl,de
 ld c,(hl)											;translate the line type

 ld a,(xlib03)
 cp 96
 jr c,$+4
 ld a,95
 ld h,a
 
 ld a,(xlib04)
; cpl
; add a,64
 cp 64
 jr c,$+4
 ld a,63
 ld l,a
 
 ld a,(xlib05)
 cp 96
 jr c,$+4
 ld a,95
 ld d,a
 
 ld a,(xlib06)
; cpl
; add a,64
 cp 64
 jr c,$+4
 ld a,63
 ld e,a
 
 ld a,c
 bcall(_mos_fastline)
 ld a,(xlib07)
 or a
 jp nz,fastcopy
 ret 

 #ifdef false
;###################################################
;Until this routine is fixed or a better one that
;works is found, Celtic III must use the romcall
;to draw the line.
;
 ld hl,DrawLine
 ld de,$8000
 ld bc,DrawLineEnd-DrawLine
 ldir
;DL_VReplace uses C | XOR: A9 AND:A1 CPL:2F
;DL_HReplace uses C |  OR: B1
;0 = DrawSingleLineBlack
;1 = DrawSingleLineWhite
;2 = DrawSingleLineInvert
 ld hl,$00B1
 or a
 jr z,xDrawShapeLine1
 ld h,$A9
 dec a
 jr z,xDrawShapeLine1
 dec a
 ld l,a
xDrawShapeLine1:
 ld (DL_VReplace),hl
 ld (DL_HReplace),hl
 ld a,(xlib03)
 ld de,(xlib04)
 ld d,a
 ld a,(xlib05)
 ld hl,(xlib06)
 ld h,a
 ld ix,plotsscreen
 call $8000
 ld a,(xlib07)
 or a
 ret nz
 jp FastCopy

; Very Fast Line Drawing Routine (2002/05/19) by Patai Gergely
; Used in Celtic III under its terms of use, edited for functionality.

; ld de,$0000   ; Hint: top left corner is at 0,0 and the bottom right at 95,63
; ld hl,$1008
; ld ix,SAVESSCREEN
; call DrawLine  ;content removed simply to illustrate routine function

DrawLine:       ; This routine draws an unclipped line on an IX-pointed screen from (d,e) to (h,l)
 ld a,h         ; Calculating delta X and swapping points if negative
 sub d          ; (Lines are always drawn from left to right)
 jp nc,DL_okaydx
 ex de,hl
 neg
DL_okaydx = ($-DrawLine)+$8000
 push af        ; Saving DX (it will be popped into DE below)
 ld b,0         ; Calculating the position of the first pixel to be drawn
 ld c,d         ; IX+=D/8+E*12 (actually E*4+E*4+E*4)
 srl c
 srl c
 srl c
 add ix,bc
 ld c,e
 sla c
 sla c
 add ix,bc
 add ix,bc
 add ix,bc
 ld a,d         ; Calculating the starting pixel mask
 ld c,$80
 and 7
 jp z,DL_okaymask
DL_calcmask = ( $ - DrawLine ) + $8000
 srl c
 dec a
 jp nz,DL_calcmask
DL_okaymask = ( $ - DrawLine ) + $8000
 ld a,l         ; Calculating delta Y and negating the Y increment if necessary
 sub e          ; This is the last instruction for which we need the original data
 ld hl,12
 jp nc,DL_okaydy
 ld hl,-12
 neg
DL_okaydy = ( $ - DrawLine ) + $8000
 pop de         ; Recalling DX
 ld e,a         ; D=DX, E=DY
 cp d
 jp c,DL_horizontal ; Line is rather horizontal than vertical
 ld (DL_VLinc+1),hl ; Modifying y increment
 push ix        ; Loading IX to HL for speed; we don't need the old value of HL any more
 pop hl
 ld b,e         ; Pixel counter
 inc b
 srl a          ; Setting up gradient counter (A=E/2)
 ld (DL_HLinc+1),sp ; Backing up SP to a safe place
 di         ; Interrupts are undesirable when we play around with SP :)
DL_VLinc = ( $ - DrawLine ) + $8000
 ld sp,0        ; This value is replaced by +/- 12
DL_Vloop: ; = ( $ - DrawLine ) + $8000
 ex af,af'      ; Saving A to alternative register
 ld a,(hl)
DL_VReplace = ( $ - DrawLine ) + $8000
 nop
 or c           ; Writing pixel to current position
 ld (hl),a
 ex af,af'      ; Recalling A (faster than push-pop, and there's no need for SP)
 add hl,sp
 sub d          ; Handling gradient
 jp nc,DL_VnoSideStep
 rrc c          ; Rotating mask
 jp nc,DL_VnoByte   ; Handling byte boundary
 inc hl
DL_VnoByte = ( $ - DrawLine ) + $8000
 add a,e
DL_VnoSideStep = ( $ - DrawLine ) + $8000
 djnz DL_Vloop
 ld sp,(DL_HLinc+1)
 ret
DL_horizontal = ( $ - DrawLine ) + $8000
 ld (DL_HLinc+1),hl ; Modifying y increment
 push ix        ; Loading IX to HL for speed; we don't need the old value of HL any more
 pop hl
 ld b,d         ; Pixel counter
 inc b
 ld a,d         ; Setting up gradient counter
 srl a
 ld (DL_VLinc+1),sp ; Backing up SP to a safe place
 di             ; Interrupts again...
DL_HLinc = ( $ - DrawLine ) + $8000
 ld sp,0        ; This value is replaced by +/- 12
DL_Hloop:       ; equ ( $ - DrawLine ) + $8000
 ex af,af'      ; Saving A to alternative register
 ld a,(hl)
DL_HReplace  = ( $ - DrawLine ) + $8000
 nop
 or c           ; Writing pixel to current position
 ld (hl),a
 ex af,af'      ; Recalling A
 rrc c          ; Rotating mask
 jp nc,DL_HnoByte   ; Handling byte boundary
 inc hl
DL_HnoByte = ( $ - DrawLine ) + $8000
 sub e          ; Handling gradient
 jp nc,DL_HnoSideStep
 add hl,sp
 add a,d
DL_HnoSideStep = ( $ - DrawLine ) + $8000
 djnz DL_Hloop
 ld sp,(DL_VLinc+1)
 ret
DrawLineEnd:
#endif

;==============================
;cmd 11
xGetcalcVersion:  ;routine pulled from FlashCrash :P
	ld de,xLIBEnd
	push de
	ld de,GetListDim
	push de
	ld hl,0
	in a,(2)
	bit 7,a
	ret z
	inc hl
	bit 5,a
	ret z
	inc hl
	in a,($21)
	rrca
	ret nc
	inc hl
	ret
;==============================
;cmd 10
;xl2=fn xl3=pnum
xExecArcdProg:
	ld a,(xlib02)
	or a
	jr z,xEAPCopy
	dec a
	jr z,xEAPRemoveOne
	ld a,16
	dec a
	push af
		call xEAPReuse+5
		jr c,$+5
		bcall(_DelVarArc)
		pop af
	jr nz,$-11
	ret
xEAPRemoveOne:
	call xEAPReuse
	ret c
	bcall(_DelVarArc)
	ret
xEAPCopy:
	call xEAPLookup    ;Get source program information.
	ex de,hl           ;Size now in HL
	push hl            ;
		bcall(_EnoughMem) ;Check for free RAM
		jp c,Err_NOMEM    ;No effect other than to avoid time consuming pop/push in killing routine
		call xEAPReuse    ;Check for status of temp program
		pop hl             ;
	ret nc             ;Kill if already exists.
	bcall(_CreateProtProg) ;Otherwise, create new file with HL as size
	inc de             ;DE points to start of such file.
	inc de             ;Eh.
	push de            ;Save the starting address.
		call xEAPLookup   ;Get source program.
		ld c,e            ;Put size into BC
		ld b,d            ;
		pop de             ;Pop address of creation.
	ld a,b
	or c
	ret z              ;kill routine if size of program to copy is zero
	ld a,(cpage)       ;load ...
	bcall(_FlashToRAM) ;Then copy.
	ret
xEAPLookup:
 bcall(_RclAns)
 call getsourcestring+1
 call getprogramname+6
 call getprogram+3
 ret
xEAPReuse:
 ld a,(xlib03)
 and $0F
 ld de,Op1
 ld hl,xEAPNameBase
 ld bc,7
 ldir
 ex de,hl
 ld bc,$300A ;b='0', c=10
 cp c
 jr c,$+4
 inc b 
 sub c
 ld (hl),b
 inc hl
 add a,'0'
 ld (hl),a
 bcall(_ChkFindSym)
 ret
xEAPNameBase:
.db $05,"XTEMP0"
;==============================
;cmd 09
;xL2=fn xL3=pic#
xCreatePic:
	ld a,(xlib03)
	or a
	jr nz,$+4
	ld a,10
	dec a
	ld e,a
	ld a,(xlib02)
	cp 1
	jr nz,xCreateNewPic
	ld a,e
	call SearchForPic
	ret c
	bcall(_DelVarArc)
	ret
xCreateNewPic:
	or a
	push de
		call z,fastcopy
		pop de
	ld a,e
	call PicAToOp1
	call SearchForPic  ;deos not use Op1, but relies on intact register A.
	ret nc
	ld hl,768-12
	ld a,(xlib04)
	dec a
	jr nz,$+5
	ld hl,768
	push hl
		ld a,7			;tPictObj
		bcall($4E70)  ;CreateVar
		pop bc
	inc de
	inc de
	ld hl,plotsscreen
	ldir
	ret
 
;==============================
;cmd 08
xGetKey:
 call getKBD
 ld l,a
 ld h,0
 call GetListDim  ;loads HL to Op1
 jp xLIBEnd       ;set to output a value instead of cancel parse
;==============================
;cmd 07
xRunIndicator:
 ld a,(xlib02)
 or a
 jr nz,xRunIndicatorOn
 bcall(_RunIndicOff)
 ret
xRunIndicatorOn:
 bcall(_RunIndicOn)
 ret
;==============================
;cmd 06 updateLCD already taken care of. ReadME: lolwut?
;==============================
;cmd 05
xChangeContrast:
 ld a,(xlib02)
 or a
 jr nz,$+12
 ld a,(xlib03)
 ld (contrast),a
 add a,$D8
 out ($10),a
 ld a,(contrast)
 bcall(_SetXXOp1)
 jp xLIBEnd
;==============================
;cmd 04
;scrolldir,scrollstep,updatelcd = xlibcmd
;shiftnum,direction,screenupdate= pcarcmd
xScrollScreen:
 ld hl,(xlib02)   ;scrolldirection
 ld de,xScrollScrnT
 add hl,de
 ld a,(hl)
 ld (var2),a      ;to direction
 ld a,(xlib03)    ;scroll step
 ld (var1),a      ;to shiftnum
 ld a,(xlib04)    ;update lcd
 ld (var3),a      ;to screenupdate
 jp pShiftScreen
xScrollScrnT:
.db 4  ;u
.db 8  ;d
.db 1  ;l
.db 2  ;r
.db 5  ;ul
.db 6  ;ur
.db 9  ;dl
.db 10 ;dr
;==============================
;cmd 03
xRecallPic: ;1   2          3              4
;usage: real(3,rPIC_Num,rPIC_Method,Recall_UpdateLCD
;identity(7,x,a+4b)
;pDispPic: A=Pic, var3=mask
 ld a,(xlib04)
 or a
 jr z,$+4
 ld a,$04
 ld hl,(xlib03)
 or l            ;OR the method
 ld (var3),a     ;location for PicArc memory read
 ld a,(xlib04)
 ld (var4),a
 ld a,(xlib02)   ;getting Pic #
 jp pDispPic
;==============================
;Drawtilemap
;cmd 02  WORK: INPUT PIC NUMBER
;            1      2           3        4       5         6         7
;usage: real(2,Matrice_Name,X_Offset,Y_Offset,MapWidth,MapHeight,ScreenStartX,
;    8          9            10        11        12          13       14
;ScreenEndX,ScreenStartY,ScreenEndY,mPIC_Num,Tile_Method,Tile_Size,Map_UpdateLCD
;matrix = type 2
;xLIB anomalies: tilesize is checked only if it's 16. If not, defaults to 8
dtm_matrix equ xlib02
dtm_xpos   equ xlib03
dtm_ypos   equ xlib04
dtm_width  equ xlib05
dtm_height equ xlib06
dtm_xstart equ xlib07
dtm_xend   equ xlib08
dtm_ystart equ xlib09
dtm_yend   equ xlib10
dtm_source equ xlib11
dtm_method equ xlib12
dtm_tsize  equ xlib13
dtm_ScrUpd equ xlib14

xDrawTileMap:
	call cacheallimgs           ;cache all image files prior to processing
	res mode16,(iy+nflags)      ;reset 16*16 flag
	ld a,(dtm_tsize)            ;check for tilesize
	cp 16                       ;if equal to 16
	jr nz,$+6                   ;then continue on
	set mode16,(iy+nflags)      ;to set the 16*16 flag
;load correct routine into memory 
;Z=set mode 16, NZ=do not set mode 16
	jr z,xDTMSetMode16
	ld hl,-96
	ld (temp4),hl     ;96 tiles per map
	ld hl,xDTM08Start
	ld de,$8000
	ld bc,xDTM08End-xDTM08Start
	ldir
	ld a,(dtm_method)
	or a
	jr nz,xDTMSetMode08_NotOverwrite
	push af
		xor a
		ld ($8000+xDTM08Vert_JrOffset-xDTM08Start+1),a
		pop af
xDTMSetMode08_NotOverwrite:
	call loadlogicinst
	ld ($8000+xDTM08W1-xDTM08Start),a
	ld de,$0C08                 ;D=Xmax E=Ymax
	jr xDTMSetModeColl
xDTMSetMode16:
	ld hl,-24
	ld (temp4),hl
	ld hl,xDTM16Start
	ld de,$8000
	ld bc,xDTM16End-xDTM16Start
	ldir
	ld a,(dtm_method)
	or a
	jr nz,xDTMSetMode16_NotOverwrite
	push af
		xor a
		ld ($8000+xDTM16Vert_JrOffset-xDTM16Start+1),a
		pop af
xDTMSetMode16_NotOverwrite:
	call loadlogicinst
	ld ($8000+xDTM16W1-xDTM16Start),a
	ld ($8000+xDTM16W2-xDTM16Start),a
	ld de,$0604                 ;D=Xmax E=Ymax
xDTMSetModeColl:
	ld a,(dtm_xend) \ ld h,a    ;
	ld a,(dtm_yend) \ ld l,a    ;
	ld a,(dtm_xstart) \ ld b,a  ;
	ld a,(dtm_ystart) \ ld c,a  ;
	or a                        ;clear carry flag
	sbc hl,bc                   ;and check for bounds with only one instruction :)
	ret c                       ;return if carry
	bit 7,l                     ;check for carry on LSB
	ret nz                      ;return if "sign" is negative
	; inc h                       ;check if these increments are actually
	; inc l                       ;necessary, since users specify 12,8 for ends.
	ld a,l
	cp e
	jr c,$+3
	ld l,e
	ld a,h
	cp d
	jr c,$+3
	ld h,d
	ld (temp1),hl               ;Stuff dX and dY into temp1 (dX=MSB,dY=LSB)
	ld (temp2),bc               ;Stuff startpoints into temp2
	bit xsprt,(iy+nflags)
	jp nz,xDTMStringBuffer
	ld hl,$5C02
	ld (Op1),hl
	ld hl,(dtm_matrix)
	ld h,0
	ld (Op1+2),hl
	rst 10h                     ;Then lookup the variable
	ret c                       ;Kill routine if it does not exist
	ld a,(de)                   ;read # of entries in the row
	inc de                      ;but we're not really interested in the number of
	inc de                      ;rows there are. It's the user's responsibility. 
	ld l,a                      ;Load this to HL
	ld h,$00                    ;...
	call Mul9                   ;And multiply this by 9 to obtain # of bytes in row
	ld (temp3),hl               ;Save this for row skipping in temp3
	push hl                     ;Also save this value for immediate use.
		ld hl,(dtm_xpos)           ;Get our X-offset
		call mul9                  ;and multiply that by 9, as well.
		add hl,de                  ;Add this to the starting address of the matrix
		pop de                      ;Restore rowskip value in DE
	ld a,(dtm_ypos)             ;Put the number of rows in A
	ld b,a
	or a
	jr z,$+5                    ;skip over rowskip if there's nothing to skip
	add hl,de                   ;And repeatedly add this to simulate multiplication
	djnz $-1                    ;By the # of rows. OS will not allow this to be >99
;xLTM: series. Vertically-aligning the tilemap to handle differences.
;Tilemap will be constructed by LSB=Pic#,MSB=Tile#
	ld bc,(temp1)               ;B=deltaX , C=deltaY
	ld de,tempswaparea          ;tilemap buffer located here to overcome 8*8/16*16
xLTMMoveRight:
 push bc
  push hl
xLTMMoveDown:
   push bc
    push hl
     push de
      call ConvOp1C+4   ;convert number at HL
      ld hl,(temp4)     ;subtraction factor
      ld b,0            ;clear B for negative stuffs
      ex de,hl          ;exchange. HL=number DE=number to subtract
      add hl,de         ;repeatedly add to simulate division
      jr nc,$+4         ;jump over loop if such has happened.
      djnz $-3          ;jump back to repeat addition
      sbc hl,de         ;since there's no carry at this point, safely subtract here.
      ld a,(dtm_source) ;check image source
      or a              ;Zero
      jr nz,$+4         ;If not, jumpover. 
      ld a,10           ;If so, set Pic0 as source
      inc b
      neg               ;Negate A
      add a,b           ;Then add two negatives together
      neg               ;Negate the result into positive
      ld e,l            ;Move remaining tile number to E for load
     pop hl             ;
     ld (hl),a          ;
     inc hl             ;
     ld (hl),e          ;
     inc hl             ;
     ex de,hl           ;
    pop hl              ;
    ld bc,(temp3)       ;
    add hl,bc           ;
   pop bc
   dec c
   jr nz,xLTMMoveDown
  pop hl
  ld bc,9
  add hl,bc
 pop bc
 djnz xLTMMoveRight
 jp xDTMDrawStart
xDTMStringBuffer:
	bit strn16,(iy+nflags)
	jp nz,xDTM2BString
	ld hl,(dtm_width)
	ld (temp3),hl
	push hl
		ld hl,sc1
		call getsourcestring
		ld de,(dtm_xpos)
		add hl,de
		pop de
	ld bc,(dtm_ypos)
	ld a,c                ;only intersted in LSB for djnz loop
xDTMStringBuffer_OffsetContinue:
	or a
	jr z,xDTMStringBuffer_OffsetComplete
	add hl,de
	dec a
	jr xDTMStringBuffer_OffsetContinue
xDTMStringBuffer_OffsetComplete:
	ld bc,(temp1)
	ld de,tempswaparea          ;tilemap buffer located here to overcome 8*8/16*16
xLTMSMoveRight:
 push bc
  push hl
xLTMSMoveDown:
   push bc
    push hl
     push de
      ld a,(hl)
      ld hl,(temp4)     ;subtraction factor. Only L is used here
      ld b,0            ;clear B for negative stuffs
      add a,l           ;addition stuffs.
      jr nc,$+4         ;jump over loop if such has happened.
      djnz $-3          ;jump back to repeat addition
      sub l             ;
      ld c,a            ;save tile # here
      ld a,(dtm_source) ;check image source
      or a              ;Zero
      jr nz,$+4         ;If not, jumpover. 
      ld a,10            ;If so, set Pic0 as source
      inc b
      neg               ;Negate A
      add a,b           ;Then add two negatives together
      neg               ;Negate the result into positive
     pop hl             ;
     ld (hl),a          ;
     inc hl             ;
     ld (hl),c          ;
     inc hl             ;
     ex de,hl           ;
    pop hl              ;
    ld bc,(temp3)       ;
    add hl,bc           ;
   pop bc
   dec c
   jr nz,xLTMSMoveDown
  pop hl
  inc hl
 pop bc
 djnz xLTMSMoveRight
 jp xDTMDrawStart
xDTM2BString:
 ld hl,(dtm_width)
 add hl,hl
 ld (temp3),hl
 push hl
  ld hl,sc1
  call getsourcestring
  ld de,(dtm_xpos)
  add hl,de
  add hl,de
 pop de
 ld bc,(dtm_ypos)
 ld b,c                ;only intersted in LSB for djnz loop
 add hl,de
 djnz $-1
 ld bc,(temp1)
 ld de,tempswaparea          ;tilemap buffer located here to overcome 8*8/16*16
xLTMS2MoveRight:
 push bc
  push hl
xLTMS2MoveDown:
   push bc
    push hl
     push de
      ld e,(hl)
      inc hl
      ld d,(hl)
      ld hl,(temp4)     ;subtraction factor
      ld b,0            ;clear B for negative stuffs
      ex de,hl          ;exchange. HL=number DE=number to subtract
      add hl,de         ;repeatedly add to simulate division
      jr nc,$+4         ;jump over loop if such has happened.
      djnz $-3          ;jump back to repeat addition
      sbc hl,de         ;since there's no carry at this point, safely subtract here.
      ld a,(dtm_source) ;check image source
      or a              ;Zero
      jr nz,$+4         ;If not, jumpover. 
      ld a,8            ;If so, set Pic0 as source
      inc b
      neg               ;Negate A
      add a,b           ;Then add two negatives together
      neg               ;Negate the result into positive
      ld e,l            ;Move remaining tile number to E for load
     pop hl             ;
     ld (hl),a          ;
     inc hl             ;
     ld (hl),e          ;
     inc hl             ;
     ex de,hl           ;
    pop hl              ;
    ld bc,(temp3)       ;
    add hl,bc           ;
   pop bc
   dec c
   jr nz,xLTMS2MoveDown
  pop hl
  inc hl
  inc hl
 pop bc
 djnz xLTMS2MoveRight
;tempSwapArea         EQU  82A5h
 
;entrance to tilemapper routine
;HL=position on the tilemap
;DE=position on the graph buffer
;BC=looping information (B=dY, C=dX)
; ld (temp1),hl               ;Stuff dX and dY into temp1 (dX=MSB,dY=LSB)
; ld (temp2),bc               ;Stuff startpoints into temp2
xDTMDrawStart:
 ld hl,(temp2)
 push hl
  ld h,0
  call Mul96
  bit mode16,(iy+nflags)
  jr z,$+3
  add hl,hl   ;multiply by 2 again to get correct offset
  ex de,hl
 pop af
 bit mode16,(iy+nflags)
 jr z,$+3
 add a,a
 ld l,a
 ld h,0
 add hl,de
 ld de,plotsscreen
 add hl,de
 ex de,hl     ;DE is now the correct address in the screenbuffer 
 ld bc,(temp1)
 ld hl,tempswaparea
 call $8000
 res numOp1,(iy+parsFlag2)
 ld a,(dtm_scrupd)
 or a
 jp nz,fastcopy
 ret

;Split between 8*8 and 16*16 modes here. See labels for more details
xDTM08Start:
	di
	in a,(6)
	push af
xDTM08Horiz: ; equ ($-xDTM08Start)+$8000
		push bc
			push de         ;save position on graph buffer
xDTM08Vert: ; equ ($-xDTM08Start)+$8000 
				push bc         ;save the loop counter

;--changes by Kerm
					push hl
						ex af,af'						;save...
						ld a,8
						ld hl,gbuf+(64-8)*12-1			;-1 for nc
						or a
						sbc hl,de
						jr nc,xDTM08Vert_NotLastRow
xDTM08Vert_JrOffset:
						jr $+8
						ld hl,12*7
						add hl,de
						ld (hl),0
						dec a
						
xDTM08Vert_NotLastRow:
						ld ($8000+xDTM08_Height-xDTM08Start+1),a
						ex af,af'						;...and restore.
						pop hl
;--end changes by Kerm

					ld b,(hl)
					inc hl
					ld c,(hl)
					inc hl
					push hl        ;save the position of the tilemap
						push de       ;preserve buffer location while tracksprite is running
;======Tile Tracking Routine is placed INLINE
							ld e,b
							ld d,0
							ld hl,savesscreen+2 ;offset to lookup backwards
							add hl,de         ;
							add hl,de         ;
							add hl,de         ;
							ld e,(hl)         ;page. E is free throughout this routine so...
							dec hl            ;
							ld a,(hl)         ;
							dec hl            ;
							ld l,(hl)         ;
							ld h,a            ;Load to HL address. E=page.
							bit 7,h           ;save Z if page needs to be flipped.
							ex af,af'         ;save AF fast.
							ld a,c            ;Save tile # in A
							ld bc,96          ;
							sub 12            ;
							jr c,$+5          ;
							add hl,bc         ;
							jr $-5            ;
							add a,12          ;
							ld c,a            ;
							ex af,af'         ;restore AF to reflect original bit 7,h
							add hl,bc         ;
							jr nz,$+7         ;
							bit 7,h           ;
							jr z,$+3          ;
							inc e
							ld a,e
							out (6),a
;======End of Inline Tile Tracker
							pop de        ;restore buffer location
xDTM08_Height:		;To set xDTM08_Height+1 to 7 when necessary
						ld a,8        ;looping by this many bytes
						ld bc,12      ;screen buffer movement increment
xDTM08M:
						ex af,af'
						ld a,(de)   ;contents of the graph buffer
xDTM08W1:
						nop         ;to be the contents of the sprite
						ld (de),a   ;load it back to the graph buffer.
						bit 7,h
						add hl,bc
						jr nz,$+11
						bit 7,h
						jr z,$+7
						in a,(6)
						inc a
						out (6),a
						ex de,hl
						add hl,bc
						ex de,hl
						ex af,af'
						dec a
						jr nz,xDTM08M
						pop hl         ;restore position in the tilemap
					pop bc          ;restore old counter
				dec c
				jr nz,xDTM08Vert
				pop de           ;restore position in buffer back to top
			inc de            ;and in buffer
			pop bc
		djnz xDTM08Horiz ;and keep looping for as long as the counter is not empty
		pop af
	out (6),a
	ret
xDTM08End:
;x position and y position seem to have been switched.
;     1   2        3   4    5     6     7      8     9    10   11   12    13    14
;real(2,matrname,xPos,yPos,Width,height,StrtX,EndX,StrtY,EndY,Pic#,DMeth,TSiz,UpLCD
xDTM16Start:
 di
 in a,(6)
 push af
xDTM16Horiz:
  push bc           ;save looping information
   push de         ;save position on graph buffer
xDTM16Vert:
    push bc         ;save the loop counter

	;--changes by Kerm
					push hl
						ex af,af'						;save...
						ld a,16
						ld hl,gbuf+(64-16)*12-1			;-1 for nc
						or a
						sbc hl,de
						jr nc,xDTM16Vert_NotLastRow
xDTM16Vert_JrOffset:
						jr $+10
						ld hl,12*15
						add hl,de
						xor a
						ld (hl),a
						inc hl
						ld (hl),a
						ld a,15
						
xDTM16Vert_NotLastRow:
						ld ($8000+xDTM16_Height-xDTM16Start+1),a
						ex af,af'						;...and restore.
						pop hl
;--end changes by Kerm

     ld b,(hl)
     inc hl
     ld c,(hl)
     inc hl
     push hl        ;save the position of the tilemap
      push de       ;preserve buffer location while tracksprite is running
;======Tile Tracking Routine is placed INLINE
       ld e,b
       ld d,0
       ld hl,savesscreen+2 ;offset to lookup backwards
       add hl,de         ;
       add hl,de         ;
       add hl,de         ;
       ld e,(hl)         ;page. E is not used...
       dec hl            ;
       ld a,(hl)         ;
       dec hl            ;
       ld l,(hl)         ;
       ld h,a            ;Load to HL address. E=page.
       bit 7,h           ;save Z if page needs to be flipped.
       ex af,af'         ;save AF fast.
       ld a,c            ;Save tile # in A
       ld bc,192         ;
       sub 6             ;
       jr c,$+5          ;
       add hl,bc         ;
       jr $-5            ;
       add a,6           ;
       add a,a           ;
       ld c,a            ;
       ex af,af'         ;restore AF to reflect original bit 7,h
       add hl,bc         ;
       jr nz,$+7         ;
       bit 7,h           ;
       jr z,$+3          ;
       inc e             ;
       ld a,e            ;
       out (6),a         ;+46 from start load
;======End of Inline Tile Tracker
      pop de        ;restore buffer location
xDTM16_Height:		;To set xDTM16_Height+1 to 14 when necessary
      ld a,16       ;looping by this many bytes
      ld bc,11      ;moving down one tile.
xDTM16M:
      push af
       ld a,(de)
xDTM16W1:
       nop         ;to be the contents of the sprite
       ld (de),a   ;load it back to the graph buffer.
       inc de
       bit 7,h
       inc hl
       jr nz,$+11
       bit 7,h
       jr z,$+7
       in a,(6)
       inc a
       out (6),a
       ld a,(de)
xDTM16W2:
       nop
       ld (de),a
       bit 7,h
       add hl,bc
       jr nz,$+11
       bit 7,h
       jr z,$+7
       in a,(6)
       inc a
       out (6),a
       ex de,hl
       add hl,bc
       ex de,hl
      pop af
      dec a
      jr nz,xDTM16M
     pop hl         ;restore position in the tilemap
    pop bc          ;restore old counter
    dec c
    jp nz,$8000+xDTM16Vert-xDTM16Start ;loop back while moving downward
   pop de           ;restore position in buffer back to top
  pop bc
  inc de            ;and in buffer
  inc de            ;two since this is a 16*16 tile
  dec b
  jp nz,$8000+xDTM16Horiz-xDTM16Start ;and keep looping for as long as the counter is not empty
 pop af
 out (6),a
 ret
xDTM16End:

;==============================
;cmd 01
spr_disp_x    equ xlib02
spr_disp_y    equ xlib03
spr_disp_w    equ xlib04
spr_disp_h    equ xlib05
spr_pic_num   equ xlib06
spr_pic_x     equ xlib07
spr_pic_y     equ xlib08
spr_dispmeth  equ xlib09
spr_flip      equ xlib10
spr_updateLCD equ xlib11
xDrawSprite:
	bit xsprt,(iy+nflags)
	jr nz,$+9
	ld a,(spr_disp_x)
	and %00000111
	jp z,xDrawSprite2       ;sprite is aligned. Have the alternate routine take over
	ld hl,CBSStart  ;Located in _STD.z80. Routine written by James Montelongo.
	ld de,$8000
	ld bc,CBSEnd-CBSStart
	ldir
	ld a,(spr_dispmeth)
	and %00000011
;0=Overwrite ; 1=AND ; 2=OR ; 3=XOR
;CBSLoad1 and CBSLoad3 focus on the AND E section 
;CBSLoad2 and CBSLoad4 focus on the AND D section
;DE loads to &E
;HL loads to &D
;XOR(HL)=$AE , XOR D=$AA , XOR E =$AB , AND(HL)=$A6
; OR D  =$B2 , OR E =$B3 , OR(HL)=$B6
;AND D  =$A2 , AND E=$A3
	or a
	jr nz,xDSSkip1         ;If no jump, use OVERWRITE (special) logic
	ld a,$AE
	ld hl,$AEA3
	ld (CBSLoad1),hl
	ld (CBSLoad3),hl
	ld (CBSLoad1-1),a
	ld (CBSLoad3-1),a
	dec l
	ld (CBSLoad2),hl
	ld (CBSLoad4),hl
	ld (CBSLoad2-1),a
	ld (CBSLoad4-1),a
	jr xDSSkipCO
xDSSkip1:
	dec a
	jr nz,xDSSkip2         ;If no jump, use AND logic
	ld de,$A6B2
	ld hl,$A6B3            ;A set to zero in dec a above, to write NOP
	jr xDSSkipC
xDSSkip2:
	dec a
	jr nz,xDSSkip3         ;If no jump, use OR logic
	jr xDSSkipCO
	ld de,$00B6
	push de \ pop hl
	jr xDSSkipC 
xDSSkip3:               ;No more jumps. Use XOR logic
	ld de,$00AE
	push de \ pop hl
xDSSkipC:
	ld (CBSLoad1),de
	ld (CBSLoad2),hl
	ld (CBSLoad3),de
	ld (CBSLoad4),hl
xDSSkipCO:
	call xDSSpriteBuffer
;######################
 ld a,(spr_flip)
 or a
 call nz,xDrawSpriteR
;Change the routine in order to flip an already-buffered sprite
;######################
	ld ix,savesscreen
	ld a,(spr_disp_w)
	ld c,a
	ld a,(spr_disp_h)
	ld b,a
	ld a,(spr_disp_x)
	ld d,a
	ld a,(spr_disp_y)
	ld e,a
	call ClipBigSprite
	ld a,(spr_updateLCD)   ;if update is allowed
	or a                   ;is not zero
	jp nz,fastcopy         ;then copy contents to the screen
	ret                    ;else, just exit.

;Clip Big Sprite
;by James Montelongo
;MAX SIZE: 64x64
;ix - Sprite
;b  - height
;c  - width in bytes
;d  - x
;e  - y
;Edited by Rodger Weisman to work in Celtic III and will be edited BY Celtic III
;And also heavily/poorly commented coz I'm such a noob. Really.
ClipBigSprite:
; Early out, Check if its even remotely on screen
    ld a,e               ;Y position in A
    cp 64                ;Virtual subtract with 64
    ret p                ;Kill routine if start Y is past 64 (still positive)
    add a,b              ;Add this to height
    ret m                ;If still above top edge, kill (y+h still offscreen)
    ret z                ;If on top edge, kill routine.
    ld a,d               ;Load in X width
    cp 96                ;check if past left edge
    ret p                ;Else, kill routine if virtual subtraction is positive
    ld a,c               ;Multiply Width by 8
    add a,a              ;
    add a,a              ;
    add a,a              ;
    add a,d              ;
    ret m                ;
    ret z                ;Kill routine if past right edge
    ld a,e               ;Load Y to A
    or a                 ;And check condition
    jp p,Check_clip_bottom ;If Y is positive, Check to see if there's bottom clipping
    neg                  ;Negative Y
    push de              ;Save (x,y)
     ld hl,0              ;Clear out HL
     ld d,l               ;Also clear out MSB of DE
     ld e,a               ;Negative Y in DE, except D is not sign extended.
     bit 2,c              ;C is width in bytes. Check if in 8-12 range
     jr z,$+2+1           ;If not, then jump over adding negative Y
     add hl,de            ;...
     add hl,hl            ;...
     bit 1,c              ;...
     jr z,$+2+1           ;...
     add hl,de            ;...
     add hl,hl            ;...
     bit 0,c              ;...
     jr z,$+2+1           ;...
     add hl,de            ;...
     pop de               ;Restore (x,y)
    ex de,hl             ;...
    add ix,de            ;Here you can save the top offset
    ex de,hl             ;...
    ld e,0               ;...
    neg                  ;...
    add a,b              ;...
    ld b,a               ;...
Check_clip_bottom:
    ld a,e               ;...
    add a,b              ;...
    sub 64               ;...
    jr c,Check_clip_Left ;...
    neg                  ;...
    add a,b              ;...
    ld b,a               ;...
Check_clip_Left:
                         ; at this point you may want to save b
    xor a                ;Clear A
    ld (bigskip),a       ;Clear Sprite skip value
    ld a,Clipleftsize    ;Load size of the routine
    ld (Do_Clipleft),a   ;Initialize jump to jump OVER the shifter.
    ld a,d               ;load up x position
    or a                 ;check if positive
    jp p,Check_clip_right ; if so, then skip to check clip on RIGHT edge
    cpl                  ;negative of x position
    and $F8              ;and mask out the lowest three bits
    rra                  ;rotate the number
    rra                  ;to fill those bits
    rra                  ;...
    ex de,hl             ;save the clipped left offset
    ld e,a               ;The result is being stored to DE ; HL=(x,y) here
    ld d,0               ;...
    add ix,de            ;add this to IX to clip on this side
    ld (bigskip),a       ;And store this negative number for later use
    ex de,hl             ;restore DE to its rightful position
    inc a                ;increment A to reflect its NEG equivalent
    neg                  ;and restore its positive status.
    add a,c              ;Add this to width in bytes
    ld c,a               ;and stuff this result back into C
    xor a                ;clear out A
    ld (Do_Clipleft),a   ;and also initialize Do_Clipleft for noskip.
    ld a,d               ;load x position into A again
    and $07              ;get lowest 3 bits
    ld d,a               ;and stuff that back into DE for bit shifting
Check_clip_right:
                         ;
    ld a,Cliprightsize   ;Initialize routine to jump over cliprightside
    ld (Do_Clipright),a  ;Yeah. Initialize.
    ld a,c               ;store width into A
    add a,a              ;x8
    add a,a              ;...
    add a,a              ;...
    add a,d              ;and add this to x position to get far right edge
    sub 96               ;determine if the result needs to be clipped.
    jp m,Check_clip_middle ;if so, then perform clipping on right edge.
    and $F8              ;clear out the lowest three bits
    rra                  ;and move to fill in those bits
    rra                  ;...
    rra                  ;...
    ld l,a               ;Stuff this into spare L
    ld a,(bigskip)       ;Get point for bigskip
    add a,l              ;Add this to whatever's in this value.
    inc a                ;Increment for routine purposes ?
    ld (bigskip),a       ;And store that as SMC for jumpover
    neg                  ;negate the number that was stored as jumpover
    add a,c              ;add this to width
    ld c,a               ;and store new width to C
    xor a                ;clear out
    ld (Do_Clipright),a  ;this variable
Check_clip_middle:
                         ; This is where C should be saved.
    xor a                ;Initialize Do_ClipMiddle
    ld (Do_ClipMiddle),a ;...
    ld a,c               ;Load back width
    or a                 ;check if zero
    jp nz,dontskipmiddle ;If not zero, then there is a nonzero width to process
    ld a,ClipMiddlesize  ;Otherwise,
    ld (Do_ClipMiddle),a ;do code to skip the middle section
dontskipmiddle:
    ld l,e               ;load Y position into L for addition
    ld a,d               ;save X position for later use.
    ld h,0               ;Clear out MSB of H
    ld d,h               ;And D
    add hl,hl            ;Then multiply by 12
    add hl,de            ;to get the correct row.
    add hl,hl            ;...
    add hl,hl            ;...
    ld e,a               ;load X position into E
    and $07              ;And clear out all but the 3 least significant bits
    xor 7                ;get 1's compliment of the remaining three bits
    ld (BigRot1),a       ;And load up all the bitRot routines with this.
    ld (BigRot2),a       ;...
    ld (BigRot3),a       ;...
    srl e                ;And dividing E by 8 as it is now X-pos or something.
    srl e                ;...
    srl e                ;...
    add hl,de            ;Add this to offset
    ld de,gbuf           ;...
    add hl,de            ;And then track it to the graphbuffer
    ld de,CIIISpriteMask ;CIII-specific optimization relies on fact that
    add a,e              ;address will not require MSB adjustment to access
    ld e,a               ;table. Ever.
    ld a,(de)
                         ; This is where gbuf offset should be saved.
    ld d,a               ;save mask to D
    cpl                  ;
    ld e,a               ;And opposite mask to E
                         ;masks should be saved to
    jp $8000
CBSnn:
CBSStart:
BigSpriteRow = ( $ - CBSnn ) + $8000
BigSpriteRow2:           ;
    push bc              ;
    push hl              ;
    ld b,c               ;
Do_Clipleft = ( $ - CBSnn ) + $8000 + 1
    jr Clipleft2         ;
    ld a,(ix)            ;
    inc ix               ;
BigRot1 = ( $ - CBSnn ) + $8000 + 1
    jr $                 ;
    rrca                 ;
    rrca                 ;
    rrca                 ;
    rrca                 ;
    rrca                 ;
    rrca                 ;
    rrca                 ;
BigMask0 = ( $ - CBSnn ) + $8000
    and e                ;get left side mask for left clip
CBSLoad1 = ( $ - CBSnn ) + $8000
    nop                  ;NOP set used for extra logic by SMC (copied to RAM)
    or (hl)              ;and OR it with this
    ld (hl),a            ;and store back to buffer, but no increment
Clipleft = ( $ - CBSnn ) + $8000
Clipleft2:               ;
Clipleftsize = Clipleft-(Do_Clipleft+1)
                         ;
Do_ClipMiddle  = ( $ - CBSnn ) + $8000 + 1
    jr $+2               ;
BigSpriteloop = ( $ - CBSnn ) + $8000 
BigSpriteloop2:          ;
    ld a,(ix)            ;get next byte of the sprite
    inc ix               ;
BigRot2 = ( $ - CBSnn ) + $8000 + 1
    jr $                 ;
    rrca                 ;
    rrca                 ;
    rrca                 ;
    rrca                 ;
    rrca                 ;
    rrca                 ;
    rrca                 ;
    ld c,a               ;rotate around and save this
BigMask1 = ( $ - CBSnn ) + $8000
    and d                ;get right side of sprite
CBSLoad2 = ( $ - CBSnn ) + $8000
    nop
    or (hl)              ;and OR this to complete byte
    ld (hl),a            ;
    inc hl               ;goto next pointer.
    ld a,c               ;Restore rotated sprite byte
BigMask2 = ( $ - CBSnn ) + $8000
    and e                ;Get left side
CBSLoad3 = ( $ - CBSnn ) + $8000
    nop
    or (hl)              ;And OR this into the buffer
    ld (hl),a            ;
    djnz BigSpriteloop2  ;If there's more to do, then keep doing it until edge.
ClipMiddle = ( $ - CBSnn ) + $8000
ClipMiddlesize = ClipMiddle-(Do_ClipMiddle+1)
                         ;
Do_ClipRight = ( $ - CBSnn ) + $8000 + 1
    jr ClipRight2        ;
    ld a,(ix)            ;
BigRot3 = ( $ - CBSnn ) + $8000 + 1
    jr $                 ;
    rrca                 ;
    rrca                 ;
    rrca                 ;
    rrca                 ;
    rrca                 ;
    rrca                 ;
    rrca                 ;
BigMask3 = ( $ - CBSnn ) + $8000
    and d                ;
CBSLoad4 = ( $ - CBSnn ) + $8000
    nop
    or (hl)              ;
    ld (hl),a            ;
ClipRight = ( $ - CBSnn ) + $8000
ClipRight2:              ;
Cliprightsize = ClipRight-(Do_ClipRight+1)
    pop hl               ;

    ld bc,12             ;width of the screen
    add hl,bc            ;
                         ;
bigskip = ( $ - CBSnn ) + $8000 + 1
    ld bc,0              ;
    add ix,bc            ;
    pop bc               ;
    djnz BigSpriteRow2   ;
    ret                  ;
CBSEnd:

xDSBuffer:
 in a,(6)   ;Coz the z80 just wouldn't work without it :P
 push af
  ld a,(cpage)
  out (6),a
  ex af,af'
xDSBufferL:
  push bc
   ex af,af'
    ld a,(hl)						;get a byte here
xDSBufferW:
    nop
    ld (de),a						;store it
    inc de							;go to the next buffer location
    bit 7,h
    inc hl
    jr nz,$+11
    bit 7,h
    jr z,$+7
    in a,(6)						;page shifting if we reached the boundary
    inc a
    out (6),a
    djnz xDSBufferL+2
   ex af,af'
   ld c,a
   ld b,0
   bit 7,h
   add hl,bc						;10 for a 16-bit-wide sprite, eg.
   jr nz,$+11
   bit 7,h
   jr z,$+7
   in a,(6)
   inc a
   out (6),a
  pop bc
  dec c
  jr nz,xDSBufferL
 pop af
 out (6),a
 ret
xDSBufferEnd:
;LSft <> Rsft
;1010 <> 1100
;Initialize by checking bit 0 of Right side and setting carry if needed
;
;
;
;

xDrawSpriteR:
;sprite buffered to savesscreen already. Awaiting move to perform flip
 ld hl,savesscreen
 ld de,savesscreen
 ld a,(spr_disp_w)
 ld c,a
 ld a,(spr_disp_h)
 ld b,a
xDrawSpriteRL:
 push bc
  push hl
   push de
    ld b,0
    add hl,bc   ;(DE)=left side, (HL)=right side
    ld b,c
    dec hl
xDrawSpriteRDL:
    dec c
    jr z,xDrawSpriteRMid
    ld a,(de)
    call RLA_RRHL_4
	call RLA_RRHL_4
    rla
    ld (de),a
    inc de
    dec hl
    dec c
    jr nz,xDrawSpriteRDL
    jr xDrawSpriteRColl
xDrawSpriteRMid:
    ld a,(de)
	call RLA_RRHL_4
    rla
    rrd
xDrawSpriteRColl:     
    ld c,b
    ld b,0
   pop hl
   add hl,bc
   ex de,hl
  pop hl
  add hl,bc
 pop bc
 djnz xDrawSpriteRL
 ret
RLA_RRHL_4:
    rla \ rr (hl)
    rla \ rr (hl)
    rla \ rr (hl)
    rla \ rr (hl)
	ret

xDrawSprite2:               ;alternate routine designed to execute faster.
;It may be better to keep Y coordinates as 16-bit numbers. For extreme clipping.
 
sprdw equ spr_disp_w
sprdh equ spr_disp_h
sprdx equ spr_disp_x
sprdy equ spr_disp_y
sprpx equ spr_pic_x
sprpy equ spr_pic_y
 
 ld a,(spr_disp_w)
 or a
 jr nz,$+6
 inc a
 ld (spr_disp_w),a   ;For later testing to see if nothing needs to be drawn
 ld a,(spr_disp_h)
 or a
 jr nz,$+6
 inc a
 ld (spr_disp_h),a
 xor a
 ld (sprdw+1),a  ;clearing out MSB's of all since they're not really needed
 ld (sprdh+1),a
 ld (sprdy+1),a
 ld (sprpx+1),a
 ld (sprpy+1),a
 
 ld hl,(spr_disp_x)  ;divide display x by 8 to provide easy alignment.
 ld a,l
 sra h \ rra
 sra h \ rra
 sra h \ rra
 ld l,a
 ld (spr_disp_x),hl
;##############################################
               ;xpos stored in A already.
 ld de,xDSFinishAlign
 push de
 ld hl,(sprdw) ;width
 or a          ;check xpos if positive
 jp p,xDS2NCL  ;if so, no need to clip
 add a,l       ;add negative xpos to width
 ret m         ;kill routine if negative.
 ret z         ;kill routine if zero.
 ld (sprdw),a  ;remaining width
 neg           ;negate remaining width
 add a,l       ;and add that to total width to get width that was removed
 ld hl,(sprpx) ;
 add a,l       ;increment this a little bit.
 ld (sprpx),a  ;
 xor a         ;
 ld (sprdx),a  ;
xDS2NCL:
 ld hl,(sprdw)
 ld a,(sprdx)
 sub 12
 ret p
 dec a
 add a,l
 jp m,xDS2NCR
 cpl
 add a,l
 ld (sprdw),a
xDS2NCR:
 ld a,(sprdy)
 ld l,a
 ld hl,(sprdh)
 or a
 jp p,xDS2NCT
 add a,l
 ret m
 ret z
 ld (sprdh),a
 neg
 add a,l
 ld hl,(sprpy)
 add a,l
 ld (sprpy),a
 xor a
 ld (sprdy),a
xDS2NCT:
 ld hl,(sprdh)
 ld a,(sprdy)
 sub 64
 ret p
 dec a
 add a,l
 jp m,xDS2NCB
 cpl
 add a,l
 ld (sprdh),a
xDS2NCB:
 pop de
;##############################################
;buffer the sprite using the new parameters
;and then draw the aligned sprite.
 call xDSSpriteBuffer   ;buffers the sprite 
 ld a,(spr_flip)
 or a
 call nz,xDrawSpriteR
 ld hl,xDSAlignedSprS
 ld de,$8000
 ld bc,xDSAlignedSprE-xDSAlignedSprS
 ldir
 ld a,(spr_dispmeth)
 or a
 call nz,loadlogicinst
 ld (xDSAlignedSpr2),a
 ld a,(spr_disp_w)
 ld (xDSAlignedSpr1),a
 cpl
 add a,13
 ld l,a
 ld h,0
 ld (xDSAlignedSpr3),hl
 ld hl,(spr_disp_y)
 add hl,hl ;x2
 ld e,l
 ld d,h
 add hl,hl ;x4
 add hl,de ;x6
 add hl,hl ;x12
 ld de,(spr_disp_x)
 ld d,0    ;this number cannot be trusted.
 add hl,de
 ld de,plotsscreen
 add hl,de  ;number now in position to display.
 ld a,(spr_disp_h)
 ld b,a
 ld de,savesscreen
 call $8000
xDSFinishAlign:
 ld a,(spr_updateLCD)
 or a
 jp nz,fastcopy
 ret
 
;input: HL=spritelocation to draw DE=savesscreen B=height
;SMC:   width and skiplogic and logic
xDSAlignedSprS:
 push bc
xDSAlignedSpr1 =  ( $ - xDSAlignedSprS ) + $8000 + 1  ;width written here
  ld b,0
  ld a,(de)
xDSAlignedSpr2 =  ( $ - xDSAlignedSprS ) + $8000      ;logic written here
  nop
  ld (hl),a
  inc de
  inc hl
  djnz $-5
xDSAlignedSpr3 =  ( $ - xDSAlignedSprS ) + $8000 + 1  ;skip written here
  ld bc,0000  
  add hl,bc
 pop bc
 djnz xDSAlignedSprS
 ret
xDSAlignedSprE: 

xDSSpriteBuffer:
	bit noimg,(iy+nflags)
	jp nz,xDSNoBuffering  ;passthrough buffering if sprite data already provided
	ld hl,xDSBuffer
	ld de,tempswaparea   ;not being used by anything else this run
	ld bc,xDSBufferEnd-xDSBuffer
	ldir
	ld a,(spr_dispmeth)
	bit 2,a
	jr z,$+7
	ld a,$2F    ;cpl instruction
	ld (xDSBufferW-xDSBuffer+tempswaparea),a
	ld a,(spr_pic_num)
	call GetPic+3
	push hl
		push de
			ld hl,(spr_pic_y) ;picY
			add hl,hl        ;x2
			ld d,h \ ld e,l  ;set temp
			add hl,hl        ;x4
			add hl,de        ;x6
			add hl,hl        ;x12
			ld de,(spr_pic_x)
			add hl,de
			pop de
		pop bc
	call addbctohl1pg
	ld a,(spr_disp_h)
	or a
	ret z
	cp 64+1
	ret nc
	ld c,a
	ld d,63+1
	ld a,e
	or a
	jr nz,$+3
	inc d					;if it's $300 in size, allow the 64th row
	ld a,(spr_pic_y)
	add a,c
	cp d
	jr c,xDSSpriteBuffer_NoHeightReduction
	sub 64
	cpl
	add a,c
	ld c,a
xDSSpriteBuffer_NoHeightReduction:
	ld a,(spr_disp_w)
	or a
	jr nz,$+6
	inc a
	ld (spr_disp_w),a
	cp 12+1
	ret nc
	ld b,a
	ld de,savesscreen ;buffering the sprite in this location
	push hl
		push bc
			push de
				ld a,(spr_disp_h)
				sub c
				jr z,xDSSpriteBuffer_NoInitClear
				ld d,b
				ld e,a
				xor a
xDSSpriteBuffer_ClearCountLoop:
				add a,e
				djnz xDSSpriteBuffer_ClearCountLoop
				push af
					ld b,c
					ld hl,savesscreen
xDSSpriteBuffer_ClearSkipLoopOuter:
					ld a,d
xDSSpriteBuffer_ClearSkipLoopInner:					
					inc hl
					dec a
					jr nz,xDSSpriteBuffer_ClearSkipLoopInner
					djnz xDSSpriteBuffer_ClearSkipLoopOuter
					pop bc
xDSSpriteBuffer_ClearLoop:
				ld (hl),0
				inc hl
				djnz xDSSpriteBuffer_ClearLoop
xDSSpriteBuffer_NoInitClear:
				pop de
			pop bc
		pop hl
;B=width,C=height
	di
	ld a,b
	cpl
	add a,13   ;to get next row
	ex af,af'  ;Accumulator juggling ^_^
	call tempswaparea ;location to call for the next set.
xDSNoBuffering:
 ret

;==============================
;cmd 00
xClearScreen:
;	bcall(_rclans)						;Ans to Op1
	call clearbuf
	ld a,(xlib02)
	or a
	call nz,fastcopy					;Leaves ans in Op1
	jp xLIBEndNoOut
