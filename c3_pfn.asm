;CELTIC major revision 3 by Rodger Weisman
;This software is licensed in accordance to the attached
;ReadMe file, however perverted it may be.
;subroutines PicARC functions
;PicArc ADDONS
;================================
pFormat:
 call getprogramname
 bcallnc(_DelVarArc)
 ld hl,7
 ld a,(Op1)
 cp 5
 jr nz,$+3
 inc a
 bcall($4E70)   ;CreateVar
 inc de
 inc de
 ld hl,PADBStart
 ld bc,7
 ldir
 jp Set1ThenEnd
PADBStart:
.db $BB,$6D,$C9,"C3P",$00

pRemovePic:
 call getprogram
 or a
 jp nz,Err_PRGMARCHIVED
 ld a,(var2)
 call GetDBEntry
 inc de
 inc de
 push de
  bcall(_DelMem)  ;Delete memory, HL=adr, DE=bytes
 pop bc
 call updatesizedel
 ld de,6
 add hl,de
 dec (hl)
 jp Set1ThenEnd

pDBDim:
 call getprogram
 ld de,6
 call adddetohl
 call gethl
 ld l,a
 ld h,0
 bcall(_SetXXXXOp2)
 bcall(_Op2toOp1)
 ret

pDBQUERY1:
	ld a,(var1)
	ld b,a
	or b  \ jr z,pFormat       ;0
	dec b \ jr z,pAppendPic    ;1
	dec b \ jr z,pRemovePic    ;2
	dec b \ jr z,pDBDIM        ;3
	dec b \ jr z,pAppendPic    ;4
	dec b \ jr z,pRetrievePic  ;5
	dec b \ jr z,pCopyPic      ;6
	ret
 
pAppendPic:
	push af
		ld a,(var2)
		call getpic+3  ;(cpage):HL = address
		pop af
	dec a   ;relies on the fact that normal entry is 1 so no compression is 0.
	call PicInStream
	inc bc
	inc bc
	push bc
		push bc
			bcall(_MemChk)
			pop bc
		or a
		sbc hl,bc
		jp c,Err_NOMEM  
		call getprogram
		or a
		jp nz,Err_PRGMARCHIVED
		ld de,PADBStart
		call CheckFileHeader
		ld bc,6
		add hl,bc
		ld a,(hl)   ;get previous entry # to go to
		inc (hl)    ;increment number of entries
		or a
		sbc hl,bc
		call GetDBEntry
		ex de,hl        ;HL=size (discarded), DE=location
		pop hl           ;restore size to insert
	push de          ;save addy
		push hl         ;save size
			bcall(_InsertMem)  ;HL=mem, DE=addy
			pop bc          ;restore size
		call updatesizeins
		pop hl
	dec bc
	dec bc
	ld (hl),c
	inc hl
	ld a,(cmask)
	ld (hl),b
	or (hl)
	ld (hl),a
	inc hl
	ex de,hl
	ld hl,savesscreen
	ldir
	jp Set1ThenEnd

pRetrievePic:
 call getprogram
 ld a,(var2)
 call GetDBEntry
 call PicOutstream
 ld de,savesscreen
 ld hl,plotsscreen
 ld bc,$02F4
 ld a,(var3)
 and %00000011
 or a  \ jr z,RetrievePic1
 dec a \ jr z,RetrievePic2
 dec a \ jr z,RetrievePic3
 jr RetrievePic4
pCopyPicICA:
 ld hl,plotsscreen
 push hl
 jr pCopyPicIC
pCopyPic:
 call getprogram
 ld a,(var2)
 call GetDBEntry
 call PicOutStream
 ld a,(var3)
 ld hl,savesscreen
 push hl
pCopyPicIC:  ;separate because of the plotsscreen/savesscreen problem
  or a
  jr nz,$+4
  ld a,10
  dec a
  call PicAToOp1
  rst 10h
  bcallnc(_DelVarArc)
  bcall(_CreatePict)
  inc de
  inc de
 pop hl
 ld bc,756
 ldir
 jp Set1ThenEnd
 
 
RetrievePic1:
 ld a,(de) \ ld (hl),a
 inc hl \ inc de
 dec bc \ ld a,b \ or c
 jr nz,$-7
 jr RetrievePic5
RetrievePic2:
 ld a,(de)
 and (hl)
 ld (hl),a
 inc hl \ inc de
 dec bc \ ld a,b \ or c
 jr nz,$-8
 jr RetrievePic5
RetrievePic3:
 ld a,(de)
 or (hl)
 ld (hl),a
 inc hl \ inc de
 dec bc \ ld a,b \ or c
 jr nz,$-8
 jr RetrievePic5
RetrievePic4:
 ld a,(de)
 xor (hl)
 ld (hl),a
 inc hl \ inc de
 dec bc \ ld a,b \ or c
 jr nz,$-8
RetrievePic5:
 res numOP1,(iy+ParsFlag2)
 ld a,(var3)
 bit 2,a
 jp nz,fastcopy
 ret

;================================
pTogglePic:
 ld a,(var1)
 ld hl,(var2)
 inc l
 dec l \ jr z,pDispPic
 dec l \ jr z,pCopyPicICA  ; --> merge into subroutine of another routine
 dec l \ jr z,pArcUnArcPic
 dec l \ jr z,pDelPic
 ret
pDelPic:
 or a
 jr nz,$+4
 ld a,10
 dec a
 call PicAToOp1
 rst 10h
 jp c,Err_PICNOTFOUND
 bcall(_DelVarArc)
 bcall(_SetXXOp1)
 ret
 
pArcUnArcPic:
 dec a
 push af
  call PicAToOp1
  rst 10h
  jp c,Err_PICNOTFOUND
  bcall(_Arc_Unarc)
 pop af
 call PicAToOp1
 rst 10h
 ld a,b
 bcall(_SetXXOp1)
 ret

pDispPic:
 call getpic+3
 res numOP1,(iy+ParsFlag2)
 push hl
  ld a,b
  or a
  ld a,(var3)
  jr nz,pDispPicInFlash
pDispPicInRAM:
  ld hl,pDispPicRCR
  ld de,$8000
  ld bc,pDispPicRCRE-pDispPicRCR
  ldir
  and $03
  call loadlogicinst
  ld (pDispPicRCR2-pDispPicRCR+$8000),a
  jr pDispPicColl
pDispPicInFlash:
  ld de,-($4700-12)
  add hl,de
  jr nc,pDispPicInRAM
  ld hl,pDispPicRC
  ld de,$8000
  ld bc,pDispPicRCE-pDispPicRC
  ldir
  and $03
  call loadlogicinst
  ld (pDispPicRC2-pDispPicRC+$8000),a
pDispPicColl:
	pop hl
	di
	ld bc,768-12
	ld a,(var4)
	dec a
	jr nz,$+5
	ld bc,768
	call $8000
	ld a,(var3)
	bit 2,a
	jp nz,fastcopy
	ret
;and (hl) = $A6
;xor (hl) = $AE
;or (hl)  = $B6
;ld a,(hl)= *$00  input allows routine to change this to actual opcode
pDispPicRC:
 in a,(6)
 push af
  ld a,(cpage)
  out (6),a
  ld de,plotsscreen
;  ld bc,$02F4
  ld a,(de)
pDispPicRC2:
  nop           ;SMC the instruction here
  ld (de),a
  inc de
  bit 7,h
  inc hl
  jr nz,pDispPicRC3
  bit 7,h
  jr z,pDispPicRC3
  in a,(6)
  inc a
  out (6),a
  ld h,$40
pDispPicRC3:
  dec bc
  ld a,b
  or c
  jr nz,pDispPicRC2-1
 pop af
 out (6),a
 ret
pDispPicRCE:
pDispPicRCR:
 in a,(6)
 push af
  ld a,(cpage)
  out (6),a
  ld de,plotsscreen
;  ld bc,$02F4
  ld a,(de)
pDispPicRCR2:
  nop           ;SMC the instruction here
  ld (de),a
  inc de
  inc hl
  dec bc
  ld a,b
  or c
  jr nz,pDispPicRCR2-1
 pop af
 out (6),a
 ret
pDispPicRCRE:
;================================
pGetGroup:
 set gGroup,(iy+myflag)
 call getprogram
 ld a,(Op1)
 cp $17  ;groupobj
 jp nz,Err_NOTAGROUP

 ld bc,0
GetGPicL1:
 call lookupEOF
 jr c,buildgrplist
 push bc
  call groupgetnext
 pop bc
 ld a,(Op2)
 cp $07      ;Pic obj
 jr nz,GetGPicL1
 inc bc
 jr GetGPicL1
buildgrplist:
 ld a,b
 or c
 jp z,Err_NULLVAR   ;no images in group file
 push bc
  call getprogram
  ld (temp1),hl
 pop hl
 bcall($4312)       ;bcall CreateTempRList, undocumented.
 inc de
 inc de
buildgrplistL:
 push de
  ld hl,(temp1)
  call lookupEOF
  jr c,endgrouplist
  call groupgetnext
  ld (temp1),hl
  ld a,(Op2)
  push af
   ld hl,(Op2+2)
   ld h,0
   bcall(_SetXXXXOp2)
  pop af
 pop de
 cp $07
 jr nz,buildgrplistL
 ld hl,Op2
 ld bc,9
 ldir
 jr buildgrplistL
endgrouplist:
 pop de
 bcall(_Op4toOp1)
 ret
;================================
pExtGroup:
 set gGroup,(iy+myflag)
 call getprogram
 ld a,(Op1)
 cp $17  ;groupobj
 jp nz,Err_NOTAGROUP
ExtGroupL1:
 call lookupEOF
 jp c,Err_PICNOTFOUND
 call groupgetnext
 ld a,(Op2)
 cp $07
 jr nz,ExtGroupL1
 ld a,(Op2+2)
 ld b,a
 ld a,(var1)
 cp b
 jr nz,ExtGroupL1
 bcall(_Op2toOp1)
 rst 10h
 jr c,$+5
 bcall(_DelVarArc)
 ld hl,$0300-12
 bcall(_CreatePict)
 inc de
 inc de
 ld hl,(gSOF)
 ld bc,(gSIZE)
 ld a,(gSOF+2)
 ld (cpage),a
 call GetHLInc
 ld (de),a
 inc de
 dec bc
 ld a,b
 or c
 jr nz,$-8
 jp Set1ThenEnd
;================================================================================================
;================================================================================================
;================================================================================================
;================================================================================================
pStringTile:
;Passes the hard stuff to the xlib routine DrawTileMap
;                  x02   x03
;identity(10,"STR",dispX,dispY,width,height,BegX,EndX,BegY,EndY,Pic#,Logic,TS,UDL,2_byte_mode)
;real(2,MatrixName,dispX,dispY,Width,Height,BegX,EndX,BegY,EndY,Pic#,Logic,TS,UDL)
 set xsprt,(iy+nflags)
 ld de,xlib03
 ld hl,var1
 ld bc,24
 ldir
;xLIB support contains the tilemap buffering routine necessary
;for string support. Branches to specific on bit xsprt,(iy+nflags)
 jp xDrawTileMap
;
;
;=========================================
;OLD CODE
;ROLL BACK IF CHANGES ARE UNACCEPTABLE.
;=========================================
; ld a,(nargs)
; cp 8
; jp nz,Err_ARGUMENT
; call cacheimgs
; ld hl,pSTintile
; ld de,$8000
; ld bc,pSTouttile-pSTintile
; ldir
; ld a,(var7)
; and %00000011
; call loadlogicinst
; ld (pSTOverwrite-pSTintile+$8000),a
; ld hl,sc1
; rst 20h
; rst 10h
; jp c,Err_STRNOTFOUND
; inc de
; inc de        ;jump past size bytes of the string; They're not needed.
; ld bc,(var3)  ;width to BC
; ld hl,0       ;offset to HL
; ld a,(var5)  ;y-pos
; or a          ;determine if y-pos is zero.
; jr z,$+6      ;jump if so. No need to add anything.
; add hl,bc     ;add width to offset.
; dec a         ;decrement y-pos until it is zero
; jr nz,$-2     ;jump back to adding if not zero.
; add hl,de     ;add offset to address of string
; ld de,(var4)  ;width
; add hl,de     ;current position with offsets added.
; ld de,plotsscreen ;load DE to plosscreen for drawing
; ld c,12          ;looping to the right twelve times
;pSTmoveright:
; push hl          ;save position of tilemap
;  push de         ;save position on graph buffer
;   ld b,8         ;loop moving downward eight times
;pSTmovedown:
;   push bc        ;save the loop counter
;    push hl       ;save the position of the tilemap
;     push de      ;preserve buffer location while tracksprite is running
;      ld a,(hl)   ;get the byte from the tilemap for tracksprite
;      call tracksprite  ;obtain the starting point of the sprite
;     pop de       ;restore buffer location
;     ld a,8       ;looping by this many bytes
;     ld bc,12     ;moving down one tile.
;     call $8000   ;intile routine
;    pop hl        ;restore position in the tilemap
;    ld bc,(var3)  ;get width of the tilemap
;    add hl,bc     ;and add to current position of tilemap to get next tile
;   pop bc         ;restore old counter
;   djnz pSTmovedown ;loop back while moving downward
;  pop de          ;restore position in buffer back to top
; pop hl           ;restore position in tilemap back to top
; inc hl           ;increment (right) in tilemap
; inc de           ;and in buffer
; dec c            ;decrement column counter
; jr nz,pSTmoveright ;and keep looping for as long as the counter is not empty
; res numOp1,(iy+parsFlag2)
; ld a,(var7)
; bit 2,a
; jp nz,fastcopy
; ret
;
;pSTintile:
;     push af
;      ld a,(de)   ;contents of the graph buffer
;pSTOverWrite:
;      nop         ;to be the contents of the sprite
;      ld (de),a   ;load it back to the graph buffer.
;      bit 7,h
;      add hl,bc
;      jr nz,$+12
;      bit 7,h
;      jr z,$+8
;      push hl
;       ld hl,cpage
;       inc (hl)
;      pop hl
;      ex de,hl
;      add hl,bc
;      ex de,hl
;     pop af
;     dec a
;     jr nz,pSTintile
;     ret   ;for copy in tile routine
;pSTouttile:

;================================
;identity(5,Str?,x,y,w,h,logic,flip,update_lcd)
;Relies on xLIB-supported sprite routine
pPutSprite:
;identity(5,Str?,x,y,w,h,logic,flip,update_lcd)
 set noimg,(iy+nflags)
 ld hl,sc1
 call getsourcestring
 ex de,hl
 ld hl,savesscreen
 call HexStringToBin
 ld de,xlib02
 ld hl,var1
 ld bc,8
 ldir
 push hl
  ld hl,6
  add hl,de
  ex de,hl
 pop hl
 ld bc,6
 ldir
 jp xDrawSprite
;================================
;              var1         var2    var3
;              bnum3        bnum2   bnum1
;identity(12,shiftnumber,shifttype,dispmeth)
;1111
;|||\left
;|||-right
;||--up
;|---down
;decimal table:
;     left: 1
;    right: 2
;       up: 4    
;   upleft: 5
;  upright: 6
;     down: 8   
; downleft: 9
;downright: 10
;Dispmeth: 0=nodisplay 1=display
;shiftnum,direction,screenupdate
pShiftScreen:
 ld a,(var1)
 or a
 jp z,Err_ARGUMENT
pShiftScreenL:
 push af
  ld a,(var2)
  rra
  call c,pShiftSLeft
  rra 
  call c,pShiftSRight
  rra
  call c,pShiftSup
  rra
  call c,pShiftSDown
 pop af
 dec a
 jr nz,pShiftScreenL
 res numOp1,(iy+parsFlag2)
 ld a,(var3)
 or a
 jp nz,fastCopy
 ret

pShiftSDown:
 ld de,$02FF+plotsscreen
 ld hl,($02FF-12)+plotsscreen
 ld bc,$0300-12
 lddr
; ld hl,plotsscreen
; ld de,plotsscreen+1
; ld bc,11
; ld (hl),$0
; ldir
 ret
pShiftSup:
 ld hl,plotsscreen+12
 ld de,plotsscreen
 ld bc,$0300-12
 ldir
; ld hl,plotsscreen+$0300-12
; ld de,plotsscreen+$0300-11
; ld bc,11
; ld (hl),$0
; ldir
 ret
pShiftSLeft:
 ld b,64
 ld hl,plotsscreen+$02FF
pShiftSLeftLoop:
 rrc (hl)
 ex af,af'
 rlc (hl)
 ex af,af'
 rl (hl)  \ dec hl
 rl (hl)  \ dec hl
 rl (hl)  \ dec hl
 rl (hl)  \ dec hl
 rl (hl)  \ dec hl
 rl (hl)  \ dec hl
 rl (hl)  \ dec hl
 rl (hl)  \ dec hl
 rl (hl)  \ dec hl
 rl (hl)  \ dec hl
 rl (hl)  \ dec hl
 rl (hl)  \ dec hl
 djnz pShiftSLeftLoop
 ret
pShiftSRight:
 ld b,64
 ld hl,plotsscreen
pShiftSRightLoop:
 rlc (hl)
 ex af,af'
 rrc (hl)
 ex af,af'
 rr (hl)  \ inc hl
 rr (hl)  \ inc hl
 rr (hl)  \ inc hl
 rr (hl)  \ inc hl
 rr (hl)  \ inc hl
 rr (hl)  \ inc hl
 rr (hl)  \ inc hl
 rr (hl)  \ inc hl
 rr (hl)  \ inc hl
 rr (hl)  \ inc hl
 rr (hl)  \ inc hl
 rr (hl)  \ inc hl
 djnz pShiftSRightLoop
 ret
;================================
pFillMap:
;         v0            v1    v2   v3    v4
;identity(8,"HEXSTRING",Right,Down,LOGIC,UpdateLCD)
;HexStringToBin:  DE=Source, HL=Destination, BC=counter (in output bytes)
;getsourcestring: HL=data_start;BC=file_size;DE=VAT_location
 ld hl,pFillMapS
 ld de,$8000
 ld bc,pFillMapE-pFillMapS
 ldir
 ld a,(var3)
 or a
 call nz,loadlogicinst
 ld (pFillMapW),a
 ld hl,sc1
 call getsourcestring
 ex de,hl
 ld hl,tempswaparea
 ld bc,8
 push hl    ;saving this for second copy
  push bc
   call HexStringToBin
  pop bc
 pop hl
 push hl   ;saving HL for tile rotation address
  ld de,tempswaparea+8
  ldir
 pop hl    ;saving HL for offset lookup
 push hl
  ld b,16
  ld a,(var1)
  and 7
  or a
  jr z,$+12 ;do not shift if there is no need to do so.
  ld c,a
  rrc (hl)
  dec a
  jr nz,$-3
  ld a,c
  inc hl
  djnz $-7
 pop hl
 ld a,(var2)
 and 7
 ld e,a
 ld d,0
 add hl,de
 ld (pFillMapA),hl
 ld hl,plotsscreen
 di
 call $8000
 ld a,(var4)
 or a
 jp nz,fastcopy
 bcall(_Op1Set0)
 ret
pFillMapS:
 ld a,8
 ex af,af'
pFillMapA  = ($ - pFillMapS) + $8000 + 1 
  ld de,0000
  ld c,8
  ld b,12
  ld a,(de)
pFillMapW  = ( $ - pFillMapS) + $8000
  nop
  ld (hl),a
  inc hl
  djnz $-4
  inc e         ;this part of tempswaparea will not pagefault
  dec c
  jr nz,$-10
 ex af,af'
 dec a
 jr nz,pFillMapS+2
 ret
pFillMapE:

;================================
pDrawBox:
;            v1 v2 v3 v4 v5
;         b6 b5 b4 b3 b2 b1
;identity(13,x1,y1,x2,y2,dispmeth), bit 7 holds screen update flag
	ld a,(nargs)
	cp 6
	jp nz,Err_ARGUMENT
	ld a,(var5)
	and $0F
	cp 7+1
	jp nc,Err_BOUNDS1
	ld c,a
	ld a,(var1) ;x1
	ld h,a
	ld a,(var2) ;y1
	ld l,a
	ld a,(var3) ;x2
	cp 96
	jr c,$+4
	ld a,95
	ld d,a
	ld a,(var4) ;y2
	cp 64
	jr c,$+4
	ld a,63
	ld e,a
	ld a,(var5)
	and %10000000
	ld (var7),a			;LCD update
	jp xDrawShape_hl_de_c 
 
#ifdef false
;disjointed end. Please optimize later. Maybe? 
;H=x1 L=y1 D=x2 E=Y2
;Masks are held as LSB=XOR, MSB=OR to simulate possible AND situations
;temp1 = left edge
;temp2 = right edge
;temp3 = LSB:drawing loop ; MSB: skipping loop
;temp4 = height
;temp5 = center mask
;temp6 = left top/bottom edge
;temp7 = right top/bottom edge
;temp8 = center top/bottom mask
;temp9 = LSB: left side bit count ; MSB: right side bit count
pDBDraw:
 ld a,e              ;load Y2 to register A, far side.
 cp 64               ;and check if it goes below the screen.
 jp nc,Err_BOUNDS2   ;Flag error if going below the bottom of the screen
 sub l               ;Check for Y1 position
 jp c,Err_BOUNDS3    ;If it is in incorrect, flag error.
 sub 2               ;Check to see if results are less than two.
 jp c,Err_BOUNDS4    ;If so, improper loop detected.
 inc a
 ld (temp4),a        ;load this as a row counter (number of rows)
 ld a,d
 cp 96
 jp nc,Err_BOUNDS5
 cp h
 jp c,Err_BOUNDS6
 xor a               ;Clear out register A for shifting in.
 srl h \ rra         ;Dividing H by two and loading the remainder into A.
 srl h \ rra         ;Dividing by two more and moving that bit into A.
 srl h               ;Again, but keep that bit into carry as we move
 rla \ rla \ rla     ;that remainder into the appropriate position.
 ld (temp9),a        ;saving remainder for further testing
;==                  ;
 push hl
  or a                ;determine if the remainder is zero. If so,
  jr z,$+27           ;no shifting is needed.
  ld bc,(temp1)       ;load the left side masks in and
  ld hl,(temp6)
  srl h
  srl l
  srl b               ;shift both of them right, each for OR mask
  srl c               ;and the XOR mask.
  dec a               ;decrement the counter and
  jr nz,$-9           ;loop back if the remainder still !=0
  ld (temp1),bc       ;load back the altered bitmasks.
  ld (temp6),hl
;==                  ;A is already cleared out for this next run
  srl d \ rra         ;...
  srl d \ rra         ;
  srl d               ;
  rla \ rla \ rla     ;
  ld (temp9+1),a      ;saving remainder for further testing
;==                  ;
  cpl
  add a,8
  or a                ;
  jr z,pDBDraw292     ;
  ld bc,(temp2)       ;
  ld hl,(temp7)
pDBDraw291:
  sla h
  sla l
  sla b               ;
  sla c               ;
  dec a               ;
  jr nz,pDBDraw291    ;
  ld (temp2),bc       ;...
  ld (temp7),hl
pDBDraw292:
;==                  ;
 pop hl
 ld a,d              ;loading in reduced x2 position into A
 cp 12               ;checking to see if it's beyond the last column.
 jp nc,Err_BOUNDS5   ;Return error if attempting to draw outside the screen
 sub h               ;and checking that reduced x1 for possible errors.
 jp c,Err_BOUNDS6    ;Issue error if x1 is bigger than x2.
 call z,fDBInnerTest ;remask the masks if byte position is the same
 ld (temp3),a        ;Save byte increment range. 0=same,1=two,2=one in mid, 3=etc.
 cpl                 ;negate that minus one.
 add a,13            ;And add 13 to it to get runaround increment.
 ld (temp3+1),a      ;load the jumpahead loop counter to the byte after the column counter
 ld a,h              ;save x1 position; y1 already loaded as L.
 ld h,$00            ;clear out the MSB for addition purposes
 add hl,hl           ;multiplying by two
 ld b,h              ;save the *2
 ld c,l              ;...
 add hl,hl           ;*4
 add hl,bc           ;*6
 add hl,hl           ;*12
 ld bc,plotsscreen   ;get buffer location
 add hl,bc           ;And add this to offset
 ld c,a              ;load X offset
 ld b,0              ;and then
 add hl,bc           ;add this to offset to obtain complete offset.
 ex de,hl            ;put the buffer location into DE.
 call fDBtops        ;
 ld bc,(temp4-1)     ;
 push bc             ;
  call fDBmiddle     ;
 pop bc              ;
 djnz $-5            ;
 call fDBtops        ;
 ret                 ;
fDBtops:             ;
 ld hl,(temp7)       ;
 push hl             ;
 ld hl,(temp8)       ;
 push hl             ;
 ld hl,(temp6)       ;
 push hl             ;
 jr fDBCommons       ;
fDBmiddle:           ;
 ld hl,(temp2)       ;
 push hl             ;
 ld hl,(temp5)       ;
 push hl             ;
 ld hl,(temp1)       ;
 push hl             ;
fDBCommons:          ;
 ld bc,(temp3)       ;load loop counters here.
 pop hl              ;get first mask off the stack
 ld a,(de)           ;get first byte from the buffer
 or h                ;Run that mask
 xor l               ;and XOR that mask
 ld (de),a           ;load the result back into DE
 ld a,c              ;check if the counter is zero (no increment)
 or a                ;if it is zero
 jr z,$+16           ;jump past to masking the first byte all over again
 inc de              ;otherwise, increment and
 dec c               ;check to see if it is now zero.
 jr z,$+12           ;If so, then jump to mask the second byte
 pop hl              ;Otherwise, pop the next mask set in.
 ld a,(de)           ;And start working the center
 or h                ;series by OR-ing and
 xor l               ;XOR-ing the masks in.
 ld (de),a           ;Then store the result back into the register.
 inc de              ;increment pointer
 dec c               ;and determine if there is a need to loop back
 jr nz,$-6           ;Loop until this counter is zero.
 push hl             ;push this to allow for conditional pop.
 pop hl              ;and pop twice
 pop hl              ;to access the last mask.
 ld a,(de)           ;get the data from the buffer
 or h                ;and OR this
 xor l               ;and XOR this
 ld (de),a           ;and load the data back to the buffer
 inc de              ;increment pointer for however long
 djnz $-1            ;B is not zero.
 ret                 ;Then return to caller.
fDBCommonsEnd:
fDBInnerTest:
 push af
  push hl
   ld bc,(temp9) ;temp9+0=left side, temp9+1=right side
   ld a,b
   cp c
   jp c,Err_BOUNDS6 ;throw error if first bound is bigger than second bound 
   ld de,$FFFF
   ld a,b
   cpl
   add a,8
   ld b,a
   jr z,$+7
   or a
   rl d      ;left edge spacing
   djnz $-3
   ld a,c
   or a
   ld b,c
   jr z,$+7
   or a
   rr e      ;right edge spacing
   djnz $-3
   ld hl,temp1
   ld a,(hl) \ and d \ ld (hl),a \ inc hl ;temp1+0
   ld a,(hl) \ and d \ ld (hl),a \ inc hl ;temp1+1
   ld a,(hl) \ and e \ ld (hl),a \ inc hl ;temp2+0
   ld a,(hl) \ and e \ ld (hl),a \ inc hl ;temp2+0
   ld hl,temp6
   ld a,(hl) \ and d \ ld (hl),a \ inc hl ;temp6+0
   ld a,(hl) \ and d \ ld (hl),a \ inc hl ;temp6+1
   ld a,(hl) \ and e \ ld (hl),a \ inc hl ;temp7+0
   ld a,(hl) \ and e \ ld (hl),a \ inc hl ;temp7+1
  pop hl
 pop af
 ret
pDBDrawTable:  ;five entries so far, each 12 bytes long
;    t1    t2    t5    t6    t7    t8
;XOR LSB, OR MSB
;00 draw line \ keep inside (OK)
.dw $8000,
.dw    $0100,
.dw    $0000,
.dw    $FF00,
.dw    $FF00,
.dw    $FF00
;01 clear line only, keep inside
.dw $8080,
.dw    $0101,
.dw    $0000,
.dw    $FFFF,
.dw    $FFFF,
.dw    $FFFF
;02 invert line only, keep inside
.dw $0080,
.dw    $0001,
.dw    $0000,
.dw    $00FF,
.dw    $00FF,
.dw    $00FF
;03 set all (fill)
.dw $FF00,
.dw    $FF00,
.dw    $FF00,
.dw    $FF00,
.dw    $FF00,
.dw    $FF00
;04 clear all (fill)
.dw $FFFF,
.dw    $FFFF,
.dw    $FFFF,
.dw    $FFFF,
.dw    $FFFF,
.dw    $FFFF
;05 invert all (fill)
.dw $00FF,
.dw    $00FF,
.dw    $00FF,
.dw    $00FF,
.dw    $00FF,
.dw    $00FF
;06 Draw line \ clear center
.dw $FF7F,
.dw    $FFFE,
.dw    $FFFF,
.dw    $FF00,
.dw    $FF00,
.dw    $FF00
;07 clear line \ black center
.dw $FF80,
.dw    $FF01,
.dw    $FF00,
.dw    $FFFF,
.dw    $FFFF,
.dw    $FFFF
;08 draw line \ invert center
.dw $807F,
.dw    $01FE,
.dw    $00FF,
.dw    $FF00,
.dw    $FF00,
.dw    $FF00
pDDBTZaiEnd:  ;hugs to another cute one
#endif
;================================

pBoxShift:
bsxpos   equ var1
bsypos   equ var2
bswidth  equ var3
bsheight equ var4
;         0 1 2 3 4 5   6    7      8
;identity(9,x,y,w,h,Dir,Type,Repeat,LCD_update)
 ld a,(bsxpos)
 ld hl,(bswidth)
 ld e,l           ;E=width for later use
 sub 12
 jp nc,Err_BOUNDS1
 add a,l
 dec a
 jp p,Err_BOUNDS2
 ld a,(bsypos)
 ld hl,(bsheight)
 ld d,l           ;D=height for later use
 sub 64
 jp nc,Err_BOUNDS3
 add a,l
 dec a
 jp p,Err_BOUNDS4
 ld a,l
 cp 2
 jp c,Err_BOUNDS5 ;boxes must not be shorter than 2
 ld (temp2),de
 
 ld hl,(bsypos)
 add hl,hl
 add hl,hl
 ld e,l
 ld d,h
 add hl,hl
 add hl,de
 ld de,(bsxpos)
 add hl,de
 ld (temp1),hl     ;temp1 = address to start buffer writing 

 ld a,(var6)
 and 3
 ld b,a
 ld a,(var4)
 and 3
 add a,a
 add a,8
 djnz $-2
 ld e,a
 ld d,0
 ld hl,pBSShiftBlock
 add hl,de
 ld a,(hl)
 inc hl
 ld h,(hl)
 ld l,a      ;address to jump to retrieved here
 ld a,(var7)
 ld b,a
 push bc
  push hl
   ld de,$+9
   push de
   ld de,(temp2)  ;iterations required.
   jp (hl)
;RET from called routine takes us to here.
  pop hl
 pop bc
 djnz $-9
 ld a,(var8)
 or a
 jp nz,fastCopy
 ret


pBSShiftBlock:
.dw pBSSKeepU
.dw pBSSKeepD
.dw pBSSKeepL
.dw pBSSKeepR

.dw pBSSBlackU
.dw pBSSBlackD
.dw pBSSBlackL
.dw pBSSBlackR

.dw pBSSWhiteU
.dw pBSSWhiteD
.dw pBSSWhiteL
.dw pBSSWhiteR

.dw pBSSRotU
.dw pBSSRotD
.dw pBSSRotL
.dw pBSSRotR

pBSSKeepU:
 ld de,(temp1)
 ld hl,(temp1)
 ld bc,12
 add hl,bc

 ld a,(temp2+1)
 dec a
 ld b,a
pBSSKeepULoop:
 push bc
  push hl
   push de
    ld bc,(temp2)
    ld b,0
    ldir
   pop hl
   ld bc,12
   add hl,bc
   ex de,hl
  pop hl
  add hl,bc
 pop bc
 djnz pBSSKeepULoop
 ret
 
pBSSKeepD:
 ld l,d
 ld h,0
 add hl,hl
 add hl,hl
 ld d,h
 ld e,l
 add hl,hl
 add hl,de
 ld de,(temp2)
 ld d,0
 add hl,de
 dec hl
 ld de,(temp1)
 add hl,de
 ld d,h
 ld e,l
 ld bc,-12
 add hl,de
 
 ld a,(temp2+1)
 dec a
 ld b,a
pBSSKeepDLoop:
 push bc
  push hl
   push de
    ld bc,(temp2)
    ld b,0
    lddr
   pop hl
   ld bc,-12
   add hl,bc
   ex de,hl
  pop hl
  add hl,bc
 pop bc
 djnz pBSSKeepDLoop
 ret

pBSSKeepL:
 ld hl,(temp1)
 push hl
  ld a,(hl)  ;get backup
  rra        ;shift in that first bit to keep
  ld b,e
  rl (hl)
  inc hl
  djnz $-3
 pop hl
 ld bc,12
 add hl,bc
 dec d
 jr nz,$-15
 ret

pBSSKeepR:
;E=width
;D=height
 ld hl,(temp1)
 ld a,d
 ld d,0
 add hl,de  ;end of line
 ld d,a
 dec hl
 push hl
  ld a,(hl)  ;get backup
  rla        ;shift in that first bit to keep
  ld b,e
  rr (hl)
  dec hl
  djnz $-3
 pop hl
 ld bc,12
 add hl,bc
 dec d
 jr nz,$-14
 ret

pBSSBlackU:
 ret
pBSSBlackD:
 ret
pBSSBlackL:
 ret
pBSSBlackR:
 ret

pBSSWhiteU:
 ret
pBSSWhiteD:
 ret
pBSSWhiteL:
 ret
pBSSWhiteR:
 ret

pBSSRotU:
 ret
pBSSRotD:
 ret
pBSSRotL:
 ret
pBSSRotR:
 ret
 

;================================

pDrawText:  ;ehh... wait for a few minutes.
;args - 2b total args on stack
;nargs - 1b num args. Includes command argument.
;sargs - 1b num args
;
;fracDrawLFont,(iy+fontFlags) draws large font
;textEraseBelow,(iy+textFlags) erasebelow
;textInverse,(iy+textFlags) reverse video
;textWrite,(iy+sGrFlags)    1=write to buffer
;
;bcall(_get_tok_strng)
;in: HL=ptr to token out: A=strlen BC=strlen OP3=stringloc
;bcall(_isa2bytetok)
;in: A=1st byte of token out:NZ if it's first part of 2bytetok. Z if not.
;
;var1 = flags
;bit: 0 = set largefont
;     1 = set texterasebelow
;     2 = set textinverse
;     3 = set hexoutput mode
;var2 = newx
;var3 = newy
 ld hl,sc1
 call getsourcestring
 jp z,Err_NULLVAR
 ld a,(iy+fontFlags) \ ld e,a
 ld a,(iy+textFlags) \ ld d,a
 push de  ;saving flags

 ld a,(nargs)
 cp 3
 jr c,$+14
 ld a,(var2)
 ld (pencol),a
 ld a,(var3)
 ld (penrow),a
 ld a,(var1)
 
 res fracDrawLFont,(iy+fontFlags)
 res textEraseBelow,(iy+textFlags)
 res textInverse,(iy+textFlags)
 set textWrite,(iy+sGrFlags)
 rrca \ jr nc,$+6
 set fracDrawLFont,(iy+fontFlags)
 rrca \ jr nc,$+6
 set textEraseBelow,(iy+textFlags)
 rrca \ jr nc,$+6
 set textInverse,(iy+textFlags)
 rrca \ jr c,pDrawTextHexMode
pDrawTextL1:
 push hl
  push bc
   bcall(_get_tok_strng)
   ld b,a
   ld hl,Op3
   ld a,(hl)
   inc hl
   bcall(_VPutMap)
   djnz $-5
  pop bc
 pop hl
 ld a,(hl)
 bcall(_isA2ByteTok)
 inc hl
 dec bc
 jr nz,$+4
 inc hl
 dec bc
 ld a,b
 or c
 jr nz,pDrawTextL1
 jr pDrawTextEnd
pDrawTextHexMode:
 or a
 rr b \ rr c
 jp c,Err_INVALIDSTRING   ;string not hex.
pDrawTextHexModeL:
 ld e,(hl)
 inc hl
 ld d,(hl)
 inc hl
 ld (Op6),de
 push hl
  call ConvertTtoH
  ld a,h
  bcall(_VPutMap)
 pop hl
 dec bc
 ld a,c
 or b
 jr nz,pDrawTextHexModeL
pDrawTextEnd:
 pop hl
 ld a,l \ ld (iy+fontFlags),a
 ld a,h \ ld (iy+textFlags),a
 res textWrite,(iy+sGrFlags)
 ld a,(var1)
 bit 4,a
 jp nz,fastCopy
 ret

;================================
