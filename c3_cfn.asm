;CELTIC major revision 3 by Rodger Weisman
;This software is licensed in accordance to the attached
;ReadMe file, however perverted it may be.
;Subroutines Celtic III functions
fToggleSetT:  ;get program stats
;output: "1234XXXXX"
;1= A/R Arc/RAM
;2= H/V Hidden/Visible
;3= L/W Lock/Writable
;4= Reserved (space)
;XXXXX = five digit size
;
 set gGroup,(iy+myflag)
 ld hl,8
 call makestring+3  ;output: DE=addy
 push de
  call getprogram
 pop hl
 ld a,(cpage)
 ld bc,$4152 ;C='A' B='R'
 jp p,$+5
 ld b,'E'    ;If in ExRAM, "E"
 or a
 call LoadBIncOnZero
 ld bc,$4856 ;'V'+(256*'H')
 bit Hidden,(IY+myflag)
 call LoadBIncOnZero
 ld a,(Op1)
 ld bc,$4C57 ;'L'+(256*'W')
 cp $05
 call LoadBIncOnZero
 ex de,hl  ;DE=addressOfString, HL=size
 call convHLtoDec
 bcall(_Op4toOp1)
 ret
LoadBIncOnZero:
 ld (hl),c
 jr z,$+3
 ld (hl),b
 inc hl
 ret
fToggleSet:
 call getprogramname
 ld a,(Op1)
; 0=arc/unarchive
; 1=lock/unlock
; 2=prog/appvar
; 3=hide/unhide
; 4=delete
; 5=create
; 6=progtoString
; 7=getprogramstats
; >6=nothing
 ld bc,(var1)
 inc c   ;preserves carry flag
 dec c \ jr z,fToggleSetA ;arc
 dec c \ jr z,fToggleSetL ;lock
 dec c \ jr z,fToggleSetV ;p<->a
 dec c \ jr z,fToggleSetH ;hide
 dec c \ jr z,fToggleSetD ;del
 dec c \ jr z,fToggleSetC ;create
 dec c \ jr z,fToggleSetS ;prog->str
 dec c \ jr z,fToggleSetT ;get stats
 ret
fToggleSetA:  ;archive
 jr c,fToggleSetL       ;jump to same condition to hit Err_PRGMNOTFOUND
 bcall(_Arc_Unarc)
 call getprogramname
 ld a,b
 jr fToggleSetCom
fToggleSetL:  ;lock program
 jp c,Err_PRGMNOTFOUND
 cp AppVarObj
 jr nz,$+6
 bcall(_Op1Set2)
 ret
 inc a
 cp 7
 jr nz,$+4
 ld a,5
 ld (hl),a
 sub 5
fToggleSetCom:
 bcall(_SetXXOp1)
 ret
fToggleSetV:   ;toggle avar
 jr c,fToggleSetL       ;jump to same condition to hit Err_PRGMNOTFOUND
 ld bc,appvarobj+(256*progobj) ;b=progobj c=appvarobj
 cp c
 jr nz,$+3
 ld c,b
 ld a,c
 ld (hl),a
 jr fToggleSetCom 
fToggleSetH:  ;hide program
 jr c,fToggleSetL       ;jump to same condition to hit Err_PRGMNOTFOUND
 ld de,-7
 add hl,de
 ld a,$40
 cp (hl)
 jr nc,$+4
 ld a,-$40
 add a,(hl)
 ld (hl),a
 cp $40
 ld a,0     ;preserving carry flag
 adc a,a    ;carry sets A=1, else A=0
 jr fToggleSetCom
fToggleSetD:   ;delete file
 set gGroup,(iy+myflag)
 call getprogramname
 jr c,fToggleSetL       ;jump to same condition to hit Err_PRGMNOTFOUND
 bcall(_DelVarArc)
 bcall(_Op1Set1)
 ret
fToggleSetC:   ;createprogram
 jp nc,Err_PRGMFOUND
 ld hl,0
 and $1F  ;A already set to var type
 bcall($4E70)  ;_CreateVar: A=type, OP1+1=name, HL=size
 bcall(_Op1Set1)
 ret
fToggleSetS:   ;program to string
 call getprogram
 ex de,hl
 ld a,h
 or l
 jp z,Err_NULLSTRING
 call makestring+3
 push de
  call getprogram
  ld b,d
  ld c,e
 pop de
 call gethlinc
 ld (de),a
 inc de
 dec bc
 ld a,b
 or c
 jr nz,$-8
 bcall(_Op4toOp1)
 ret
;============================
fLineRead:
 call getline
 jr nc,$+11
 bit ConEOF,(iy+myflag)
 jp nz,Err_LINENOTFOUND
 jr fGetLineNum
 ld a,b
 or c
 jp z,Err_NULLLINE
 ld h,b
 ld l,c
 call makestring+3
 push de
  call getline
 pop de
 ld hl,(pLine)
 ld a,(pLine+2)
 ld (cpage),a
 call GetHLInc
 ld (de),a
 inc de
 dec bc
 ld a,b
 or c
 jr nz,$-8
 bcall(_Op4ToOp1)
 ret
fGetLineNum:
 call fGetLineNumCore
 jp GetListDim        ;leeching off of this routine to get output number
fGetLineNumCore:
 call getprogram
 call setupfindline
 ex de,hl            ;DE taken for addy, HL used for counter.
 ld hl,$0000
fGetLineNum0:
 push hl
  call $8000
 pop hl
 inc hl
 bit HitEOF,(iy+myflag)
 jr z,fGetLineNum0
 ret
;============================
fLineWrite:
 call getprogram
 or a
 jp nz,Err_PRGMARCHIVED
 call getLine
 jp c,Err_LINENOTFOUND
 ld hl,sc2
 call getsourcestring
 jp z,Err_NULLSTRING
 ld de,(pLine)       ;getting pointer to start of line
 inc bc              ;increment to accomodate for tENTER appended.
 ld h,b              ;loading to HL for insert mem size
 ld l,c              ;...
 push hl
  push bc
   push de
    bcall(_EnoughMem)
    jp c,Err_NOMEM
   pop de
  pop bc
 pop hl
 push hl             ;DE is already set to address of insertion
  bcall(_InsertMem)  ;insert the memory
 pop bc
 call updatesizeins
 push de
  ld hl,sc2
  call getsourcestring        ;get the string variable again. It's HL is address to copy
 pop de
 ldir                ;copy string
 ld a,tEnter         ;loading up tENTER
 ld (de),a           ;append tENTER
 bcall(_Op1Set1)
 ret
;============================
fLineErase:
 call getprogram        ;get vectors
 or a                   ;checking
 jp nz,Err_PRGMARCHIVED ;error if archived
 call getLine           ;get line address, store to (pLine)
 jp c,Err_LINENOTFOUND  ;If carry invoked, (pLine) has nothing useful
 bit HitEOF,(iy+myflag) ;check to see if last line was hit
 jr nz,$+3              ;if so, jump over since no NewLine char is there
 inc bc                 ;increment to include tENTER
 res numOP1,(iy+ParsFlag2)
 ld a,b                 ;Check line size.
 or c                   ;checking
 ret z                  ;Quit routine if zero.
 ld hl,(pLine)          ;Address in DE
 ld d,b                 ;Amount in  HL
 ld e,c                 ;...
 push bc                ;Preserve size
  bcall(_DelMem)        ;Delete memory, HL=adr, DE=bytes
  bcall(_Op1Set1)       ;set Op1 to output value
 pop bc                 ;get back size

 jp updatesizedel       ;update size then RET
;============================
fLineReplace:
 call getprogram        ;out: hl=address, bc=size
 or a                   ;check if it is archived
 jp nz,Err_PRGMARCHIVED ;quit if archived. Can't edit archived vars.
 call getLine           ;get line. (pline)=start of line
 jp c,Err_LINENOTFOUND  ;error conditions do not allow proper editing
 push bc                ;save length of the string to remove
  ld hl,sc2
  call getsourcestring  ;get the length of the line to replace with
  ld h,b                ;put this into HL. We'll be comparing with this
  ld l,c                ;...
  jp z,Err_NULLSTRING   ;Nevermind the load. If this was zero, replace with nothing.
 pop de                 ;restore with size to remove.
 push hl                ;save the size of the string
  sbc hl,de              ;Size of replacement - size to remove. HL contains size change
  push hl                ;preserve the change in program size
   jr c,$+10             ;if the removal was of greater size, don't check mem.
   push de               ;preserve bytes to delete
    bcall(_EnoughMem)    ;check if the addition of HL bytes is enough
   pop de                ;restore bytes to delete
   jp c,Err_NOMEM        ;if not, then kill the routine.
   ld hl,(pline)         ;loading with address to remove data
   push hl               ;save address of deletion
    bcall(_DelMem)       ;remove memory from program, including possible ending newline
   pop de                ;restore address deleted from in DE
  pop bc                 ;restore change in program size. Not needed right now.
 pop hl                  ;restore size of the string
 push bc                 ;save (again) the change in program size
  bcall(_InsertMem)      ;insert that memory
 pop bc                  ;restore the change in the program size
 call updatesizeins      ;update the size of the file (BC)
 ld hl,sc2               
 call getsourcestring    ;get contents of str9, HL=address, BC=size
 ld de,(pline)           ;address of insertion point
 ldir                    ;copy string to program
 bcall(_Op1Set1)
 ret                     ;exit routine
;============================
fFindProg:
; ld hl,ProgramEnd
; push hl
 call initFindProg
 ld hl,0
ProgFindLoop1:
 call checkVATEnd
 jr c,ProgFindEndL1
 push hl
  call startFindProg
 pop hl
 jr c,ProgFindLoop1
 inc hl    ;for added space
 add hl,bc
 jr ProgFindLoop1
ProgFindEndL1:
 ld a,h
 or L
 jp z,Err_NULLVAR
 call makestring+3
 push de
  call initFindProg
 pop hl
ProgFindLoop:
 call checkVATEnd
 jr c,ProgFindEnd
 push hl       ;save address of the output string
  call startFindProg  ;serach for program information
 pop hl        ;restore address of the output string
 jr c,ProgFindLoop
 ex de,hl      ;putting in DE address to write to
 ld hl,Op1     ;reading from OP1
 ld a,(hl)     ;loading type byte from OP1
 cp AppVarObj  ;check if appvar type
 jr z,$+3      ;if so, then skip increment to not write type byte in
 inc hl        ;if anything other than appvar, then increment to not include type
 ldir          ;block copy
 ex de,hl      ;put writing pointer back into HL
 ld (hl),$29   ;write space token into file
 inc hl        ;move pointer up one more
 jr ProgFindLoop ;loop back to the program find loop
ProgFindEnd:
 bcall(_Op4toOp1)
 ret

initFindProg:
 ld hl,(progPtr)  ;location of start of prog VAT
 ld (temp1),hl    ;(temp1) = VAT position
 ld hl,sc1
 ld a,(hl)
 or a
 jr z,initFindPEmpty
 call getsourcestring
 ld (temp2),hl    ;(temp2) = string location
 ld (pLine),bc    ;(pLine) = recycled pointer for size.
 ret
initFindPEmpty:
 ld hl,0
 ld (pLine),hl
 ret

checkVATEnd:
 ld de,(temp1)
 ld a,(pTemp)
 sub e
 ld a,(pTemp+1)
 sbc a,d
 ccf
 ret
 pop bc
sfpFail:
 xor a
 ld c,a
 scf
 ret
startFindProg: ;two errors. carry=EOV, C=0 for no entry (try again)
 ex de,hl      ;put the pointer to the VAt into HL
 ld de,Op1     ;put beginning of the destination into DE for manual LDI
 ld a,(hl)     ;load filetype byte into A for comparison
 ld (de),a     ;but it is copied into (DE) before anything is done.
 inc de        ;increment pointer to copy to align to name
 push af
  ld bc,-6     ;skip past data to get to the name information of VAT entry
  add hl,bc    ;perform addition
  ld c,(hl)    ;load size information to register C
  ld b,c       ;backup register C to B for use
  dec hl       ;decrement HL to move it to start of the name information
  ld a,(hl)    ;retrieve byte of name
  ld (de),a    ;store that byte into OP1 space
  dec hl       ;move down to next byte of the name
  inc de       ;increment to write next byte of the name
  djnz $-4     ;loop back using stored B in previous operation
  ld (temp1),hl ;store next VAT position entry back into current pointer
  xor a        ;clear register A
  ld (de),a    ;and null-terminate the string, in case it's not 8 characters long
 pop af       ;performing filetype test after this.
 sub $05      ;/do not pass through if not correct type
 jr z,$+10    ;|jump past any further testing. We have correct type.
 dec a        ;|check for protprog object
 jr z,$+7     ;|jump past further testing.
 inc c        ;|increment namespace size for appvar type
 cp $15-$06   ;|check for appvar type
 jr nz,sfpFail
 push bc              ;save the contents of the size information
  bcall(_ChkFindSym)  ;use the contents of string to perform lookup
  call getprogram+3   ;loads up info for reading. (A):HL=SOF
  ld bc,(pLine)       ;Str0 size
  ld de,(temp2)       ;Str0 location
  ld a,b              ;start checking...
  or c                ;checking if BC=0
  jr z,strtFndProgEnd ;skip testing on null string, user wants all progs
  call getHL          ;Obtain contents of (HL)
  ex de,hl            ;put DE into HL for testing
  cp (hl)             ;comparing up the first byte of program
  ex de,hl            ;switch back for routine
  jr nz,sfpFail-1     ;if no match, then jump to routine exit
  inc de              ;increment DE
  call IncHL          ;increment (cpage):HL
  dec bc              ;decrement BC
  ld a,b              ;checking if
  or c                ;BC == 0
  jr nz,$-15          ;continue looping if there are more letters to test
strtFndProgEnd:
 pop bc              ;remove stack
 ret                 ;Op1=name, C=name length only
;============================
fUngroupFile:
 set gGroup,(iy+myflag)
 call getprogram
 ld a,(Op1)
 cp $17  ;groupobj
 jp nz,Err_NOTAGROUP
 res numOP1,(iy+ParsFlag2)   ;cancel the parse.
 ld a,(var1)
 res overw,(iy+nflags)
 or a
 jr z,$+6
 set overw,(iy+nflags)
 push hl
fLUFFY1:
 pop hl
 call lookupEOF
 ret c
 call groupGetNext
 push hl
  push de
   bcall(_Op2ToOp1)
   bcall(_ChkFindSym)
  pop de
  jr c,fUgFSkipExist
  bit overw,(iy+nflags)
  jr z,fLUFFY1   ;do not attempt to extract if it already exists
  push de
   bcall(_ChkFindSym)
   bcall(_DelVarArc)
  pop de
fUgFSkipExist:
  ld a,(Op1)
  cp $09
  jr c,$+6
  cp $0C
  jr c,fLUFFY1   ;do not attempt to extract this var.
  ld a,(Op1)
  or a
  jr z,fUgFSetS9
  cp $0C
  jr z,fUgFSetS18
  inc de
  inc de   ;to copy size field too
  jr fUgFSkipSet
fUgFSetS9:
  ld de,9
  jr fUgFSkipSet
fUgFSetS18:
  ld de,18
fUgFSkipSet:
  push de
   ld hl,(gSIZE)
   bcall(_CreateVar)
  pop bc
  ld hl,(gSOFp)
  ld a,(gSOFp+2)
  ld (cpage),a
  call gethlinc
  ld (de),a
  inc de
  dec bc
  ld a,b
  or c
  jr nz,$-8
  jp fLUFFY1   ;done extracting this var, moving on to the next one...
;################################################
; set gGroup,(iy+myflag)
; call getprogram
; ld a,(Op1)
; cp $17  ;groupobj
; jp nz,Err_NOTAGROUP
;fUFL01:
; call lookupEOF
; ret c
; call groupGetNext
; push hl
;  push de
;   bcall(_Op2ToOp1)
;  pop de
; pop hl
; ld a,(Op1)
; cp $09
; jr c,$+6
; cp $0C
; jr c,fUFL01
; or a
; jr z,$+6
; cp  $0C
; jr nz,fUFL02
; push af
;  ld a,e
;  ld (Op1),a
;  ld a,d
;  ld (Op1+1),a
;  ld b,7
;  ld de,Op1+2
;  call getHLInc
;  ld (de),a
;  inc de
;  djnz $-5
; pop af
; or a
; jr z,fUFL03
; ld de,Op2
; ld b,9
; call getHLInc
; ld (de),a
; inc de
; djnz $-5
;fUFL03:
; ld a,(Op1)
; push hl
;  bcall(_CreateVar)
; pop hl
; jr fUFL01
;fUFL02:
; push hl
;  push de
;   ex de,hl
;   ld a,(Op1)
;   bcall(_CreateVar)
;   inc de
;   inc de
;  pop bc
; pop hl
; call getHLInc
; ld (de),a
; inc de
; dec bc
; ld a,b
; or c
; jr nz,$-8
; jp fUFL01
;============================
fGetGroup:
 set gGroup,(iy+myflag)
 call getprogram
 ld a,(Op1)
 cp $17  ;groupobj
 jp nz,Err_NOTAGROUP

 ld bc,0
getgrouploop:
 call lookupEOF
 jr c,buildgrpstr
 push bc
  call groupgetnext
 pop bc
 jr c,getgrouploop
 ld a,(nsize)
 inc a
 add a,c
 ld c,a
 jr nc,$+3
 inc b
 ld a,(Op2)
 cp AppVarObj
 jr nz,getgrouploop
 inc bc
 jr getgrouploop
buildgrpstr:
 ld h,b
 ld l,c
 call makestring+3
 push de
  call getprogram
 pop de
getgrouploop2:
 push de
  call lookupEOF
 pop de
 jr c,getgroupEND
 push de
  call groupgetnext
 pop de
 jr c,getgrouploop2
 ld bc,(nsize)
 push hl
  ld hl,Op2
  ld a,(hl)
  inc hl
  cp AppVarObj
  jr nz,$+4
  inc c
  dec hl
  ld b,0
  ldir
  ld a,$29
  ld (de),a
  inc de
 pop hl
 jr getgrouploop2
getgroupEND:
 bcall(_Op4toOp1)
 ret
;============================
fExtGroup:
 set gGroup,(iy+myflag)
 ld de,(var1)
 push de
  call getprogram
 pop de
 ld a,(Op1)
 cp $17  ;groupobj
 jp nz,Err_NOTAGROUP
 
ExtGrItLoop:
 push de
  call lookupEOF
  jp c,Err_PRGMNOTFOUND
  call groupgetnext
 pop de
 jr c,ExtGrItLoop
 dec de
 ld a,d
 or e
 jr nz,ExtGrItLoop
ExtGrItCont:
 bcall(_Op2toOp1)
 bcall(_ChkFindSym)
 jp nc,Err_PRGMFOUND
 ld hl,(gSIZE)
 push hl
  ld a,(Op1)
  cp AppVarObj
  jr z,$+7
  bcall(_CreateProg)
  jr $+5
  bcall(_CreateAppVar)
  inc de
  inc de
  ld hl,(gSOF)
 pop bc
 ld a,(gSOF+2)
 ld (cpage),a
 call GetHLInc
 ld (de),a
 inc de
 dec bc
 ld a,b
 or c
 jr nz,$-8
 bcall(_Op1Set1)
 ret
;============================
fBinRead:
 ld hl,(var2)        ;get Y
 ld a,l              ;check
 or h                ;if zero
 jp z,Err_NULLVAR    ;If zero, then error condition; not reading any bytes
 push hl             ;store # of bytes to read
  add hl,hl          ;double this for string recall creation
  call makestring+3  ;create Str9 with as many bytes as hex digits
  push de            ;save position of string
   ld hl,(var3)
   ld a,(sargs)
   or a
   jr z,$+5
   call getprogram   ;locate program variable
   ld de,(var1)      ;store back nth byte
   call AddDEtoHL    ;get position to start reading from
   push hl
    bcall(_Op4toOp1)
   pop hl
  pop de             ;restore position of the string
 pop bc              ;restore # of bytes to read as BC for looping
 jp BinStringToHex
;============================
fBinWrite:
;Ans bytes from start (DE), Str9=hex string, Str0=file
;Does not check to see if reading past the end of the file.
 call getprogram     ;locate program position in HL
 or a                ;checking if in archive
 jp nz,Err_PRGMARCHIVED ;quit on error condition. Can't write to archived items.
 ld de,(var1)
 add hl,de           ;size+insertion offset. will not invoke carry unless abused
 push hl             ;saving offset for later insertion
  push hl            ;saving offset again for _InsertMem
   ex de,hl          ;for needed subtraction
   ld hl,(EOF)       ;end of file address
   sbc hl,de         ;EOF - CUR. Should never exceed EOF else something is wrong.
   jp c,Err_LINENOTFOUND ;reusing same error code as those found in line read
   ld hl,sc2
   call getsourcestring      ;locate string for its size.
   srl b             ;Divide by two to determine
   rr c              ;# of bytes to insert
   jp c,Err_INVALIDSTRING
   ld h,b            ;load this to HL for _InsertMem
   ld l,c            ;...
   push hl           ;preserve size information
    bcall(_EnoughMem) ;check to see if there's enough memory to insert
   pop hl            ;restore size information
   jp c,Err_NOMEM    ;exit on no memory condition
  pop de             ;restore address to insert at
  push hl            ;preserve size
   bcall(_InsertMem) ;HL=bytes, DE=address
  pop bc             ;restore size to update file with
  call updatesizeins ;update the file's size
  push bc            ;save the size again
   ld hl,sc2
   call getsourcestring      ;get the address of the string at HL
   push hl
    bcall(_Op1Set1)
   pop de
  pop bc             ;restore the size so we don't have to do srl b \ rr c again
 pop hl              ;restore insertion address of the file
 jp HexStringToBin
;============================
fBinDelete:
 call getprogram  ;get program position in HL.
 or a             ;checking if in archive
 jp nz,Err_PRGMARCHIVED ;throw error condition if it is
 ld de,(var1)
 add hl,de        ;add offset and position to form position from which to delete.
 push hl          ;save this position.
  ld bc,(var2)    ;get the number of bytes to delete.
  add hl,bc       ;add this together.
  ld de,(EOF)     ;get the address of the end of file
  ex de,hl        ;exchange HL and DE so carry is set if # of bytes at end exceeds EOF
  sbc hl,de       ;EOF-current(plusdel)
  jp c,Err_LINENOTFOUND ;throw error condition if trying to delete past end of file.
 pop hl           ;restore the file's position.
 ld d,b           ;load the amount to delete loaded from (ipart) a while back.
 ld e,c           ;...
 push bc          ;save the amount to delete
  bcall(_DelMem)  ;HL=address, DE=bytes. Delete the memory.
 pop bc           ;restore amount that was deleted.
 bcall(_Op1Set1)
 jp updatesizedel
;============================
fGetArgType:
 ld a,(sc1)
 bcall(_SetXXOp1)
 ret
;===========================
fStringToBin:
 ld hl,sc1
 call getsourcestring
 srl b
 rr c
 push hl
  push bc
   jp c,Err_INVALIDSTRING
   ld h,b
   ld l,c
   call makestring+3
   push de
    bcall(_Op4toOp1)
   pop hl
  pop bc
 pop de
 jp HexStringToBin
;===========================
fBinToString:
 ld hl,sc1
 call getsourcestring
 push hl
  push bc
   ld l,c
   ld h,b
   add hl,hl
   call makestring+3
   push de
    bcall(_Op4toOp1)
   pop de
  pop bc
 pop hl
 jp BinStringToHex          ;exit
;===========================
fGetListElem:
 set gList,(iy+myflag)
 call getprogram
 ld a,(Op1)
 cp $0D
 jr z,$+7
 cp $01
 jp nz,Err_NOTALIST
 ex de,hl            ;Now, HL=size, DE=addy
 ld bc,(var1)        ;bc=getting this size
 ld a,b              ;loading size MSB to A for...
 or c                ;checking to see if B and C are zero.
 jr z,GetListDim     ;If so, check for the size of the variable
 sbc hl,bc           ;check
 jp c,Err_ENTRYNOTFOUND ;if exceeded, quit on err
 dec bc              ;adjust to correct seek
 ld h,b              ;copy bc to hl
 ld l,c              ;
 add hl,hl           ;multiply hl by 9
 add hl,hl           ;
 add hl,hl           ;
 add hl,bc           ;
 ld a,(Op1)
 cp $0D
 push af
  jr nz,$+3
  add hl,hl           ;multiply by 18 for complex list objects.
  ex de,hl            ;
  call adddetohl      ;add in offset with starting address
  ld bc,9             ;copy 9 bytes
  ld a,(cpage)        ;get page
  ld de,Op1           ;destination in Op1
  bcall(_FlashToRAM)  ;OP1 now contains flash version
 pop af
 ret nz               ;return if it's a real list. else...
 ld bc,9
 bcall(_FlashToRAM)   ;keep going with the write.
 ret
 
;================================
fChkstats:
 ld a,(var1)
 or a  \ jr z,fChkRAM
 dec a \ jr z,fChkROM
 dec a \ jr z,fChkSerial
 dec a \ jr z,fChkOSVer
 ret
fChkRAM:
	bcall(_freeRAM)					;bytes of free RAM
GetListDim:         ;leeching off of this routine.
	bcall(_SetXXXXOp2)
	bcall(_Op2toOp1)
	ret
fChkRAMRoutine:
	call fChkRAM
	jp xLIBEnd			;don't clean op1!
fChkROM:
	bcall(_ChkfreeArc)
	ld hl,Op1Set65536
	rst 20h
	ld hl,($839F)
	ld a,l
	ld l,h
	ld h,a
	bcall(_SetXXXXop2)
	bcall(_FPMult)
	ld hl,($83A1)
	ld a,l
	ld l,h
	ld h,a
	bcall(_SetXXXXOp2)
	bcall(_FPAdd)
	ret
fChkSerial:
	bcall($807E)
	bcall(_Op4toOp6)   ;move needed data out of harm's way
	ld hl,Op6
	ld bc,5
	jp fBinToString+6 
fChkOSVer:
	ld hl,4
	call makestring+3
	ld hl,$0064
	ldir
	bcall(_Op4toOp1)
	ret
;================================
fGroupMem:
 set gGroup,(iy+myflag)
 call getprogram
 ld a,(Op1)
 cp $17  ;groupobj
 jp nz,Err_NOTAGROUP
 ld de,(var2)
 push de
  call lookupEOF
  jp c,Err_PRGMNOTFOUND
  call groupgetnext
 pop de
 jr c,$-11
 dec de
 ld a,d
 or e
 jr nz,$-16
 ld hl,(gsize)
 jp GetListDim   ;leech off of routine that does work to output

;================================
fExecHex:
 ld hl,sc1
 call getsourcestring
 ex de,hl
 srl b
 rr c
 jp c,Err_INVALIDSTRING
 ld hl,-$02FE
 add hl,bc
 jp c,Err_INVALIDSTRING
 ld hl,savesscreen
 call HexStringToBin
 ld (hl),$C9         ;save RET byte at end of the string
 jp savesscreen
;================================
fNumToString:
;Test call to ConvFloatToText
;Previous entry stored to Op2. Check for that.
;input: DE=location to store string, Op1=numberToConvert
;real=0 list=1 matrix=2
 bcall(_Op2toOp1)
 ld a,(Op1)
 and %00011111
 or a  \ jp z,fNTSReal
 dec a \ jp z,fNTSList
 dec a \ jp z,fNTSMatrix
 sub 2
 jp nz,Err_ARGUMENT   ;passthru to allow list name to be passed in case it's archived.
fNTSListString:
 set gList,(iy+myflag)
 call getprogramname
 jp c,Err_PRGMNOTFOUND
 ld a,(Op1)
 dec a
 jp nz,Err_NOTALIST
 ld hl,Op1
 ld de,sc1
 ld bc,9
 ldir
 jp fNTSList
 
fNTSMatrix:
;lsb contains all in one row, msb contains number of rows
 call fNTSLookupFile ;size info is in BC
 push bc           ;save "size" field of variable
  push bc          ;save size filed of variable for further use
   ld a,b
   or c
   jp z,Err_NULLVAR ;kill routine if not.
   push hl          ;save the start of the matrix
    ld hl,0         ;set HL to zero for couting.
    ld d,l          ;set this to zero for adding the LSB value repeatedly
    ld e,c          ;set LSB for #'s in row
    add hl,de       ;accumulate the repeated addition for simulated multiplication.
    djnz $-1        ;keep looping until the counter hits zero.
    ld c,l          ;load the final results
    ld b,h          ;to BC for counting purposes.
   pop hl   ;to get total bytes in
   call fNTSGetSize
  pop bc            ;Restore matrix size
  ld hl,0
  ld a,b
  ld b,l
  inc c
  add hl,bc
  dec a
  jr nz,$-2
  inc hl
  inc hl
  add hl,de         ;gets correct size to build string with
  call makestring+3        ;Then create the string containing that code.
  ld hl,tLBrack + (256*tRBrack)
  ld (temp1),hl
  ld a,L
  ld (de),a
  inc de
  call fNTSLookupFile
 pop bc                    ;size field
fNTSMatrix1:
 push bc
  ld b,0
  call fNTSWriteList
 pop bc
 dec b
 jr nz,fNTSMatrix1
 ld a,tRBrack
 ld (de),a
 bcall(_Op4toOp1)
 ret

fNTSReal:
 ld de,sc1
 push de
  call ConvFloatToText
  ld l,c
  ld h,b
  push bc
   call makestring+3
  pop bc
 pop hl
 ldir
 bcall(_Op4toOp1)
 ret

fNTSList:
 call fNTSLookupFile
 ld a,c
 or b
 jp z,Err_NULLVAR
 push bc                  ;save list size. Each element gets one comma except last which gets an ending bracket
  push bc                 ;one more push for later's size count.
   call fNTSGetSize
  pop hl
  add hl,de                ;add on to text size to compensate for insertted commas
  inc hl                   ;Increment to handle end of list issue.
  call makestring+3        ;Then create the string containing that code.
  ld hl,tLBrace + (256*tRBrace)
  ld (temp1),hl
  call fNTSLookupFile
 pop bc
 call fNTSWriteList
 bcall(_Op4toOp1)
 ret
 
fNTSGetSize:
 ld de,0
 push bc                  ;save list iterations size
  push hl
   push de
    ld de,Op1             ;copy to OP1
    ld bc,9               ;nine bytes
    ld a,(cpage)          ;from this page
    bcall(_FlashToRAM)    ;Copy now.
    ld de,Op3             ;Write dummy write to
    call ConvFloatToText  ;Op3.
   pop hl                 ;string size info
   add hl,bc
   ex de,hl
  pop hl                  ;restore list info
  ld bc,9
  call addbctohl1pg
 pop bc                   ;restore list size information
 dec bc                   ;decrement
 ld a,c                   ;and check to
 or b                     ;see if it is zero.
 jr nz,fNTSGetSize+3      ;If not, keep looping.
 ret
fNTSWriteList:
 ld a,(temp1+0)
 ld (de),a
 inc de
 push bc
  push hl
   push de
    ld de,Op1
    ld bc,9
    ld a,(cpage)
    bcall(_FlashToRAM)
   pop de
   call ConvFloatToText
   ld a,tComma
   ld (de),a
   inc de
  pop hl
  ld bc,9
  call addbctohl1pg
 pop bc
 dec bc
 ld a,b
 or c
 jr nz,fNTSWriteList+5
 dec de
 ld a,(temp1+1)
 ld (de),a
 inc de
 ret
fNTSLookupFile:
 push de                  ;
  ld hl,sc1               ;
  rst 20h                 ;
  bcall(_ChkFindSym)      ;
  call getprogram+3       ;
  ld b,d
  ld c,e
 pop de                   ;
 ret
;================================
fEdit1Byte:
;det(31,"binstrng",start,newbyte)
;Works best if using a string variable since no output will
;be given. For the sake of keeping RAM intact.
 ld hl,sc1
 call getsourcestring
 ex de,hl
 ld hl,(var1)
 push hl
  or a
  sbc hl,bc
  jp nc,Err_LINENOTFOUND
  bit mode16,(iy+nflags)
  jr z,$+10
  ld bc,1                ;check one more byte out.
  sbc hl,bc
  jp nc,Err_LINENOTFOUND
 pop hl
 add hl,de
 ld de,(var2)
 ld (hl),e
 bit mode16,(iy+nflags)
 jr z,$+4
 inc hl
 ld (hl),d
 res numOp1,(iy+parsFlag2)
 ret
;================================
fModeChange:
 call findappvar   ;gets address of the appvar in HL
 ld a,(var2)
 cp C3AVSize-3
 jp Err_LINENOTFOUND
 inc hl
 inc hl
 inc hl  ;increment past very important data in appvar
 ld e,a
 ld d,$00
 add hl,de
 ld a,(var1)
 or a
 ld a,(hl)
 jr nz,fModeChangeC
fModeChangeR:
 call ConvertHtoT1b
 ld hl,2
 call makestring+3
 ld hl,Op6
 ldir
 ret 
fModeChangeC:
 ld c,a
 ld a,(var3)
 ld (hl),a
 ld a,c
 jr fModeChangeR
;================================
fIndexFile:   ;if possible, creates a file used for indexing by line number
	;idx file has -6- -S-E-V-E-N- SIX byte header: (not $BB,$6D,$C9,"C3I", but) $BB,$6D,$AB,$C9,"3I"
	res numOp1,(iy+parsFlag2)
	ld hl,sc2
	call getprogramname+3
	bcallnc(_DelVarArc)
	call fGetLineNumCore  ;HL=#oflines
	inc hl                ;used for starting element
	add hl,hl             ;entries are 2 bytes long
	push hl
		ld de,25             ;header size and possible VAT size	;CRM 2012-05-01: Added one for rand/Ans "ignore", removed one for "C"
		add hl,de            ;possible size difference with slack. Check for free mem.
		push hl
			bcall(_MemChk)
			pop bc
		or a
		sbc hl,de            ;AvailableMemory - MemoryNeeded =RemainingMemory.
		jp c,Err_NOMEM
		ld hl,sc2
		call getprogramname+3  ;put name in Op1 for creation
		pop hl
	ld bc,6				;CRM 2012-05-01: WAS 6, added ignore byte, removed "C"
	push bc
		add hl,bc
		ld a,(Op1)
		bcall(_CreateVar)
		inc de
		inc de
		ld hl,fIndexFileHead
		pop bc
	ldir
	push de
		call getprogram
		pop ix   ;loading IX to address in index file
	ex de,hl            ;DE taken for addy, HL used for counter.
	ld hl,$0000
	res HitEOF,(iy+myflag)
	res ConEOF,(iy+myflag)
fGetLineNum1:
	ex de,hl
	ld (ix+0),e
	inc ix
	ld (ix+0),d
	inc ix
	ex de,hl
	push hl
		call $8000
		pop hl
	add hl,bc
	bit ConEOF,(iy+myflag)
	jr z,fGetLineNum1

	bcall(_zeroop1)
	
	inc (ix-2)      ;adjusting final line
	ret nz
	inc (ix-1)      ;if MSB needs incrementing too.
	ret
fIndexFileHead:
	.db $BB,$6D,$AB,$C9,"3I"		;CRM 2012-05-01: Added $AB (xor e) for ignore
;================================
fLookupIndex:
 ld hl,sc2             ;
 call getprogramname+3 ;
 ld hl,op1
 call getprogram+3     ;
;add in code later that will actually test contents of first six bytes for index file
 ex de,hl
 ld bc,-8
 add hl,bc
 sra h
 rr l
 ld (temp1),hl
 ex de,hl
 ld de,fIndexFileHead
 call CheckFileHeader
 ld de,4               ;skip over file header minus minimum number
 call adddetohl        ;
;
 ld de,(var1)          ;get line # offset
 push hl
  ld hl,(temp1)
  or a
  sbc hl,de
  jp c,Err_LINENOTFOUND
 pop hl
 
 
 ld a,d
 or e                  ;checking if zero was passed. If so, then output # of lines
 jp z,fLookupIndexLines
 sla e                 ;double it for index sizing
 rl  d                 ;
 call adddetohl        ;
 call gethlinc         ;
 ld e,a                ;
 call gethlinc         ;
 ld d,a                ;
 push de               ;
  ld de,(var2)         ;
  push hl
   ld hl,(var1)
   add hl,de
   ld bc,(temp1)
   dec hl              ;final line+1 to deal with carry-based fencepost error
   or a
   sbc hl,bc           ;final line - total lines. Should always trigger carry.
   jp nc,Err_LINENOTFOUND
  pop hl
  sla e                ;
  rl  d                ;
  add hl,de            ;
  call gethlinc        ;
  ld e,a               ;
  call gethlinc        ;
  ld d,a               ;
 pop bc                ;
 ex de,hl              ;
 or a                  ;
 sbc hl,bc    ;length to copy = HL 
 dec hl       ;kludge thrown in to remove the offending newline character
 ld a,l
 or h
 jp z,Err_NULLLINE
 push hl      ;length
  push bc     ;offset
   call makestring+3
   push de    ;addy
    call getprogram
   pop bc     ;addy
  pop de      ;offset
  call adddetohl
  ld e,c
  ld d,b      ;moving addy to DE for writing
 pop bc       ;length to use
 call gethlinc
 ld (de),a
 inc de
 dec bc
 ld a,b
 or c
 jr nz,$-8
 bcall(_Op4toOp1)
 ret
fLookupIndexLines:
 ld hl,(temp1)
 jp GetListDim     ;leeching off of this routine yet again to output number
;================================
fErrorHandle:
;TI-BASIC error catcher. Base code by Benjamin Moody
;Original file released to public domain
;Edits for (hopeful) Celtic III functionality by Rodger Weisman
;;; Error codes:
;;;  Overflow     1    Memory      14    StatPlot    27   Validation  40
;;;  DivBy0       2    Invalid     15    TolTooSmall 28	  Length      41
;;;  SingularMat  3    IllegalNest 16    Reserved    29	  Application 42
;;;  Domain       4    Bound       17    Mode        30	  AppErr1     43
;;;  Increment    5    GraphRange  18    LnkErr      31	  AppErr2     44
;;;  Break        6    Zoom        19    LnkMemErr   32	  ExpiredApp  45
;;;  NonReal      7    Label       20    LnkTransErr 33	  BadAddr     46
;;;  Syntax       8    Stat        21    LnkDupErr   34	  Archived    47
;;;  DataType     9    Solver      22    LnkMemFull  35	  Version     48
;;;  Argument    10    Singularity 23    Unknown     36	  ArchFull    49
;;;  DimMismatch 11    SignChange  24    Scale       37	  Variable    50
;;;  Dimension   12    Iterations  25    IdNotFound  38	  Duplicate   51
;;;  Undefined   13    BadGuess    26    NoMode      39

begPC		equ 965bh	; beginning of variable
curPC		equ 965Dh	; current position
endPC		equ 965Fh	; end of variable
 ld a,(var1)
 or a
 jr nz,fErrorHandleS
 call getprogramname
 ld a,b
 jp z,fErrorHandleP
 bcall(_Arc_Unarc)
 jr $-10
fErrorHandleS:
 ld hl,TempName
 rst 20h
 bcall(_ChkFindSym)
 jr c,$+5
 bcall(_Delvar)
 ld hl,sc1
 call getsourcestring
 push bc		; size of program
  push hl		; start of data
   push bc		; size
    ld hl,TempName
    rst 20h
    pop hl		; size
   bcall(_CreateProg)
   ld (hl),TempProgObj
   pop hl		; start
  pop bc		; size
 inc de
 inc de
 ldir
 ;; Save current parser state
fErrorHandleP:
 ld hl,parseVar
 rst 20h
 rst 18h   ;rPUSHREALO1
 ld hl,(curPC)
 ld de,(begPC)
 or a
 sbc hl,de
 push hl			; current - beginning
  ld hl,(endPC)
  or a
  sbc hl,de
  push hl		; end - beginning
   ld hl,ErrH
   AppOnErr(ErrH)
   ld hl,TempName
   rst 20h
   bcall(_ParseInp)
   AppOffErr
   xor a
ErrH:
   and 7fh
   push af
    bcall(_PopRealO1)
    ld hl,OP1
    ld de,parseVar
    bcall(_Mov9B)	
    bcall(_ChkFindSym)
    jp c,Err_ENTRYNOTFOUND
    inc de
    inc de
    ld (begPC),de
   pop af
  pop hl		; end - beginning
  add hl,de
  ld (endPC),hl
 pop hl			; current - beginning
 add hl,de
 ld (curPC),hl
 bcall(_SetXXOp1)
 ret
TempName:
.db TempProgObj,"tempexec"

;================================
fMatToStr:
	ld hl,sc1
	rst 20h
	ld a,(Op1)
	cp 2
	jp nz,Err_ARGUMENT
	bcall(_ChkFindSym)
	ld b,a
	jp nz,Err_PRGMARCHIVED
	ex de,hl
	ld e,(hl)
	inc hl
	ld b,(hl)
	inc hl
	push hl
		ld d,0
		ld h,d
		ld l,d
		add hl,de
		djnz $-1
		pop de     ;DE=matrixaddress
	push hl    ;save number of loops
		add hl,hl ;make this the size
		push de   ;save matrixaddress
			call makestring+3  ;making a string should not change matrix address
			pop hl    ;recall matrix address. String output on DE
		pop bc   

fMatToStrL:
	push bc
		push hl
			push de
				call ConvOp1C+3
				ld a,e
				call ConvertHtoT1b
				pop de
			ld hl,Op6
			ldi
			ldi
			pop hl
		ld bc,9
		add hl,bc
		pop bc
	dec bc
	ld a,b
	or c
	jr nz,fMatToStrL
	bcall(_Op4toOp1)
	ret
;================================
fStringRead:
 ld hl,sc1
 call getsourcestring
 ld de,(var1)             ;start location
 ld a,d
 or e
 jp z,fStringReadSize
 dec de
 add hl,de
 ld bc,(var2)
 ld a,c
 or b
 jp z,Err_NULLVAR
 push hl
  push bc
   push bc
   pop hl
   add hl,hl
   bcall(_CreateTStrng)
   push de
    bcall(_Op4toOp1)
   pop de
  pop bc
 pop hl
 inc de
 inc de
 jp BinStringtoHex
fStringReadSize:
 ld h,b
 ld l,c
 jp GetListDim
;================================
fHexToDec:
 ld hl,sc1
 call getsourcestring
 ld de,Op6
 ld bc,4
 ldir
 call ConvertTtoH
 jp GetListDim
;================================
fDecToHex:
 ld hl,(var1)
 call ConvertHtoT2b
 ld b,4
 ld hl,Op6
 ld a,(var2)
 or a
 jr nz,fDecToHexNJ
 ld a,'0'
 cp (hl)
 inc hl
 jr nz,$+4
 djnz $-4
 dec hl
 ld a,b
 or a
 jr nz,$+3
 inc b
fDecToHexNJ:
 ld c,b
 ld b,0
 push bc
  push hl
   push bc
   pop hl
   call makestring+3
  pop hl
 pop bc
 ldir
 ret

;================================
fEditWord:
 set mode16,(iy+nflags)
 jp fEdit1Byte
;================================
fBitOperate:
 ld hl,fBORouteS
 ld de,$8000
 ld bc,fBORouteE-fBORouteS
 ldir
 ld hl,CIIILogicSeries
 ld a,(var3)
 and 3
 add a,l
 ld l,a
 ld a,(hl)
 ld (fBORouteW1),a
 inc a
 ld (fBORouteW2),a
 ld hl,(var1)
 ld de,(var2)
 jp $8000
fBORouteS:
 ld a,h
fBORouteW1 = ( $ - fBORouteS ) + $8000
 nop
 ld h,a
 ld a,l
fBORouteW2 = ( $ - fBORouteS ) + $8000
 nop
 ld l,a
 jp GetListDim
fBORouteE:

;================================
fGetProgList:
;det(32,"SERACHSTRING",SearchType)
;NOTE: gList is used to check for appvar condition, since lists can
;      found using SetupEditor command
 ld hl,sc1
 call getsourcestring
 ld b,0
 ld a,c
 or a
 jp z,Err_NULLSTRING
 cp 9
 jp nc,Err_ENTRYTOOLONG
 ld de,Op3
 ld (de),a
 inc de
 ldir
 ld a,(var1)
 cp 1
 jr nz,$+6
 set gList,(iy+myflag)
 cp 2
 jr nz,$+6
 set gGroup,(iy+myflag)
 ld de,0
 ld hl,(progptr)
fGPLloop1:
 push de
  call matchVAT
 pop de
 jr c,fGPLEnd1
 inc de
 ld a,(Op2)
 add a,e
 ld e,a
 ld a,d
 adc a,$00
 ld d,a
 jr fGPLloop1
fGPLEnd1:
 ex de,hl
 ld a,h
 or l
 jp z,Err_PRGMNOTFOUND
 push hl
  bcall(_PushOP3)
 pop hl
 call makestring+3
 push de
  bcall(_PopOP3)
 pop de
 ld hl,(progptr)
fGPLloop2:
 push de
  call matchVAT
 pop de
 ret c
 push hl
  ld hl,Op2
  ld c,(hl)
  ld b,0
  inc hl
  ldir
  ld a,tSpace
  ld (de),a
  inc de
 pop hl
 jr fGPLloop2
;================================
;det(33,function,args)




fDoLink:
 ret

;================================
fOnBlock:
;code pulled from Justin Wales' program "ONBLOCK", modified to
;run correctly in Celtic III

	bcall(_RclAns)
	bcall(_ConvOp1)
	ld a,e
	or a
	jr z,Install_Interrupt
	dec a
	ret nz
Uninstall_Interrupt:
	im 1
	ret
Install_Interrupt:

	di
	ld hl,$9900
	ld de,$9901
	ld bc,256
	ld (hl),$9a
	ldir


	ld	hl,interrupt_start			
	ld	de,$9a9a				
	ld	bc,interrupt_end-interrupt_start	
	ldir						
	ld	a,$99
	ld	i,a			
	im	2			;switch to mode 2
	ei				;enable interrupts
	ret




Interrupt_Start:
	ex af,af'
	exx
	in a,($03)
	and %11111110
	out ($03),a
	call $003A
	reti
Interrupt_End:


;================================
;================================
;================================
