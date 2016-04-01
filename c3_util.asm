;CELTIC major revision 3 by Rodger Weisman
;This software is licensed in accordance to the attached
;ReadMe file, however perverted it may be.
;Subroutines, main utility file
;Utility routines file for Celtic III project
;
;From WikiTI romcall reference:
;CreateVar : bcall($4E70) A=type, HL=size, OP1+1 = varname
;CreateTempString: bcall($4324) HL=length. Output: Op4=name, HL=vat, DE=data
;CreateTempRList:  bcall($4312) HL=elements. Output: Op4=name, HL=vat, DE=data
;=============================================================
;Collected subroutines and their function:
;ReturnString:    Creates new string containing character "1".
; inputs:          No input
; output:          Returns to homescreen
;ProgramErr:      Outputs "error" code string.
;  ...             Address input of string made to stack
;  ...             Returns to homescreen
;xlcmpexit:       Sets Op1 to real 0, then exits.
;  ...             No input
;  ...             Returns to homescreen
;makestring+3:    Creates string of specified size.
;                  HL=size of new temp string
;                  BC=size;DE=address_of_data;Op4,Op1=file_name
;getsourcestring: Obtains address of string given the address of its name
;                  HL=9byte_location_of_name
;                  HL=data_start;BC=file_size;DE=VAT_location
;getsourcestring_plain: Obtains address of string given the address of its name, plus length in de-tokenized chars
;                  HL=9byte_location_of_name
;                  IX=data_start;DE=character_length
;getsize:         Small blurb of code that obtains size of variable given initial address
;                  EX DE,HL immediately after ChkFindSym if in RAM.
;                  HL=data_start;BC=file_size
;getprogramname:  Retrieves name of program from string which name is in sc1 location.
;                  No input; if getprogramname+3, HL=address_of_string_name
;                  Results of ChkFindSym, except A. Hidden,(IY+myflag) set if hidden
;getprogram:      Obtains location of program file from named string.
;                  SC1 location contains string name to fetch program name/lookup from.
;                  HL=address;DE=size;(cpage)=page#;(SOF)=startoffile(beforesize),EOF...
;getvarcoll:      Subroutine of getprogram
;                  HL=start of file (immediately after chkfindsym in RAM
;                  Same output as getprogram
;getHL:           Retrieves byte from HL be it in RAM or Archive.
;                  (cpage)=page,HL=address
;                  A=byte at (cpage):HL
;getHLInc:        Retrieves a byte from HL and increments HL.
;                  same as getHL
;                  same as getHL
;adddetohl:       Adds number DE to address HL wrt pages.
;                  (cpage)=page,HL=address,DE=increment_amount
;                  (cpage) and HL updated; DE intact.
;findlineRAM:     Locates the next line of BASIC code in RAM
;                  DE=address of current line
;                  DE=end of line, (pline) = address upon entry.
;findlineARC:     Must be copied to RAM. Locates next line in BASIC code.
;                  (cpage):DE=current line
;                  (cpage):DE=next line;(pline) reflects what was input
;getlineargs:     Loads up (rpart) and (ipart) with arguments.
;                  Call after argument enumeration.
;                  (rpart) is main argument, (ipart) is potential secondary.
;setupfindline:   Loads correct routine to $8000 for line reading.
;                  HL=address to start reading from (tells if in arc or ram)
;                  Routine is loaded to RAM for execution
;
;getline:         Retrieves the correct line given inputs.
;                  (rpart)=line#;(ipart)=#ofLines
;                  DE=address_of_line;BC:size_of_line_adjusted
;groupGetNext:    Get next entry in a group file.
;                  HL=next entry in group, or after chkfindsym/exdehl
;                  HL=next entry, (gsize)(gSOF)(gEOF)(Op2) updated of skipped entry
;lookupEOF:       Sets carry if (cpage):HL hits the end of the specified file
;                  HL=current pointer. (EOF) is matched against (cpage):HL
;                  carry flag set if pointers match (EOF hit). Preserves regs.
;debug0:          Outputs values of registers as a breakpoint. Press ON to continue.
;                  None.
;                  None.
;convHLtoDec:     Converts HL to decimal.
;                  HL=number. DE=address to output string
;                  Address updated with ASCII text of string.
;ConvOp1C:        Converts Op1 to a usable integer.
;                  Op1=valid float 0-65535
;                  DE=OP1 converted to int
;ConvFloatToText: Converts floating point number to text. Feature-packed! lol
;                  Op1=floating point number;DE=address to start writing #
;                  ...duh.
;ConvAToText:     Converts register A to text, 0 to 255.
;                  A=number;DE=address to store the string
;                  ...psh.
;AddDecPt:        Internal.
;                  ...It's a secret.
;                  ...No, really.
;getOp1Len:       Internal.
;                  ...It's another secret.
;                  ...For real.
;updatesizedel:   Updates the file's size bytes if bytes were deleted.
;                  (SOF)=StartOfFile (set by getprogram); BC=amount deleted
;                  File is updated.
;updatesizeins:   Updates the file's size bytes if bytes were insertted.
;                  (SOF)=StartOfFile (set by getprogram); BC=amount insertted.
;                  File is updated.
;findappvar:      Very specific. Finds the Celtic III appvar quick and unsafe
;                  Whee.
;                  Address to the file. Autounarchives if necessary
;GetDBEntry:      Obtains the PicArc database entry given number.
;                  (cpage):HL = start of database, A=database entry
;                  (cpage):HL = StrtO'Entry@size, DE=size C=compression meth
;PicInStream:     Picture file to database entry
;                  (cpage):HL = img file start, DE=size. A=!0 try compression
;                  BC=size of data field, location in savesscreen;
;PicOutStream:    Database entry to picture file
;                  input is the output of GetDBEntry
;                  savesscreen= restored image file
;PicAToOp1:       Outputs Pic File name in Op1 given register A.
;                  ...
;                  Op1=constructed filename for PicA.
;getpic:          Retrieves data about pic file.
;                  (rpart)=pic file. Jump ahead 3 bytes to set A to own.
;                  Just like the getprogram command. See that way above.
;loadlogicinst:   Loads A with a logic instruction for SMC'd RAM copies.
;                  A=logic.
;                  A=instruction. See ReadME.
;cacheallimgs:    Caches all image files to savesscreen for adr/page table.
;                  ...
;                  ...
;cacheimgs:       Caches images from first specified pic file to the next two
;                  (var4)=pic file to start
;                  ...
;tracksprite:     d00d. seriously.
;                  I am.
;                  Too sleepy.
;addbctohl1pg:    Adds BC to HL and increments page if necessary. Only 1 page
;                  .Duh.
;                  -.-
;HexStringToBin:  Converts a string to its binary representation as a "string"
;                 DE=Source, HL=Destination, BC=counter (in output bytes)
;                 ... just like any string copying routine.
;BinStringToHex:  Converts a string to its hexadecimal equivalent as a string.
;                 DE=Destination, HL=source, BC=counter (in input bytes)
;                 ... Like above. If you don't understand, you fail.
;Mul9:            Multiplies a number by 9
;                 HL is the original number.
;                 HL*9 -> HL
;Mul96:           Multiplies a number by 9
;                 HL is the original number
;                 HL*96 -> HL
;SearchVAT:       Same inputs as ChkFindSym
;                 Same outputs as ChkFindSym
;                 Written to avoid the romcall slowdown BS.
;SearchForPic:    A=Pic#
;                 Same outputs as ChkFindSym, FindSym, or rst 10h
;                 For SPEEDY lookups of image files
;CheckFileHeader: (cpage):HL = fileDataStart, DE=Teststring.
;                 No error is thrown if the file passes check
;                 All registers are preserved.
;matchVAT:        Traverses VAT and finds names that matches partial OP3
;                 OP3=Partial name. (OP3)=size, HL=VATPointer
;                 Carry if EOV else Op2=size+name of file, HL set to next


makestring:
#ifdef c3_errsys
	ld hl,8
#else
	ld hl,2
#endif
	push hl
		bcall(_CreateTStrng)
		push de
			bcall(_Op4toOp1)
;   ld de,ostrng
;   ld hl,Op1
;   ld bc,9
;   ldir
			pop de
		pop bc
	inc de
	inc de
	ret

copysourcestring_plain_zt:
	push hl
		ex de,hl
		call getsourcestring
		pop de
copysourcestring_plain_zt_raw:
	ld ix,0
	call getsourcestring_loop
	xor a
	ld (de),a					;there's the zero termination
	ret

getsourcestring_plain:
	call getsourcestring
	ld de,0
	push hl
		pop ix
getsourcestring_loop:
	ld a,b
	or c
	ret z
	push bc
		push hl
			push de
				push ix
					call My_Get_Tok_Strng
					pop ix
				pop de
			push ix
				pop hl
			ld a,h
			or l
			jr nz,getsourcestring_loop_length
getsourcestring_loop_copy:
			ld hl,Op3			;de already popped
			ldir
			pop hl
		jr getsourcestring_loop_finish
getsourcestring_loop_length:
			ld l,c
			ld h,0
			add hl,de
			pop de
		ex de,hl
getsourcestring_loop_finish:
		pop bc
	ld a,(hl)
	inc hl
	dec bc
	bcall(_IsA2ByteTok)
	jr nz,getsourcestring_loop
	inc hl
	dec bc
	jr getsourcestring_loop

My_Get_Tok_Strng:
	ld a,(hl)
	cp $81
	jr z,My_Get_Tok_Strng_D6
	bcall(_Get_Tok_Strng)
	ret
My_Get_Tok_Strng_D6:
	ld a,$D6
	ld (Op3),a
	ld a,2
	ld c,a
	ld b,0
	ret

getsourcestring:
	rst 20h
	ld a,(Op1)
	res 5,a
	cp 4
	jp nz,Err_NOTASTRING
	rst 10h
	jp c,Err_STRNOTFOUND
	ld a,b
	or a
	jp nz,Err_STRARCHIVED  ;if archived, generate error
	ex de,hl
getsize:
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	ld a,c
	or b
	ret

getprogramname:
	ld hl,sc1
	call getsourcestring  ;skip over the "LD HL,Str9" part
	jp z,Err_NULLVAR  ;Str0 is null. Cannot accept input.
	ex de,hl
	ld hl,-22  ;This number picked to prevent terrible buffer overflow
	add hl,bc  ;Can't be too long.
	jp c,Err_ENTRYTOOLONG
	ld hl,Op1
	ld a,(de)         ;DE=input string. first byte
	cp $15            ;rowSwap( token equates to AppVar type
	jr z,skip923      ;jump over in order to copy in the type byte as well
	bit gList,(iy+myflag)
	jr z,skip922
	cp $5D            ;official list token. L1 thru L6
	jr z,$+6          ;skip to handling list
	cp $EB            ;list token used to name user-defined lists
	jr nz,skip922     ;skip to comparing all else if this is not a list
	ld (hl),$01       ;type byte for rList
	inc hl
	ld a,$5D          ;list type
	ld (hl),a
	inc hl            ;increment pointer to fill in list name data
	inc de            ;increment pointer to get list name data
	dec bc            ;decrement size of data for the list.
	jr skip923
skip922:
	bit gGroup,(iy+myflag) ;
	jr z,$+7               ;
	ld a,(de)              ;
	cp $17                 ;*row( token equates to Group type
	jr z,skip923           ;
	ld (hl),$05  ;Prog obj
	inc hl
skip923:
	ex de,hl
	ld b,c                   ;
getprognamesubloop:       ;
	ld a,(hl)                ;
	inc hl                   ;
	cp $BB                   ;
	jr nz,getprognameminloop ;2bytetok skip if not found.
	ld a,(hl)                ;
	inc hl                   ;
	dec b                    ;
	cp $BB                   ;byte not repeated in series. Skipping $BB in series.
	jr c,$+3                 ;because of that, if less than that num, do not alter num
	dec a                    ;
	sub $B0-$61              ;decrease by appropriate number of bytes. Yeah.
getprognameminloop:       ;
	ld (de),a                ;
	inc de                   ;
	djnz getprognamesubloop  ;
	xor a                    ;would not reach this point if A!=0. See "LD A,B" earlier.
	ld (de),a                ;clear out next byte for null-terminated string
	bcall(_ChkFindSym)       ;check for program with name retrieved from Str0
	jr nc,$+30               ;skip if program info is found. Else, search for hidden
	ld hl,Op1+1              ;
	ld a,(hl)                ;
	add a,-$40               ;
	ld (hl),a                ;
	bcall(_ChkFindSym)       ;
	jr c,$+8                 ;
	set Hidden,(IY+myflag)   ;
	jr $+12                  ;
	push af                  ;
		ld a,(Op1+1)            ;
		add a,$40               ;set back to normal
		ld (Op1+1),a            ;
		pop af                   ; 
	ret

getprogram:  ;outputs and resets vectors for program data using Str0 stuffs.
	call getprogramname
	jp c,Err_PRGMNOTFOUND    ;kill program with this error if program is not found
	ex de,hl
	ld a,b
	ld (cpage),a
	or a
	jr z,$+15      ;2 skip if already unarchived
	ld de,9        ;3
	call addDEtoHL ;3
	call getHLinc  ;3
	ld e,a         ;1 ,D already set to zero
	call addDEtoHL ;3 = 15
getvarcoll:
	ld (SOF),hl   ;address at start of size field
	call getHLinc
	ld e,a
	call getHLinc
	ld d,a
	ld a,(cpage)
	; ld (SOF+2),a  ;page
	push af
		push hl
			call addDEtoHL  ;add in size and compensate for page stuffs if in Flash
			ld (EOF),hl   ;address
			ld a,(cpage)
			ld (EOF+2),a  ;page
			pop hl
		pop af
	ld (cpage),a
	ret

;================================
;================================
;THESE ROUTINES NEED MASSIVE OVERHAUL
getHL:
	ld a,(hl)
	bit 7,h
	ret nz
	push hl
		in a,(7)
		push af
			ld a,(cpage)
			out (7),a
			res 6,h
			set 7,h
			pop af
		ld l,(hl)
		out (7),a
		ld a,l
		pop hl
	ret

getHLinc:
	call getHL
IncHL:
	bit 7,h
	inc hl
	ret nz
	bit 7,h
	ret z
	push hl
	ld hl,cpage
	inc (hl)
	pop hl
	res 7,h
	set 6,h
	ret

addDEtoHL:   ;cares for >16384 possibility. Overglorified ADD HL,DE wrt/ pages
	bit 7,h
	jr nz,addDEtoHLskip
	push af
		push hl
			ld a,d
			rlca
			rlca
			and 00000011b  ;get two MSbits of H, and add to page with that
			ld hl,cpage
			add a,(hl)
			ld (hl),a
			ld l,d
			ld a,d
			and 00111111b
			ld d,a
			ld a,l
			pop hl
		add hl,de
		ld d,a
		pop af
	jr IncHL+4
addDEtoHLskip:
	add hl,de
	ret

findLineRAM:
	ld (pLine),de
	ld bc,0
findLineRAML:
	ld hl,(EOF)
	or a
	sbc hl,de
	jr nz,findLineRAMC
	bit HitEOF,(iy+myflag)
	jr z,$+6
	set ConEOF,(iy+myflag)
	set HitEOF,(iy+myflag)
	ret
findLineRAMC:
	ld a,(de)
	inc de
	inc bc
	bcall(_IsA2ByteTok)					;**no regs destroyed
	jr z,findLineRAMC_2Btok
	cp tEnter
	jr nz,findLineRAML
	ret
findLineRAMC_2Btok:
	inc de
	inc bc
	jr findLineRAML
findLineRAMEnd:
;==
findlineARC:   ;DE=addy (cpage)=page
	in a,(6)
	push af
		ld (pLine),de
		ld a,(cpage)
		out (6),a
		ld (pLine+2),a
		ld bc,0
		ld hl,(EOF)
FLARC03:
		ld a,l
		sub e
		jr nz,FLARC01					;jp nz,FLARC01-FindLineARC+$8000
		ld a,h
		sub d
		jr nz,FLARC01					;jp nz,FLARC01-FindLineARC+$8000
		in a,(6)
		push hl
			ld hl,EOF+2
			sub (hl)
			pop hl
		jr nz,FLARC01					;jp nz,FLARC01-FindLineARC+$8000
		ld hl,(flgadr)
		bit HitEOF,(iy+myflag)
		jr z,$+6
		set ConEOF,(iy+myflag)
		set HitEOF,(iy+myflag)
		pop af
	out (6),a
	ret
FLARC01:
		inc bc
		ld a,(de)
		inc e
		jr nz,FLARC02						;jp nz,FLARC02-FindLineARC+$8000
		inc d
		jp p,FLARC02-FindLineARC+$8000
		ld d,a
		in a,(6)
		inc a
		out (6),a
		ld a,d
		ld d,$40
FLARC02:
		bcall(_IsA2ByteTok)					;**no regs destroyed
		jr z,FLARC02_2Btok
		cp tEnter
		jr nz,FLARC03						;jp nz,FLARC03-FindLineARC+$8000
		in a,(6)
		ld (cpage),a
	pop af
	out (6),a
	ret
FLARC02_2Btok:
		inc bc
		ld a,(de)
		inc e
		jr nz,FLARC03						;jp nz,FLARC02-FindLineARC+$8000
		inc d
		jp p,FLARC03-FindLineARC+$8000
		ld d,a
		in a,(6)
		inc a
		out (6),a
		ld a,d
		ld d,$40
		jr FLARC03
FindLineARCEnd:
 
 
getlineargs:
	ld hl,(var1)
	ld de,(var2)
	ld (rpart),hl
	ld (ipart),de
	ret

setupfindline: ;HL=address of file
	bit 7,h
	ld de,$8000
	push hl
		jr z,setupfindline_Arc
		ld hl,findLineRAM
		ld bc,findlineRAMEnd-findLineRAM
		ldir
		pop hl
	ret
setupfindline_Arc:
		ld hl,findLineARC
		ld bc,findlineARCEnd-findLineARC
		ldir
		pop hl
	ret

getLine:
	call getprogram     ;HL=addy
	call setupfindline
	ld de,(var1)
	ld a,d
	or e
	scf
	ret z               ;set carry then exit if ANS==0
	ex de,hl
getLineLoop:
	push hl
		call $8000
		pop hl
	bit ConEOF,(iy+myflag)
	scf
	ret nz
	dec hl
	ld a,h
	or l
	jr nz,getLineLoop

	ld hl,(pline)
	ld (temp1),hl
	ld a,(pline+2)
	ld (temp2),a
	ld hl,(var2)
	inc hl    ;overcome initial zero if imag part is not filled
GetLineLoop2:
	bit ConEOF,(iy+myflag)
	jr nz,GetLineEndFix
	dec hl
	ld a,l
	or h
	jr z,GetLineEnd
	push hl
		push bc
			call $8000
			pop hl
		add hl,bc
		ld c,l
		ld b,h
		pop hl
	jr GetLineLoop2
GetLineEndFix:
	res ConEOF,(iy+myflag)  ;do not invoke EOF errors if just reading past
GetLineEnd:
	ld hl,(temp1)
	ld (pline),hl
	ld a,(temp2)
	ld (cpage),a
	bit HitEOF,(iy+myflag)
	jr nz,$+3
	dec bc    ;stripping trailing tEnter if not EOF

	ret

GroupGetNext:  ;this is a call to get next variable in a group
	push hl
		call getHL  ;get type byte
		and $1F     ;clear off masks above
		ld l,a      ;loading to DE
		xor a       ;clear off A to clear off H
		ld h,a       ;clear off MSB of HL
		ld de,OBRTIN  ;load table of values
		add hl,de    ;get offset in table
		ld l,(hl)   ;get byte indicating relative address
		ld h,a      ;clear off MSB of HL
		ld de,OBRT  ;load ending table
		add hl,de   ;get address of the routine
		ex (sp),hl   ;exchange the pushed HL with this address HL
		ret           ;pop the address HL off the stack
GetVATDatSt:
	call GrpSymTblColl
	call getHLInc
GetVATDatMerge:
	ld b,a
	ld (nsize),a
	ld de,Op2+1
	call getHLInc
	ld (de),a
	inc de
	djnz $-5
	xor a
	ld (de),a
	ld (gSOFp),hl
	ld a,(cpage)
	ld (gSOFp+2),a
	call getHLInc
	ld e,a
	call getHLInc
	ld d,a
	ld (gSOF),hl
	ld a,(cpage)
	ld (gSOF+2),a
	ex de,hl      ;DE=address, HL=size
	ld (gSIZE),hl
	ret
GetSymTbl:
	call GrpSymTblColl
	ld a,3
	jr GetVATDatMerge
GrpSymTblColl:
	call getHL
	ld (OP2),a
	ld de,6
	call addDEtoHL
	ret
 
;one-byte type call table
;table start= "OBRT"
;See link guide for types
;and the correct handler
OBRTIN:
	.db HandleReal-OBRT        ;00h=real
	.db HandleList-OBRT        ;01h=list
	.db HandleMatrix-OBRT      ;02h=matrix
	.db HandleString-OBRT      ;03h=equ
	.db HandleString-OBRT      ;04g=string
	.db HandleProgAppVar-OBRT  ;05h=prog
	.db HandleProgAppVar-OBRT  ;06h=protprog
	.db HandleString-OBRT      ;07h=pic
	.db HandleString-OBRT      ;08h=GDB
	.db HandleString-OBRT      ;09h=unknown
	.db HandleString-OBRT      ;0Ah=unknown
	.db HandleString-OBRT      ;0Bh=window settings
	.db HandleCplx-OBRT        ;0Ch=Complex number
	.db HandleCplxList-OBRT    ;0Dh=Complex list
	.db HandleString-OBRT      ;0Eh=unknown
	.db HandleString-OBRT      ;0Fh=window settings
	.db HandleString-OBRT      ;10h=saved window settings
	.db HandleString-OBRT      ;11h=table setup
	.db HandleString-OBRT      ;12h=unknown
	.db HandleString-OBRT      ;13h=backup
	.db HandleString-OBRT      ;14h=(used to delete flashapps)
	.db HandleProgAppVar-OBRT  ;15h=AppVAR
	.db HandleError-OBRT       ;16h=Error condition.
	.db HandleError-OBRT       ;17h=Error condition.
	.db HandleError-OBRT       ;18h=Error condition.
	.db HandleError-OBRT       ;19h=Error condition.
	.db HandleError-OBRT       ;1Ah=Error condition.
	.db HandleError-OBRT       ;1Bh=Error condition.
	.db HandleError-OBRT       ;1Ch=Error condition.
	.db HandleError-OBRT       ;1Dh=Error condition.
	.db HandleError-OBRT       ;1Eh=Error condition.
	.db HandleError-OBRT       ;1Fh=Error condition.
OBRT:
;fill in with rest of values and write routine to use relative values
HandleError:
	bcall(_ErrDomain)         ;serious error needs to be flagged down. NOW.
HandleString:
	call getSymTbl
	jr GroupMultSkip
HandleCplx:
	ld c,16
	jr $+4
HandleReal:
	ld c,7
	call getSymTbl
	ld h,b     ;B=0, set H to 0
	ld l,c
	jr GroupMultSkip
HandleList:
	call getVATDatSt
	jr groupmult9
HandleCplxList:
	call getVATDatSt
	; jr groupmult18
GroupMult18:
	add hl,hl
GroupMult9:
	ld c,l
	ld b,h
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,bc
GroupMultSkip:
	ex de,hl
	call addDEtoHL
	or a
	scf
	ret
HandleMatrix:
	call getSymTbl
	ld c,h
	ld h,b
	ld a,l
	add hl,bc
	dec a
	jr nz,$-2
	jr groupmult9
HandleProgAppVar:
	call getVATDatSt
	ex de,hl
	call addDEtoHL
	ld (gEOF),hl
	ld a,(cpage)
	ld (gEOF+2),a
	or a
	ret

lookupEOF:  ;hl=address carry set if EOF hit
	push de
		ld de,(EOF)
		or a
		sbc hl,de
		add hl,de
		scf
		ccf
		pop de
	ret nz
	push hl
		ld hl,(EOF+2)
		ld h,a         ;preserves A
		ld a,(cpage)
		cp l
		ld a,h
		pop hl
	ccf
	ret

#ifdef celtic3debug
debug0:
	DI
	push hl \ push bc \ push de \ push af
	push ix \ push hl \ push de \ push bc \ push af
	ld hl,0 \ ld (currow),hl
	call fastcopy
	pop hl \ call convertHtoT2b \ ld hl,$0020 \ ld (Op6+4),hl \ ld hl,Op6 \ bcall(_PutS) ;AF
	pop hl \ call convertHtoT2b \ ld hl,$0020 \ ld (Op6+4),hl \ ld hl,Op6 \ bcall(_PutS) ;BC
	pop hl \ call convertHtoT2b \ ld hl,$0020 \ ld (Op6+4),hl \ ld hl,Op6 \ bcall(_PutS) ;DE
	bcall(_NewLine)
	pop hl \ call convertHtoT2b \ ld hl,$0020 \ ld (Op6+4),hl \ ld hl,Op6 \ bcall(_PutS) ;HL
	pop hl \ call convertHtoT2b \ ld hl,$0020 \ ld (Op6+4),hl \ ld hl,Op6 \ bcall(_PutS) ;IX
	ld hl,0 \ add hl,sp \ call convertHtoT2b \ ld de,$0020 \ ld (Op6+4),de \ ld hl,Op6 \ bcall(_PutS) ;SP
	bcall(_NewLine)
	in a,(4)    ;waits for ON key to be pressed.
	bit 3,a
	jr nz,$-4   ;keeps program from going anywhere
	in a,(4)    ;waits for ON key to be depressed.
	bit 3,a
	jr z,$-4    ;keeps program from going anywhere
	pop af \ pop de \ pop bc \ pop hl
	ret
#endif		;celtic3debug

convHLtoDec:
	ld bc,-10000
	call convHLtoDec1
	ld bc,-1000
	call convHLtoDec1
	ld bc,-100
	call convHLtoDec1
	ld c,-10
	call convHLtoDec1
	ld c,-1
convHLtoDec1:
	ld a,'0'-1
convHLtoDec2:
	inc a
	add hl,bc
	jr c,convHLtoDec2
	sbc hl,bc				;nc automatically set
	ld (de),a
	inc de
	ret
 
ConvOp1C:
;tstrn = buffer area
	ld hl,Op1
	di
	ld de,0
	res isneg,(iy+nflags)
	ld a,(hl)
	bit 7,a
	jr z,$+6
	set isneg,(iy+nflags)
	res 7,a
	or a
	ret nz   ;quit if this is not a real-type variable
	inc hl
	ld a,-$7F
	add a,(hl)
	ret nc
	inc hl
	cp 6
	ret nc
	ld (tstrn),sp
	ex de,hl
	ld b,a
	cpl
	add a,6
	add a,a
	ld l,a
	ld sp,ConvOp1CT
	add hl,sp
	ld sp,hl
	ld hl,0
	ld a,b
ConvOp1CL:
			ex af,af'
			pop bc
		ld a,(de)
		and $F0
		rrca
		rrca
		rrca
		rrca
		sub 1
		jr c,$+5
		add hl,bc
		jr $-5
		ex af,af'
		dec a
		jr z,ConvOp1CE
		ex af,af'
		pop bc
	ld a,(de)
	and $0F
	sub 1
	jr c,$+5
	add hl,bc
	jr $-5
	inc de
	ex af,af'
	dec a
	jr nz,ConvOp1CL
ConvOp1CE:
	bit isneg,(iy+nflags)
	jr z,$+9
	ld a,h \ cpl \ ld h,a  ;retrieves "negative" number equivalent for input
	ld a,l \ cpl \ ld l,a
	inc hl
	ex de,hl
	ld sp,(tstrn)
	ret

ConvOp1CT:
.dw 10000
.dw 1000
.dw 100
.dw 10
.dw 1

ConvFloatToText:  ;input: DE=location to store string, Op1=numberToConvert
	res nexpon,(iy+nflags)
	ld a,(Op1+1)
	cp $8A
	jr nc,CFTTExpo
	cp $80
	jr c,CFTTExpo
	jr CFTTExpoS
CFTTExpo:
	set nexpon,(iy+nflags)
CFTTExpoS: 
	call getOp1Len
	ld a,(Op1+1)
	sub $7F
	bit nexpon,(iy+nflags)
	jr z,$+4
	ld a,1    ;ensure there's only one number before the decimal point
	ld c,a
	ld a,(Op1)
	bit 7,a
	ld a,b
	jr z,$+8
	inc a
	ex de,hl
	ld (hl),tChs  ;negative sign
	inc hl
	ex de,hl
	ld (strlen),a
	ld hl,Op1+2
CFTTLoop:
	ld a,(hl)
	rlca
	rlca
	rlca
	rlca
	and $0F
	add a,$30
	ld (de),a
	inc de
	call addDecPt
	jr c,GFTTLE
	ld a,(hl)
	inc hl
	and $0F
	add a,$30
	ld (de),a
	inc de
	call addDecPt
	jr nc,CFTTLoop
GFTTLE:
	bit nexpon,(iy+nflags)
	jr z,GFTTSK
	ld a,tEE
	ld (de),a
	inc de
	ld a,(Op1+1)
	sub $80
	jr nc,$+4
	neg
	push de
		call ConvAtoText  ;DE=start of source
		pop hl
	ex de,hl
	ld a,(Op1+1)
	ld c,b
	inc b
	bit 7,a
	jr nz,$+7
	inc b
	ld a,tChs
	ld (de),a
	inc de
	; ld hl,Op6
	ldi
	ld a,c
	or c
	jr nz,$-4
	ld hl,strlen
	ld a,b
	add a,(hl)
	ld (hl),a
GFTTSK: 
 ld a,(strlen)
 ld c,a
 ld b,0
 ret
 
ConvAtoText:
	call $+18
	ld hl,Op6
	ld a,'0'
	inc hl
	cp (hl)
	jr z,$-2
	ex de,hl
	or a
	sbc hl,de
	ld b,l
	ret
	ld l,a
	ld h,0
	ld de,Op6+1
	ld bc,-100
	call $+10
	ld c,-10
	call $+5
	ld c,-1
	ld a,'0'-1
	inc a
	add hl,bc
	jr c,$-2
	sbc hl,bc
	ld (de),a
	inc de
	ret

addDecPt:
 dec b   ;end of the number, zeroes stripped
 scf
 ret z
 or a
 dec c  ;number of numbers to display before decimal point
 ret nz
 push hl
  ld a,tDecPt
  ld (de),a
  inc de
  ld hl,strlen
  inc (hl)
 pop hl
 ret
 
getOp1Len:
 ld b,10
 ld hl,Op1+6
 ld a,(hl)
 and $0F
 jr nz,$+12
 dec b
 ld a,(hl)
 and $F0
 jr nz,$+6
 dec hl
 dec b
 jr nz,getOp1Len+5
 ld a,(Op1+1)
 sub $80
 ret c   ;keep B for sci input. Trailing zeros no longer significant
 cp b
 ret c
 cp 11
 jr c,$+4
 ld a,9
 inc a
 ld b,a
 ret
 
updatesizedel:   ;update size for delete. Negates BC so becomes HL+(-BC)
	ld a,c
	cpl
	ld c,a
	ld a,b
	cpl
	ld b,a
	inc bc
updatesizeins:   ;size update for insert. (SOF) = start of file, BC= positive
	ld hl,(SOF)
	ld a,(hl)
	add a,c
	ld (hl),a
	inc hl
	ld a,(hl)
	adc a,b
	ld (hl),a
	inc hl
	ret

 
findappvar:     ;routine used to quickly find the Celtic III appvar (illegally named)
	ld hl,(progptr)
	ld de,(ptemp)
	ld bc,-6
findappvarloop:
	bit 4,(hl)     ;if not zero, is either appvar or group. Unsafe, but fast.
	add hl,bc      ;does not affect zero flag.
	ld c,(hl)      ;gets size info in C
	dec hl         ;move to next byte. In name.
	jr z,findappvarEL ;if bit 4,(hl). is 1, is a group or appvar.
	cp 4			;"DCS7"
	jr nz,findappvarEL
	ld a,(hl)          ;...
	dec hl
	dec c
	cp 'D'
	jr nz,findappvarEL
	ld a,(hl)
	cp 'C'
	jr nz,findappvarEL
	dec hl
	dec c
	ld a,(hl)
	cp 'S'
	jr nz,findappvarEL
	dec hl
	dec c
	ld a,(hl)
	cp '7'
	jr z,findappvargood
	findappvarEL:
	ld a,c
	neg
	ld c,a
	add hl,bc
	ld c,-6
	or a
	sbc hl,de
	add hl,de
	jp nz,findappvarloop
	scf
	ret
findappvargood:
	inc hl
	inc hl
	inc hl
	ld a,(hl)
	or a
	jr z,$+11
	ld hl,AppVarName3
	rst 20h
	bcall(_Arc_Unarc)
	jr findappvar
	inc hl
	ld a,(hl)
	inc hl
	ld l,(hl)					;ldhlind
	ld h,a
	ld de,2+9					;size bytes, HomeSave's skipped
	add hl,de
	ret

GetDBEntry: ;(cpage):HL = start of database, A=database entry
;Output (cpage):HL = start of entry (at size field), DE=size information C=compression method
 ld de,PADBStart       ;label defined in _PFN.z80
 call CheckFileHeader
 ld de,6
 call addDEtoHL
 ld b,a         ;entry # to B
 call getHLInc  ;get # of entries
 ld c,a         ;put # of entries into C
 ld a,b         ;entry # into A
 cp c           ;entry# - # of entries
 jp nc,Err_DBENOTFOUND
 ld a,b
 or a
 jr z,GetDBEntry2
 call getHLInc
 ld e,a
 call getHLInc
 and 3
 ld d,a
 call addDEtoHL
 djnz $-13
GetDBEntry2:
 call getHLInc
 ld e,a
 call getHL
 ld c,a         ;store base byte at C
 and %00000011  ;mask this byte to get size only
 ld d,a         ;and store this away
 ld a,c         ;retrieve base byte and do conversions.
 rrca
 rrca
; rrca  to predouble the bytes needed for outstream. Leave this out.
 and %00111110
 ld c,a
 dec hl   ;move pointer back to start @ size bytes
 ld a,h
 cp $3f
 ret nz
 ld h,$7F
 ld a,(cpage)
 dec a
 ld (cpage),a
 ret

PicInStream:
;picture file to database entry
;input: (cpage):HL = image file start, DE=size. A=!0 to try compression
;output: BC=size of data field, location in savesscreen
 push af
  call saveimageloc
 pop af
 or a
 jr nz,PicInStreamA
PICC00:
 ld bc,$02F4
 ld de,savesscreen
 push bc
  call gethlinc
  ld (de),a
  inc de
  dec bc
  ld a,c
  or b
  jp nz,$-8
 pop bc
 xor a
 ld (cmask),a
 ret
PicInStreamA:
 ld bc,$02F4 \ push bc    ;first size push to the stack. Control value
 call loadimageloc
 call PICC01 \ push bc    ;second size push to the stack
 call loadimageloc
 call PICC02 \ push bc    ;third size push to the stack
 ld b,(PicInCEnd-PicInCompress)/2
 ld c,1
 ld de,$02F4
PicInStreamB:  ;locating minimum number on table
 pop hl
 or a
 sbc hl,de
 jr nc,$+5     ;jump over location save if this number greater than smallest #
 add hl,de
 ex de,hl      ;saving new size information as DE
 ld c,b        ;looping number for this entry
 djnz PicInStreamB
 dec c                  ;to match up with later DJNZ decrement of B. Works.
 push bc                ;compression method stored first in stack
  ld hl,PicInStreamC
  push hl                 ;return location stored second in stack
   ld hl,PicInCompress  ;start of the compression routine table minus init
   add hl,bc              ;add add the offset pointing to entry in the table
   add hl,bc
   ld a,(hl)              ;retrieve the address to that point
   inc hl
   ld h,(hl)
   ld l,a
   push hl                ;and push that address to the table
    call loadimageloc
    ret                   ;pop off table address
PicInStreamC:
 pop de                   ;retrieve compression method used
 ld a,e
 add a,a
 add a,a
 add a,a
 ld (cmask),a
 ret

PicInCompress:
.dw PICC00     ;no compression
.dw PICC01     ;rle
.dw PICC02     ;no compression. Used as a debug check for high bytes
PicInCEnd:

PICC01:
 call unusedbytecheck
 jr nc,$+6
 ld bc,$FFFF
 ret
 ld ix,savesscreen+1
 ld (ix-1),a    ;write in the unused byte
 ld d,a          ;load this into status byte
 call gethl      ;prime the routine by reading of the first byte
 inc a           ;ensure that E will never test true in the first part
 ld c,a          ;by loading a byte that will never match the "previous"
;D=statusbyte
;B=internalcounter
;C=previousbyte
PICC01L:
 ld b,0
 call PICC01S    ;B=1
 jr c,PICC01L
 call PICC01S    ;B=2
 jr c,PICC01L
 call PICC01S    ;B=3
 jr c,PICC01L
PICC01M:
 call getHL
 xor c
 jr nz,PICC01B   ;If next byte does not match, force write w/o increment
 call incHL
 call lookupEOF
 inc b
 jr c,PICC01B    ;if EOF is hit, force writeout.
 ld a,b
 or a
 jr nz,PICC01M   ;if not zero (maxed out), keep looping.
PICC01B:
 ld (ix-3),d     ;Write in the identifier byte.
 ld (ix-2),b     ;Write in the number of type bytes.
 ld (ix-1),c     ;Write in the type byte
 jr nc,PICC01L   ;If no carry is set, keep looping.
PICC01C:
 ld hl,savesscreen+$300
 ld bc,-savesscreen
 add ix,bc
 push ix
 pop bc
 ret
PICC01S:
 inc b
 call gethlinc   ;Get first byte in series.
 ld (ix+0),a     ;write out byte retrieved immediately
 inc ix          ;and increment ix pointer to next spot.
 call lookupEOF  ;find if EOF has been reached.
 jr nc,$+5       ;jump over hacked exit if EOF is not reached.
 pop af          ;kill the stack entry used for the return
 jr PICC01C      ;and jump to image file finalizing
 cp c            ;compare previous byte.
 ld c,a          ;load new previous byte. Test is now done.
 scf             ;
 ret nz          ;kill routine with carry if A!=C.
 or a            ;otherwise, end routine with matching #'s. No carry
 ret
 
PICC02:
 jp PICC00
 ret
 
saveimageloc:
 ld (temp1),hl
 ld a,(cpage)
 ld (temp2),a
 ld (temp3),de
 ret
loadimageloc:
 ld hl,(temp1)
 ld a,(temp2)
 ld (cpage),a
 ld de,(temp3)
 ret
 
unusedbytecheck:
	call saveimageloc
	ld d,0
unusedbytecheckL:
	ld bc,$02F4
	call getHLInc
	cp d
	jp z,unusedbyteF
	dec bc
	ld a,b
	or c
	jr nz,unusedbytecheckL+3        ;loop for total 768-12 times to search for byte.
unusedbytefound:
	push de
		call loadimageloc
		pop af    ;loads to A the value needed
	ret
unusedbyteF:
	push de
		call loadimageloc
		pop de
	dec d
	jr nz,unusedbytecheckL
	scf
	ret

PicOutStream:
;database entry to picture file
;input: output of GetDBEntry
;output: savesscreen= restored image file
	call IncHL
	call IncHL
	ld a,c
	cp PicInExpandEnd-PicInExpand
	jp nc,Err_DBINVALID
	push hl
		ld l,a
		ld h,0
		ld bc,PicInExpand
		add hl,bc
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ex (sp),hl
		ret 


PicInExpand:
.dw PICD00     ;No compression
.dw PICD01     ;rle
.dw PICD02     ;vertical rle (not implemented)
PicInExpandEnd:
PICD00:
 ld bc,savesscreen
 call getHLinc
 ld (bc),a
 inc bc
 dec de
 ld a,d
 or e
 jr nz,$-8
 ret

PICD01:
;RLE-compressed image
;DE=size counter
;IX=savesscreen output
;C=status byte
;B=internal counter
 ld ix,savesscreen
 call getHLInc
 ld c,a
 dec de          ;first decrement for status byte
PICD01L:
 call getHLInc   ;get possible status byte. Else...
 cp c
 jr nz,PICD01C
 call getHLInc  ;get counter byte
 ld b,a         ;
 call getHLInc  ;get type byte
PICD01A:
 ld (IX+0),a
 inc ix
 djnz PICD01A
 dec de
 dec de     ;decremented twice plus one more for the RLE compression set.
 jr PICD01F
PICD01C:
 ld (IX+0),a
 inc ix
PICD01F:
 dec de
 ld a,d
 or e
 jr nz,PICD01L
 ld hl,savesscreen+$300
 ret
 
PICD02:
 ret
 
PicAToOp1:
 ld hl,$6007  ;tvarpict and picobj all in one
 ld (Op1+0),hl
 ld l,a       ;pic# then zero for naming conventions
 ld h,0
 ld (Op1+2),hl
 ret
getpic:
 ld a,(var1)
 or a
 jr nz,$+4
 ld a,10
 dec a
 call SearchForPic
 jp c,Err_PICNOTFOUND
 ex de,hl
 ld a,b
 or a
 ld (cpage),a
 jr z,$+8  ;jump if not in archive to skip pointer movement.
 ld de,12
 call adddetohl
 jp getvarcoll
;getpicbuf:
; ld hl,plotsscreen+($0300-$000C)
; ld (EOF),hl
; ld hl,plotsscreen
; ld (SOF),hl
; ld de,$0300-$000C
; ret

SearchForPic:
 di
 ex af,af'       ;save pic # to search for in A'
 ld de,(progptr) ;load end of symtable to register
 ld bc,-9        ;Skip over fixed-width symtable entries by this
 ld hl,symtable  ;starting address
 ld a,7          ;search for pic object type. Weird that A is never touched :S
SearchForPicL:
 cp (hl)
 jp nz,SearchForPicS
 push hl
  ld bc,-7
  add hl,bc
  ex af,af'
  cp (hl)
  jp z,SearchForPicD
  ex af,af'
  ld bc,-9
 pop hl
SearchForPicS:
 add hl,bc
 or a
 sbc hl,de
 add hl,de
 jp nz,SearchForPicL
 scf
 ret
SearchForPicD:
  inc hl
  inc hl
  ld b,(hl)
  inc hl
  ld d,(hl)
  inc hl
  ld e,(hl)
 pop hl
 ret    ;previous cp (hl) resets carry flag coz of match.

loadlogicinst:
 ld b,a
 or a
 ld a,$7E  ;ld a,(hl)
 ret z     ;
 ld a,$A6
 dec b
 ret z
 add a,$10
 dec b
 ret z
 sub $08
 ret 
cacheallimgs:
 di
 ld (temp1),sp
 ld sp,savesscreen+$0300
 ld hl,myblankscreen
 in a,(6)
 ld d,a
 ld e,0
 ld b,e
 push de \ inc sp \ push hl
 djnz $-3
 ld sp,(temp1)
 ld hl,symTable
cacheallimgsL:
 ld a,(hl)
 cp $07
 jr nz,cacheallimgsLE
 push hl
  dec hl \ dec hl \ dec hl
  ld e,(hl) \ dec hl
  ld d,(hl) \ dec hl
  ld a,(hl) \ dec hl \ dec hl
  ld c,(hl)
  inc de
  inc de
  ex de,hl
  or a
  jr z,$+17
  ld de,12
  bit 7,h
  add hl,de
  jr nz,$+9
  bit 7,h
  jr z,$+5
  ld h,$40
  inc a
  ex de,hl
  ld hl,savesscreen
  ld b,0
  add hl,bc
  add hl,bc
  add hl,bc
  ld (hl),e \ inc hl
  ld (hl),d \ inc hl
  ld (hl),a
 pop hl
cacheallimgsLE:
 ld de,-9
 add hl,de
 ld de,(progptr)
 or a
 sbc hl,de
 add hl,de
 jr nz,cacheallimgsL
 ret
;has dependency on FPS pops
cacheimgs:
 ld hl,$6007
 ld (Op1),hl
 ld hl,(var1)
 ld h,0
 ld (Op1+2),hl
 ld a,h
 ld (Op1+4),a
 rst 10h
 jp c,Err_PICNOTFOUND
 ex de,hl
 inc hl
 inc hl
 ld (temp1),hl
 ld a,b
 ld (temp2),a
 ld hl,Op1+2
 inc (hl)
 rst 10h
 ret c
 ex de,hl
 inc hl
 inc hl
 ld (temp3),hl
 ld a,b
 ld (temp4),a
 ld hl,Op1+2
 inc (hl)
 rst 10h
 ret c
 ex de,hl
 inc hl
 inc hl
 ld (temp5),hl
 ld a,b
 ld (temp6),a
 ret
 
TrackSprite:       ;
 cp 96             ;check to see if byte is beyond 96
 ld b,a            ;load that to B for count
 jr nc,tsttmp2     ;CUR-TEST. If no carry, number is higher than current pic.
 ld hl,(temp1)     ;
 ld a,(temp2)      ;load address of current pic now.
 ld (cpage),a      ;
 jr trackspritec   ;
tsttmp2:           ;
 sub 96            ;perform subtraction and
 ld b,a            ;
 cp 96             ;test for next set
 jr nc,tsttmp3     ;if no carry, then load automatically from third pic.
 ld hl,(temp3)     ;
 ld a,(temp4)      ;
 ld (cpage),a      ;
 jr trackspritec   ;
tsttmp3:           ;
 sub 96            ;
 ld b,a            ;
 ld hl,(temp5)     ;
 ld a,(temp6)      ;
 ld (cpage),a      ;
trackspritec:      ;
 ld a,b            ;save tile number
 ld bc,96          ;96 bytes to the next sprite down
 sub 12            ;subtract 12 from A for next sprite down
 jr c,$+7          ;If A dips below 12, then we'll add remainder for correct offset
 call addbctohl1pg ;otherwise, add 96 to HL to get to the next row down.
 jr $-7            ;loop back until carry is hit.
 add a,12          ;restore A back to nonnegative below 12 number.
 ld c,a            ;load this number
 ld b,0            ;to BC and then
 call addbctohl1pg ;add it to HL
 ret               ;return.
                   ;
addbctohl1pg:      ;
 bit 7,h           ;
 add hl,bc         ;
 ret nz            ;
 bit 7,h           ;
 ret z             ;
 push hl           ;
  ld hl,cpage      ;
  inc (hl)         ;
 pop hl            ;
 ret               ;

;hl=start of data, bc=number of bytes of data
#define lcdreset lcdres-rlelcd+$8000
#define inchlpaged hlp-rlelcd+$8000
rlelcd:
 di
 ld a,7
 out ($10),a
 ld b,6
 ld d,$7f
 call lcdreset
lcdloop:
 ld a,(hl)
 cp $00                ;SMC the identifier byte when copied to RAM
 jr nz,regularbyte
 call inchlpaged
 push bc
 ld b,(hl)
 dec b
 call inchlpaged
 ld a,(hl)
rleloop:
 out ($11),a
 dec e
 push af
 call z,lcdreset
 pop af
 djnz rleloop
 pop bc
regularbyte:
 out ($11),a
 call inchlpaged
 dec e
 call z,lcdreset
 dec bc
 inc b
 dec b
 jr nz,lcdloop
 inc c
 dec c
 jr nz,lcdloop
 ld a,5
 out ($10),a
 ret
lcdres:
 inc d
 ld a,d
 out ($10),a
 ex (sp),hl
 ex (sp),hl
 inc hl
 dec hl
 ld e,12
 ld a,$20
 out ($10),a
 ret
hlp:
 bit 7,h
 inc hl
 ret nz
 bit 7,h
 ret z
 ld h,a
 in a,(6)
 inc a
 out (6),a
 ld a,h
 ld h,$40
 ret

HexStringToBin:
 call convertsub     ;get hex digit and increment DE
 rlca                ;we got MSN from source. shift to accomodate this
 rlca                ;but this by no means prevents erroneous input
 rlca                ;by using a character outside the range of 0-F
 rlca                ;...
 and $F0
 ld (hl),a           ;save this to destination.
 call convertsub     ;get next hex digit and increment DE
 and $0F
 or (hl)             ;%11110000 | %00001111 = %11111111. Combines results to
 ld (hl),a           ;form the full byte. Store this back.
 inc hl              ;move to next byte in destination
 dec bc              ;decrement the counter
 ld a,b              ;check to see
 or c                ;if it is now zero.
 jr nz,$-21
 ret

BinStringToHex:
 call getHLinc		 ;(hl) -> A
 push hl             ;preserve address
  call convertHtoT1b ;call conversion routine to convert A to hex text
  ld hl,Op6          ;which happens to be in Op6
  inc bc             ;increment to counteract the two decrements by LDI
  ldi                ;copy byte to string (DE)
  ldi                 ;copy another byte to complete writing one "byte"
 pop hl              ;restore address.
 ;inc hl
 jp pe,BinStringToHex	  ;If last LDI hits zero, then parity is set.
 ret

Mul9:
 push de
  ld e,l
  ld d,h
  add hl,hl
  add hl,hl
  add hl,hl
  add hl,de
 pop de
 ret
Mul96:
 push de
  ld e,l
  ld d,h
  add hl,de ;x2
  add hl,de ;x3
  add hl,hl ;x6
  add hl,hl ;x12
  add hl,hl ;x24
  add hl,hl ;x48
  add hl,hl ;x96
 pop de
 ret

SearchVAT:
 ld hl,(Op1)
 ld h,$00
 ld de,SearchVTbl
 add hl,de
 ld a,(hl)
 or a
 jr nz,SearchVP
SearchVS:
 ld hl,(progptr)
 ld de,-6
 add hl,de
 ex de,hl
 ld hl,symtable-6
 ld bc,-9
SearchVSL:
 push hl
  ld a,(Op1+1)
  cp (hl)
  dec hl
  jp nz,SearchVSN
  ld a,(Op1+2)
  cp (hl)
  dec hl
  jp nz,SearchVSN
  ld a,(Op1+3)
  cp (hl)
  dec hl
  jp nz,SearchVSN
  ld bc,4
  add hl,bc
  ld b,(hl)
  inc hl
  ld d,(hl)
  inc hl
  ld e,(hl)
  jr SearchVSE
SearchVSN:
 pop hl
 add hl,bc
 or a
 sbc hl,de
 add hl,de
 jp nz,SearchVSL
 scf
 ret
SearchVSE:
 pop hl
 ld a,(Op1)
 or a
 ret
SearchVP:
 ld hl,Op1+1
 xor a
 ld c,a
 cp (hl)
 jr z,$+8
 inc hl
 inc c
 bit 3,c
 jr z,$-7
 ld hl,(progptr)
 ld de,-6
SearchVPL:
 ld a,(Op1)
 cp (hl)
 jr nz,SearchVPS
 push hl
  ld de,Op1+1
  ld b,c
  ld a,(de)
  cp (hl)
  jr nz,SearchVPSP
  inc de
  inc hl
  djnz $-6
 pop hl
 push hl
  dec hl
  dec hl
  dec hl
  ld e,(hl)
  dec hl
  ld d,(hl)
  dec hl
  ld b,(hl)
 pop hl
 or a
 ret
SearchVPSP:
  ld de,-6
 pop hl
SearchVPS:
 add hl,de
 push de
  ld a,(hl)
  neg
  ld e,a
  add hl,de
  ld de,(ptemp)
  or a
  sbc hl,de
  add hl,de
 pop de
 jr nz,SearchVPL
 scf
 ret

SearchVTbl:
	.db 0  ;00 real
	.db 1  ;01 list
	.db 0  ;02 matrix
	.db 0  ;03 equation
	.db 0  ;04 string
	.db 1  ;05 program
	.db 1  ;06 protected program
	.db 0  ;07 picture
	.db 0  ;08 graph database
	.db 0  ;09 unused
	.db 0  ;0A unused
	.db 0  ;0B new equation
	.db 0  ;0C complex
	.db 1  ;0D complex list
	.db 0  ;0E unused
	.db 0  ;0F unused
	.db 0  ;10 unused
	.db 0  ;11 unused
	.db 0  ;12 unused
	.db 0  ;13 unused
	.db 0  ;14 application. Passthrough.
	.db 1  ;15 appvar
	.db 1  ;16 temporary program
	.db 1  ;17 group
;All others will rely on the following stream of zeros as passthrough
myblankscreen:
.fill 768,0


Op1Set65536:
	.db $00,$84,$65,$53,$60,$00,$00,$00,$00,$00,$00

CheckFileHeader:
 push bc
  push de
   push hl
    push af
     ld a,(cpage)
     push af
      ld b,6
      ld a,(de)
      inc de
      ld c,a
      call getHLInc
      cp c
      jp nz,Err_INVALIDFILE
      djnz $-10
     pop af
     ld (cpage),a
    pop af
   pop hl
  pop de
 pop bc
 ret

matchVAT:
 ld de,(ptemp)
 push hl
  or a
  sbc hl,de
 pop hl
 scf
 ret z        ;end of VAT encountered if set to here. Set carry then exit.
 ld a,(hl)
 ld de,-6
 add hl,de
 ld b,(hl)
 dec hl                 ;hl set to read off name from VAT
 bit gList,(iy+myflag)  ;stand-in for appvar lookup
 jr z,$+8
 cp appvarobj
 jr nz,matchVATskip
 jr matchVATcont
 bit gGroup,(iy+myflag)
 jr z,$+8
 cp groupobj
 jr nz,matchVATskip
 jr matchVATcont
 sub 5
 jr z,matchVATcont
 dec a
 jr nz,matchVATskip
matchVATcont:
 ld ix,Op3
 ld (ix-11),b  ;setting Op2 (output) to size of suspect file
 ld a,(ix)     ;getting size of string to test
 cp b          ;lookforsize-sizefound. To skip, sizefound be smaller (nc)
 jr z,$+4
 jr nc,matchVATskip
 inc ix
 ld c,a       ;saving counter used for searching Op1 name
matchVATLoop:
 ld a,(hl)
 ld (ix-11),a
 cp (ix+00)
 jr nz,matchVATskip
 inc ix
 dec hl
 dec b
 dec c 
 jr nz,matchVATLoop
 ld a,b
 or a
 ret z
 ld a,(hl)
 ld (ix-11),a
 inc ix
 dec hl
 djnz $-7
 ret
matchVATskip:
 ld a,b
 neg
 ld c,a
 ld b,$FF
 add hl,bc
 jr matchVAT
 