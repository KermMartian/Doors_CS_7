;-----------------------------------------------------------
;	Filename:		FldSv.asm
;	Long name:  	Folder Backup and Restore Functions
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	May 12, 2010
;
; Please consult license.txt for the fair use agreement
; implicitly binding by you continuing to read past this
; point.  Immediately close and delete this file if you do
; not agree to the terms of the agreement.
;----------------------------------------------------------

FldSave:
	ld hl,AVOff_FldSave
	call DAVLCheckOffset
	ld a,(hl)
	or a
	ret z
FldSave_NoAVCheck:
	ld hl,FldSvAppVarName						;If none exists, try to create it
	rst 20h
	bcall(_chkfindsym)
	jr c,FldSaveNew

	ld hl,AVOff_FolderDirty						;Folder backup dirty/clean
	call DAVLCheckOffset
	ld a,(hl)
	or a
	ret z							;No update is necessary
	
	;if we get here, it is necessary to do an update
	;first, we need to see if an appvar already exists, and delete it
FldSaveNew:
	xor a
	ld (ScratchVar),a				;mode: 0=count, 1=create
	ld hl,FldSvAppVarName
	rst 20h
	bcall(_chkfindsym)
	jr c,FldSaveNoPredelete
	bcall(_delvararc)				;Delete from arc or RAM (should be arc'ed though)
FldSaveNoPredelete:
	ld ix,0				;appvar size

	;now it's time to actually back up the folders.  We'll need to figure
	;out first how much memory we'll need for the appvar, then actually
	;put it together.  On the first pass we'll just record namelength of all programs
	;with non-main folders, as well as the 1+8-byte [00]+[name] name of fldrs

FldSaveNoPredeleteNoIXClear:
	ld hl,(ProgPtr)
FldSave_Pass1Loop:
	ld de,(ptemp)
	bcall(_mos_cphlde)
	jp z,FldSave_Pass2Setup
	ld a,(hl)
	and $1f
	sub 5							;5
	jr z,FldSave_Pass1Prgm
	dec a							;6
	jr z,FldSave_Pass1Prgm
FldSave_Pass1Cont:
	dec hl
FldSave_Pass1ContMain:			;specified program is in main folder
	ld c,0						;c=0: not a folderized prgm
	ld de,-5
	add hl,de					;1 byte cheaper than 5 "dec hl"s
	ld b,(hl)
FldSave_Pass1ContPrgmSaved:
	ld a,5						;"%FLD#"
	sub b
	jr nz,FldSave_Pass1ContNotPossibleFld
	set 1,c						;set bit 1 = 2's place					
								;c=3: could be folderized folder or program
								;c=2: could be folder
								;c=1: must be folderized program
								;c=0: neither folder nor folderized prgm
FldSave_Pass1ContNotPossibleFld:
	ld a,c
	or a
	jr z,FldSave_Pass1SkipOut
	ld de,BaseFld+1				;"%FLD..."
	push hl
		dec hl
FldSave_Pass1NameLoop:
		ld a,(de)
		cp (hl)
		jr z,FldSave_Pass1NameLoop2
		res 1,c					;can't be folder
FldSave_Pass1NameLoop2:
		dec hl
		push hl
			ld hl,BaseFld+1+3	;on the "D" of "%FLD"
			bcall(_mos_cphlde)
			pop hl
		jr nz,FldSave_Pass1NameLoop3
		bit 1,c
		jr nz,FldSave_Pass1Fldr
FldSave_Pass1NameLoop3:
		inc de
		djnz FldSave_Pass1NameLoop
		pop hl
	dec c						;c=1: must be folderized prog
	jr nz,FldSave_Pass1SkipOut
	ld b,(hl)
	push hl
		inc b					;folder location
		ld a,(ScratchVar)
		or a
		jr nz,FldSave_Pass2Prgm
		inc b					;size byte
		ld e,b
		ld d,0
		add ix,de
		pop hl
	jr FldSave_Pass1SkipOut
FldSave_Pass2Prgm:
		ld (ix+0),b				;entry length, not including length byte
		push hl
			ld de,5
			add hl,de
			ld a,(hl)
			ld (ix+1),a			;page
			pop hl
		ld b,(hl)
		inc ix
		inc ix
		dec hl
FldSave_Pass2PrgmLoop:
		ld a,(hl)
		ld (ix+0),a
		dec hl
		inc ix
		djnz FldSave_Pass2PrgmLoop
		pop hl
FldSave_Pass1SkipOut:
	ld b,(hl)
	dec hl
FldSave_Pass1SkipOutNameLoop:
	dec hl
	djnz FldSave_Pass1SkipOutNameLoop
	jr FldSave_Pass1Loop
FldSave_Pass1Prgm:
	dec hl
	ld a,(hl)
	dec a
	jr z,FldSave_Pass1ContMain
	ld de,-5
	add hl,de
	ld b,(hl)
	ld c,1						;c=1: could be a folderized prgm or fldr
	jr FldSave_Pass1ContPrgmSaved
FldSave_Pass1Fldr:
;		ld a,(hl)
;		dec a					;was this %FLD1 (main)?
;		jr z,FldSave_Pass1Fldr_SkipMain
		ld a,(ScratchVar)
		or a
		jr nz,FldSave_Pass2Fldr
		ld de,11				;size (bit 7 set for fldr), then number, then location, then up to 8 bytes
		add ix,de
		jr FldSave_Pass1Fldr_SkipMain
FldSave_Pass2Fldr:
		ld a,(hl)
		ld (ix+1),a				;folder number
		pop hl
	push hl
		ld b,10+128				;name length + number + location, bit 7 also set to indicate folder
		ld (ix+0),b				;entry length, not including length byte
		push hl
			ld de,5
			add hl,de
			ld a,(hl)
			ld (ix+2),a			;T2
			pop hl
		inc hl					;page
		inc hl					;DAH
		ld d,(hl)
		inc hl					;DAL
		ld e,(hl)
		ex de,hl
		inc hl
		inc hl
		inc ix
		inc ix
		inc ix
		ld b,8
FldSave_Pass2FldrLoop:
		ld a,(hl)
		ld (ix+0),a
		inc hl
		inc ix
		djnz FldSave_Pass2FldrLoop
FldSave_Pass1Fldr_SkipMain:
		pop hl
	jr FldSave_Pass1SkipOut
FldSave_Pass2Setup:
	ld a,(ScratchVar)
	dec a
	jr nz,FldSave_Pass2Setup2
	ld hl,FldSvAppVarName
	rst 20h
	bcall(_chkfindsym)
	call Arc_Unarc
	ld hl,AVOff_FolderDirty
	call DAVLCheckOffset
	ld (hl),0
	ret								;all set if ScratchVar is 1
FldSave_Pass2Setup2:
	push ix							;save the requested size...
		ld hl,FldSvAppVarName
		rst 20h
		pop hl						;...and recall it.
	ld a,h
	or l
	ret z							;don't create it if there's nothing to save
	bcall(_createappvar)			;LSB of size is at de, MSB of VAT entry at hl
	inc de
	inc de
	push de
		pop ix						;ix is pointer to current AppVar location
	ld a,1
	ld (ScratchVar),a				;mode 1 is fill in the contents of the appvar now
	jp FldSaveNoPredeleteNoIXClear

FldRestore:
	xor a
	ld hl,ScratchVar
	ld (hl),a						;(ScratchVar)=0: No backup file found
	push hl
		ld hl,FldSvAppVarName
		rst 20h
		bcall(_chkfindsym)
		pop hl
	ret c							;no folder save file exists
	ld a,b
	or a
	jr z,FldRestoreNoUnarc
	bcall(_arc_unarc)
	jr FldRestore
FldRestoreNoUnarc:
	ld (hl),2						;(ScratchVar)=2: Perfect restoration performed
	ex de,hl
	push hl
		bcall(_ldhlind)
		pop de
	ex de,hl
	inc hl
	inc hl							;hl = first byte of data, de = size
	push de
FldRestoreLoop:
		pop de
	ld a,e
	or d
	jr nz,FldRestoreLoopCont
	ld hl,FldSvAppVarName
	rst 20h
	bcall(_chkfindsym)
	ret c
	bcall(_arc_unarc)				;rearchive
	ret
FldRestoreLoopCont:
	ld c,(hl)						;data length
	push hl
		ld l,c
		res 7,l
		ld h,0
		inc hl
		ex de,hl
		or a
		sbc hl,de
		pop de
	ex de,hl
	push de
		bit 7,c							;is it a folder?
		jr nz,FldRestoreFolder
		inc hl
		ld a,(hl)
		ld de,Op1
		push af
			push de
				ld b,0
				push bc
					ldir
					pop bc
					ld a,9
					sub c
					jr z,FldRestoreName9B
				xor a
				ld (de),a	;zero-term the name
FldRestoreName9B:
				pop de						;hl is now on the next entry, btw
			push hl
				ex de,hl
				ld (hl),5						;data type: program
				bcall(_chkfindsym)
				pop de
			jr c,FldRestoreNotFound
			pop af
		dec hl
		ld (hl),a							;a should still be folder number
FldRestoreFinishLoop:
		ex de,hl
		jr FldRestoreLoop
FldRestoreNotFound:
			pop af
		ld a,1						;1=partial restoration
		ld (ScratchVar),a
		jr FldRestoreFinishLoop
FldRestoreFolder:
		res 7,c
		inc hl
		ld a,(hl)
		inc hl
		ld b,(hl)
		inc hl
		push hl
			push bc
				ld hl,BaseFld
				ld de,Op1
				ld bc,5
				ldir
				ld (de),a
				inc de
				xor a
				ld (de),a
				bcall(_chkfindsym)
				jr nc,FldRestoreFolderExists
				ld hl,8
				bcall(_createprog)		;folders are progs, not protprogs
				dec hl
				pop bc
			ld (hl),b
			inc de
			inc de
			ld bc,8
			pop hl
		ldir
		jp FldRestoreLoop
FldRestoreFolderExists:
				pop bc
			pop hl
		ld bc,8
		add hl,bc
		jp FldRestoreLoop
