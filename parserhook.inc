;-----------------------------------------------------------
;	Filename:		parserhook.asm
;	Long name:  	HomeRun Hook for homescreen execution
;	Authors:		Kerm Martian aka Christopher Mitchell
;	Last Update:	Unknown
;
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

ParserHook:
	add a,e
	or a
	jr z,ParserHookChkPrgm
	cp a
	ret
ParserHookChkPrgm:
	ld hl,parseVar
	push hl
		rst 20h
		pop hl
	ld a,(hl)
	cp 5						;ProgObj
	jr z,ParserHookRunPrgm	
	cp 6						;ProtProgObj
	jr nz,ParserHookExit
ParserHookRunPrgm:
	bcall(_chkfindsym)
	jr c,ParserHookExit
	ld a,(parseVar+1)
    cp '#'
	jr z,ParserHookHomescreen
	push hl
		bcall(_DisableParserHook)
		bcall(_DisableLaunchKeyhook)
		res indicRun,(iy+IndicFlags)
		pop hl

	bcall(_RunProgFromHook)
	push af
		set CurAble,(iy+CurFlags)
		res textWrite,(iy+SgrFlags)
		ld a,%11010000
		out (0),a
		im 1
		ei
		bcall(_getcsc)
;		ld a,40h \ bcall(_newContext)	;homescreen context
		bcall(_EnableLaunchKeyhook)
		bcall(_EnableMyParserhook)
		pop af
	jr c,ParserHookFullExit
;	res plotDisp,(iy+plotFlags)
	or 1						;reset zero flag
	ret
ParserHookFullExit:
	bcall(_clrtxtshd)
	bcall(_homeup)
	ld a,kClear
	bjump(_jforcecmd)
ParserHookExit:
	bcall(_EnableMyParserHook)
	xor a
	ret
ParserHookHomescreen:
;HL points to size bytes
		ex de,hl
		ld c,(hl)
		inc hl
		ld b,(hl)
		inc hl
		ld (wByteCount),bc
ParserHookHomescreenFindLoop:
		call getHomescreenToken
		jr c,ParserHookExit
		cp 5Fh
		jr z,prgmTokenFound
ParserHookHomescreenParseLoop:
		call getHomescreenToken
		jr c,ParserHookExit
		cp 3Eh
		jr z,ParserHookHomescreenFindLoop
		cp 3Fh
		jr z,ParserHookHomescreenFindLoop
		jr ParserHookHomescreenParseLoop
prgmTokenFound:
		push hl
			ld hl,parseVar
			ld de,basicProgBackup
			ld bc,9
			ldir
			pop hl
		ld ix,parseVar+1
		call getHomescreenByte
		jr c,ParserHookExit
		cp '#'
		jr z,ParserHookExit
prgmTokenLoop:
		ld (ix+0),a
		inc ix
		call getHomescreenByte
		jr c,prgmTokenLoopOut
		cp 3Eh
		jr z,prgmTokenLoopOut
		cp 3Fh
		jr z,prgmTokenLoopOut
		jr prgmTokenLoop
prgmTokenLoopOut:
		xor a
		ld (ix+0),a
		call ParserHookChkPrgm
		ret nz
		ld hl,basicProgBackup
		ld de,parseVar
		ld bc,9
		ldir
		ret
getHomescreenToken:
		call getHomescreenByte
		ret c
		B_CALL(_isA2ByteTok)
		scf
		ccf
		ret nz
		push af
		call getHomescreenByte
		ld e,a
		pop bc
		ld a,b
		ret
getHomescreenByte:
		ld d,(hl)
		inc hl
		ld bc,(wByteCount)
		dec bc
		ld (wByteCount),bc
		inc bc
		ld a,b
		or c
		ld a,d
		ret nz
		scf
		ret
