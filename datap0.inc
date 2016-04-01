;-----------------------------------------------------------
;	Filename:		datap0.inc
;	Long name:  	Data for app page 0
;	Authors:		Kerm Martian aka Christopher Mitchell
;	Last Update:	Unknown
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

SMAboutTxt3:
	.db "(c)2001-13 Cemetech     6.3.0",0
InfoPopSize:
	.db "Size: ",0
InfoPopSize2:
	.db " bytes",0
InfoPopSize3:
	.db "Desc: ",0
MemoryPop_RAM:
	.db "RAM:",0
MemoryPop_ARC:
	.db "ARC:",0
MemoryPop_KSlash:
	.db "K"
MemoryPop_Slash:
	.db "/",0
MemoryPop_K:
	.db "K",0
MemoryPop_24576:
	.db "24576",0
FolderTextString:
	.db "Folder",0

CrashTxt_Fld0:
	.db "No",0
CrashTxt_Fld1:
	.db "Some",0
CrashTxt_Fld2:
	.db "All",0
CrashTxt_FldFinish:
	.db " folders restored.",0


PropSprite:
	.db $FF,$FF,$FF,$FF,$F8,$80,$00,$00,$00,$08,$8C,$E8,$EE,$EA,$48,$8A
	.db $C8,$C4,$C1,$88,$8A,$88,$84,$89,$88,$8C,$EE,$E4,$E2,$48,$80,$00
	.db $00,$08,$08,$80,$08,$E6,$A1,$88,$80,$08,$A8,$CA,$48,$80,$08,$A8
	.db $A3,$C8,$80,$0E,$E6,$AB,$C8,$80,$00,$00,$00,$08,$93,$1A,$AA,$E9
	.db $C8,$AA,$A3,$AA,$C0,$C8,$BB,$23,$AA,$89,$48,$AA,$9A,$A4,$E1,$08
	.db $80,$00,$00,$08,$08,$80,$06,$EC,$A3,$88,$80,$08,$AA,$AA,$48,$80
	.db $08,$AC,$42,$48,$80,$06,$E8,$49,$C8,$80,$00,$00,$00,$08,$80,$00
	.db $6A,$EA,$88,$80,$00,$8A,$40,$48,$80,$00,$8A,$4A,$08,$80,$00,$6E
	.db $41,$48,$80,$00,$00,$08,$08,$80,$C4,$EE,$E0,$88,$80,$AA,$C4,$CB
	.db $C8,$80,$CE,$24,$83,$C8,$80,$8A,$E4,$E8,$88,$80,$00,$00,$00,$08
	.db $8E,$E8,$CE,$C9,$88,$88,$A8,$AC,$A2,$68,$8C,$A8,$A8,$CA,$28,$88
	.db $EE,$CE,$A3,$E8,$80,$00,$00,$08,$08,$8C,$EC,$4E,$E0,$C8,$8A,$CA
	.db $AE,$CB,$48,$8C,$8A,$EA,$83,$48,$8A,$EA,$AA,$E8,$C8,$80,$00,$00
	.db $00,$08,$80,$02,$AC,$EA,$88,$80,$03,$AA,$C1,$48,$80,$03,$AA,$8A
	.db $88,$80,$02,$AC,$E1,$48,$80,$00,$00,$08,$08
#ifdef enableeditor
	.db $80,$03,$B2,$E0,$C8
	.db $80,$03,$2A,$49,$48
	.db $80,$02,$2A,$41,$88
	.db $80,$03,$B2,$4A,$08
	.db $80,$00,$00,$00,$08
#endif
	.db $FF,$FF,$FF,$FF,$F0

#ifdef lang_french
#ifdef Folders
BaseFld:
	.db 6,"%FLD",1,0
#endif

EnterNameTxt:
	.db "Nom de fichier: ",0
FolderIcon:
	.db $00,$00,$38,$F0,$47,$08,$80,$1C
	.db $80,$E2,$87,$02,$88,$04,$50,$04
	.db $50,$04,$50,$04,$50,$08,$50,$30
	.db $33,$C0,$3C,$00,$00,$00,$00,$00

ErrorTxtList:
	.dw et501
	.dw et502
	.dw et503
	.dw et504
	.dw et505
	.dw et506
et501:	.db "Fichier invalide",0
et502:	.db "Librairie manquante",0
et503:	.db "Fichier non trouv",$96,0
et504:	.db "M",$96,"moire insuffisante",0
et505:	.db "Bureau vide",0
et506:	.db "Erreur d'archive",0
BASICErrorTxt:
		.db "Erreur d'exécutn",0
GotoErrL1:
		.db "1. Quitter",0
GotoErrL2:
		.db "2. Goto l'erreur",0
;SMAboutTxt1:
;	.db "Aide et cr",$96,"dits:",0
;SMAboutTxt2:
;	.db "http://dcs.cemetech.net",0
CrashTxt1:
	.db "Votre calculatrice a crashé.",0
CrashTxt2:
	.db "Pr",$96,"f",$96,"rences restaur",$96,"es.",0
CrashTxt3:
	.db "http://dcs.cemetech.net",0
CrashTxt4:
	.db "Appuyez sur ",$c1,"Enter]",0
ErrTxt:
	.db "Erreur #",0
;BackupAppVarTxt:
;	.db "Archiving appvar.",0
NMRAM_Message:
	.db "Erreur de GUI: M",$96,"moire",0
#endif

#ifdef lang_german
#ifdef Folders
BaseFld:
	.db 6,"%FLD",1,0
#endif

EnterNameTxt:
	.db "Nom de fichier: ",0
FolderIcon:
	.db $00,$00,$38,$F0,$47,$08,$80,$1C
	.db $80,$E2,$87,$02,$88,$04,$50,$04
	.db $50,$04,$50,$04,$50,$08,$50,$30
	.db $33,$C0,$3C,$00,$00,$00,$00,$00

ErrorTxtList:
	.dw et501
	.dw et502
	.dw et503
	.dw et504
	.dw et505
	.dw et506
et501:	.db "Unzul",$91,"ssiges Filetype",0
et502:	.db "Fehlende Bibliothek",0
et503:	.db "Akte nicht gefunden",$96,0
et504:	.db "Nicht gen",$B1,"gend Ged",$91,"chtnis",0
et505:	.db "Leerer Schreibtisch",0
et506:	.db "Archiv-St",$A9,"rung",0
BASICErrorTxt:
		.db "BASIC Durchf",$B1,"hrung Störung",0
;SMAboutTxt1:
;	.db "Hilfe und Gutschriften:",0
;SMAboutTxt2:
;	.db "http://dcs.cemetech.net",0
CrashTxt1:
	.db 0,0
CrashTxt2:
	.db "Pr",$91,"ferenzen wieder hergestellt",0
CrashTxt3:
	.db "http://dcs.cemetech.net",0
CrashTxt4:
	.db "Bet",$91,"tigen ",$c1,"Enter]",0
ErrTxt:
	.db "St",$A9,"rung #",0
BackupAppVarTxt:
	.db "Archiving appvar...",0
NMRAM_Message:
	.db "St",$A9,"rung GUI: Ged",$91,"chtnis",0
#endif

#ifdef lang_spanish
#ifdef Folders
BaseFld:
	.db 6,"%FLD",1,0
#endif

EnterNameTxt:
	.db "Enter filename: ",0
FolderIcon:
	.db $00,$00,$38,$F0,$47,$08,$80,$1C
	.db $80,$E2,$87,$02,$88,$04,$50,$04
	.db $50,$04,$50,$04,$50,$08,$50,$30
	.db $33,$C0,$3C,$00,$00,$00,$00,$00

ErrorTxtList:
	.dw et501
	.dw et502
	.dw et503
	.dw et504
	.dw et505
	.dw et506
et501:	.db "Filetype inv",$8E,"lido",0
et502:	.db "Biblioteca que falta",0
et503:	.db "Archivo no encontrado",0
et504:	.db "No bastante memoria",0
et505:	.db "Tablero del escritorio vacío",0
et506:	.db "Error del archiv",0
BASICErrorTxt:
		.db "Error del BASIC ",0
GotoErrL1:
		.db "1. Quit",0
GotoErrL2:
		.db "2. Goto error",0
;SMAboutTxt1:
;	.db "Ayuda y cr",$96,"ditos:",0
;SMAboutTxt2:
;	.db "http://dcs.cemetech.net",0
CrashTxt1:
	.db "Tu calculadora estrellada.",0
CrashTxt2:
	.db "Las preferencias restauraron.",0
CrashTxt3:
	.db "http://dcs.cemetech.net",0
CrashTxt4:
	.db "Prensa ",$c1,"Enter]",0
ErrTxt:
	.db "Error #",0
BackupAppVarTxt:
	.db "Archiving appvar.",0
NMRAM_Message:
	.db "GUI Error: Memoria",0
#endif

#ifdef lang_english
#ifdef Folders
BaseFld:
	.db 6,"%FLD",1,0
#endif

EnterNameTxt:
	.db "Enter filename: ",0
FolderIcon:
	.db $00,$00,$38,$F0,$47,$08,$80,$1C
	.db $80,$E2,$87,$02,$88,$04,$50,$04
	.db $50,$04,$50,$04,$50,$08,$50,$30
	.db $33,$C0,$3C,$00,$00,$00,$00,$00


ErrCodes:
ec501:	.db "TYP"
ec502:	.db "LIB"
ec503:	.db "FNF"
ec504:	.db "MEM"
ec505:	.db "NPG"
ec506:	.db "MEM"
ec507:	.db "OVR"
ec508:	.db "DV0"
ec509:	.db "SNG"
ec510:	.db "DOM"
ec511:	.db "INC"
ec512:	.db "BRK"
ec513:	.db "NRL"
ec514:	.db "SYN"
ec515:	.db "TYP"
ec516:	.db "ARG"
ec517:	.db "MSM"
ec518:	.db "DIM"
ec519:	.db "UND"
ec520:	.db "MEM"
ec521:	.db "INV"
ec522:	.db "ILN"
ec523:	.db "BND"
ec524:	.db "RNG"
ec525:	.db "ZOM"
ec526:	.db "LBL"
ec527:	.db "STS"
ec528:	.db "SLV"
ec529:	.db "SNG"
ec530:	.db "SCH"
ec531:	.db "ITR"
ec532:	.db "BDG"
ec533:	.db "STP"
ec534:	.db "TNM"
ec535:	.db "RSV"
ec536:	.db "MDE"
ec537:	.db "LNK"
ec538:	.db "MEM"
ec539:	.db "XMT"
ec540:	.db "DUP"
ec541:	.db "MEM"
ec542:	.db "TYP"
ec543:	.db "SCL"
ec544:	.db "IDN"
ec545:	.db "MDE"
ec546:	.db "VAL"
ec547:	.db "LEN"
ec548:	.db "APP"
ec549:	.db "AE1"
ec550:	.db "AE2"
ec551:	.db "EXP"
ec552:	.db "ADR"
ec553:	.db "ARC"
;ec554:	.db "VER"
;ec555:	.db "AFL"
;ec556:	.db "VAR"
;ec557:	.db "DUP"
ErrCodesEnd:

prgmTxt:
	.db "prgm",0

ErrorTxtList:
	.dw et501
	.dw et502
	.dw et503
	.dw et504
	.dw et505
	.dw et506
et501:	.db "Invalid Filetype",0
et502:	.db "Missing Library",0
et503:	.db "File Not Found",0
et504:	.db "Not Enough Mem",0
et505:	.db "Empty Desktop",0
et506:	.db "Archive Error",0
BASICErrorTxt:
		.db "BASIC Ex Err:",0
GotoErrL1:
		.db "1. Quit",0
GotoErrL2:
		.db "2. Goto error",0
;SMAboutTxt1:
;	.db "Help and credits:",0
;SMAboutTxt2:
;	.db "http://dcs.cemetech.net",0
CrashTxt1:
	.db "Your calc crashed or reset.",0
CrashTxt2:
	.db "Preferences restored.",0
CrashTxt3:
	.db "http://dcs.cemetech.net",0
CrashTxt4:
	.db "Press ",$c1,"Enter]",0
ErrTxt:
	.db "Error #",0
BackupAppVarTxt:
	.db "Archiving appvar.",0
NMRAM_Message:
	.db "GUI Error: Need RAM",0
#endif

FldSvAppVarName:
	.db 15h,"FLDSV7",0
AppVarName:
	.db 15h,"DCS7",0
dcsBASICtxt:
	.db $3E,"DCS"
ADCStxt:
	.db 5,"ZDCS",0
BGProgName:
	.db 5,"ZDCSBG",0
TmpProgName:
	.db 5,"dcsasmex",0
TmpProgName2:
	.db 5,"zemsascd",0				;temp files will always start with z+1
GUIavNamep0:
	.db 15h,"gui7",0
UpFldIcon:
	.db %00100100
	.db %01101100
	.db %11111100
	.db %01101100
	.db %00100100
StartMenuSprite:
	.db %01111111,%11110000
	.db %01111111,%11110010
	.db %11111101,%11111010
	.db %11111010,%11111010
	.db %11110101,%01111010
	.db %11111010,%11111010
	.db %11110111,%01111010
	.db %01111111,%11110010
	.db %01111111,%11110000
StartMenuSprite2:
	.db %01000000,%00010000
	.db %01000000,%00010010
	.db %10001000,%10001010
	.db %10000101,%00001010
	.db %10001010,%10001010
	.db %10000101,%00001010
	.db %10000010,%00001010
	.db %01000000,%00010010
	.db %01000000,%00010000
ScrollBarData:
	.db 89,0,43,0
	.dw 6,0,0,0
ScrollBarDataEnd:
TaskbarIcons:
	.db $0A,$A7,$FC,$44
	.db $1F,$F4,$06,$28
	.db $10,$14,$06,$10
	.db $1F,$F4,$06,$28
	.db $0A,$A7,$FC,$44
MOUSE_SPRITE:
	.db %01111111
	.db %00111111
	.db %00011111
	.db %00001111
	.db %00000111
	.db %00001111
	.db %01000111
	.db %11100111
	
	.db %10000000
	.db %11000000
	.db %10100000
	.db %10010000
	.db %10001000
	.db %11010000
	.db %10101000
	.db %00011000
ErrDialogMask:
	.db %10000011
	.db %00000001
	.db %00000001
	.db %00000001
	.db %00000001
	.db %00000001
	.db %10000011
	.db %11000111
ErrDialogIcon:
	.db $7c,$82,$AA,$92
	.db $aa,$82,$6C,$10
ArcIcon:
	.db $E0,$60,$A0,$80
HideIcon:
	.db $A0,$E0,$A0
LockIcon:
	.db $40,$A0,$E0,$E0
PIconIon:
	.db %01111101,%11010010
	.db %10000010,%00101101
	.db %01101110,%10101101
	.db %00101010,%10100101
	.db %00101010,%10101001
	.db %01101110,%10101101
	.db %10000010,%00101101
	.db %01111101,%11010010
	.db %00000000,%00000000
	.db %00000000,%00000110
	.db %00111000,%00001011
	.db %01011100,%00101111
	.db %10001110,%10100110
	.db %11011110,%10000000
	.db %11111110,%10100110
	.db %01111100,%00101011
PIcondcsASM:
	.db $7F,$D2,$7F,$D5,$4F,$D5,$43,$C9
	.db $41,$C0,$41,$D1,$41,$D1,$45,$D1
	.db $41,$CE,$41,$C0,$41,$CE,$31,$11
	.db $0D,$11,$03,$1F,$00,$00,$00,$00
PIconASMos:
	.db $20,$00,$50,$00,$53,$68,$FA,$54
	.db $89,$54,$8B,$54,$00,$00,$00,$04
	.db $FB,$86,$A9,$3F,$21,$06,$73,$84
	.db $00,$33,$7F,$4A,$00,$49,$00,$33
PIconBASICos:
	.db $C0,$00,$A0,$00,$E6,$D3,$92,$84
	.db $96,$54,$E6,$D3,$00,$00,$00,$04
	.db $FB,$86,$A9,$3F,$21,$06,$73,$84
	.db $00,$33,$7F,$4A,$00,$49,$00,$33
PIconBASICaxesrc:
	.db $01,$80,$1A,$58,$3E,$7C,$3F,$FC
	.db $3F,$FC,$3E,$7C,$1A,$58,$0A,$50
	.db $02,$40,$FF,$FF,$C4,$71,$BD,$AF
	.db $CC,$6F,$F5,$AF,$8D,$B1,$FF,$FF

CrashIcon:
	.db $01,$80,$03,$C0
	.db $03,$c0,$06,$60
	.db $06,$60,$0E,$70
	.db $0E,$70,$1E,$78
	.db $1E,$78,$3E,$7C
	.db $3E,$7C,$7F,$FE
	.db $7E,$7E,$FE,$7F
	.db $FF,$FF,$7F,$FE
sHomescreen:
    .db 05h,21h,0
aDriveIcon:
	.db $00,$00,$00,$80,$01,$E0,$03,$98,$07,$06,$0F,$01,$1E,$03,$3E,$07
	.db $7E,$0E,$7F,$1C,$4F,$B8,$33,$F0,$0C,$E0,$03,$C0,$00,$80,$00,$00

;---------------------------------------------------
; The following contains the areas for the tabfuncs
; calls, as (x, y, x, y)
;---------------------------------------------------
TabFuncAreas_0:
	.db 3,3,26,28				;Prgm 1 of 6		;0
	.db 32,3,55,28				;Prgm 2 of 6		;1
	.db 61,3,84,28				;Prgm 3 of 6		;2
	.db 3,29,26,54				;Prgm 4 of 6		;3
	.db 32,29,55,54				;Prgm 5 of 6		;4
	.db 61,29,84,54				;Prgm 6 of 6		;5
	.db 0,55,17,63				;DCS Menu			;6
	.db 17,55,24,63				;Fldr back			;7
	.db 89,0,95,6				;Scroll up			;8
	.db 89,7,95,55				;Scroll down		;9
	.db 88,55,95,63				;Quit				;10

TabFuncAreas_1:
	.db 53,3,88,8				;Delete				;0
	.db 53,8,88,13				;Lock				;1
	.db 53,13,88,18				;Archive			;2
	.db 53,18,88,23				;Copy				;3
	.db 53,23,88,28				;Cut				;4
	.db 53,28,88,33				;Paste				;5
	.db 53,33,88,38				;Folder				;6
	.db 53,38,88,43				;Rename				;7
	.db 53,43,88,48				;Hide				;8
	.db 53,48,88,53 			;Edit				;9
	.db 88,55,95,63				;Quit				;10
	.db 0,0,51,64				;Cancel				;11
	