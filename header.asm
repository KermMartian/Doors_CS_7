;-----------------------------------------------------------
;	Filename:		header.asm
;	Long name:  	Header Defines and Equates
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	June 2, 2006
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

;+====================================+
;|              SECTION 1             |
;|             BUILD FLAGS            |
;+====================================+

;versionNum .equ	"7.3"
;===Build Flags for CALCnet2===
#define enableCn22
;#define enablecn2eis
;#define cn2fakeserial
#define cn2debug
;#define cn2fakeserial_gcnhost
#define cn2_installerinflash

;===Build Flags for Execution===
#define mossupport
#define showbasicerrors
#define hook3
#define noswap
#define basichooktempfix
#define celtic3support
#define dcsblibs
;#define celtic3debug
;#define adcsswap
;#define highmemoryswp
;#define adcsloader
#define guiswap_VATtop					;swap the VAT entry for gui7 to VAT highmem for speed purposes
#define cn2_basic						;offer CALCnet 2.2 to BASIC programs

;===Build Flags for GUI===
;#define smgt
;#define intro
;#define multiplefonts

;===Build Flags for Folder and Version Support===
#define lang_english
;#define lang_french
;#define lang_spanish
#define alphasort
#define ProEdition
#ifdef ProEdition
#define Folders
#endif

;===Editions
;#define edition_elfprince
;#define edition_souvik
;#define edition_merth
;#define edition_abrum
#define edition_normal

;===Extensions Build Flags===
;#define USBDrivers
#define AnsHide
#define enableeditor
;#define easteregg

;+====================================+
;|              SECTION 2             |
;|        DEFINES AND EQUATES         |
;+====================================+

; AppVar Format:
; B> Use>        Length>
; 0  ProgNameSpace  9
; 9  ProgNamespace2 9
; 18 SafeArcFlags   2
; 20 SafeArcFlags3  1

AppVarSize	.equ	81d
C3AVSize	.equ	8d					;at offset +9 to +16, inclusive
_freeRAM 	.equ	_memChk
_enoughRAM	.equ	_memChk
_parseImp	.equ	_parseInp
pendfile	.equ	$8A3A
SEram		.equ	$86EC
;All possible SafeRAM Areas:
;AppBackupScreen	9872h	768 -->9898,9999,9A9A,9B9B would work for the interrupt!
cn2_Int_RAM	.equ	9999h
;SavesScreen		86ECh	768 -->that leaves this for DCS programs to use 
;StatsVars		8A3Ah	477  \___ Contiguous 531 bytes -> now used for internal variables
;AnovaVars		8C17h	54   /



;+---------------------------+
;|       Pendfile Format     |
;+---------------------------+
;
;+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
;| 0 --> | 1 --> | 2 --> | 3 --> | 4 --> | 5 --> | 6 --> | 7 --> | 8 --> | 9 --> |
;+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
;| Exist | Data Start -> | Data Size --> | NameL | Parent Start  | Parent VATLoc |
;+-------+---------------+---------------+-------+---------------+---------------+

ALEVectorStart	.equ	pendfile+10	;pendfile takes 10 bytes while ALEs are 48 bytes max
									;3 bytes per routine, 16 routines max
ProgExecuting 	.equ	$1
ionVectors		.equ	$966E+80
ionLibs			.equ	$8
dcsVectors		.equ	ionVectors+(3*IonLibs)

bPort				.equ  $00

SendCLDL		.equ	$00
SendCHDL		.equ	$01
SendCLDH		.equ	$02
SendCHDH		.equ	$03

ClockMask		.equ	$01
ClockHigh		.equ	$00
ClockLow		.equ	$01

DataMask		.equ	$02
DataHigh		.equ	$00
DataLow			.equ	$02

LinkMask		.equ 	$03
GetCLDL			.equ	$03
GetCHDL			.equ	$02
GetCLDH			.equ	$01
GetCHDH			.equ	$00

TotalRAMSize	.equ	25000d
#define ION_COMPATIBILITY		0	; Ion compatibility number
#define	LIBRARY_COMPATIBILITY	0	; Library compatibility number
#define NUMBER_OF_LIBRARIES		8	; Number of library routines
#define MODULE_COMPATIBILITY	2	; Module compatibility number
#define iVersion_MAJOR			1	; Version (major)
#define iVersion_MINOR			6	; Version (minor)
#DEFINE bcall(xxxx) b_call(xxxx)

;+----------------------------+
;|   Doors CS Internal Vars   |
;+----------------------------+
MainSafeRam		.equ	AppBackupScreen		;Stats RAM [CORRECT FOR PLUS]

;VFAT Format: (6 rows x 5 columns)
;+---------------+-------+--------------+-------+-------+-------+
;| 0 >>>>>>>>>>> | 1 >>> | 2 >>>>>>>>>> | 3 >>> | 4 >>> | 5 >>> |[byte offset]
;+---------------+-------+--------------+-------+-------+-------+
;| VAT Loc (LSB) | (MSB) | Prog L (LSB) | (MSB) | ROM P | Flags |
;+---------------+-------+--------------+-------+-------+-------+
NameX			.equ	MainSafeRAM		;1 byte
NameY			.equ	NameX+1			;1 byte
SpriteX			.equ	NameY+1			;1 byte
SpriteY			.equ	SpriteX+1		;1 byte
IconSpace8b		.equ	SpriteY+1		;8 bytes
IconSpace32b	.equ	IconSpace8b+8	;32 bytes
MseY			.equ	IconSpace32b+32	;1 byte
MseX			.equ	MseY+1			;1 byte
ScratchVar		.equ	MseX+1			;1 byte
ScratchWord		.equ	ScratchVar+1	;2 bytes
FreeRAM			.equ	ScratchWord+2	;2 byte
APDnext			.equ	FreeRAM+2		;2 bytes
dAPDTimer		.equ	APDnext+2		;1 byte
CurROMPage		.equ	dAPDTimer+1		;1 byte
AppVarLoc		.equ	CurROMPage+1	;2 bytes
SETable			.equ	AppVarLoc+2		;16 bytes
	SEData1		.equ	SETable
	SEData2		.equ	SETable+4
	SEData3		.equ	SETable+8
	SEData4		.equ	SETable+12		;73 bytes reserved

VFAT			.equ	SETable+16		;36 bytes for Virtual File Allocation Table
PrevVATStart	.equ	VFAT+36			;2 bytes
CurVATStart		.equ	PrevVATStart+2	;2 bytes
NextVATStart	.equ	CurVATStart+2	;2 bytes
ProgsOnScreen	.equ	NextVATStart+2	;1 byte
VATMode			.equ	ProgsOnScreen+1	;1 byte
TotalProgs		.equ	VATMode+1		;1 byte
TabFuncMode		.equ	TotalProgs+1	;1 byte
LastClick		.equ	TabFuncMode+1		;1 byte
ProgsDone		.equ	LastClick+1		;1 byte
ProgsToDo		.equ	ProgsDone+1		;1 byte
ModuleCount		.equ	ProgsToDo+1		;1 byte
CurVATPtr		.equ	ModuleCount+1	;2 bytes
SETotal			.equ	CurVATPtr+2		;1 byte
CurFldr			.equ	SETotal+1		;1 byte
PasteWord		.equ	CurFldr+1		;2 bytes		//129 to here
PrevVATArray	.equ	PasteWord+2		;up to 123 bytes = 61 programs max??? (or 61 screens = 366 prgms?)
;------------------------------------------------------------
;TOTAL:									;172 bytes

randData		.equ	$966e+78d		;2 bytes

;Aliases
;HomeSaveA		.equ	NameX						;1 byte		;now defuncted in favor of storage in appvar V
;HomeSaveHL		.equ	APDNext						;2 bytes	;for RawKeyHook during BASIC progs           |
;HomeSaveA2		.equ	PrevVATStart				;1 byte		;                                            |
;HomeSaveHL2		.equ	PrevVATStart+1				;2 bytes	;for AppChangeHook during BASIC progs        |
;HomeSaveA3		.equ	CurVatStart+1				;1 byte		;                                            |
;HomeSaveHL3		.equ	NextVATStart				;2 bytes	;for ParserHook during BASIC progs           ^

MseWord			.equ	MseY						;2 bytes
saveIX			.equ	randData					;2 bytes
ScrollPer		.equ	IconSpace8b					;2 bytes
ScrollMin		.equ	IconSpace8b+2				;2 bytes
ScrollMax		.equ	IconSpace8b+4				;2 bytes
ScrollCur		.equ	IconSpace8b+6				;2 bytes
api_drawline_color .equ	ScratchVar					;1 byte
linein_render_type .equ ScratchVar					;1 byte
GUIStackStage	.equ	IconSpace32b				;20 bytes
GUIStackMouse	.equ	IconSpace8b					;2 bytes
GUIStackMouseEnd .equ	IconSpace8b+2				;2 bytes
GUIMouseHookLoc	.equ	IconSpace8b+4				;2 bytes
GUIMseSave		.equ	IconSpace8b+6				;2 bytes

GUIReturnApp	.equ	NameY						;3 bytes: NameY, SpriteX, SpriteY
StartMenuShadow	.equ	$98F3						;165
StartMenuShadow1End = StartMenuShadow+165
StartMenuShadow2	.equ	$9B01						;114

;File Open/Save Areas
FileOSRAM		.equ	SETable					;total of 16 bytes
FileOSCurFld	.equ	FileOSRAM				;1 byte						;Current folder ID
FileOSTotFF		.equ	FileOSRAM+1				;1 byte						;Total files/folders in folder
FileOSCur		.equ	FileOSRAM+2				;1 byte						;Current offset
FileOSOnscreen	.equ	FileOSRAM+3				;1 byte						;How many fles/folders onscreen
FileOSYCoord	.equ	FileOSRAM+4				;1 byte
FileOSContext	.equ	FileOSRAM+5				;1 byte
FPopsWhere		.equ	FileOSRAM+6				;2 bytes
FPopsHowMuch	.equ	FileOSRAM+8				;2 bytes
FileOSExclude	.equ	FileOSRAM+10			;1 byte		;11 bytes used	;Whether to show only files for this program

;FileOSSaveAlpha	.equ	FileOSRAM+10			;5 bytes total
FileOSSaveAlpha	.equ	Op6			;5 bytes total

;GUIS Mouse Interactive Function Variables (GUISMIFV Tables)
GUITextRAM 		.equ	IconSpace32b+16				;imathptrs - NO LONGER IMATHPTRS
GUITextRX		.equ	GUITextRAM
GUITextRY		.equ	GUITextRAM+1
GUITextCurX		.equ	GUITextRAM+2				;cursor x from left of box
GUITextCurY		.equ	GUITextRAM+3				;cursor y from top of box
GUITextScreenOffset .equ IconSpace32b+4			;should be ok...	# of bytes from text start to screen start
GUITextCurOffset .equ	GUITextRAM+4				;# of bytes from screen start to cursor
GUITextCurStart .equ	GUITextRAM+6				;the first byte of text
GUITextWidthPx 	.equ	GUITextRAM+8
GUITextRows		.equ	GUITextRAM+9
TempCharRAM		.equ	IconSpace32b+6			;3-10 bytes
TxtEntrySave	.equ	FreeRAM				;2 bytes

MouseSpriteLoc 	.equ 	SpriteX					;2 bytes: SpriteX and SpriteY
SwapFromProgSW	.equ 	IconSpace8b			;2 bytes
SwapToProgSW	.equ 	IconSpace8b+2			;2 bytes

CMouseMask   	.equ   IconSpace32b
CMouse      	.equ   IconSpace32b+8
CX         		.equ   IconSpace32b+16
CY         		.equ   IconSpace32b+17
CGStimer      	.equ   IconSpace32b+18
CBlinktmr   	.equ   IconSpace32b+19 
skDown          .equ   01h
skLeft          .equ   02h
skRight         .equ   03h
skUp            .equ   04h
skEnter         .equ   09h
sk2nd           .equ   36h

MouseAccelSpd	.equ	$84D3			;1 byte
MouseAccelFac	.equ	MouseAccelSpd+1	;1 byte
MouseAccelAct	.equ	MouseAccelFac+1	;1 byte
MouseAccelNxCt	.equ	MouseAccelAct+1	;1 byte
MouseSlowest	.equ	250
MouseChange		.equ	5

MouseMode		.equ	VATMode

EditorMode		.equ	ScratchVar
wByteCount		.equ	ScratchWord
PrevProgType	.equ	FreeRAM
basicProgBackup	.equ	TempCharRAM

hookBackup		.equ	IconSpace8b

;+----------------------------+
;| Doors CS Flag Definitions  |
;+----------------------------+
dcsProgFlags	.equ	33	;asm_flags_1		Base (iy+n) additive
dcsGUIFlags		.equ	33	;ditto

;pretty sure these are wrong
dpLocked		.equ	0	;asm_flags_1_0		0=Unlocked		1=Locked
dpDoorsCS		.equ	1	;asm_flags_1_1		0=Non-DCS		1=Doors CS Compatible
dpIon			.equ	2	;asm_flags_1_2		0=Non-Ion		1=Ion Program
dpASM			.equ	3	;asm_flags_1_3		0=BASIC prog	1=ASM prog
dpFolder		.equ	4	;asm_flags_1_4		0=non-Folder	1=Folder
dpCompressed	.equ	5	;asm_flags_1_5		0=Uncompressed	1=compressed
dpMOS			.equ	6	;asm_flags_1_6		0=non-MOS		1=MOS
dpArc			.equ	7	;asm_flags_1_7		0=in RAM		1=in ROM

reDrawnFlag				.equ	0	;0=not redrawn; 1=redrawn
AlphaUpFlag				.equ	1	;0=alpha not up; 1= alpha onscreen
TextCursorVisibleFlag	.equ	2	;0=text cursor not visible; 1= text cursor visible

; Common Flag Mask Examples:
;	%00101100  = Ion
;	%00101110  = MOS
;	%00101010  = DCS
;	%00001000  = TI-OS ASM
;	%00000010  = DCS BASIC
;	%00001010  = DCS AP
;	%00000000  = TI-OS BASIC
;	%11111111  = Invalid

;_SetHomeHook equ 		4FABh
;_ClrHomeHook equ 		4FAEh
apiFlg2 equ 			41
sysHookFlg equ 			52
appInpPrmptInit equ 	0
appInpPrmptDone equ 	1
appChangeHookPtr equ	9BB0h
appWantHome equ 		4
ProgObj	equ				5
ProtProgObj	equ			6

_saveCmdShadow equ 		4573h
_saveShadow equ 		4576h
_rstrShadow equ 		4579h

cmdExec equ 			6
cmdFlags equ			12
cxExtApps equ 			058h

_executeapp equ 		4C51h
_EnableGetKeyHook = 	4F7Bh
_EnableGetCSCHook=		4F7Bh
_DisableGetKeyHook = 	4F7Eh 
_DisableGetCSCHook=		4F7Eh
_EnableRawKeyHook = 	4F66h
_DisableRawKeyHook = 	4F6Fh
_EnableMenuHook=		5083h
_DisableMenuHook=		5086h
_EnableLocalizeHook=	4F93h
_DisableLocalizeHook=	4F96h
_EnableAppChangeHook=	502Ch
_DisableAppChangeHook=	502Fh
_EnableParserHook=		5026h
_DisableParserHook=		5029h
_JForceCmd=				402Ah
_CreateTStrng .equ		4324h
_CreateTRList .equ		4312h
_CreateTempRList .equ	_CreateTRList
_CreateVar    .equ		4E70h
_CreateTempRMat .equ	431Eh
_CreateTempCList .equ	4318h
_CreateTempString .equ	_CreateTStrng
_fillBasePageTable .equ	5011h

AppObj		EQU	  		14h		;application, only used in menus/link
stack_bottom equ 		0FE70h
minContrast = 			10d
parseVar = 				9652h
_ChkFreeArc	EQU 		5014h
MenuCurrent = 			85DEh 
k2 = 					090h
kQuit =					040h
kInputDone =			03Fh
kOff =					kInputDone
kClear =				009h
AppVarObj = 			15h
GroupObj =				17h		;group.
ParsFlag2 =				7		;PARSER flags
numOP1 = 				0		; 1=RESULT IN OP1, 0=NO RESULT
TempProgObj =			16h		;program, home deletes when finished
fontFlags =				50
fracDrawLFont =			2
shiftLwrAlph =			5		; 1=lower case, 0=upper case
apiFlg4 = 				43
fullScrnDraw = 			2		; DRAW INTO LAST ROW/COL OF SCREEN

AVOff_HomeSaveA = 		0
AVOff_HomeSaveHL =		1
AVOff_HomeSaveA2 =		3
AVOff_HomeSaveHL2 =		4
AVOff_HomeSaveA3 =		6
AVOff_HomeSaveHL3 =		7
AVOff_CelticIIIAV =		9
AVOff_SPSave =			21
AVOff_GUIWinType = 		23
AVOff_GUIWinX = 		24
AVOff_GUIWinY = 		25
AVOff_CurrFolder = 		26
AVOff_CurrScreen = 		27
AVOff_Alpha =			32
AVOff_Lowercase =		33
AVOff_XLC3Libs = 		34
;AVOff_CALCnet2 =		35
AVOff_FldSave = 		35
AVOff_Offscript =		36
AVOff_MouseAccel =		37
AVOff_MseSpriteMask =	40
AVOff_MseSprite =		48
AVOff_MouseSpeed =		66
AVOff_CrashContext =	67
AVOff_ASortEnabled =	68
AVOff_ASMExecActive = 	69
AVOff_OnPrgmHook =		70
AVOff_OnPrgmHookSave =  71
AVOff_ParserHook = 		75
AVOff_ParserHookSave =  76
AVOff_FolderDirty = 	80

RelocatablePtr1	.equ	84EBh
RelocatablePtr2	.equ	84EDh
RAMChecksum		.equ	85BEh
lcd_busy_quick	.equ	000Bh

basic_prog		equ 9652h
nextParseByte		equ 965Dh ;basic_pc
