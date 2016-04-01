;************************************************************************************************************************
;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM7    7MMMMMMMMMMMMM7   +7MMMMMMMMMMMMMMMMMM7  MMMM
;MMMMMMMMMMMM,   MMMMMMMMMMMMMMMZ                  MMMMMMMMMMMMMMM=    7MMM7     MMMMMM    NMMI    MMMMMMMMMMMM     7MMMM
;MMMMMMMMMMM      ?MMMMMMMMMMMMMMMMM=   MMMMMMMMM    +MMMMMMMMMM   NMMMMMMMMMM,  MMMMN  7MMMMMMM,  MMMMMMMMMZ   MMMMMMMMM
;MMMMMMMMM=         MMMMMMMMMMMMMMMM=   MMMMMMMMMMM    MMMMMMM    MMMMMMMMMMMMO  MMM8  IMMMMMMMMZ  MMMMMMMM   8MMMMMMMMMM
;MMMMMMMN     MM?    ?MMMMMMMMMMMMMM=   MMMMMMMMMMMM    MMMMM   $MMMMMMMMMMMMMM  MMM   7MMMMMMMMM  MMMMMMD   MMMMMMMMMMMM
;MMMMMM     MMMMMM     MMMMMMMMMMMMM=   MMMMMMMMMMMMM    MMM$   MMMMMMMMMMMMMMMMMMMM    NMMMMMMMMMMMMMMM+   MMMMMMMMMMMMM
;MMMMN     MMMMMMMM?    +MMMMMMMMMMM=   MMMMMMMMMMMMM7   MMM   ZMMMMMMMMMMMMMMMMMMMMO    ~MMMMMMMMMMMMMM   MMMMMMMMMMMMMM
;MMM     MMMMMMMMMMMM     MMMMMMMMMM=   MMMMMMMMMMMMMM   ZM+   MMMMMMMMMMMMMMMMMMMMMMM      7MMMMMMMMMM    7       =MMMMM
;MO    7MMMMM   OMMMMM?    $MMMMMMMM=   MMMMMMMMMMMMMM   ?M    MMMMMMMMMMMMMMMMMMMMMMMM:        MMMMMM+    +7MM7     DMMM
;M?   MMMMMM     ?MMMMMM   OMMMMMMMM=   MMMMMMMMMMMMMM   ?M    MMMMMMMMMMMMMMMMMMMMMMMMMMM?       DMMM    MMMMMMMM    :MM
;MMMMMMMMM=        MMMMMMMMMMMMMMMMM=   MMMMMMMMMMMMMM   OM=   ZMMMMMMMMMMMMMMMMMMMMMMMMMMMMM$     $MM    MMMMMMMMM    MM
;MMMMMMMN     MM    .MMMMMMMMMMMMMMM=   MMMMMMMMMMMMMM   MMM    MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMD    MM    MMMMMMMMM7   MM
;MMMMMM     MMMMM     MMMMMMMMMMMMMM=   MMMMMMMMMMMMM$   MMM7   MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMZ   7M    MMMMMMMMMD   MM
;MMMMN     MMMMMMMM    .MMMMMMMMMMMM=   MMMMMMMMMMMMM   MMMMM    NMMMMMMMMMMMMM  MMM: 7MMMMMMMMMM   8M,   NMMMMMMMMO   MM
;MMM     MMMMMMMMMMM?    +MMMMMMMMMM=   MMMMMMMMMMMM   NMMMMMM    DMMMMMMMMMMM8  MMM:  MMMMMMMMMN  ,MMM    MMMMMMMM    MM
;M+    7MMMMMMMMMMMMMM    .MMMMMMMMM=   MMMMMMMMMN   +MMMMMMMMMN    MMMMMMMMMM:  MMM:  MMMMMMMMM   MMMMM   ~MMMMMM:  +MMM
;M    MMMMMMMMMMMMMMMMM?   MMMMMZ                  MMMMMMMMMMMMMMM     7MMMZI    MMM:   =NMM7,   MMMMMMMMO   :8M?   MMMMM
;MM?7MMMMMMMMMMMMMMMMMMMM8MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM7    :7MMMMMMMMMMZ7   77MMMMMMMMMMMMMM$   7MMMMMMMM
;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
;*	                            Version 6.2.0
;*	                              TI-83+ Edition
;*	                              By Kerm Martian
;*	                           admin@cemetech.net
;*	                           http://www.Cemetech.net
;************************************************************************************************************************
;*                                 Build 6.3.0 beta
;*                                 Compiled May 25, 2008
;****************************************************************************************************

;-----------------------------------------------------------
;	Filename:		dcs6.asm
;	Long name:  	Doors CS 6 Program Core
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	June 2, 2006
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

.exportmode Assembly \ .export
.binarymode ti8xapp                 ; TI-83+ Application
.variablename "DoorsCS7"            ; Application name (shown in 'Apps' menu)

;#DEFINE APP
;#DEFINE APPNAME "DoorsCS6"
;#DEFINE APPPAGES 3
#DEFINE NOAPPHEADER
#DEFINE NOEND
#DEFINE TI83P
#DEFINE NOTOKENS

#INCLUDE "newdefs.inc"
#include "tokens.inc"
#INCLUDE "header.asm"
#INCLUDE "notes.inc"
;#INCLUDE "dcs6.inc"
.defpage 0, 16*1024, $4000          ; Page 0 definition
.defpage 1, 16*1024, $4000          ; Page 1 definition
.defpage 2, 16*1024, $4000          ; Page 2 definition

.page 0                             ; Start page 0
Page0Start:
                                    ; ? header is added in here for us
    .block 128                      ; Advance 128 bytes for header

FileStart:
ASMStart:
	jp RealStart

;--------------------------
#include "vectors.asm"				;Page 0 vector and branch tables
;--------------------------
#include "ionlibsp0.inc"			;Page 0 Ion libraries
#include "dcslibsp.inc"				;Doors CS libraries
#include "fonts.inc"				;Font-related functions
#include "basicprg.inc"				;Basic program execution
#include "detecttype.inc"			;Program type detection functions
#include "vatfind.asm"				;VFAT construction routine
#include "runprog.asm"				;Full runprog routines
#include "progchain.asm"			;Chaining routines like push and pop
#ifdef enableCn2
#include "calcnet2.asm"				;CalcNet linking protocol
#endif
#ifdef alphasort
#include "sortalph.inc"				;Alpha program sorting
#endif
#include "ap.asm"					;Associated program functions
#include "easteregg.asm"			;Credits screen
#ifdef USBDrivers
#include "usbdrivers.asm"			;USB drivers?
#endif
#include "mouse_p0.asm"				;Page 0 Desktop mouse functions
#include "fldsv.asm"				;Folder backup and restoration
;--------------------------

;RealStart: is here:
#include "main.asm"				;here's where all the page 0 magic happens!

ResetAppPage0a:
	pop af
	pop af
	pop af
ResetAppPage0:
	ret
Page0End:
.echoln "Page 0 is ",Page0End-Page0Start," bytes long (",16384-Page0End+Page0Start," bytes to spare)"

.page 1
Page1Start:
ResetAppPage1:
	bcall(_ResetAppPage0a)
#include "vectorsp2.asm"
#ifdef mossupport
#include "moslibs.asm"
#endif
#include "ionlibsp1.inc"
#include "guitools.inc"
#include "guimouse.asm"
#include "mouse_p1.asm"
#include "startmenu.inc"
#include "cedit.asm"
#include "apguis.asm"
#include "propstring.asm"
#include "datap1.inc"
Page1End:

.echoln "Page 1 is ",Page1End-Page1Start," bytes long (",16384-Page1End+Page1Start," bytes to spare)"

.page 2
Page2Start:
ResetAppPage2:
	bcall(_ResetAppPage0a)
#include "vectorsp3.asm"
#include "ionlibsp2.inc"
#include "parserhook.inc"
#ifdef celtic3support
#include "c3_hook.asm"
#include "c3_std.asm"
#include "c3_util.asm"
#include "c3_xfn.asm"
#include "c3_pfn.asm"
#include "c3_cfn.asm"
#endif

;#include "celtic3src/_HKH.z80"  ;hook handler
;#include "c3_std.asm"  ;subroutines used frequently
;#include "celtic3src/_UTL.z80"  ;utility file
;#include "celtic3src/_CFN.z80"  ;C3 fn
;#include "celtic3src/_PFN.z80"  ;FA fn
;#include "celtic3src/_XFN.z80"  ;XL fn
;#include "celtic3src/_UFN.z80"  ;Unused

#include "datap2.inc"
Page2End:

.echoln "Page 2 is ",Page2End-Page2Start," bytes long (",16384-Page2End+Page2Start," bytes to spare)"
FileEnd:
.end
END