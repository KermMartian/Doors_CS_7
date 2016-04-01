;************************************************************************************************************************
;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM7    7MMMMMMMMMMMMM7   +7MMMMMMMMMMMMMMMMMM77 MMMM
;MMMMMMMMMMMM,   MMMMMMMMMMMMMMMZ                  MMMMMMMMMMMMMMM=    7MMM7     MMMMMM    NMMI    MMMMMMMMMMMM 777 7MMMM
;MMMMMMMMMMM      ?MMMMMMMMMMMMMMMMM=   MMMMMMMMM    +MMMMMMMMMM   NMMMMMMMMMM,  MMMMN  7MMMMMMM,  MMMMMMMMMZ 7 MMMMMMMMM
;MMMMMMMMM=         MMMMMMMMMMMMMMMM=   MMMMMMMMMMM    MMMMMMM    MMMMMMMMMMMMO  MMM8  IMMMMMMMMZ  MMMMMMMM 7 8MMMMMMMMMM
;MMMMMMMN     MM?    ?MMMMMMMMMMMMMM=   MMMMMMMMMMMM    MMMMM   $MMMMMMMMMMMMMM  MMM   7MMMMMMMMM  MMMMMMD 7 MMMMMMMMMMMM
;MMMMMM     MMMMMM     MMMMMMMMMMMMM=   MMMMMMMMMMMMM    MMM$   MMMMMMMMMMMMMMMMMMMM    NMMMMMMMMMMMMMMM+ 7 MMMMMMMMMMMMM
;MMMMN     MMMMMMMM?    +MMMMMMMMMMM=   MMMMMMMMMMMMM7   MMM   ZMMMMMMMMMMMMMMMMMMMMO    ~MMMMMMMMMMMMMM 7 MMMMMMMMMMMMMM
;MMM     MMMMMMMMMMMM     MMMMMMMMMM=   MMMMMMMMMMMMMM   ZM+   MMMMMMMMMMMMMMMMMMMMMMM      7MMMMMMMMMM 7777777777 =MMMMM
;MO    7MMMMM   OMMMMM?    $MMMMMMMM=   MMMMMMMMMMMMMM   ?M    MMMMMMMMMMMMMMMMMMMMMMMM:        MMMMMM+ 7  +7MM7   7 DMMM
;M?   MMMMMM     ?MMMMMM   OMMMMMMMM=   MMMMMMMMMMMMMM   ?M    MMMMMMMMMMMMMMMMMMMMMMMMMMM?       DMMM  7 MMMMMMMM  7 :MM
;MMMMMMMMM=        MMMMMMMMMMMMMMMMM=   MMMMMMMMMMMMMM   OM=   ZMMMMMMMMMMMMMMMMMMMMMMMMMMMMM$     $MM  7 MMMMMMMMM  7 MM
;MMMMMMMN     MM    .MMMMMMMMMMMMMMM=   MMMMMMMMMMMMMM   MMM    MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMD    MM  7 MMMMMMMMM7 7 MM
;MMMMMM     MMMMM     MMMMMMMMMMMMMM=   MMMMMMMMMMMMM$   MMM7   MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMZ   7M  7 MMMMMMMMMD 7 MM
;MMMMN     MMMMMMMM    .MMMMMMMMMMMM=   MMMMMMMMMMMMM   MMMMM    NMMMMMMMMMMMMM  MMM: 7MMMMMMMMMM   8M, 7 NMMMMMMMMO 7 MM
;MMM     MMMMMMMMMMM?    +MMMMMMMMMM=   MMMMMMMMMMMM   NMMMMMM    DMMMMMMMMMMM8  MMM:  MMMMMMMMMN  ,MMM  7 MMMMMMMM 7  MM
;M+    7MMMMMMMMMMMMMM    .MMMMMMMMM=   MMMMMMMMMN   +MMMMMMMMMN    MMMMMMMMMM:  MMM:  MMMMMMMMM   MMMMM 7 ~MMMMMM:7 +MMM
;M    MMMMMMMMMMMMMMMMM?   MMMMMZ                  MMMMMMMMMMMMMMM     7MMMZI    MMM:   =NMM7,   MMMMMMMMO 7 :8M? 7 MMMMM
;MM?7MMMMMMMMMMMMMMMMMMMM8MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM7    :7MMMMMMMMMMZ7   77MMMMMMMMMMMMMM$   7MMMMMMMM
;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
;*	                              By Kerm Martian
;*	                           admin@cemetech.net
;*	                           http://www.Cemetech.net
;************************************************************************************************************************
;*                                 Build 7.3
;*                                 Compiled July 14, 2014
;************************************************************************************************************************

;-----------------------------------------------------------
;	Filename:		dcs7.asm
;	Long name:  	Doors CS 7 Program Core
;	Author:			Kerm Martian aka Christopher Mitchell
;	Last Update:	November 13, 2012
;
;Please consult license.txt for the fair use agreement
;implicitly binding by you continuing to read past this
;point.  Close and delete this file if you do not agree
;to the terms of the agreement.
;-----------------------------------------------------------

;.exportmode Assembly \ .export
.binarymode intel                 ; TI-83+ Application

#DEFINE NOAPPHEADER
#DEFINE NOEND
#DEFINE TI83P
#DEFINE NOTOKENS
.nolist
#INCLUDE "newdefs.inc"
#include "tokens.inc"
#INCLUDE "header.asm"
#INCLUDE "notes.inc"
;#INCLUDE "dcs6.inc"
.list

.defpage 0, 16*1024, $4000          ; Page 0 definition
.defpage 1, 16*1024, $4000          ; Page 1 definition
.defpage 2, 16*1024, $4000          ; Page 2 definition

.page 0                             ; Start page 0
Page0Start:
.echoln "-Page 0----------------------"
	; Master Field
	.db	80h, 0Fh, 0, 0, 0, 0
	; Signing Key ID
	.db	80h, 12h, 1, 4 ; or 15 for the TI-84+CSE
	;revision
	.db 80h,21h,7   ; 7 
	.db 80h,31h,35  ;   3 b 3
	; Name
	.db	80h, 48h, "DoorsCS7"
	; Disable TI splash screen.
	.db	80h, 90h
	; Pages
	.db	80h, 81h, 3
	; Date stamp.  Apparently, the calculator doesn't mind if you put
	; nothing in this.
	.db	03h, 22h, 09h, 00h
	; Date stamp signature.  Since nothing ever checks this, there's no
	; reason ever to update it.  Or even have data in it.
	.db	02h, 00
	; Final field
	.db	80h, 70h              
FileStart:

ASMStart:
	jp RealStart    

;Extra routines go here in the header padding. should move some stuff here later.
#include "header_routs.inc"
 
.echoln "    > Page 0 header space free: ",3+128-($-Page0Start)," bytes"
.fill 3+128-($-Page0Start), $FF

;--------------------------
page0_sub0:
#include "vectors.asm"				;Page 0 vector and branch tables
.echoln "    > Page 0: vectors.asm is    ",$-page0_sub0," bytes long"
;--------------------------
page0_sub1:
#include "ionlibsp0.inc"			;Page 0 Ion libraries
.echoln "    > Page 0: ionlibsp0.inc is  ",$-page0_sub1," bytes long"
;--------------------------
page0_sub2:
#include "dcslibsp.inc"				;Doors CS libraries
.echoln "    > Page 0: dcslibsp.inc is   ",$-page0_sub2," bytes long"
;--------------------------
page0_sub3:
#include "fonts.inc"				;Font-related functions
.echoln "    > Page 0: fonts.inc is      ",$-page0_sub3," bytes long"
;--------------------------
page0_sub4:
#include "basicprg.inc"				;Basic program execution
.echoln "    > Page 0: basicprg.inc is   ",$-page0_sub4," bytes long"
;--------------------------
page0_sub5:
#include "detecttype.inc"			;Program type detection functions
.echoln "    > Page 0: detecttype.inc is ",$-page0_sub5," bytes long"
;--------------------------
page0_sub6:
#include "vatfind.asm"				;VFAT construction routine
.echoln "    > Page 0: vatfind.asm is    ",$-page0_sub6," bytes long"
;--------------------------
page0_sub7:
#include "runprog.asm"				;Full runprog routines
.echoln "    > Page 0: runprog.asm is    ",$-page0_sub7," bytes long"
;--------------------------
page0_sub8:
#include "progchain.asm"			;Chaining routines like push and pop
.echoln "    > Page 0: progchain.asm is  ",$-page0_sub8," bytes long"
;--------------------------
#ifdef enableCn22
page0_sub9:
#include "calcnet22.asm"			;CalcNet linking protocol
.echoln "    > Page 0: calcnet22.asm is   ",$-page0_sub9," bytes long"
#endif
;--------------------------
page0_sub11:
#include "ap.asm"					;Associated program functions
.echoln "    > Page 0: ap.asm is         ",$-page0_sub11," bytes long"
;--------------------------
#ifdef easteregg
page0_sub12:
#include "easteregg.asm"			;Credits screen
.echoln "    > Page 0: easteregg.asm is  ",$-page0_sub12," bytes long"
#endif
;--------------------------
#ifdef USBDrivers
page0_sub13:
#include "usbdrivers.asm"			;USB drivers?
.echoln "    > Page 0: usbdrivers.asm is ",$-page0_sub13," bytes long"
#endif
;--------------------------
page0_sub14:
#include "mouse_p0.asm"				;Page 0 Desktop mouse functions
.echoln "    > Page 0: mouse_p0.asm is   ",$-page0_sub14," bytes long"
;--------------------------
page0_sub15:
#include "fldsv.asm"				;Folder backup and restoration
.echoln "    > Page 0: fldsv.asm is      ",$-page0_sub15," bytes long"
;--------------------------
;RealStart: is here:
page0_sub16:
#include "main.asm"				;here's where all the page 0 magic happens!

ResetAppPage0a:
	pop af
	pop af
	pop af
ResetAppPage0:
	ret
.echoln "    > Page 0: main.asm is       ",$-page0_sub16," bytes long"
;--------------------------

Page0End:

.page 1
Page1Start:
.echoln "-Page 1----------------------"
ResetAppPage1:
	bcall(_ResetAppPage0a)
;--------------------------
#ifdef false
page1_sub0:
#include "vectorsp2.asm"
.echoln "    > Page 1: vectorsp2.asm is  ",$-page1_sub0," bytes long"
#endif
;--------------------------
#ifdef mossupport
page1_sub1:
#include "moslibs.asm"
.echoln "    > Page 1: moslibs.asm is    ",$-page1_sub1," bytes long"
#endif
;--------------------------
page1_sub2:
#include "ionlibsp1.inc"
.echoln "    > Page 1: ionlibsp1.inc is  ",$-page1_sub2," bytes long"
;--------------------------
page1_sub3:
#include "guitools.inc"
.echoln "    > Page 1: guitools.inc is   ",$-page1_sub3," bytes long"
;--------------------------
page1_sub4:
#include "guimouse.asm"
.echoln "    > Page 1: guimouse.asm is   ",$-page1_sub4," bytes long"
;--------------------------
page1_sub5:
#include "mouse_p1.asm"
.echoln "    > Page 1: mouse_p1.asm is   ",$-page1_sub5," bytes long"
;--------------------------
page1_sub6:
#include "startmenu.inc"
.echoln "    > Page 1: startmenu.inc is  ",$-page1_sub6," bytes long"
;--------------------------
page1_sub7:
#include "cedit.asm"
.echoln "    > Page 1: cedit.asm is      ",$-page1_sub7," bytes long"
;--------------------------
page1_sub8:
#include "apguis.asm"
.echoln "    > Page 1: apguis.asm is     ",$-page1_sub8," bytes long"
;--------------------------
page1_sub9:
#include "propstring.asm"
.echoln "    > Page 1: propstring.asm is ",$-page1_sub9," bytes long"
;--------------------------
#ifdef alphasort
page1_sub10:
#include "sortalph.inc"				;Alpha program sorting
.echoln "    > Page 1: sortalph.inc is   ",$-page1_sub10," bytes long"
#endif
;--------------------------
page1_sub11:
#include "datap1.inc"
.echoln "    > Page 1: datap1.inc is     ",$-page1_sub11," bytes long"
;--------------------------
Page1End:

.page 2
Page2Start:
.echoln "-Page 2----------------------"
ResetAppPage2:
	bcall(_ResetAppPage0a)
;--------------------------
page2_sub0:
#include "vectorsp3.asm"
.echoln "    > Page 2: vectorsp3.asm is  ",$-page2_sub0," bytes long"
;--------------------------
page2_sub1:
#include "ionlibsp2.inc"
.echoln "    > Page 2: ionlibsp2.inc is  ",$-page2_sub1," bytes long"
;--------------------------
page2_sub2:
#include "parserhook.inc"
.echoln "    > Page 2: parserhook.inc is ",$-page2_sub2," bytes long"
;--------------------------
#ifdef celtic3support
page2_sub3:
#include "c3_hook.asm"
.echoln "    > Page 2: c3_hook.asm is    ",$-page2_sub3," bytes long"
page2_sub4:
#include "c3_std.asm"
.echoln "    > Page 2: c3_std.asm is     ",$-page2_sub4," bytes long"
page2_sub5:
#include "c3_util.asm"
.echoln "    > Page 2: c3_util.asm is    ",$-page2_sub5," bytes long"
page2_sub6:
#include "c3_xfn.asm"
.echoln "    > Page 2: c3_xfn.asm is     ",$-page2_sub6," bytes long"
page2_sub7:
#include "c3_pfn.asm"
.echoln "    > Page 2: c3_pfn.asm is     ",$-page2_sub7," bytes long"
page2_sub8:
#include "c3_cfn.asm"
.echoln "    > Page 2: c3_cfn.asm is     ",$-page2_sub8," bytes long"
#endif
#ifdef dcsblibs
page2_sub9:
#include "dcsblibs.asm"
.echoln "    > Page 2: dcsblibs.asm is   ",$-page2_sub9," bytes long"
#endif

;--------------------------

#include "datap2.inc"
Page2End:

.echoln "Page 0 is ",Page0End-Page0Start," bytes long (",16384-Page0End+Page0Start," bytes to spare)"
.echoln "Page 1 is ",Page1End-Page1Start," bytes long (",16384-Page1End+Page1Start," bytes to spare)"
.echoln "Page 2 is ",Page2End-Page2Start," bytes long (",16384-Page2End+Page2Start," bytes to spare)"

FileEnd:
.end
END
