usbPipe0      EQU 0000h
usbPipe1      EQU 0100h
usbPipe2      EQU 0200h
usbPipe3      EQU 0300h

usbBulk       EQU 20h
usbInt        EQU 30h

usbPacket8    EQU 0100h
usbPacket16   EQU 0200h
usbPacket32   EQU 0400h
usbPacket64   EQU 0800h

pipeIn        EQU 0
pipeOut       EQU 1

cbNewData EQU 0
cbStall   EQU 1
cbNak     EQU 2

descDevice    EQU 0
descConfig    EQU 1
descString    EQU 2

uGetVersion		EQU 0
uDriverInit		EQU 1
uIsDeviceConnected	EQU 2
uHostInit		EQU 3
uHostKill		EQU 4
uReadDescriptor		EQU 5
uGetIDs			EQU 6
uGetClass		EQU 7
uConfigureDevice	EQU 8
uSetupOutPipe		EQU 9
uSetupInPipe		EQU 10
uFindDescriptor		EQU 11
uSendData		EQU 12
uAutoSetup		EQU 13
uSendControlData	EQU 14
uReqData             EQU 15
uDriverKill          EQU 16
uMouseInit           EQU 17
uMouseGetKey         EQU 18 
uWaitTimerB		EQU 19
uPipeInfo		EQU 20

USBDriverEntryPoint EQU 404Ch


offInBuf             EQU 0                              ;64
offPacketSizeTable   EQU offInBuf+64                    ;8
offCallBackTable     EQU offPacketSizeTable+8           ;16
offErrorCode         EQU offCallBackTable+16            ;2
offMouseFlags        EQU offErrorCode+2                 ;1
offMouseBtn          EQU offMouseFlags+1                ;1
offMouseX            EQU offMouseBtn+1                  ;2
offMouseY            EQU offMouseX+2                    ;2
offMouseSavedData    EQU offMouseY+2                    ;1
offCallBack          EQU 16

mouseRight    EQU 1
mouseLeft     EQU 2
mouseUp       EQU 4
mouseDown     EQU 8

mouseBitRight EQU 0
mouseBitLeft  EQU 1
mouseBitUp    EQU 2
mouseBitDown  EQU 3


errPort82 EQU 1
errPort91Bit2 EQU 2
errPort91Bit4 EQU 3
errPort4C EQU 4
errPort4D EQU 5
errPort3A EQU 6      
errPort81 EQU 7
errPort8C EQU 8
errPort8F EQU 9
errEPIndex EQU 10
errEPGetDes EQU 11
errNoDevice EQU 12
errFindDescriptor EQU 13
errBadClass EQU 14
errTimeout EQU 15
errBadID EQU 16
errReadDescriptor EQU 17
errInit EQU 18
errAutoSetup EQU 19

.deflong U_CALL(label)
#IFNDEF USBDriverCall
#DEFINE USBDriverCall
#ENDIF
 IF label="uAutoSetup"
       ld     de,USBDriverCall+offCallBack
 ENDIF
	ld	a,label
	call	USBDriverCall
.enddeflong

.deflong U_CALL_INIT(label)
#IFNDEF USBDriverCall
#DEFINE USBDriverCall
#ENDIF
       ld     hl,uuAppName
       rst    20h
       B_CALL FindApp
       jr     c,uuDone
       ld     de,USBDriverCall
       ld     hl,uuDriverCode
       ld     bc,uuDriverSize
       ldir
       ld     (USBDriverCall+uuMod1 + 1),a
       in     a,(6)
       ld     (USBDriverCall+uuMod2 +1),a
       ld     (USBDriverCall+uuMod3 +1),a
       or     a
       jr     uuDone
uuAppName:
       db     AppObj,"USBDRV8X",0
uuDriverCode:
       push   af
uuMod1 EQU $ - uuDriverCode
       ld     a,0
       out    (6),a
       pop    af
       call   USBDriverEntryPoint
       push   af
uuMod2 EQU $ - uuDriverCode
       ld     a,0
       out    (6),a
       pop    af
       ret
uuCallBack:
       in     a,(6)
       push   af
uuMod3 EQU $-uuDriverCode
       ld     a,0
       out    (6),a
       call   label
       pop    af
       out    (6),a
       ret
uuDriverSize EQU $ - uuDriverCode
uuDone:


 .enddeflong
 