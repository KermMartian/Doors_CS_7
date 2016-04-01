.nolist
USBDriverCall = AppBackupScreen+96
USBDriverBuf  = AppBackupScreen+128
myvars = AppBackupScreen+256
savesscreen	= $86ec	;reserved for the FAT buffer
#include "usb8x.inc"
.list
USBInitialize:
	in a,(2)
	ld b,a
	and 80h
	jp z,USBTI83p
	in a,(21h)
	and 3
	jr z,USBTI84p
	bit 5,b
	jp z,USBTI83p
USBTI84p:
	U_CALL_INIT(Stub)
	jp c,USBTI83p			;USBDRV8x not present
	in a,(6)
	ld (USBPage),a
	U_CALL(GetVersion)
	ld de,12
	or a		
	sbc hl,de
	jr c,USBTI83p			;old version of USBDRV8x
	ld hl,USBDriverBuf
	;call quit	;good through here
	U_CALL(DriverInit)
	jr c,USBTI83p
	ld hl,savesscreen
	ld de,savesscreen+256
	U_CALL(MSD_Initialize)
	jp c,USBTI83p
	U_CALL(UFI_Initialize)
	jp c,USBTI83p
	U_CALL(FAT_Initialize)
	jp c,USBTI83p

	U_CALL(HostKill)
	U_CALL(DriverKill)
USBTI83p:					;we dont want 83+s
	ret
Stub:
	ret	