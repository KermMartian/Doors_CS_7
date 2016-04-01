
.block 128

	jp imVersion
	jp imRandom
	jp imPutSprite
	jp imLargeSprite
	jp imGetPixel
	jp imFastCopy
	jp mj_iDetect
	jp imDecompress
#ifdef mossupport
moslibs2:
	jp mos_directIn
	jp mos_sendbytetios
	jp mos_getbytetios
	jp mos_version
	jp mos_setvputs
	jp mos_setpixel
	jp mos_FastcopyS
	jp mos_delayb
	jp mos_multhe
	jp mos_multhl
	jp mos_QuitToShell
	jp mos_fastline
	jp mos_pixelonhl
	jp mos_pixeloff
	jp mos_pixelxor
	jp mos_pixeltest_
	jp mos_pixeloffhl
	jp mos_pixelxorhl 
	jp mos_pixeltesthl
	jp mos_fastlineb
	jp mos_fastlinew
	jp mos_fastlinex
	jp mos_pointonc
	jp mos_pointoffc
	jp mos_pointxorc
	jp mos_centertext
	jp mos_cphlbc
	jp mos_putsprite8
	jp mos_fastcopyb
	jp mos_vputsc
	jp mos_scrolld7
	jp mos_vnewline
	jp mos_rand127
	jp mos_disprle
	jp mos_cphlde
	jp NotImplemented  ;not used
	jp mos_fastlined
	jp mos_disprlel
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp mos_getnext  ;not used
	jp NotImplemented  ;not used
	jp mos_compstrs
	jp mos_nextstr
	jp NotImplemented  ;not used
	jp mos_fastrectangle
	jp mos_gettext
	jp mos_gettextv
	jp mos_fastrectangle_save
	jp mos_vputa
	jp dRunProg
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp mos_Filledrectangle
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp mos_compstrsn
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp mos_Filledrectangle_Save
	jp mos_getbytetiosw
	jp NotImplemented  ;not used - VATSwap
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp mos_setupint
	jp NotImplemented  ;not used
	jp mos_largespritehl
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
	jp NotImplemented  ;not used
#endif
dcsLibAdditions2:							;old routines from 4.0-5.0 included for compatibility
	jp dSmall_Window
	jp dClrDialogFull
	jp dLargeWindow
	jp dClrWinFull
	jp dPlaySound
	jp dVDispHL
	jp dPause
	jp dhDetect
LibTableGUI2:								;new routines from 6.0
	jp OpenGUIStack
	jp CloseGUIStack
	jp PushGUIStack
	jp PopGUIStack
	jp RenderGUI
	jp PopGUIStacks
	jp GUIMouseRAM
	jp GUIFindFirst
	jp GUIFindNext
Cn2Libs2:
#ifdef enableCn2
	jp Cn2j_Setup
	jp Cn2j_ClearSend
	jp Cn2j_ClearRec
	jp Cn2j_Setdown
;	jp Cn2j_Int_Start
#else
	jp NotImplemented
	jp NotImplemented
	jp NotImplemented
	jp NotImplemented
#endif
APLibs2:
	jp FileOpen
	jp FileSave
	jp FileSaveAs
	
	jp dDispLongInt
	jp Cn2GetK
	jp dDPutMap
	jp mj_APGui_gui7ToTop

dSmall_Window:	bcall(_Small_Window) \ ret
dClrDialogFull:	bcall(_ClrDialogFull) \ ret
dLargeWindow:	bcall(_LargeWindow) \ ret
dClrWinFull:	bcall(_ClrWinFull) \ ret
dPlaySound:		bcall(_PlaySound) \ ret
dvDispHL:		bcall(_vdisphl) \ ret
dPause:			bcall(_Pause) \ ret
dhdetect:		bcall(_hdetect) \ ret
dDispLongInt:	bcall(_DispLongInt) \ ret
dDPutMap:		bcall(_DPutMap) \ ret
mj_APGui_gui7ToTop:		bcall(_APGui_gui7ToTop) \ ret
mj_iDetect:					bcall(_iDetect) \ ret
dRunProg:		bcall(_mj_Runprog) \ ret

#ifdef enableCn2
Cn2j_Setup:					bcall(_Cn2_Setup) \ ret
Cn2j_Setdown:				bcall(_Cn2_Setdown) \ ret
Cn2j_ClearSend:				bcall(_Cn2_Clear_SendBuf) \ ret
Cn2j_ClearRec:				bcall(_Cn2_Clear_RecBuf) \ ret
#endif

