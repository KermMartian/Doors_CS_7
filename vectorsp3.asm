;.block 128

immRoutineJumpTable:
	jp immVersion
	jp immRandom
	jp immPutSprite
	jp immLargeSprite
	jp immGetPixel
	jp immFastCopy
	jp immDetect
	jp immDecompress
#ifdef false
#ifdef mossupport
mmoslibs:
mdirectin:                jp mmj_directIn
msendbytetios:            jp mmj_sendbytetios
mgetbytetios:             jp mmj_getbytetios
mversion:                 jp mmj_version
msetvputs:                jp mmj_setvputs
msetpixel:                jp mmj_setpixel
mfastcopys:               jp mmj_FastcopyS
mdelayb:                  jp mmj_delayb
mmulthe:                  jp mmj_multhe
mmulthl:                  jp mmj_multhl
mquittoshell:             jp mmj_QuitToShell
mfastline:                jp mmj_fastline
mpixelonhl:               jp mmj_pixelonhl
mpixeloff:                jp mmj_pixeloff
mpixelxor:                jp mmj_pixelxor
mpixeltest:               jp mmj_pixeltest
mpixeloffhl:              jp mmj_pixeloffhl
mpixelxorhl:              jp mmj_pixelxorhl 
mpixeltesthl:             jp mmj_pixeltesthl
mfastlineb:               jp mmj_fastlineb
mfastlinew:               jp mmj_fastlinew
mfastlinex:               jp mmj_fastlinex
mpointonc:                jp mmj_pointonc
mpointoffc:               jp mmj_pointoffc
mpointxorc:               jp mmj_pointxorc
mcentertext:              jp mmj_centertext
mcphlbc:                  jp mmj_cphlbc
mputsprite8:              jp mmj_putsprite8
mfastcopyb:               jp mmj_fastcopyb
mvputsc:                  jp mmj_vputsc
mscrolld7:                jp mmj_scrolld7
mvnewline:                jp mmj_vnewline
mrand127:                 jp mmj_rand127
mdisprle:                 jp mmj_disprle
mcphlde:                  jp mmj_cphlde
mscreentopic:             jp mmj_NotImplemented  ;not used
mfastlined:               jp mmj_fastlined
mdisprlel:                jp mmj_disprlel
mgetnextgoodprog:         jp mmj_NotImplemented  ;not used
mgetprevgoodprog:         jp mmj_NotImplemented  ;not used
mgetnext:                 jp mmj_getnext  ;not used
mgetprev:                 jp mmj_NotImplemented  ;not used
mcompstrs:                jp mmj_compstrs
mnextstr:                 jp mmj_nextstr
mgetinfo:                 jp mmj_NotImplemented  ;not used
mfastrectangle:           jp mmj_fastrectangle
mgettext:                 jp mmj_gettext
mgettextv:                jp mmj_gettextv
mFastRectangle_Save:      jp mmj_fastrectangle_save
mvputa:                   jp mmj_vputa
mmrunprog:                jp mmj_runprog  ;not used
misgoodprog:              jp mmj_NotImplemented  ;not used
mexistfold:               jp mmj_NotImplemented  ;not used
mdelprog:                 jp mmj_NotImplemented  ;not used
mfilledrectangle:         jp mmj_Filledrectangle
mnextfolder:              jp mmj_NotImplemented  ;not used
mdelfolder:               jp mmj_NotImplemented  ;not used
mmoveall:                 jp mmj_NotImplemented  ;not used
mcurFoldName:             jp mmj_NotImplemented  ;not used
mcurfoldnamea:            jp mmj_NotImplemented  ;not used
mcreatefolder:            jp mmj_NotImplemented  ;not used
mcompstrsn:               jp mmj_compstrsn
mfolder_menu_start:       jp mmj_NotImplemented  ;not used
moptions_screen:          jp mmj_NotImplemented  ;not used
mput_folder_name_top:     jp mmj_NotImplemented  ;not used
mgeneral_key_routine:     jp mmj_NotImplemented  ;not used
mfind_num_good_progs:     jp mmj_NotImplemented  ;not used
mput_scrollbar:           jp mmj_NotImplemented  ;not used
minvert_lines:            jp mmj_NotImplemented  ;not used
minvert_1_line:           jp mmj_NotImplemented  ;not used
mright_align_value:       jp mmj_NotImplemented  ;not used
mput_mirageos_header:     jp mmj_NotImplemented  ;not used
mput_size_graphic:        jp mmj_NotImplemented  ;not used
msendprog:                jp mmj_NotImplemented  ;not used
mhideprog:                jp mmj_NotImplemented  ;not used
marcprog:                 jp mmj_NotImplemented  ;not used
mfilledrectangle_save:    jp mmj_Filledrectangle_Save
mgetbytetiosw:            jp mmj_getbytetiosw
mvatswap:                 jp mmj_NotImplemented  ;not used
mrenameprog:              jp mmj_NotImplemented  ;not used
mrenamefolder:            jp mmj_NotImplemented  ;not used
msysmain:                 jp mmj_NotImplemented  ;not used
msetupint:                jp mmj_setupint
mmove_gui_prog:           jp mmj_NotImplemented  ;not used
mlargespritehl:           jp mmj_largespritehl
mUpdate_Scrollbar:        jp mmj_NotImplemented  ;not used
mInitial_Scrollbar:       jp mmj_NotImplemented  ;not used
msortallfolds:            jp mmj_NotImplemented  ;not used
mdofoldsort:              jp mmj_NotImplemented  ;not used
mgetfoldsort:             jp mmj_NotImplemented  ;not used
msetfoldsort:             jp mmj_NotImplemented  ;not used
mIncrease_Cur_Element:    jp mmj_NotImplemented  ;not used
mDecrease_Cur_Element:    jp mmj_NotImplemented  ;not used
mIncrease_Max_Elements:   jp mmj_NotImplemented  ;not used
mDecrease_Max_Elements:   jp mmj_NotImplemented  ;not used
mAdd_A_To_Cur_Element:    jp mmj_NotImplemented  ;not used
mSub_A_From_Cur_Element:  jp mmj_NotImplemented  ;not used
mAdd_A_To_Max_Elements:   jp mmj_NotImplemented  ;not used
mSub_A_From_Max_Elements: jp mmj_NotImplemented  ;not used
mSkip_Forward_B_From_Top: jp mmj_NotImplemented  ;not used
mGet_Curgoodprog_Ptr:     jp mmj_NotImplemented  ;not used
mgetchecksum:             jp mmj_NotImplemented  ;not used
mfreearc:                 jp mmj_NotImplemented  ;not used
mswapram:                 jp mmj_NotImplemented  ;not used
mhideall:                 jp mmj_NotImplemented  ;not used
#endif
mdcsLibAdditions:							;old routines from 4.0-5.0 included for compatibility
	jp mmj_Small_Window
	jp mmj_ClrDialogFull
	jp mmj_LargeWindow
	jp mmj_ClrWinFull
	jp mmj_PlaySound
	jp mmj_VDispHL
	jp mmj_Pause
	jp mmj_hDetect
mLibTableGUI:								;new routines from 6.0
	jp mmj_VOpenGUIStack
	jp mmj_VCloseGUIStack
	jp mmj_VPushGUIStack
	jp mmj_VPopGUIStack
	jp mmj_VRenderGUI
	jp mmj_VPopGUIStacks
	jp mmj_VGUIMouseRAM
	jp mmj_VGUIFindFirst
	jp mmj_VGUIFindNext
#ifdef enableCn2
mCn2Libs:
	jp Cn2mmj_Setup
	jp Cn2mmj_Clear_SendBuf
	jp Cn2mmj_Clear_RecBuf
	jp Cn2mmj_Setdown
;	jp Cn2j_Int_Start
#else
	jp mmj_NotImplemented
	jp mmj_NotImplemented
	jp mmj_NotImplemented
	jp mmj_NotImplemented
#endif
mAPLibs:
	jp mmj_FileOpen
	jp mmj_FileSave
	jp mmj_FileSaveAs
	
	jp mmj_DispLongInt
	jp mmj_Cn2GetK
	jp mmj_DPutMap
	jp mmj_APGui_gui7ToTop

#ifdef mossupport
mmj_getnext:			bcall(_mos_getnext) \ ret
;mmj_vatswap:			bcall(_mos_vatswap) \ ret
mmj_directIn:			bcall(_mos_directin) \ ret
mmj_sendbytetios:        bcall(_mos_sendbytetios) \ ret
mmj_getbytetios:             bcall(_mos_getbytetios) \ ret
mmj_version:                 bcall(_mos_version) \ ret
mmj_setvputs:                bcall(_mos_setvputs) \ ret
mmj_setpixel:                bcall(_mos_setpixel) \ ret
mmj_fastcopys:               bcall(_mos_FastcopyS) \ ret
mmj_delayb:                  bcall(_mos_delayb) \ ret
mmj_multhe:                  bcall(_mos_multhe) \ ret
mmj_multhl:                  bcall(_mos_multhl) \ ret
mmj_quittoshell:             bcall(_mos_QuitToShell) \ ret
mmj_fastline:                bcall(_mos_fastline) \ ret
mmj_pixelonhl:               bcall(_mos_pixelonhl) \ ret
mmj_pixeloff:                bcall(_mos_pixeloff) \ ret
mmj_pixelxor:                bcall(_mos_pixelxor) \ ret
mmj_pixeltest:               bcall(_mos_pixeltest_) \ ret
mmj_pixeloffhl:              bcall(_mos_pixeloffhl) \ ret
mmj_pixelxorhl:              bcall(_mos_pixelxorhl ) \ ret
mmj_pixeltesthl:             bcall(_mos_pixeltesthl) \ ret
mmj_fastlineb:               bcall(_mos_fastlineb) \ ret
mmj_fastlinew:               bcall(_mos_fastlinew) \ ret
mmj_fastlinex:               bcall(_mos_fastlinex) \ ret
mmj_pointonc:                bcall(_mos_pointonc) \ ret
mmj_pointoffc:               bcall(_mos_pointoffc) \ ret
mmj_pointxorc:               bcall(_mos_pointxorc) \ ret
mmj_centertext:              bcall(_mos_centertext) \ ret
mmj_cphlbc:                  bcall(_mos_cphlbc) \ ret
mmj_putsprite8:              bcall(_mos_putsprite8) \ ret
mmj_fastcopyb:               bcall(_mos_fastcopyb) \ ret
mmj_vputsc:                  bcall(_mos_vputsc) \ ret
mmj_scrolld7:                bcall(_mos_scrolld7) \ ret
mmj_vnewline:                bcall(_mos_vnewline) \ ret
mmj_rand127:                 bcall(_mos_rand127) \ ret
mmj_disprle:                 bcall(_mos_disprle) \ ret
mmj_cphlde:                  bcall(_mos_cphlde) \ ret
mmj_fastlined:               bcall(_mos_fastlined) \ ret
mmj_disprlel:                bcall(_mos_disprlel) \ ret
mmj_NotImplemented:          bcall(_NotImplemented) \ ret  ;not used
mmj_compstrs:                bcall(_mos_compstrs) \ ret
mmj_nextstr:                 bcall(_mos_nextstr) \ ret
mmj_fastrectangle:           bcall(_mos_fastrectangle) \ ret
mmj_gettext:                 bcall(_mos_gettext) \ ret
mmj_gettextv:                bcall(_mos_gettextv) \ ret
mmj_FastRectangle_Save:      bcall(_mos_fastrectangle_save) \ ret
mmj_vputa:                   bcall(_mos_vputa) \ ret
mmj_filledrectangle:         bcall(_mos_Filledrectangle) \ ret
mmj_compstrsn:               bcall(_mos_compstrsn) \ ret
mmj_filledrectangle_save:    bcall(_mos_Filledrectangle_Save) \ ret
mmj_getbytetiosw:            bcall(_mos_getbytetiosw) \ ret
mmj_setupint:                bcall(_mos_setupint) \ ret
mmj_largespritehl:           bcall(_mos_largespritehl) \ ret
mmj_VOpenGUIStack:				bcall(_OpenGUIStack) \ ret
mmj_VCloseGUIStack:				bcall(_CloseGUIStack) \ ret
mmj_VPushGUIStack:				bcall(_PushGUIStack) \ ret
mmj_VPopGUIStack:				bcall(_PopGUIStack) \ ret
mmj_VRenderGUI:					bcall(_RenderGUI) \ ret
mmj_VPopGUIStacks:				bcall(_PopGUIStacks) \ ret
mmj_VGUIMouseRAM:				bcall(_GUIMouseRAM) \ ret
mmj_VGUIFindFirst:				bcall(_GUIFindFirst) \ ret
mmj_VGUIFindNext:				bcall(_GUIFindNext) \ ret
mmj_FileOpen:				bcall(_FileOpen) \ ret
mmj_FileSave:				bcall(_FileSave) \ ret
mmj_FileSaveAs:				bcall(_FileSaveAs) \ ret
mmj_Cn2GetK:					bcall(_Cn2GetK) \ ret
#endif

#ifdef enableCn2
Cn2mmj_Setup:				bcall(_Cn2_Setup) \ ret
Cn2mmj_Clear_SendBuf:		bcall(_Cn2_Clear_SendBuf) \ ret
Cn2mmj_Clear_RecBuf:		bcall(_Cn2_Clear_RecBuf) \ ret
Cn2mmj_Setdown:				bcall(_Cn2_Setdown) \ ret
#endif

mmj_Small_Window:	bcall(_Small_Window) \ ret
mmj_ClrDialogFull:	bcall(_ClrDialogFull) \ ret
mmj_LargeWindow:	bcall(_LargeWindow) \ ret
mmj_ClrWinFull:	bcall(_ClrWinFull) \ ret
mmj_PlaySound:		bcall(_PlaySound) \ ret
mmj_vDispHL:		bcall(_vdisphl) \ ret
mmj_Pause:			bcall(_Pause) \ ret
mmj_hdetect:		bcall(_hdetect) \ ret
mmj_DispLongInt:			bcall(_DispLongInt) \ ret
mmj_DPutMap:				bcall(_DPutMap) \ ret
mmj_APGui_gui7ToTop:		bcall(_APGui_gui7ToTop) \ ret
mmj_RunProg:		bcall(_mj_Runprog) \ ret
#endif
