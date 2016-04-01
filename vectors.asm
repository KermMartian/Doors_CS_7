IonJumpmap:
	jp iVersion
	jp iRandom
	jp iPutSprite
	jp iLargeSprite
	jp iGetPixel
	jp iFastCopy
	jp iDetect
	jp iDecompress
#define mossupport
#ifdef mossupport
moslibs:
directin:                jp mj_directIn
sendbytetios:            jp mj_sendbytetios
getbytetios:             jp mj_getbytetios
version:                 jp mj_version
setvputs:                jp mj_setvputs
setpixel:                jp mj_setpixel
fastcopys:               jp mj_FastcopyS
delayb:                  jp mj_delayb
multhe:                  jp mj_multhe
multhl:                  jp mj_multhl
quittoshell:             jp mj_QuitToShell
fastline:                jp mj_fastline
pixelonhl:               jp mj_pixelonhl
pixeloff:                jp mj_pixeloff
pixelxor:                jp mj_pixelxor
pixeltest:               jp mj_pixeltest
pixeloffhl:              jp mj_pixeloffhl
pixelxorhl:              jp mj_pixelxorhl 
pixeltesthl:             jp mj_pixeltesthl
fastlineb:               jp mj_fastlineb
fastlinew:               jp mj_fastlinew
fastlinex:               jp mj_fastlinex
pointonc:                jp mj_pointonc
pointoffc:               jp mj_pointoffc
pointxorc:               jp mj_pointxorc
centertext:              jp mj_centertext
cphlbc:                  jp mj_cphlbc
putsprite8:              jp mj_putsprite8
fastcopyb:               jp mj_fastcopyb
vputsc:                  jp mj_vputsc
scrolld7:                jp mj_scrolld7
vnewline:                jp mj_vnewline
rand127:                 jp mj_rand127
disprle:                 jp mj_disprle
cphlde:                  jp mj_cphlde
screentopic:             jp mj_NotImplemented  ;not used
fastlined:               jp mj_fastlined
disprlel:                jp mj_disprlel
getnextgoodprog:         jp mj_NotImplemented  ;not used
getprevgoodprog:         jp mj_NotImplemented  ;not used
getnext:                 jp mj_getnext  ;not used
getprev:                 jp mj_NotImplemented  ;not used
compstrs:                jp mj_compstrs
nextstr:                 jp mj_nextstr
getinfo:                 jp mj_NotImplemented  ;not used
fastrectangle:           jp mj_fastrectangle
gettext:                 jp mj_gettext
gettextv:                jp mj_gettextv
FastRectangle_Save:      jp mj_fastrectangle_save
vputa:                   jp mj_vputa
mrunprog:                jp mj_runprog
isgoodprog:              jp mj_NotImplemented  ;not used
existfold:               jp mj_NotImplemented  ;not used
delprog:                 jp mj_NotImplemented  ;not used
filledrectangle:         jp mj_Filledrectangle
nextfolder:              jp mj_NotImplemented  ;not used
delfolder:               jp mj_NotImplemented  ;not used
moveall:                 jp mj_NotImplemented  ;not used
curFoldName:             jp mj_NotImplemented  ;not used
curfoldnamea:            jp mj_NotImplemented  ;not used
createfolder:            jp mj_NotImplemented  ;not used
compstrsn:               jp mj_compstrsn
folder_menu_start:       jp mj_NotImplemented  ;not used
options_screen:          jp mj_NotImplemented  ;not used
put_folder_name_top:     jp mj_NotImplemented  ;not used
general_key_routine:     jp mj_NotImplemented  ;not used
find_num_good_progs:     jp mj_NotImplemented  ;not used
put_scrollbar:           jp mj_NotImplemented  ;not used
invert_lines:            jp mj_NotImplemented  ;not used
invert_1_line:           jp mj_NotImplemented  ;not used
right_align_value:       jp mj_NotImplemented  ;not used
put_mirageos_header:     jp mj_NotImplemented  ;not used
put_size_graphic:        jp mj_NotImplemented  ;not used
sendprog:                jp mj_NotImplemented  ;not used
hideprog:                jp mj_NotImplemented  ;not used
arcprog:                 jp mj_NotImplemented  ;not used
filledrectangle_save:    jp mj_Filledrectangle_Save
getbytetiosw:            jp mj_getbytetiosw
vatswap:                 jp mj_NotImplemented  ;not used
renameprog:              jp mj_NotImplemented  ;not used
renamefolder:            jp mj_NotImplemented  ;not used
sysmain:                 jp mj_NotImplemented  ;not used
setupint:                jp mj_setupint
move_gui_prog:           jp mj_NotImplemented  ;not used
largespritehl:           jp mj_largespritehl
Update_Scrollbar:        jp mj_NotImplemented  ;not used
Initial_Scrollbar:       jp mj_NotImplemented  ;not used
sortallfolds:            jp mj_NotImplemented  ;not used
dofoldsort:              jp mj_NotImplemented  ;not used
getfoldsort:             jp mj_NotImplemented  ;not used
setfoldsort:             jp mj_NotImplemented  ;not used
Increase_Cur_Element:    jp mj_NotImplemented  ;not used
Decrease_Cur_Element:    jp mj_NotImplemented  ;not used
Increase_Max_Elements:   jp mj_NotImplemented  ;not used
Decrease_Max_Elements:   jp mj_NotImplemented  ;not used
Add_A_To_Cur_Element:    jp mj_NotImplemented  ;not used
Sub_A_From_Cur_Element:  jp mj_NotImplemented  ;not used
Add_A_To_Max_Elements:   jp mj_NotImplemented  ;not used
Sub_A_From_Max_Elements: jp mj_NotImplemented  ;not used
Skip_Forward_B_From_Top: jp mj_NotImplemented  ;not used
Get_Curgoodprog_Ptr:     jp mj_NotImplemented  ;not used
getchecksum:             jp mj_NotImplemented  ;not used
freearc:                 jp mj_NotImplemented  ;not used
swapram:                 jp mj_NotImplemented  ;not used
hideall:                 jp mj_NotImplemented  ;not used
#endif
dcsLibAdditions:							;old routines from 4.0-5.0 included for compatibility
	jp Small_Window
	jp ClrDialogFull
	jp LargeWindow
	jp ClrWinFull
	jp PlaySound
	jp VDispHL
	jp Pause
	jp hDetect
LibTableGUI:								;new routines from 6.0
	jp VOpenGUIStack
	jp VCloseGUIStack
	jp VPushGUIStack
	jp VPopGUIStack
	jp VRenderGUI
	jp VPopGUIStacks
	jp VGUIMouseRAM
	jp VGUIFindFirst
	jp VGUIFindNext
#ifdef enableCn22
Cn2Libs:
	jp Cn3_Setup
	jp Cn3_Clear_SendBuf
	jp Cn3_Clear_RecBuf
	jp Cn3_Setdown
;	jp Cn2j_Int_Start
#else
	jp mj_NotImplemented	;Cn2_Setup
	jp mj_NotImplemented	;Cn2_Clear_SendBuf
	jp mj_NotImplemented	;Cn2_Clear_RecBuf
	jp mj_NotImplemented	;Cn2_Setdown
#endif
APLibs:
	jp mj_FileOpen
	jp mj_FileSave
	jp mj_FileSaveAs
	
	jp DispLongInt
	jp mj_Cn2GetK
	jp DPutMap
	jp APGui_gui7ToTop
	jp VPushGUIStacks
	jp VGUIFindThis
	jp mj_DGetCharWidthLocFromA
	
#ifdef mossupport
.branch mos_directin
.branch mos_sendbytetios
.branch mos_getbytetios
.branch mos_version
.branch mos_setvputs
.branch mos_setpixel
.branch mos_FastcopyS
.branch mos_delayb
.branch mos_multhe
.branch mos_multhl
.branch mos_QuitToShell
.branch mos_fastline
.branch mos_pixelonhl
.branch mos_pixeloff
.branch mos_pixelxor
.branch mos_pixeltest_
.branch mos_pixeloffhl
.branch mos_pixelxorhl 
.branch mos_pixeltesthl
.branch mos_fastlineb
.branch mos_fastlinew
.branch mos_fastlinex
.branch mos_pointonc
.branch mos_pointoffc
.branch mos_pointxorc
.branch mos_centertext
.branch mos_cphlbc
.branch mos_putsprite8
.branch mos_fastcopyb
.branch mos_vputsc
.branch mos_scrolld7
.branch mos_vnewline
.branch mos_rand127
.branch mos_disprle
.branch mos_cphlde
.branch mos_fastlined
.branch mos_disprlel
.branch NotImplemented
.branch mos_compstrs
.branch mos_nextstr
.branch mos_fastrectangle
.branch mos_gettext
.branch mos_gettextv
.branch mos_fastrectangle_save
.branch mos_vputa
.branch mos_Filledrectangle
.branch mos_compstrsn
.branch mos_Filledrectangle_Save
.branch mos_getbytetiosw
.branch mos_setupint
.branch mos_largespritehl
.branch mos_getnext
;.branch mos_vatswap
#endif

.branch OpenGUIStack
.branch CloseGUIStack
.branch PushGUIStack
.branch PushGUIStacks
.branch PopGUIStack
.branch PopGUIStacks
.branch GUIMouse
.branch GUIMouseRAM
.branch RenderGUI
.branch RenderGUISub
.branch GUIFindFirst
.branch GUIFindNext
.branch GUIFindThis

.branch Small_Window
.branch ClrDialogFull
.branch LargeWindow
.branch ClrWinFull
.branch PlaySound
.branch VDispHL
.branch Pause
.branch hDetect
.branch ResetAppPage0a
.branch iRandom
.branch iGetPixel
.branch iPutSprite
.branch iLargeSprite
.branch iFastCopy
.branch iDetect
.branch iDecompress
.branch iVersion
.branch vputsapp
.branch putsapp

;.branch DrawCustomRectangle
.branch VertLine
;.branch api_drawline_set
.branch MultADE
.branch DivHLDE
.branch imPutSpriteMask
.branch GUIDrawHourglass
.branch dputmap
.branch vdispa

.branch DAVLCheck
.branch DAVLCheckOffset
.branch RenderGUIGetWinX
.branch OpenStartMenu
.branch RenderDesktopRetFrom2
.branch RealStartNoResetRetFrom2
;.branch Restart
;.branch Shutdown
.branch QuitToOS
.branch GUIRScrollVertDesk
;#ifdef enableCn22
.branch Cn3_Setup
.branch Cn3_Setdown
;.branch Cn22_Clear_Send	Buf
;.branch Cn22_Clear_RecBuf
;.branch Cn2_Int_Start
;#endif
.branch AboutDisplay
.branch Arc_UnarcP2
.branch BackupAppVar
.branch FldSearch
.branch FileOpen
.branch FileSave
.branch FileSaveAs
.branch DispLongInt
.branch DebounceOn
.branch hook1retfromp2
.branch DesktopMouseReturn
.branch PropMenuMouseReturn
.branch PropString
.branch Cn2GetK
.branch asmcheckwriteback
.branch EnableLaunchKeyhook
.branch DisableLaunchKeyhook
;.branch APOpenGuiCopyToRAM
.branch EnableMyParserhook
.branch DisableMyParserHook
.branch RunProgFromHook
.branch RunSEs
.branch InfoPop
.branch MemoryPop
.branch TabFunc_GetCoords
.branch TabFunc_Tab
.branch TabFunc_ShiftTab
.branch Off
.branch PushProgChain
.branch GetProgChainTop
.branch APGui_gui7ToTop
.branch PopProgChain
.branch SwapProgChain
.branch GetRAMName
.branch GetRAMNameAP
.branch hook1_1
.branch InitTmpASMOp1
.branch GetArcStatus
.branch ExitDoorsCS
.branch mj_RunProg
.branch GetVarWidth
.branch dbfClearAnsStack_OffPage
.branch DGetCharWidth
.branch FldSave_NoAVCheck
.branch sort
.branch SMHelpOffpage
.branch DGetCharWidthLocFromA

#ifdef mossupport
mj_getnext:			bcall(_mos_getnext) \ ret
;mj_vatswap:			bcall(_mos_vatswap) \ ret
mj_directIn:			bcall(_mos_directin) \ ret
mj_sendbytetios:        bcall(_mos_sendbytetios) \ ret
mj_getbytetios:             bcall(_mos_getbytetios) \ ret
mj_version:                 bcall(_mos_version) \ ret
mj_setvputs:                bcall(_mos_setvputs) \ ret
mj_setpixel:                bcall(_mos_setpixel) \ ret
mj_fastcopys:               bcall(_mos_FastcopyS) \ ret
mj_delayb:                  bcall(_mos_delayb) \ ret
mj_multhe:                  bcall(_mos_multhe) \ ret
mj_multhl:                  bcall(_mos_multhl) \ ret
mj_quittoshell:             bcall(_mos_QuitToShell) \ ret
mj_fastline:                bcall(_mos_fastline) \ ret
mj_pixelonhl:               bcall(_mos_pixelonhl) \ ret
mj_pixeloff:                bcall(_mos_pixeloff) \ ret
mj_pixelxor:                bcall(_mos_pixelxor) \ ret
mj_pixeltest:               bcall(_mos_pixeltest_) \ ret
mj_pixeloffhl:              bcall(_mos_pixeloffhl) \ ret
mj_pixelxorhl:              bcall(_mos_pixelxorhl ) \ ret
mj_pixeltesthl:             bcall(_mos_pixeltesthl) \ ret
mj_fastlineb:               bcall(_mos_fastlineb) \ ret
mj_fastlinew:               bcall(_mos_fastlinew) \ ret
mj_fastlinex:               bcall(_mos_fastlinex) \ ret
mj_pointonc:                bcall(_mos_pointonc) \ ret
mj_pointoffc:               bcall(_mos_pointoffc) \ ret
mj_pointxorc:               bcall(_mos_pointxorc) \ ret
mj_centertext:              bcall(_mos_centertext) \ ret
mj_cphlbc:                  bcall(_mos_cphlbc) \ ret
mj_putsprite8:              bcall(_mos_putsprite8) \ ret
mj_fastcopyb:               bcall(_mos_fastcopyb) \ ret
mj_vputsc:                  bcall(_mos_vputsc) \ ret
mj_scrolld7:                bcall(_mos_scrolld7) \ ret
mj_vnewline:                bcall(_mos_vnewline) \ ret
mj_rand127:                 bcall(_mos_rand127) \ ret
mj_disprle:                 bcall(_mos_disprle) \ ret
mj_cphlde:                  bcall(_mos_cphlde) \ ret
mj_fastlined:               bcall(_mos_fastlined) \ ret
mj_disprlel:                bcall(_mos_disprlel) \ ret
mj_NotImplemented:          bcall(_NotImplemented) \ ret  ;not used
mj_compstrs:                bcall(_mos_compstrs) \ ret
mj_nextstr:                 bcall(_mos_nextstr) \ ret
mj_fastrectangle:           bcall(_mos_fastrectangle) \ ret
mj_gettext:                 bcall(_mos_gettext) \ ret
mj_gettextv:                bcall(_mos_gettextv) \ ret
mj_FastRectangle_Save:      bcall(_mos_fastrectangle_save) \ ret
mj_vputa:                   bcall(_mos_vputa) \ ret
mj_filledrectangle:         bcall(_mos_Filledrectangle) \ ret
mj_compstrsn:               bcall(_mos_compstrsn) \ ret
mj_filledrectangle_save:    bcall(_mos_Filledrectangle_Save) \ ret
mj_getbytetiosw:            bcall(_mos_getbytetiosw) \ ret
mj_setupint:                bcall(_mos_setupint) \ ret
mj_largespritehl:           bcall(_mos_largespritehl) \ ret
VOpenGUIStack:				bcall(_OpenGUIStack) \ ret
VCloseGUIStack:				bcall(_CloseGUIStack) \ ret
VPushGUIStack:				bcall(_PushGUIStack) \ ret
VPushGUIStacks:				bcall(_PushGUIStacks) \ ret
VPopGUIStack:				bcall(_PopGUIStack) \ ret
VRenderGUI:					bcall(_RenderGUI) \ ret
VPopGUIStacks:				bcall(_PopGUIStacks) \ ret
VGUIMouseRAM:				bcall(_GUIMouseRAM) \ ret
VGUIFindFirst:				bcall(_GUIFindFirst) \ ret
VGUIFindNext:				bcall(_GUIFindNext) \ ret
VGUIFindThis:				bcall(_GUIFindThis)	\ ret
mj_FileOpen:				bcall(_FileOpen) \ ret
mj_FileSave:				bcall(_FileSave) \ ret
mj_FileSaveAs:				bcall(_FileSaveAs) \ ret
mj_Cn2GetK:					bcall(_Cn2GetK) \ ret
mj_DGetCharWidthLocFromA:	bcall(_DGetCharWidthLocFromA) \ ret

#endif