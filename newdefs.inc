
.nolist

;+----------------------------------------+
;|      Doors CS v6.0 Include File        |
;|         TI-83+/TI-84+ Version          |
;|         Revised May 24, 2005           |
;|           By Kerm Martian              |
;+----------------------------------------+

;----> Safe Memory Areas
; saferam1 = 768 bytes (APDRAM)
; saferam2 = 531 bytes (statRAM)
; saferam3 = 128 bytes (textMem)
; saferam4 = 66 bytes (OpXs)
; saferam5 = 10 bytes (iMathPtrs)

#DEFINE equ .equ
#DEFINE EQU .equ
#DEFINE end .end
#DEFINE END .end
#IFDEF	TI83P

#IFDEF APP
progstart =$4000
#ELSE
progstart	=9D95h
#ENDIF

#define AppOnErr(xxxx) ld hl,xxxx \ call APP_PUSH_ERRORH
#define AppOffErr call APP_POP_ERRORH
APP_PUSH_ERRORH .EQU	59h
APP_POP_ERRORH	.EQU	5Ch
#define bcall(xxxx)	rst 28h \ .dw xxxx
#define	bcallz(xxxx)	jr nz,$+5 \ rst 28h \ .dw xxxx
#define	bcallnz(xxxx)	jr	z,$+5 \ rst 28h \ .dw xxxx
#define	bcallc(xxxx)	jr	nc,$+5 \ rst 28h \ .dw xxxx
#define	bcallnc(xxxx)	jr	c,$+5	\ rst 28h \ .dw xxxx
#define bjump(xxxx)	call 50h \ .dw xxxx
#define b_call(xxxx)	rst 28h \ .dw xxxx
#define	b_callz(xxxx)	jr nz,$+5 \ rst 28h \ .dw xxxx
#define	b_callnz(xxxx)	jr	z,$+5 \ rst 28h \ .dw xxxx
#define	b_callc(xxxx)	jr	nc,$+5 \ rst 28h \ .dw xxxx
#define	b_callnc(xxxx)	jr	c,$+5	\ rst 28h \ .dw xxxx
#define b_jump(xxxx)	call 50h \ .dw xxxx
 .addinstr BCALL *       EF   3 NOP 1
appBackupScreen	=$9872	; 768 bytes
_AbsO1O2Cp	EQU 410Eh
_AbsO1PAbsO2	EQU 405Ah
_ACos		EQU 40DEh
_ACosH		EQU 40F0h
_ACosRad	EQU 40D2h
_AdrLEle	EQU 462Dh
_AdrMEle	EQU 4609h
_AdrMRow	EQU 4606h
_AllEq		EQU 4876h
_AllocFPS	EQU 43A5h
_AllocFPS1	EQU 43A8h
_Angle		EQU 4102h
_AnsName	EQU 4B52h
_ApdSetup	EQU 4C93h
_AppGetCalc	EQU 4C78h
_AppGetCbl	EQU 4C75h
_AppInit	EQU 404Bh  ;***
_Arc_Unarc	EQU 4FD8h
_ASin		EQU 40E4h
_ASinH		EQU 40EDh
_ASinRad	EQU 40DBh
_ATan		EQU 40E1h
_ATan2		EQU 40E7h
_ATan2Rad	EQU 40D8h
_ATanH		EQU 40EAh
_ATanRad	EQU 40D5h

_BinOPExec	EQU 4663h
_Bit_VertSplit	EQU 4FA8h
_BufClr		EQU 5074h
_BufCpy		EQU 5071h

_CAbs		EQU 4E97h
_CAdd		EQU 4E88h
_CanAlphIns	EQU 4C69h
_CDiv		EQU 4E94h
_CDivByReal	EQU 4EBBh
_CEtoX		EQU 4EA9h
_CFrac		EQU 4EC1h
_CheckSplitFlag	EQU 49F0h
_Chk_Batt_Low	EQU 50B3h  ;***
_ChkFindSym	EQU 42F1h
_CIntgr		EQU 4EC4h
_CircCmd	EQU 47D4h
_CkInt		EQU 4234h
_CkOdd		EQU 4237h
_CkOP1C0	EQU 4225h
_CkOP1Cplx	EQU 40FCh
_CkOP1FP0	EQU 4228h
_CkOP1Pos	EQU 4258h
_CkOP1Real	EQU 40FFh
_CkOP2FP0	EQU 422Bh
_CkOP2Pos	EQU 4255h
_CkOP2Real	EQU 42DFh
_CkPosInt	EQU 4231h
_CkValidNum	EQU 4270h
_CleanAll	EQU 4A50h
_ClearRect	EQU 4D5Ch
_ClearRow	EQU 4CEDh
_CLine		EQU 4798h
_CLineS		EQU 479Bh
_CLN		EQU 4EA0h
_CLog		EQU 4EA3h
_CloseEditBuf	EQU 48D3h  ;***
_CloseEditBufNoR	EQU 476Eh
_CloseEditEqu	EQU 496Ch  ;***
_CloseProg	EQU 4A35h
_ClrGraphRef	EQU 4A38h
_ClrLCD		EQU 4543h
_ClrLCDFull	EQU 4540h
_ClrLp		EQU 41D1h
_ClrOP1S	EQU 425Eh
_ClrOP2S	EQU 425Bh
_ClrScrn	EQU 4549h
_ClrScrnFull	EQU 4546h
_ClrTxtShd	EQU 454Ch
_CMltByReal	EQU 4EB8h
_CmpSyms	EQU 4A4Ah
_CMult		EQU 4E8Eh
_Conj		EQU 4EB5h
_ConvDim	EQU 4B43h
_ConvDim00	EQU 4B46h
_ConvLcToLr	EQU 4A23h
_ConvLrToLc	EQU 4A56h
_ConvOP1	EQU 4AEFh
_COP1Set0	EQU 4105h
_Cos		EQU 40C0h
_CosH		EQU 40CCh
_CpHLDE		EQU 400Ch
_CPoint		EQU 4DC8h
_CPointS	EQU 47F5h
_CpOP1OP2	EQU 4111h
_CpOP4OP3	EQU 4108h
_CpyO1ToFPS1	EQU 445Ch
_CpyO1ToFPS2	EQU 446Bh
_CpyO1ToFPS3	EQU 4477h
_CpyO1ToFPS4	EQU 4489h
_CpyO1ToFPS5	EQU 4483h
_CpyO1ToFPS6	EQU 447Dh
_CpyO1ToFPS7	EQU 4480h
_CpyO1ToFPST	EQU 444Ah
_CpyO2ToFPS1	EQU 4459h
_CpyO2ToFPS2	EQU 4462h
_CpyO2ToFPS3	EQU 4474h
_CpyO2ToFPS4	EQU 4486h
_CpyO2ToFPST	EQU 4444h
_CpyO3ToFPS1	EQU 4453h
_CpyO3ToFPS2	EQU 4465h
_CpyO3ToFPST	EQU 4441h
_CpyO5ToFPS1	EQU 4456h
_CpyO5ToFPS3	EQU 4471h
_CpyO6ToFPS2	EQU 4468h
_CpyO6ToFPST	EQU 4447h
_CpyStack	EQU 4429h
_CpyTo1FPS1	EQU 4432h
_CpyTo1FPS10	EQU 43F3h
_CpyTo1FPS11	EQU 43D8h
_CpyTo1FPS2	EQU 443Bh
_CpyTo1FPS3	EQU 4408h
_CpyTo1FPS4	EQU 440Eh
_CpyTo1FPS5	EQU 43DEh
_CpyTo1FPS6	EQU 43E4h
_CpyTo1FPS7	EQU 43EAh
_CpyTo1FPS8	EQU 43EDh
_CpyTo1FPS9	EQU 43F6h
_CpyTo1FPST	EQU 4423h
_CpyTo2FPS1	EQU 442Fh
_CpyTo2FPS2	EQU 4438h
_CpyTo2FPS3	EQU 4402h
_CpyTo2FPS4	EQU 43F9h
_CpyTo2FPS5	EQU 43DBh
_CpyTo2FPS6	EQU 43E1h
_CpyTo2FPS7	EQU 43E7h
_CpyTo2FPS8	EQU 43F0h
_CpyTo2FPST	EQU 4420h
_CpyTo3FPS1	EQU 442Ch
_CpyTo3FPS2	EQU 4411h
_CpyTo3FPST	EQU 441Dh
_CpyTo4FPST	EQU 441Ah
_CpyTo5FPST	EQU 4414h
_CpyTo6FPS2	EQU 43FFh
_CpyTo6FPS3	EQU 43FCh
_CpyTo6FPST	EQU 4417h
_CpyToFPS1	EQU 445Fh
_CpyToFPS2	EQU 446Eh
_CpyToFPS3	EQU 447Ah
_CpyToFPST	EQU 444Dh
_CpyToStack	EQU 4450h
_Create0Equ	EQU 432Ah
_CreateAppVar	EQU 4E6Ah
_CreateCList	EQU 431Bh
_CreateCplx	EQU 430Ch
_CreateEqu	EQU 4330h
_CreatePair	EQU 4B0Dh
_CreatePict	EQU 4333h
_CreateProg	EQU 4339h
_CreateProtProg	EQU 4E6Dh
_CreateReal	EQU 430Fh
_CreateRList	EQU 4315h
_CreateRMat	EQU 4321h
_CreateStrng	EQU 4327h
_CRecip		EQU 4E91h
_CSqRoot	EQU 4E9Dh
_CSquare	EQU 4E8Bh
_CSub		EQU 4E85h
_CTenX		EQU 4EA6h
_CTrunc		EQU 4EBEh
_Cube		EQU 407Bh
_CursorOff	EQU 45BEh
_CursorOn	EQU 45C4h
_CXrootY	EQU 4EACh
_CYtoX		EQU 4EB2h

_DarkLine	EQU 47DDh
_DarkPnt	EQU 47F2h
_DataSize	EQU 436Ch
_DataSizeA	EQU 4369h
_DeallocFPS	EQU 439Fh
_DeallocFPS1	EQU 43A2h
_DecO1Exp	EQU 4267h
_DelListEl	EQU 4A2Fh
_DelMem		EQU 4357h
_DelRes		EQU 4A20h
_DelVar		EQU 4351h
_DelVarArc	EQU 4FC6h
_DelVarNoArc	EQU 4FC9h
_DisableApd	EQU 4C84h
_Disp		EQU 4F45h
_DispDone	EQU 45B5h
_DispEOL	EQU 45A6h
_DispHL		EQU 4507h
_DisplayImage	EQU 4D9Bh
_DispMenuTitle	EQU 5065h
_DispOP1A	EQU 4BF7h
_DivHLBy10	EQU 400Fh
_DivHLByA	EQU 4012h
_DrawCirc2	EQU 4C66h
_DrawCmd	EQU 48C1h
_DrawRectBorder	EQU 4D7Dh
_DrawRectBorderClear	EQU 4D8Ch
_DToR		EQU 4075h

_EditProg	EQU 4A32h
_EnableApd	EQU 4C87h
_EnoughMem	EQU 42FDh
_EOP1NotReal	EQU 4279h
_Equ_or_NewEqu	EQU 42C4h
_EraseEOL	EQU 4552h
_EraseRectBorder	EQU 4D86h
_ErrArgument	EQU 44ADh
_ErrBadGuess	EQU 44CBh
_ErrBreak	EQU 44BFh
_ErrD_OP1_0	EQU 42D3h
_ErrD_OP1_LE_0	EQU 42D0h
_ErrD_OP1Not_R	EQU 42CAh
_ErrD_OP1NotPos	EQU 42C7h
_ErrD_OP1NotPosInt	EQU 42CDh
_ErrDataType	EQU 44AAh
_ErrDimension	EQU 44B3h
_ErrDimMismatch	EQU 44B0h
_ErrDivBy0	EQU 4498h
_ErrDomain	EQU 449Eh
_ErrIncrement	EQU 44A1h
_ErrInvalid	EQU 44BCh
_ErrIterations	EQU 44C8h
_ErrLinkXmit	EQU 44D4h
_ErrMemory	EQU 44B9h
_ErrNon_Real	EQU 44A4h
_ErrNonReal	EQU 4A8Ch
_ErrNotEnoughMem	EQU 448Ch
_ErrOverflow	EQU 4495h
_ErrSignChange	EQU 44C5h
_ErrSingularMat	EQU 449Bh
_ErrStat	EQU 44C2h
_ErrStatPlot	EQU 44D1h
_ErrSyntax	EQU 44A7h
_ErrTolTooSmall	EQU 44CEh
_ErrUndefined	EQU 44B6h
_EToX		EQU 40B4h
_Exch9		EQU 43D5h
_ExLp		EQU 4222h
_ExpToHex	EQU 424Fh

_Factorial	EQU 4B85h
_FillRect	EQU 4D62h
_FillRectPattern	EQU 4D89h
_Find_Parse_Formula	EQU 4AF2h
_FindAlphaDn	EQU 4A47h
_FindAlphaUp	EQU 4A44h
_FindApp		EQU 4C4Eh
_FindAppDn		EQU 4C4Bh
_FindAppNumPages	EQU 509Bh
_FindAppUp		EQU 4C48h
_FindSym	EQU 42F4h
_FiveExec	EQU 467Eh
_FixTempCnt	EQU 4A3Bh
_FlashToRam	EQU 5017h
_ForceFullScreen	EQU 508Fh
_FormBase	EQU 50AAh
_FormDCplx	EQU 4996h
_FormEReal	EQU 4990h
_FormReal	EQU 4999h
_FourExec	EQU 467Bh
_FPAdd		EQU 4072h
_FPDiv		EQU 4099h
_FPMult		EQU 4084h
_FPRecip	EQU 4096h
_FPSquare	EQU 4081h
_FPSub		EQU 406Fh
_Frac		EQU 4093h

_getHardwareVersion EQU 80BAh
_Get_Tok_Strng	EQU 4594h  ;***
_GetBaseVer	EQU 4C6Fh
_GetCSC		EQU 4018h
_GetKey		EQU 4972h
_GetLToOP1	EQU 4636h
_GetMToOP1	EQU 4615h
_GetSysInfo 	EQU 50DDh  ; v. 1.13+ only
_GetTokLen	EQU 4591h  ;***
_GrBufClr	EQU 4BD0h
_GrBufCpy	EQU 486Ah
_GrphCirc	EQU 47D7h

_HLTimes9	EQU 40F9h
_HorizCmd	EQU 48A6h
_HTimesL	EQU 4276h

_IBounds	EQU 4C60h
_IBoundsFull	EQU 4D98h
_ILine		EQU 47E0h
_IncLstSize	EQU 4A29h
_InsertList	EQU 4A2Ch
_InsertMem	EQU 42F7h
_Int		EQU 40A5h
_Intgr		EQU 405Dh
_InvCmd		EQU 48C7h
_InvertRect	EQU 4D5Fh
_InvOP1S	EQU 408Dh
_InvOP1SC	EQU 408Ah
_InvOP2S	EQU 4090h
_InvSub		EQU 4063h
_IOffset	EQU 4C63h
_IPoint		EQU 47E3h
_IsA2ByteTok	EQU 42A3h  ;***
_IsEditEmpty	EQU 492Dh  ;***

_JError		EQU 44D7h
_JErrorNo	EQU 4000h
_JForceCmdNoChar	EQU 4027h
_JForceGraphKey	EQU 5005h
_JForceGraphNoKey	EQU 5002h

_KeyToString	EQU 45CAh

_LCD_BUSY	EQU 4051h  ;***
_LCD_DRIVERON	equ 4978h  ;***
_LdHLind	equ 4009h  ;***
_LineCmd	EQU 48ACh
_LnX		EQU 40ABh
_Load_SFont	EQU 4783h
_LoadCIndPaged	EQU 501Dh
_LoadDEIndPaged	EQU 501Ah
_LoadPattern	EQU 4CB1h
_LogX		EQU 40AEh

_Max		EQU 4057h
_MemChk		EQU 42E5h
_MemClear	EQU 4C30h
_MemSet		EQU 4C33h
_Min		EQU 4054h
_Minus1		EQU 406Ch
_Mov10B		EQU 415Ch
_Mov18B		EQU 47DAh
_Mov7B		EQU 4168h
_Mov8B		EQU 4165h
_Mov9B		EQU 415Fh
_Mov9OP1OP2	EQU 417Dh
_Mov9OP2Cp	EQU 410Bh
_Mov9ToOP1	EQU 417Ah
_Mov9ToOP2	EQU 4180h
_MovFrOP1	EQU 4183h

_NewLine	EQU 452Eh  ;***
_NZIf83Plus	EQU 50E0h  ; v. 1.13+ only

_OneVar		EQU 4BA3h
_OP1ExOP2	EQU 421Fh
_OP1ExOP3	EQU 4219h
_OP1ExOP4	EQU 421Ch
_OP1ExOP5	EQU 420Dh
_OP1ExOP6	EQU 4210h
_OP1ExpToDec	EQU 4252h
_OP1Set0	EQU 41BFh
_OP1Set1	EQU 419Bh
_OP1Set2	EQU 41A7h
_OP1Set3	EQU 41A1h
_OP1Set4	EQU 419Eh
_OP1ToOP2	EQU 412Fh
_OP1ToOP3	EQU 4123h
_OP1ToOP4	EQU 4117h
_OP1ToOP5	EQU 4153h
_OP1ToOP6	EQU 4150h
_OP2ExOP4	EQU 4213h
_OP2ExOP5	EQU 4216h
_OP2ExOP6	EQU 4207h
_OP2Set0	EQU 41BCh
_OP2Set1	EQU 41ADh
_OP2Set2	EQU 41AAh
_OP2Set3	EQU 4198h
_OP2Set4	EQU 4195h
_OP2Set5	EQU 418Fh
_OP2Set60	EQU 4AB0h
_OP2Set8	EQU 418Ch
_OP2SetA	EQU 4192h
_OP2ToOP1	EQU 4156h
_OP2ToOP3	EQU 416Eh
_OP2ToOP4	EQU 411Ah
_OP2ToOP5	EQU 414Ah
_OP2ToOP6	EQU 414Dh
_OP3Set0	EQU 41B9h
_OP3Set1	EQU 4189h
_OP3Set2	EQU 41A4h
_OP3ToOP1	EQU 413Eh
_OP3ToOP2	EQU 4120h
_OP3ToOP4	EQU 4114h
_OP3ToOP5	EQU 4147h
_OP4Set0	EQU 41B6h
_OP4Set1	EQU 4186h
_OP4ToOP1	EQU 4138h
_OP4ToOP2	EQU 411Dh
_OP4ToOP3	EQU 4171h
_OP4ToOP5	EQU 4144h
_OP4ToOP6	EQU 4177h
_OP5ExOP6	EQU 420Ah
_OP5Set0	EQU 41B3h
_OP5ToOP1	EQU 413Bh
_OP5ToOP2	EQU 4126h
_OP5ToOP3	EQU 4174h
_OP5ToOP4	EQU 412Ch
_OP5ToOP6	EQU 4129h
_OP6ToOP1	EQU 4135h
_OP6ToOP2	EQU 4132h
_OP6ToOP5	EQU 4141h
_OutputExpr	EQU 4BB2h

_PagedGet	EQU 5023h
_ParseInp	EQU 4A9Bh
_PDspGrph	EQU 48A3h
_PixelTest	EQU 48B5h
_Plus1		EQU 4069h
_PointCmd	EQU 48B2h
_PointOn	EQU 4C39h
_PopMCplxO1	EQU 436Fh  ;***
_PopOP1		EQU 437Eh
_PopOP3		EQU 437Bh
_PopOP5		EQU 4378h
_PopReal	EQU 4393h
_PopRealO1	EQU 4390h
_PopRealO2	EQU 438Dh
_PopRealO3	EQU 438Ah
_PopRealO4	EQU 4387h
_PopRealO5	EQU 4384h
_PopRealO6	EQU 4381h
_PosNo0Int	EQU 422Eh
_PToR		EQU 40F3h
_PushMCplxO1	EQU 43CFh  ;***
_PushMCplxO3	EQU 43C6h  ;***
_PushOP1	EQU 43C9h
_PushOP3	EQU 43C3h
_PushOP5	EQU 43C0h
_PushReal	EQU 43BDh
_PushRealO1	EQU 43BAh
_PushRealO2	EQU 43B7h
_PushRealO3	EQU 43B4h
_PushRealO4	EQU 43B1h
_PushRealO5	EQU 43AEh
_PushRealO6	EQU 43ABh
_PutAway	EQU 403ch  ;***
_PutC		EQU 4504h
_PutMap		EQU 4501h
_PutPS		EQU 4510h
_PutPSB		EQU 450dh  ;***
_PutS		EQU 450Ah
_PutTokString	EQU 4960h
_PutToL		EQU 4645h
_PutToMat	EQU 461Eh

_RandInit	EQU 4B7Fh
_Random		EQU 4B79h
_Rcl_StatVar	EQU 42DCh
_RclAns		EQU 4AD7h
_RclGDB2	EQU 47D1h
_RclN		EQU 4ADDh
_RclSysTok	EQU 4AE6h
_RclVarSym	EQU 4AE3h
_RclX		EQU 4AE0h
_RclY		EQU 4ADAh
_Rec1stByte	EQU 4EFAh
_Rec1stByteNC	EQU 4EFDh
_RecAByteIO	EQU 4F03h
_RedimMat	EQU 4A26h
_Regraph	EQU 488Eh
_ReleaseBuffer	EQU 4771h  ;***
_ReloadAppEntryVecs	EQU 4C36h
_RestoreDisp	EQU 4870h
_RName		EQU 427Fh
_RndGuard	EQU 409Fh
_RnFx		EQU 40A2h
_Round		EQU 40A8h
_RToD		EQU 4078h
_RToP		EQU 40F6h
_RunIndicOff	EQU 4570h
_RunIndicOn	EQU 456Dh

_SaveDisp	EQU 4C7Bh
_SendAByte	EQU 4EE5h
_SendVarCmd	EQU 4A14h ;***
_SetAllPlots	EQU 4FCCh
_SetExSpeed 	EQU 50BFh ; v. 1.13+ only
_SetFuncM	EQU 4840h
_SetNorm_Vals	EQU 49FCh
_SetParM	EQU 4849h
_SetPolM	EQU 4846h
_SetSeqM	EQU 4843h
_SetTblGraphDraw	EQU 4C00h
_SetupPagedPtr	EQU 5020h
_SetXXOP1	EQU 478Ch
_SetXXOP2	EQU 478Fh
_SetXXXXOP2	EQU 4792h
_SFont_Len	EQU 4786h
_ShRAcc		equ 41D4h
_Sin		EQU 40BDh
_SinCosRad	EQU 40BAh
_SinH		EQU 40CFh
_SinHCosH	EQU 40C6h
_SqRoot		EQU 409Ch
_SrchVLstDn	EQU 4F12h
_SrchVLstUp	EQU 4F0Fh
_SStringLength	EQU 4CB4h
_StMatEl	EQU 4AE9h
_StoAns		EQU 4ABFh
_StoGDB2	EQU 47CEh
_StoN		EQU 4ACBh
_StoOther	EQU 4AD4h
_StoR		EQU 4AC5h
_StoRand	EQU 4B7Ch
_StoSysTok	EQU 4ABCh
_StoT		EQU 4ACEh
_StoTheta	EQU 4AC2h
_StoX		EQU 4AD1h
_StoY		EQU 4AC8h
_StrCopy	EQU 44E3h
_StrLength	EQU 4C3Fh   

_Tan		EQU 40C3h
_TanH		EQU 40C9h
_TanLnF		EQU 48BBh
_TenX		EQU 40B7h
_ThetaName	EQU 427Ch
_ThreeExec	EQU 4675h
_Times2		EQU 4066h
_TimesPt5	EQU 407Eh
_TName		EQU 428Eh
_ToFrac		EQU 4657h
_Trunc		EQU 4060h
_TwoVarSet	EQU 50D4h  ; v. 1.13+ only

_UCLineS	EQU 4795h
_UnLineCmd	EQU 48AFh
_UnOPExec	EQU 4672h

_VertCmd	EQU 48A9h
_VPutMap	EQU 455Eh
_VPutS		EQU 4561h
_VPutSN		EQU 4564h
_VtoWHLDE	EQU 47FBh


_XftoI		EQU 4804h
_Xitof		EQU 47FEh
_XName		EQU 4288h
_XRootY		EQU 479Eh

_YftoI		EQU 4801h
_YName		EQU 428Bh
_YToX		EQU 47A1h

_Zero16D	EQU 41B0h
_ZeroOP		EQU 41CEh
_ZeroOP1	EQU 41C5h
_ZeroOP2	EQU 41C8h
_ZeroOP3	EQU 41CBh
_ZmDecml	EQU 484Fh
_ZmFit		EQU 485Bh
_ZmInt		EQU 484Ch
_ZmPrev		EQU 4852h
_ZmSquare	EQU 485Eh
_ZmStats	EQU 47A4h
_ZmTrig		EQU 4861h
_ZmUsr		EQU 4855h
_ZooDefault	EQU 4867h
ramStart             EQU  8000h
appData              EQU  8000h
ramCode              EQU  8100h
ramCodeEnd           EQU  822Fh
baseAppBrTab         EQU  8230h
bootTemp             EQU  8251h
appSearchPage        EQU  82A3h
tempSwapArea         EQU  82A5h
appID                EQU  838Dh
ramReturnData        EQU  83EDh
arcInfo              EQU  83EEh
savedArcInfo         EQU  8406h
appInfo              EQU  8432h
appBank_jump         EQU  843Ch
appPage              EQU  843Eh
kbdScanCode          EQU  843Fh
kbdKey               EQU  8444h
kbdGetKy             EQU  8445h
keyExtend            EQU  8446h
contrast             EQU  8447h
apdSubTimer          EQU  8448h
apdTimer             EQU  8449h
curTime              EQU  844Ah
curRow               EQU  844Bh
curCol               EQU  844Ch
curOffset            EQU  844Dh
curUnder             EQU  844Eh
curY                 EQU  844Fh
curType              EQU  8450h
curXRow              EQU  8451h
prevDData            EQU  8452h
lFont_record         EQU  845Ah
sFont_record         EQU  8462h
tokVarPtr            EQU  846Ah
tokLen               EQU  846Ch
indicMem             EQU  846Eh
indicCounter         EQU  8476h
indicBusy            EQU  8477h
OP1                  EQU  8478h
OP1M                 EQU  847Ah
OP2                  EQU  8483h
OP2M                 EQU  8485h
OP2EXT               EQU  848Ch
OP3                  EQU  848Eh
OP3M                 EQU  8490h
OP4                  EQU  8499h
OP4M                 EQU  849Bh
OP5                  EQU  84A4h
OP5M                 EQU  84A6h
OP6                  EQU  84AFh
OP6M                 EQU  84B1h
OP6EXT               EQU  84B8h
progToEdit           EQU  84BFh
nameBuff             EQU  84C7h
equ_edit_save        EQU  84D2h
iMathPtr1            EQU  84D3h
iMathPtr2            EQU  84D5h
iMathPtr3            EQU  84D7h
iMathPtr4            EQU  84D9h
iMathPtr5            EQU  84DBh
chkDelPtr1           EQU  84DDh
chkDelPtr2           EQU  84DFh
insDelPtr            EQU  84E1h
upDownPtr            EQU  84E3h
fOutDat              EQU  84E5h
asm_data_ptr1        EQU  84EBh
asm_data_ptr2        EQU  84EDh
asm_sym_ptr1         EQU  84EFh
asm_sym_ptr2         EQU  84F1h
asm_ram              EQU  84F3h
asm_ind_call         EQU  8507h
textShadow           EQU  8508h
textShadCur          EQU  8588h
textShadTop          EQU  858Ah
textShadAlph         EQU  858Bh
textShadIns          EQU  858Ch
cxMain               EQU  858Dh
cxPPutAway           EQU  858Fh
cxPutAway            EQU  8591h
cxRedisp             EQU  8593h
cxErrorEP            EQU  8595h
cxSizeWind           EQU  8597h
cxPage               EQU  8599h
cxCurApp             EQU  859Ah
cxPrev               EQU  859Bh
monQH                EQU  85AAh
monQT                EQU  85ABh
monQueue             EQU  85ACh
onSP                 EQU  85BCh
promptRow            EQU  85C0h
promptCol            EQU  85C1h
promptIns            EQU  85C2h
promptShift          EQU  85C3h
promptRet            EQU  85C4h
promptValid          EQU  85C6h
promptTop            EQU  85C8h
promptCursor         EQU  85CAh
promptTail           EQU  85CCh
promptBtm            EQU  85CEh
varType              EQU  85D0h
varCurrent           EQU  85D1h
varClass             EQU  85D9h
menuActive           EQU  85DCh
menuAppDepth         EQU  85DDh
MenuCurrent          EQU  85DEh
ProgCurrent          EQU  85E8h
userMenuSA           EQU  85FEh
ioPrompt             EQU  865Fh
dImageWidth          EQU  8660h
RectFillPHeight      equ  8660h
RectFillPWidth       equ  8661h
RectFillPattern      equ  8662h
ioFlag               EQU  8670h
sndRecState          EQU  8672h
ioErrState           EQU  8673h
header               EQU  8674h
ioData               EQU  867Dh
ioNewData            EQU  8689h
bakHeader            EQU  868Bh
penCol               EQU  86D7h
penRow               EQU  86D8h
rclQueue             EQU  86D9h
rclQueueEnd          EQU  86DBh
errNo                EQU  86DDh
errSP                EQU  86DEh
errOffset            EQU  86E0h
saveSScreen          EQU  86ECh
bstCounter           EQU  89EEh
flags                EQU  89F0h
statVars             EQU  8A3Ah
anovaf_vars          EQU  8C17h
infVars              EQU  8C4Dh
curGStyle            EQU  8D17h
curGY                EQU  8D18h
curGX                EQU  8D19h
curGY2               EQU  8D1Ah
curGX2               EQU  8D1Bh
freeSaveY            EQU  8D1Ch
freeSaveX            EQU  8D1Dh
XOffset              EQU  8DA1h
YOffset              EQU  8DA2h
lcdTallP             EQU  8DA3h
pixWideP             EQU  8DA4h
pixWide_m_1          EQU  8DA5h
pixWide_m_2          EQU  8DA6h
lastEntryPTR         EQU  8DA7h
lastEntryStk         EQU  8DA9h
numLastEntries       EQU  8E29h
currLastEntry        EQU  8E2Ah
curInc               EQU  8E67h
uXmin                EQU  8E7Eh
uXmax                EQU  8E87h
uXscl                EQU  8E90h
uYmin                EQU  8E99h
uYmax                EQU  8EA2h
uYscl                EQU  8EABh
uThetMin             EQU  8EB4h
uThetMax             EQU  8EBDh
uThetStep            EQU  8EC6h
uTmin                EQU  8ECFh
uTmax                EQU  8ED8h
uTStep               EQU  8EE1h
uPlotStart           EQU  8EEAh
unMax                EQU  8EF3h
uu0                  EQU  8EFCh
uv0                  EQU  8F05h
unMin                EQU  8F0Eh
uu02                 EQU  8F17h
uv02                 EQU  8F20h
uw0                  EQU  8F29h
uPlotStep            EQU  8F32h
uXres                EQU  8F3Bh
uw02                 EQU  8F44h
Xmin                 EQU  8F50h
Xmax                 EQU  8F59h
Xscl                 EQU  8F62h
Ymin                 EQU  8F6Bh
Ymax                 EQU  8F74h
Yscl                 EQU  8F7Dh
ThetaMin             EQU  8F86h
ThetaMax             EQU  8F8Fh
ThetaStep            EQU  8F98h
TminPar              EQU  8FA1h
TmaxPar              EQU  8FAAh
Tstep                EQU  8FB3h
PlotStart            EQU  8FBCh
nMax                 EQU  8FC5h
u0                   EQU  8FCEh
v0                   EQU  8FD7h
nMin                 EQU  8FE0h
u02                  EQU  8FE9h
v02                  EQU  8FF2h
w0                   EQU  8FFBh
PlotStep             EQU  9004h
XresO                EQU  900Dh
w02                  EQU  9016h
un1                  EQU  901Fh
un2                  EQU  9028h
vn1                  EQU  9031h
vn2                  EQU  903Ah
wn1                  EQU  9043h
wn2                  EQU  904Ch
fin_N                EQU  9055h
fin_I                EQU  905Eh
fin_PV               EQU  9067h
fin_PMT              EQU  9070h
fin_FV               EQU  9079h
fin_PY               EQU  9082h
fin_CY               EQU  908Bh
cal_N                EQU  9094h
cal_I                EQU  909Dh
cal_PV               EQU  90A6h
cal_PMT              EQU  90AFh
cal_FV               EQU  90B8h
cal_PY               EQU  90C1h
smallEditRAM         EQU  90D3h
XFact                EQU  913Fh
YFact                EQU  9148h
Xres_int             EQU  9151h
deltaX               EQU  9152h
deltaY               EQU  915Bh
shortX               EQU  9164h
shortY               EQU  916Dh
lower                EQU  9176h
upper                EQU  917Fh
XOutSym              EQU  918Ch
XOutDat              EQU  918Eh
YOutSym              EQU  9190h
YOutDat              EQU  9192h
inputSym             EQU  9194h
inputDat             EQU  9196h
prevData             EQU  9198h
TblMin               EQU  92B3h
TblStep              EQU  92BCh
P1Type		     EQU  92C9h
SavX1List	     EQU  92CAh
SavY1List	     EQU  92CFh
SavF1List	     EQU  92D4h
P1FrqOnOff	     EQU  92D9h
P2Type		     EQU  92DAh
SavX2List	     EQU  92DBh
SavY2List	     EQU  92E0h
SavF2List	     EQU  92E5h
P2FrqOnOff	     EQU  92EAh
P3Type		     EQU  92EBh	
SavX3List	     EQU  92ECh
SavY3List	     EQU  92F1h
SavF3List	     EQU  92F6h
P3FrqOnOff	     EQU  92FBh
plotSScreen          EQU  9340h
seed1                EQU  9640h
seed2                EQU  9649h
cmdShadow            EQU  966Eh
cmdShadCur           EQU  96EEh
cmdShadAlph          EQU  96F0h
cmdShadIns           EQU  96F1h
cmdCursor            EQU  96F2h
editTop              EQU  96F4h
editCursor           EQU  96F6h
editTail             EQU  96F8h
editBtm              EQU  96FAh
editSym              EQU  9706h
editDat              EQU  9708h
GY0		EQU 977Fh
GY1		EQU 9776h
GY2		EQU 9777h
GY3		EQU 9778h
GY4		EQU 9779h
GY5		EQU 977Ah
GY6		EQU 977Bh
GY7		EQU 977Ch
GY8		EQU 977Dh
GY9		EQU 977Eh
GX1		EQU 9780h
GX2		EQU 9781h
GX3		EQU 9782h
GX4		EQU 9783h
GX5		EQU 9784h
GX6		EQU 9785h
GU		EQU 978Ch
GV		EQU 978Dh
GW		EQU 978Eh
GR1		EQU 9786h
GR2		EQU 9787h
GR3		EQU 9788h
GR4		EQU 9789h
GR5		EQU 978Ah
GR6		EQU 978Bh
winTop               EQU  97A5h
winBtm               EQU  97A6h
winLeftEdge          EQU  97A7h
winLeft              EQU  97A8h
winAbove             EQU  97AAh
winRow               EQU  97ACh
winCol               EQU  97AEh
fmtDigits            EQU  97B0h
fmtString            EQU  97B1h
fmtConv              EQU  97F2h
fmtLeft              EQU  9804h
fmtIndex             EQU  9806h
fmtMatSym            EQU  9808h
fmtMatMem            EQU  980Ah
EQS                  EQU  980Ch
tSymPtr1             EQU  9818h
tSymPtr2             EQU  981Ah
chkDelPtr3           EQU  981Ch
chkDelPtr4           EQU  981Eh
tempMem              EQU  9820h
fpBase               EQU  9822h
FPS                  EQU  9824h
OPBase               EQU  9826h
OPS                  EQU  9828h
pTempCnt             EQU  982Ah
cleanTmp             EQU  982Ch
pTemp                EQU  982Eh
progPtr              EQU  9830h
newDataPtr           EQU  9832h
pagedCount           EQU  9834h
pagedPN              EQU  9835h
pagedGetPtr          EQU  9836h
pagedPutPtr          EQU  9838h
pagedBuf             EQU  983Ah
appErr1              EQU  984Dh
appErr2              EQU  985Ah
flashByte1           EQU  9867h
flashByte2           EQU  9868h
freeArcBlock         EQU  9869h
arcPage              EQU  986Bh
arcPtr               EQU  986Ch
appRawKeyHandle      EQU  9870h
customHeight         EQU  9B72h
localLanguage        EQU  9B73h
cursorHookPtr        EQU  9B7Ch
rawKeyHookPtr        EQU  9B84h
getKeyHookPtr        EQU  9B88h
fontHookPtr          EQU  9B9Ch
restartClr           EQU  9BD0h
localTokStr          EQU  9D65h
keyForStr            EQU  9D76h
keyToStrRam          EQU  9D77h
sedMonSp             EQU  9D88h
bpSave               EQU  9D8Ah
userMem              EQU  9D95h
symTable             EQU  0FE66h
lcdinstport           = $10
lcddataport           = $11
newDispF        EQU  8		;Derivative mode flags

vat = progptr
gbuf equ plotsscreen

;Extra and misnamed romcalls defined in ION.INC:
_memfree = _memchk
_homeup		=$4558
_cleargbuf = _GrBufClr
_indicatorOn = _RunIndicOn
_indicatorOff = _RunIndicOff
_copygbuf = _GrBufCpy
_clrlcdf = _clrlcdfull
_cleartextshad = _ClrTxtShd
_clrscr = _ClrLCD
_clrscrf = _ClrScrnFull
_getk		= 4744h
_chksysvar = _ChkFindSym
_deletemem = _delmem
_lcdbusy = _LCD_BUSY

#ENDIF
trigflags       .EQU       0           ; IY OFFSET VALUE
trigdeg         .EQU       2
;
plotflags       .EQU       2           ; IY OFFSET VALUE
plotloc         .EQU       1
plotdisp        .EQU       2
;
grfmodeflags    .EQU       2           ; IY OFFSET VALUE
grffuncm        .EQU       4
grfpolarm       .EQU       5
grfparamm       .EQU       6
grfrecurm       .EQU       7
;
graphflags      .EQU       3           ; IY OFFSET VALUE
graphdraw       .EQU       0
graphcursor     .EQU       2
;
grfdbflags      .EQU       4           ; IY OFFSET VALUE
grfdot          .EQU       0
grfsimul        .EQU       1
grfgrid         .EQU       2
grfpolar        .EQU       3
grfnocoord      .EQU       4
grfnoaxis       .EQU       5
grflabel        .EQU       6
;
textflags       .EQU       5           ; IY OFFSET VALUE
textEraseBelow  .EQU       1
textScrolled    .EQU       2
textInverse     .EQU       3
;
apdFlags	EQU  8		;Automatic power-down
apdAble 	EQU    2	; 1=APD enabled
apdRunning	EQU    3	; 1=APD clock running
;
onflags         .EQU       9           ; IY OFFSET VALUE
onRunning       .EQU       3
onInterrupt     .EQU       4
;
statflags       .EQU       9           ; IY OFFSET VALUE
statsvalid      .EQU       6
;
fmtflags        .EQU       10          ; IY OFFSET VALUE
fmtExponent     .EQU       0
fmtEng          .EQU       1
;
nummode         .EQU       10          ; IY OFFSET VALUE
FMTREAL         .EQU       5
FMTRECT         .EQU       6
FMTPOLAR        .EQU       7
;
curflags        .EQU       12          ; IY OFFSET VALUE
curAble         .EQU       2
curOn           .EQU       3
curLock         .EQU       4
;
appflags        .EQU       13          ; IY OFFSET VALUE
appTextSave     .EQU       1
appAutoScroll   .EQU       2
;
PLOTFLAG2       .EQU       17          ; IY OFFSET VALUE
EXPR_PARAM      .EQU       3
EXPR_WRITING    .EQU       4
;
indicflags      .EQU       18          ; IY OFFSET VALUE
indicRun        .EQU       0
indicOnly       .EQU       2
;
shiftflags      .EQU       18          ; IY OFFSET VALUE
shift2nd        .EQU       3
shiftAlpha      .EQU       4
shiftALock      .EQU       6
;
tblflags        .EQU       19          ; IY OFFSET VALUE
AutoFill        .EQU       4
AutoCalc        .EQU       5
;
sgrflags        .EQU       20          ; IY OFFSET VALUE
grfSplit        .EQU       0
VertSplit       .EQU       1
WRITE_ON_GRAPH  .EQU       4
textwrite       .EQU       7
;
appLwrCaseFlag  EQU  36
lwrCaseActive   EQU  3
;
asm_flag1       .EQU       33          ; IY OFFSET VALUE
asm_flag1_0     .EQU       0
asm_flag1_1     .EQU       1
asm_flag1_2     .EQU       2
asm_flag1_3     .EQU       3
asm_flag1_4     .EQU       4
asm_flag1_5     .EQU       5
asm_flag1_6     .EQU       6
asm_flag1_7     .EQU       7
;
asm_flag2       .EQU       34          ; IY OFFSET VALUE
asm_flag2_0     .EQU       0
asm_flag2_1     .EQU       1
asm_flag2_2     .EQU       2
asm_flag2_3     .EQU       3
asm_flag2_4     .EQU       4
asm_flag2_5     .EQU       5
asm_flag2_6     .EQU       6
asm_flag2_7     .EQU       7
;
asm_flag3       .EQU       35          ; IY OFFSET VALUE
asm_flag3_0     .EQU       0
asm_flag3_1     .EQU       1
asm_flag3_2     .EQU       2
asm_flag3_3     .EQU       3
asm_flag3_4     .EQU       4
asm_flag3_5     .EQU       5
asm_flag3_6     .EQU       6
asm_flag3_7     .EQU       7
.list
;End of file

#IFDEF APP

#IFNDEF NOAPPHEADER
.org $4000
;This is the application header definition area required for all apps.
 .db 080h,0Fh    ;Field: Program length
 .db   00h,00h,00h,00h ;Length=0 (N/A for unsigned apps)
 .db 080h,012h    ;Field: Program type
 #ifdef TI73
 	.db 01h, 02h  ;type=Shareware, TI-73
 #else
  .db   01h,04h  ;Type= Shareware, TI-83Plus
 #endif
 .db 080h,021h    ;Field: App ID
 .db   01h       ;Id = 1
 .db 080h,031h    ;Field: App Build
 .db   01h       ;Build = 1
 .db 080h,048h    ;Field: App Name
 .db APPNAME
 .db 080h,081h    ;Field: App Pages
 .db APPPAGES
 .db 080h,090h    ;No default splash screen
 .db 03h,026h ,09h,04h, 04h,06fh,01bh,80h     ;Field: Date stamp- 5/12/1999
 .db 02h,0dh,040h                             ;Dummy encrypted TI date stamp signature
 .db 0a1h ,06bh ,099h ,0f6h ,059h ,0bch ,067h 
 .db 0f5h ,085h ,09ch ,09h ,06ch ,0fh ,0b4h ,03h ,09bh ,0c9h 
 .db 03h ,032h ,02ch ,0e0h ,03h ,020h ,0e3h ,02ch ,0f4h ,02dh 
 .db 073h ,0b4h ,027h ,0c4h ,0a0h ,072h ,054h ,0b9h ,0eah ,07ch 
 .db 03bh ,0aah ,016h ,0f6h ,077h ,083h ,07ah ,0eeh ,01ah ,0d4h 
 .db 042h ,04ch ,06bh ,08bh ,013h ,01fh ,0bbh ,093h ,08bh ,0fch 
 .db 019h ,01ch ,03ch ,0ech ,04dh ,0e5h ,075h 
 .db 80h,7Fh      ;Field: Program Image length
 .db   0,0,0,0    ;Length=0, N/A
 .db   0,0,0,0    ;Reserved
 .db   0,0,0,0    ;Reserved
 .db   0,0,0,0    ;Reserved
 .db   0,0,0,0    ;Reserved
#ENDIF
#ENDIF


;end