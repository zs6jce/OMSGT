     H DATFMT(*ISO)
      ********************************************************************************************
      *
      *  Copyright : Remain BV, Netherlands. All rights reserved.
      *
      *  Program   : OMH055
      *  Function  : Route Maintenance
      *  Author    : Remain Software
      *
      ********************************************************************************************
      *
      *  Modification Log:
      *
      *  ####    Date    Programmer  Comment
      *  ---- ---------- ----------  -------------------------------------------------------------
      *
      ********************************************************************************************
      *
      *   INDICATOR USAGE:
      *
      *   KC - Exit program
      *   KL - Return
      *   KV - Print
      *
      *   40 - Process Subfile
      *
      *   50 - SFLDSP
      *   51 - SFLDSPCTL, SFLEND
      *   52 - SFLDLT & SFLCLR
      *
      *   91 - Printer Overflow
      *
      ********************************************************************************************
      * Files
     FOMH055D   CF   E             Workstn SFILE(OMH055S1:RRN1)
     FOMRCFL1   UF A E           K Disk
     FOMHDDL1   IF   E           K Disk
     FOMSTRL1   IF   E           K Disk
     FOMSTAL1   IF   E           K Disk
     FOMHDEL1   IF   E           K Disk
      ********************************************************************************************
     D                SDS
     D  PGMNAM           *PROC
     D  PGMSGQ               001    010
     D  @#WSID               244    253
     D  @#USER               254    263
      *
      * Parameter fields
     D P_Appc          S              5A                                        Application code
     D P_Err#          S             10A                                        Request number
      *
     D ActRev          S              3A                                        Request number
     D RevNeg          S              3A                                        Request number
     D WNCrhr          S              5  0
     D WNPlhr          S              5  0
      *
     D RRN             S              5  0
     D RRN1            S              5  0
     D Index           S                   Like(RRN)
     D WRK             S             15  5
      *
     D @VAR1           S             15
     D @VAR2           S             15
     D @VAR3           S             15
     D @ARRAY          S             80
      *
     D w_Changed       S               N
     D w_Chg           S               N
     D w_Add           S               N
     D w_Chk           S               N
     D w_Pmt           S               N
     D w_Del           S               N
     D ErrID           S              7A
     D ErrDta          S            255A
     D ErrChk          S              4A
     D SVUser          S                   Like(RCUser)
      *
     D K#Hedc          S                   Like(RCHedc)
     D K#Strc          S                   Like(RCStrc)
     D K#Stac          S                   Like(RCStac)
     D K#Actc          S                   Like(RCActc)
     D K#User          S                   Like(RCUser)
      *
     D P#Hedc          S                   Like(RCHedc)
     D P#Strc          S                   Like(RCStrc)
     D P#Stac          S                   Like(RCStac)
     D P#Actc          S                   Like(RCActc)
      *
     D P#APPC          S             10A
     D P#GRID          S             10A
     D P#USCL          S             10A
     D P#USID          S             50A
     D P#USDC          S             50A
      ********************************************************************************************
      *                  M  A  I  N  -  L  I  N  E
      ********************************************************************************************
      * Do all prerequisite checks
     C                   Exsr      R90_Init_Pgm
      * Process screen 1 (Select from subfile)
     C                   If        *InLR = *Off
     C                   Exsr      R94_Proc_Scr1
     C                   EndIf
      *
      ********************************************************************************************
      *    R50_Prompt - Prompt User field
      *******prompt*******************************************************************************
     C     R50_Prompt    BegSr
      *
     C                   Call      'OMX1192'
     C                   Parm                    P#HEDC
     C                   Parm                    P#USID
     C                   Parm                    P#USDC
      * Get the user id e-mail adress
     C                   Eval      RCUser = P#USID
     C                   Eval      K#Hedc = P#HEDC
     C                   Eval      K#User = P#USID
     C     $DE1K1        Chain     OMHDEL1
     C                   If        %Found(OMHDEL1)
     C                   Eval      FMEEMC = DEEMAC
     C                   Else
     C                   Eval      FMEEMC = *BLANKS
     C                   EndIf
      *
     C                   EndSr
      ********************************************************************************************
      *    R62_Fill_Scr2 - Fill fields for screen 2
      ********************************************************************************************
     C     R62_Fill_Scr2 BegSr
      *
     C                   If        not(W_Chg)
      * If Add mode clear fields
     C                   Eval      RCUSER = *Blanks
     C                   Eval      RCSTRC = *Blanks
     C                   Eval      RCACTC = '0'
     C                   Eval      FMEEMC = *Blanks
     C                   Eval      RCEEMC = *Blanks
     C                   Eval      RCBCCE = *Blanks
     C                   Eval      RCMATF = *Blanks
     C                   Eval      RCMATL = *Blanks
     C                   Eval      RCMATM = *Blanks
     C                   Eval      RCSUL# = '0'
     C                   Eval      RCSTAT = '0'
     C                   Eval      RCEXTI = '0'
     C                   Else
      * If Change or delete mode get fields
     C     $RC1K2        Chain     OMRCFL1
     C                   Eval      FMUSER = RCUSER
     C                   Eval      FMACTC = RCACTC
     C                   Eval      FMEEMC = RCEEMC
     C                   Eval      FMBCCE = RCBCCE
     C                   Eval      FMMATF = RCMATF
     C                   Eval      FMMATL = RCMATL
     C                   Eval      FMMATM = RCMATM
     C                   Eval      FMSTAT = RCSTAT
     C                   EndIf
      *
     C                   EndSr
      ********************************************************************************************
      *    R64_Proc_Scr2 - Process screen 2
      ********************************************************************************************
     C     R64_Proc_Scr2 BegSr
      *
     C                   Do
      * Display screen 2
     C     R64_01        Tag
     C                   Write     MsgCtl
     C                   Exfmt     OMH055F2
     C                   Eval      *In60 = *Off
     C                   Eval      *In61 = *Off
     C                   Eval      *In62 = *Off
     C                   Eval      *In63 = *Off
     C                   Eval      *In64 = *Off
     C                   Eval      w_Chk = *Off
     C                   Eval      w_Pmt = *Off
     C                   Eval      ErrId = *Blanks
      * Clear message subfile
     C                   Exsr      ClrMsg
      * Process screen input
     C                   Select
      * Check if we must leave
     C                   When      *InKC OR *InKL Or w_Del                      F3  Exit
     C                   Leave                                                  F12 Return
      * Prompt
     C                   When      *InKD = *On                                  F4  Prompt
     C                   Eval      w_Pmt = *On
     C                   If        FLDW1 = 'RCUSER'
     C                   Exsr      R50_Prompt
     C                   Endif
      * Process Add/Chg input
     C                   Other
     C                   Eval      RCHEDC = FMHEDC
     C                   Eval      RCSTRC = FMSTRC
     C                   Eval      RCSTAC = FMSTAC
     C                   Eval      RCSUL# = '0'
     C                   Eval      RCEXTI = '0'
      *----------------------------------------------------------------
      * Check for e-mail adress when not prompted
      *----------------------------------------------------------------
     C                   Eval      K#Hedc = RCHEDC
     C                   Eval      K#User = RCUSER
     C     $DE1K1        Chain     OMHDEL1
     C                   If        %Found(OMHDEL1)
     C                   Eval      FMEEMC = DEEMAC
     C                   If        SVUSER <> RCUSER
     C                   Eval      SVUSER = RCUSER
     C                   Goto      R64_01
     C                   EndIf
     C                   Else
     C                   Eval      FMEEMC = 'Not Found'
     C                   EndIf
     C                   Eval      RCEEMC = FMEEMC
      *----------------------------------------------------------------
      * Check for errors on the input fields
      *----------------------------------------------------------------
     C                   Exsr      R69_ErrChk
     C                   If        w_Chk
     C                   Goto      R64_01
     C                   EndIf
      *----------------------------------------------------------------
      * If something is changed update the record
      *----------------------------------------------------------------
     C                   If        W_Chg
     C                   Eval      w_Changed = *On
     C                   Update    OMRCFR
     C                   Else
      *----------------------------------------------------------------
      * Check if record to be added is allowed
      *----------------------------------------------------------------
     C                   If        w_Add
     C                   Eval      K#User = RCUser
     C     $RC1K2        Chain     OMRCFL1
     C                   If        %Found(OMRCFL1)
     C                   Eval      *In60 = *On
     C                   Eval      MSGID = 'FILE001'
     C                   Eval      MSGF = 'OMHMSG'
     C                   Exsr      SndMsg
     C                   Goto      R64_01
     C                   Else
      *----------------------------------------------------------------
      * Check if record to be added not already exists
      *----------------------------------------------------------------
     C                   Eval      K#Hedc = RCHedc
     C                   Eval      K#User = RCUser
     C     $DE1K1        Chain     OMHDEL1
     C                   If        %Found(OMHDEL1)
     C                   Eval      w_Changed = *On
     C                   Write     OMRCFR
     C                   Else
     C                   Eval      *In60 = *On
     C                   Eval      MSGID = 'USRE003'
     C                   Eval      MSGF = 'OMHMSG'
     C                   Exsr      SndMsg
     C                   Goto      R64_01
     C                   EndIf
     C                   EndIf
     C                   EndIf
     C                   EndIf
      *
     C                   EndSl
      *
     C                   If        w_Pmt
     C                   Goto      R64_01
     C                   EndIf
      *
     C                   EndDo
      *
     C                   EndSr
      ********************************************************************************************
      *    R69_ErrChk    - Check for possible errors
      ********************************************************************************************
     C     R69_ErrChk    BegSr
      *
     C                   Select
     C                   When      RCUSER = *Blanks
     C                   Eval      *In60 = *On
     C                   Eval      w_Chk = *On
     C                   When      FMEEMC = *Blanks
     C                   Eval      *In60 = *On
     C                   Eval      *In61 = *On
     C                   Eval      w_Chk = *On
     C                   When      RCMATF = *Blanks
     C                   Eval      *In62 = *On
     C                   Eval      w_Chk = *On
     C                   When      RCMATL = *Blanks
     C                   Eval      *In63 = *On
     C                   Eval      w_Chk = *On
     C                   When      RCMATM = *Blanks
     C                   Eval      *In64 = *On
     C                   Eval      w_Chk = *On
     C                   EndSl
      *
     C                   Eval      MSGF = 'OMHMSG'
     C                   If        *In60 = *On Or
     C                             *In62 = *On Or
     C                             *In63 = *On Or
     C                             *In64 = *On
     C                   If        *In61 = *On
     C                   Eval      MSGDTA = RCUSER
     C                   Eval      MSGID = 'EMA0003'
     C                   Else
     C                   Eval      MSGID = 'REQ0002'
     C                   Eval      MSGDTA = 'field'
     C                   EndIf
     C                   Exsr      SndMsg
     C                   Else
     C                   Call      'OMH055C'
     C                   Parm                    RCMATL
     C                   Parm                    RCMATF
     C                   Parm                    RCMATM
     C                   Parm                    ErrID
     C                   Parm                    ErrDta
     C                   Parm                    ErrChk
      *
     C                   If        ErrID <> *BLANKS
     C                   Eval      w_Chk = *On
     C                   Select
     C                   When      ErrChk = '*LIB'
     C                   Eval      *In63 = *On
     C                   When      ErrChk = '*FIL'
     C                   Eval      *In62 = *On
     C                   When      ErrChk = '*MBR'
     C                   Eval      *In64 = *On
     C                   EndSl
     C                   Eval      MSGID = ErrID
     C                   Eval      MSGDTA = ErrDta
     C                   Eval      MSGF = 'QCPFMSG'
     C                   Exsr      SndMsg
     C                   EndIf
      *
     C                   EndIf
      *
     C                   EndSr
      ********************************************************************************************
      *    R80_Init_Sfl1 - Clear subfile
      ********************************************************************************************
     C     R80_Init_Sfl1 BegSr
      * Clear subfile
     C                   Eval      *In50 = *Off
     C                   Eval      *In51 = *Off
     C                   Eval      *In52 = *On
     C                   Write     OMH055C1
     C                   Eval      *In51 = *On
     C                   Eval      *In52 = *Off
     C                   Eval      RRN1 = *Zero
      *
     C                   EndSr
      ********************************************************************************************
      *    R82_Fill_Sfl1 - Fill subfile
      ********************************************************************************************
     C     R82_Fill_Sfl1 BegSr
      * Process file
     C     $RC1K1        Setll(E)  OMRCFL1
     C                   If        %Found(OMRCFL1)
     C     $RC1K1        Reade(E)  OMRCFR
      *
     C                   DoW       not %EOF(OMRCFL1)
     C                   Eval      S1Optn = *Zero
      * Write subfile record
     C                   Eval      RRN1 = RRN1 + 1
     C                   Write     OMH055S1
     C                   Eval      *In50 = *On
     C     $RC1K1        Reade(E)  OMRCFL1
     C                   EndDo
     C                   EndIf
      *
     C                   EndSr
      ********************************************************************************************
      *    R84_Proc_Sfl1 - Process Subfile
      ********************************************************************************************
     C     R84_Proc_Sfl1 BegSr
      *
     C     R84_01        Tag
     C                   Write     MsgCtl
      * Display Command screen
     C                   Write     OMH055H1
      * Display Subfile screen
     C                   Exfmt     OMH055C1
     C                   Eval      w_Changed = *Off
     C                   Eval      w_Chg = *Off
     C                   Eval      w_Add = *Off
     C                   Eval      w_Del = *Off
      * Clear Msg Subfile
     C                   Exsr      ClrMsg
      * Process Subfile screen
     C                   If        *InKC = *On Or                               F3  Exit
     C                             *InKL = *On                                  F12 Return
     C                   Eval      *InLR = *On
     C                   EndIf                                                  F3 or F12
      * F6 Add
     C                   If        *InKF = *On                                  F21 Print
     C                   Eval      W_Add = *On
     C                   Eval      *In70 = *Off
     C                   Exsr      R62_Fill_Scr2
     C                   Exsr      R64_Proc_Scr2
     C                   If        *InKL = *On
     C                   Eval      *InKL = *Off
     C                   Eval      w_Changed = *On
     C                   EndIf                                                  F21 Print
     C                   EndIf                                                  F21 Print
      *
     C                   If        *InLR = *Off                                 LR
      * Process Subfile records
     C                   If        RRN1 > 0                                     Subfile recs exist
     C                   Readc     OMH055S1                               40
     C                   DoW       *In40 = *Off
     C                   Select
      * Change Route
     C                   When      S1Optn = 2                                   5 = Detail Inquiry
     C                   Eval      S1Optn = *Zero
     C                   Eval      w_Changed = *On
     C                   Eval      W_Chg = *On
     C                   Eval      *In70 = *On
     C                   Eval      K#User = RCUser
     C                   Exsr      R62_Fill_Scr2
     C                   Exsr      R64_Proc_Scr2
     C                   Eval      *InKL = *Off
     C                   Update    OMH055S1
      * Delete Route
     C                   When      S1Optn = 4                                   5 = Detail Inquiry
     C                   Eval      w_Changed = *On
     C                   Eval      S1Optn = *Zero
     C                   Eval      W_Chg = *On
     C                   Eval      w_Del = *On
     C                   Eval      *In70 = *On
     C                   Eval      *In71 = *On
     C                   Eval      MSGID = 'DLT0001'
     C                   Eval      MSGF = 'OMHMSG'
     C                   Exsr      SndMsg
     C                   Eval      K#User = RCUser
     C                   Exsr      R62_Fill_Scr2
     C                   Exsr      R64_Proc_Scr2
     C                   If        not(*InKC) And
     C                             not(*InKL)
     C                   Eval      K#User = RCUser
     C     $RC1K2        Setll(E)  OMRCFL1
     C                   If        %Found(OMRCFL1)
     C     $RC1K2        Reade(E)  OMRCFL1
     C                   Delete    OMRCFL1
     C                   Endif
     C                   Endif
      *
     C                   Eval      *InKL = *Off
     C                   Eval      *In70 = *Off
     C                   Eval      *In71 = *Off
      *
     C                   EndSl
      *
     C                   Readc     OMH055S1                               40
     C                   EndDo
     C                   EndIf                                                  RRN1 > 0
      *
     C                   If        w_Changed
      * Clear Subfile
     C                   Exsr      R80_Init_Sfl1
      * Fill Subfile
     C                   Exsr      R82_Fill_Sfl1
     C                   GoTo      R84_01
     C                   Else
     C                   SetOn                                        LR
     C                   EndIf
     C                   EndIf                                                  LR
      *
     C                   EndSr
      ********************************************************************************************
      *    R90_Init_Pgm  - Init program (Operations, etc)
      ********************************************************************************************
     C     R90_Init_Pgm  BegSr
      *
     C     *Entry        Plist
     C                   Parm                    P#Hedc
     C                   Parm                    P#Strc
     C                   Parm                    P#Stac
      *
     C     $X901C        PLIST
     C                   PARM                    MSGID             7
     C                   PARM                    MSGF             10
     C                   PARM      '*LIBL'       MSGLIB           10
     C                   PARM                    MSGDTA          256
      *
     C     $X902C        PLIST
     C                   PARM                    PGMSGQ
      *
     C     $ST1K1        KList
     C                   Kfld                    K#Hedc
     C                   Kfld                    K#Stac
      *
     C     $TR1K1        KList
     C                   Kfld                    K#Hedc
     C                   Kfld                    K#Strc
      *
     C     $RC1K1        KList
     C                   Kfld                    K#Hedc
     C                   Kfld                    K#Strc
     C                   Kfld                    K#Stac
      *
     C     $RC1K2        KList
     C                   Kfld                    K#Hedc
     C                   Kfld                    K#Strc
     C                   Kfld                    K#Stac
     C                   Kfld                    K#User
      *
     C     $DE1K1        KList
     C                   Kfld                    K#Hedc
     C                   Kfld                    K#User
      *
     C                   Eval      K#Hedc = P#Hedc
     C                   Eval      K#Strc = P#Strc
     C                   Eval      K#Stac = P#Stac
      *
     C                   Eval      FMHedc = P#Hedc
     C                   Eval      FMStrc = P#Strc
     C                   Eval      FMStac = P#Stac
     C     K#Hedc        Chain     OMHDDL1
     C                   Eval      FMHedd = DDHEDD
      *
     C     $TR1K1        Chain     OMSTRL1
     C                   If        %Found(OMSTRL1)
     C                   Eval      FMStrd = TRSTRD
     C                   Else
     C                   Eval      FMStrd = 'All status routes'
     C                   EndIf
      *
     C     $ST1K1        Chain     OMSTAL1
     C                   If        %Found(OMSTAL1)
     C                   Eval      FMStad = TASTAD
     C                   Else
     C                   Eval      FMStad = 'All status'
     C                   EndIf
      *
     C                   EndSr
      ********************************************************************************************
      *    R94_Proc_Scr1 - Process screen 1
      ********************************************************************************************
     C     R94_Proc_Scr1 BegSr
      * Clear Subfile
     C                   Exsr      R80_Init_Sfl1
      * Fill Subfile
     C                   Exsr      R82_Fill_Sfl1
      * Process Subfile
     C                   Exsr      R84_Proc_Sfl1
      *
     C                   EndSr
      ********************************************************************************************
      *    *Inzsr        - Init program (Definitons, etc.)
      ********************************************************************************************
     C     *Inzsr        BegSr
      *
     C                   Eval      K#Hedc = P#Hedc
     C                   Eval      K#Strc = P#Strc
     C                   Eval      K#Stac = P#Stac
     C                   Eval      K#Actc = P#Actc
      *
     C                   EndSr
      ********************************************************************************************
      *    SndMsg        - Send a message
      ********************************************************************************************
     C     SndMsg        Begsr
      *
      *          ------------------------------------------
      *          Call Program sender
      *          ------------------------------------------
     C                   CALL      'OMH901C'     $X901C
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   Endsr
      ********************************************************************************************
      *    ClrMsg        - Clear a message
      ********************************************************************************************
     C     ClrMsg        Begsr
      *
      *          ------------------------------------------
      *          Call message queue clearing program
      *          ------------------------------------------
     C                   CALL      'OMH902C'     $X902C
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   Endsr
