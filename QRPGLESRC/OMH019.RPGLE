     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH019                                        *
      * Function ...: Incident type maintenance                     *
      * Module .....: Request Management                            *
      * Author .....: Remain Software                               *
      * Parameters .: P#HEDC - Helpdesk code (optional)             *
      *               P#STAT - Status                               *
      *                                                             *
      * This program is used to enter the incident types.           *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used files                                                  *
      * ----------------------------------------------------------- *
      * OMHDDL1  - Helpdesk definition logical                      *
      * OMHITL1  - Helpdesk incident types logical                  *
      * OMHTVL1  - Selections and default subsets                   *
      * OMHDEL1  - Helpdesk employee logical                        *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Sub programs                                                *
      * ----------------------------------------------------------- *
      * OMH901C    - Send message to calling program                *
      * OMH902C    - Clear program message queue                    *
      * OMH904C    - Get message data from message file             *
      * OMX105     - Select helpdesk code                           *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators                                             *
      * ----------------------------------------------------------- *
      *                                                             *
      * 99     - General purpose indicator.                         *
      *                                                             *
      * LOOPxx - Indicator to handle xx screen main loop.           *
      * ERORxx - Indicator to handle xx screen error loop.          *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Files                                                       *
      * ----------------------------------------------------------- *
     FOMHDDL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(INFO2)
     FOMHITL1   UF A E           K DISK    INFSR(*PSSR)
     F                                     INFDS(INFO3)
     FOMHTVL1   UF A E           K DISK    INFSR(*PSSR)
     F                                     INFDS(INFO4)
     FOMHDEL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(INFO5)
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Extension specifications                                    *
      *                                                             *
      * VAR   Contains substitution text                            *
      * MSD   Contains variable message id (compile time array)     *
      * MC    Used to fill message variables in message data parm.  *
      * TXT   Contains screen texts in the available language       *
      * TXD   Contains variable message id (compile time array)     *
      * ----------------------------------------------------------- *
     D VAR             S            256    DIM(20)
     D MSD             S              7    DIM(20) CTDATA PERRCD(1)
     D TXT             S            256    DIM(20)
     D TXD             S              7    DIM(20) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
      *COPY QCPYSRC,OMSREFREN
      * ----------------------------------------------------------- *
      * Input specifications                                        *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Reference file is used for field definition
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          File information data structures
      *          ------------------------------------------
     D INFO2           DS            57
     D INFO3           DS            57
     D INFO4           DS            57
     D INFO5           DS            57
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  E#MSID                40     46
     D  E#FILE               201    208
     D  D#USID               254    263
      *
      *          ------------------------------------------
      *          P#USI - USRI0200 Format
      *
      *          The following table lists the fields in the USRI0200
      *          (user information) format.
      *
      *          UI2BR# (B4)   Bytes returned to buffer
      *          UI2BA# (B4)   Bytes available for buffer
      *          UI2UPC (C10)  User profile code
      *          UI2UCC (C10)  User class code
      *          UI2SAC (C15)  Special authority code
      *          UI2GPC (C10)  Group profile code
      *          UI2OWC (C10)  Owner code
      *          UI2GAC (C10)  Group authority code
      *          UI2LAC (C10)  Limit capabilities code
      *          ------------------------------------------
     D P#USI           DS
     D UI2BR#                         9B 0
     D UI2BA#                         9B 0
     D UI2UPC                        10
     D UI2UCC                        10
     D UI2SAC                        15
     D UI2GPC                        10
     D UI2OWC                        10
     D UI2GAC                        10
     D UI2LAC                        10
      *
      *          ------------------------------------------
      *          Initialize the record number array
      *          ------------------------------------------
     D                 DS
     D B1                             9B 0 INZ(0)
     D  P#RNRA                        4    OVERLAY(B1)
      *
      *          ------------------------------------------
      *          Helpdesk data structures.
      *          ------------------------------------------
      /COPY QCPYLESRC,OMHLPDS
      *
      *          ------------------------------------------
      *          Error buffer for API call.
      *          ------------------------------------------
      /COPY QAPILESRC,ERR_BUFFER
      *
      *          ------------------------------------------
      *          Binary number definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFBINARY
      *
      *          ------------------------------------------
      *          Input buffer (7Kb)
      *          ------------------------------------------
     D P#BUF           DS          7168
      *
      *          ------------------------------------------
      *          Date and time stamp data structure
      *          ------------------------------------------
     D D#STMP          DS
     D D#STDT                         7  0
     D D#STTM                         6  0
      *
      *          ------------------------------------------
      *          Buffer for VR0001
      *          Constant holding the length of this buffer
      *          ------------------------------------------
     D V#0001          DS
     D B1PA1C                        10
     D B1PG1D                        50
     D B1PA2C                        10
     D B1PG2D                        50
     D B1PA3C                        10
     D B1PG3D                        50
     D B1F4TX                        10
     D L#VR01          C                   CONST(190)
      *
      *          ------------------------------------------
      *          Buffer for VR0002
      *          Constant holding the length of this buffer
      *          ------------------------------------------
     D V#0002          DS
     D B2DATE                         7  0
     D B2TIME                         6  0
     D B2DOTC                         1
     D L#VR02          C                   CONST(14)
      *
      *          ------------------------------------------
      *          Buffer for LD0101
      *          ------------------------------------------
     D V#0101          DS
     D B3OPTI                         4B 0
     D E3OPTI                         1
     D B3ITPC                         5
     D E3ITPC                         1
     D B3ITPD                        30
     D E3ITPD                         1
     D B3HEDC                         5
     D E3HEDC                         1
     D B3HEDD                        30
     D B3POST                         5
     D L#VR11          C                   CONST(81)
      *
      *          ------------------------------------------
      *          Buffer for VR0301
      *          ------------------------------------------
     D V#0301          DS
     D B4ACTC                        10
     D E4ACTC                         1
     D B4ITPC                         5
     D E4ITPC                         1
     D B4ITPD                        30
     D E4ITPD                         1
     D B4ADD                          1
     D B4CHG                          1
     D B4DSP                          1
     D B4CPY                          1
     D L#VR31          C                   CONST(52)
      *
      *          ------------------------------------------
      *          Constants used in program
      *          ------------------------------------------
     D C#NORM          C                   CONST('*NORM')
     D C#TERM          C                   CONST('*TERM')
      *
      *          ------------------------------------------
      *          Constants used for panel group
      *          ------------------------------------------
     D C#PNLG          C                   CONST('OMH019D   *LIBL     ')
     D C#OPTI          C                   CONST('FMOPTI    ')
     D C#ITPC          C                   CONST('FMITPC    ')
      *
     D ENTER           C                   CONST(0)
     D F3              C                   CONST(-4)
     D F6              C                   CONST(6)
     D F5              C                   CONST(5)
     D F12             C                   CONST(-8)
     D WAITEX          C                   CONST(-20)
     D C#MSGQ          C                   CONST('OMHMSG    *LIBL     ')
      * Prototype for OMH019
     D OMH019          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMX105'
     D OMX105          PR                  EXTPGM('OMX105')
     D B3HEDC_                             LIKE(B3HEDC)
     D W#HEDD_                             LIKE(W#HEDD)
      * Prototype for 'QUIADDLE'
     D QUIADDLE        PR                  EXTPGM('QUIADDLE')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#BUF_                              LIKEDS(P#BUF)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#VRNC_                             LIKE(P#VRNC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#OPTC_                             LIKE(P#OPTC)
     D P#LEHC_                             LIKE(P#LEHC)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'QUIADDLM'
     D QUIADDLM        PR                  EXTPGM('QUIADDLM')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#BUF_                              LIKEDS(P#BUF)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#VRNC_                             LIKE(P#VRNC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#OPTC_                             LIKE(P#OPTC)
     D P#LEHC_                             LIKE(P#LEHC)
     D P#NOR#_                             LIKE(P#NOR#)
     D P#RNRA_                             LIKE(P#RNRA)
     D P#RSZ#_                             LIKE(P#RSZ#)
     D P#RCC#_                             LIKE(P#RCC#)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'OMH901C'
     D OMH901C         PR                  EXTPGM('OMH901C')
     D D#MSID_                             LIKE(D#MSID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D D#MSGD_                             LIKE(D#MSGD)
      *
      *          ------------------------------------------
      *          Parameter list for OMH902C (Clear message queue)
      * Prototype for 'QUICLOA'
     D QUICLOA         PR                  EXTPGM('QUICLOA')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#CLOC_                             LIKE(P#CLOC)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'OMH902C'
     D OMH902C         PR                  EXTPGM('OMH902C')
     D PGMQ_                               LIKE(PGMQ)
      *
      *          ------------------------------------------
      *          Parameter list for OMH904C (Get message data text)
      * Prototype for 'QWCCVTDT'
     D QWCCVTDT        PR                  EXTPGM('QWCCVTDT')
     D P#FMIC_                             LIKE(P#FMIC)
     D P#IB_                               LIKE(P#IB)
     D P#FMOC_                             LIKE(P#FMOC)
     D P#BUF_                              LIKEDS(P#BUF)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'QUIDLTL'
     D QUIDLTL         PR                  EXTPGM('QUIDLTL')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'QUIDSPP'
     D QUIDSPP         PR                  EXTPGM('QUIDSPP')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#FNR#_                             LIKE(P#FNR#)
     D P#PNLC_                             LIKE(P#PNLC)
     D P#RDOC_                             LIKE(P#RDOC)
     D P#ERR_                              LIKE(P#ERR)
     D P#USTC_                             LIKE(P#USTC)
     D P#PSC#_                             LIKE(P#PSC#)
     D P#PMQC_                             LIKE(P#PMQC)
     D P#MRKC_                             LIKE(P#MRKC)
     D P#CPOC_                             LIKE(P#CPOC)
     D P#LLEC_                             LIKE(P#LLEC)
     D P#ELEC_                             LIKE(P#ELEC)
     D P#WTM#_                             LIKE(P#WTM#)
      * Prototype for 'QUIGETLE'
     D QUIGETLE        PR                  EXTPGM('QUIGETLE')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#BUF_                              LIKEDS(P#BUF)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#VRNC_                             LIKE(P#VRNC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#POSC_                             LIKE(P#POSC)
     D P#COPC_                             LIKE(P#COPC)
     D P#SECC_                             LIKE(P#SECC)
     D P#SHNC_                             LIKE(P#SHNC)
     D P#XOPC_                             LIKE(P#XOPC)
     D P#LEHC_                             LIKE(P#LEHC)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'QUIGETLM'
     D QUIGETLM        PR                  EXTPGM('QUIGETLM')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#BUF_                              LIKEDS(P#BUF)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#VRNC_                             LIKE(P#VRNC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#POSC_                             LIKE(P#POSC)
     D P#COPC_                             LIKE(P#COPC)
     D P#SECC_                             LIKE(P#SECC)
     D P#SHNC_                             LIKE(P#SHNC)
     D P#XOPC_                             LIKE(P#XOPC)
     D P#LEHC_                             LIKE(P#LEHC)
     D P#NOR#_                             LIKE(P#NOR#)
     D P#RSZ#_                             LIKE(P#RSZ#)
     D P#RCC#_                             LIKE(P#RCC#)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'QUIGETV'
     D QUIGETV         PR                  EXTPGM('QUIGETV')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#BUF_                              LIKEDS(P#BUF)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#VRNC_                             LIKE(P#VRNC)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'QUIOPNDA'
     D QUIOPNDA        PR                  EXTPGM('QUIOPNDA')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#PNLQ_                             LIKE(P#PNLQ)
     D P#APS#_                             LIKE(P#APS#)
     D P#ILV#_                             LIKE(P#ILV#)
     D P#FSHC_                             LIKE(P#FSHC)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'QUIPUTV'
     D QUIPUTV         PR                  EXTPGM('QUIPUTV')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#BUF_                              LIKEDS(P#BUF)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#VRNC_                             LIKE(P#VRNC)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'QUIRMVLE'
     D QUIRMVLE        PR                  EXTPGM('QUIRMVLE')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#XOPC_                             LIKE(P#XOPC)
     D P#LEHC_                             LIKE(P#LEHC)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'QUISETLA'
     D QUISETLA        PR                  EXTPGM('QUISETLA')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#LCOC_                             LIKE(P#LCOC)
     D P#PDVC_                             LIKE(P#PDVC)
     D P#DPAC_                             LIKE(P#DPAC)
     D P#ATAC_                             LIKE(P#ATAC)
     D P#ERR_                              LIKE(P#ERR)
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      * Prototype for 'QUIUPDLE'
     D QUIUPDLE        PR                  EXTPGM('QUIUPDLE')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#BUF_                              LIKEDS(P#BUF)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#VRNC_                             LIKE(P#VRNC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#OPTC_                             LIKE(P#OPTC)
     D P#LEHC_                             LIKE(P#LEHC)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'OMH029'
     D OMH029          PR                  EXTPGM('OMH029')
     D P#OPTI_                             LIKE(P#OPTI)
     D P#HEDC_                             LIKE(P#HEDC)
     D P#SUL#_                             LIKE(P#SUL#)
     D P#STAT_                             LIKE(P#STAT)
     D D#D7HD_                             LIKE(D#D7HD)
     D D#D8HD_                             LIKE(D#D8HD)
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      * Prototype for 'OMH904C'
     D OMH904C         PR                  EXTPGM('OMH904C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGTXT_                             LIKE(MSGTXT)
      *
      *          ------------------------------------------
      *          Incident type keylist.
      *
      *          ------------------------------------------
      *          Parameter list for OMH905C (Get user profile)
      * Prototype for 'OMH961'
     D OMH961          PR                  EXTPGM('OMH961')
     D PGM_                                LIKEDS(PGM)
     D FILE_                               LIKE(FILE)
      * Prototype for 'QMHMOVPM'
     D QMHMOVPM        PR                  EXTPGM('QMHMOVPM')
     D P#MSKC_                             LIKE(P#MSKC)
     D P#MSTA_                             LIKE(P#MSTA)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#PMQC_                             LIKE(P#PMQC)
     D P#PSC#_                             LIKE(P#PSC#)
     D P#ERR_                              LIKE(P#ERR)
      * *ENTRY Interface for Main Procedure
     D OMH019          PI
     D P#HEDC                         5
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D EROR01          S                   LIKE(INDI)
     D EROR03          S                   LIKE(INDI)
     D EROR04          S                   LIKE(INDI)
     D FILE            S             57
     D I#ADD           S                   LIKE(INDI)
     D I#CHG           S                   LIKE(INDI)
     D I#COPY          S                   LIKE(INDI)
     D I#DSP           S                   LIKE(INDI)
     D I#PSSR          S              1
     D IX              S              5  0
     D IY              S              5  0
     D LOOP01          S                   LIKE(INDI)
     D LOOP03          S                   LIKE(INDI)
     D LOOP04          S                   LIKE(INDI)
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#GRCL          S                   LIKE(USID)
     D P#GRID          S                   LIKE(USID)
     D P#HEDD          S                   LIKE(HEDD)
     D P#IB            S             10
     D P#MSTA          S             40
     D P#OPTI          S                   LIKE(INDI)
     D P#SUL#          S                   LIKE(SUL#)
     D P#USCL          S                   LIKE(USID)
     D P#USDC          S                   LIKE(USDC)
     D P#USID          S                   LIKE(USID)
     D S#HEDC          S                   LIKE(HEDC)
     D S#ITPC          S                   LIKE(ITPC)
     D S1LEHC          S                   LIKE(P#LEHC)
     D S4LEHC          S                   LIKE(P#LEHC)
     D W#HEDD          S                   LIKE(HEDD)
     D W1LEHC          S                   LIKE(P#LEHC)
     D W4LEHC          S                   LIKE(P#LEHC)
      *----------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *----------------------------------------------------------------------
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Main line                                                   *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Parameters
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Start screen processing.
      *          ------------------------------------------
     C                   EXSR      SRSC01
      *
      *          ------------------------------------------
      *          Close the program
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC01 - Screen 1 handling.                                  * /
      *                                                              * /
      * This routine sets up the first panel. This panel displays    * /
      * all available helpdesk definitions.                          * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSC01        BEGSR
      *
      *          ------------------------------------------
      *          Init display 1
      *          ------------------------------------------
     C                   EXSR      SRIN01
      *
      *          ------------------------------------------
      *          Do until end of display requested
      *          ------------------------------------------
     C                   DOU       LOOP01 = '0'                                 B01
      *
      *          ------------------------------------------
      *          Do until no more errors in entry fields
      *          ------------------------------------------
     C                   DOU       EROR01 = '0'                                 B02
      *
      *          ------------------------------------------
      *          Set the date and time.
      *          ------------------------------------------
     C                   EXSR      SRDATE
      *
      *          ------------------------------------------
      *          Display the first panel.
      *          ------------------------------------------
     C                   MOVEL     'PA01'        P#PNLC
     C                   MOVE      'Y'           P#RDOC
     C                   MOVE      'N'           P#USTC
     C                   Z-ADD     -1            P#WTM#
     C                   MOVE      'FRST'        P#MRKC
     C                   EXSR      SRDSPP
      *
      *          ------------------------------------------
      *          Reset display
      *          ------------------------------------------
     C                   EXSR      CLRMSG
     C                   EVAL      EROR01 = *OFF
     C                   EVAL      LOOP01 = *OFF
      *
      *          ------------------------------------------
      *          Function key processing.
      *          F3 & F12 is exit
      *          ------------------------------------------
     C                   IF        P#FNR# = F3                                  B03
     C                             OR P#FNR# = F12
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          F5=Refresh
      *          ------------------------------------------
     C                   IF        P#FNR# = F5                                  B03
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F4=Prompt
      *          ------------------------------------------
     C                   IF        P#FNR# > 400                                 B04
     C                             AND P#FNR# < 499
     C                   EXSR      SRF401
     C                   EVAL      LOOP01 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          No keys, edit.
      *          ------------------------------------------
     C                   EXSR      SRED01
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          End error loop
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Process the input or function key F6 (=Add).
      *          ------------------------------------------
     C                   IF        P#FNR# = F6                                  B02
     C                   EVAL      B3OPTI = *ZEROS
     C                   EXSR      SRSC03
     C                   MOVE      'A'           P#CPOC
     C                   MOVE      W1LEHC        P#LLEC
     C                   EVAL      LOOP01 = *ON
     C                   ELSE                                                   X02
     C                   EXSR      SRPR01
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End screen loop
      *          ------------------------------------------
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRIN01 - Screen 1 init                                       * /
      *                                                              * /
      * This routine initiates the first panel.                      * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Fill the screen data if not already done.
      *          ------------------------------------------
     C                   IF        B1PA1C = *BLANKS                             B01
     C                   EVAL      B1PG1D = TXT(1)
     C                   EVAL      %SUBST(B1PA1C:1:8) = 'OMH01901'
     C                   EVAL      B1PG2D = TXT(2)
     C                   EVAL      %SUBST(B1PA2C:1:8) = 'OMH01902'
     C                   EVAL      B1PG3D = TXT(3)
     C                   EVAL      %SUBST(B1PA3C:1:8) = 'OMH01903'
     C                   EVAL      B1F4TX = TXT(8)
      *
      *          ------------------------------------------
      *          Fill the variable pool.
      *          ------------------------------------------
     C                   MOVEL     'VR0001'      P#VRNC
     C                   Z-ADD     L#VR01        P#LOB#
     C                   MOVEL     V#0001        P#BUF
     C                   EXSR      SRPUTV
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the last entered helpdesk code.
      *          ------------------------------------------
     C                   IF        D#PARM > *ZEROS                              B01
     C                   IF        P#HEDC = '*PRV'                              B02
     C                   EVAL      P#HEDC = *BLANKS
     C                   ENDIF                                                  E02
     C                   EVAL      B3HEDC = P#HEDC
     C                   ENDIF                                                  E01
      *
     C                   EVAL      TVHEDC = *BLANKS
     C                   EVAL      TVUSID = D#USID
     C                   EVAL      %SUBST(TVFMTC:1:8) = 'OMH01901'
     C                   EVAL      TVRTV# = *ZEROS
      *
     C                   IF        B3HEDC = *BLANKS                             B01
     C     $RTVK1        CHAIN(N)  OMHTVL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B02
     C                   EVAL      B3HEDC = TVSDTA
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check if the code exists.
      *          ------------------------------------------
     C     B3HEDC        SETLL     OMHDDL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B01
     C                   EVAL      B3HEDC = *BLANKS
     C                   EVAL      B3HEDD = *BLANKS
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check if authorised.
      *          ------------------------------------------
     C     $HDEK1        CHAIN     OMHDEL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B01
     C                             AND DESUL# <> '9'                            B01
     C                   EVAL      B3HEDC = *BLANKS
     C                   EVAL      B3HEDD = *BLANKS
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do the refresh
      *          ------------------------------------------
     C                   EXSR      SRRF01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRF01 - Screen 1 refresh.                                   * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRF01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   MOVEL     'NEXT'        P#OPTC                          (SRADLE)
      *
      *          ------------------------------------------
      *          Delete the list.
      *          ------------------------------------------
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'VR0101'      P#VRNC
     C                   MOVEL     'LD0101'      P#LSTC
     C                   EXSR      SRDLTL
      *
      *          ------------------------------------------
      *          Fill the list if the helpdesk code is filled.
      *          ------------------------------------------
     C                   IF        B3HEDC <> *BLANKS                            B01
      *
      *          ------------------------------------------
      *          Get the helpdesk description
      *          ------------------------------------------
     C     B3HEDC        CHAIN     OMHDDL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEL     DDHEDD        B3HEDD
     C                   ENDIF                                                  E02
      *
     C                   EVAL      S#HEDC = B3HEDC
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Read the first record from the file
      *          ------------------------------------------
     C     B3HEDC        SETLL     OMHITL1
     C     B3HEDC        READE     OMHITL1
     C                   EVAL      *IN99 = %EOF
      *
     C                   EVAL      B3OPTI = 0
     C                   EVAL      E3OPTI = '0'
      *
      *          ------------------------------------------
      *          Do while records, fill buffer & write the list entry.
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
     C                   MOVE      ITITPC        B3ITPC
     C                   MOVE      ITITPD        B3ITPD
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRADLE
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     B3HEDC        READE     OMHITL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Start with default cursor positioning
      *          ------------------------------------------
     C                   MOVE      'NONE'        P#LLEC
     C                   MOVE      'D'           P#CPOC
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRED01 - Edit screen 1 input.                                * /
      *                                                              * /
      * This routine checks if the user entered correct data on      * /
      * this display.                                                *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          Get the control fields.
      *          ------------------------------------------
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   EXSR      SRGETV
     C                   MOVEL     P#BUF         V#0101
      *
      *          ------------------------------------------
      *          If the helpdesk code is blanks, then change it to
      *          the previous value.
      *          ------------------------------------------
     C                   IF        B3HEDC = *BLANKS                             B01
     C                   IF        S#HEDC <> *BLANKS                            B02
     C                   EVAL      B3HEDC = S#HEDC
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
     C                   IF        P#FNR# <> F6                                 B03
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          No previous value, error if f6 was pressed.
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   IF        P#FNR# = F6                                  B03
     C                   EVAL      MSGID = 'SELE004'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If not F6.
      *          ------------------------------------------
     C                   IF        P#FNR# <> F6                                 B01
      *
      *          ------------------------------------------
      *          Read all the options, and check if they are allowed.
      *          Fill the search data.
      *          ------------------------------------------
     C                   EVAL      B3OPTI = *ZEROS
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Fill the search parameters and get the first entry.
      *          ------------------------------------------
     C                   MOVEL     'LD0101  '    P#LSTC
     C                   MOVE      'FSLT'        P#POSC
     C                   MOVE      'Y'           P#COPC
     C                   MOVEL     'NE'          P#SECC
     C                   MOVE      C#OPTI        P#SECC
     C                   MOVE      'N'           P#XOPC
     C                   EXSR      SRGTLE
      *
      *          ------------------------------------------
      *          If the helpdesk code was changed and entries where
      *          made then this is not allowed.
      *          ------------------------------------------
     C                   IF        S#HEDC <> B3HEDC                             B02
     C                             AND P#LEHC <> '****'
     C                   EVAL      E3HEDC = *ON
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Process all options.
      *          ------------------------------------------
     C                   DOW       P#LEHC <> '****'                             B03
     C                   MOVEL     P#BUF         V#0101
      *
      *          ------------------------------------------
      *          Check if the record is still active
      *          ------------------------------------------
     C     $HITK1        SETLL     OMHITL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B04
     C                   EVAL      EROR01 = *ON
     C                   EVAL      E3OPTI = *ON
     C                   EVAL      MSGID = 'FILE002'
     C                   EXSR      SNDMSG
     C                   MOVE      'SAME'        P#OPTC
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRUPLE
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Get the next.
      *          ------------------------------------------
     C                   EVAL      B3OPTI = *ZEROS
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
      *
     C                   MOVE      'NSLT'        P#POSC
     C                   EXSR      SRGTLE
     C                   ENDDO                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If F6 and the helpdesk code was changed then
      *          this is not allowed.
      *          ------------------------------------------
     C                   IF        P#FNR# = F6                                  B01
     C                             AND S#HEDC <> B3HEDC
     C                   EVAL      E3HEDC = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Send 'You can not change the list ...' message if
      *          options where typed or a f6=add initiated.
      *          ------------------------------------------
     C                   IF        E3HEDC = *ON                                 B01
     C                   EVAL      MSGDTA = VAR(3)
     C                   EVAL      MSGID = 'LST0002'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If no errors and the helpdesk code was changed
      *          then check if exsists.
      *          ------------------------------------------
     C                   IF        EROR01 <> *ON                                B01
     C                             AND S#HEDC <> B3HEDC
     C     B3HEDC        SETLL     OMHDDL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEA     VAR(3)        MC(1)
     C                   MOVEA     B3HEDC        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   EVAL      E3HEDC = *ON
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Check if the user is authorized to it.
      *          ------------------------------------------
     C     $HDEK1        SETLL     OMHDEL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B03
     C                   MOVEA     VAR(3)        MC(1)
     C                   MOVEA     B3HEDC        MC(33)
     C                   EVAL      MC(65) = '1'
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'USG0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   EVAL      E3HEDC = *ON
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Position if no errors where found.
      *          ------------------------------------------
     C                   IF        B3POST <> *BLANKS                            B01
     C                             AND EROR01 = *OFF
     C                   EXSR      SRPO01
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRF401 - F4 handler for screen 1                             * /
      *                                                              * /
      * This routine handles the F4=Prompt requests for the first    * /
      * display.                                                     *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRF401        BEGSR
      *
      *          ------------------------------------------
      *          401 is the helpdesk code.
      *          ------------------------------------------
     C                   IF        P#FNR# = 401                                 B01
     C                   EVAL      S#HEDC = B3HEDC
     C                   CALLP     OMX105 ( B3HEDC : W#HEDD )
      *
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
      *
     C                   IF        B3HEDC <> S#HEDC                             B02
     C                   EXSR      SRCHGH
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPR01 - Process screen 1.                                   * /
      *                                                              * /
      * This routine processes screen 1. All the options from the    * /
      * list, or the F6=Add key, are processed.                      *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          Save the helpdesk code.
      *          ------------------------------------------
     C     $RTVK1        CHAIN     OMHTVL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   EVAL      %SUBST(TVSDTA:1:5) = B3HEDC
     C                   IF        *IN99 = *ON
     C                   WRITE     OMHTVR
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMHTVR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Confirm delete of requested records (if any).
      *          ------------------------------------------
     C                   EXSR      SRDC01
      *
      *          ------------------------------------------
      *          If not cancel then process the list.
      *          ------------------------------------------
     C                   IF        P#FNR# <> F12                                B02
      *
      *          ------------------------------------------
      *          Set the search value.
      *          ------------------------------------------
     C                   EVAL      B3OPTI = 0
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Get the first list entry.
      *          ------------------------------------------
     C                   EVAL      P#BUF = *BLANKS
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'LD0101'      P#LSTC
     C                   MOVE      'FSLT'        P#POSC
     C                   MOVE      'Y'           P#COPC
     C                   MOVEL     'NE'          P#SECC
     C                   MOVE      C#OPTI        P#SECC
     C                   MOVE      *BLANKS       P#SHNC
     C                   MOVE      'N'           P#XOPC
     C                   EXSR      SRGTLE
      *
      *          ------------------------------------------
      *          Do while records found.
      *          ------------------------------------------
     C                   DOW       P#LEHC <> '****'                             B03
     C                   MOVEL     P#BUF         V#0101
      *
     C                   EVAL      LOOP01 = *ON
     C                   MOVE      P#LEHC        W1LEHC
     C                   MOVE      P#LEHC        S1LEHC
      *
      *          ------------------------------------------
      *          4=Delete
      *          ------------------------------------------
     C                   IF        B3OPTI = 4                                   B04
     C     $HITK1        DELETE    OMHITL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Remove the list entry.
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF                                 B05
     C                   EXSR      SRRMLE
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     B3ITPC        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'DLT0017'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E05
      *
     C                   EVAL      B3OPTI = 0
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          2=Change
      *          3=Copy
      *          5=Display
      *          ------------------------------------------
     C                   ELSE                                                   X04
     C                   IF        B3OPTI = 2                                   B05
     C                             OR B3OPTI = 3
     C                             OR B3OPTI = 5
     C                   EVAL      S#ITPC = B3ITPC
     C                   EXSR      SRSC03
     C                   IF        P#FNR# = F12                                 B06
     C                   LEAVE
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          Update the list entry if not delete. This also sets
      *          the option field to zeros for the next search.
      *          ------------------------------------------
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'LD0101'      P#LSTC
     C                   MOVE      'N'           P#XOPC
     C                   MOVEL     'VR0101'      P#VRNC
     C                   MOVEL     'HNDL'        P#POSC
     C                   MOVE      S1LEHC        P#SHNC
     C                   MOVE      'Y'           P#COPC
     C                   EXSR      SRGTLE
     C                   MOVEL     P#BUF         V#0101
      *
     C                   EVAL      B3OPTI = 0
     C                   MOVEL     V#0101        P#BUF
     C                   MOVE      'SAME'        P#OPTC
     C                   EXSR      SRUPLE
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Get next
      *          ------------------------------------------
     C                   EVAL      P#BUF = *BLANKS
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'VR0101'      P#VRNC
     C                   MOVEL     'LD0101'      P#LSTC
     C                   MOVE      'FSLT'        P#POSC
     C                   MOVE      'Y'           P#COPC
     C                   MOVEL     'NE'          P#SECC
     C                   MOVE      C#OPTI        P#SECC
     C                   MOVE      *BLANKS       P#SHNC
     C                   MOVE      'N'           P#XOPC
     C                   EXSR      SRGTLE
     C                   ENDDO                                                  E03
      *
      *          ------------------------------------------
      *          If cancel then quit processing.
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   EVAL      LOOP01 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Set positioning options.
      *          ------------------------------------------
     C                   MOVE      'A'           P#CPOC
     C                   MOVE      W1LEHC        P#LLEC
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPO01 - Screen 1 positioning                                *
      *                                                              *
      * This routine positiones the list if the position to field    *
      * is changed for screen 1.                                     *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPO01        BEGSR
      *
      *          ------------------------------------------
      *          Fill the search data.
      *          ------------------------------------------
     C                   EVAL      B3ITPC = B3POST
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Fill the search parameters and get the first entry.
      *          ------------------------------------------
     C                   MOVE      'FSLT'        P#POSC
     C                   MOVE      'Y'           P#COPC
     C                   MOVEL     'GE'          P#SECC
     C                   MOVE      C#ITPC        P#SECC
     C                   MOVE      'N'           P#XOPC
     C                   EXSR      SRGTLE
      *
      *          ------------------------------------------
      *          If not found, then position on last.
      *          ------------------------------------------
     C                   IF        P#LEHC = '****'                              B01
     C                   MOVE      'LAST'        P#POSC
     C                   EXSR      SRGTLE
      *
      *          ------------------------------------------
      *          If greater then position previous.
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   MOVEL     P#BUF         V#0101
     C                   IF        B3ITPC > B3POST                              B02
     C                   MOVE      'PREV'        P#POSC
     C                   MOVE      'N'           P#COPC
     C                   EXSR      SRGTLE
     C                   IF        P#LEHC = '****'                              B03
     C                   MOVE      'TOP '        P#LEHC
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Position
      *          ------------------------------------------
     C                   MOVE      'SAME'        P#LCOC
     C                   MOVE      'S'           P#ATAC
     C                   IF        P#LEHC <> '****'                             B01
     C                   MOVE      P#LEHC        P#DPAC
     C                   ELSE                                                   X01
     C                   MOVE      'BOT '        P#DPAC
     C                   ENDIF                                                  E01
     C                   EXSR      SRSLAT
      *
      *          ------------------------------------------
      *          Blank the position to.
      *          ------------------------------------------
     C                   EVAL      B3POST = *BLANKS
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRDC01 - Delete confirmation for list 1                      *
      *                                                              *
      * This routine collects all entries with option '4' in the     *
      * first list and copies them to the second list and the list   *
      * is displayed.                                                *
      *                                                              *
      * Output  - P#FNR#     100 (confirmed)                         *
      *                      F12 (cancel requested)                  *
      *                        0 (no delete requests where done)     *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRDC01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      *IN99 = *OFF
      *
      *          ------------------------------------------
      *          Delete the confirmation list.
      *          ------------------------------------------
     C                   MOVEL     'LD0201'      P#LSTC
     C                   EXSR      SRDLTL
      *
      *          ------------------------------------------
      *          Set the search value.
      *          ------------------------------------------
     C                   EVAL      B3OPTI = 4
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Get list multiple entries (10 at a time)
      *          ------------------------------------------
     C                   EVAL      P#BUF = *BLANKS
     C     L#VR11        MULT      10            P#LOB#
     C                   MOVEL     'LD0101'      P#LSTC
     C                   MOVE      'FSLT'        P#POSC
     C                   MOVE      'Y'           P#COPC
     C                   MOVEL     'EQ'          P#SECC
     C                   MOVE      C#OPTI        P#SECC
     C                   MOVE      *BLANKS       P#SHNC
     C                   MOVE      'N'           P#XOPC
     C                   Z-ADD     10            P#NOR#
     C                   Z-ADD     L#VR11        P#RSZ#
     C                   EXSR      SRGTLM
      *
      *          ------------------------------------------
      *          Do while records found.
      *          ------------------------------------------
     C                   DOW       P#RCC# > *ZERO                               B01
     C                   EVAL      *IN99 = *ON
      *
      *          ------------------------------------------
      *          Add list multiple entries.
      *          ------------------------------------------
     C     L#VR11        MULT      P#RCC#        P#LOB#
     C                   MOVEL     'LD0201'      P#LSTC
     C                   MOVE      'NEXT'        P#OPTC
     C                   Z-ADD     P#RCC#        P#NOR#
     C                   Z-ADD     L#VR11        P#RSZ#
     C                   EXSR      SRADLM
      *
      *          ------------------------------------------
      *          Get the next ten entries.
      *          ------------------------------------------
     C                   EVAL      P#BUF = *BLANKS
     C     L#VR11        MULT      10            P#LOB#
     C                   MOVEL     'LD0101'      P#LSTC
     C                   MOVE      'NSLT'        P#POSC
     C                   MOVE      'Y'           P#COPC
     C                   MOVEL     'EQ'          P#SECC
     C                   MOVE      C#OPTI        P#SECC
     C                   MOVE      *BLANKS       P#SHNC
     C                   MOVE      'N'           P#XOPC
     C                   Z-ADD     10            P#NOR#
     C                   Z-ADD     L#VR11        P#RSZ#
     C                   EXSR      SRGTLM
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          If entries found. Set the date and time.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B01
     C                   EXSR      SRDATE
      *
      *          ------------------------------------------
      *          Display the confirmation panel.
      *          ------------------------------------------
     C                   MOVE      *BLANKS       P#PNLC
     C                   MOVE      'NONE'        P#LLEC
     C                   MOVE      'D'           P#CPOC
     C                   MOVEL     'PA02'        P#PNLC
     C                   MOVE      'N'           P#RDOC
     C                   MOVE      'N'           P#USTC
     C                   Z-ADD     -1            P#WTM#
     C                   MOVE      'FRST'        P#MRKC
     C                   EXSR      SRDSPP
      *
      *          ------------------------------------------
      *          Return F12 or 100.
      *          ------------------------------------------
     C                   IF        P#FNR# <> F12                                B02
     C                   Z-ADD     100           P#FNR#
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          No entries found, return 0.
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   EVAL      P#FNR# = 0
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC03 - Screen 3 handling.                                  *
      *                                                              *
      * This routine allows the user to add, change, display or      *
      * copy a incident type code.                                   *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSC03        BEGSR
      *
      *          ------------------------------------------
      *          Init display 1
      *          ------------------------------------------
     C                   EXSR      SRIN03
      *
      *          ------------------------------------------
      *          Do until end of display requested
      *          ------------------------------------------
     C                   DOU       LOOP03 = '0'                                 B01
      *
      *          ------------------------------------------
      *          Do until no more errors in entry fields
      *          ------------------------------------------
     C                   DOU       EROR03 = '0'                                 B02
      *
      *          ------------------------------------------
      *          Set the date and time.
      *          ------------------------------------------
     C                   EXSR      SRDATE
      *
      *          ------------------------------------------
      *          Display the panel.
      *          ------------------------------------------
     C                   MOVE      'Y'           P#RDOC
     C                   MOVE      'N'           P#USTC
     C                   MOVE      'NONE'        P#LLEC
     C                   MOVE      'D'           P#CPOC
     C                   Z-ADD     -1            P#WTM#
     C                   MOVE      'FRST'        P#MRKC
     C                   MOVEL     'PA03'        P#PNLC
     C                   EXSR      SRDSPP
      *
      *          ------------------------------------------
      *          Clear the programs message queue.
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Reset loop controllers
      *          ------------------------------------------
     C                   EVAL      EROR03 = *OFF
     C                   EVAL      LOOP03 = *OFF
      *
      *          ------------------------------------------
      *          Function key processing.
      *          F3=Exit, F12=Cancel
      *          ------------------------------------------
     C                   IF        P#FNR# = F12                                 B03
     C                   LEAVESR                                                (ENDSR)
     C                   ELSE                                                   X03
      *
     C                   IF        P#FNR# = F3                                  B04
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          F5 is refresh
      *          ------------------------------------------
     C                   IF        P#FNR# = F5                                  B05
     C                   EXSR      SRRF03
     C                   EVAL      EROR03 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          Edit
      *          ------------------------------------------
     C                   EXSR      SRED03
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If errors in display the re-enter error loop
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Process
      *          ------------------------------------------
     C                   EXSR      SRPR03
      *
      *          ------------------------------------------
      *          End screen loop
      *          ------------------------------------------
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C     SC0399        ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRIN03 - Screen 3 init.                                      *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRIN03        BEGSR
      *
      *          ------------------------------------------
      *          Determine the action.
      *          ------------------------------------------
     C                   EVAL      I#ADD = *OFF
     C                   EVAL      I#CHG = *OFF
     C                   EVAL      I#COPY = *OFF
     C                   EVAL      I#DSP = *OFF
     C                   EVAL      E4ACTC = *ON
      *
     C                   IF        B3OPTI = 2                                   B01
     C                   EVAL      I#CHG = *ON
     C                   EVAL      B4ACTC = TXT(5)
     C                   ELSE                                                   X01
      *
     C                   IF        B3OPTI = 3                                   B02
     C                   EVAL      I#COPY = *ON
     C                   EVAL      B4ACTC = TXT(7)
     C                   ELSE                                                   X02
      *
     C                   IF        B3OPTI = 5                                   B03
     C                   EVAL      I#DSP = *ON                                  E01
     C                   EVAL      B4ACTC = TXT(6)
     C                   ELSE                                                   X03
      *
     C                   IF        P#FNR# = F6                                  B04
     C                   EVAL      I#ADD = *ON
     C                   EVAL      B4ACTC = TXT(4)
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do the refresh
      *          ------------------------------------------
     C                   EXSR      SRRF03
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRF03 - Screen 3 refresh.                                   *
      *                                                              *
      * This routine fills the display fields. If F6 was pressed     *
      * then the fields are set to blanks. If 3=Copy or 2=Change     *
      * then the display fields are set to the values from the       *
      * list entry.                                                  *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRF03        BEGSR
      *
      *          ------------------------------------------
      *          Reset error variables.
      *          ------------------------------------------
     C                   EVAL      E4ITPC = *OFF
     C                   EVAL      E4ITPD = *OFF
      *
      *          ------------------------------------------
      *          If not add, then fill the bufferdata from the
      *          file fields, else reset.
      *          ------------------------------------------
     C                   IF        I#ADD = *OFF                                 B01
     C     $HITK1        CHAIN     OMHITL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   EVAL      B4ITPC = B3ITPC
     C                   MOVE      ITITPD        B4ITPD
     C                   ELSE                                                   X01
     C                   EVAL      B4ITPC = *BLANKS
     C                   EVAL      B4ITPD = *BLANKS
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If copy then clear the keyfield.
      *          ------------------------------------------
     C                   IF        I#COPY = *ON                                 B01
     C                   EVAL      B4ITPC = *BLANKS
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the display option indicator.
      *          ------------------------------------------
     C                   EVAL      B4CHG = I#CHG
     C                   EVAL      B4DSP = I#DSP
     C                   EVAL      B4CPY = I#COPY
     C                   IF        I#ADD = *ON                                  B01
     C                             OR I#COPY = *ON
     C                   EVAL      B4ADD = *ON
     C                   ELSE                                                   X01
     C                   EVAL      B4ADD = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the display file buffer
      *          ------------------------------------------
     C                   MOVEL     V#0301        P#BUF
     C                   Z-ADD     L#VR31        P#LOB#
     C                   MOVEL     'VR0301'      P#VRNC
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRED03 - Check screen 3 input                                *
      *                                                              *
      * This routine checks the screen 3 fields.                     *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRED03        BEGSR
      *
      *          ------------------------------------------
      *          Get the fields.
      *          ------------------------------------------
     C                   MOVEL     'VR0301'      P#VRNC
     C                   Z-ADD     L#VR31        P#LOB#
     C                   EXSR      SRGETV
     C                   MOVEL     P#BUF         V#0301
      *
      *          ------------------------------------------
      *          Reset the error dialog variables.
      *          ------------------------------------------
     C                   EVAL      E4ACTC = *ON
     C                   EVAL      E4ITPC = *OFF
     C                   EVAL      E4ITPD = *OFF
      *
      *          ------------------------------------------
      *          The type code may not be blanks.
      *          ------------------------------------------
     C                   IF        B4ITPC = *BLANKS                             B01
     C                   EVAL      MSGDTA = VAR(1)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      E4ITPC = *ON
     C                   EVAL      EROR03 = *ON
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          If addition then the report code may not exist
      *          ------------------------------------------
     C                   IF        I#ADD = *ON                                  B02
     C                             OR I#COPY = *ON                              B01
     C     $HITK2        SETLL     OMHITL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B03
     C                   EVAL      MSGID = 'FILE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR03 = *ON
     C                   EVAL      E4ITPC = *ON
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The description may not be blanks.
      *          ------------------------------------------
     C                   IF        B4ITPD = *BLANKS                             B01
     C                   EVAL      MSGDTA = VAR(2)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      E4ITPD = *ON
     C                   EVAL      EROR03 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Update the dialog variables.
      *          ------------------------------------------
     C                   MOVEL     V#0301        P#BUF
     C                   MOVEL     'VR0301'      P#VRNC
     C                   Z-ADD     L#VR31        P#LOB#
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRAE03 - Add a list entry in the list                        *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRAE03        BEGSR
      *
      *          ------------------------------------------
      *          Get the insert position.
      *          ------------------------------------------
     C                   EVAL      P#BUF = *BLANKS
     C                   MOVEL     '*NONE   '    P#VRNC
     C                   MOVEL     'LD0101'      P#LSTC
     C                   MOVE      'FSLT'        P#POSC
     C                   MOVE      'N'           P#COPC
     C                   MOVEL     'GT'          P#SECC
     C                   MOVE      C#ITPC        P#SECC
     C                   MOVE      *BLANKS       P#SHNC
     C                   MOVE      'N'           P#XOPC
     C                   EXSR      SRGTLE
      *
      *          ------------------------------------------
      *          If no position found, then it is the last.
      *          ------------------------------------------
     C                   IF        P#LEHC = '****'                              B01
     C                   MOVE      'LAST'        P#OPTC
     C                   ELSE                                                   X01
     C                   MOVE      'PREV'        P#OPTC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Add the list entry.
      *          ------------------------------------------
     C                   EVAL      B3OPTI = *ZEROS
     C                   EVAL      E3OPTI = *OFF
     C                   MOVE      ITITPC        B3ITPC
     C                   MOVE      ITITPD        B3ITPD
     C                   MOVEL     V#0101        P#BUF
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'VR0101'      P#VRNC
     C                   MOVEL     'LD0101'      P#LSTC
     C                   MOVE      *BLANKS       P#LEHC
     C                   EXSR      SRADLE
      *
      *          ------------------------------------------
      *          Position on this entry.
      *          ------------------------------------------
     C                   MOVE      P#LEHC        W1LEHC
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPR03 - Screen 3 processing.                                *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPR03        BEGSR
      *
      *          ------------------------------------------
      *          If this was copy or add then write.
      *          ------------------------------------------
     C                   IF        I#COPY = *ON                                 B01
     C                             OR I#ADD = *ON
     C                   EVAL      LOOP03 = *ON
      *
      *          ------------------------------------------
      *          If this is change then act only if any of the
      *          fields where changed.
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   IF        I#CHG = *ON                                  B02
     C                   IF        B4ITPC <> ITITPC                             B03
     C                             OR B4ITPD <> ITITPD
     C                   EVAL      LOOP03 = *ON
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If changes where made, then fill the file fields from
      *          the buffer.
      *          ------------------------------------------
     C                   IF        LOOP03 = *ON                                 B01
     C     $HITK2        CHAIN     OMHITL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
     C                   MOVE      B3HEDC        ITHEDC
     C                   MOVE      B4ITPC        ITITPC
     C                   MOVE      B4ITPD        ITITPD
      *
      *          ------------------------------------------
      *          Change the list entry.
      *          ------------------------------------------
     C                   IF        I#CHG = *ON                                  B02
     C                             OR I#DSP = *ON
     C                   EVAL      B3OPTI = *ZEROS
     C                   EVAL      B3ITPC = B4ITPC
     C                   EVAL      B3ITPD = B4ITPD
     C                   MOVEL     V#0101        P#BUF
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVE      'SAME'        P#OPTC
     C                   EXSR      SRUPLE
      *
      *          ------------------------------------------
      *          Add a list entry.
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   EXSR      SRAE03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Add
      *          ------------------------------------------
     C                   IF        I#ADD = *ON                                  B02
     C                             OR I#COPY = *ON
     C                   WRITE     OMHITR
      *
     C                   EVAL      I#ADD = *OFF
     C                   EVAL      I#COPY = *OFF
     C                   EVAL      I#CHG = *ON
      *
      *          ------------------------------------------
      *          Change
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   IF        I#CHG = *ON                                  B03
     C                   UPDATE    OMHITR
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Set the display field if it was changed.
      *          ------------------------------------------
     C                   IF        I#CHG = *ON                                  B02
     C                   EVAL      B4ACTC = TXT(5)
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill the display option indicator.
      *          ------------------------------------------
     C                   EVAL      B4CHG = I#CHG
     C                   EVAL      B4DSP = I#DSP
     C                   EVAL      B4CPY = I#COPY
      *
     C                   IF        I#ADD = *ON                                  B02
     C                             OR I#COPY = *ON
     C                   EVAL      B4ADD = *ON
     C                   ELSE                                                   X02
     C                   EVAL      B4ADD = *OFF
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Update the dialog variables.
      *          ------------------------------------------
     C                   MOVEL     V#0301        P#BUF
     C                   MOVEL     'VR0301'      P#VRNC
     C                   Z-ADD     L#VR31        P#LOB#
     C                   EXSR      SRPUTV
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRADLE - Add list entry                                      * /
      *                                                              * /
      * The add list entry (QUIADDLE) API adds one new entry to a    * /
      * list.                                                        * /
      *                                                              * /
      * Input        P#HNDC  Handle code                             *
      *              P#BUF   Variable buffer                         *
      *              P#LOB#  Length of buffer                        *
      *              P#VRNC  Variable record name                    *
      *              P#LSTC  List code                               *
      *              P#OPTC  Option                                  *
      * Both         P#ERR   Error parameter data structure          *
      *                                                              *
      * Output     - P#LEHC  List entry handle                       *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRADLE        BEGSR
      *
      *          ------------------------------------------
      *          Add list entry
      *          ------------------------------------------
     C                   CALLP     QUIADDLE ( P#HNDC : P#BUF : P#LOB# :
     C                             P#VRNC : P#LSTC : P#OPTC : P#LEHC :
     C                             P#ERR )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRADLM - Add list multiple entries                           *
      *                                                              *
      * This routine calls the add list multiple entry (QUIADDLM)    *
      * which write multiple record to the list.                     *
      *                                                              *
      * Input        P#HNDC  Handle code                             *
      *              P#BUF   Variable buffer                         *
      *              P#LOB#  Length of buffer                        *
      *              P#VRNC  Variable record name                    *
      *              P#LSTC  List code                               *
      *              P#OPTC  Option                                  *
      *              P#NOR#  Number of records                       *
      *              P#RNRA  Record numbers to write                 *
      *              P#RSZ#  Record size                             *
      *                                                              *
      * Output     - P#LEHC  List entry handle                       *
      *              P#RCC#  Record count                            *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRADLM        BEGSR
      *
      *          ------------------------------------------
      *          If more than zero
      *          ------------------------------------------
     C                   IF        P#NOR# > *ZEROS                              B01
      *
      *          ------------------------------------------
      *          Add list multiple entry
      *          ------------------------------------------
     C                   CALLP     QUIADDLM ( P#HNDC : P#BUF : P#LOB# :
     C                             P#VRNC : P#LSTC : P#OPTC : P#LEHC :
     C                             P#NOR# : P#RNRA : P#RSZ# : P#RCC# :
     C                             P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B02
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The value of this field is loaded in the high
      *          order halfword. Divide by FFFFh in order to shift
      *          it to the low order halfword.
      *          ------------------------------------------
     C                   IF        P#RCC# >= 65536                              B01
     C                   DIV       65536         P#RCC#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRCLAP - Close application                                   * /
      *                                                              * /
      * This routine closes the application by calling the close     * /
      * application API.                                             * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      *                                                              *
      * This subroutine sets the following default parameters for    *
      * this API call.                                               *
      *                                                              *
      * P#CLOS = 'M' Perform normal close operation                  *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRCLAP        BEGSR
      *
      *          ------------------------------------------
      *          Close the display application
      *          ------------------------------------------
     C                   EVAL      P#CLOC = 'M'
     C                   CALLP     QUICLOA ( P#HNDC : P#CLOC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * CLRMSG - Clear message subfile                               * /
      *                                                              * /
      * This routine calls a subprogram wich clears the message      * /
      * subfile.                                                     * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     CLRMSG        BEGSR
      *
      *          ------------------------------------------
      *          Call message queue clearing program
      *          ------------------------------------------
     C                   CALLP     OMH902C ( PGMQ )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRCVTD - Convert date and time format                        *
      *                                                              *
      * The convert date and time format API (QWCCVTDT) converts     *
      * date and time values from one format to another. It can      *
      * also be used to retrieve the current machine clock time and  *
      * return it in a specific format.                              *
      *                                                              *
      * Input      - P#FMIC  Input format code                       *
      *              P#IB    Input buffer                            *
      *              P#FMOC  Output format code                      *
      *                                                              *
      * Output     - P#BUF   Output buffer                           *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRCVTD        BEGSR
      *
      *          ------------------------------------------
      *          Convert Date and Time Format
      *          ------------------------------------------
     C                   CALLP     QWCCVTDT ( P#FMIC : P#IB : P#FMOC :
     C                             P#BUF : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRDATE - Set date and time                                   * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRDATE        BEGSR
      *
      *          ------------------------------------------
      *          Get the jobs date and time
      *          ------------------------------------------
     C                   MOVEL     '*CURRENT'    P#FMIC
     C                   EVAL      P#IB = *BLANKS
     C                   MOVEL     '*YMD    '    P#FMOC
     C                   EXSR      SRCVTD
     C                   MOVEL     P#BUF         D#STMP
      *
     C                   EVAL      B2DATE = D#STDT
     C                   EVAL      B2TIME = D#STTM
      *
     C                   IF        B2DOTC = '1'                                 B01
     C                   EVAL      B2DOTC = '0'
     C                   ELSE                                                   X01
     C                   EVAL      B2DOTC = '1'
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the variable pool.
      *          ------------------------------------------
     C                   MOVEL     'VR0002'      P#VRNC
     C                   Z-ADD     L#VR02        P#LOB#
     C                   MOVEL     V#0002        P#BUF
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRDLTL - Delete a list                                       * /
      *                                                              * /
      * This routine deletes a list by calling the QUIDLTL API.      * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *              P#LSTC  List code                               *
      * Both       - P#ERR   Error parameter data structure          *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRDLTL        BEGSR
      *
      *          ------------------------------------------
      *          Delete list
      *          ------------------------------------------
     C                   CALLP     QUIDLTL ( P#HNDC : P#LSTC : P#ERR )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRDSPP - Display panel.                                      * /
      *                                                              * /
      * This routine displays a panel by calling the display panel   * /
      * API.                                                         * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *              P#RDOC  Redisplay option  ('Y' or 'N')          *
      *              P#PNLC  Panel name                              *
      *              P#USTC  User task   'N'= New task               *
      *                                  'O'= Old task               *
      *              P#WTM#  Wait time   -1 = Indefinite wait time   *
      *                                   0 = Display panel & return *
      *                                  xx = Wait xx seconds        *
      *              P#MRKC Message key  'FRST'=Show all messages    *
      *                                  *BLANK=Don't show           *
      *                                                              *
      * Output     - P#FNR#  Requested function    0 = Enter         *
      *                                           -4 = Exit          *
      *                                           -8 = Cancel        *
      *                                          -10 = Prompt        *
      *                                          -20 = Exceed wait   *
      *                                           xx = User defined  *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * This subroutine sets the following default parameters for    *
      * this API call.                                               *
      *                                                              *
      * P#PSC# =  0         Use the message queue of program P#MSGQ  *
      * P#PMQQ =  '*CALLER' Program message queue to be used         *
      * P#LLEC =  'NONE'    Do not use list entry handler            *
      * P#ELEC =  'NONE'    Do not use error list entry              *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRDSPP        BEGSR
      *
      *          ------------------------------------------
      *          Display the panel
      *          ------------------------------------------
     C                   EVAL      P#PSC# = 0
     C                   EVAL      P#PMQC = 'OMH019 '
     C                   EVAL      P#ELEC = 'NONE'
     C                   CALLP     QUIDSPP ( P#HNDC : P#FNR# : P#PNLC :
     C                             P#RDOC : P#ERR : P#USTC : P#PSC# :
     C                             P#PMQC : P#MRKC : P#CPOC : P#LLEC :
     C                             P#ELEC : P#WTM# )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGTLE - Get list entry                                      * /
      *                                                              * /
      * This routine calls the get list entry API (QUIGETLE) which   * /
      * gets an entry from the list and is used for positioning.     * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *              P#LOB#  Length of buffer                        *
      *              P#VRNC  Variable record name                    *
      *              P#LSTC  List name                               *
      *              P#POSC  Positioning option code                 *
      *              P#COPC  Copy option code                        *
      *              P#SECC  Selection criteria code                 *
      *              P#SHNC  Selection handle code                   *
      *              P#XOPC  Extend option code                      *
      * Both       - P#ERR   Error parameter data structure          *
      *                                                              *
      * Output     - P#BUF   Buffer                                  *
      *              P#LEHC  List entry handle code                  *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * This subroutine sets the following default parameters for    *
      * this API call.                                               *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGTLE        BEGSR
      *
      *          ------------------------------------------
      *          Get list entry
      *          ------------------------------------------
     C                   CALLP     QUIGETLE ( P#HNDC : P#BUF : P#LOB# :
     C                             P#VRNC : P#LSTC : P#POSC : P#COPC :
     C                             P#SECC : P#SHNC : P#XOPC : P#LEHC :
     C                             P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   IF        D#MSID <> 'CPF6A98'                          B02
     C                             AND D#MSID <> 'CPF6A93'                      B02
     C                             AND D#MSID <> 'CPF6A92'                      B02
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X02
     C                   MOVE      '****'        P#LEHC
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGTLM - Get list multiple entries                           *
      *                                                              *
      * This routine calls the get list multiple entries API.        *
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *              P#LOB#  Length of buffer                        *
      *              P#VRNC  Variable record name                    *
      *              P#LSTC  List name                               *
      *              P#POSC  Positioning option code                 *
      *              P#COPC  Copy option code                        *
      *              P#SECC  Selection criteria code                 *
      *              P#SHNC  Selection handle code                   *
      *              P#XOPC  Extend option code                      *
      *              P#NOR#  Number of records                       *
      *              P#RSZ#  Record size                             *
      *                                                              *
      * Output     - P#BUF   Buffer                                  *
      *              P#LEHC  List entry handle code                  *
      *              P#RCC#  Record count                            *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * This subroutine sets the following default parameters for    *
      * this API call.                                               *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGTLM        BEGSR
      *
      *          ------------------------------------------
      *          Add list multiple entry
      *          ------------------------------------------
     C                   CALLP     QUIGETLM ( P#HNDC : P#BUF : P#LOB# :
     C                             P#VRNC : P#LSTC : P#POSC : P#COPC :
     C                             P#SECC : P#SHNC : P#XOPC : P#LEHC :
     C                             P#NOR# : P#RSZ# : P#RCC# : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   IF        D#MSID = 'CPF6A98'                           B02
     C                   EVAL      P#BUF = *BLANKS
     C                   MOVE      *BLANKS       D#MSID
     C                   EVAL      P#RCC# = *ZEROS
     C                   ELSE                                                   X02
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The value of this field is loaded in the high
      *          order halfword. Divide by 10000h in order to shift
      *          it to the low order halfword.
      *          ------------------------------------------
     C                   IF        P#RCC# >= 65536                              B01
     C                   DIV       65536         P#RCC#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGETV - Get dialog variable                                 * /
      *                                                              * /
      * This routine calls the get dialog variable API (QUIGETV)     * /
      * which gets a variable record.                                * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *              P#BUF   Input buffer                            *
      *              P#LOB#  Length of buffer                        *
      *              P#VRNC  Variable record name                    *
      *                                                              *
      * Output     - *NONE                                           *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGETV        BEGSR
      *
      *          ------------------------------------------
      *          Add list entry
      *          ------------------------------------------
     C                   CALLP     QUIGETV ( P#HNDC : P#BUF : P#LOB# :
     C                             P#VRNC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SROPDA - Open display application.                           * /
      *                                                              * /
      * This routine calls the open display application API QUIOPNDA * /
      *                                                              *
      * Input      - P#PNLQ  Qualified panelgroup name               *
      *              P#APS#  Application scope                       *
      *              P#ILV#  Interface level                         *
      *              P#FSHC  Full screen help code                   *
      *                                                              *
      * Output     - P#HNDC  Application handle                      *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * This subroutine sets the following default parameters for    *
      * this API call.                                               *
      *                                                              *
      * D#APPS = -1  Application scope is the calling program        *
      * D#ILVL =  2  Use interface level 2                           *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SROPDA        BEGSR
      *
      *          ------------------------------------------
      *          Open display application
      *          ------------------------------------------
     C                   CALLP     QUIOPNDA ( P#HNDC : P#PNLQ : P#APS# :
     C                             P#ILV# : P#FSHC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPUTV - Put dialog variable                                 * /
      *                                                              * /
      * This routine calls the put dialog variable API (QUIPUTV)     * /
      * which updates a variable record.                             * /
      *                                                              * /
      * Input      - P#HNDC  Application handle                      *
      *              P#BUF   Input buffer                            *
      *              P#LOB#  Length of buffer                        *
      *              P#VRNC  Variable record name                    *
      *                                                              *
      * Output     - *NONE                                           *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPUTV        BEGSR
      *
      *          ------------------------------------------
      *          Add list entry
      *          ------------------------------------------
     C                   CALLP     QUIPUTV ( P#HNDC : P#BUF : P#LOB# :
     C                             P#VRNC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRMLE - Remove list entry                                   *
      *                                                              *
      * This routine calls the remove list entry API (QUIRMVLE).     *
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *              P#LSTC  List name                               *
      *              P#XOPC  Extend option code                      *
      *                                                              *
      * Output     - P#LEHC  List entry handle code                  *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRMLE        BEGSR
      *
      *          ------------------------------------------
      *          Add list entry
      *          ------------------------------------------
     C                   CALLP     QUIRMVLE ( P#HNDC : P#LSTC : P#XOPC :
     C                             P#LEHC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#MSID <> *BLANKS                            B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSLAT - Set list attributes                                 * /
      *                                                              * /
      * This routine calls the set list attribute API (QUISETLA)     * /
      * which sets the list attributes.                              * /
      *                                                              * /
      * Input      - P#HNDC  Application handle                      *
      *              P#LSTC  List name                               *
      *              P#LCOC  List contents code                      *
      *              P#PDVC  Program dialog variable                 *
      *              P#DPAC  Display position attribute              *
      *              P#ATAC  Allow trim attribute code               *
      *                                                              *
      * Output     - *NONE                                           *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSLAT        BEGSR
      *
      *          ------------------------------------------
      *          Set list attributes
      *          ------------------------------------------
     C                   CALLP     QUISETLA ( P#HNDC : P#LSTC : P#LCOC :
     C                             P#PDVC : P#DPAC : P#ATAC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SNDMSG - Send message                                        * /
      *                                                              * /
      * This routine displays an error wich occured when the user    * /
      * keyed data validation.                                       * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SNDMSG        BEGSR
      *
      *          ------------------------------------------
      *          Call send message program
      *          ------------------------------------------
     C                   EVAL      MSGF = 'OMHMSG '
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C  ( MSGID : MSGF : MSGLIB :
     C                             MSGDTA )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRUPLE - Update list entry                                   * /
      *                                                              * /
      * This routine calls the update list entry API (QUIUPDLE).     * /
      *                                                              * /
      * Input        P#HNDC  Handle code                             *
      *              P#BUF   Variable buffer                         *
      *              P#LOB#  Length of buffer                        *
      *              P#VRNC  Variable record name                    *
      *              P#LSTC  List code                               *
      *              P#OPTC  Option                                  *
      *                                                              *
      * Output     - P#LEHC  List entry handle                       *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRUPLE        BEGSR
      *
      *          ------------------------------------------
      *          Update list entry
      *          ------------------------------------------
     C                   CALLP     QUIUPDLE ( P#HNDC : P#BUF : P#LOB# :
     C                             P#VRNC : P#LSTC : P#OPTC : P#LEHC :
     C                             P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCHGH - Change the Helpdesk                                *
      *                                                             *
      * This routine changes the helpdesk                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCHGH        BEGSR
      *
      *          ------------------------------------------
      *          If the HelpDesk code is blanks, then reset it.
      *          ------------------------------------------
     C                   IF        B3HEDC = *BLANKS                             B01
     C                   EVAL      B3HEDC = S#HEDC
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Change the helpdesk
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '9'
     C                   EVAL      P#HEDC = B3HEDC
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD )
      *
      *          ------------------------------------------
      *          If errors, reset
      *          ------------------------------------------
     C                   IF        P#STAT <> C#NORM                             B02
     C                   EVAL      E3HEDC = *ON
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Fill screen fields from parameters
      *          ------------------------------------------
     C                   EVAL      B3HEDC = P#HEDC
     C                   MOVEL     D8HEDD        B3HEDD
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * *INZSR - Program initialisation                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Indicator definitions
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      IX = *ZEROS
     C                   EVAL      IY = *ZEROS
      *
      *          ------------------------------------------
      *          Savefield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parmeterfield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          API parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter list for OMH029 (Get info)
      *          ------------------------------------------
     C     $MH029        PLIST
     C                   PARM                    P#OPTI
     C                   PARM                    P#HEDC
     C                   PARM                    P#SUL#
     C                   PARM                    P#STAT
     C                   PARM                    D#D7HD
     C                   PARM                    D#D8HD
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      *          ------------------------------------------
     C     $X901C        PLIST
     C                   PARM                    MSGID
     C                   PARM      'OMHMSG '     MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGDTA
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      *          ------------------------------------------
     C     $901C2        PLIST
     C                   PARM                    D#MSID
     C                   PARM      'QCPFMSG'     MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    D#MSGD
      *
      *          ------------------------------------------
      *          Parameter list for OMH902C (Clear message queue)
      *          ------------------------------------------
     C     $X902C        PLIST
     C                   PARM                    PGMQ
      *
      *          ------------------------------------------
      *          Parameter list for OMH904C (Get message data text)
      *          ------------------------------------------
     C     $X904C        PLIST
     C                   PARM                    MSGID
     C                   PARM      'OMHMSG'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGTXT
      *
      *          ------------------------------------------
      *          Parameter list for OMH905C (Get user profile)
      *          ------------------------------------------
     C     $X905C        PLIST
     C                   PARM                    P#USID
     C                   PARM                    P#USCL
     C                   PARM                    P#GRID
     C                   PARM                    P#GRCL
     C                   PARM                    P#USDC
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH9042 (Get message data text)
      *          ------------------------------------------
     C     $X9042        PLIST
     C                   PARM                    MSGID
     C                   PARM      'OMHDSP'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGTXT
      *
      *          ------------------------------------------
      *          Incident type keylist.
      *          ------------------------------------------
     C     $HITK1        KLIST
     C                   KFLD                    S#HEDC
     C                   KFLD                    B3ITPC
      *
     C     $HITK2        KLIST
     C                   KFLD                    B3HEDC
     C                   KFLD                    B4ITPC
      *
      *          ------------------------------------------
      *          Selections and default subsets file keylist.
      *          ------------------------------------------
     C     $RTVK1        KLIST
     C                   KFLD                    TVHEDC
     C                   KFLD                    TVUSID
     C                   KFLD                    TVFMTC
     C                   KFLD                    TVRTV#
      *
      *          ------------------------------------------
      *          Helpdesk employee keylist
      *          ------------------------------------------
     C     $HDEK1        KLIST
     C                   KFLD                    B3HEDC
     C                   KFLD                    D#USID
      *
      *          ------------------------------------------
      *          Retrieve authorization
      *          ------------------------------------------
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '9'
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD )
      *
      *          ------------------------------------------
      *          Continue if authorisation ok
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B01
      *
      *          ------------------------------------------
      *          Get the language text.
      *          ------------------------------------------
     C                   EXSR      SRGVAR                                       OMHMSG
     C                   EXSR      SRGTXT                                       OMHDSP
      *
      *          ------------------------------------------
      *          Open display application
      *          ------------------------------------------
     C                   MOVE      C#PNLG        P#PNLQ
     C                   Z-SUB     1             P#APS#
     C                   Z-ADD     2             P#ILV#
     C                   MOVE      'N'           P#FSHC
     C                   EXSR      SROPDA
      *
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRTTLR - Close the program                                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          Close the display application
      *          ------------------------------------------
     C                   EXSR      SRCLAP
      *
      *          ------------------------------------------
      *          Return to caller
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGTXT  -  Get display file text                             *
      *                                                              *
      * This subroutine gets the data needed for this program to     *
      * display on the panels.                                       *
      * It is called one time per program invocation.                *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGTXT        BEGSR
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 10                                 B01
     C                   EVAL      MSGID = TXD(MS)
      *
      *          ------------------------------------------
      *          Call subprogram
      *          ------------------------------------------
     C                   IF        MSGID <> *BLANKS                             B02
     C                   EVAL      MSGF = 'OMHDSP'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill message arrays
      *          ------------------------------------------
     C                   EVAL      %SUBST(TXT(MS):1:132) = MSGTXT
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGVAR  -  Get variable message text                         *
      *                                                              *
      * This subroutine gets the data needed for this program to     *
      * substitute in message texts.                                 *
      * It is called one time per program invocation.                *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGVAR        BEGSR
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 10                                 B01
     C                   EVAL      MSGID = MSD(MS)
      *
      *          ------------------------------------------
      *          Call subprogram
      *          ------------------------------------------
     C                   IF        MSGID <> *BLANKS                             B02
     C                   EVAL      MSGF = 'OMHMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill message arrays
      *          ------------------------------------------
     C                   EVAL      %SUBST(VAR(MS):1:132) = MSGTXT
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * *PSSR  - Error handler                                      *
      *                                                             *
      * Standard RPG error handler.                                 *
      * Make sure the data structure PGM is defined as the          *
      * program status data structure (SDS) with a length of 429.   *
      * The file information data structure is also passed, but     *
      * not the complete data structure, only the relevant info.    *
      * After the program is called, the messages in the queue      *
      * of this program are passes back to the queue of the         *
      * calling program and the *CANCL procedure is invoked.        *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *PSSR         BEGSR
      *
      *          ------------------------------------------
      *          Do if error not in this routine.
      *          ------------------------------------------
     C                   IF        I#PSSR <> *ON                                B01
     C                   EVAL      I#PSSR = *ON
      *
      *          ------------------------------------------
      *          Post information about the last used file
      *          ------------------------------------------
     C                   IF        E#FILE = 'OMHDDL1 '                          B03
     C                   MOVEL     INFO2         FILE
     C                   ELSE                                                   X03
     C                   IF        E#FILE = 'OMHITL1 '                          B04
     C                   MOVEL     INFO3         FILE
     C                   ELSE                                                   X04
     C                   IF        E#FILE = 'OMHTVL1 '                          B05
     C                   MOVEL     INFO4         FILE
     C                   ELSE                                                   X05
     C                   IF        E#FILE = 'OMHDEL1 '                          B06
     C                   MOVEL     INFO5         FILE
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Call the error handler
      *          ------------------------------------------
     C                   CALLP     OMH961 ( PGM : FILE )
      *
      *          ------------------------------------------
      *          Move the messages to the queue of the caller
      *          ------------------------------------------
     C                   EVAL      %SUBST(P#MSTA:1:5) = '*COMP'
     C                   EVAL      P#MSTA = %TRIMR(P#MSTA) + '     ' +
     C                             '*DIAG'
     C                   EVAL      P#MSTA = %TRIMR(P#MSTA) + '     ' +
     C                             '*ESCA'
     C                   EVAL      P#MSTA = %TRIMR(P#MSTA) + 'PE'
     C                   EVAL      P#MSTA = %TRIMR(P#MSTA) + '   ' + '*INFO'
     C                   EVAL      P#MSKC = *BLANKS
     C                   EVAL      P#LOB# = 4
     C                   EVAL      P#PMQC = '*'
     C                   EVAL      P#PSC# = 1
     C                   CALLP     QMHMOVPM ( P#MSKC : P#MSTA : P#LOB# :
     C                             P#PMQC : P#PSC# : P#ERR )
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR     '*CANCL'
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Array specifications                                        *
      * ----------------------------------------------------------- *
**      MSD
INC0001    Incident type                     1
DSC0001    Description                       2
HLP0001    Helpdesk code                     3
**      TXD
MH01902    Work with Incident Types          1
MH01903    Confirm Delete of Incident Types  2
MH01904    Maintain Incident Types           3
ACT0001    ADD                               4
ACT0002    CHANGE                            5
ACT0004    DISPLAY                           6
ACT0005    COPY                              7
CHO0022    (F4)                              8
