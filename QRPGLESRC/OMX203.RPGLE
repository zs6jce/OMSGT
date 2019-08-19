     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMX203.                                       *
      * Function ...: User Option Maintenance (HELPDESK)            *
      * Author .....: Remain Software                               *
      * Parameters .: P#SUL# - Support level                        *
      *               P#UOPC - User option                          *
      *                                                             *
      * User option maintenance. Not HD  managers are only allowed  *
      * to display and select a user option. HD  managers are       *
      * allowed to maintain the user options.                       *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used files                                                  *
      * ----------------------------------------------------------- *
      *                                                             *
      * Files are used in the incomplete list handler (OMX2032)     *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Sub programs                                                *
      * ----------------------------------------------------------- *
      *                                                             *
      * OMH901C - Send message                                      *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators                                             *
      * ----------------------------------------------------------- *
      *                                                             *
      * 99 - General purpose indicator.                             *
      *                                                             *
      * LOOP01 - Indicator to handle first screen main loop.        *
      * EROR01 - Indicator to handle first screen main loop.        *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Files                                                       *
      * ----------------------------------------------------------- *
     FOMHUOL1   UF A E           K DISK    INFSR(*PSSR)
     F                                     INFDS(HUOL1)
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
      *                                                             *
      * ----------------------------------------------------------- *
     D VAR             S            256    DIM(20)
     D MSD             S              7    DIM(20) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
     D TXT             S             50    DIM(20)
     D TXD             S              7    DIM(20) CTDATA PERRCD(1)
      *COPY QCPYSRC,OMSREFREN
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Input pecifications                                         *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          The field reference file is used for field definitions
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          File information data structures
      *          ------------------------------------------
     D HUOL1           DS            57
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  E#FILE               201    208
      *
      *          ------------------------------------------
      *          Date and time stamp data structure
      *          ------------------------------------------
     D D#STMP          DS
     D D#STDT                         7  0
     D D#STTM                         6  0
      *
      *          ------------------------------------------
      *          Buffer
      *          ------------------------------------------
     D P#BUF           DS          6144
      *
      *          ------------------------------------------
      *          Error buffer for API call.
      *          ------------------------------------------
     D P#ERR           DS
     D D#NBP#                         9B 0 INZ(272)
     D D#NBA#                         9B 0
     D D#MSID                         7
     D D#MSGD                 17    272
      *
      *          ------------------------------------------
      *          Binary number definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFBINARY
      *
      *          ------------------------------------------
      *          Buffer for VR0001
      *          Constant holding the length of this buffer
      *          ------------------------------------------
     D V#0001          DS
     D FMPA1C                        10
     D FMPG1D                        30
     D FMXPGM                        20    INZ('OMX2032   *LIBL     ')
     D FMCUL#                         1  0
     D L#VR01          C                   CONST(61)
      *
      *          ------------------------------------------
      *          Buffer for VR0002
      *          Constant holding the length of this buffer
      *          ------------------------------------------
     D V#0002          DS
     D FMDATE                         7  0
     D FMTIME                         6  0
     D FMDOTC                         1
     D L#VR02          C                   CONST(14)
      *
      *          ------------------------------------------
      *          Data buffer
      *          ------------------------------------------
     D V#0101          DS
     D FMIADD                         1
     D FMICHG                         1
     D FMIDSP                         1
     D FMUOPC                         2
     D  FMUOP1                        1    OVERLAY(FMUOPC)
     D  FMUOP2                        1    OVERLAY(FMUOPC:2)
     D I#UOPC                         1
     D FMUOPD                        30
     D I#UOPD                         1
     D FMSUL#                         1
     D I#SUL#                         1
     D FMPGMN                        10
     D I#PGMN                         1
     D FMPGML                        10
     D I#PGML                         1
     D FMPRMI                         1
     D I#PRMI                         1
     D FMINTL                         8
     D I#INTL                         1
     D FMPOSS                         2
     D FMOPTI                         4B 0
     D I#OPTI                         1
     D FMVIEW                         4B 0
      *
     D L#VR11          C                   CONST(79)
     D S#0101          DS            79
      *
      *          ------------------------------------------
      *          Record numbers array for add list multiple entries
      *          ------------------------------------------
     D P#RNRA                         9B 0 INZ(0)
      *
      *          ------------------------------------------
      *          Constants used in program
      *          ------------------------------------------
     D C#LIBL          C                   CONST('*LIBL     ')
     D C#OMSG          C                   CONST('OMHMSG    ')
     D C#ODSP          C                   CONST('OMHDSP    ')
     D C#ALPH          C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
      *
      *          ------------------------------------------
      *          Constants used for panel group
      *          ------------------------------------------
     D C#UOPC          C                   CONST('FMUOPC    ')
     D C#OPTI          C                   CONST('FMOPTI    ')
     D C#PNLQ          C                   CONST('OMX203D   *LIBL     ')
     D WAITEX          C                   CONST(-20)
      *
     D ENTER           C                   CONST(0)
     D F3              C                   CONST(3)
     D F5              C                   CONST(5)
     D F6              C                   CONST(6)
     D F10             C                   CONST(10)
     D F11             C                   CONST(11)
     D F12             C                   CONST(12)
      * Prototype for OMX203
     D OMX203          PR
     D P#SUL#_                             LIKE(P#SUL#)
     D P#UOPC_                             LIKE(P#UOPC)
      * Prototype for 'OMX2032'
     D OMX2032         PR                  EXTPGM('OMX2032')
      * Prototype for 'QUIADDLE'
     D QUIADDLE        PR                  EXTPGM('QUIADDLE')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#BUF_                              LIKEDS(P#BUF)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#VRNC_                             LIKE(P#VRNC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#OPTC_                             LIKE(P#OPTC)
     D P#LEHC_                             LIKE(P#LEHC)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'OMH901C'
     D OMH901C         PR                   EXTPGM('OMH901C')
     D D#MSID_                             LIKE(D#MSID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D D#MSGD_                             LIKE(D#MSGD)
      *
      *          ------------------------------------------
      *          Parameter list for OMH902C (Clear message queue)
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
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUIADDPW'
     D QUIADDPW        PR                  EXTPGM('QUIADDPW')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#PWLC_                             LIKE(P#PWLC)
     D P#ROW#_                             LIKE(P#ROW#)
     D P#COL#_                             LIKE(P#COL#)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUICLOA'
     D QUICLOA         PR                  EXTPGM('QUICLOA')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#CLOC_                             LIKE(P#CLOC)
     D P#ERR_                              LIKEDS(P#ERR)
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
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUIDLTL'
     D QUIDLTL         PR                  EXTPGM('QUIDLTL')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUIDSPP'
     D QUIDSPP         PR                  EXTPGM('QUIDSPP')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#FNR#_                             LIKE(P#FNR#)
     D P#PNLC_                             LIKE(P#PNLC)
     D P#RDOC_                             LIKE(P#RDOC)
     D P#ERR_                              LIKEDS(P#ERR)
     D P#USTC_                             LIKE(P#USTC)
     D P#PSC#_                             LIKE(P#PSC#)
     D P#PMQC_                             LIKE(P#PMQC)
     D P#MRKC_                             LIKE(P#MRKC)
     D P#CPOC_                             LIKE(P#CPOC)
     D P#LLEC_                             LIKE(P#LLEC)
     D P#ELEC_                             LIKE(P#ELEC)
     D P#WTM#_                             LIKE(P#WTM#)
      * Prototype for 'QUIGETV'
     D QUIGETV         PR                  EXTPGM('QUIGETV')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#BUF_                              LIKEDS(P#BUF)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#VRNC_                             LIKE(P#VRNC)
     D P#ERR_                              LIKEDS(P#ERR)
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
     D P#ERR_                              LIKEDS(P#ERR)
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
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUIOPNDA'
     D QUIOPNDA        PR                  EXTPGM('QUIOPNDA')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#PNLQ_                             LIKE(P#PNLQ)
     D P#APS#_                             LIKE(P#APS#)
     D P#ILV#_                             LIKE(P#ILV#)
     D P#FSHC_                             LIKE(P#FSHC)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUIPUTV'
     D QUIPUTV         PR                  EXTPGM('QUIPUTV')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#BUF_                              LIKEDS(P#BUF)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#VRNC_                             LIKE(P#VRNC)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUIRMVLE'
     D QUIRMVLE        PR                  EXTPGM('QUIRMVLE')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#XOPC_                             LIKE(P#XOPC)
     D P#LEHC_                             LIKE(P#LEHC)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUIRMVPW'
     D QUIRMVPW        PR                  EXTPGM('QUIRMVPW')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#ROPC_                             LIKE(P#ROPC)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUISETSC'
     D QUISETSC        PR                  EXTPGM('QUISETSC')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUISETLA'
     D QUISETLA        PR                  EXTPGM('QUISETLA')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#LCOC_                             LIKE(P#LCOC)
     D P#PDVC_                             LIKE(P#PDVC)
     D P#DPAC_                             LIKE(P#DPAC)
     D P#ATAC_                             LIKE(P#ATAC)
     D P#ERR_                              LIKEDS(P#ERR)

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
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'OMH904C'
     D OMH904C         PR                  EXTPGM('OMH904C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGTXT_                             LIKE(MSGTXT)
      *
      *          ------------------------------------------
      *          Get the display file text
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
     D P#ERR_                              LIKEDS(P#ERR)
      * *ENTRY Interface for Main Procedure
     D OMX203          PI
     D P#SUL#                         1
     D P#UOPC                         2
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D s#opti          S              4P 0
     D s#view          S              4P 0
     D EROR01          S                   LIKE(INDI)
     D EROR02          S                   LIKE(INDI)
     D FILE            S             57
     D I#PSSR          S              1
     D LOOP01          S                   LIKE(INDI)
     D LOOP02          S                   LIKE(INDI)
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#IB            S             10
     D P#MSTA          S             40
     D S#LEHC          S                   LIKE(P#LEHC)
     D S#POSS          S                   LIKE(FMPOSS)
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
      *          Program parameters
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Exit if called without parameters
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      D#PARM = *ZEROS
     C                   EXSR      SRTTLR
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Do the processing routine
      *          ------------------------------------------
     C                   EXSR      SRVERW
      *
      *          ------------------------------------------
      *          Do the total last record routine
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRVERW - Main program processing routine                    *
      *                                                             *
      * This routine calls the first screen routine until the user  *
      * terminates the program by pressing command key F3.          *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Call screen 1 processing
      *          ------------------------------------------
     C                   EXSR      SRSC01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC01 - Work with user options                              * /
      *                                                              * /
      * This routine handles screen 1. It allows users to            * /
      * select and display and code '9' users to maintain the user   * /
      * options.                                                     *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSC01        BEGSR
      *
      *          ------------------------------------------
      *          Init this screen
      *          ------------------------------------------
     C                   EXSR      SRIN01
      *
      *          ------------------------------------------
      *          Enter display loop
      *          ------------------------------------------
     C                   DOU       LOOP01 = '0'                                 B01
      *
      *          ------------------------------------------
      *          Enter error loop
      *          ------------------------------------------
     C                   DOU       EROR01 = '0'                                 B02
      *
      *          ------------------------------------------
      *          Set global values.
      *          ------------------------------------------
     C                   EVAL      %SUBST(FMPA1C:1:8) = 'OMX20301'
     C                   IF        P#SUL# = '9'                                 B03
     C                   EVAL      FMPG1D = TXT(4)                              Work with
     C                   ELSE                                                   X03
     C                   EVAL      FMPG1D = TXT(1)                              Select
     C                   ENDIF                                                  E03
     C                   MOVEL     V#0001        P#BUF
     C                   Z-ADD     L#VR01        P#LOB#
     C                   MOVEL     'VR0001'      P#VRNC
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Set the date and time.
      *          ------------------------------------------
     C                   EXSR      SRDATE
      *
      *          ------------------------------------------
      *          Display the selection window
      *          ------------------------------------------
     C                   MOVE      *BLANKS       P#PNLQ
     C                   MOVE      C#PNLQ        P#PNLQ
     C                   MOVEL     'OMX20301'    P#PNLC
     C                   Z-ADD     -1            P#WTM#
     C                   MOVE      'N'           P#USTC
     C                   EXSR      SRDSPP
     C                   MOVE      'Y'           P#RDOC
      *
      *          ------------------------------------------
      *          Get the buffer
      *          ------------------------------------------
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   EXSR      SRGETV
     C                   MOVEL     P#BUF         V#0101
      *
      *          ------------------------------------------
      *          Clear the message queue
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Set control indicators
      *          ------------------------------------------
     C                   MOVE      'D'           P#CPOC
     C                   EVAL      LOOP01 = *OFF
     C                   EVAL      EROR01 = *OFF
      *
      *          ------------------------------------------
      *          Function key processing.
      *          F3 and F12 exit.
      *          ------------------------------------------
     C                   IF        P#FNR# = F3                                  B03
     C                             OR P#FNR# = F12
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F5 is refresh
      *          ------------------------------------------
     C                   IF        P#FNR# = F5                                  B04
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          F6 is add.
      *          ------------------------------------------
     C                   IF        P#FNR# = F6                                  B05
     C                   EXSR      SRSC02
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          F10 Shows only the keyfields.
      *          ------------------------------------------
     C                   IF        P#FNR# = F10                                 B06
     C                   EVAL      EROR01 = *ON
     C                   IF        FMVIEW = 3                                   B07
     C                   EVAL      FMVIEW = S#VIEW
     C                   ELSE                                                   X07
     C                   EVAL      S#VIEW = FMVIEW
     C                   EVAL      FMVIEW = 3
     C                   ENDIF                                                  E07
      *
     C                   MOVEL     V#0101        P#BUF
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   EXSR      SRPUTV
     C                   ELSE                                                   X06
      *
      *          ------------------------------------------
      *          F11 changes the view.
      *          ------------------------------------------
     C                   IF        P#FNR# = F11                                 B07
     C                   EVAL      EROR01 = *ON
     C                   ADD       1             FMVIEW
     C                   IF        FMVIEW > 2                                   B08
     C                   EVAL      FMVIEW = *ZEROS
     C                   ENDIF                                                  E08
      *
     C                   MOVEL     V#0101        P#BUF
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   EXSR      SRPUTV
     C                   ELSE                                                   X07
      *
      *          ------------------------------------------
      *          Edit.
      *          ------------------------------------------
     C                   EXSR      SRED01
     C                   ENDIF                                                  E07
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Process the user input.
      *          ------------------------------------------
     C                   EXSR      SRPR01
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRIN01 - Intialize display 1                                 * /
      *                                                              * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Set global values.
      *          ------------------------------------------
     C                   MOVE      P#SUL#        FMCUL#
     C                   EVAL      %SUBST(FMPA1C:1:8) = 'OMX20301'
     C                   IF        P#SUL# = '9'                                 B01
     C                   EVAL      FMPG1D = TXT(4)                              Work with
     C                   ELSE                                                   X01
     C                   EVAL      FMPG1D = TXT(1)                              Select
     C                   ENDIF                                                  E01
     C                   MOVEL     V#0001        P#BUF
     C                   Z-ADD     L#VR01        P#LOB#
     C                   MOVEL     'VR0001'      P#VRNC
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Build the list.
      *          ------------------------------------------
     C                   EXSR      SRRF01
      *
      *          ------------------------------------------
      *          Set screen image
      *          ------------------------------------------
     C                   EXSR      SRSCIM
      *
      *          ------------------------------------------
      *          Add a popup window
      *          ------------------------------------------
     C                   MOVEL     '*ROWCOL'     P#PWLC
     C                   Z-ADD     2             P#ROW#
     C                   Z-ADD     10            P#COL#
     C                   EXSR      SRAPUW
      *
      *          ------------------------------------------
      *          Set the view.
      *          ------------------------------------------
     C                   EVAL      FMVIEW = *ZEROS
     C                   MOVEL     V#0101        P#BUF
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          First time display.
      *          ------------------------------------------
     C                   MOVE      'N'           P#RDOC
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRF01 - Work with user options refresh                      * /
      *                                                              * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRF01        BEGSR
      *
      *          ------------------------------------------
      *          Close the incomplete list handler (call without parms)
      *          ------------------------------------------
     C                   CALLP     OMX2032()
      *
      *          ------------------------------------------
      *          Delete the list.
      *          ------------------------------------------
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'VR0101'      P#VRNC
     C                   MOVEL     'OMX203L1'    P#LSTC
     C                   EXSR      SRDLTL
      *
     C                   MOVE      'TOP '        P#LCOC
     C                   MOVEL     'FMXPGM  '    P#PDVC
     C                   MOVE      'SAME'        P#DPAC
     C                   MOVE      'Y'           P#ATAC
     C                   EXSR      SRSLAT
      *
      *          ------------------------------------------
      *          Start with default cursor positioning
      *          ------------------------------------------
     C                   MOVE      'NONE'        P#LLEC
     C                   MOVE      'NONE'        P#ELEC
     C                   MOVE      'D'           P#CPOC
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRED01 - Work with user options edit.                        * /
      *                                                              * /
      * This routine checks if the only selected option is '1'       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          Check if option '1' was entered as the only option.
      *          Set the search value.
      *          ------------------------------------------
     C                   EVAL      FMOPTI = 1
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Get the first list entry.
      *          ------------------------------------------
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'OMX203L1'    P#LSTC
     C                   MOVE      'FSLT'        P#POSC
     C                   MOVE      'Y'           P#COPC
     C                   MOVEL     'EQ'          P#SECC
     C                   MOVE      C#OPTI        P#SECC
     C                   MOVE      *BLANKS       P#SHNC
     C                   MOVE      'N'           P#XOPC
     C                   EXSR      SRGTLE
     C                   MOVEL     P#BUF         V#0101
      *
      *          ------------------------------------------
      *          If an option was found.
      *          ------------------------------------------
     C                   IF        P#LEHC <> '****'                             B01
     C                   EVAL      LOOP01 = *ON
     C                   MOVE      P#LEHC        S#LEHC
      *
      *          ------------------------------------------
      *          Get the first not option 1
      *          ------------------------------------------
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'VR0101'      P#VRNC
     C                   MOVEL     'OMX203L1'    P#LSTC
     C                   MOVE      'FSLT'        P#POSC
     C                   MOVE      'Y'           P#COPC
     C                   MOVEL     'GT'          P#SECC
     C                   MOVE      C#OPTI        P#SECC
     C                   MOVE      *BLANKS       P#SHNC
     C                   MOVE      'N'           P#XOPC
     C                   EXSR      SRGTLE
     C                   MOVEL     P#BUF         V#0101
      *
      *          ------------------------------------------
      *          If found then set the option to error.
      *          ------------------------------------------
     C                   IF        P#LEHC <> '****'                             B02
     C                   EVAL      I#OPTI = *ON
     C                   EVAL      MSGID = 'OPTE017'
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
      *
      *          ------------------------------------------
      *          Put the buffer and the list entry.
      *          ------------------------------------------
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   MOVE      S#LEHC        P#LEHC
     C                   EXSR      SRPUTV
     C                   EXSR      SRUPLE
      *
      *          ------------------------------------------
      *          Reset the list entry error indicator.
      *          ------------------------------------------
     C                   EVAL      I#OPTI = *OFF
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Set positioning options.
      *          ------------------------------------------
     C                   MOVE      'NONE'        P#LLEC
     C                   MOVE      S#LEHC        P#ELEC
     C                   MOVE      'A'           P#CPOC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If no errors the position to.
      *          ------------------------------------------
     C                   IF        EROR01 = *OFF                                B01
     C                             AND FMPOSS <> *BLANKS
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
      * SRPR01 - Process screen 1.                                   * /
      *                                                              * /
      * This routine gets the first selected list entry.             * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          Confirm delete of requested records (if any).
      *          ------------------------------------------
     C                   IF        P#SUL# = '9'                                 B01
     C                   EXSR      SRDC01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If not cancel then process the list.
      *          ------------------------------------------
     C                   IF        P#FNR# <> F12                                B01
      *
      *          ------------------------------------------
      *          Set the search value.
      *          ------------------------------------------
     C                   EVAL      FMOPTI = 0
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Get the first list entry.
      *          ------------------------------------------
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'OMX203L1'    P#LSTC
     C                   MOVE      'FSLT'        P#POSC
     C                   MOVE      'Y'           P#COPC
     C                   MOVEL     'NE'          P#SECC
     C                   MOVE      C#OPTI        P#SECC
     C                   MOVE      *BLANKS       P#SHNC
     C                   MOVE      'N'           P#XOPC
     C                   EXSR      SRGTLE
     C                   MOVEL     P#BUF         V#0101
      *
      *          ------------------------------------------
      *          Do while records found.
      *          ------------------------------------------
     C                   DOW       P#LEHC <> '****'                             B02
     C                   EVAL      LOOP01 = *ON
     C                   MOVE      P#LEHC        S#LEHC
      *
      *          ------------------------------------------
      *          4=Delete
      *          ------------------------------------------
     C                   IF        FMOPTI = 4                                   B03
      *
     C     FMUOPC        SETLL     OMHUOL1
     C     FMUOPC        DELETE    OMHUOL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
     C                   EXSR      SRRMLE
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     FMUOPC        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'DLT0017'
     C                   EXSR      SNDMSG
      *
      *          ------------------------------------------
      *          2=Change
      *          3=Copy
      *          5=Display
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   IF        FMOPTI = 2                                   B04
     C                             OR FMOPTI = 3
     C                             OR FMOPTI = 5
     C                   EXSR      SRSC02
     C                   IF        P#FNR# = F12                                 B05
     C                   LEAVE
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          1=Select
      *          ------------------------------------------
     C                   ELSE                                                   X04
     C                   IF        FMOPTI = 1                                   B05
     C                   EVAL      P#UOPC = FMUOPC
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Set the search value.
      *          ------------------------------------------
     C                   EVAL      FMOPTI = 0
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Get next
      *          ------------------------------------------
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'VR0101'      P#VRNC
     C                   MOVEL     'OMX203L1'    P#LSTC
     C                   MOVE      'FSLT'        P#POSC
     C                   MOVE      'Y'           P#COPC
     C                   MOVEL     'NE'          P#SECC
     C                   MOVE      C#OPTI        P#SECC
     C                   MOVE      *BLANKS       P#SHNC
     C                   MOVE      'N'           P#XOPC
     C                   EXSR      SRGTLE
     C                   MOVEL     P#BUF         V#0101
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          If cancel then quit processing.
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   EVAL      LOOP01 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Set positioning options.
      *          ------------------------------------------
     C                   MOVE      S#LEHC        P#LLEC
     C                   MOVE      'NONE'        P#ELEC
     C                   MOVE      'A'           P#CPOC
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
     C                   MOVEL     'OMX203L2'    P#LSTC
     C                   EXSR      SRDLTL
      *
      *          ------------------------------------------
      *          Set the search value.
      *          ------------------------------------------
     C                   EVAL      FMOPTI = 4
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Get list multiple entries (10 at a time)
      *          ------------------------------------------
     C     L#VR11        MULT      10            P#LOB#
     C                   MOVEL     'OMX203L1'    P#LSTC
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
     C                   MOVEL     'OMX203L2'    P#LSTC
     C                   MOVE      'NEXT'        P#OPTC
     C                   Z-ADD     P#RCC#        P#NOR#
     C                   Z-ADD     L#VR11        P#RSZ#
     C                   EXSR      SRADLM
      *
      *          ------------------------------------------
      *          Get the next ten entries.
      *          ------------------------------------------
     C     L#VR11        MULT      10            P#LOB#
     C                   MOVEL     'OMX203L1'    P#LSTC
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
      *          Display the confirmation panel if entries found.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B01
     C                   MOVE      *BLANKS       P#PNLC
     C                   MOVE      'D'           P#CPOC
     C                   MOVEL     'OMX20302'    P#PNLC
     C                   MOVE      'N'           P#RDOC
     C                   MOVE      'N'           P#USTC
     C                   Z-ADD     -1            P#WTM#
     C                   MOVE      'FRST'        P#MRKC
     C                   MOVE      'D'           P#CPOC
     C                   EXSR      SRDSPP
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
     C                   EVAL      FMUOPC = FMPOSS
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
     C                   MOVE      C#UOPC        P#SECC
     C                   MOVE      'Y'           P#XOPC
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
     C                   IF        FMPOSS > FMUOPC                              B02
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
     C                   EVAL      FMPOSS = *BLANKS
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
      * SRSC02 - Maintain user option                                * /
      *                                                              * /
      * This routine handles screen 2. With this display you can add * /
      * copy, display or change a user option.                       * /
      * This is only allowed for code '9' users.                     *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSC02        BEGSR
      *
      *          ------------------------------------------
      *          Init this screen
      *          ------------------------------------------
     C                   EXSR      SRIN02
      *
      *          ------------------------------------------
      *          Enter display loop
      *          ------------------------------------------
     C                   DOU       LOOP02 = '0'                                 B01
      *
      *          ------------------------------------------
      *          Enter error loop
      *          ------------------------------------------
     C                   DOU       EROR02 = '0'                                 B02
      *
      *          ------------------------------------------
      *          Set the date and time.
      *          ------------------------------------------
     C                   EXSR      SRDATE
      *
      *          ------------------------------------------
      *          Save the buffer.
      *          ------------------------------------------
     C                   EVAL      S#0101 = V#0101
      *
      *          ------------------------------------------
      *          Display the data (in the same window)
      *          ------------------------------------------
     C                   MOVE      *BLANKS       P#PNLQ
     C                   MOVE      C#PNLQ        P#PNLQ
     C                   MOVEL     'OMX20303'    P#PNLC
     C                   Z-ADD     -1            P#WTM#
     C                   MOVE      'N'           P#USTC
     C                   EXSR      SRDSPP
      *
      *          ------------------------------------------
      *          Clear the message queue
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Set control indicators
      *          ------------------------------------------
     C                   MOVE      'Y'           P#RDOC
     C                   MOVE      'D'           P#CPOC
     C                   EVAL      LOOP02 = *OFF
     C                   EVAL      EROR02 = *OFF
      *
      *          ------------------------------------------
      *          Get the buffer
      *          ------------------------------------------
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
     C                   EXSR      SRGETV
     C                   MOVEL     P#BUF         V#0101
      *
      *          ------------------------------------------
      *          Re-loop if changes
      *          ------------------------------------------
     C                   IF        V#0101 <> S#0101                             B03
     C                   EVAL      LOOP02 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Function key processing.
      *          F3 is exit.
      *          ------------------------------------------
     C                   IF        P#FNR# = F3                                  B03
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F5 is refresh
      *          ------------------------------------------
     C                   IF        P#FNR# = F5                                  B04
     C                   EXSR      SRRF02
     C                   EVAL      EROR02 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Edit.
      *          ------------------------------------------
     C                   IF        P#FNR# <> F12                                B05
     C                   EXSR      SRED02
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Process the user input.
      *          ------------------------------------------
     C                   IF        P#FNR# <> F12                                B02
     C                   EXSR      SRPR02
     C                   ENDIF                                                  E02
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Use action list positioning for the list (QUIDSPP)
      *          ------------------------------------------
     C                   MOVE      'A'           P#CPOC
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRIN02 - Maintain user option init                           * /
      *                                                              * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRIN02        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init.
      *          ------------------------------------------
     C                   IF        P#FNR# = F6                                  B01
     C                   EVAL      FMOPTI = *ZEROS
     C                   ENDIF                                                  E01
      *
     C                   EVAL      FMIADD = *OFF
     C                   EVAL      FMICHG = *OFF
     C                   EVAL      FMIDSP = *OFF
      *
     C                   EVAL      S#OPTI = FMOPTI
      *
      *          ------------------------------------------
      *          Update the list entry.
      *          ------------------------------------------
     C                   IF        S#OPTI <> *ZEROS                             B01
     C                   EVAL      FMOPTI = *ZEROS
     C                   MOVEL     V#0101        P#BUF
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'VR0101'      P#VRNC
     C                   EXSR      SRUPLE
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Determine the action.
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      S#OPTI = *ZEROS                              X01
     C                   EVAL      FMIADD = *ON
     C                   EVAL      FMPG1D = TXT(2)
      *
     C                   WHEN      S#OPTI = 2                                   X01
     C                   EVAL      FMICHG = *ON
     C                   EVAL      FMPG1D = TXT(5)
      *
     C                   WHEN      S#OPTI = 3                                   X01
     C                   EVAL      FMIADD = *ON
     C                   EVAL      FMPG1D = TXT(6)
      *
     C                   WHEN      S#OPTI = 5                                   X01
     C                   EVAL      FMIDSP = *ON
     C                   EVAL      FMPG1D = TXT(3)
      *
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          Set global values.
      *          ------------------------------------------
     C                   EVAL      %SUBST(FMPA1C:1:8) = 'OMX20303'
     C                   MOVEL     V#0001        P#BUF
     C                   Z-ADD     L#VR01        P#LOB#
     C                   MOVEL     'VR0001'      P#VRNC
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Build the display.
      *          ------------------------------------------
     C                   EXSR      SRRF02
      *
      *          ------------------------------------------
      *          Use default positioning (parms for QUIDSPP)
      *          ------------------------------------------
     C                   MOVE      'N'           P#RDOC
     C                   MOVE      'D'           P#CPOC
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRF02 - Maintain user option refresh                        * /
      *                                                              * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRF02        BEGSR
      *
      *          ------------------------------------------
      *          Get the data if not add.
      *          ------------------------------------------
     C                   IF        S#OPTI <> *ZEROS                             B01
     C     FMUOPC        CHAIN(N)  OMHUOL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ELSE                                                   X01
     C                   RESET                   OMHUOR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If copy, then clear the keyfield.
      *          ------------------------------------------
     C                   IF        S#OPTI = 3                                   B01
     C                   MOVE      *BLANKS       UOUOPC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the screen fields from the record format.
      *          ------------------------------------------
     C                   MOVE      UOUOPC        FMUOPC
     C                   MOVE      UOUOPD        FMUOPD
     C                   MOVE      UOSUL#        FMSUL#
     C                   MOVE      UOPGMN        FMPGMN
     C                   MOVE      UOPGML        FMPGML
     C                   MOVE      UOPRMI        FMPRMI
     C                   MOVE      UOINTL        FMINTL
      *
      *          ------------------------------------------
      *          If add then fill with defaults.
      *          ------------------------------------------
     C                   IF        S#OPTI = *ZEROS                              B01
     C                   EVAL      FMSUL# = '0'
     C                   EVAL      FMPRMI = '1'
     C                   EVAL      FMINTL = '*V1R1M0 '
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Put the buffer.
      *          ------------------------------------------
     C                   MOVEL     'VR0101'      P#VRNC
     C                   Z-ADD     L#VR11        P#LOB#
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
      * SRED02 - Maintain user options edit.                           /
      *                                                              * /
      * This routine edits the user input.                           *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRED02        BEGSR
      *
      *          ------------------------------------------
      *          Not if display.
      *          ------------------------------------------
     C                   IF        FMIDSP = *OFF                                B01
      *
      *          ------------------------------------------
      *          If add, the field is right adjusted.
      *          ------------------------------------------
     C                   IF        FMIADD = *ON                                 B02
     C                   IF        FMUOP1 <> *BLANKS                            B03
     C                             AND FMUOP2 = *BLANKS
     C                   EVAL      FMUOP2 = FMUOP1
     C                   EVAL      FMUOP1 = *BLANKS
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If add, user option may not be blanks.
      *          ------------------------------------------
     C                   IF        FMUOPC = *BLANKS                             B03
     C                   EVAL      MSGDTA = VAR(1)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      I#UOPC = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          User option must contain at least one alphabet char.
      *          ------------------------------------------
     C     C#ALPH        CHECK     FMUOP1                                 99
     C                   IF        *IN99 = *ON                                  B04
     C     C#ALPH        CHECK     FMUOP2                                 99
     C                   IF        *IN99 = *ON                                  B05
     C                   EVAL      MSGDTA = VAR(1)
     C                   EVAL      MSGID = 'ALP0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      I#UOPC = *ON
     C                   ENDIF                                                  E05
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          User option may not already exist.
      *          ------------------------------------------
     C     FMUOPC        SETLL     OMHUOL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B05
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     FMUOPC        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'EXI0007'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      I#UOPC = *ON
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Option description may not be blanks.
      *          ------------------------------------------
     C                   IF        FMUOPD = *BLANKS                             B02
     C                   MOVEA     VAR(2)        MC(1)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      I#UOPD = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Program name may not be blanks.
      *          ------------------------------------------
     C                   IF        FMPGMN = *BLANKS                             B02
     C                   MOVEA     VAR(3)        MC(1)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      I#PGMN = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Program library may not be blanks.
      *          ------------------------------------------
     C                   IF        FMPGML = *BLANKS                             B02
     C                   MOVEA     VAR(4)        MC(1)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      I#PGML = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Authorisation code must be '0' to '9'.
      *          ------------------------------------------
     C                   IF        FMSUL# < '0'                                 B02
     C                             OR FMSUL# > '9'
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      I#SUL# = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Pass parameters must be '0' or '1'.
      *          ------------------------------------------
     C                   IF        FMPRMI <> '0'                                B02
     C                             AND FMPRMI <> '1'
     C                   EVAL      MSGID = 'OPTE016'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      I#PRMI = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Interface level must be V1R1M0
      *          ------------------------------------------
     C                   IF        FMINTL <> '*V1R1M0'                          B02
     C                   MOVEA     VAR(5)        MC(1)
     C                   EVAL      MC(33) = '*'
     C                   EVAL      MC(34) = 'V'
     C                   EVAL      MC(35) = '1'
     C                   EVAL      MC(36) = 'R'
     C                   EVAL      MC(37) = '1'
     C                   EVAL      MC(38) = 'M'
     C                   EVAL      MC(39) = '0'
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'OPTE002'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      I#INTL = *ON
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Update the buffer
      *          ------------------------------------------
     C                   MOVEL     V#0101        P#BUF
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'VR0101'      P#VRNC
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Reset the error indicators
      *          ------------------------------------------
     C                   EVAL      I#UOPC = *OFF
     C                   EVAL      I#UOPD = *OFF
     C                   EVAL      I#PGMN = *OFF
     C                   EVAL      I#PGML = *OFF
     C                   EVAL      I#SUL# = *OFF
     C                   EVAL      I#PRMI = *OFF
     C                   EVAL      I#INTL = *OFF
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPR02 - Maintain user option process.                       * /
      *                                                              * /
      * This routine processes the user input.                       * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPR02        BEGSR
      *
      *          ------------------------------------------
      *          Not if display.
      *          ------------------------------------------
     C                   IF        FMIDSP = *OFF                                B01
      *
      *          ------------------------------------------
      *          Get the record if not add.
      *          ------------------------------------------
     C                   IF        FMIADD = *OFF                                B02
     C     FMUOPC        CHAIN     OMHUOL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ELSE                                                   X02
     C                   EVAL      *IN99 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill the file buffer.
      *          ------------------------------------------
     C                   MOVE      FMUOPC        UOUOPC
     C                   MOVE      FMUOPD        UOUOPD
     C                   MOVE      FMSUL#        UOSUL#
     C                   MOVE      FMPGMN        UOPGMN
     C                   MOVE      FMPGML        UOPGML
     C                   MOVE      FMPRMI        UOPRMI
     C                   MOVE      FMINTL        UOINTL
      *
      *          ------------------------------------------
      *          Write or update.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON
     C                   WRITE     OMHUOR
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMHUOR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Add a list entry if add.
      *          ------------------------------------------
     C                   IF        FMIADD = *ON                                 B02
     C                   EXSR      SRAE02
      *
      *          ------------------------------------------
      *          If add, then switch to change mode.
      *          ------------------------------------------
     C                   EVAL      S#OPTI = 2
     C                   EVAL      FMIADD = *OFF
     C                   EVAL      FMICHG = *ON
     C                   EVAL      FMPG1D = TXT(5)
      *
      *          ------------------------------------------
      *          Set global values.
      *          ------------------------------------------
     C                   MOVEL     V#0001        P#BUF
     C                   Z-ADD     L#VR01        P#LOB#
     C                   MOVEL     'VR0001'      P#VRNC
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Set detail values.
      *          ------------------------------------------
     C                   MOVEL     V#0101        P#BUF
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'VR0101'      P#VRNC
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Reloop if add.
      *          ------------------------------------------
     C                   EVAL      LOOP02 = *ON
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Update the list entry.
      *          ------------------------------------------
     C                   EVAL      FMOPTI = *ZEROS
     C                   MOVE      UOUOPC        FMUOPC
     C                   MOVE      UOUOPD        FMUOPD
     C                   MOVE      UOSUL#        FMSUL#
     C                   MOVE      UOPGMN        FMPGMN
     C                   MOVE      UOPGML        FMPGML
     C                   MOVE      UOPRMI        FMPRMI
     C                   MOVE      UOINTL        FMINTL
     C                   MOVEL     V#0101        P#BUF
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'VR0101'      P#VRNC
     C                   MOVEL     'OMX203L1'    P#LSTC
     C                   EXSR      SRUPLE
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRAE02 - Add a list entry in the list                        *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRAE02        BEGSR
      *
      *          ------------------------------------------
      *          Get the insert position.
      *          ------------------------------------------
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'OMX203L1'    P#LSTC
     C                   MOVE      'FSLT'        P#POSC
     C                   MOVE      'N'           P#COPC
     C                   MOVEL     'GT'          P#SECC
     C                   MOVE      C#UOPC        P#SECC
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
     C                   EVAL      FMOPTI = *ZEROS
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'VR0101'      P#VRNC
     C                   MOVE      *BLANKS       P#LEHC
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRADLE
      *
      *          ------------------------------------------
      *          Position on this entry.
      *          ------------------------------------------
     C                   MOVE      P#LEHC        P#LLEC
     C                   MOVE      'NONE'        P#ELEC
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRADLE - Add list entry                                      * /
      *                                                              * /
      * This routine calls the add list entry API (QUIADDLE).        * /
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
     C     SRADLE        BEGSR
      *
      *          ------------------------------------------
      *          Add list multiple entry
      *          ------------------------------------------
     C                   CALLP     QUIADDLE ( P#HNDC : P#BUF : P#LOB# :
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
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRADLM - Add list multiple entries                           *
      *                                                              *
      * This routine calls the put dialog variable API (QUIPUTV)     *
      * which updates a variable record.                             *
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
      * SRAPUW - Add pop-up window                                   * /
      *                                                              * /
      * This routine initiates the application for pop-up window     * /
      * display by calling the add pop-up window API.                * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *              P#PWLC  Popup window location                   *
      *              P#ROW#  Absolute row used                       *
      *              P#COL#  Absolute column used                    *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRAPUW        BEGSR
      *
      *          ------------------------------------------
      *          Add a pop up window
      *          ------------------------------------------
     C                   CALLP     QUIADDPW ( P#HNDC : P#PWLC : P#ROW# :
     C                             P#COL# : P#ERR )
      *
      *          ------------------------------------------
      *          Catch errors.
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
      * SRDLTL - Delete a list                                       * /
      *                                                              * /
      * This routine deletes a list by calling the QUIDLTL API.      * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *              P#LSTC  List code                               *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRDLTL        BEGSR
      *
      *          ------------------------------------------
      *          Close the display application
      *          ------------------------------------------
     C                   CALLP     QUIDLTL ( P#HNDC : P#LSTC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                             AND D#MSID <> 'CPF6A91'
     C                             AND D#MSID <> 'CPF6A92'
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   IF        *INLR = *OFF
     C                   EXSR      SRTTLR
     C                   ENDIF
     C                   ELSE                                                   X01
     C                   EVAL      D#MSID = *BLANKS
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
     C                   EVAL      FMDATE = D#STDT
     C                   EVAL      FMTIME = D#STTM
      *
     C                   IF        FMDOTC = '1'                                 B01
     C                   EVAL      FMDOTC = '0'
     C                   ELSE                                                   X01
     C                   EVAL      FMDOTC = '1'
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
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRDSPP        BEGSR
      *
      *          ------------------------------------------
      *          Display the panel
      *          ------------------------------------------
     C                   EVAL      P#PSC# = 0
     C                   EVAL      P#PMQC = '*CALLER'
     C                   EVAL      P#MRKC = 'FRST'
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
      *          Get dialog variable
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
      *          Add list entry
      *          ------------------------------------------
     C                   EVAL      P#LEHC = '****'
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
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901c ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X02
     C                   MOVE      '****'        P#LEHC
     C                   EVAL      D#MSID = *BLANKS
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
     C                   EVAL      D#MSID = *BLANKS
     C                   EVAL      P#RCC# = *ZEROS
     C                   ELSE                                                   X02
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901c ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The value of this field is loaded in the high
      *          order halfword. Divide by 10000h in order to shift
      *          it to the low order halfword. This is an IBM bug.
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
     C                   CALLP     OMH901c ( D#MSID : MSGF : MSGLIB :
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
     C                   CALLP     OMH901c ( D#MSID : MSGF : MSGLIB :
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
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901c ( D#MSID : MSGF : MSGLIB :
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
      * SRRPUW - Remove pop-up window                                * /
      *                                                              * /
      * This routine removes the most recent added pop-up window.    * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *              P#ROPC  Remove option                           *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRPUW        BEGSR
      *
      *          ------------------------------------------
      *          Remove the window
      *          ------------------------------------------
     C                   EVAL      P#ROPC = 'L'
     C                   CALLP     QUIRMVPW ( P#HNDC : P#ROPC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901c ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSCIM - Set screen image                                    * /
      *                                                              * /
      * This routine sets the screen image.                          * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSCIM        BEGSR
      *
      *          ------------------------------------------
      *          Remove the window
      *          ------------------------------------------
     C                   CALLP     QUISETSC ( P#HNDC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901c ( D#MSID : MSGF : MSGLIB :
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
     C                   CALLP     OMH901c ( D#MSID : MSGF : MSGLIB :
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
      * SNDMSG - Send message                                        *
      *                                                              *
      * This routine displays an error wich occured during user      *
      * keyed data validation.                                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SNDMSG        BEGSR
      *
      *          ------------------------------------------
      *          Call Program sender
      *          ------------------------------------------
     C                   EVAL      MSGF = 'OMHMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( MSGID : MSGF : MSGLIB : MSGDTA
     C                             )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRUPLE - Update list entry                                   *
      *                                                              *
      * This routine calls the update list entry API (QUIUPDLE).     *
      *                                                              *
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
      *          Add list multiple entry
      *          ------------------------------------------
     C                   EVAL      P#OPTC = 'SAME'
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
     C                   CALLP     OMH901c ( D#MSID : MSGF : MSGLIB :
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
      * *INZSR - Program initialisation                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          IMMEDIATE return if no parameters because the program
      *          was never opened (if it is here).
      *          ------------------------------------------
     C                   EVAL      *INLR = (D#PARM = *ZEROS)
     C                   IF        *INLR = *ON
     C                   RETURN
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Indicator definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Savefields
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Other API Parameter Definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      *          ------------------------------------------
     C     $X901C        PLIST
     C                   PARM                    MSGID
     C                   PARM      'OMHMSG'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGDTA
      *
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
     C                   PARM                    MSGF
     C                   PARM                    MSGLIB
     C                   PARM                    MSGTXT
      *
      *          ------------------------------------------
      *          Get the display file text
      *          ------------------------------------------
     C                   EXSR      SRGTXT
      *
      *          ------------------------------------------
      *          Get the message substitution.
      *          ------------------------------------------
     C                   EXSR      SRGVAR
      *
      *          ------------------------------------------
      *          Open the display application
      *          ------------------------------------------
     C                   MOVE      C#PNLQ        P#PNLQ
     C                   Z-ADD     -1            P#APS#
     C                   EVAL      P#ILV# = *ZEROS
     C                   MOVE      'N'           P#FSHC
     C                   EXSR      SROPDA
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
      *          Subroutine init.
      *          ------------------------------------------
     C                   EVAL      MSGLIB = C#LIBL
     C                   EVAL      MSGF = C#ODSP
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
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill message arrays
      *          ------------------------------------------
     C                   EVAL      TXT(MS) = MSGTXT
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
      *          Subroutine init.
      *          ------------------------------------------
     C                   EVAL      MSGLIB = C#LIBL
     C                   EVAL      MSGF = C#OMSG
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
     C                   IF        E#FILE = 'OMHUOL1 '                          B02
     C                   MOVEL     HUOL1         FILE
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
      *          Close and exit.
      *          ------------------------------------------
     C                   EVAL      D#PARM = *ZEROS
     C                   EXSR      SRTTLR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRTTLR - Total last record processing                       *
      *                                                             *
      * This routine sets the last record indicator on.             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          If close requested.
      *          ------------------------------------------
     C                   IF        D#PARM = *ZEROS                              B01
      *
      *          ------------------------------------------
      *          Close the incomplete list handler
      *          ------------------------------------------
     C                   CALLP     OMX2032()
      *
      *          ------------------------------------------
      *          Seton last record
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
      *
      *          ------------------------------------------
      *          Close the display application if ever opened.
      *          ------------------------------------------
     C                   EXSR      SRCLAP
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Delete the list if not LR.
      *          ------------------------------------------
     C                   IF        *INLR = *OFF                                 B01
     C                   EXSR      SRDLTL
     C                   EXSR      SRRPUW
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Return to caller.
      *          ------------------------------------------
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Array specifications                                        *
      * ----------------------------------------------------------- *
**      MSD
USR0004    user option                         1
DSC0001    description                         2
PGM0001    program name                        3
LIB0001    library                             4
LEV0006    OMS interface level                 5
**      TXD
MX20301    Select User Option                  1
MX20303    Add User Option                     2
MX20304    Display User Option                 3
MX20305    Work with User Options              4
MX20306    Maintain User Option                5
MX20307    Copy User Option                    6
