     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH003.                                       *
      * Function ...: Question list maintenance                     *
      * Author .....: W.S. Jongman.                                 *
      * Module .....: HelpDesk                                      *
      * Startdate ..: 03-09-96                                      *
      * Parameters .: P#HEDC    -  HelpDesk code                    *
      *               P#STAT    -  Status                           *
      *                                                             *
      * This program allows helpdesk managers + the secofr to       *
      * enter the HelpDesk question lists                           *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMHQLL1   UF A E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HQLL1)
     FOMQLQL1   UF A E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(QLQL1)
     FOMANSL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(ANSL1)
     FOMQLAL1   UF A E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(QLAL1)
     FOMHQUL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HQUL1)
     FOMHDSL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HDSL1)
     FOMSTRL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(STRL1)
     FOMSREL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(SREL1)
     FOMSTAL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(STAL1)
     FOMHITL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HITL1)
     FOMH003D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
     F                                     SFILE(OMH00301:RRN01)
     F                                     SFILE(OMH00305:RRN03)
     F                                     SFILE(OMH00308:RRN04)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension spec's                                             *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * ANS   Used to store answers                                  *
      * QSQ   Used to store question seq in question list            *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(20)
     D MSD             S              7    DIM(20) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
      *
      *COPY QCPYSRC,OMSREFREN
      *
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
     D HQLL1           DS            57
     D QLQL1           DS            57
     D HQUL1           DS            57
     D HDSL1           DS            57
     D ANSL1           DS            57
     D QLAL1           DS            57
     D STRL1           DS            57
     D SREL1           DS            57
     D STAL1           DS            57
     D HITL1           DS            57
      *
      *          ------------------------------------------
      *          Question & answer arrays.
      *          ------------------------------------------
     D P#QUE           DS
     D  QUE                    1   5000  0
     D                                     DIM(1000)
     D P#ANS           DS
     D  ANS                    1   3000  0
     D                                     DIM(1000)
     D P#QLS           DS
     D  QLS                    1   5000  0
     D                                     DIM(1000)
     D P#QSQ           DS
     D  QSQ                    1   3000  0
     D                                     DIM(1000)
      *
      *          ------------------------------------------
      *          Screen information data structure
      *          ------------------------------------------
     D INFO            DS
     D  STATUS           *STATUS
     D  D#CRC#               378    379B 0
      *
      *          ------------------------------------------
      *          Binary number formatting for message data.
      *          ------------------------------------------
     D                 DS
     D D#BIN4                         9B 0
     D  D#BIA4                        4    OVERLAY(D#BIN4)
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  E#FILE               201    208
     D  PGMQ             *PROC
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
      *          ------------------------------------------
     D C#TEST          C                   CONST('*TEST')
      *
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#EXIT          C                   CONST('*EXIT')
     D C#CNCL          C                   CONST('*CNCL')
      * Prototype for OMH003
     D OMH003          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH902C'
     D OMH902C         PR                  EXTPGM('OMH902C')
     D PGMQ_                               LIKE(PGMQ)
      *
      *          ------------------------------------------
      *          Parameter list for OMH904C (Get message data text)
      * Prototype for 'OMH901C'
     D OMH901C         PR                  EXTPGM('OMH901C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGDTA_                             LIKE(MSGDTA)
      *
      *          ------------------------------------------
      * Prototype for 'OMX105'
     D OMX105          PR                  EXTPGM('OMX105')
     D FMHEDC_                             LIKE(FMHEDC)
     D HEDD_                               LIKE(HEDD)
      * Prototype for 'OMX121'
     D OMX121          PR                  EXTPGM('OMX121')
     D FMHEDC_                             LIKE(FMHEDC)
     D FMQUE#_                             LIKE(FMQUE#)
     D INDI_                               LIKE(INDI)
     D QUES_                               LIKE(QUES)
      * Prototype for 'OMX122'
     D OMX122          PR                  EXTPGM('OMX122')
     D FMHEDC_                             LIKE(FMHEDC)
     D FMQUE#_                             LIKE(FMQUE#)
     D FMANS#_                             LIKE(FMANS#)
     D ANSC_                               LIKE(ANSC)
      * Prototype for 'OMX120'
     D OMX120          PR                  EXTPGM('OMX120')
     D FMHEDC_                             LIKE(FMHEDC)
     D FMGLS#_                             LIKE(FMGLS#)
     D DESC_                               LIKE(DESC)
      * Prototype for 'OMX124'
     D OMX124          PR                  EXTPGM('OMX124')
     D FMHEDC_                             LIKE(FMHEDC)
     D FMGLS#_                             LIKE(FMGLS#)
     D FMGQS#_                             LIKE(FMGQS#)
     D QUE#_                               LIKE(QUE#)
     D INDI_                               LIKE(INDI)
     D QUES_                               LIKE(QUES)
      * Prototype for 'OMX106'
     D OMX106          PR                  EXTPGM('OMX106')
     D FMHEDC_                             LIKE(FMHEDC)
     D TABC_                               LIKE(TABC)
     D FMSEV1_                             LIKE(FMSEV1)
     D DESC_                               LIKE(DESC)
      * Prototype for 'OMX125'
     D OMX125          PR                  EXTPGM('OMX125')
     D FMHEDC_                             LIKE(FMHEDC)
     D FMITPC_                             LIKE(FMITPC)
     D FMITPD_                             LIKE(FMITPD)
      * Prototype for 'OMX126'
     D OMX126          PR                  EXTPGM('OMX126')
     D FMHEDC_                             LIKE(FMHEDC)
     D FMSTRC_                             LIKE(FMSTRC)
     D STRD_                               LIKE(STRD)
      * Prototype for 'OMX127'
     D OMX127          PR                  EXTPGM('OMX127')
     D FMHEDC_                             LIKE(FMHEDC)
     D FMSTRC_                             LIKE(FMSTRC)
     D FMRES#_                             LIKE(FMRES#)
     D STAT_                               LIKE(STAT)
     D STAD_                               LIKE(STAD)
      * Prototype for 'OMX305'
     D OMX305          PR                  EXTPGM('OMX305')
     D P#QOAI_                             LIKE(P#QOAI)
     D P#QUES_                             LIKE(P#QUES)
     D P#ANSC_                             LIKE(P#ANSC)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (Send message)
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
      *          OMHQL Question list file on question list
      * Prototype for 'OMH961'
     D OMH961          PR                  EXTPGM('OMH961')
     D PGM_                                LIKEDS(PGM)
     D P#FILE_                             LIKE(P#FILE)
      * Prototype for 'QMHMOVPM'
     D QMHMOVPM        PR                  EXTPGM('QMHMOVPM')
     D P#MSKC_                             LIKE(P#MSKC)
     D P#MSTA_                             LIKE(P#MSTA)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#PMQC_                             LIKE(P#PMQC)
     D P#PSC#_                             LIKE(P#PSC#)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'OMH007'
     D OMH007          PR                  EXTPGM('OMH007')
      * Prototype for 'OMH904C'
     D OMH904C         PR                  EXTPGM('OMH904C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGTXT_                             LIKE(MSGTXT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH029 (Get info)
      * *ENTRY Interface for Main Procedure
     D OMH003          PI
     D P#HEDC                         5
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D k#ans#          S              3P 0
     D k#qls#          S              5P 0
     D k#qsq#          S              3P 0
     D k#que#          S              5P 0
     D k#res#          S              3P 0
     D p#pri#          S              3P 0
     D p#qls#          S              5P 0
     D p#res#          S              3P 0
     D p#usf3          S             15P 0
     D s#qsq#          S              3P 0
     D w#cr03          S              5P 0
     D w#cr04          S              5P 0
     D EROR01          S                   LIKE(*IN99)
     D EROR02          S                   LIKE(*IN99)
     D EROR03          S                   LIKE(*IN99)
     D EROR04          S                   LIKE(*IN99)
     D EROR05          S                   LIKE(*IN99)
     D I#PSSR          S              1
     D I#SC05          S                   LIKE(INDI)
     D I#0071          S                   LIKE(*IN99)
     D K#ITPC          S                   LIKE(ITPC)
     D K#SEVC          S                   LIKE(SEVC)
     D K#STAT          S                   LIKE(STAT)
     D K#STRC          S                   LIKE(STRC)
     D K#TABC          S                   LIKE(TABC)
     D LOOP01          S                   LIKE(*IN99)
     D LOOP02          S                   LIKE(*IN99)
     D LOOP03          S                   LIKE(*IN99)
     D LOOP04          S                   LIKE(*IN99)
     D LOOP05          S                   LIKE(*IN99)
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#ACTN          S                   LIKE(ACTN)
     D P#ANSC          S                   LIKE(ANSC)
     D P#CDE#          S                   LIKE(CTN#)
     D P#CTN#          S                   LIKE(CTN#)
     D P#FILE          S             57
     D P#HEDD          S                   LIKE(HEDD)
     D P#INC#          S                   LIKE(INC#)
     D P#ITPC          S                   LIKE(ITPC)
     D P#MSTA          S             40
     D P#OPTI          S                   LIKE(INDI)
     D P#QOAI          S                   LIKE(INDI)
     D P#QUES          S                   LIKE(QUES)
     D P#SEV1          S                   LIKE(SEV1)
     D P#SEV2          S                   LIKE(SEV2)
     D P#SEV3          S                   LIKE(SEV3)
     D P#STRC          S                   LIKE(STRC)
     D P#SUL#          S                   LIKE(SUL#)
     D P#USF1          S                   LIKE(USF1)
     D P#USF2          S                   LIKE(USF2)
     D RRN01           S              5  0
     D RRN03           S              5  0
     D RRN04           S              5  0
     D S#HEDC          S                   LIKE(HEDC)
     D S#IN80          S                   LIKE(*IN80)
     D S#IN81          S                   LIKE(*IN81)
     D W#CR01          S              5  0
     D W#IX            S              5  0
     D W#MSGI          S              1
     D W#OPTI          S                   LIKE(FMOPTI)
     D W#SAV#          S              5  0
     D W#VALC          S              5  0
     D W#WRKI          S              1
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
      *          Get the parameters
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Do the processing routine if authorised
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      P#STAT = C#NORM
     C                   EXSR      SRVERW
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Do the total last record routine
      *          ------------------------------------------
     C                   EXSR      SRTTLR
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
      * SNDMSG - Send message                                        * /
      *                                                              * /
      * This routine displays an error wich occured during user      * /
      * keyed data validation.                                       * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SNDMSG        BEGSR
      *
      *          ------------------------------------------
      *          Call Program sender
      *          ------------------------------------------
     C                   EVAL      MSGF = 'OMHMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( MSGID : MSGF : MSGLIB :
     C                             MSGDTA )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * PROMPT - Prompt a field                                      * /
      *                                                              * /
      * This routine prompts a field.                                * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     PROMPT        BEGSR
      *
      *          ------------------------------------------
      *          Display 1.
      *          HelpDesk code
      *          ------------------------------------------
     C                   IF        FMFMTC = 'OMH00302'                          B01
     C                             AND FMFLDC = 'FMHEDC'                        B02
     C                   MOVE      FMHEDC        S#HEDC
     C                   CALLP     OMX105 ( FMHEDC : HEDD )
     C                   IF        FMHEDC <> S#HEDC                             B02
     C                   EXSR      SRCHGH
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Display 4.
      *          Question
      *          ------------------------------------------
     C                   IF        FMFMTC = 'OMH00309'                          B02
     C                             AND FMFLDC = 'FMQUE#'                        B02
     C                   EVAL      QUES = FMQUE2
     C                   CALLP     OMX121 ( FMHEDC : FMQUE# : INDI : QUES )
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Display 5.
      *          Answer
      *          ------------------------------------------
     C                   IF        FMFMTC = 'OMH00311'                          B03
     C                             OR FMFMTC = 'OMH00312'                       B03
     C                   IF        FMFLDC = 'FMANS#'                            B04
     C                             AND *IN80 = *OFF
     C                             AND *IN80 = *OFF
     C                   CALLP(E)  OMX122 ( FMHEDC : FMQUE# : FMANS# : ANSC
     C                             )
     C                   IF        %ERROR = *OFF
     C                   EVAL      FMANS2 = ANSC
     C                   ENDIF
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Question list
      *          ------------------------------------------
     C                   IF        FMFLDC = 'FMGLS#'                            B04
     C                   CALLP(E)  OMX120 ( FMHEDC : FMGLS# : DESC )
     C                   IF        %ERROR = *OFF
     C                   EVAL      F2DES1 = DESC
     C                   ENDIF
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Question in question list
      *          ------------------------------------------
     C                   IF        FMFLDC = 'FMGQS#'                            B04
     C                   CALLP(E)  OMX124 ( FMHEDC : FMGLS# : FMGQS# : QUE#
     C                             : INDI : QUES )
     C                   IF        %ERROR = *OFF
     C                   EVAL      F2QUE2 = QUES
     C                   ENDIF
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Severity 1
      *          ------------------------------------------
     C                   IF        FMFLDC = 'FMSEV1'                            B04
     C                   EVAL      TABC = '01'
     C                   CALLP(E)  OMX106 ( FMHEDC : TABC : FMSEV1 : DESC )
     C                   IF        %ERROR = *OFF
     C                   EVAL      FMSE1D = DESC
     C                   ENDIF
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Severity 2
      *          ------------------------------------------
     C                   IF        FMFLDC = 'FMSEV2'                            B04
     C                   EVAL      TABC = '02'
     C                   CALLP(E)  OMX106 ( FMHEDC : TABC : FMSEV2 : DESC )
     C                   IF        %ERROR = *OFF
     C                   EVAL      FMSE2D = DESC
     C                   ENDIF
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Severity 3
      *          ------------------------------------------
     C                   IF        FMFLDC = 'FMSEV3'                            B04
     C                   EVAL      TABC = '03'
     C                   CALLP(E)  OMX106 ( FMHEDC : TABC : FMSEV3 : DESC )
     C                   IF        %ERROR = *OFF
     C                   EVAL      FMSE3D = DESC
     C                   ENDIF
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Incident type
      *          ------------------------------------------
     C                   IF        FMFLDC = 'FMITPC'                            B04
     C                   CALLP     OMX125 ( FMHEDC : FMITPC : FMITPD )
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Status route
      *          ------------------------------------------
     C                   IF        FMFLDC = 'FMSTRC'                            B04
     C                   CALLP(E)  OMX126 ( FMHEDC : FMSTRC : STRD )
     C                   IF        %ERROR = *OFF
     C                   EVAL      FMSTRD = STRD
     C                   ENDIF
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Route entry sequence (= status)
      *          ------------------------------------------
     C                   IF        FMFLDC = 'FMRES#'                            B04
     C                   CALLP(E)  OMX127 ( FMHEDC : FMSTRC : FMRES# : STAT
     C                             : STAD )
     C                   IF        %ERROR = *OFF
     C                   EVAL      FMSTAD = STAD
     C                   ENDIF
     C                   ENDIF                                                  E04
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Invalid prompt location.
      *          ------------------------------------------
     C                   EVAL      MSGID = 'PMT0002'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E03
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
      * SRVERW - Main program processing routine                    *
      *                                                             *
      * This routine calls the first screen routine until the field *
      * P#STAT is filled.                                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Enter loop to call screen 1 processing
      *          ------------------------------------------
     C                   EXSR      SRSC01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC01 - Work with question lists                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EXSR      SRIN01
      *
      *          ------------------------------------------
      *          Do while no end
      *          ------------------------------------------
     C                   DOU       LOOP01 = *OFF                                B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR01 = *OFF                                B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH00303
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        RRN01 = *ZEROS                               B03
     C                   EVAL      *IN72 = *ON
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   EXFMT     OMH00302
      *
      *          ------------------------------------------
      *          CMD-12 is CMD-3 is exit
      *          ------------------------------------------
     C                   IF        *INKL = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
     C                   IF        *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   EVAL      W#CR01 = D#CRC#
      *
      *          ------------------------------------------
      *          Clear message queue
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Setof screen error indicators
      *          ------------------------------------------
     C                   EVAL      *IN70 = *OFF
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      *IN72 = *OFF
     C                   EVAL      EROR01 = *OFF
     C                   EVAL      LOOP01 = *OFF
      *
      *          ------------------------------------------
      *          Command key processing.
      *          F4=Prompt
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   EXSR      PROMPT
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B04
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Change the HelpDesk if required.
      *          ------------------------------------------
     C                   IF        S#HEDC <> FMHEDC                             B05
     C                   EXSR      SRCHGH
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          F6=Add question list
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B06
     C                   EXSR      SRSC02
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X06
      *
      *          ------------------------------------------
      *          Edit
      *          ------------------------------------------
     C                   EXSR      SRED01
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors or command key
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do processing
      *          ------------------------------------------
     C                   IF        *INKC = *OFF                                 B02
     C                   EXSR      SRPR01
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End process loop
      *          ------------------------------------------
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIN01 - Screen 1 initialisation                            *
      *                                                             *
      * This routine blanks the input fields on the screen.         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMPOS
     C                   EVAL      S#HEDC = P#HEDC
      *
      *          ------------------------------------------
      *          Do refresh subroutine
      *          ------------------------------------------
     C                   EXSR      SRRF01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF01 - Work with question lists  - refresh                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   MOVE      S#HEDC        FMHEDC
     C                   EVAL      RRN01 = *ZEROS
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH00302
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Read first security record
      *          ------------------------------------------
     C     FMHEDC        SETLL     OMHQLL1
     C     FMHEDC        READE(N)  OMHQLL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      QLQLS#        FMQLS#
     C                   MOVE      QLDESC        FMDESC
     C                   ADD       1             RRN01
     C                   WRITE     OMH00301
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     FMHEDC        READE(N)  OMHQLL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Fill current subfile relative record number
      *          to display first subfile page
      *          ------------------------------------------
     C                   Z-ADD     1             CRRN01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED01 - Work with question lists - edit                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
     C                   EVAL      W#VALC = *ZEROS
     C                   EVAL      W#IX = 1
     C                   EVAL      CRRN01 = *ZEROS
     C                   EVAL      W#MSGI = *OFF
     C                   EVAL      W#SAV# = RRN01
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   READC(E)  OMH00301
     C                   EVAL      *IN99 = %ERROR OR %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B01
     C                   MOVE      FMOPTI        W#WRKI
     C                   IF        W#WRKI = *BLANKS                             B02
     C                   MOVEL     FMOPTI        W#WRKI
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      W#WRKI        FMOPTI
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Do if option is not blank
      *          ------------------------------------------
     C                   IF        FMOPTI <> *BLANKS                            B02
      *
      *          ------------------------------------------
      *          If option is valid, then check the number of valid
      *          codes. *IN93 is the subfile next change indicator
      *          ------------------------------------------
     C                   IF        FMOPTI = ' 2'                                B03
     C                             OR FMOPTI = ' 4'
     C                             OR FMOPTI = ' 8'
     C                             OR FMOPTI = '12'
      *
      *          ------------------------------------------
      *          Delete, still details?
      *          ------------------------------------------
     C                   IF        FMOPTI = ' 4'                                B04
     C     $QL1K1        SETLL     OMQLQL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B05
     C                   EVAL      MSGID = 'DLT0029'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Option is valid, check if subfile record is still
      *          active if not then error, else ok.
      *          ------------------------------------------
     C                   IF        EROR01 = *OFF                                B04
     C     $QL1K1        SETLL     OMHQLL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B05
     C                   EVAL      MSGID = 'FILE002'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X05
     C                   ADD       1             W#VALC
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If option is other than valid, then field is in error
      *          Reverse screen fields to indicate this
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
      *
      *          ------------------------------------------
      *          Send the message
      *          ------------------------------------------
     C                   IF        W#MSGI = *OFF                                B04
     C                   MOVEL(P)  FMOPTI        MSGDTA
     C                   EVAL      MSGID = 'OPTE021'
     C                   EXSR      SNDMSG
     C                   EVAL      W#MSGI = *ON
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If an error was found, then position on the page of
      *          the first error
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B03
     C                             AND *IN70 = *ON
     C                   Z-ADD     RRN01         CRRN01
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Update subfile
      *          ------------------------------------------
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH00301
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   UPDATE    OMH00301
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH00301
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          If subfile must be positioned, and no errors occured
      *          then position it.
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B01
     C                             AND FMPOS <> *ZEROS
     C                   FOR       W#IX = 1 TO W#SAV#                           B02
     C     W#IX          CHAIN     OMH00301
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        FMQLS# >= FMPOS                              B03
     C                   Z-ADD     W#IX          CRRN01
     C                   EVAL      W#IX = W#SAV#
     C                   MOVE      *ZEROS        FMPOS
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E03
     C                   ENDFOR                                                 E02
      *
     C                   IF        FMPOS <> *ZEROS                              B02
     C                   MOVE      *ZEROS        FMPOS
     C                   Z-ADD     W#SAV#        CRRN01
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E02
      *
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Fill the current relative record on the page to
      *          display.
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B02
     C                   Z-ADD     W#CR01        CRRN01
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Reset overall relative record number
      *          ------------------------------------------
     C                   EVAL      RRN01 = W#SAV#
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR01 - Work with question lists - process                 *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      W#IX = 1
      *
      *          ------------------------------------------
      *          Read subfile next changed record
      *          If records in subfile and valid codes found.
      *          ------------------------------------------
     C                   IF        RRN01 > *ZEROS                               B01
     C                             AND W#VALC > *ZEROS
     C                   READC     OMH00301
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Read subfile until valid option found
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Loop again for options where typed.
      *          ------------------------------------------
     C                   EVAL      LOOP01 = *ON
     C                   EVAL      W#CR01 = RRN01
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPTI        W#OPTI
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    OMH00301
      *
      *          ------------------------------------------
      *          Fill the description
      *          ------------------------------------------
     C                   MOVEL     FMDESC        FMDES1
      *
      *          ------------------------------------------
      *          Do action
      *          ------------------------------------------
     C                   SELECT                                                 B03
     C                   WHEN      W#OPTI = ' 8'                                X03
     C                   EXSR      SRTEST
     C                   WHEN      W#OPTI = '12'                                X03
     C                   EXSR      SRSC03
     C                   OTHER                                                  X03
     C                   EXSR      SRSC02
     C                   ENDSL                                                  E03
      *
      *          ------------------------------------------
      *          Exit if F3
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B03
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Leave if F12.
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                   LEAVE
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next changed
      *          ------------------------------------------
     C                   READC     OMH00301
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If end of file found, then end loop
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Reset subfile relative record number
      *          ------------------------------------------
     C                   Z-ADD     W#CR01        CRRN01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC02 - Screen 2 processing routine                        *
      *                                                             *
      * This routine displays the second screen. The flow of the    *
      * screen is slightly different then normal. The change, add   *
      * and delete options, are combined into one screen, controled *
      * with indicator 80 & 81. 80 controls the protection of the   *
      * question list, and 81 controls the protection of the        *
      * description.                                                *
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 off = Change                                    *
      * 80 on  & 81 on  = Delete                                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC02        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EXSR      SRIN02
      *
      *          ------------------------------------------
      *          Do while no end
      *          ------------------------------------------
     C                   DOU       LOOP02 = *OFF                                B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR02 = *OFF                                B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the screen
      *          ------------------------------------------
     C                   EXFMT     OMH00304
      *
      *          ------------------------------------------
      *          F3=Exit
      *          ------------------------------------------
     C                   IF        *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Clear message queue
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Setof screen error indicators
      *          ------------------------------------------
     C                   EVAL      *IN(70) = *OFF
     C                   EVAL      *IN(71) = *OFF
     C                   EVAL      EROR02 = *OFF
     C                   EVAL      LOOP02 = *OFF
      *
      *          ------------------------------------------
      *          Do edit routine if not F3 or F12.
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B03
     C                   EXSR      SRED02
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do processing if not F12
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B02
     C                   EXSR      SRPR02
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End process loop
      *          ------------------------------------------
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIN02 - Screen 2 initialisation                            *
      *                                                             *
      * This routine blanks the input fields on the screen.         *
      * Pending on the required action.           The change, add   *
      * and delete options, are combined into one screen, controled *
      * with indicator 80 & 81. 80 controls the protection of the   *
      * question list, and 81 controls the protection of the        *
      * description.                                                *
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 off = Change                                    *
      * 80 on  & 81 on  = Delete                                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN02        BEGSR
      *
      *          ------------------------------------------
      *          Determine the action
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B01
     C                   EVAL      *IN(80) = *OFF
     C                   EVAL      *IN(81) = *OFF
     C                   ELSE                                                   X01
     C                   IF        W#OPTI = ' 2'                                B02
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *OFF
     C                   ELSE                                                   X02
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *ON
      *
      *          ------------------------------------------
      *          Display deletion message
      *          ------------------------------------------
     C                   EVAL      MSGID = 'DLT0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do refresh subroutine
      *          ------------------------------------------
     C                   EXSR      SRRF02
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF02 - Screen 2 refresh                                   *
      *                                                             *
      * This screen is called for future processing.                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF02        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields if addition
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
      *
      *                    --------------------------------
      *                    Get the last sequence number
      *                    --------------------------------
     C     FMHEDC        SETGT     OMHQLL1
     C     FMHEDC        READPE(N) OMHQLL1
     C                   EVAL      *IN99 = %EOF
     C                   IF        *IN99 = *ON
     C                   Z-ADD     10            FMQLS#
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C     QLQLS#        ADD       10            FMQLS#
     C                   ENDIF
     C                   MOVE      *BLANKS       FMDESC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get record if change.
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B01
     C                             AND *IN81 = *OFF
     C     $QL1K1        CHAIN(N)  OMHQLL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      QLQLS#        FMQLS#
     C                   MOVE      QLDESC        FMDESC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED02 - Screen 2 edit                                      *
      *                                                             *
      * This routine checks if the data is ok.                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED02        BEGSR
      *
      *          ------------------------------------------
      *          Test if possible
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
      *
      *          ------------------------------------------
      *          Check if the record exists
      *          ------------------------------------------
     C     $QL1K1        SETLL     OMHQLL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      MSGID = 'FILE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN70 = *ON
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
      * SRPR02 - Process screen 2                                   *
      *                                                             *
      * This routine updates the question list file                 *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR02        BEGSR
      *
      *          ------------------------------------------
      *          Loop again if fields where changed.
      *          ------------------------------------------
     C                   MOVE      *IN40         LOOP02
      *
      *          ------------------------------------------
      *          Delete the file if wanted.
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B01
     C                             AND *IN81 = *ON
     C     $QL1K1        DELETE    OMHQLL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Do if something changed.
      *          ------------------------------------------
     C                   IF        *IN40 = *ON                                  B02
      *
      *          ------------------------------------------
      *          Get the file
      *          ------------------------------------------
     C     $QL1K1        CHAIN     OMHQLL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill the file
      *          ------------------------------------------
     C                   MOVE      FMHEDC        QLHEDC
     C                   MOVE      FMQLS#        QLQLS#
     C                   MOVE      FMDESC        QLDESC
      *
      *          ------------------------------------------
      *          Add or update the file. Fill description only on add.
      *          *IN99=*OFF security record found in previous chain
      *          *IN99=*ON  security record not found in previous chain
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B03
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMHQLR
     C                   ENDIF
     C                   ELSE                                                   X03
     C                   IF        *IN99 = *ON
     C                   WRITE     OMHQLR
     C                   ENDIF
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If in ADD mode then force CHANGE mode.
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B03
     C                             AND *IN81 = *OFF
     C                   EVAL      *IN80 = *ON
     C                   ENDIF                                                  E03
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
      * SRSC03 - Work with questions                                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC03        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EXSR      SRIN03
      *
      *          ------------------------------------------
      *          Do while no end
      *          ------------------------------------------
     C                   DOU       LOOP03 = *OFF                                B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR03 = *OFF                                B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH00307
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        RRN03 = *ZEROS                               B03
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   EXFMT     OMH00306
      *
      *          ------------------------------------------
      *          CMD-12 is leave, CMD-3 is exit
      *          ------------------------------------------
     C                   IF        *INKL = *ON
     C                   LEAVE
     C                   ENDIF
     C                   IF        *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   EVAL      W#CR03 = D#CRC#
      *
      *          ------------------------------------------
      *          Clear message queue
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Setof screen error indicators
      *          ------------------------------------------
     C                   EVAL      *IN70 = *OFF
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      *IN72 = *OFF
     C                   EVAL      EROR03 = *OFF
     C                   EVAL      LOOP03 = *OFF
      *
      *          ------------------------------------------
      *          Command key processing.
      *          F4=Prompt
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   EXSR      PROMPT
     C                   EVAL      EROR03 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B04
     C                   EXSR      SRRF03
     C                   EVAL      EROR03 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Change the HelpDesk if required.
      *          ------------------------------------------
     C                   IF        S#HEDC <> FMHEDC                             B05
     C                   EXSR      SRCHGH
     C                   EVAL      EROR03 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          F6=Connect new question
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B06
     C                   EXSR      SRSC04
     C                   EVAL      EROR03 = *ON
     C                   ELSE                                                   X06
      *
      *          ------------------------------------------
      *          Edit
      *          ------------------------------------------
     C                   EXSR      SRED03
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors or command key
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          CMD-12 is leave
      *          ------------------------------------------
     C                   IF        *INKL = *ON
     C                   LEAVE
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Do processing
      *          ------------------------------------------
     C                   EXSR      SRPR03
      *
      *          ------------------------------------------
      *          End process loop
      *          ------------------------------------------
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIN03 - Screen 3 initialisation                            *
      *                                                             *
      * This routine blanks the input fields on the screen.         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN03        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMPOS
     C                   EVAL      S#HEDC = P#HEDC
      *
      *          ------------------------------------------
      *          Fill the question list question key.
      *          ------------------------------------------
     C                   Z-ADD     FMQLS#        K#QLS#
      *
      *          ------------------------------------------
      *          Do refresh subroutine
      *          ------------------------------------------
     C                   EXSR      SRRF03
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF03 - Work with questions                                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF03        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   MOVE      S#HEDC        FMHEDC
     C                   EVAL      RRN03 = *ZEROS
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH00306
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Read first question list question
      *          ------------------------------------------
     C     $LQ1K1        SETLL     OMQLQL1
     C     $LQ1K1        READE(N)  OMQLQL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      LQQUE#        FMQUE#
     C                   MOVE      LQQUE#        K#QUE#
     C                   MOVE      LQQSQ#        FMQSQ#
      *
      *                    --------------------------------
      *                    Get the question
      *                    --------------------------------
     C     $QU1K1        CHAIN     OMHQUL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL     QUQUES        FMQUE1
      *
      *                    --------------------------------
      *                    Write the subfile record
      *                    --------------------------------
     C                   ADD       1             RRN03
     C                   WRITE     OMH00305
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     $LQ1K1        READE(N)  OMQLQL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Fill current subfile relative record number
      *          to display first subfile page
      *          ------------------------------------------
     C                   Z-ADD     1             CRRN03
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED03 - Work with questions - edit                         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED03        BEGSR
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
     C                   EVAL      W#VALC = *ZEROS
     C                   EVAL      W#IX = 1
     C                   EVAL      CRRN03 = *ZEROS
     C                   EVAL      W#MSGI = *OFF
     C                   EVAL      W#SAV# = RRN03
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   READC(E)  OMH00305
     C                   EVAL      *IN99 = %ERROR OR %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B01
     C                   MOVE      FMOPTI        W#WRKI
     C                   IF        W#WRKI = *BLANKS                             B02
     C                   MOVEL     FMOPTI        W#WRKI
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      W#WRKI        FMOPTI
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Do if option is not blank
      *          ------------------------------------------
     C                   IF        FMOPTI <> *BLANKS                            B02
      *
      *          ------------------------------------------
      *          If option is valid, then check the number of valid
      *          codes. *IN93 is the subfile next change indicator
      *          ------------------------------------------
     C                   IF        FMOPTI = ' 4'                                B03
     C                             OR FMOPTI = ' 5'
     C                             OR FMOPTI = '12'
      *
      *          ------------------------------------------
      *          Option is valid, check if subfile record is still
      *          active in security file if not then error, else ok.
      *          ------------------------------------------
     C     $QL1K1        SETLL     OMHQLL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B04
     C                   EVAL      MSGID = 'FILE002'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR03 = *ON
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X04
     C                   ADD       1             W#VALC
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If option is other than valid, then field is in error
      *          Reverse screen fields to indicate this
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR03 = *ON
      *
      *          ------------------------------------------
      *          Send the message
      *          ------------------------------------------
     C                   IF        W#MSGI = *OFF                                B04
     C                   MOVEL(P)  FMOPTI        MSGDTA
     C                   EVAL      MSGID = 'OPTE021'
     C                   EXSR      SNDMSG
     C                   EVAL      W#MSGI = *ON
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If an error was found, then position on the page of
      *          the first error
      *          ------------------------------------------
     C                   IF        CRRN03 = *ZEROS                              B03
     C                             AND *IN70 = *ON
     C                   Z-ADD     RRN03         CRRN03
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Update subfile
      *          ------------------------------------------
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH00305
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   UPDATE    OMH00305
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH00305
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          If subfile must be positioned, and no errors occured
      *          then position it.
      *          ------------------------------------------
     C                   IF        CRRN03 = *ZEROS                              B01
     C                             AND FMPOS <> *ZEROS
     C                   FOR       W#IX = 1 TO W#SAV#                           B02
     C     W#IX          CHAIN     OMH00305
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        FMQLS# >= FMPOS                              B03
     C                   Z-ADD     W#IX          CRRN03
     C                   EVAL      W#IX = W#SAV#
     C                   MOVE      *ZEROS        FMPOS
     C                   EVAL      EROR03 = *ON
     C                   ENDIF                                                  E03
     C                   ENDFOR                                                 E02
      *
     C                   IF        FMPOS <> *ZEROS                              B02
     C                   MOVE      *ZEROS        FMPOS
     C                   Z-ADD     W#SAV#        CRRN03
     C                   EVAL      EROR03 = *ON
     C                   ENDIF                                                  E02
      *
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Fill the current relative record on the page to
      *          display.
      *          ------------------------------------------
     C                   IF        CRRN03 = *ZEROS                              B02
     C                   Z-ADD     W#CR03        CRRN03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Reset overall relative record number
      *          ------------------------------------------
     C                   EVAL      RRN03 = W#SAV#
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR03 - Work with questions - process                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR03        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      W#IX = 1
      *
      *          ------------------------------------------
      *          Read subfile next changed record
      *          If records in subfile and valid codes found.
      *          ------------------------------------------
     C                   IF        RRN03 > *ZEROS                               B01
     C                             AND W#VALC > *ZEROS
     C                   READC     OMH00305
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Read subfile until valid option found
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Loop again for options where typed.
      *          ------------------------------------------
     C                   EVAL      LOOP03 = *ON
     C                   EVAL      W#CR03 = RRN03
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPTI        W#OPTI
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    OMH00305
      *
      *          ------------------------------------------
      *          Do action
      *          ------------------------------------------
     C                   IF        W#OPTI <> ' 5'                               B03
     C                   EXSR      SRSC04
      *
      *          ------------------------------------------
      *          5=Display question
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   MOVE      FMQUE#        K#QUE#
     C     $QU1K1        CHAIN     OMHQUL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   EVAL      P#QOAI = *ON
     C                   MOVE      QUQUES        P#QUES
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMX305 ( P#QOAI : P#QUES : P#ANSC :
     C                             P#STAT )
      *
     C                   IF        P#STAT = C#EXIT                              B04
     C                   EVAL      *INKC = *ON
     C                   ENDIF                                                  E04
      *
     C                   IF        P#STAT = C#CNCL                              B04
     C                   EVAL      *INKL = *ON
     C                   ENDIF                                                  E04
      *
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Exit if F3
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B03
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Leave if F12.
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                   LEAVE
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next changed
      *          ------------------------------------------
     C                   READC     OMH00305
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If end of file found, then end loop
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Reset subfile relative record number
      *          ------------------------------------------
     C                   Z-ADD     W#CR03        CRRN03
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC04 - Maintain question                                  *
      *                                                             *
      * This display is used to maintain the question which is      *
      * connected to the question list. The indicator settings      *
      * are.                                                        *
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 off = Change                                    *
      * 80 on  & 81 on  = Delete                                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC04        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EXSR      SRIN04
      *
      *          ------------------------------------------
      *          Do while no end
      *          ------------------------------------------
     C                   DOU       LOOP04 = *OFF                                B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR04 = *OFF                                B02
      *
      *          ------------------------------------------
      *          If subfile is empty, then write record to
      *          clear some lines on the display.
      *          ------------------------------------------
     C                   IF        RRN04 = *ZEROS                               B03
     C                   WRITE     OMH003CL
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH00310
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        RRN04 = *ZEROS                               B03
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   EXFMT     OMH00309
      *
      *          ------------------------------------------
      *          CMD-12 is leave, CMD-3 is exit
      *          ------------------------------------------
     C                   IF        *INKL = *ON
     C                   LEAVE
     C                   ENDIF
     C                   IF        *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   EVAL      W#CR04 = D#CRC#
      *
      *          ------------------------------------------
      *          Clear message queue
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Setof screen error indicators
      *          ------------------------------------------
     C                   EVAL      *IN70 = *OFF
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      *IN72 = *OFF
     C                   EVAL      EROR04 = *OFF
     C                   EVAL      LOOP04 = *OFF
      *
      *          ------------------------------------------
      *          Command key processing.
      *          F4=Prompt
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   EXSR      PROMPT
     C                   EVAL      EROR04 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B04
     C                   EXSR      SRRF04
     C                   EVAL      EROR04 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          F6=Connect answer
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B05
     C                   EVAL      S#IN80 = *IN80
     C                   EVAL      S#IN81 = *IN81
     C                   EXSR      SRSC05
     C                   EVAL      EROR04 = *ON
     C                   EVAL      *IN(80) = S#IN80
     C                   EVAL      *IN(81) = S#IN81
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          Edit
      *          ------------------------------------------
     C                   EXSR      SRED04
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors or command key
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          CMD-12 is leave
      *          ------------------------------------------
     C                   IF        *INKL = *ON
     C                   LEAVE
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Do processing
      *          ------------------------------------------
     C                   EXSR      SRPR04
      *
      *          ------------------------------------------
      *          End process loop
      *          ------------------------------------------
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIN04 - Screen 4 initialisation                            *
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 off = Change                                    *
      * 80 on  & 81 on  = Delete                                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN04        BEGSR
      *
      *          ------------------------------------------
      *          Determine the action
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B01
     C                   MOVEL     *BLANKS       FMQUE2
     C                   EVAL      *IN(80) = *OFF
     C                   EVAL      *IN(81) = *OFF
     C                   ELSE                                                   X01
      *
     C                   MOVEL     FMQUE1        FMQUE2
     C                   IF        W#OPTI = '12'                                B02
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *OFF
     C                   ELSE                                                   X02
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *ON
      *
      *          ------------------------------------------
      *          Display deletion message
      *          ------------------------------------------
     C                   EVAL      MSGID = 'DLT0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the question key.
      *          ------------------------------------------
     C                   Z-ADD     FMQSQ#        K#QSQ#
     C                   Z-ADD     FMQUE#        K#QUE#
      *
      *          ------------------------------------------
      *          Do refresh subroutine
      *          ------------------------------------------
     C                   EXSR      SRRF04
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF04 - Work with answers                                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF04        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      RRN04 = *ZEROS
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH00309
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Determine the next free sequence number if add.
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C     $QL1K1        SETGT     OMQLQL1
     C     $QL1K1        READPE(N) OMQLQL1
     C                   EVAL      *IN99 = %EOF
     C     LQQSQ#        ADD       10            FMQSQ#
     C                   MOVE      *ZEROS        FMQUE#
      *
      *          ------------------------------------------
      *          Else fill the key and read answer and actions records
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   MOVE      FMQSQ#        K#QSQ#
      *
      *          ------------------------------------------
      *          Read first answer for this question.
      *          ------------------------------------------
     C     $LA1K1        SETLL     OMQLAL1
     C     $LA1K1        READE(N)  OMQLAL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      LAANS#        FMANS#
      *
      *                    --------------------------------
      *                    Get the answer
      *                    --------------------------------
     C     $NS1K1        CHAIN     OMANSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL     NSANSC        FMANS1
      *
      *                    --------------------------------
      *                    Write the subfile record
      *                    --------------------------------
     C                   ADD       1             RRN04
     C                   WRITE     OMH00308
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     $LA1K1        READE(N)  OMQLAL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill current subfile relative record number
      *          to display first subfile page
      *          ------------------------------------------
     C                   Z-ADD     1             CRRN04
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED04 - Work with Answers edit                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED04        BEGSR
      *
      *          ------------------------------------------
      *          The record may not exist
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C                   Z-ADD     FMQSQ#        K#QSQ#
     C     $LQ1K3        SETLL     OMQLQL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      *IN72 = *ON
     C                   EVAL      EROR04 = *ON
     C                   EVAL      MSGID = 'FILE001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          The question number not be zeros
      *          ------------------------------------------
     C                   IF        FMQUE# = *ZERO                               B02
     C                   EVAL      MSGDTA = VAR(3)
     C                   EVAL      EROR04 = *ON
     C                   EVAL      *IN71 = *ON
     C                   EVAL      MSGID = 'ZERE001'
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          The question number must exist
      *          ------------------------------------------
     C                   Z-ADD     FMQUE#        K#QUE#
     C     $QU1K1        SETLL     OMHQUL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B03
     C                   MOVEA     VAR(3)        MC(1)
     C                   Z-ADD     FMQUE#        D#BIN4
     C                   MOVEA     D#BIA4        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      EROR04 = *ON
     C                   EVAL      *IN71 = *ON
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
     C                   IF        RRN04 > *ZEROS                               B02
     C                   EVAL      W#VALC = *ZEROS
     C                   EVAL      W#IX = 1
     C                   EVAL      CRRN04 = *ZEROS
     C                   EVAL      W#MSGI = *OFF
     C                   EVAL      W#SAV# = RRN04
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   READC(E)  OMH00308
     C                   EVAL      *IN99 = %ERROR OR %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B03
     C                   MOVE      FMOPTI        W#WRKI
     C                   IF        W#WRKI = *BLANKS                             B04
     C                   MOVEL     FMOPTI        W#WRKI
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      W#WRKI        FMOPTI
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Do if option is not blank
      *          ------------------------------------------
     C                   IF        FMOPTI <> *BLANKS                            B04
      *
      *          ------------------------------------------
      *          If option is valid, then check the number of valid
      *          codes. *IN93 is the subfile next change indicator
      *          ------------------------------------------
     C                   IF        FMOPTI = ' 2'                                B05
     C                             OR FMOPTI = ' 4'
     C                             OR FMOPTI = ' 5'
     C                             OR FMOPTI = ' 8'
      *
      *          ------------------------------------------
      *          Option is valid, check if subfile record is still
      *          active if not then error, else ok.
      *          ------------------------------------------
     C                   Z-ADD     FMANS#        K#ANS#
     C     $LA1K2        SETLL     OMQLAL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B06
     C                   EVAL      MSGID = 'FILE002'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR04 = *ON
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X06
     C                   ADD       1             W#VALC
     C                   ENDIF                                                  E06
      *
      *          ------------------------------------------
      *          If option is other than valid, then field is in error
      *          Reverse screen fields to indicate this
      *          ------------------------------------------
     C                   ELSE                                                   X05
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR04 = *ON
      *
      *          ------------------------------------------
      *          Send the message
      *          ------------------------------------------
     C                   IF        W#MSGI = *OFF                                B06
     C                   MOVEL(P)  FMOPTI        MSGDTA
     C                   EVAL      MSGID = 'OPTE021'
     C                   EXSR      SNDMSG
     C                   EVAL      W#MSGI = *ON
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          If an error was found, then position on the page of
      *          the first error
      *          ------------------------------------------
     C                   IF        CRRN04 = *ZEROS                              B05
     C                             AND *IN70 = *ON
     C                   Z-ADD     RRN04         CRRN04
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          Update subfile
      *          ------------------------------------------
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH00308
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X04
     C                   UPDATE    OMH00308
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH00308
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E03
      *
      *          ------------------------------------------
      *          Reset overall relative record number
      *          ------------------------------------------
     C                   EVAL      RRN04 = W#SAV#
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
      * SRPR04 - Work with Answers process                          *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR04        BEGSR
      *
      *          ------------------------------------------
      *          If this is add.
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C                   MOVE      FMHEDC        LQHEDC
     C                   Z-ADD     FMQLS#        LQQLS#
     C                   Z-ADD     FMQSQ#        LQQSQ#
     C                   Z-ADD     FMQUE#        LQQUE#
     C                   WRITE     OMQLQR
     C                   EVAL      LOOP04 = *ON
     C                   EVAL      *IN80 = *ON
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          If this is delete.
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B02
     C                             AND *IN81 = *ON
     C                   DOU       *IN99 = *ON                                  B03
     C     $LQ1K2        DELETE    OMQLAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDDO                                                  E03
     C                   DOU       *IN99 = *ON                                  B03
     C     $LQ1K2        DELETE    OMQLQL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDDO                                                  E03
     C                   EVAL      LOOP04 = *OFF
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   IF        RRN04 > *ZEROS                               B03
     C                   EVAL      W#IX = 1
      *
      *          ------------------------------------------
      *          Read subfile next changed record
      *          If records in subfile and valid codes found.
      *          ------------------------------------------
     C                   IF        RRN04 > *ZEROS                               B04
     C                             AND W#VALC > *ZEROS
     C                   READC     OMH00308
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Read subfile until valid option found
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B05
      *
      *          ------------------------------------------
      *          Loop again for options where typed.
      *          ------------------------------------------
     C                   EVAL      LOOP04 = *ON
     C                   EVAL      W#CR04 = RRN04
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPTI        W#OPTI
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    OMH00308
      *
      *          ------------------------------------------
      *          Fill the description
      *          ------------------------------------------
     C                   MOVEL     FMANS1        FMANS2
      *
      *          ------------------------------------------
      *          Do action
      *          ------------------------------------------
     C                   IF        W#OPTI <> ' 8'                               B06
     C                   EVAL      S#IN80 = *IN80
     C                   EVAL      S#IN81 = *IN81
     C                   EXSR      SRSC05
     C                   Z-ADD     FMQLS#        K#QLS#
     C                   Z-ADD     FMQSQ#        K#QSQ#
     C                   Z-ADD     FMQUE#        K#QUE#
     C                   EVAL      *IN80 = S#IN80
     C                   EVAL      *IN81 = S#IN81
     C                   ELSE                                                   X06
      *
      *          ------------------------------------------
      *          8=Display question & answer
      *          ------------------------------------------
     C                   MOVE      FMQUE#        K#QUE#
     C     $QU1K1        CHAIN     OMHQUL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      QUQUES        P#QUES
      *
     C     $NS1K1        CHAIN     OMANSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      NSANSC        P#ANSC
     C                   EVAL      P#QOAI = '3'
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMX305 ( P#QOAI : P#QUES : P#ANSC :
     C                             P#STAT )
      *
     C                   IF        P#STAT = C#EXIT                              B07
     C                   EVAL      *INKC = *ON
     C                   ENDIF                                                  E07
      *
     C                   IF        P#STAT = C#CNCL                              B07
     C                   EVAL      *INKL = *ON
     C                   ENDIF                                                  E07
     C                   ENDIF                                                  E06
      *
      *          ------------------------------------------
      *          Exit if F3
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B06
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E06
      *
      *          ------------------------------------------
      *          Leave if F12.
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B06
     C                   LEAVE
     C                   ENDIF                                                  E06
      *
      *          ------------------------------------------
      *          Read next changed
      *          ------------------------------------------
     C                   READC     OMH00308
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If end of file found, then end loop
      *          ------------------------------------------
     C                   ENDDO                                                  E05
      *
      *          ------------------------------------------
      *          Reset subfile relative record number
      *          ------------------------------------------
     C                   Z-ADD     W#CR04        CRRN04
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
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
      * SRSC05 - Add answer and action                              *
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 off = Change                                    *
      * 80 on  & 81 on  = Delete                                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC05        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EXSR      SRIN05
      *
      *          ------------------------------------------
      *          Do while no end
      *          ------------------------------------------
     C                   DOU       LOOP05 = *OFF                                B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR05 = *OFF                                B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the screen
      *          ------------------------------------------
     C                   IF        I#SC05 = *OFF                                B03
     C                   EXFMT     OMH00311
     C                   ELSE                                                   X03
     C                   EXFMT     OMH00312
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          F3=Exit
      *          ------------------------------------------
     C                   IF        *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Clear message queue
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Setof screen error indicators
      *          ------------------------------------------
     C                   EVAL      *IN(60) = *OFF
     C                   EVAL      *IN(61) = *OFF
     C                   EVAL      *IN(62) = *OFF
     C                   EVAL      *IN(63) = *OFF
     C                   EVAL      *IN(64) = *OFF
     C                   EVAL      *IN(65) = *OFF
     C                   EVAL      *IN(66) = *OFF
     C                   EVAL      *IN(67) = *OFF
     C                   EVAL      *IN(68) = *OFF
     C                   EVAL      *IN(69) = *OFF
     C                   EVAL      *IN(70) = *OFF
     C                   EVAL      *IN(71) = *OFF
     C                   EVAL      *IN(72) = *OFF
     C                   EVAL      *IN(73) = *OFF
     C                   EVAL      *IN(74) = *OFF
     C                   EVAL      *IN(75) = *OFF
     C                   EVAL      *IN(76) = *OFF
     C                   EVAL      *IN(77) = *OFF
     C                   EVAL      *IN(78) = *OFF
     C                   EVAL      *IN(79) = *OFF
     C                   EVAL      EROR05 = *OFF
     C                   EVAL      LOOP05 = *OFF
      *
      *          ------------------------------------------
      *          If F4 was pressed then retain the cursor locations
      *          else do normal cursor positioning.
      *          ------------------------------------------
     C                   IF        *INKD = *OFF
     C                   EVAL      FMROW# = *ZEROS
     C                   EVAL      FMCOL# = *ZEROS
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Roll up and down.
      *          ------------------------------------------
     C                   IF        *IN25 = *ON                                  B03
     C                             OR *IN26 = *ON                               B03
     C                   IF        *IN25 = *ON
     C                   EVAL      I#SC05 = *ON
     C                   ENDIF
     C                   IF        *IN26 = *ON
     C                   EVAL      I#SC05 = *OFF
     C                   ENDIF
     C                   EVAL      *IN25 = *OFF
     C                   EVAL      *IN26 = *OFF
     C                   EVAL      EROR05 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Prompt
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B04
     C                   EXSR      PROMPT
     C                   EVAL      EROR05 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Do edit routine if not F3 or F12.
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B05
     C                   EXSR      SRED05
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do processing if not F12
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B02
     C                   EXSR      SRPR05
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End process loop
      *          ------------------------------------------
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIN05 - Initiate adding an answer and actions              *
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 off = Change                                    *
      * 80 on  & 81 on  & 82 off = Delete                           *
      * 80 on  & 81 on  & 82 on  = Display                          *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN05        BEGSR
      *
      *          ------------------------------------------
      *          Determine the action
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B01
     C                   EVAL      *IN(80) = *OFF
     C                   EVAL      *IN(81) = *OFF
     C                   EVAL      *IN(82) = *OFF
     C                   ELSE                                                   X01
     C                   IF        W#OPTI = ' 2'                                B02
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *OFF
     C                   ELSE                                                   X02
     C                   IF        W#OPTI = ' 5'                                B03
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *ON
     C                   EVAL      *IN(82) = *ON
     C                   ELSE                                                   X03
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *ON
     C                   EVAL      *IN(82) = *OFF
      *
      *          ------------------------------------------
      *          Display deletion message
      *          ------------------------------------------
     C                   EVAL      MSGID = 'DLT0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Start with the first screen
      *          ------------------------------------------
     C                   EVAL      I#SC05 = *OFF
      *
      *          ------------------------------------------
      *          Do refresh subroutine
      *          ------------------------------------------
     C                   EXSR      SRRF05
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF05 - Refresh answer and actions.                        *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF05        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields if addition
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                   EVAL      FMANS# = *ZEROS
     C                   EVAL      FMGLS# = *ZEROS
     C                   EVAL      FMGQS# = *ZEROS
     C                   MOVE      *BLANKS       FMSEV1
     C                   MOVE      *BLANKS       FMSEV2
     C                   MOVE      *BLANKS       FMSEV3
     C                   EVAL      FMPRI# = *ZEROS
     C                   MOVE      *BLANKS       FMSTRC
     C                   MOVE      *ZEROS        FMRES#
     C                   MOVE      *BLANKS       FMITPC
     C                   MOVE      *BLANKS       FMUSF1
     C                   MOVE      *BLANKS       FMUSF2
     C                   EVAL      FMUSF3 = *ZEROS
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get record if change/delete/display
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B01
     C                   MOVE      FMANS#        K#ANS#
     C     $LA1K2        CHAIN(N)  OMQLAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   Z-ADD     LAGLS#        FMGLS#
     C                   Z-ADD     LAGQS#        FMGQS#
     C                   MOVE      LASEV1        FMSEV1
     C                   MOVE      LASEV2        FMSEV2
     C                   MOVE      LASEV3        FMSEV3
     C                   Z-ADD     LAPRI#        FMPRI#
     C                   MOVE      LASTRC        FMSTRC
     C                   Z-ADD     LARES#        FMRES#
     C                   MOVE      LAITPC        FMITPC
     C                   MOVE      LAUSF1        FMUSF1
     C                   MOVE      LAUSF2        FMUSF2
     C                   Z-ADD     LAUSF3        FMUSF3
      *
     C                   IF        FMANS# <> *ZEROS                             B02
     C     $NS1K1        CHAIN     OMANSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON
     C                   MOVEL     *ALL'*?'      FMANS2
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   MOVEL     NSANSC        FMANS2
     C                   ENDIF
     C                   ENDIF                                                  E02
      *
     C                   IF        FMGLS# <> *ZEROS                             B02
     C                   Z-ADD     FMGLS#        K#QLS#
     C     $QL1K2        CHAIN(N)  OMHQLL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON
     C                   MOVEL     *ALL'*?'      F2DES1
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   MOVEL     QLDESC        F2DES1
     C                   ENDIF
     C                   Z-ADD     FMQLS#        K#QLS#
     C                   ENDIF                                                  E02
      *
     C                   IF        FMGQS# <> *ZEROS                             B02
     C                   Z-ADD     FMGLS#        K#QLS#
     C                   Z-ADD     FMGQS#        K#QSQ#
     C     $LQ1K3        CHAIN(N)  OMQLQL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B03
     C                   MOVEL     *ALL'*?'      F2QUE2
     C                   ELSE                                                   X03
     C                   Z-ADD     LQQUE#        K#QUE#
     C     $QU1K1        CHAIN     OMHQUL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON
     C                   MOVEL     *ALL'*?'      F2QUE2
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   MOVEL     QUQUES        F2QUE2
     C                   ENDIF
     C                   ENDIF                                                  E03
     C                   Z-ADD     FMQLS#        K#QLS#
     C                   ENDIF                                                  E02
      *
     C                   IF        FMSEV1 <> *BLANKS                            B02
     C                   EVAL      K#TABC = '01'
     C                   MOVE      FMSEV1        K#SEVC
     C     $DS1K1        CHAIN     OMHDSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON
     C                   MOVEL     *ALL'*?'      FMSE1D
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   MOVEL     DSDESC        FMSE1D
     C                   ENDIF
     C                   ENDIF                                                  E02
      *
     C                   IF        FMSEV2 <> *BLANKS                            B02
     C                   EVAL      K#TABC = '02'
     C                   MOVE      FMSEV2        K#SEVC
     C     $DS1K1        CHAIN     OMHDSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON
     C                   MOVEL     *ALL'*?'      FMSE2D
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   MOVEL     DSDESC        FMSE2D
     C                   ENDIF
     C                   ENDIF                                                  E02
      *
     C                   IF        FMSEV3 <> *BLANKS                            B02
     C                   EVAL      K#TABC = '03'
     C                   MOVE      FMSEV3        K#SEVC
     C     $DS1K1        CHAIN     OMHDSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON
     C                   MOVEL     *ALL'*?'      FMSE3D
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   MOVEL     DSDESC        FMSE3D
     C                   ENDIF
     C                   ENDIF                                                  E02
      *
     C                   IF        FMSTRC <> *BLANKS                            B02
     C                   MOVE      FMSTRC        K#STRC
     C     $TR1K1        CHAIN     OMSTRL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF
     C                   MOVEL     TRSTRD        FMSTRD
     C                   ENDIF
     C                   IF        *IN99 = *ON
     C                   MOVEL     *ALL'*?'      FMSTRD
     C                   ENDIF
     C                   ENDIF                                                  E02
      *
     C                   IF        FMRES# <> *ZEROS                             B02
     C                   Z-ADD     FMRES#        K#RES#
     C                   MOVE      *BLANKS       RESTAT
     C     $RE1K1        CHAIN     OMSREL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        RESTAT <> *BLANKS                            B03
     C                   MOVEL     RESTAT        K#STAT
     C     $TA1K1        CHAIN     OMSTAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF
     C                   MOVEL     TASTAD        FMSTAD
     C                   ENDIF
     C                   IF        *IN99 = *ON
     C                   MOVE      *BLANKS       FMSTAD
     C                   ENDIF
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
     C                   IF        FMITPC <> *BLANK                             B02
     C                   MOVE      FMITPC        K#ITPC
     C     $IT1K1        CHAIN     OMHITL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF
     C                   MOVE      ITITPD        FMITPD
     C                   ENDIF
     C                   IF        *IN99 = *ON
     C                   MOVEL     *ALL'*?'      FMITPD
     C                   ENDIF
     C                   ENDIF                                                  E02
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the descriptions with F4=List
      *          ------------------------------------------
     C                   IF        FMANS# = *ZEROS                              B01
     C                   MOVEL(P)  VAR(5)        FMANS2
     C                   ENDIF                                                  E01
      *
     C                   IF        FMGLS# = *ZEROS                              B01
     C                   MOVEL(P)  VAR(5)        F2DES1
     C                   ENDIF                                                  E01
      *
     C                   IF        FMGQS# = *ZEROS                              B01
     C                   MOVEL(P)  VAR(5)        F2QUE2
     C                   ENDIF                                                  E01
      *
     C                   IF        FMSEV1 = *BLANKS                             B01
     C                   MOVEL(P)  VAR(5)        FMSE1D
     C                   ENDIF                                                  E01
      *
     C                   IF        FMSEV2 = *BLANKS                             B01
     C                   MOVEL(P)  VAR(5)        FMSE2D
     C                   ENDIF                                                  E01
      *
     C                   IF        FMSEV3 = *BLANKS                             B01
     C                   MOVEL(P)  VAR(5)        FMSE3D
     C                   ENDIF                                                  E01
      *
     C                   IF        FMSTRC = *BLANKS                             B01
     C                   MOVEL(P)  VAR(5)        FMSTRD
     C                   ENDIF                                                  E01
      *
     C                   IF        FMRES# = *ZEROS                              B01
     C                   MOVEL(P)  VAR(5)        FMSTAD
     C                   ENDIF                                                  E01
      *
     C                   IF        FMITPC = *BLANKS                             B01
     C                   MOVEL(P)  VAR(5)        FMITPD
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED05 - Screen 5 edit                                      *
      *                                                             *
      * This routine checks if the data is ok.                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED05        BEGSR
      *
      *          ------------------------------------------
      *          If add then the answer may not exit.
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C                   MOVE      FMQLS#        K#QLS#
     C                   MOVE      FMQSQ#        K#QSQ#
     C                   MOVE      FMANS#        K#ANS#
     C     $LA1K2        SETLL     OMQLAL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      MSGID = 'FILE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR05 = *ON
     C                   EVAL      *IN70 = *ON
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The answer must exist.
      *          ------------------------------------------
     C     $NS1K1        CHAIN     OMANSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B01
     C                   MOVEA     VAR(1)        MC(1)
     C                   Z-ADD     FMANS#        D#BIN4
     C                   MOVEA     D#BIA4        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      *IN70 = *ON
      *
      *                    --------------------------------
      *                    Send the message depending on the
      *                    visability of the field.
      *                    --------------------------------
     C                   IF        I#SC05 = *ON                                 B02
     C                   EVAL      MSGID = 'MSG0005'
     C                   IF        EROR05 = *OFF                                B03
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E03
     C                   ELSE                                                   X02
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E02
     C                   EVAL      EROR05 = *ON
      *
      *                    --------------------------------
      *                    The answer exists.
      *                    --------------------------------
     C                   ELSE                                                   X01
     C                   MOVEL     NSANSC        FMANS2
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The target list must exist if it is used
      *          ------------------------------------------
     C                   IF        FMGLS# > *ZEROS                              B01
     C                   Z-ADD     FMGLS#        K#QLS#
     C     $QL1K2        CHAIN(N)  OMHQLL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   Z-ADD     FMQLS#        K#QLS#                          restore key
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEA     VAR(2)        MC(1)
     C                   Z-ADD     FMANS#        D#BIN4
     C                   MOVEA     D#BIA4        MC(33)
     C                   EVAL      *IN71 = *ON
      *
      *                    --------------------------------
      *                    Send the message depending on the
      *                    visability of the field.
      *                    --------------------------------
     C                   IF        I#SC05 = *ON                                 B03
     C                   EVAL      MSGID = 'MSG0005'
     C                   IF        EROR05 = *OFF                                B04
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E04
     C                   ELSE                                                   X03
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E03
     C                   EVAL      EROR05 = *ON
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The question must exist in the question list
      *          a zero question is allowed. This indicates the
      *          first question in the list.
      *          ------------------------------------------
     C                   IF        FMGQS# <> *ZEROS                             B01
     C                   Z-ADD     FMGLS#        K#QLS#
     C                   Z-ADD     FMGQS#        K#QSQ#
     C     $LQ1K3        SETLL     OMQLQL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEA     VAR(3)        MC(1)
     C                   MOVEA     VAR(2)        MC(33)
     C                   MOVEA     MC            MSGDTA
      *
      *                    --------------------------------
      *                    Send the message depending on the
      *                    visability of the field.
      *                    --------------------------------
     C                   IF        I#SC05 = *ON                                 B03
     C                   EVAL      MSGID = 'MSG0005'
     C                   IF        EROR05 = *OFF                                B04
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E04
     C                   ELSE                                                   X03
     C                   EVAL      MSGID = 'FILE003'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E03
     C                   EVAL      EROR05 = *ON
     C                   EVAL      *IN72 = *ON
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Restore the key
      *                    --------------------------------
     C                   Z-ADD     FMQLS#        K#QLS#
     C                   Z-ADD     FMQSQ#        K#QSQ#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check the severity codes.
      *          ------------------------------------------
     C                   IF        FMSEV1 <> *BLANKS                            B01
     C                   EVAL      K#TABC = '01'
     C                   MOVE      FMSEV1        K#SEVC
     C     $DS1K1        SETLL     OMHDSL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEA     VAR(4)        MC(1)
     C                   MOVEA     FMSEV1        MC(33)
     C                   MOVEA     MC            MSGDTA
      *
      *                    --------------------------------
      *                    Send the message depending on the
      *                    visability of the field.
      *                    --------------------------------
     C                   IF        I#SC05 = *ON                                 B03
     C                   EVAL      MSGID = 'MSG0005'
     C                   IF        EROR05 = *OFF                                B04
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E04
     C                   ELSE                                                   X03
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E03
     C                   EVAL      EROR05 = *ON
     C                   EVAL      *IN73 = *ON
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
     C                   IF        FMSEV2 <> *BLANKS                            B01
     C                   EVAL      K#TABC = '02'
     C                   MOVE      FMSEV2        K#SEVC
     C     $DS1K1        SETLL     OMHDSL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEA     VAR(4)        MC(1)
     C                   MOVEA     FMSEV2        MC(33)
      *
      *                    --------------------------------
      *                    Send the message depending on the
      *                    visability of the field.
      *                    --------------------------------
     C                   IF        I#SC05 = *ON                                 B03
     C                   EVAL      MSGID = 'MSG0005'
     C                   IF        EROR05 = *OFF                                B04
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E04
     C                   ELSE                                                   X03
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E03
     C                   EVAL      EROR05 = *ON
     C                   EVAL      *IN74 = *ON
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
     C                   IF        FMSEV3 <> *BLANKS                            B01
     C                   EVAL      K#TABC = '03'
     C                   MOVE      FMSEV3        K#SEVC
     C     $DS1K1        SETLL     OMHDSL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEA     VAR(4)        MC(1)
     C                   MOVEA     FMSEV3        MC(33)
      *
      *                    --------------------------------
      *                    Send the message depending on the
      *                    visability of the field.
      *                    --------------------------------
     C                   IF        I#SC05 = *ON                                 B03
     C                   EVAL      MSGID = 'MSG0005'
     C                   IF        EROR05 = *OFF                                B04
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E04
     C                   ELSE                                                   X03
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E03
     C                   EVAL      EROR05 = *ON
     C                   EVAL      *IN75 = *ON
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The status route must exist.
      *          ------------------------------------------
     C                   IF        FMSTRC <> *BLANKS                            B01
     C                   MOVE      FMSTRC        K#STRC
     C     $TR1K1        CHAIN     OMSTRL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEA     VAR(7)        MC(1)
     C                   MOVEA     FMSTRC        MC(33)
     C                   MOVEA     MC            MSGDTA
      *
      *                    --------------------------------
      *                    Send the message depending on the
      *                    visability of the field.
      *                    --------------------------------
     C                   IF        I#SC05 <> *ON                                B03
     C                   EVAL      MSGID = 'MSG0005'
     C                   IF        EROR05 = *OFF                                B04
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E04
     C                   ELSE                                                   X03
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E03
     C                   EVAL      EROR05 = *ON
     C                   EVAL      *IN77 = *ON
      *
     C                   ELSE                                                   X02
     C                   MOVEL     TRSTRD        FMSTRD
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The status must exist in the route and
      *          a blank status is allowed. This indicates the
      *          first status in the route.
      *          ------------------------------------------
     C                   IF        FMRES# <> *ZEROS                             B01
     C                   Z-ADD     FMRES#        K#RES#
     C     $RE1K1        CHAIN     OMSREL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEA     VAR(8)        MC(1)
     C                   MOVEA     VAR(7)        MC(33)
     C                   MOVEA     MC            MSGDTA
      *
      *                    --------------------------------
      *                    Send the message depending on the
      *                    visability of the field.
      *                    --------------------------------
     C                   IF        I#SC05 <> *ON                                B03
     C                   EVAL      MSGID = 'MSG0005'
     C                   IF        EROR05 = *OFF                                B04
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E04
     C                   ELSE                                                   X03
     C                   EVAL      MSGID = 'FILE003'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E03
     C                   EVAL      EROR05 = *ON
     C                   EVAL      *IN72 = *ON
      *
     C                   ELSE                                                   X02
     C                   MOVEL     RESTAT        K#STAT
     C     $TA1K1        CHAIN     OMSTAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL     TASTAD        FMSTAD
     C                   IF        *IN99 = *ON
     C                   MOVE      *BLANKS       FMSTAD
     C                   ENDIF
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The incident type must exist.
      *          ------------------------------------------
     C                   IF        FMITPC <> *BLANK                             B01
     C                   MOVE      FMITPC        K#ITPC
     C     $IT1K1        CHAIN     OMHITL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEA     VAR(9)        MC(1)
     C                   MOVEA     FMITPC        MC(33)
     C                   MOVEA     MC            MSGDTA
      *
      *                    --------------------------------
      *                    Send the message depending on the
      *                    visability of the field.
      *                    --------------------------------
     C                   IF        I#SC05 <> *ON                                B03
     C                   EVAL      MSGID = 'MSG0005'
     C                   IF        EROR05 = *OFF                                B04
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E04
     C                   ELSE                                                   X03
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E03
     C                   EVAL      EROR05 = *ON
     C                   EVAL      *IN79 = *ON
      *
     C                   ELSE                                                   X02
     C                   MOVEL     ITITPD        FMITPD
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
      * SRPR05 - Process screen 5                                   *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR05        BEGSR
      *
      *          ------------------------------------------
      *          Write if addition
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C                   MOVE      FMHEDC        LAHEDC
     C                   Z-ADD     FMQLS#        LAQLS#
     C                   Z-ADD     FMQSQ#        LAQSQ#
     C                   Z-ADD     FMQUE#        LAQUE#
     C                   Z-ADD     FMANS#        LAANS#
     C                   Z-ADD     FMGLS#        LAGLS#
     C                   Z-ADD     FMGQS#        LAGQS#
     C                   MOVE      FMSEV1        LASEV1
     C                   MOVE      FMSEV2        LASEV2
     C                   MOVE      FMSEV3        LASEV3
     C                   MOVE      FMSTRC        LASTRC
     C                   Z-ADD     FMRES#        LARES#
     C                   Z-ADD     FMPRI#        LAPRI#
     C                   MOVE      FMITPC        LAITPC
     C                   MOVE      FMUSF1        LAUSF1
     C                   MOVE      FMUSF2        LAUSF2
     C                   MOVE      FMUSF3        LAUSF3
     C                   WRITE     OMQLAR
     C                   EVAL      LOOP05 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Update if updation
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B01
     C                             AND *IN81 = *OFF
     C                   Z-ADD     FMQLS#        K#QLS#
     C                   Z-ADD     FMQSQ#        K#QSQ#
     C                   Z-ADD     FMQUE#        K#QUE#
     C                   Z-ADD     FMANS#        K#ANS#
     C     $LA1K2        CHAIN     OMQLAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      FMHEDC        LAHEDC
     C                   Z-ADD     FMQLS#        LAQLS#
     C                   Z-ADD     FMQSQ#        LAQSQ#
     C                   Z-ADD     FMQUE#        LAQUE#
     C                   Z-ADD     FMANS#        LAANS#
     C                   Z-ADD     FMGLS#        LAGLS#
     C                   Z-ADD     FMGQS#        LAGQS#
     C                   MOVE      FMSEV1        LASEV1
     C                   MOVE      FMSEV2        LASEV2
     C                   MOVE      FMSEV3        LASEV3
     C                   MOVE      FMSTRC        LASTRC
     C                   Z-ADD     FMRES#        LARES#
     C                   Z-ADD     FMPRI#        LAPRI#
     C                   MOVE      FMITPC        LAITPC
     C                   MOVE      FMUSF1        LAUSF1
     C                   MOVE      FMUSF2        LAUSF2
     C                   MOVE      FMUSF3        LAUSF3
     C                   UPDATE    OMQLAR
     C                   MOVE      *IN40         LOOP05
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Delete if deletion.
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B01
     C                             AND *IN81 = *ON
     C                             AND *IN82 = *OFF                             B02
     C                   Z-ADD     FMQLS#        K#QLS#
     C                   Z-ADD     FMQSQ#        K#QSQ#
     C                   Z-ADD     FMANS#        K#ANS#
     C     $LA1K2        DELETE    OMQLAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If in ADD mode then force CHANGE mode.
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C                   EVAL      *IN80 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRTEST - Test question list                                 *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRTEST        BEGSR
      *
      *          ------------------------------------------
      *          Fill parameters
      *          ------------------------------------------
     C                   Z-ADD     FMQLS#        P#QLS#
      *
      *          ------------------------------------------
      *          Call program to test question list
      *          ------------------------------------------
     C                   EVAL      QUE = *ZEROS
     C                   EVAL      ANS = *ZEROS
     C                   EVAL      QLS = *ZEROS
     C                   EVAL      QSQ = *ZEROS
      *
     C                   CALL      'OMH0071'     $H0071                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#0071 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#0071 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
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
     C                   IF        FMHEDC = *BLANKS                             B01
     C                   MOVE      S#HEDC        FMHEDC
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Change the helpdesk
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '9'
     C                   MOVE      FMHEDC        P#HEDC
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD )
      *
      *          ------------------------------------------
      *          If errors, reset
      *          ------------------------------------------
     C                   IF        P#STAT <> C#NORM                             B02
     C                   EVAL      *IN71 = *ON
     C                   EVAL      *IN72 = *ON
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Fill screen fields from parameters
      *          ------------------------------------------
     C                   MOVE      P#HEDC        FMHEDC
     C                   MOVEL     D8HEDD        FMHEDD
     C                   EXSR      SRIN01
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
      *          Indicator definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Key field definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Save field definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      W#CR01 = *ZEROS
      *
      *          ------------------------------------------
      *          Indicator definitions
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          API parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter list for OMH007
      *          ------------------------------------------
     C     $H0071        PLIST
     C                   PARM                    P#HEDC
     C                   PARM      '*TEST'       P#CTN#
     C                   PARM      *BLANKS       P#INC#
     C                   PARM                    P#QLS#
     C                   PARM      C#TEST        P#ACTN
     C                   PARM                    P#QUE
     C                   PARM                    P#ANS
     C                   PARM                    P#QLS
     C                   PARM                    P#QSQ
     C                   PARM      C#NORM        P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMX305 (Display Q & A string)
      *          ------------------------------------------
     C     $MX305        PLIST
     C                   PARM                    P#QOAI
     C                   PARM                    P#QUES
     C                   PARM                    P#ANSC
     C                   PARM      C#NORM        P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (Send message)
      *          ------------------------------------------
     C     $X901C        PLIST
     C                   PARM                    MSGID
     C                   PARM      'OMHMSG'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGDTA
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
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGTXT
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
      *          OMHQL Question list file on question list
      *          ------------------------------------------
     C     $QL1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMQLS#
      *
     C     $QL1K2        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    K#QLS#
      *
      *          ------------------------------------------
      *          OMQLQ Question list question file on question list
      *          ------------------------------------------
     C     $LQ1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    K#QLS#
      *
     C     $LQ1K2        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    K#QLS#
     C                   KFLD                    FMQSQ#
      *
     C     $LQ1K3        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    K#QLS#
     C                   KFLD                    K#QSQ#
      *
      *          ------------------------------------------
      *          OMHQU Question file on question
      *          ------------------------------------------
     C     $QU1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    K#QUE#
      *
      *          ------------------------------------------
      *          OMQLA Question list question file on question list
      *          ------------------------------------------
     C     $LA1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    K#QLS#
     C                   KFLD                    K#QSQ#
     C                   KFLD                    K#QUE#
      *
     C     $LA1K2        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    K#QLS#
     C                   KFLD                    K#QSQ#
     C                   KFLD                    FMQUE#
     C                   KFLD                    K#ANS#
      *
      *          ------------------------------------------
      *          OMANS Answers
      *          ------------------------------------------
     C     $NS1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMQUE#
     C                   KFLD                    FMANS#
      *
      *          ------------------------------------------
      *          OMHDS Severities
      *          ------------------------------------------
     C     $DS1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    K#TABC
     C                   KFLD                    K#SEVC
      *
      *          ------------------------------------------
      *          OMSTR Status route file
      *          ------------------------------------------
     C     $TR1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    K#STRC
      *
      *          ------------------------------------------
      *          OMSRE Status route entries
      *          ------------------------------------------
     C     $RE1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    K#STRC
     C                   KFLD                    K#RES#
      *
      *          ------------------------------------------
      *          OMSTA Status file
      *          ------------------------------------------
     C     $TA1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    K#STAT
      *
      *          ------------------------------------------
      *          OMHIT Status file
      *          ------------------------------------------
     C     $IT1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    K#ITPC
      *
      *          ------------------------------------------
      *          Retrieve authorization
      *          ------------------------------------------
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '9'
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD )
      *
      *          ------------------------------------------
      *          Continue if authorization ok
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B01
      *
      *          ------------------------------------------
      *          Fill screen fields from parameters
      *          ------------------------------------------
     C                   MOVE      P#HEDC        FMHEDC
     C                   MOVEL     D8HEDD        FMHEDD
      *
      *          ------------------------------------------
      *          Get message data
      *          ------------------------------------------
     C                   EXSR      SRGVAR
     C                   ENDIF                                                  E01
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
      * calling program and the SRTTLR routine is invoked.          *
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
     C                   SELECT                                                 B02
     C                   WHEN      E#FILE = 'OMH003D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMHQLL1 '                          X02
     C                   MOVEL     HQLL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMQLQL1 '                          X02
     C                   MOVEL     QLQL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHQUL1 '                          X02
     C                   MOVEL     HQUL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMANSL1 '                          X02
     C                   MOVEL     ANSL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMQLAL1 '                          X02
     C                   MOVEL     QLAL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHDSL1 '                          X02
     C                   MOVEL     HDSL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMSTRL1 '                          X02
     C                   MOVEL     STRL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMSREL1 '                          X02
     C                   MOVEL     SREL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMSTAL1 '                          X02
     C                   MOVEL     STAL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHITL1 '                          X02
     C                   MOVEL     HITL1         P#FILE
      *
     C                   ENDSL                                                  E02
      *
      *          ------------------------------------------
      *          Call the error handler
      *          ------------------------------------------
     C                   CALLP     OMH961 ( PGM : P#FILE )
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
      *          ------------------------------------------
      *          Else exit immediately.
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Exit.
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#TERM
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
      *          Close called programs if necessary
      *          ------------------------------------------
     C                   IF        I#0071 = *ON                                 B01
     C                   CALLP     OMH007()
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Seton last record
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
      * SRGVAR  -  Get variable message text                         *
      *                                                              *
      * This subroutine gets the data needed for this program to     *
      * retrieve the data wich is filled in message texts.           *
      * It is called one time per program execution.                 *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGVAR        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init.
      *          ------------------------------------------
     C                   EVAL      %SUBST(MSGF:1:6) = 'OMHMSG'
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 20                                 B01
     C                   EVAL      MSGID = MSD(MS)
      *
      *          ------------------------------------------
      *          Call subprogram
      *          ------------------------------------------
     C                   IF        MSGID <> *BLANKS                             B02
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill message array
      *          ------------------------------------------
     C                   EVAL      %SUBST(VAR(MS):1:132) = MSGTXT
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          Fill message gotten indicator
      *          ------------------------------------------
     C                   EVAL      MSGGET = *ON
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
**
ANS0001 - answer number              1
LST0003 - question list              2
QUE0001 - question                   3
SEV0001 - severity code              4
PMT0005 - F4=List                    5
PRI0003 - Priority                   6
STR0001 - status route               7
STA0001 - status                     8
INC0001 - incident type              9
