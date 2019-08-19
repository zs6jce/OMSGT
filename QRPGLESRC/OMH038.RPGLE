     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH038.                                       *
      * Function ...: Work with Distributions                       *
      * Author .....: Remain Software                               *
      * Module .....: HelpDesk                                      *
      * Parameters .: *NONE                                         *
      *                                                             *
      * This program allows helpdesk managers + the secofr to       *
      * receive the HelpDesk distributions.                         *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMHMTL1   UF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HMTL1)
     FOMHMOL1   UF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HMOL1)
     FOMQLAL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(QLAL1)
     FOMHMQL1   UF A E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HMQL1)
     FOMHMHL1   UF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HMHL1)
     FOMHMHL2   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HMHL2)
     F                                     RENAME(OMHMHR:HMHR)
     FOMH038D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
     F                                     SFILE(OMH03801:RRN01)
     F                                     SFILE(OMH03805:RRN03)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension spec's                                             *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * RAW   Woooooh                                                *
      * QUE   Used to store question numbers                         *
      * ANS   Used to store answers                                  *
      * QLS   Used to store question lists                           *
      * QSQ   Used to store question seq in question list            *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(12)
     D MSD             S              7    DIM(12) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
     D RAW             S              1    DIM(500)
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
     D HMHL1           DS            57
     D HMQL1           DS            57
     D QLAL1           DS            57
     D HMTL1           DS            57
     D HMOL1           DS            57
     D HMHL2           DS            57
      *
      *          ------------------------------------------
      *          Question & answer arrays.
      *          ------------------------------------------
     D P#QUE           DS
     D  QUE                    1   5000  0 INZ(0)
     D                                     DIM(1000)
     D P#ANS           DS
     D  ANS                    1   3000  0 INZ(0)
     D                                     DIM(1000)
     D P#QLS           DS
     D  QLS                    1   5000  0 INZ(0)
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
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  E#FILE               201    208
     D  PGMQ             *PROC
      *
      *          ------------------------------------------
      *          Number for message formatting
      *          ------------------------------------------
     D                 DS
     D D#DEC#                         5  0
     D  D#DECA                        5    OVERLAY(D#DEC#)
      *
      *          ------------------------------------------
      *          Compile the distribution id
      *          ------------------------------------------
     D                 DS
     D D#DSID                        20
     D  FMADDR                        8    OVERLAY(D#DSID)
     D  FMUSRI                        8    OVERLAY(D#DSID:9)
     D  FMDSQ#                        4    OVERLAY(D#DSID:17)
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
      *          Named constants
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#EXIT          C                   CONST('*EXIT')
     D C#CNCL          C                   CONST('*CNCL')
     D C#PROC          C                   CONST('*PROC')
     D C#QUES          C                   CONST('*QUES')
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
      *          Parameter list for OMH902C (Clear message queue)
      * Prototype for 'OMX105'
     D OMX105          PR                  EXTPGM('OMX105')
     D FMHEDC_                             LIKE(FMHEDC)
     D HEDD_                               LIKE(HEDD)
      * Prototype for 'OMX128'
     D OMX128          PR                  EXTPGM('OMX128')
     D FMCUSC_                             LIKE(FMCUSC)
     D NAMD_                               LIKE(NAMD)
      * Prototype for 'OMX129'
     D OMX129          PR                  EXTPGM('OMX129')
     D FMCUSC_                             LIKE(FMCUSC)
     D FMCCTC_                             LIKE(FMCCTC)
     D NAMD_                               LIKE(NAMD)
      * Prototype for 'OMX1192'
     D OMX1192         PR                  EXTPGM('OMX1192')
     D FMHEDC_                             LIKE(FMHEDC)
     D FMRUID_                             LIKE(FMRUID)
     D USDC_                               LIKE(USDC)
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
      * Prototype for 'OMH037_2'
     D OMH037_2        PR                  EXTPGM('OMH037_2')
     D FMDSID_                             LIKE(FMDSID)
     D INDI_                               LIKE(INDI)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Key list for OMQLAL1 (Question list answers)
      * Prototype for 'OMH0071'
     D OMH0071         PR                  EXTPGM('OMH0071')
     D MHHEDC_                             LIKE(MHHEDC)
     D CTN#_                               LIKE(CTN#)
     D INC#_                               LIKE(INC#)
     D P#QLS#_                             LIKE(P#QLS#)
     D P#ACTN_                             LIKE(P#ACTN)
     D P#QUE_                              LIKEDS(P#QUE)
     D P#ANS_                              LIKEDS(P#ANS)
     D P#QLS_                              LIKEDS(P#QLS)
     D P#QSQ_                              LIKEDS(P#QSQ)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH029 (Get info)
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
      *          Retrieve authorization
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
      * Prototype for 'OMH904C'
     D OMH904C         PR                  EXTPGM('OMH904C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGTXT_                             LIKE(MSGTXT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH0071 (Asking Questions)
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D p#qls#          S              5P 0
     D EROR01          S                   LIKE(INDI)
     D EROR02          S                   LIKE(INDI)
     D EROR03          S                   LIKE(INDI)
     D I#PSSR          S              1
     D IQ              S              5  0
     D IX              S              5  0
     D LOOP01          S                   LIKE(INDI)
     D LOOP02          S                   LIKE(INDI)
     D LOOP03          S                   LIKE(INDI)
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#ACTN          S                   LIKE(ACTN)
     D P#DSID          S                   LIKE(DSID)
     D P#FILE          S             57
     D P#HEDC          S                   LIKE(HEDC)
     D P#MSTA          S             40
     D P#OPTI          S                   LIKE(INDI)
     D P#STAT          S                   LIKE(STAT)
     D P#SUL#          S                   LIKE(SUL#)
     D P2STAT          S                   LIKE(STAT)
     D RRN01           S              5  0
     D RRN03           S              5  0
     D S#HEDC          S                   LIKE(HEDC)
     D SP              S              5  0
     D W#CRC#          S              5  0
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
     C                   CALLP     OMH901C  ( MSGID : MSGF : MSGLIB :
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
      *          Display 2.
      *          HelpDesk code
      *          ------------------------------------------
     C                   IF        FMFMTC = 'OMH03804'                          B01
     C                             AND FMFLDC = 'FMHEDC'                        B02
     C                   CALLP     OMX105 ( FMHEDC : HEDD )
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Customer
      *          ------------------------------------------
     C                   IF        FMFMTC = 'OMH03804'                          B02
     C                             AND FMFLDC = 'FMCUSC'                        B02
     C                   CALLP     OMX128 ( FMCUSC : NAMD )
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Contact
      *          ------------------------------------------
     C                   IF        FMFMTC = 'OMH03804'                          B03
     C                             AND FMFLDC = 'FMCCTC'                        B02
     C                   CALLP     OMX129 ( FMCUSC : FMCCTC : NAMD )
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Employee
      *          ------------------------------------------
     C                   IF        FMFMTC = 'OMH03804'                          B04
     C                             AND FMFLDC = 'FMRUID'                        B02
     C                   CALLP     OMX1192 ( FMHEDC : FMRUID : USDC )
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Status route
      *          ------------------------------------------
     C                   IF        FMFMTC = 'OMH03804'                          B05
     C                             AND FMFLDC = 'FMSTRC'                        B02
     C                   CALLP     OMX126 ( FMHEDC : FMSTRC : STRD )
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          Status route sequence
      *          ------------------------------------------
     C                   IF        FMFMTC = 'OMH03804'                          B06
     C                             AND FMFLDC = 'FMRES#'                        B02
     C                   CALLP     OMX127 ( FMHEDC : FMSTRC : FMRES# : STAT
     C                             : STAD )
     C                   ELSE                                                   X06
      *
      *          ------------------------------------------
      *          Invalid prompt location.
      *          ------------------------------------------
     C                   EVAL      *IN71 = *ON
     C                   EVAL      MSGID = 'PMT0002'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
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
      * SRSC01 - Work with distributions                            *
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
     C                   DOU       LOOP01 = '0'                                 B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR01 = '0'                                 B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH03803
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
     C                   EXFMT     OMH03802
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
     C                   EVAL      W#CRC# = D#CRC#
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
     C                   EVAL      EROR01 = '1'
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Edit
      *          ------------------------------------------
     C                   EXSR      SRED01
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
     C                   EXSR      SRPR01
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
      * SRRF01 - Work with questions refresh                        *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      RRN01 = *ZEROS
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH03802
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Read first distribution record
      *          ------------------------------------------
     C     *BLANKS       CHAIN     OMHMHL2
     C                   EVAL      *IN99 = NOT %FOUND
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
     C                   MOVEL     MHDSID        FMDSID
     C                   MOVEL     MHSUBJ        F2SUBJ
     C                   MOVE      MHUSRI        FMUSRI
     C                   MOVE      MHADDR        FMADDR
     C                   MOVE      MHDSID        FMDSQ#
     C                   MOVE      MHHEDC        FMHEDC
     C                   MOVE      MHINC#        FMINC#
     C                   MOVE      MHCUSC        FMCUSC
     C                   MOVE      MHCCTC        FMCCTC
     C                   MOVEL     MHSTAT        FMSTAT
     C                   ADD       1             RRN01
     C                   WRITE     OMH03801
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C                   READ      OMHMHL2
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
      * SRED01 - Work with Distributions edit                       *
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
     C                   EVAL      W#MSGI = '0'
     C                   EVAL      W#SAV# = RRN01
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   READC(E)  OMH03801
     C                   EVAL      *IN99 = %ERROR OR %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Do if option is not blank
      *          ------------------------------------------
     C                   IF        FMOPTI <> *BLANKS                            B02
      *
     C                   MOVE      FMOPTI        W#WRKI
     C                   IF        W#WRKI = *BLANKS                             B03
     C                   MOVEL     FMOPTI        W#WRKI
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      W#WRKI        FMOPTI
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If option is valid, then check the number of valid
      *          codes. *IN93 is the subfile next change indicator
      *          ------------------------------------------
     C                   IF        FMOPTI = ' 1'                                B03
     C                             OR FMOPTI = ' 2'
     C                             OR FMOPTI = ' 4'
     C                             OR FMOPTI = ' 5'
     C                             OR FMOPTI = ' 8'
     C                             OR FMOPTI = '12'
     C                             OR FMOPTI = '22'
      *
      *          ------------------------------------------
      *          Record must be ok for option 1.
      *          ------------------------------------------
     C                   IF        FMOPTI = ' 1'                                B04
     C     FMDSID        CHAIN     OMHMHL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        MHSTAT <> C#NORM                             B05
     C                   EVAL      INDI = '0'
     C                   CALLP     OMH037_2 ( FMDSID : INDI : P#STAT )
     C                   MOVE      P#STAT        MHSTAT
     C                   ENDIF                                                  E05
      *
     C                   IF        MHSTAT <> C#NORM                             B05
     C                   EVAL      MSGID = 'ERR0020'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Check if subfile record is still
      *          active. If not then error, else ok.
      *          ------------------------------------------
     C                   IF        EROR01 = *OFF                                B04
     C     FMDSID        SETLL     OMHMHL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B05
     C                   EVAL      MSGID = 'FILE002'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X05
      *
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
     C                   UPDATE    OMH03801
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   UPDATE    OMH03801
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH03801
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Fill the current relative record on the page to
      *          display.
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B01
     C                   Z-ADD     W#CRC#        CRRN01
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
      * SRPR01 - Work with Distributions process                    *
      *                                                             *
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
     C                   READC     OMH03801
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
     C                   EVAL      W#CRC# = RRN01
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPTI        W#OPTI
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    OMH03801
      *
      *          ------------------------------------------
      *          1=Set process flag
      *          ------------------------------------------
     C                   IF        W#OPTI = ' 1'                                B03
     C     FMDSID        CHAIN     OMHMHL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      C#PROC        MHSTAT
     C                   UPDATE    OMHMHR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          2=Change
      *          ------------------------------------------
     C                   IF        W#OPTI = ' 2'                                B03
     C                   EXSR      SRSC02
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          4=Delete
      *          ------------------------------------------
     C                   IF        W#OPTI = ' 4'                                B03
      *
     C     FMDSID        DELETE    OMHMQL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B04
     C     FMDSID        DELETE    OMHMQL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDDO                                                  E04
      *
     C     FMDSID        DELETE    OMHMTL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B04
     C     FMDSID        DELETE    OMHMTL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDDO                                                  E04
      *
     C     FMDSID        DELETE    OMHMOL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B04
     C     FMDSID        DELETE    OMHMOL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDDO                                                  E04
      *
     C     FMDSID        DELETE    OMHMHL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B04
     C     FMDSID        DELETE    OMHMHL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDDO                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          8=Text, 5=Raw text
      *          ------------------------------------------
     C                   IF        W#OPTI = ' 8'                                B03
     C                             OR W#OPTI = ' 5'                             B03
     C                   EXSR      SRSC03
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          12=Q&A
      *          ------------------------------------------
     C                   IF        W#OPTI = '12'                                B03
     C     FMDSID        CHAIN(N)  OMHMQL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   Z-ADD     MQQLS#        P#QLS#
     C                   EVAL      P#ACTN = C#QUES
      *
     C                   EVAL      IQ = 1
     C                   DOW       *IN99 = *OFF                                 B04
     C     $LA1K1        CHAIN     OMQLAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   Z-ADD     MQQLS#        QLS(IQ)
     C                   Z-ADD     MQANS#        ANS(IQ)
     C                   Z-ADD     MQQSQ#        QSQ(IQ)
     C                   Z-ADD     LAQUE#        QUE(IQ)
     C                   ADD       1             IQ
     C     FMDSID        READE     OMHMQL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E04
      *
     C                   EVAL      CTN# = '*NEW'
     C                   EVAL      INC# = *BLANKS
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH0071 ( MHHEDC : CTN# : INC# : P#QLS# :
     C                              P#ACTN : P#QUE : P#ANS : P#QLS : P#QSQ :
     C                              P#STAT )
      *
     C                   IF        P#STAT = C#TERM                              B04
     C                   EVAL      *INKC = *ON
     C                   ELSE                                                   X04
      *
     C                   IF        P#STAT = C#CNCL                              B05
     C                   EVAL      *INKL = *ON
     C                   ELSE                                                   X05
      *
      *                    --------------------------------
      *                    Write back the new Q&A.
      *                    --------------------------------
     C                   DOU       *IN99 = *ON                                  B06
     C     FMDSID        DELETE    OMHMQL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDDO                                                  E06
      *
     C                   EVAL      IQ = 1
     C                   MOVE      FMDSID        MQDSID
     C                   DOW       QLS(IQ) > *ZEROS                             B06
     C                   Z-ADD     QLS(IQ)       MQQLS#
     C                   Z-ADD     ANS(IQ)       MQANS#
     C                   Z-ADD     QSQ(IQ)       MQQSQ#
     C                   WRITE     OMHMQR
     C                   ADD       1             IQ
     C                   ENDDO                                                  E06
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          22=Check
      *          ------------------------------------------
     C                   IF        W#OPTI = '22'                                B03
     C                   EVAL      INDI = '1'
     C                   CALLP     OMH037_2 ( FMDSID : INDI : P#STAT )
      *
     C     FMDSID        CHAIN     OMHMHL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      P#STAT        MHSTAT
     C                   UPDATE    OMHMHR
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
     C                   READC     OMH03801
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
     C                   Z-ADD     W#CRC#        CRRN01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC02 - Change distribution data                           *
      *                                                             *
      * Change parameters that where not received correctly.        *
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
     C                   DOU       LOOP02 = '0'                                 B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR02 = '0'                                 B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the screen
      *          ------------------------------------------
     C                   EXFMT     OMH03804
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
     C                   EVAL      EROR02 = '0'
     C                   EVAL      LOOP02 = '0'
      *
      *          ------------------------------------------
      *          F4=Prompt
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   EXSR      PROMPT
     C                   EVAL      EROR02 = *ON                                 E03
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Do edit routine if not F3 or F12.
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B04
     C                   EXSR      SRED02
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors
      *          ------------------------------------------
     C                   ENDDO                                                  E02
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
      * SRIN02 - Change distribution init                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN02        BEGSR
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
      * SRRF02 - Change distributions refresh                       *
      *                                                             *
      * This screen is called for future processing.                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF02        BEGSR
      *
      *          ------------------------------------------
      *          Get record else
      *          ------------------------------------------
     C     FMDSID        CHAIN(N)  OMHMHL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      MHDSID        FMDSID
     C                   MOVE      MHSTAT        FMSTAT
     C                   MOVEL     MHSUBJ        FMSUBJ
     C                   MOVE      MHHEDC        FMHEDC
     C                   MOVE      MHINC#        FMINC#
     C                   MOVE      MHSHID        FMSHID
     C                   MOVE      MHCUSC        FMCUSC
     C                   MOVE      MHCCTC        FMCCTC
     C                   MOVE      MHSTAT        FMSTAT
     C                   MOVE      MHRUID        FMRUID
     C                   MOVE      MHSTRC        FMSTRC
     C                   MOVE      MHRES#        FMRES#
     C                   MOVE      MHCSRC        FMCSRC
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED02 - Change distribution edit                           *
      *                                                             *
      * This routine checks if the data is ok.                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED02        BEGSR
      *
      *          ------------------------------------------
      *          Fill the file.
      *          ------------------------------------------
     C                   EXSR      SRPR02
      *
      *          ------------------------------------------
      *          Test without display
      *          ------------------------------------------
     C                   MOVE      FMDSID        FMDSID
     C                   EVAL      INDI = '0'
     C                   CALLP     OMH037_2 ( FMDSID : INDI : P#STAT )
      *
      *          ------------------------------------------
      *          If not normal then show it.
      *          ------------------------------------------
     C                   IF        P#STAT <> C#NORM                             B01
     C                   EVAL      INDI = '1'
     C                   CALLP     OMH037_2 ( FMDSID : INDI : P#STAT )
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the status
      *          ------------------------------------------
     C                   MOVE      P#STAT        FMSTAT
      *
      *          ------------------------------------------
      *          Fill the file.
      *          ------------------------------------------
     C                   EXSR      SRPR02
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR02 - Process change distribtution                       *
      *                                                             *
      * This routine updates the distribution header file           *
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
      *          Get the file
      *          ------------------------------------------
     C     FMDSID        CHAIN     OMHMHL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill the file
      *          ------------------------------------------
     C                   MOVE      FMDSID        MHDSID
     C                   MOVE      FMSTAT        MHSTAT
     C                   MOVEL     FMSUBJ        MHSUBJ
     C                   MOVE      FMSHID        MHSHID
     C                   MOVE      FMHEDC        MHHEDC
     C                   MOVE      FMINC#        MHINC#
     C                   MOVE      FMCUSC        MHCUSC
     C                   MOVE      FMCCTC        MHCCTC
     C                   MOVE      FMRUID        MHRUID
     C                   MOVE      FMCSRC        MHCSRC
     C                   MOVE      FMSTRC        MHSTRC
     C                   MOVE      FMRES#        MHRES#
     C                   MOVE      FMSTAT        MHSTAT
      *
      *          ------------------------------------------
      *          Update
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMHMHR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC03 - Work with distribution texts                       *
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
     C                   DOU       LOOP03 = '0'                                 B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR03 = '0'                                 B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH03807
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        RRN03 = *ZEROS                               B03
     C                   EVAL      *IN72 = *ON
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   EXFMT     OMH03806
      *
      *          ------------------------------------------
      *          CMD-12 is leave CMD-3 is exit
      *          ------------------------------------------
     C                   IF        *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
     C                   IF        *INKL = *ON
     C                   LEAVE
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
     C                   EVAL      EROR03 = *OFF
     C                   EVAL      LOOP03 = *OFF
      *
      *          ------------------------------------------
      *          F5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B03
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = '1'
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Edit
      *          ------------------------------------------
     C                   EXSR      SRED03
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
     C                   IF        *INKL = *OFF
     C                   EXSR      SRPR03
     C                   ENDIF
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
      * SRIN03 - Work with text init                                *
      *                                                             *
      * This routine blanks the input fields on the screen.         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN03        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields
      *          ------------------------------------------
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
      * SRRF03 - Work with text refresh                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF03        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      RRN03 = *ZEROS
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH03806
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          8=Text.
      *          ------------------------------------------
     C                   IF        W#OPTI = ' 8'                                B01
      *
      *          ------------------------------------------
      *          Read first text record
      *          ------------------------------------------
     C     FMDSID        CHAIN(N)  OMHMTL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   MOVE      MTINCD        FMINCD
     C                   ADD       1             RRN03
     C                   WRITE     OMH03805
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     FMDSID        READE(N)  OMHMTL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          5=Raw.
      *          ------------------------------------------
     C                   IF        W#OPTI = ' 5'                                B01
      *
      *          ------------------------------------------
      *          Read first raw text record
      *          ------------------------------------------
     C     FMDSID        CHAIN(N)  OMHMOL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Remove the control codes
      *          ------------------------------------------
     C                   MOVEA     MODDTA        RAW
     C                   FOR       IX = 1 TO 500                                B03
     C                   IF        RAW(IX) < X'40'                              B04
     C                   EVAL      RAW(IX) = X'40'
     C                   ENDIF                                                  E04
     C                   ENDFOR                                                 E03
     C                   MOVEA     RAW           MODDTA
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   EVAL      SP = 1
     C                   FOR       IX = 1 TO 7                                  B03
     C                   EVAL      FMINCD = %SUBST(MODDTA:SP:70)
     C                   ADD       1             RRN03
     C                   WRITE     OMH03805
     C                   ADD       70            SP
     C                   ENDFOR                                                 E03
     C                   EVAL      FMINCD = %SUBST(MODDTA:490:10)
     C                   ADD       1             RRN03
     C                   WRITE     OMH03805
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     FMDSID        READE(N)  OMHMOL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
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
      * SRED03 - Work with Distribution text edit                   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED03        BEGSR
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR03 - Work with Distribution texts process               *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR03        BEGSR
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
     C                   EVAL      IQ = *ZEROS
     C                   EVAL      IX = *ZEROS
     C                   EVAL      SP = *ZEROS
      *
      *          ------------------------------------------
      *          API parameter definition
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
      *          Parameter list for OMH0071 (Asking Questions)
      *          ------------------------------------------
     C     $H0071        PLIST
     C                   PARM                    MHHEDC
     C                   PARM      '*NEW'        CTN#
     C                   PARM      *BLANKS       INC#
     C                   PARM                    P#QLS#
     C                   PARM                    P#ACTN
     C                   PARM                    P#QUE
     C                   PARM                    P#ANS
     C                   PARM                    P#QLS
     C                   PARM                    P#QSQ
     C                   PARM      C#NORM        P#STAT
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
      *          Retrieve authorization
      *          ------------------------------------------
     C     $MH372        PLIST
     C                   PARM                    FMDSID
     C                   PARM                    INDI
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Key list for OMQLAL1 (Question list answers)
      *          ------------------------------------------
     C     $LA1K1        KLIST
     C                   KFLD                    MHHEDC
     C                   KFLD                    MQQLS#
     C                   KFLD                    MQQSQ#
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
      *          Continue if authorisation ok
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B01
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
     C                   WHEN      E#FILE = 'OMH038D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMHMHL1 '                          X02
     C                   MOVEL     HMHL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHMOL1 '                          X02
     C                   MOVEL     HMOL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHMQL1 '                          X02
     C                   MOVEL     HMQL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMQLAL1 '                          X02
     C                   MOVEL     QLAL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHMHL2 '                          X02
     C                   MOVEL     HMHL2         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHMTL1 '                          X02
     C                   MOVEL     HMTL1         P#FILE
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
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Exit.
      *          ------------------------------------------
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
     C                   FOR       MS = 1 TO 1                                  B01
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
     C                   EVAL      MSGGET = '1'
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
**
STA0001 - status                                1
