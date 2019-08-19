     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH020                                        *
      * Function ...: Employee status route access maintenance      *
      * Author .....: Remain Software                               *
      * Module .....: HelpDesk                                      *
      * Parameters .: P#HEDC    -  HelpDesk code                    *
      *               P#EMPC    -  Employee                         *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMSTRL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(STRL1)
     FOMESAL1   UF A E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(ESAL1)
     FOMH020D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
     F                                     SFILE(OMH02001:RRN01)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension spec's                                             *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(12)
     D MSD             S              7    DIM(12) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
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
     D STRL1           DS            57
     D ESAL1           DS            57
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
      *          Helpdesk data structures
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
      *          Number for message formatting
      *          ------------------------------------------
     D                 DS
     D D#DEC#                         5  0
     D  D#DECA                        5    OVERLAY(D#DEC#)
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#EXIT          C                   CONST('*EXIT')
     D C#CNCL          C                   CONST('*CNCL')
     D C#ALL           C                   CONST('*ALL ')
      *
     D C#NEW           C                   CONST('*NEW ')
     D C#CMP           C                   CONST('*CMP ')
     D C#LCK           C                   CONST('*LCK ')
     D C#LOCK          C                   CONST('*LOCK')
     D C#ANY           C                   CONST('*ANY ')
     D C#PLND          C                   CONST('*PLND')
     D C#OMS           C                   CONST('*OMS ')
      * Prototype for OMH020
     D OMH020          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#HEDD_                             LIKE(P#HEDD)
     D P#EMPC_                             LIKE(P#EMPC)
     D P#USDC_                             LIKE(P#USDC)
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
      *          Parameter list for OMH902C (Clear message queue)
      * Prototype for 'OMH014'
     D OMH014          PR                  EXTPGM('OMH014')
     D P#HEDC_                             LIKE(P#HEDC)
     D P#STRC_                             LIKE(P#STRC)
     D P#STRD_                             LIKE(P#STRD)
     D P#ACTC_                             LIKE(P#ACTC)
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
      *          OMESA Status route access file
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
      *          Parameter list for OMX126 (Select status route)
      * *ENTRY Interface for Main Procedure
     D OMH020          PI
     D P#HEDC                         5
     D P#HEDD                     99999C
     D P#EMPC                        10
     D P#USDC                     99999C
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D EROR01          S                   LIKE(INDI)
     D EROR02          S                   LIKE(INDI)
     D I#PSSR          S              1
     D K#EMPC          S                   LIKE(USID)
     D K#HEDC          S                   LIKE(HEDC)
     D K#STRC          S                   LIKE(STRC)
     D LOOP01          S                   LIKE(INDI)
     D LOOP02          S                   LIKE(INDI)
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#ACTC          S                   LIKE(ACTC)
     D P#FILE          S             57
     D P#MSTA          S             40
     D P#OPTI          S                   LIKE(INDI)
     D P#STRC          S                   LIKE(STRC)
     D P#STRD          S                   LIKE(STRD)
     D P#SUL#          S                   LIKE(SUL#)
     D P1HEDC          S                   LIKE(HEDC)
     D P1STRC          S                   LIKE(STRC)
     D P1STRD          S                   LIKE(STRD)
     D RRN01           S              5  0
     D S#HEDC          S                   LIKE(HEDC)
     D W#ALLI          S                   LIKE(INDI)
     D W#CRC#          S              5  0
     D W#F4I           S                   LIKE(INDI)
     D W#IX            S              5  0
     D W#MSGI          S              1
     D W#OPTI          S                   LIKE(FMOPTI)
     D W#SAV#          S              5  0
     D W#STRC          S                   LIKE(STRC)
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
     C                   CALLP     OMH901C  ( MSGID : MSGF : MSGLIB :
     C                             MSGDTA )
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
      * SRSC01 - Work with employees                                *
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
     C                   WRITE     OMH02003
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
     C                   IF        RRN01 = 0                                    B03
     C                   EXFMT     OMH02005
     C                   ELSE                                                   X03
     C                   EXFMT     OMH02002
     C                   ENDIF                                                  E03
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
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      *IN72 = *OFF
     C                   EVAL      EROR01 = *OFF
     C                   EVAL      LOOP01 = *OFF
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
      *          F6=Add status route
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
      * SRRF01 - Work with questions refresh                        *
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
     C                   WRITE     OMH02002
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Read first status access record
      *          ------------------------------------------
     C                   MOVE      *LOVAL        K#STRC
     C                   EVAL      K#HEDC = P#HEDC
     C                   EVAL      K#EMPC = P#EMPC
     C     $SA1K3        SETLL     OMESAL1
     C     $SA1K2        READE     OMESAL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If *ALL indicated: fill from status route file
      *          ------------------------------------------
     C                   IF        *IN99 = *ON
     C                   EVAL      *IN43 = *ON
     C                   MOVE      *LOVAL        FMSTRC
     C     $TR1K2        SETLL     OMSTRR
     C     $TR1K1        READE     OMSTRR
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 <> *ON
      *
      *          ------------------------------------------
      *          Fill subfile record, write, read next one from file
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVEL     TRSTRC        FMSTRC
     C                   MOVEL     TRSTRD        FMSTRD
     C                   ADD       1             RRN01
     C                   WRITE     OMH02001
     C     $TR1K1        READE     OMSTRR
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO
      *
      *          ------------------------------------------
      *          Else fill from employee status route access file
      *          ------------------------------------------
     C                   ELSE
     C                   EVAL      *IN43 = *OFF
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVEL     SASTRC        FMSTRC
     C                   MOVEL     SAHEDC        FMHEDC
      *
      *          ------------------------------------------
      *          Retrieve description from status route file
      *          ------------------------------------------
     C     $TR1K2        CHAIN     OMSTRR
     C                   EVAL      *IN98 = NOT %FOUND
     C                   IF        *IN98 = *OFF
     C                   MOVEL     TRSTRD        FMSTRD
     C                   ADD       1             RRN01
     C                   WRITE     OMH02001
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     $SA1K2        READE     OMESAL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
     C                   ENDIF
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
      * SRED01 - Work with Employee edit                            *
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
     C                   READC(E)  OMH02001
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
     C                   IF        FMOPTI = ' 4'                                B03
     C                             OR FMOPTI = '12'
      *
      *          ------------------------------------------
      *          Option is valid, check if subfile record is still
      *          active in security file if not then error, else ok.
      *          ------------------------------------------
     C     $TR1K2        SETLL     OMSTRL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B04
     C                   EVAL      MSGID = 'FILE002'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
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
     C                   UPDATE    OMH02001
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   UPDATE    OMH02001
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH02001
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          If subfile must be positioned, and no errors occured
      *          then position it.
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B01
     C                             AND FMPOS <> *BLANKS
     C                   FOR       W#IX = 1 TO W#SAV#                           B02
     C     W#IX          CHAIN     OMH02001
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        FMSTRC >= FMPOS                              B03
     C                   Z-ADD     W#IX          CRRN01
     C                   EVAL      W#IX = W#SAV#
     C                   MOVE      *BLANKS       FMPOS
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E03
     C                   ENDFOR                                                 E02
      *
     C                   IF        FMPOS <> *BLANKS                             B02
     C                   MOVE      *BLANKS       FMPOS
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
     C                   Z-ADD     W#CRC#        CRRN01
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
      * SRPR01 - Work with Status Process                           *
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
     C                   READC     OMH02001
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
     C                   UPDATE    OMH02001
      *
      *          ------------------------------------------
      *          Do action if option is '4' or '12'
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      W#OPTI = '12'
     C                   EXSR      SRSC03
     C                   WHEN      W#OPTI <> *BLANKS
     C                   EXSR      SRSC02
     C                   ENDSL
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
     C                   READC     OMH02001
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
      * SRSC02 - Screen 2 processing routine                        *
      *                                                             *
      * This routine displays the second screen.                    *
      *                                                             *
      * 80 off & 81 off = Add                                       *
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
     C                   EXFMT     OMH02004
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
     C                   EVAL      *IN(72) = *OFF
     C                   EVAL      EROR02 = '0'
     C                   EVAL      LOOP02 = '0'
      *
      *          ------------------------------------------
      *          F4 prompt for status route
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   EXSR      SRLIST
     C                   EVAL      EROR02 = *ON
     C                   EVAL      W#F4I = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Do edit routine if not F3 or F12
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
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 on  = Delete                                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN02        BEGSR
      *
      *          ------------------------------------------
      *          Determine the action, set screen indicators
      *          ------------------------------------------
     C                   EVAL      *IN41 = *OFF
     C                   IF        *INKF = *ON                                  B01
     C                   EVAL      *IN(80) = *OFF
     C                   EVAL      *IN(81) = *OFF
     C                   ELSE                                                   X01
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *ON
      *
      *          ------------------------------------------
      *          Display deletion message
      *          ------------------------------------------
     C                   EVAL      MSGID = 'DLT0001'
     C                   EXSR      SNDMSG
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
     C                   MOVE      *BLANKS       FMSTRC
     C                   MOVE      *BLANKS       FMSTRD
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Get record in case of deletion
      *          ------------------------------------------
     C                   MOVEL     FMHEDC        K#HEDC
     C                   MOVEL     FMEMPC        K#EMPC
     C                   MOVEL     FMSTRC        K#STRC
     C     $SA1K2        CHAIN     OMESAR
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          If *ALL not indicated, read same file
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF
     C     $SA1K3        CHAIN(N)  OMESAR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL     SASTRC        FMSTRC
     C                   MOVEL     SASTRC        FMSTRC
     C                   MOVEL     SAHEDC        FMHEDC
     C     $TR1K2        CHAIN     OMSTRR
     C                   EVAL      *IN98 = NOT %FOUND
     C                   MOVEL     TRSTRD        FMSTRD
     C                   ELSE
      *
      *          ------------------------------------------
      *          Else: screenfields remain the same
      *          ------------------------------------------
     C                   EVAL      W#ALLI = *ON
     C                   ENDIF                                                  E01
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
      *          Addition.
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
      *
      *          ------------------------------------------
      *          Blanks no good.
      *          ------------------------------------------
     C                   IF        FMSTRC = *BLANKS                             B02
     C                   EVAL      MSGDTA = VAR(1)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = '1'
     C                   EVAL      *IN70 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if the record already exists
      *          ------------------------------------------
     C                   IF        FMSTRC <> '*ALL'                             B02
     C                   MOVEL     FMSTRC        K#STRC
     C     $SA1K3        CHAIN     OMESAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B03
     C                   EVAL      MSGID = 'FILE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = '1'
     C                   EVAL      *IN70 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Check if the is valid
      *          ------------------------------------------
     C     $TR1K2        CHAIN     OMSTRL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B03
     C                   EVAL      MSGDTA = VAR(1)
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = '1'
     C                   EVAL      *IN70 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Fill description
      *          ------------------------------------------
     C                   IF        EROR02 = *OFF                                B03
     C                   MOVEL     TRSTRD        FMSTRD
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
      * SRPR02 - Process screen 2                                   *
      *                                                             *
      * This routine updates the statusses file                     *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR02        BEGSR
      *
      *          ------------------------------------------
      *          If we have used F4: act as though fields were changed
      *          ------------------------------------------
     C                   IF        W#F4I = *ON
     C                   EVAL      *IN40 = W#F4I
     C                   EVAL      W#F4I = *OFF
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Loop again if fields where changed.
      *          ------------------------------------------
     C                   EVAL      LOOP02 = *IN40
      *
      *          ------------------------------------------
      *          Delete the file if wanted.
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B01
     C                             AND *IN81 = *ON
      *
      *          ------------------------------------------
      *          *ALL: deletion measns adding the rest
      *          ------------------------------------------
     C                   IF        W#ALLI = *ON
     C                   MOVEL     FMSTRC        K#STRC
     C                   MOVE      *LOVAL        FMSTRC
      *
      *          ------------------------------------------
      *          Read from status route file to add
      *          ------------------------------------------
     C     $TR1K2        SETLL     OMSTRR
     C     $TR1K1        READE     OMSTRR
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF
      *
      *          ------------------------------------------
      *          One will not be added
      *          ------------------------------------------
     C                   IF        TRSTRC <> K#STRC
     C                   MOVEL     TRSTRC        SASTRC
     C                   MOVEL     FMHEDC        SAHEDC
     C                   MOVEL     FMEMPC        SAEMPC
     C                   WRITE(E)  OMESAR
     C                   EVAL      *IN99 = %ERROR
     C                   ENDIF
     C     $TR1K1        READE     OMSTRR
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO
     C                   EVAL      W#ALLI = *OFF
     C                   ELSE
      *
      *          ------------------------------------------
      *          If not *ALL: simply delete record
      *          ------------------------------------------
     C     $SA1K3        DELETE    OMESAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Do if something changed.
      *          ------------------------------------------
     C                   IF        *IN40 = *ON                                  B02
      *
      *          ------------------------------------------
      *          Do if no request for all status routes
      *          ------------------------------------------
     C                   IF        FMSTRC <> C#ALL                              B03
      *
      *          ------------------------------------------
      *          Get the file
      *          ------------------------------------------
     C     $TR1K2        CHAIN     OMSTRL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill the file
      *          ------------------------------------------
     C                   MOVEL     FMHEDC        SAHEDC
     C                   MOVEL     FMSTRC        SASTRC
     C                   MOVEL     FMEMPC        SAEMPC
      *
      *          ------------------------------------------
      *          Add to the file
      *          ------------------------------------------
     C                   WRITE     OMESAR
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Do if request for all status routes
      *          ------------------------------------------
     C                   EVAL      *IN41 = *ON
     C                   MOVE      *LOVAL        SASTRC
     C                   MOVEL     FMHEDC        SAHEDC
     C                   MOVEL     FMEMPC        SAEMPC
     C     $SA1K3        SETLL     OMESAR
     C     $SA1K2        READE     OMESAR
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Remove them all from file
      *          ------------------------------------------
     C                   DOW       *IN99 <> *ON
     C                   DELETE    OMESAR
     C     $SA1K2        READE     OMESAR
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO
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
      * SRLIST - List                                               *
      *                                                             *
      * This routine controls the F4 key                            *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRLIST        BEGSR
      *
      *          ------------------------------------------
      *          List of status routes
      *          ------------------------------------------
     C                   EVAL      P1HEDC = P#HEDC
     C                   CALL      'OMX126'      $MX126                   99
     C                   MOVEL     P1STRC        FMSTRC
     C                   MOVEL     P1STRD        FMSTRD
     C                   EVAL      *IN40 = *ON
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC03 - Screen 3 processing routine                        *
      *                                                             *
      * This routine calls program OMH014 which is the status action*
      * maintenance function.                                       *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC03        BEGSR
      *
      *          ------------------------------------------
      *          Call the program.
      *          ------------------------------------------
     C                   EVAL      P#STRC = FMSTRC
     C                   EVAL      P#STRD = FMSTRD
     C                   EVAL      P#ACTC = '50'
     C                   CALLP     OMH014 ( P#HEDC : P#STRC : P#STRD :
     C                             P#ACTC : P#STAT )
      *
      *          ------------------------------------------
      *          Set the correct indicators
      *          ------------------------------------------
     C                   IF        P#STAT = C#EXIT                              B01
     C                   EVAL      *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X01
     C                   IF        P#STAT = C#CNCL                              B02
     C                   EVAL      *INKL = *ON
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
      * SRCHGH - Change the Helpdesk                                *
      *                                                             *
      * This routine changes the helpdesk                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCHGH        BEGSR
      *
      *          ------------------------------------------
      *          If the HelpDesk code is blanks, then reset
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
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Fill screen fields from parameters
      *          ------------------------------------------
     C                   MOVE      P#HEDC        FMHEDC
     C                   MOVEL     P#HEDD        FMHEDD
     C                   MOVEL     P#EMPC        FMEMPC
     C                   MOVEL     P#USDC        FMUSDC
     C                   EXSR      SRIN01
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
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
      *          Keyfield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
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
      *          Parameter list for OMX126 (Select status route)
      *          ------------------------------------------
     C     $MX126        PLIST
     C                   PARM                    P1HEDC
     C                   PARM                    P1STRC
     C                   PARM                    P1STRD
      *
      *          ------------------------------------------
      *          Parameter list for OMH010 (Action maintenance)
      *          ------------------------------------------
     C     $MH014        PLIST
     C                   PARM                    P#HEDC
     C                   PARM      FMSTRC        P#STRC
     C                   PARM      FMSTRD        P#STRD
     C                   PARM      '50'          P#ACTC
     C                   PARM                    P#STAT
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
      *          OMESA Status route access file
      *          ------------------------------------------
     C     $SA1K3        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#EMPC
     C                   KFLD                    K#STRC
     C     $SA1K2        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#EMPC
      *
      *          ------------------------------------------
      *          OMSTR Status file
      *          ------------------------------------------
     C     $TR1K2        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMSTRC
     C     $TR1K1        KLIST
     C                   KFLD                    FMHEDC
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
      *          Fill screen fields from parameters
      *          ------------------------------------------
     C                   MOVE      P#HEDC        FMHEDC
     C                   MOVEL     P#HEDD        FMHEDD
     C                   MOVE      P#EMPC        FMEMPC
     C                   MOVEL     P#USDC        FMUSDC
      *
      *          ------------------------------------------
      *          Get message data
      *          ------------------------------------------
     C                   EXSR      SRGVAR
      *
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
     C                   WHEN      E#FILE = 'OMH020D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMSTRL1 '                          X02
     C                   MOVEL     STRL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMESAL1 '                          X02
     C                   MOVEL     ESAL1         P#FILE
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
      *          Error in *PSSR, exit immediately.
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   EVAL      *INLR = *ON
     C                   RETURN
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
      *          End normal
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
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
STR0001 - status route                          1
