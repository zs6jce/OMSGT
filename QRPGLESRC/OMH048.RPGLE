     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH048                                        *
      * Function ...: Maintenance predefined texts                  *
      * Author .....: Remain Software                               *
      * Module .....: HelpDesk                                      *
      * Parameters .: P#HEDC    -  HelpDesk code                    *
      *               P#STAT    -  Status                           *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMTIPL1   UF A E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(TIPL1)
     FOMH048D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
     F                                     SFILE(OMH04801:RRN01)
     F                                     SFILE(OMH04805:RRN04)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension spec's                                             *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message varibles in message data parm.   *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(12)
     D MSD             S              7    DIM(12) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
     D ARR             S              5  0 DIM(2)
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
     D TIPL1           DS            57
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
      *
     D C#NEW           C                   CONST('*NEW ')
     D C#CMP           C                   CONST('*CMP ')
     D C#LCK           C                   CONST('*LCK ')
     D C#LOCK          C                   CONST('*LOCK')
     D C#ANY           C                   CONST('*ANY ')
     D C#PLND          C                   CONST('*PLND')
     D C#OMS           C                   CONST('*OMS ')
      * Prototype for OMH048
     D OMH048          PR
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
      *          Parameter list for OMH902C (Clear message queue)
      * Prototype for 'OMX105'
     D OMX105          PR                  EXTPGM('OMX105')
     D FMHEDC_                             LIKE(FMHEDC)
     D FMHEDD_                             LIKE(FMHEDD)
      * Prototype for 'OMH049'
     D OMH049          PR                  EXTPGM('OMH049')
     D P#HEDC_                             LIKE(P#HEDC)
     D P#TXID_                             LIKE(P#TXID)
     D P#HDR#_                             LIKE(P#HDR#)
     D P#ITXT_                             LIKE(P#ITXT)
     D P#SUL#_                             LIKE(P#SUL#)
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
      *          OMTIP Status file
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
      *          Parameter list for OMH010 (Action maintenance)
      * *ENTRY Interface for Main Procedure
     D OMH048          PI
     D P#HEDC                         5
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D p#hdr#          S              5P 0
     D EROR01          S                   LIKE(INDI)
     D EROR02          S                   LIKE(INDI)
     D EROR04          S                   LIKE(INDI)
     D EROR06          S                   LIKE(INDI)
     D I#PSSR          S              1
     D IZ              S              3  0
     D LOOP01          S                   LIKE(INDI)
     D LOOP02          S                   LIKE(INDI)
     D LOOP04          S                   LIKE(INDI)
     D LOOP06          S                   LIKE(INDI)
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#ACTC          S                   LIKE(ACTC)
     D P#FILE          S             57
     D P#INCD          S                   LIKE(INCD)
     D P#ITXT          S                   LIKE(ITXT)
     D P#MSTA          S             40
     D P#OPTI          S                   LIKE(INDI)
     D P#SUL#          S                   LIKE(SUL#)
     D P#TXID          S                   LIKE(MSID)
     D RRN01           S              5  0
     D RRN04           S              5  0
     D S#HEDC          S                   LIKE(HEDC)
     D TRRN01          S              5  0
     D W#CRC1          S              5  0
     D W#CRC4          S              5  0
     D W#DELI          S              1
     D W#HDR#          S              5  0
     D W#IX            S              5  0
     D W#MSGI          S              1
     D W#OPTI          S                   LIKE(FMOPTI)
     D W#SAV#          S              5  0
     D W#TXID          S              7
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
     C                   IF        FMFMTC = 'OMH04802'                          B01
     C                             AND FMFLDC = 'FMHEDC'                        B02
     C                   MOVE      FMHEDC        S#HEDC
     C                   CALLP     OMX105 ( FMHEDC : FMHEDD )
     C                   IF        FMHEDC <> S#HEDC                             B02
     C                   EXSR      SRCHGH
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Invalid prompt location.
      *          ------------------------------------------
     C                   EVAL      *IN71 = *ON
     C                   EVAL      MSGID = 'PMT0002'
     C                   EXSR      SNDMSG
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
     C                   WRITE     OMH04803
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
     C                   EXFMT     OMH04802
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
     C                   EVAL      W#CRC1 = D#CRC#
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
      *          F6=Add status
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
     C                   WRITE     OMH04802
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Read first status record
      *          ------------------------------------------
     C     FMHEDC        SETLL     OMTIPL1
     C     FMHEDC        READE     OMTIPL1
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
     C                   IF        IPTXID <> W#TXID
     C                   MOVEL     IPTXID        W#TXID
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVEL     IPTXID        FMTXID
     C                   MOVEL     IPINCD        FMINCD
     C                   ADD       1             RRN01
     C                   WRITE     OMH04801
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     FMHEDC        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Fill current subfile relative record number
      *          to display first subfile page
      *          ------------------------------------------
     C                   Z-ADD     1             CRRN01
     C                   EVAL      TRRN01 = RRN01
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
     C                   READC(E)  OMH04801
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
     C                   IF        FMOPTI = ' 2'                                B03
     C                             OR FMOPTI = ' 4'
      *
      *          ------------------------------------------
      *          Option is valid, check if subfile record is still
      *          active in security file if not then error, else ok.
      *          ------------------------------------------
     C     $IP1K1        SETLL     OMTIPL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B05
     C                   EVAL      MSGID = 'FILE002'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X05
     C                   ADD       1             W#VALC
     C                   ENDIF                                                  E05
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
     C                   UPDATE    OMH04801
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   UPDATE    OMH04801
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH04801
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          If subfile must be positioned, and no errors occured
      *          then position it.
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B01
     C                             AND FMPOS <> *BLANKS
     C                   FOR       W#IX = 1 TO TRRN01                           B02
     C     W#IX          CHAIN     OMH04801
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        FMTXID >= FMPOS                              B03
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
     C                   Z-ADD     W#CRC1        CRRN01
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
     C                   READC     OMH04801
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
     C                   EVAL      W#CRC1 = RRN01
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPTI        W#OPTI
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    OMH04801
      *
      *          ------------------------------------------
      *          Do action if option is '4' or '2'
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      W#OPTI = ' 2'
     C                   EXSR      SRSC04
     C                   WHEN      W#OPTI = ' 4'
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
     C                   READC     OMH04801
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
     C                   Z-ADD     W#CRC1        CRRN01
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
      * status field   and 81 controls the protection of the        *
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
     C                   EXFMT     OMH04804
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
      * status field   and 81 controls the protection of the        *
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
     C                   MOVE      *BLANKS       FMTXID
     C                   MOVE      *BLANKS       FMINCD
     C                   MOVE      *BLANKS       FMITXT
     C                   MOVE      *BLANKS       FMSUL#
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Get record else
      *          ------------------------------------------
     C     $IP1K1        CHAIN(N)  OMTIPL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      IPTXID        FMTXID
     C                   MOVEL     IPINCD        FMINCD
     C                   MOVEL     IPITXT        FMITXT
     C                   MOVEL     IPSUL#        FMSUL#
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
     C                   IF        FMTXID = *BLANKS                             B02
     C                   EVAL      MSGDTA = VAR(1)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = '1'
     C                   EVAL      *IN70 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Blanks no good.
      *          ------------------------------------------
     C                   IF        FMINCD = *BLANKS                             B02
     C                   EVAL      MSGDTA = VAR(2)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = '1'
     C                   EVAL      *IN71 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Blanks no good.
      *          ------------------------------------------
     C                   IF        FMITXT < '1'                                 B02
     C                             OR FMITXT > '4'                              B02
     C                   EVAL      MSGID = 'TYP0004'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = '1'
     C                   EVAL      *IN72 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Blanks no good.
      *          ------------------------------------------
     C                   IF        FMSUL# < '0'                                 B02
     C                             OR FMSUL# > '9'                              B02
     C                   EVAL      MSGID = 'SUL0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = '1'
     C                   EVAL      *IN73 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if the record already exists
      *          ------------------------------------------
     C     $IP1K1        CHAIN     OMTIPL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B02
     C                   EVAL      MSGID = 'FILE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = '1'
     C                   EVAL      *IN70 = *ON
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          All actions (not delete)
      *          ------------------------------------------
     C                   IF        *IN81 = *OFF                                 B01
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
      * This routine updates the statuses file                      *
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
     C                   EVAL      FMHDR# = 0
     C     $IP1K2        SETLL     OMTIPL1
     C     $IP1K1        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF
     C                   DELETE    OMTIPL1
     C     $IP1K1        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO
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
     C     $IP1K1        CHAIN     OMTIPL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill the file
      *          ------------------------------------------
     C                   MOVE(P)   FMHEDC        IPHEDC
     C                   MOVE(P)   FMTXID        IPTXID
     C                   MOVEL(P)  FMINCD        IPINCD
     C                   MOVEL(P)  FMSUL#        IPSUL#
     C                   MOVEL(P)  FMITXT        IPITXT
     C                   Z-ADD     10            IPHDR#
     C                   Z-ADD     10            IPLIN#
     C                   MOVE      '1'           IPACTC
      *
      *          ------------------------------------------
      *          Add or update the file. Fill description only on add.
      *          *IN99='0' status record found in previous chain
      *          *IN99='1' status record not found in previous chain
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B03
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMTIPR
     C                   ENDIF
     C                   ELSE                                                   X03
     C                   IF        *IN99 = *ON
     C                   WRITE     OMTIPR
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
      * SRSC04 - Screen 4 processing routine                        *
      *                                                             *
      * This routine calls program OMH010 which is the status action*
      * maintenance function.                                       *
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
     C                   DOU       LOOP04 = '0'                                 B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR04 = '0'                                 B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH04803
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        RRN04 = *ZEROS                               B03
     C                   EVAL      *IN72 = *ON
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   EXFMT     OMH04806
      *
      *          ------------------------------------------
      *          CMD-12 is CMD-3 is exit
      *          ------------------------------------------
     C                   IF        *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   EVAL      W#CRC4 = D#CRC#
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
      *          Change the HelpDesk if required.
      *          ------------------------------------------
     C                   IF        S#HEDC <> FMHEDC                             B05
     C                   EXSR      SRCHGH
     C                   EVAL      EROR04 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          F6=Add status
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B06
     C                   EXSR      SRSC05
     C                   EVAL      EROR04 = *ON
     C                   ELSE                                                   X06
      *
      *          ------------------------------------------
      *          Edit
      *          ------------------------------------------
     C                   EXSR      SRED04
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
     C                   EXSR      SRPR04
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
      * SRIN04 - Screen 4 initialisation                            *
      *                                                             *
      * This routine blanks the input fields on the screen.         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN04        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMPOS
     C                   EVAL      S#HEDC = P#HEDC
     C                   MOVEL(P)  FMINCD        FMINC4
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
      * SRRF04 - Work with questions refresh                        *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF04        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   MOVE      S#HEDC        FMHEDC
     C                   MOVE      FMTXID        FMTXI4
     C                   EVAL      RRN04 = *ZEROS
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH04806
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Read first status record
      *          ------------------------------------------
     C     $IP1K1        SETLL     OMTIPL1
     C     $IP1K1        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   EVAL      W#HDR# = 0
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B01
     C                   IF        IPHDR# <> W#HDR#
     C                   Z-ADD     IPHDR#        W#HDR#
      *
      *          ------------------------------------------
      *          Fill overall description
      *          ------------------------------------------
     C                   IF        IPHDR# = 10
     C                             AND IPLIN# = 10
     C                   MOVEL(P)  IPINCD        FMINC4
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVEL     IPTXID        FMTXID
     C                   MOVEL     IPITXT        FMITXT
     C                   MOVEL     IPSUL#        FMSUL#
     C                   MOVEL     IPINCD        FMINCD
     C                   Z-ADD     IPHDR#        FMHDR#
     C                   ADD       1             RRN04
     C                   WRITE     OMH04805
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     $IP1K1        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
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
      * SRED04 - Work with Employee edit                            *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED04        BEGSR
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
     C                   EVAL      W#VALC = *ZEROS
     C                   EVAL      W#IX = 1
     C                   EVAL      CRRN04 = *ZEROS
     C                   EVAL      W#MSGI = '0'
     C                   EVAL      W#SAV# = RRN01
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   READC(E)  OMH04805
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
     C                   IF        FMOPTI = ' 2'                                B03
     C                             OR FMOPTI = ' 4'
      *
      *          ------------------------------------------
      *          Option is valid, check if subfile record is still
      *          active in security file if not then error, else ok.
      *          ------------------------------------------
     C     $IP1K1        SETLL     OMTIPL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B05
     C                   EVAL      MSGID = 'FILE002'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X05
     C                   ADD       1             W#VALC
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          If option is other than valid, then field is in error
      *          Reverse screen fields to indicate this
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR04 = *ON
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
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Check whether deletion is possible
      *          ------------------------------------------
     C                   IF        FMOPTI = ' 4'
     C     $IP1K2        SETLL     OMTIPL1
     C     $IP1K1        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   IF        *IN99 = *OFF
     C     $IP1K1        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      MSGDTA = VAR(1)
     C                   EVAL      MSGID = 'DEL0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR04 = '1'
     C                   EVAL      *IN70 = *ON
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          If an error was found, then position on the page of
      *          the first error
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B03
     C                             AND *IN70 = *ON
     C                   Z-ADD     RRN04         CRRN04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Update subfile
      *          ------------------------------------------
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH04805
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   UPDATE    OMH04805
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH04805
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          If subfile must be positioned, and no errors occured
      *          then position it.
      *          ------------------------------------------
     C                   IF        CRRN04 = *ZEROS                              B01
     C                             AND FMPOS <> *BLANKS
     C                   FOR       W#IX = 1 TO W#SAV#                           B02
     C     W#IX          CHAIN     OMH04805
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        FMTXID >= FMPOS                              B03
     C                   Z-ADD     W#IX          CRRN04
     C                   EVAL      W#IX = W#SAV#
     C                   MOVE      *BLANKS       FMPOS
     C                   EVAL      EROR04 = *ON
     C                   ENDIF                                                  E03
     C                   ENDFOR                                                 E02
      *
     C                   IF        FMPOS <> *BLANKS                             B02
     C                   MOVE      *BLANKS       FMPOS
     C                   Z-ADD     W#SAV#        CRRN04
     C                   EVAL      EROR04 = *ON
     C                   ENDIF                                                  E02
      *
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Fill the current relative record on the page to
      *          display.
      *          ------------------------------------------
     C                   IF        CRRN04 = *ZEROS                              B02
     C                   Z-ADD     W#CRC4        CRRN04
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Reset overall relative record number
      *          ------------------------------------------
     C                   EVAL      RRN04 = W#SAV#
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR04 - Work with Status Process                           *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR04        BEGSR
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
     C                   READC     OMH04805
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
     C                   EVAL      LOOP04 = *ON
     C                   EVAL      W#CRC4 = RRN04
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPTI        W#OPTI
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    OMH04805
      *
      *          ------------------------------------------
      *          Do action if option is '4' or '2'
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      W#OPTI = ' 2'
     C                   EXSR      SRSC05
     C                   WHEN      W#OPTI = ' 4'
     C                   EXSR      SRSC06
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
     C                   READC     OMH04805
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If end of file found, then end loop
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          If end of processing, rearrange file if neccesary
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      W#DELI <> '0'
     C                   EXSR      SRARRF
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Reset subfile relative record number
      *          ------------------------------------------
     C                   Z-ADD     W#CRC4        CRRN04
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC05 - Screen 5 processing routine                        *
      *                                                             *
      * This routine calls program OMH049 which handles the texts   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC05        BEGSR
      *
      *          ------------------------------------------
      *          Call the program.
      *          ------------------------------------------
     C                   IF        *INKF = *ON
     C                   EVAL      P#TXID = *BLANKS
     C                   EVAL      P#ITXT = *BLANKS
     C                   EVAL      P#SUL# = *BLANKS
     C                   Z-ADD     99999         FMHDR#
     C     $IP1K2        SETGT     OMTIPL1
     C     $IP1K1        READPE    OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C     10            ADD       IPHDR#        P#HDR#
     C                   EVAL      P#ACTC = '60'
     C                   ELSE
     C                   MOVEL     FMTXID        P#TXID
     C                   Z-ADD     FMHDR#        P#HDR#
     C                   MOVEL     FMITXT        P#ITXT
     C                   MOVEL     FMSUL#        P#SUL#
     C                   EVAL      P#ACTC = '20'
     C                   ENDIF
     C                   EVAL      P#TXID = FMTXID
     C                   CALLP     OMH049 ( P#HEDC : P#TXID : P#HDR# :
     C                             P#ITXT : P#SUL# : P#ACTC : P#STAT )
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
      * SRSC06 - Screen 6 processing routine                        *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC06        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EXSR      SRIN06
      *
      *          ------------------------------------------
      *          Do while no end
      *          ------------------------------------------
     C                   DOU       LOOP06 = '0'                                 B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR06 = '0'                                 B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the screen
      *          ------------------------------------------
     C                   EXFMT     OMH04807
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
     C                   EVAL      *IN(73) = *OFF
     C                   EVAL      EROR06 = '0'
     C                   EVAL      LOOP06 = '0'
      *
      *          ------------------------------------------
      *          Do edit routine if not F3 or F12.
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B03
     C                   EXSR      SRED06
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
     C                   EXSR      SRPR06
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
      * SRIN06 - Screen 2 initialisation                            *
      *                                                             *
      * This routine blanks the input fields on the screen.         *
      * Pending on the required action.           The change, add   *
      * and delete options, are combined into one screen, controled *
      * with indicator 80 & 81. 80 controls the protection of the   *
      * status field   and 81 controls the protection of the        *
      * description.                                                *
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 off = Change                                    *
      * 80 on  & 81 on  = Delete                                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN06        BEGSR
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
     C                   EXSR      SRRF06
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF06 - Screen 2 refresh                                   *
      *                                                             *
      * This screen is called for future processing.                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF06        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields if addition
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C                   MOVE      *BLANKS       FMTXID
     C                   MOVE      *BLANKS       FMINCD
     C                   MOVE      *BLANKS       FMITXT
     C                   MOVE      *BLANKS       FMSUL#
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Get record else
      *          ------------------------------------------
     C     $IP1K2        CHAIN(N)  OMTIPL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      IPTXID        FMTXID
     C                   MOVEL     IPINCD        FMINCD
     C                   MOVEL     IPITXT        FMITXT
     C                   MOVEL     IPSUL#        FMSUL#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED06 - Screen 2 edit                                      *
      *                                                             *
      * This routine handles deletion                               *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED06        BEGSR
      *
      *          ------------------------------------------
      *          One record means header
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR06 - Process screen 2                                   *
      *                                                             *
      * This routine performs deletion                              *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR06        BEGSR
      *
      *          ------------------------------------------
      *          Loop again if fields where changed.
      *          ------------------------------------------
     C                   MOVE      *IN40         LOOP06
     C                   IF        W#DELI = '0'
     C                   EVAL      W#DELI = '1'
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Delete the bloc if wanted.
      *          ------------------------------------------
     C                   EVAL      IPLIN# = 0
     C     $IP1K3        SETLL     OMTIPL1
     C     $IP1K2        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF
     C                   IF        IPHDR# <> 10
     C                             OR IPLIN# <> 10
     C                   DELETE    OMTIPL1
     C                   ELSE
     C                   EVAL      W#DELI = '2'
     C                   ENDIF
     C     $IP1K2        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRARRF - Arrange file
      *                                                             *
      * This routine rearranges the file                            *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRARRF        BEGSR
      *
      *          ------------------------------------------
      *          Clean indicator
      *          ------------------------------------------
     C                   EVAL      IZ = 1
     C                   CLEAR                   ARR
      *
      *          ------------------------------------------
      *          Store
      *          ------------------------------------------
     C                   EVAL      FMHDR# = 0
     C     $IP1K2        SETLL     OMTIPL1
     C     $IP1K1        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF
     C                             AND IZ <= 2
     C                   IF        IPHDR# <> FMHDR#
     C                   Z-ADD     IPHDR#        FMHDR#
     C                   Z-ADD     IPHDR#        ARR(IZ)
     C                   ADD       1             IZ
     C                   ENDIF
     C     $IP1K1        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO
      *
      *          ------------------------------------------
      *          Delete first record
      *          ------------------------------------------
     C                   IF        ARR(2) <> 0
     C                             AND W#DELI = '2'
     C                   Z-ADD     10            FMHDR#
     C                   Z-ADD     10            IPLIN#
     C     $IP1K3        CHAIN     OMTIPL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF
     C                   EVAL      ARR(1) = ARR(2)
     C                   DELETE    OMTIPL1
     C                   ENDIF
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Rearrange file
      *          ------------------------------------------
     C                   IF        ARR(1) <> 10
     C                   Z-ADD     ARR(1)        FMHDR#
     C                   EVAL      IPLIN# = 0
     C     $IP1K3        SETLL     OMTIPL1
     C     $IP1K2        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF
     C                   DELETE    OMTIPL1
     C                   Z-ADD     10            IPHDR#
     C                   WRITE     OMTIPR
     C     $IP1K1        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   EVAL      W#DELI = '0'
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
      *          Parameter list for OMH010 (Action maintenance)
      *          ------------------------------------------
     C     $MH049        PLIST
     C                   PARM                    P#HEDC
     C                   PARM      FMTXID        P#TXID
     C                   PARM                    P#HDR#
     C                   PARM                    P#ITXT
     C                   PARM                    P#SUL#
     C                   PARM                    P#ACTC
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
      *          OMTIP Status file
      *          ------------------------------------------
     C     $IP1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMTXID
      *
     C     $IP1K2        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMTXID
     C                   KFLD                    FMHDR#
      *
     C     $IP1K3        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMTXID
     C                   KFLD                    FMHDR#
     C                   KFLD                    IPLIN#
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
     C                   MOVEL     D8HEDD        FMHEDD
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
     C                   WHEN      E#FILE = 'OMH048D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMTIPL1 '                          X02
     C                   MOVEL     TIPL1         P#FILE
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
     C                   ELSE
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
     C                   FOR       MS = 1 TO 2                                  B01
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
TXT0001 - text type                            1
TXT0003 - text description                     1
