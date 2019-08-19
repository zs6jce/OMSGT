     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ------------------------------------------------------------ *
      * Description                                                  *
      * ------------------------------------------------------------ *
      *                                                              *
      * Program ....: OMH016                                         *
      * Function ...: Escalation Maintenance                         *
      * Author .....: Remain Software                               *
      * Module .....: HelpDesk                                       *
      * Parameters .: P#HEDC    -  HelpDesk Code                     *
      *               P#STAT    -  Return Status                     *
      *                                                              *
      * This program maintains the Escalation file.                  *
      *                                                              *
      * Modification: 15-8-1997                                      *
      * Author .....: Remain Software                               *
      * Module .....: Add registration of exit/entry programs        *
      *                                                              *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * File specifications                                          *
      * ------------------------------------------------------------ *
     FOMESCL1   UF A E           K DISK    INFDS(ESCL1)
     F                                     INFSR(*PSSR)
     FOMSTAL1   IF   E           K DISK    INFDS(STAL1)
     F                                     INFSR(*PSSR)
     FOMH016D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
     F                                     SFILE(OMH01601:RRN01)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension specifications                                     *
      * ------------------------------------------------------------ *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm    *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(4)
     D MSD             S              7    DIM(4) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
      *COPY QCPYSRC,OMSREFREN
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Input specifications                                         *
      * ------------------------------------------------------------ *
      *
      *          ------------------------------------------
      *          Field reference file used to define fields
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          File information data structures
      *          ------------------------------------------
     D ESCL1           DS            57
     D STAL1           DS            57
      *
      *          ------------------------------------------
      *          Screen information data structure
      *          ------------------------------------------
     D INFO            DS
     D  STATUS           *STATUS
     D  D#CRC#               378    379B 0
      *
      *          ------------------------------------------
      *          Program status data structure
      *          ------------------------------------------
     D PGM            SDS
     D  E#FILE               201    208
     D  PGMQ             *PROC
      *
      *          ------------------------------------------
      *          Data structures used for positioning
      *          FMPOSS = Screen values
      *          FMPOSR = Record values
      *          ------------------------------------------
     D FMPOSS          DS             6
     D FMPOS1                         1
     D FMPOS2                         5
      *
     D FMPOSR          DS             6
     D FMSUL#                         1
     D FMSTAT                         5
      *
      *          ------------------------------------------
      *          Helpdesk data structures.
      *          ------------------------------------------
      /COPY QCPYLESRC,OMHLPDS
      *
      *          ------------------------------------------
      *          Error buffer for API call
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
     D C#NORM          C                   CONST('*NORM')
     D C#TERM          C                   CONST('*TERM')
     D C#ENTR          C                   CONST('*ENTRY')
     D C#EXIT          C                   CONST('*EXIT ')
     D C#BOTH          C                   CONST('*BOTH ')
     D C#LIBL          C                   CONST('*LIBL')
     D C#PGM           C                   CONST('*PGM')
      * Prototype for OMH016
     D OMH016          PR
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
      * Prototype for 'OMX123'
     D OMX123          PR                  EXTPGM('OMX123')
     D P#HEDC_                             LIKE(P#HEDC)
     D P#STAT_                             LIKE(P#STAT)
     D P#STAD_                             LIKE(P#STAD)
      * Prototype for 'OMH906C'
     D OMH906C         PR                  EXTPGM('OMH906C')
     D P1PGMN_                             LIKE(P1PGMN)
     D P1LIBC_                             LIKE(P1LIBC)
     D P1OBJT_                             LIKE(P1OBJT)
     D P1OBJD_                             LIKE(P1OBJD)
     D P1STAT_                             LIKE(P1STAT)
      *
      *          ------------------------------------------
      *          OMESCL1 Escalation file
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
      *          Parameter list for OMH906C (Check object)
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
      *          Parameter list for OMH029 (Get info)
      * *ENTRY Interface for Main Procedure
     D OMH016          PI
     D P#HEDC                         5
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D w#crc#          S              5P 0
     D w#ix            S              5P 0
     D w#sav#          S              5P 0
     D w#valc          S              5P 0
     D EROR01          S                   LIKE(INDI)
     D EROR02          S                   LIKE(INDI)
     D I#MSGI          S                   LIKE(INDI)
     D I#PSSR          S              1
     D LOOP01          S                   LIKE(INDI)
     D LOOP02          S                   LIKE(INDI)
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#FILE          S             57
     D P#HEDD          S                   LIKE(HEDD)
     D P#MSTA          S             40
     D P#OPTI          S                   LIKE(INDI)
     D P#STAD          S                   LIKE(STAD)
     D P#SUL#          S                   LIKE(SUL#)
     D P1LIBC          S                   LIKE(LIBC)
     D P1OBJD          S             50
     D P1OBJT          S              9
     D P1PGMN          S                   LIKE(PGMN)
     D P1STAT          S                   LIKE(STAT)
     D RRN01           S              5  0
     D W#OPTI          S                   LIKE(FMOPTI)
      *----------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *----------------------------------------------------------------------
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Main line                                                    *
      * ------------------------------------------------------------ *
      *
      *          ------------------------------------------
      *          Entry parameters
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * CLRMSG - Clear message subfile                               *
      *                                                              *
      * This routine calls a subprogram wich clears the message      *
      * subfile.                                                     *
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SNDMSG - Send message                                        *
      *                                                              *
      * This routine calls a subprogram which sends a message to the *
      * program message queue.                                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SNDMSG        BEGSR
      *
      *          ------------------------------------------
      *          Call message sender
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
      *
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
      *          Display 2 - Status code
      *          ------------------------------------------
     C                   IF        FMFMTC = 'OMH01604'                          B01
     C                             AND FMFLDC = 'FMSTAT'                        B02
     C                   CALLP(E)  OMX123 ( P#HEDC : P#STAT : P#STAD )
     C                   IF        %ERROR = *OFF
     C                   EVAL      FMSTAT = P#STAT
     C                   ENDIF
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Invalid prompt location
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
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRVERW - Main program processing routine                     *
      *                                                              *
      * ------------------------------------------------------------ *
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC01 - Work with Escalations                               *
      *                                                              *
      * ------------------------------------------------------------ *
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
     C                   WRITE     OMH01603
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if
      *          records found
      *          ------------------------------------------
     C                   IF        RRN01 = *ZEROS                               B03
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   EXFMT     OMH01602
      *
      *          ------------------------------------------
      *          CMD-3 and CMD-12 is exit
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B03
     C                             OR *INKL = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
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
      *          CMD-5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B03
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          CMD-6=Add escalation
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B04
     C                   EXSR      SRSC02
     C                   EVAL      EROR01 = *ON
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
      *          Re-enter error loop if errors or CMD key
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRIN01 - Work with Escalations - Initialization              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Clear positioning fields
      *          ------------------------------------------
     C                   EVAL      FMPOS1 = *BLANKS
     C                   EVAL      FMPOS2 = *BLANKS
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRF01 - Work with Escalations - Refresh                     *
      *                                                              *
      * ------------------------------------------------------------ *
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
     C                   WRITE     OMH01602
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Read first escalation record
      *          ------------------------------------------
     C     $SC1K1        SETLL     OMESCL1
     C     $SC1K1        READE(N)  OMESCL1
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
     C                   MOVE      SCSUL#        FMSUL#
     C                   MOVE      SCSTAT        FMSTAT
     C                   Z-ADD     SCPRL#        FMPRL#
     C                   Z-ADD     SCPRH#        FMPRH#
     C                   Z-ADD     SCDYS#        FMDYS#
     C                   Z-ADD     SCHRS#        FMHRS#
     C                   Z-ADD     SCPIN#        FMPIN#
     C                   MOVEL     SCPGMN        FMPGMN
     C                   MOVEL     SCLIBC        FMLIBC
     C                   MOVEL     SCEOEI        FMEOEI
     C                   ADD       1             RRN01
     C                   WRITE     OMH01601
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     $SC1K1        READE(N)  OMESCL1
     C                   EVAL      *IN99 = %EOF
      *
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Fill current subfile relative record
      *          number to display first subfile page
      *          ------------------------------------------
     C                   Z-ADD     1             CRRN01
      *
      *          ------------------------------------------
      *          Save overall relative record number
      *          ------------------------------------------
     C                   EVAL      W#SAV# = RRN01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRED01 - Work with Escalations - Edit                        *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
     C                   EVAL      W#VALC = *ZEROS
     C                   EVAL      W#IX = 1
     C                   EVAL      CRRN01 = *ZEROS
     C                   EVAL      I#MSGI = *OFF
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   READC(E)  OMH01601
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
      *          ------------------------------------------
      *          If option is valid, then check the number
      *          of valid codes
      *          ------------------------------------------
     C                   IF        FMOPTI = '2'                                 B03
     C                             OR FMOPTI = '4'
      *
      *          ------------------------------------------
      *          Option is valid, check if subfile record
      *          is still active in Escalation file.
      *          If not then error, else OK.
      *          ------------------------------------------
     C     $SC1K4        SETLL     OMESCL1
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
      *          If option is other than valid, then error.
      *          Reverse screen fields to indicate this.
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
      *
      *          ------------------------------------------
      *          Send the message if not yet sent
      *          ------------------------------------------
     C                   IF        I#MSGI = *OFF                                B04
     C                   MOVEL(P)  FMOPTI        MSGDTA
     C                   EVAL      MSGID = 'OPTE021'
     C                   EXSR      SNDMSG
     C                   EVAL      I#MSGI = *ON
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If an error was found, then position on
      *          the page of the first error
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
     C                   UPDATE    OMH01601
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   UPDATE    OMH01601
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH01601
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          If there has been no error
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B01
      *
      *          ------------------------------------------
      *          If position-to was filled, then reposition
      *          ------------------------------------------
     C                   IF        FMPOS1 <> *BLANKS                            B02
     C                             OR FMPOS2 <> *BLANKS
      *
      *          ------------------------------------------
      *          Read through subfile to find valid record
      *          ------------------------------------------
     C                   FOR       W#IX = 1 TO W#SAV#                           B03
     C     W#IX          CHAIN     OMH01601
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        FMPOSR >= FMPOSS                             B04
     C                   Z-ADD     W#IX          CRRN01
     C                   EVAL      W#IX = W#SAV#
     C                   EVAL      FMPOS1 = *BLANKS
     C                   EVAL      FMPOS2 = *BLANKS
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E04
     C                   ENDFOR                                                 E03
      *
      *          ------------------------------------------
      *          If no valid subfile record was found, then
      *          position to last record of subfile
      *          ------------------------------------------
     C                   IF        FMPOS1 <> *BLANKS                            B03
     C                             OR FMPOS2 <> *BLANKS                         B04
     C                   EVAL      FMPOS1 = *BLANKS
     C                   EVAL      FMPOS2 = *BLANKS
     C                   Z-ADD     W#SAV#        CRRN01
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Reset the positioning fields
      *          ------------------------------------------
     C                   EVAL      FMPOS1 = *BLANKS
     C                   EVAL      FMPOS2 = *BLANKS
      *
      *          ------------------------------------------
      *          No positioning requested
      *          ------------------------------------------
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Fill the current relative record on the
      *          page to display
      *          ------------------------------------------
     C                   Z-ADD     W#CRC#        CRRN01
      *
     C                   ENDIF                                                  E02
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPR01 - Work with Escalations - Process                     *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      W#IX = 1
      *
      *          ------------------------------------------
      *          Read subfile next changed record if
      *          records in subfile and valid codes found
      *          ------------------------------------------
     C                   IF        RRN01 > *ZEROS                               B01
     C                             AND W#VALC > *ZEROS
     C                   READC     OMH01601
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Read subfile until valid option found
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Force loop of screen 1
      *          ------------------------------------------
     C                   EVAL      LOOP01 = *ON
     C                   EVAL      W#CRC# = RRN01
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPTI        W#OPTI
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    OMH01601
      *
      *          ------------------------------------------
      *          Do action if option is '4' or '2'
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      W#OPTI <> *BLANKS
     C                   EXSR      SRSC02
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Exit if CMD-3 pressed on screen 2
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B03
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Leave loop if CMD-12 pressed on screen 2
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                   LEAVE
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next changed
      *          ------------------------------------------
     C                   READC     OMH01601
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC02 - Maintain Escalation                                 *
      *                                                              *
      * ------------------------------------------------------------ *
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
     C                   EXFMT     OMH01604
      *
      *          ------------------------------------------
      *          CMD-3=Exit
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B03
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
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
     C                   EVAL      *IN(74) = *OFF
     C                   EVAL      *IN(75) = *OFF
     C                   EVAL      *IN(76) = *OFF
     C                   EVAL      *IN(77) = *OFF
     C                   EVAL      *IN(78) = *OFF
     C                   EVAL      *IN(79) = *OFF
     C                   EVAL      EROR02 = *OFF
     C                   EVAL      LOOP02 = *OFF
      *
      *          ------------------------------------------
      *          F4=Prompt
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   EXSR      PROMPT
     C                   EVAL      EROR02 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Do edit routine if not CMD-12
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B04
     C                   EXSR      SRED02
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors or CMD key
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do processing routine if not CMD-12
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRIN02 - Maintain Escalation - Initialization                *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRIN02        BEGSR
      *
      *          ------------------------------------------
      *          Determine the action
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B01
     C                   EVAL      *IN(80) = *OFF
     C                   EVAL      *IN(81) = *OFF
     C                   ELSE                                                   X01
     C                   IF        W#OPTI = '2'                                 B02
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRF02 - Maintain Escalation - Refresh                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRF02        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields if addition
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C                   EVAL      FMSUL# = '0'
     C                   EVAL      FMSTAT = *BLANKS
     C                   EVAL      FMPRL# = *ZEROS
     C                   EVAL      FMPRH# = *ZEROS
     C                   EVAL      FMDYS# = *ZEROS
     C                   EVAL      FMHRS# = *ZEROS
     C                   EVAL      FMPIN# = *ZEROS
     C                   CLEAR                   FMLIBC
     C                   CLEAR                   FMPGMN
     C                   CLEAR                   FMEOEI
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Get record else
      *          ------------------------------------------
     C     $SC1K4        CHAIN(N)  OMESCL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   Z-ADD     SCPRH#        FMPRH#
     C                   Z-ADD     SCDYS#        FMDYS#
     C                   Z-ADD     SCHRS#        FMHRS#
     C                   Z-ADD     SCPIN#        FMPIN#
     C                   MOVEL     SCPGMN        FMPGMN
     C                   MOVEL     SCLIBC        FMLIBC
     C                   MOVEL     SCEOEI        FMEOEI
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRED02 - Maintain Escalation - Edit                          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRED02        BEGSR
      *
      *          ------------------------------------------
      *          Test if addition
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
      *
      *          ------------------------------------------
      *          Check if status code is filled
      *          ------------------------------------------
     C                   IF        FMSTAT = *BLANKS                             B02
     C                   EVAL      MSGDTA = VAR(1)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN71 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if the record exists
      *          ------------------------------------------
     C     $SC1K4        CHAIN     OMESCL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B02
     C                   EVAL      MSGID = 'FILE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN70 = *ON
     C                   EVAL      *IN71 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if status exists
      *          ------------------------------------------
     C     $TA1K1        SETLL     OMSTAL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     FMSTAT        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN71 = *ON
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Upper limit must be higher than lower
      *          ------------------------------------------
     C                   IF        FMPRH# <= FMPRL#                             B01
     C                   MOVEA     VAR(3)        MC(1)
     C                   MOVEA     VAR(4)        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'EXC0010'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN72 = *ON
     C                   EVAL      *IN73 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          At least one of the fields Number of days
      *          or Number of hours should be filled
      *          ------------------------------------------
     C                   IF        FMDYS# = *ZEROS                              B01
     C                             AND FMHRS# = *ZEROS
     C                   EVAL      MSGID = 'MAN0003'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN74 = *ON
     C                   EVAL      *IN75 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Priority increment may not be zero
      *          ------------------------------------------
     C                   IF        FMPIN# = *ZEROS                              B01
     C                   MOVEA     VAR(2)        MC(1)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'ZER0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN76 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Warning if object does not exist
      *          ------------------------------------------
     C                   IF        FMPGMN <> *BLANKS
     C                   IF        FMLIBC = *BLANKS
     C                   MOVEL(P)  C#LIBL        FMLIBC
     C                   ENDIF
     C                   MOVEL(P)  FMPGMN        P1PGMN
     C                   MOVEL(P)  FMLIBC        P1LIBC
     C                   EVAL      P1OBJT = C#PGM
     C                   CALLP     OMH906C ( P1PGMN : P1LIBC : P1OBJT :
     C                             P1OBJD : P1STAT )
     C                   IF        P1STAT = C#TERM                              B01
     C                   EVAL      MSGID = 'WAR0016'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN77 = *ON
     C                   EVAL      *IN78 = *ON
     C                   ENDIF                                                  E01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If program is filled in, indicate when to activate
      *          ------------------------------------------
     C                   IF        FMPGMN <> *BLANKS                            B01
     C                             AND FMEOEI = *BLANKS
     C                   MOVE      '0'           FMEOEI
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Give a valid "when" indication
      *          ------------------------------------------
     C                   IF        FMEOEI <> '0'
     C                             AND FMEOEI <> '1'
     C                             AND FMEOEI <> '2'
     C                             AND FMEOEI <> '3'
     C                             AND FMEOEI <> ' '
     C                   EVAL      MSGID = 'COD0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN79 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPR02 - Maintain Escalation - Process                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPR02        BEGSR
      *
      *          ------------------------------------------
      *          Loop again if fields where changed
      *          ------------------------------------------
     C                   MOVE      *IN40         LOOP02
      *
      *          ------------------------------------------
      *          Delete the file if wanted
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B01
     C                             AND *IN81 = *ON
     C     $SC1K4        DELETE    OMESCL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Do if something changed
      *          ------------------------------------------
     C                   IF        *IN40 = *ON                                  B02
      *
      *          ------------------------------------------
      *          Set screen defaults
      *          ------------------------------------------
     C                   IF        FMPGMN <> *BLANKS
     C                             AND FMLIBC = *BLANKS
     C                   MOVEL(P)  C#LIBL        FMLIBC
     C                   ELSE
     C                   IF        FMPGMN = *BLANKS
     C                   CLEAR                   FMLIBC
     C                   CLEAR                   FMEOEI
     C                   ENDIF
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Check if new record already exists
      *          ------------------------------------------
     C     $SC1K4        CHAIN     OMESCL1
     C                   EVAL      *IN99 = NOT %FOUND
      *          ------------------------------------------
      *          Key fields not changed
      *          ------------------------------------------
     C                   IF        FMHEDC = SCHEDC
     C                             AND FMSUL# = SCSUL#
     C                             AND FMSTAT = SCSTAT
     C                             AND FMPRL# = SCPRL#
     C                   EVAL      *IN99 = *OFF
     C                   ENDIF
      *
     C                   IF        *IN99 = *OFF
     C                             OR *IN80 = *ON
     C     $SC1K5        CHAIN     OMESCL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   EVAL      *IN80 = *OFF
     C                   ELSE
     C                   EVAL      *IN80 = *ON
     C                   ENDIF
      *          ------------------------------------------
      *          Fill the file
      *          ------------------------------------------
     C                   MOVE      FMHEDC        SCHEDC
     C                   MOVE      FMSUL#        SCSUL#
     C                   MOVE      FMSTAT        SCSTAT
     C                   Z-ADD     FMPRL#        SCPRL#
     C                   Z-ADD     FMPRH#        SCPRH#
     C                   Z-ADD     FMDYS#        SCDYS#
     C                   Z-ADD     FMHRS#        SCHRS#
     C                   Z-ADD     FMPIN#        SCPIN#
     C                   MOVEL     FMPGMN        SCPGMN
     C                   MOVEL     FMLIBC        SCLIBC
     C                   MOVEL     FMEOEI        SCEOEI
      *
      *          ------------------------------------------
      *          Update or write record.
      *          *IN99=*OFF record found in chain
      *          *IN99=*ON  record not found in chain
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B03
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMESCR
     C                   ENDIF
     C                   IF        *IN99 = *ON
     C                   EVAL      MSGID = 'FILE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   ENDIF
     C                   ELSE                                                   X03
     C                   IF        *IN99 = *ON
     C                   WRITE     OMESCR
     C                   ENDIF
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If in ADD mode then force CHANGE mode
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B03
     C                             AND *IN81 = *OFF
     C                   EVAL      *IN80 = *ON
     C                   ENDIF                                                  E03
      *
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Clear message queue
      *          ------------------------------------------
     C                   IF        *IN40 = *OFF
     C                   EXSR      CLRMSG
     C                   EVAL      *IN77 = *OFF
     C                   EVAL      *IN78 = *OFF
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * *INZSR - Program initialisation                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Explicit field definitions
      *          ------------------------------------------
     C                   EVAL      RRN01 = *ZEROS
     C                   EVAL      MS = *ZEROS
      *
      *          ------------------------------------------
      *          Indicator definition
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
      *          Savefields
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Indicator definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          API parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
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
      *          Parameter list for OMH906C (Check object)
      *          ------------------------------------------
     C     $X906C        PLIST
     C                   PARM                    P1PGMN
     C                   PARM                    P1LIBC
     C                   PARM      C#PGM         P1OBJT
     C                   PARM                    P1OBJD
     C                   PARM                    P1STAT
      *
      *          ------------------------------------------
      *          OMESCL1 Escalation file
      *          ------------------------------------------
     C     $SC1K1        KLIST
     C                   KFLD                    P#HEDC
      *
     C     $SC1K4        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    FMSUL#
     C                   KFLD                    FMSTAT
     C                   KFLD                    FMPRL#
      *
     C     $SC1K5        KLIST
     C                   KFLD                    SCHEDC
     C                   KFLD                    SCSUL#
     C                   KFLD                    SCSTAT
     C                   KFLD                    SCPRL#
      *
      *          ------------------------------------------
      *          OMSTAL1 Status file
      *          ------------------------------------------
     C     $TA1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    FMSTAT
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
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * *PSSR  - Standard RPG error handler                          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     *PSSR         BEGSR
      *
      *          ------------------------------------------
      *          Do if error not in this routine
      *          ------------------------------------------
     C                   IF        I#PSSR <> *ON                                B01
     C                   EVAL      I#PSSR = *ON
      *
      *          ------------------------------------------
      *          Post information about the last used file
      *          ------------------------------------------
     C                   SELECT                                                 B02
     C                   WHEN      E#FILE = 'OMH016D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMESCL1 '                          X02
     C                   MOVEL     ESCL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMSTAL1 '                          X02
     C                   MOVEL     STAL1         P#FILE
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
      *          Exit
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRTTLR - Total last record processing                        *
      *                                                              *
      * ------------------------------------------------------------ *
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGVAR  -  Get variable message text                         *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGVAR        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      %SUBST(MSGF:1:6) = 'OMHMSG'
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 4                                  B01
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Compile-time arrays                                          *
      * ------------------------------------------------------------ *
**
STA0002 - Status                                1
PIN0002 - Priority increment                    2
LOW0001 - lower limit                           3
UPP0001 - upper limit                           4
