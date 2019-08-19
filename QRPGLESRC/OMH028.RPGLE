     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ------------------------------------------------------------ *
      * Description                                                  *
      * ------------------------------------------------------------ *
      *                                                              *
      * Program ....: OMH028                                         *
      * Function ...: Escalation Property Maintenance                *
      * Author .....: Remain Software                               *
      * Module .....: HelpDesk                                       *
      *                                                              *
      * This program maintains the Escalation Property File.         *
      *                                                              *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * File specifications                                          *
      * ------------------------------------------------------------ *
     FOMEPRL1   UF A E           K DISK    INFDS(EPRL1)
     F                                     INFSR(*PSSR)
     FOMHDDL1   IF   E           K DISK    INFDS(HDDL1)
     F                                     INFSR(*PSSR)
     FOMH028D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
     F                                     SFILE(OMH02801:RRN01)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension specifications                                     *
      * ------------------------------------------------------------ *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm    *
      * DOW   Days of week                                           *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(8)
     D MSD             S              7    DIM(8) CTDATA PERRCD(1)
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
     D EPRL1           DS            57
     D HDDL1           DS            57
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
      *          Helpdesk data structures.
      *          ------------------------------------------
      /COPY QCPYLESRC,OMHLPDS
      *
      *          ------------------------------------------
      *          Remap the jobnumber
      *          ------------------------------------------
     D                 DS
     D PRJOB#                         6  0
     D  D#JOB#                        6    OVERLAY(PRJOB#)
      *
      *          ------------------------------------------
      *          Remap days of week to array
      *          ------------------------------------------
     D                 DS
     D  D#DOWA                 1      7
     D  DOW                    1      7
     D                                     DIM(7)
     D  FMSUND                 1      1
     D  FMMOND                 2      2
     D  FMTUED                 3      3
     D  FMWEDD                 4      4
     D  FMTHUD                 5      5
     D  FMFRID                 6      6
     D  FMSATD                 7      7
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
      * Prototype for 'OMH957C'
     D OMH957C         PR                  EXTPGM('OMH957C')
     D P#JOB#_                             LIKE(P#JOB#)
     D P#JOBN_                             LIKE(P#JOBN)
     D P#USID_                             LIKE(P#USID)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Retrieve authorization
      * Prototype for 'OMH909C'
     D OMH909C         PR                  EXTPGM('OMH909C')
     D P#OBJL_                             LIKE(P#OBJL)
     D P#OBJC_                             LIKE(P#OBJC)
     D P#MBRC_                             LIKE(P#MBRC)
     D P#OBJT_                             LIKE(P#OBJT)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH957C (Check job)
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
      *          Parameter list for OMH901C (Send message)
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
      *          Parameter list for OMH909C (Check object)
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D p#job#          S              6P 0
     D w#crc#          S              5P 0
     D w#ix            S              5P 0
     D w#sav#          S              5P 0
     D w#valc          S              5P 0
     D MBRC            S             10
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
     D P#HEDC          S                   LIKE(HEDC)
     D P#HEDD          S                   LIKE(HEDD)
     D P#JOBN          S                   LIKE(JOBN)
     D P#MBRC          S                   LIKE(MBRC)
     D P#MSTA          S             40
     D P#OBJC          S                   LIKE(OBJC)
     D P#OBJL          S                   LIKE(OBJL)
     D P#OBJT          S                   LIKE(OBJT)
     D P#OPTI          S                   LIKE(INDI)
     D P#STAD          S                   LIKE(STAD)
     D P#STAT          S                   LIKE(STAT)
     D P#SUL#          S                   LIKE(SUL#)
     D P#USID          S                   LIKE(USID)
     D RRN01           S              5  0
     D W#OPTI          S                   LIKE(FMOPTI)
     D W#QUAL          S             21
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
      *          Helpdesk?
      *          ------------------------------------------
     C                   SELECT                                                 B01
      *
     C                   WHEN      FMFLDC = 'FMHEDC'                            X01
     C                             AND *IN80 = *OFF
     C                             AND *IN81 = *OFF
     C                   EVAL      EROR02 = *ON
     C                   CALLP     OMX105 ( FMHEDC : HEDD )
      *
      *          ------------------------------------------
      *          Invalid prompt location
      *          ------------------------------------------
     C                   OTHER
     C                   EVAL      MSGID = 'PMT0002'
     C                   EXSR      SNDMSG
     C                   ENDSL
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
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
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC01 - Work with Escalation properties                     *
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
     C                   WRITE     OMH02803
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
     C                   EXFMT     OMH02802
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
      *          CMD-6=Add escalation property
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
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRIN01 - Work with Escalation properties init                *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Clear positioning fields
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMPOS
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
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRF01 - Work with Escalations properties refresh            *
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
     C                   WRITE     OMH02802
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Read first escalation property record
      *          ------------------------------------------
     C     *LOVAL        SETLL     OMEPRL1
     C                   READ      OMEPRL1
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
     C                   MOVE      PRHEDC        FMHEDC
     C     PRSPTM        DIV       100           FMSPTM
     C     PREPTM        DIV       100           FMEPTM
      *
     C                   MOVE      PRDOWA        D#DOWA
      *
     C                   IF        DOW(1) = *ON                                 B02
     C                   EVAL      FMSUND = '1'
     C                   ELSE                                                   X02
     C                   EVAL      FMSUND = *BLANKS
     C                   ENDIF                                                  E02
      *
     C                   IF        DOW(2) = *ON                                 B02
     C                   EVAL      FMMOND = '1'
     C                   ELSE                                                   X02
     C                   EVAL      FMMOND = *BLANKS
     C                   ENDIF                                                  E02
      *
     C                   IF        DOW(3) = *ON                                 B02
     C                   EVAL      FMTUED = '1'
     C                   ELSE                                                   X02
     C                   EVAL      FMTUED = *BLANKS
     C                   ENDIF                                                  E02
      *
     C                   IF        DOW(4) = *ON                                 B02
     C                   EVAL      FMWEDD = '1'
     C                   ELSE                                                   X02
     C                   EVAL      FMWEDD = *BLANKS
     C                   ENDIF                                                  E02
      *
     C                   IF        DOW(5) = *ON                                 B02
     C                   EVAL      FMTHUD = '1'
     C                   ELSE                                                   X02
     C                   EVAL      FMTHUD = *BLANKS
     C                   ENDIF                                                  E02
      *
     C                   IF        DOW(6) = *ON                                 B02
     C                   EVAL      FMFRID = '1'
     C                   ELSE                                                   X02
     C                   EVAL      FMFRID = *BLANKS
     C                   ENDIF                                                  E02
      *
     C                   IF        DOW(7) = *ON                                 B02
     C                   EVAL      FMSATD = '1'
     C                   ELSE                                                   X02
     C                   EVAL      FMSATD = *BLANKS
     C                   ENDIF                                                  E02
      *
     C                   Z-ADD     PRESF#        FMESF#
     C                   MOVE      PRSAMI        FMSAMI
      *
      *          ------------------------------------------
      *          Check active job.
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMMCAI
     C                   IF        PRJOBN <> *BLANKS                            B02
     C                   EVAL      P#JOB# = PRJOB#
     C                   MOVE      PRJOBN        P#JOBN
     C                   MOVE      PRUSID        P#USID
     C                   CALLP     OMH957C ( P#JOB# : P#JOBN : P#USID :
     C                             P#STAT )
     C                   IF        P#STAT = C#NORM                              B03
     C                   MOVE      *ON           FMMCAI
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Write the record in the subfile.
      *          ------------------------------------------
     C                   ADD       1             RRN01
     C                   WRITE     OMH02801
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C                   READ      OMEPRL1
     C                   EVAL      *IN99 = %EOF
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
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRED01 - Work with Escalations properties edit               *
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
     C                   READC(E)  OMH02801
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
      *          is still active in Escalation property file.
      *          If not then error, else OK.
      *          ------------------------------------------
     C     FMHEDC        SETLL     OMEPRL1
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
     C                   UPDATE    OMH02801
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   UPDATE    OMH02801
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH02801
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
     C                   IF        FMPOS <> *BLANKS                             B02
      *
      *          ------------------------------------------
      *          Read through subfile to find valid record
      *          ------------------------------------------
     C                   FOR       W#IX = 1 TO W#SAV#                           B03
     C     W#IX          CHAIN     OMH02801
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        FMHEDC >= FMPOS                              B04
     C                   Z-ADD     W#IX          CRRN01
     C                   EVAL      W#IX = W#SAV#
     C                   MOVE      *BLANKS       FMPOS
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E04
     C                   ENDFOR                                                 E03
      *
      *          ------------------------------------------
      *          If no valid subfile record was found, then
      *          position to last record of subfile
      *          ------------------------------------------
     C                   IF        FMPOS <> *BLANKS                             B03
     C                   MOVE      *BLANKS       FMPOS
     C                   Z-ADD     W#SAV#        CRRN01
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Reset the positioning fields
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMPOS
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
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPR01 - Work with Escalations properties process            *
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
     C                   READC     OMH02801
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
     C                   UPDATE    OMH02801
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
     C                   READC     OMH02801
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
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC02 - Maintain Escalation properties                      *
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
     C                   EXFMT     OMH02804
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
     C                   EVAL      *IN(60) = *OFF
     C                   EVAL      *IN(61) = *OFF
     C                   EVAL      *IN(62) = *OFF
     C                   EVAL      *IN(63) = *OFF
     C                   EVAL      *IN(64) = *OFF
     C                   EVAL      *IN(65) = *OFF
     C                   EVAL      *IN(66) = *OFF
     C                   EVAL      *IN(70) = *OFF
     C                   EVAL      *IN(71) = *OFF
     C                   EVAL      *IN(72) = *OFF
     C                   EVAL      *IN(73) = *OFF
     C                   EVAL      *IN(74) = *OFF
     C                   EVAL      *IN(75) = *OFF
     C                   EVAL      *IN(76) = *OFF
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
      *          F4=Prompt
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B03
     C                   EXSR      SRRF02
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
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRIN02 - Maintain Escalation properties init                 *
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
      *          Get the record if not add.
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B01
     C     FMHEDC        CHAIN(N)  OMEPRL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Compile the jobnumber
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B01
     C                             AND PRJOBN <> *BLANKS
     C                   MOVEL     PRJOBN        FMJOB
     C     FMJOB         CAT       '/':0         FMJOB
     C     FMJOB         CAT       PRUSID:0      FMJOB
     C     FMJOB         CAT       '/':0         FMJOB
     C     FMJOB         CAT       D#JOB#:0      FMJOB
     C                   ELSE                                                   X01
     C                   MOVEL(P)  '*NONE'       FMJOB
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
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRF02 - Maintain Escalation properties refresh              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRF02        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields if addition
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C                   MOVE      *BLANKS       FMHEDC
     C                   MOVE      *BLANKS       FMJOBD
     C                   MOVEL(P)  '*LIBL'       FMJBDL
     C                   MOVE      *ZEROS        FMESF#
     C                   MOVE      *OFF          FMSAMI
     C                   MOVEL(P)  '*NONE'       FMJOB
     C                   EVAL      D#DOWA = '0111110'
     C                   MOVE      *ZEROS        FMSPTM
     C                   MOVE      2400          FMEPTM
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Get record else
      *          ------------------------------------------
     C                   MOVE      PRHEDC        FMHEDC
     C     PRSPTM        DIV       100           FMSPTM
     C     PREPTM        DIV       100           FMEPTM
      *
     C                   IF        DOW(1) = *ON                                 B02
     C                   EVAL      FMSUND = '1'
     C                   ELSE                                                   X02
     C                   EVAL      FMSUND = '0'
     C                   ENDIF                                                  E02
      *
     C                   IF        DOW(2) = *ON                                 B02
     C                   EVAL      FMMOND = '1'
     C                   ELSE                                                   X02
     C                   EVAL      FMMOND = '0'
     C                   ENDIF                                                  E02
      *
     C                   IF        DOW(3) = *ON                                 B02
     C                   EVAL      FMTUED = '1'
     C                   ELSE                                                   X02
     C                   EVAL      FMTUED = '0'
     C                   ENDIF                                                  E02
      *
     C                   IF        DOW(4) = *ON                                 B02
     C                   EVAL      FMWEDD = '1'
     C                   ELSE                                                   X02
     C                   EVAL      FMWEDD = '0'
     C                   ENDIF                                                  E02
      *
     C                   IF        DOW(5) = *ON                                 B02
     C                   EVAL      FMTHUD = '1'
     C                   ELSE                                                   X02
     C                   EVAL      FMTHUD = '0'
     C                   ENDIF                                                  E02
      *
     C                   IF        DOW(6) = *ON                                 B02
     C                   EVAL      FMFRID = '1'
     C                   ELSE                                                   X02
     C                   EVAL      FMFRID = '0'
     C                   ENDIF                                                  E02
      *
     C                   IF        DOW(7) = *ON                                 B02
     C                   EVAL      FMSATD = '1'
     C                   ELSE                                                   X02
     C                   EVAL      FMSATD = '0'
     C                   ENDIF                                                  E02
      *
     C                   Z-ADD     PRESF#        FMESF#
     C                   MOVE      PRSAMI        FMSAMI
     C                   MOVE      PRJOBD        FMJOBD
     C                   MOVE(P)   PRJBDL        FMJBDL
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRED02 - Maintain Escalation properties edit                 *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRED02        BEGSR
      *
      *          ------------------------------------------
      *          Do edit based on action requested.
      *          ------------------------------------------
     C                   IF        *IN81 = *OFF                                 B01
     C                   IF        *IN80 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          The helpdesk must exist.
      *          ------------------------------------------
     C     FMHEDC        SETLL     OMHDDL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B03
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     FMHEDC        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN70 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Check if the record already exists
      *          ------------------------------------------
     C     FMHEDC        CHAIN     OMEPRL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B03
     C                   EVAL      MSGID = 'FILE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN70 = *ON
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Upper limit must be higher than lower
      *          ------------------------------------------
     C                   IF        FMSPTM >= FMEPTM                             B02
     C                   MOVEA     VAR(3)        MC(1)
     C                   MOVEA     VAR(4)        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'EXC0010'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN71 = *ON
     C                   EVAL      *IN72 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Test the days.
      *          ------------------------------------------
     C                   IF        FMSUND <> '1'                                B02
     C                             AND FMSUND <> '0'
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN73 = *ON
     C                   ENDIF                                                  E02
      *
     C                   IF        FMMOND <> '1'                                B02
     C                             AND FMMOND <> '0'
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN74 = *ON
     C                   ENDIF                                                  E02
      *
     C                   IF        FMTUED <> '1'                                B02
     C                             AND FMTUED <> '0'
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN75 = *ON
     C                   ENDIF                                                  E02
      *
     C                   IF        FMWEDD <> '1'                                B02
     C                             AND FMWEDD <> '0'
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN76 = *ON
     C                   ENDIF                                                  E02
      *
     C                   IF        FMTHUD <> '1'                                B02
     C                             AND FMTHUD <> '0'
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN77 = *ON
     C                   ENDIF                                                  E02
      *
     C                   IF        FMFRID <> '1'                                B02
     C                             AND FMFRID <> '0'
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN78 = *ON
     C                   ENDIF                                                  E02
      *
     C                   IF        FMSATD <> '1'                                B02
     C                             AND FMSATD <> '0'
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN79 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Submit the monitor
      *          ------------------------------------------
     C                   IF        FMSAMI <> '1'                                B02
     C                             AND FMSAMI <> '0'
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN60 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Escalation frequency
      *          ------------------------------------------
     C                   IF        FMESF# <= *ZEROS                             B02
     C                   EVAL      MSGDTA = VAR(5)
     C                   EVAL      MSGID = 'ZER0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN61 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Job description
      *          ------------------------------------------
     C                   MOVEL     FMJOBD        P#OBJC
     C                   MOVE      FMJBDL        P#OBJL
     C                   EVAL      %SUBST(P#OBJT:1:5) = '*JOBD'
     C                   EVAL      P#MBRC = *BLANKS
     C                   CALLP     OMH909C ( P#OBJL : P#OBJC : P#MBRC :
     C                             P#OBJT : P#STAT )
     C                   IF        P#STAT <> C#NORM                             B02
     C                   MOVEA     VAR(2)        MC(1)
     C                   MOVEL(P)  FMJBDL        W#QUAL
     C                   EVAL      W#QUAL = %TRIMR(W#QUAL) + '/'
     C     W#QUAL        CAT       FMJOBD:0      W#QUAL
     C                   MOVEA     W#QUAL        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN62 = *ON
     C                   EVAL      *IN63 = *ON
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
      * SRPR02 - Maintain Escalation properities process             *
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
     C     FMHEDC        DELETE    OMEPRL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Do if something changed
      *          ------------------------------------------
     C                   IF        *IN40 = *ON                                  B02
      *
      *          ------------------------------------------
      *          Get the file
      *          ------------------------------------------
     C     FMHEDC        CHAIN     OMEPRL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill the file
      *          ------------------------------------------
     C                   MOVE      FMHEDC        PRHEDC
     C                   MOVE      FMJOBD        PRJOBD
     C                   MOVE      FMJBDL        PRJBDL
     C                   MOVE      FMESF#        PRESF#
     C                   MOVE      FMSAMI        PRSAMI
     C                   IF        *IN99 = *ON
     C                   EVAL      PRJOB# = *ZEROS
     C                   MOVE      *BLANKS       PRJOBN
     C                   MOVE      *BLANKS       PRUSID
     C                   ENDIF
     C                   MOVE      D#DOWA        PRDOWA
     C     100           MULT      FMSPTM        PRSPTM
     C     100           MULT      FMEPTM        PREPTM
      *
      *          ------------------------------------------
      *          Update or write record.
      *          *IN99=*OFF record found in chain
      *          *IN99=*ON  record not found in chain
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B03
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMEPRR
     C                   ENDIF
     C                   ELSE                                                   X03
     C                   IF        *IN99 = *ON
     C                   WRITE     OMEPRR
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
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
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
      *          Parameter list for 1MH029 (Get info)
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
      *          Parameter list for OMH909C (Check object)
      *          ------------------------------------------
     C     $X909C        PLIST
     C                   PARM                    P#OBJL
     C                   PARM                    P#OBJC
     C                   PARM                    P#MBRC
     C                   PARM                    P#OBJT
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH957C (Check job)
      *          ------------------------------------------
     C     $X957C        PLIST
     C                   PARM                    P#JOB#
     C                   PARM                    P#JOBN
     C                   PARM                    P#USID
     C                   PARM                    P#STAT
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
      *          Get message data
      *          ------------------------------------------
     C                   EXSR      SRGVAR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
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
     C                   WHEN      E#FILE = 'OMH028D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMEPRL1 '                          X02
     C                   MOVEL     EPRL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHDDL1 '                          X02
     C                   MOVEL     HDDL1         P#FILE
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
     C                   FOR       MS = 1 TO 5                                  B01
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
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Compile-time arrays                                          *
      * ------------------------------------------------------------ *
**
HLP0001 - helpdesk code                         1
JBD0001 - Job description                       2
TIM0002 - start time                            3
TIM0003 - end time                              4
FRQ0001 - escallation frequency                 5
