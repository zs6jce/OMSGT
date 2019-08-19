     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ------------------------------------------------------------ *
      * Description                                                  *
      * ------------------------------------------------------------ *
      *                                                              *
      * Program ....: OMH056                                         *
      * Function ...: Status Route Maintenance                       *
      * Author .....: Remain Software                               *
      * Module .....: HelpDesk                                       *
      * Parameters .: P#HEDC    -  HelpDesk Code                     *
      *               P#STAT    -  Status Route                      *
      *               P2STAT    -  Return Status                     *
      *                                                              *
      * This program maintains the Status Action file.               *
      *                                                              *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * File specifications                                          *
      * ------------------------------------------------------------ *
     FOMSRFL1   UF A E           K DISK    INFDS(SRFL1)
     F                                     INFSR(*PSSR)
     FOMSTRL1   IF   E           K DISK    INFDS(STRL1)
     F                                     INFSR(*PSSR)
     FOMH056D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
     F                                     SFILE(OMH05601:RRN01)
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
     D SRFL1           DS            57
     D STRL1           DS            57
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
      *          Helpdesk data structures
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
      *
     D C#EOE0          C                   CONST('*ENTRY')
     D C#EOE1          C                   CONST('*EXIT')
     D C#EOEB          C                   CONST('*BOTH')
      *
     D C#V110          C                   CONST('*V1R1M0 ')
      * Prototype for OMH056
     D OMH056          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#STAT_                             LIKE(P#STAT)
     D P2STAT_                             LIKE(P2STAT)
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
      * Prototype for 'OMH906C'
     D OMH906C         PR                  EXTPGM('OMH906C')
     D P#OBJC_                             LIKE(P#OBJC)
     D P#OBJL_                             LIKE(P#OBJL)
     D P#OBJT_                             LIKE(P#OBJT)
     D P#OBJD_                             LIKE(P#OBJD)
     D P2STAT_                             LIKE(P2STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH029 (Get info)
      * Prototype for 'OMH029'
     D OMH029          PR                  EXTPGM('OMH029')
     D P#OPTI_                             LIKE(P#OPTI)
     D P#HEDC_                             LIKE(P#HEDC)
     D P#SUL#_                             LIKE(P#SUL#)
     D P2STAT_                             LIKE(P2STAT)
     D D#D7HD_                             LIKE(D#D7HD)
     D D#D8HD_                             LIKE(D#D8HD)
      *
      *          ------------------------------------------
      *          OMSTRL1 Status file
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
      *          Parameter list for OMH906C (Check object)
      * *ENTRY Interface for Main Procedure
     D OMH056          PI
     D P#HEDC                         5
     D P#STAT                        10
     D P2STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D k#asq#          S              3P 0
     D w#crc#          S              5P 0
     D w#ix            S              5P 0
     D w#sav#          S              5P 0
     D w#valc          S              5P 0
     D EROR01          S                   LIKE(INDI)
     D EROR02          S                   LIKE(INDI)
     D I#CHGP          S                   LIKE(INDI)
     D I#MSGI          S                   LIKE(INDI)
     D I#PSSR          S              1
     D K#EOEI          S                   LIKE(EOEI)
     D LOOP01          S                   LIKE(INDI)
     D LOOP02          S                   LIKE(INDI)
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#FILE          S             57
     D P#MSTA          S             40
     D P#OBJC          S                   LIKE(OBJC)
     D P#OBJD          S                   LIKE(OBJD)
     D P#OBJL          S                   LIKE(OBJL)
     D P#OBJT          S                   LIKE(OBJT)
     D P#OPTI          S                   LIKE(INDI)
     D P#SUL#          S                   LIKE(SUL#)
     D RRN01           S              5  0
     D S#SEL           S                   LIKE(FMSEL)
     D S#SEPG          S                   LIKE(FMSEPG)
     D S#SEPL          S                   LIKE(FMSEPL)
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
     C                   IF        P2STAT = C#NORM
     C                   EXSR      SRVERW
     C                   ENDIF
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
      * SRSC01 - Work with Status Actions                            *
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
     C                   WRITE     OMH05603
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
     C                   EXFMT     OMH05602
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
      *          If the entry/exit selection is blanks,
      *          then reset it
      *          ------------------------------------------
     C                   IF        FMSEL = *BLANKS                              B04
     C                   MOVE      S#SEL         FMSEL
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          CMD-6=Add status action
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B05
     C                   EXSR      SRSC02
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          Edit
      *          ------------------------------------------
     C                   EXSR      SRED01
     C                   ENDIF                                                  E05
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
      * SRIN01 - Work with Status Actions - Initialization           *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields
      *          ------------------------------------------
     C                   MOVE      *ZEROS        FMPOS
      *
      *          ------------------------------------------
      *          Set the entry/exit selection
      *          ------------------------------------------
     C                   MOVEL(P)  C#EOEB        FMSEL
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
      * SRRF01 - Work with Status Actions - Refresh                  *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRF01        BEGSR
      *
      *          ------------------------------------------
      *          Fill entry/exit selection if not correct
      *          ------------------------------------------
     C                   IF        FMSEL <> C#EOEB                              B01
     C                             AND FMSEL <> C#EOE0
     C                             AND FMSEL <> C#EOE1
     C                   MOVE      S#SEL         FMSEL
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      RRN01 = *ZEROS
      *
      *          ------------------------------------------
      *          Save entry/exit selection
      *          ------------------------------------------
     C                   MOVE      FMSEL         S#SEL
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH05602
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Fill keyfield if selection was made
      *          Indicate if entry/exit should be used in
      *          the key (*IN30 is set on in this case)
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      FMSEL = C#EOE0                               X01
     C                   EVAL      K#EOEI = '0'
     C                   EVAL      *IN30 = *ON
     C                   WHEN      FMSEL = C#EOE1                               X01
     C                   EVAL      K#EOEI = '1'
     C                   EVAL      *IN30 = *ON
     C                   WHEN      FMSEL = C#EOEB                               X01
     C                   EVAL      K#EOEI = *BLANKS
     C                   EVAL      *IN30 = *OFF
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          Read first status action record
      *          ------------------------------------------
     C                   IF        *IN30 = *OFF
     C     $AF1K1        SETLL     OMSRFL1
     C                   ENDIF
     C                   IF        *IN30 = *OFF
     C     $AF1K1        READE     OMSRFL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
     C                   IF        *IN30 = *ON
     C     $AF1K2        SETLL     OMSRFL1
     C                   ENDIF
     C                   IF        *IN30 = *ON
     C     $AF1K2        READE     OMSRFL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
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
     C                   MOVE      RFEOEI        FMEOEI
     C                   SELECT                                                 B02
     C                   WHEN      RFEOEI = '0'                                 X02
     C                   MOVEL(P)  C#EOE0        FMEOED
     C                   WHEN      RFEOEI = '1'                                 X02
     C                   MOVEL(P)  C#EOE1        FMEOED
     C                   ENDSL                                                  E02
     C                   Z-ADD     RFASQ#        FMASQ#
     C                   MOVEL     RFSEPG        FMSEPG
     C                   MOVEL     RFSEPL        FMSEPL
     C                   MOVEL     RFINTL        FMINTL
     C                   ADD       1             RRN01
     C                   WRITE     OMH05601
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C                   IF        *IN30 = *OFF
     C     $AF1K1        READE     OMSRFL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
     C                   IF        *IN30 = *ON
     C     $AF1K2        READE     OMSRFL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
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
      * SRED01 - Work with Status Actions - Edit                     *
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
     C                   READC(E)  OMH05601
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
      *          is still active in Status Actions file.
      *          If not then error, else OK.
      *          ------------------------------------------
     C     $AF1K3        SETLL     OMSRFL1
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
     C                   UPDATE    OMH05601
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   UPDATE    OMH05601
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH05601
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          If there has been no error
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B01
      *
      *          ------------------------------------------
      *          If position-to was filled, or if selection
      *          was changed, then reposition or reselect
      *          ------------------------------------------
     C                   IF        FMPOS <> *ZEROS                              B02
     C                             OR FMSEL <> S#SEL
      *
      *          ------------------------------------------
      *          If new selection was chosen, then refresh
      *          ------------------------------------------
     C                   IF        FMSEL <> S#SEL                               B03
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If position-to was filled, then reposition
      *          ------------------------------------------
     C                   IF        FMPOS <> *ZEROS                              B03
     C                   FOR       W#IX = 1 TO W#SAV#                           B04
     C     W#IX          CHAIN     OMH05601
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        FMASQ# >= FMPOS                              B05
     C                   Z-ADD     W#IX          CRRN01
     C                   EVAL      W#IX = W#SAV#
     C                   MOVE      *ZEROS        FMPOS
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E05
     C                   ENDFOR                                                 E04
      *
     C                   IF        FMPOS <> *ZEROS                              B04
     C                   MOVE      *ZEROS        FMPOS
     C                   Z-ADD     W#SAV#        CRRN01
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Reset the positioning field
      *          ------------------------------------------
     C                   MOVE      *ZEROS        FMPOS
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
      * SRPR01 - Work with Status Actions - Process                  *
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
     C                   READC     OMH05601
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Read subfile until valid option found
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Loop again for options where typed
      *          ------------------------------------------
     C                   EVAL      LOOP01 = *ON
     C                   EVAL      W#CRC# = RRN01
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPTI        W#OPTI
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    OMH05601
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
      *          Exit if CMD-3
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B03
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Leave if CMD-12
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                   LEAVE
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next changed
      *          ------------------------------------------
     C                   READC     OMH05601
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
      * SRSC02 - Maintain Status Action                              *
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
     C                   EXFMT     OMH05604
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
     C                   EVAL      EROR02 = *OFF
     C                   EVAL      LOOP02 = *OFF
      *
      *          ------------------------------------------
      *          Do edit routine if not CMD-12
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B03
     C                   EXSR      SRED02
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
      * SRIN02 - Maintain Status Action - Initialization             *
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
      * SRRF02 - Maintain Status Action - Refresh                    *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRF02        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields if addition
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C                   MOVE      *BLANKS       FMEOEI
     C                   MOVE      *ZEROS        FMASQ#
     C                   MOVE      *BLANKS       FMSEPG
     C                   MOVE      *BLANKS       FMSEPL
     C                   MOVE      C#V110        FMINTL
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Get record else
      *          ------------------------------------------
     C     $AF1K3        CHAIN(N)  OMSRFL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      RFSEPG        FMSEPG
     C                   MOVE      RFSEPL        FMSEPL
     C                   MOVE      RFINTL        FMINTL
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill save fields with current values
      *          ------------------------------------------
     C                   MOVE      FMSEPG        S#SEPG
     C                   MOVE      FMSEPL        S#SEPL
      *
      *          ------------------------------------------
      *          Set indicator 'Change Pending' off
      *          ------------------------------------------
     C                   EVAL      I#CHGP = *OFF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRED02 - Maintain Status Action - Edit                       *
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
      *          Check if the key fields are filled
      *          correctly
      *          ------------------------------------------
     C                   IF        FMEOEI <> '0'                                B02
     C                             AND FMEOEI <> '1'                            B02
     C                             OR FMASQ# = *ZEROS
      *
      *          ------------------------------------------
      *          Check if entry/exit indicator is filled
      *          ------------------------------------------
     C                   IF        FMEOEI <> '0'                                B03
     C                             AND FMEOEI <> '1'                            B03
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     VAR(3)        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'OPTE002'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN70 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Check if sequence number is filled
      *          ------------------------------------------
     C                   IF        FMASQ# = *ZEROS                              B03
     C                   EVAL      MSGDTA = VAR(2)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN71 = *ON
     C                   ENDIF                                                  E03
      *
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Check if the record exists
      *          ------------------------------------------
     C     $AF1K3        CHAIN     OMSRFL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B03
     C                   EVAL      MSGID = 'FILE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN70 = *ON
     C                   EVAL      *IN71 = *ON
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Program name should be filled
      *          ------------------------------------------
     C                   IF        FMSEPG = *BLANKS                             B01
     C                   EVAL      MSGDTA = VAR(4)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN72 = *ON
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Check if program name/library has changed
      *          ------------------------------------------
     C                   IF        FMSEPG <> S#SEPG                             B02
     C                             OR FMSEPL <> S#SEPL
      *
      *          ------------------------------------------
      *          Fill library name if it was left empty
      *          ------------------------------------------
     C                   IF        FMSEPL = *BLANKS                             B03
     C                   MOVEL(P)  '*LIBL'       FMSEPL
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Fill save fields with new values (if
      *          error was not yet found)
      *          ------------------------------------------
     C                   IF        EROR02 = *OFF                                B03
     C                   MOVEL     FMSEPG        S#SEPG
     C                   MOVEL     FMSEPL        S#SEPL
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Check if program exists
      *          ------------------------------------------
     C                   MOVEL     FMSEPG        P#OBJC
     C                   MOVE      FMSEPL        P#OBJL
     C                   EVAL      P#OBJT = *BLANKS
     C                   EVAL      %SUBST(P#OBJT:1:4) = '*PGM'
     C                   CALLP     OMH906C ( P#OBJC : P#OBJL : P#OBJT :
     C                             P#OBJD : P2STAT )
      *
      *          ------------------------------------------
      *          Send warning if not found
      *          ------------------------------------------
     C                   IF        P2STAT <> C#NORM                             B03
     C                   MOVEA     FMSEPG        MC(1)
     C                   MOVEA     FMSEPL        MC(11)
     C                   MOVEA     P#OBJT        MC(21)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'WAR0012'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN72 = *ON
     C                   EVAL      *IN73 = *ON
      *
      *          ------------------------------------------
      *          Set indicator 'Change Pending' on
      *          ------------------------------------------
     C                   EVAL      I#CHGP = *ON
      *
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check interface level
      *          ------------------------------------------
     C                   IF        FMINTL <> C#V110                             B01
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN74 = *ON
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
      * SRPR02 - Maintain Status Action - Process                    *
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
     C     $AF1K3        DELETE    OMSRFL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Do if something changed or change pending
      *          ------------------------------------------
     C                   IF        *IN40 = *ON                                  B02
     C                             OR I#CHGP = *ON
      *
      *          ------------------------------------------
      *          Get the file
      *          ------------------------------------------
     C     $AF1K3        CHAIN     OMSRFL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill the file
      *          ------------------------------------------
     C                   MOVE      FMHEDC        RFHEDC
     C                   MOVE      FMSTAT        RFSTAT
     C                   MOVE      FMEOEI        RFEOEI
     C                   Z-ADD     FMASQ#        RFASQ#
     C                   MOVE      FMSEPG        RFSEPG
     C                   MOVE      FMSEPL        RFSEPL
     C                   MOVE      FMINTL        RFINTL
      *
      *          ------------------------------------------
      *          Update or write record.
      *          *IN99=*OFF record found in chain
      *          *IN99=*ON  record not found in chain
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B03
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMSRFR
     C                   ENDIF
     C                   ELSE                                                   X03
     C                   IF        *IN99 = *ON
     C                   WRITE     OMSRFR
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
      *          Parameter list for OMH906C (Check object)
      *          ------------------------------------------
     C     $X906C        PLIST
     C                   PARM                    P#OBJC
     C                   PARM                    P#OBJL
     C                   PARM                    P#OBJT
     C                   PARM                    P#OBJD
     C                   PARM                    P2STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH029 (Get info)
      *          ------------------------------------------
     C     $MH029        PLIST
     C                   PARM                    P#OPTI
     C                   PARM                    P#HEDC
     C                   PARM                    P#SUL#
     C                   PARM                    P2STAT
     C                   PARM                    D#D7HD
     C                   PARM                    D#D8HD
      *
      *          ------------------------------------------
      *          OMSTRL1 Status file
      *          ------------------------------------------
     C     $TA1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#STAT
      *
      *          ------------------------------------------
      *          OMSRFL1 Status Actions file
      *          ------------------------------------------
     C     $AF1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#STAT
      *
     C     $AF1K2        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#STAT
     C                   KFLD                    K#EOEI
      *
     C     $AF1K3        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#STAT
     C                   KFLD                    FMEOEI
     C                   KFLD                    FMASQ#
      *
      *          ------------------------------------------
      *          Retrieve authorization
      *          ------------------------------------------
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '9'
     C                   EVAL      P2STAT = C#NORM
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P2STAT : D#D7HD : D#D8HD )
      *
      *          ------------------------------------------
      *          Continue if authorisation ok
      *          ------------------------------------------
     C                   IF        P2STAT = C#NORM                              B01
      *
      *          ------------------------------------------
      *          Fill helpdesk fields from parameters
      *          ------------------------------------------
     C                   MOVE      P#HEDC        FMHEDC
     C                   MOVEL     D8HEDD        FMHEDD
      *
      *          ------------------------------------------
      *          Get Status description
      *          ------------------------------------------
     C     $TA1K1        CHAIN     OMSTRR
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill status fields from parameters
      *          ------------------------------------------
     C                   MOVEL     P#STAT        FMSTAT
     C                   MOVEL     TRSTRD        FMSTAD
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
     C                   WHEN      E#FILE = 'OMH056D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMSRFL1 '                          X02
     C                   MOVEL     SRFL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMSTRL1 '                          X02
     C                   MOVEL     STRL1         P#FILE
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
EOE0001 - Entry/exit indicator                  1
SEQ0002 - Sequence number                       2
OPT0006 - '0' or '1'                            3
PGM0006 - Program name                          4
