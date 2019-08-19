     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ------------------------------------------------------------- *
      * Description                                                   *
      * ------------------------------------------------------------- *
      *                                                               *
      * Program ....: OMH015                                          *
      * Function ...: Customer call registration                      *
      * Module .....: HelpDesk                                        *
      * Author .....: Remain Software                               *
      * Action .....: Creation.                                       *
      *                                                               *
      * Author .....: Remain Software                               *
      * Action .....: Add contact modifications (I3103)               *
      *                                                               *
      * Author .....: Remain Software                               *
      * Action .....: Upgrade                                         *
      *                                                               *
      * Parameters .: P#ACTC    -  Action code (60=Add, 50=display)  *
      *               P#HEDC    -  HelpDesk Code                      *
      *               P#CTN#    -  Call Track Number                  *
      *               P#CUSC    -  Customer code                      *
      *               P#CCTC    -  Customer contact code              *
      *               P#STAT    -  Return Status                      *
      *                                                               *
      * This program registrates the customer calls                   *
      *                                                               *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * File specifications                                           *
      * ------------------------------------------------------------- *
     FOMCUSL1   IF   E           K DISK    INFDS(CUSL1)
     F                                     INFSR(*PSSR)
     FOMCCOL1   IF   E           K DISK    INFDS(CCOL1)
     F                                     INFSR(*PSSR)
     FOMCCAL1   IF   E           K DISK    INFDS(CCAL1)
     F                                     INFSR(*PSSR)
     FOMH015D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(10)
     D MSD             S              7    DIM(7) CTDATA PERRCD(1)
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
     D CUSL1           DS            57
     D CCOL1           DS            57
     D CCAL1           DS            57
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
     D  D#FILE               201    208
     D  D#USID               254    263
     D  PGMQ             *PROC
      *
      *          ------------------------------------------
      *          Time and date subfields
      *          ------------------------------------------
     D                 DS                  INZ
     D D#TIME                        14  0
     D  FMINTM                        6  0 OVERLAY(D#TIME)
     D  FMINDT                        8  0 OVERLAY(D#TIME:7)
     D   D#DAY#                       2  0 OVERLAY(FMINDT)
     D   D#MON#                       2  0 OVERLAY(FMINDT:3)
     D   D#CEN#                       2  0 OVERLAY(FMINDT:5)
     D   D#YEA#                       2  0 OVERLAY(FMINDT:7)
     D D#TEL#                         6  0
     D                 DS                  INZ
     D D#INDT                         7  0
     D  D#CEN                         1  0 OVERLAY(D#INDT)
     D  D#YEA                         2  0 OVERLAY(D#INDT:2)
     D  D#MON                         2  0 OVERLAY(D#INDT:4)
     D  D#DAY                         2  0 OVERLAY(D#INDT:6)
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
      *          Copy member for OMQCHGCC - Change call track
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQCHGCCDS
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#INTL          C                   CONST('V3R0M0')
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#EXIT          C                   CONST('*EXIT')
     D C#CNCL          C                   CONST('*CNCL')
     D C#SAME          C                   CONST('*SAME')
      * Prototype for OMH015
     D OMH015          PR
     D P#ACTC_                             LIKE(P#ACTC)
     D P#HEDC_                             LIKE(P#HEDC)
     D P#CTN#_                             LIKE(P#CTN#)
     D P#CUSC_                             LIKE(P#CUSC)
     D P#CCTC_                             LIKE(P#CCTC)
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
      * Prototype for 'OMX128'
     D OMX128          PR                  EXTPGM('OMX128')
      * Prototype for 'OMX129'
     D OMX129          PR                  EXTPGM('OMX129')
      * Prototype for 'OMH012'
     D OMH012          PR                  EXTPGM('OMH012')
      * Prototype for 'OMX967'
     D OMX967          PR                  EXTPGM('OMX967')
      * Prototype for 'OMH961'
     D OMH961          PR                  EXTPGM('OMH961')
      * Prototype for 'OMH904C'
     D OMH904C         PR                  EXTPGM('OMH904C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGTXT_                             LIKE(MSGTXT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH905C (Get user profile)
      * Prototype for 'QMHMOVPM'
     D QMHMOVPM        PR                  EXTPGM('QMHMOVPM')
     D P#MSKC_                             LIKE(P#MSKC)
     D P#MSTA_                             LIKE(P#MSTA)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#PMQC_                             LIKE(P#PMQC)
     D P#PSC#_                             LIKE(P#PSC#)
     D P#ERR_                              LIKE(P#ERR)
      * *ENTRY Interface for Main Procedure
     D OMH015          PI
     D P#ACTC                         2
     D P#HEDC                         5
     D P#CTN#                        10
     D P#CUSC                        10
     D P#CCTC                        10
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D EOF             S                   LIKE(INDI)
     D EROR01          S                   LIKE(INDI)
     D EROR02          S                   LIKE(INDI)
     D I#H012          S              1
     D I#PSSR          S              1
     D I#X128          S              1
     D I#X129          S              1
     D I#X961          S              1
     D I#X967          S              1
     D LOOP01          S                   LIKE(INDI)
     D LOOP02          S                   LIKE(INDI)
     D MA_X            S              5  0
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#CDE#          S                   LIKE(CTN#)
     D P#FILE          S             57
     D P#GRCL          S             10
     D P#GRID          S             10
     D P#INKC          S                   LIKE(INDI)
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#NAMD          S                   LIKE(NAMD)
     D P#OPTI          S                   LIKE(INDI)
     D P#PMQC          S             10
     D P#USCL          S             10
     D P#USDC          S             50
     D P#USID          S             10
     D P1STAT          S                   LIKE(STAT)
     D S#CUSC          S                   LIKE(CUSC)
     D S#IN40          S                   LIKE(*IN40)
     D SEL             S                   LIKE(INDI)
     D W#EOFI          S                   LIKE(INDI)
     D W#PROI          S              1
     D W#VALC          S              5  0
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
      *          Entry parameters
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Display the first screen
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B01
     C                   EXSR      SRSC01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Exit
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
     C                   FOR       MA_X = 1 TO %ELEM(MC)
     C                   EVAL      MC(MA_X) = *BLANK
     C                   ENDFOR
     C                   EVAL      MSGDTA = *BLANK
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC01 - Screen 1 processing                                *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC01        BEGSR
      *
      *          ------------------------------------------
      *          Init the display
      *          ------------------------------------------
     C                   EXSR      SRIN01
      *
      *          ------------------------------------------
      *          Do while changes on the screen
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
      *          Display the screen
      *          ------------------------------------------
     C                   EXFMT     OMH01501
     C                   IF        *INKC = *ON
     C                   EVAL      P#STAT = '*EXIT'
     C                   EXSR      SRTTLR
     C                   ENDIF
     C                   IF        *INKL = *ON
     C                   EVAL      P#STAT = '*CNCL'
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
     C                   EVAL      *IN(74) = *OFF
     C                   EVAL      *IN(75) = *OFF
     C                   EVAL      *IN(76) = *OFF
     C                   EVAL      *IN(77) = *OFF
     C                   EVAL      *IN(41) = *OFF
     C                   EVAL      *IN(42) = *OFF
     C                   EVAL      EROR01 = *OFF
     C                   EVAL      LOOP01 = *OFF
      *
      *          ------------------------------------------
      *          F4=List
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   EXSR      SRLIST
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
      *          F6=Add
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B05
     C                   EXSR      SRADDC
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          Do edit routine
      *          ------------------------------------------
     C                   EXSR      SRED01
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
      *          Process
      *          ------------------------------------------
     C                   IF        P#ACTC = '60'                                B02
     C                   EXSR      SRPR01
     C                   ENDIF                                                  E02
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIN01 - Screen 1 init                                      *   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Action
      *          ------------------------------------------
     C                   EVAL      *IN50 = (P#ACTC = '50')
     C                   EVAL      *IN60 = (P#ACTC = '60')
      *
      *          ------------------------------------------
      *          Pretend refresh
      *          ------------------------------------------
     C                   EVAL      *INKE = *ON
      *
      *          ------------------------------------------
      *          Refresh
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
      * SRRF01 - Screen 1 refresh                                   *   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF01        BEGSR
      *
      *          ------------------------------------------
      *          If F5 pressed then clear the fields
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B01
     C                   MOVE      P#CUSC        FMCUSC
     C                   MOVE      P#CCTC        FMCCTC
     C                   MOVE      '1'           FMCSRC
     C                   MOVE      *BLANKS       FMUSF1
     C                   MOVE      *BLANKS       FMUSF2
     C                   MOVE      *ZEROS        FMUSF3
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Reset the display only fields
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMCUNM
     C                   MOVE      *BLANKS       FMCONM
     C                   MOVE      *BLANKS       FMTEL#
     C                   MOVE      D#USID        FMUSID
      *
      *          ------------------------------------------
      *          Get the call
      *          ------------------------------------------
     C     $CA1K1        CHAIN     OMCCAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B01
     C                   MOVE      CAUSID        FMUSID
     C                   MOVE      CACUSC        FMCUSC
     C                   MOVE      CACCTC        FMCCTC
     C                   MOVE      CACSRC        FMCSRC
     C                   MOVE      CAUSF1        FMUSF1
     C                   MOVE      CAUSF2        FMUSF2
     C                   MOVE      CAUSF3        FMUSF3
     C                   Z-ADD     CAINTM        FMINTM
     C                   Z-ADD     CAINDT        D#INDT
     C                   EVAL      D#YEA# = D#YEA
     C                   EVAL      D#MON# = D#MON
     C                   EVAL      D#DAY# = D#DAY
     C                   IF        D#CEN = 1                                    B02
     C                   EVAL      D#CEN# = 20
     C                   ELSE                                                   X02
     C                   EVAL      D#CEN# = 19
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the customer data
      *          ------------------------------------------
     C     FMCUSC        CHAIN     OMCUSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B01
     C                   MOVEL     USNAMD        FMCUNM
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the contact data
      *          ------------------------------------------
     C     $CO1K1        CHAIN     OMCCOL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B01
     C                   MOVEL     CONAMD        FMCONM
     C                   MOVE      COTEL#        FMTEL#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED01 - Edit the screen                                    *   *
      *                                                             *
      * This routine checks if the screen is correctly filled       *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          If changes the re-display
      *          ------------------------------------------
     C                   MOVE      *IN40         EROR01
      *
      *          ------------------------------------------
      *          Check the contact code.
      *          ------------------------------------------
     C                   EXSR      SRCC01
      *
      *          ------------------------------------------
      *          Check the contact code. If the Add Contact indicator
      *          is on then do not check because we are adding a new
      *          contact and this routine is called from SRADDC.
      *          ------------------------------------------
     C                   EVAL      *IN(41) = *ON
     C                   EVAL      *IN(42) = *OFF
     C                   IF        FMCCTC = *BLANKS                             B01
     C                   EXSR      SRDFLT
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    Is this contact code ok?
      *                    --------------------------------
     C     $CO1K1        CHAIN     OMCCOR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B01
     C                   MOVEA     VAR(2)        MC(1)
     C                   MOVEA     FMCCTC        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'EXI0001'
     C                   EVAL      *IN71 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EVAL      *IN(41) = *ON
     C                   EVAL      *IN(42) = *OFF
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check the call source code
      *          ------------------------------------------
     C                   IF        FMCSRC < '0'                                 B01
     C                             OR FMCSRC > '4'
     C                   CLEAR                   MSGDTA
     C                   EVAL      MSGID = 'COD0001'
     C                   EVAL      *IN(41) = *OFF
     C                   EVAL      *IN(42) = *OFF
     C                   EVAL      *IN72 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Refresh the display
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
      * SRCC01 - Check if this is a valid customer                  *   *
      *                                                             *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Check the customer code
      *          ------------------------------------------
     C     SRCC01        BEGSR
      *
     C     FMCUSC        CHAIN     OMCUSR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B01
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     FMCUSC        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'EXI0001'
     C                   EVAL      *IN(41) = *OFF
     C                   EVAL      *IN(42) = *OFF
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR                                                  E01
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRDFLT - Find default customer contact                      *   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRDFLT        BEGSR
      *
      *          ------------------------------------------
      *          Initialize screen fields to blanks
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMCCTC
     C                   MOVE      *BLANKS       FMTEL#
      *
      *          ------------------------------------------
      *          Set pointer and read first record
      *          ------------------------------------------
     C     FMCUSC        SETLL     OMCCOR
     C     FMCUSC        READE     OMCCOR
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 <> *ON                                 B01
      *
      *          ------------------------------------------
      *          If this is the default contact, then fill
      *          screen fields and leave loop
      *          ------------------------------------------
     C                   IF        COCNTI = '1'                                 B02
     C                   MOVEL     COCCTC        FMCCTC
     C                   MOVEL     COTEL#        FMTEL#
     C                   LEAVE
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     FMCUSC        READE     OMCCOR
     C                   EVAL      *IN99 = %EOF
      *
     C                   ENDDO                                                  E01
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
      *          Check position: message if wrong
      *          ------------------------------------------
     C                   IF        FMFLDC <> 'FMCUSC'                           B01
     C                             AND FMFLDC <> 'FMCCTC'                       B05
     C                   EVAL      MSGID = 'MH01501'
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          List of customers
      *          ------------------------------------------
     C                   IF        FMFLDC = 'FMCUSC'                            B02
     C     FMCUSC        CHAIN     OMCUSR
     C                   EVAL      *IN98 = NOT %FOUND
     C                   MOVEL     FMCUSC        P#CUSC
     C                   CALL      'OMX128'      $MX128                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X128 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X128 = *ON
     C                   ENDIF
     C     P#CUSC        CHAIN     OMCUSR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B03
     C                   MOVEL     USCUSC        FMCUSC
     C                   EVAL      *IN(41) = *ON
     C                   EVAL      *IN(42) = *OFF
      *
      *          ------------------------------------------
      *          Find default contact for selected customer
      *          ------------------------------------------
     C                   EXSR      SRDFLT
     C                   IF        FMCCTC <> *BLANKS                            B04
     C                   EVAL      W#PROI = *ON
     C                   EVAL      *IN(41) = *OFF
     C                   EVAL      *IN(42) = *ON
     C                   ENDIF                                                  E04
     C                   ELSE                                                   X03
     C                   MOVE      *BLANKS       FMCUSC
     C                   MOVE      *BLANKS       FMCCTC
     C                   MOVE      *BLANKS       FMTEL#
     C                   ENDIF                                                  E03
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          List of contacts
      *          ------------------------------------------
     C                   IF        FMFLDC = 'FMCCTC'                            B03
     C                   MOVEL     FMCCTC        P#CCTC
     C                   MOVEL     FMCUSC        P#CUSC
      *
     C                   CALL      'OMX129'      $MX129                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X129 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X129 = *ON
     C                   ENDIF
     C                   MOVEL     P#CCTC        FMCCTC
     C                   MOVEL     P#CUSC        FMCUSC
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Refresh
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
      * SRADDC - Add customer call if possible                      *   *
      *                                                             *
      * This routine processes the display.                         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRADDC        BEGSR
      *
      *          ------------------------------------------
      *          Check if this is a vliad contact
      *          ------------------------------------------
     C                   EXSR      SRCC01
      *
      *          ------------------------------------------
      *          Call the ADD CONTACT routine
      *          ------------------------------------------
     C                   IF        EROR01 = *OFF                                B01
     C                   CALL      'OMH012'                               99
     C                   PARM      FMCUSC        P#CUSC
     C                   PARM      '*NEW '       P#CCTC
     C                   PARM                    P#STAT
      *
     C                   IF        *IN99 = *ON
     C                   EVAL      I#H012 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#H012 = *ON
     C                   ENDIF
      *
     C                   IF        P#STAT = C#EXIT                              B02
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
      *
     C                   IF        P#STAT <> C#CNCL                             B02
     C                   EVAL      W#PROI = *ON
     C                   EVAL      *IN(41) = *OFF
     C                   EVAL      *IN(42) = *ON
     C                   MOVEL     P#CCTC        FMCCTC
     C                   ENDIF                                                  E02
      *
     C                   EXSR      SRRF01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR01 - Create a call                                      *   *
      *                                                             *
      * This routine fills the registration record and writes it    *   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          Ask for the next call track number
      *          ------------------------------------------
     C                   CALL      'OMX967'      $MX967                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X967 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X967 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Fill and write
      *          ------------------------------------------
     C                   CLEAR                   C4INTL
     C                   CLEAR                   C4ACTC
     C                   CLEAR                   C4BUF
     C                   CLEAR                   C4PRC
     C                   CLEAR                   C4ERR
     C                   CLEAR                   C4FLD
     C                   MOVEL     FMCCTC        C4CCTC
     C                   MOVEL     P#HEDC        C4HEDC
     C                   IF        FMUSF1 <> *BLANKS                            B01
     C                   MOVEL     FMUSF1        C4USF1
     C                   ELSE                                                   X01
     C                   MOVEL     C#SAME        C4USF1
     C                   ENDIF                                                  E01
     C                   IF        FMUSF2 <> *BLANKS                            B01
     C                   MOVEL     FMUSF2        C4USF2
     C                   ELSE                                                   X01
     C                   MOVEL     C#SAME        C4USF2
     C                   ENDIF                                                  E01
     C                   Z-ADD     FMUSF3        C4USF3
     C                   MOVEL     FMCUSC        C4CUSC
     C                   Z-ADD     FMINTM        C4INTM
     C                   MOVEL     FMUSID        C4USID
     C                   MOVEL     P#CDE#        C4CTN#
     C                   MOVEL     FMCSRC        C4CSRC
     C                   EVAL      C4RCTM = 0
     C                   IF        D#CEN# = 20                                  B02
     C                   EVAL      D#CEN = 1
     C                   ELSE                                                   X02
     C                   EVAL      D#CEN = 0
     C                   ENDIF                                                  E02
     C                   EVAL      D#YEA = D#YEA#
     C                   EVAL      D#MON = D#MON#
     C                   EVAL      D#DAY = D#DAY#
     C                   Z-ADD     D#INDT        C4INDT
     C                   MOVEL     C#INTL        C4INTL
     C                   MOVEL     '60'          C4ACTC
     C                   MOVEL     '011'         C4PRC
     C                   EVAL      P#CTN# = P#CDE#
      *
      *          ------------------------------------------
      *          Call OMQCHGCC
      *          ------------------------------------------
     C                   CALL      'OMQCHGCC'    $CHGCC                   99
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
      *          Savefield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Savefield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      W#VALC = *ZERO
     C                   EVAL      W#PROI = *BLANKS
      *
      *          ------------------------------------------
      *          Keylist for call file
      *          ------------------------------------------
     C     $CA1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#CTN#
      *
      *          ------------------------------------------
      *          Keylist for customer contacts
      *          ------------------------------------------
     C     $CO1K1        KLIST
     C                   KFLD                    FMCUSC
     C                   KFLD                    FMCCTC
      *
      *          ------------------------------------------
      *          Parameter list for OMX128 (get customer)
      *          ------------------------------------------
     C     $MX128        PLIST
     C                   PARM                    P#CUSC
     C                   PARM                    P#NAMD
      *
      *          ------------------------------------------
      *          Parameter list for OMX129 (get contact)
      *          ------------------------------------------
     C     $MX129        PLIST
     C                   PARM                    P#CUSC
     C                   PARM                    P#CCTC
     C                   PARM                    P#NAMD
      *
      *          ------------------------------------------
      *          Parameter list for OMH012 (contact maintenance)
      *          ------------------------------------------
     C     $MH012        PLIST
     C                   PARM                    P#CUSC
     C                   PARM                    P#STAT
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
     C                   PARM                    P#OPTI
      *
      *          ------------------------------------------
      *          Parameter list for OMH905C (Get user profile)
      *          ------------------------------------------
     C     $MX967        PLIST
     C                   PARM                    P#HEDC
     C                   PARM      '2'           P#OPTI
     C                   PARM                    P#CDE#
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMQCHGCC
      *          ------------------------------------------
     C     $CHGCC        PLIST
     C                   PARM                    C4INTL
     C                   PARM                    C4ACTC
     C                   PARM                    C4BUF
     C                   PARM                    C4PRC
     C                   PARM                    C4ERR
     C                   PARM                    C4FLD
      *
      *          ------------------------------------------
      *          Get the date en the time
      *          ------------------------------------------
     C                   TIME                    D#TIME
     C                   IF        D#CEN# = 19                                  B01
     C                   EVAL      C4INDT = 0
     C                   ELSE                                                   X01
     C                   Z-ADD     1000000       C4INDT
     C                   ENDIF                                                  E01
     C     D#YEA#        MULT      10000         D#TEL#
     C                   ADD       D#TEL#        C4INDT
     C                   EVAL      D#TEL# = D#MON# * 100
     C                   ADD       D#TEL#        C4INDT
     C                   ADD       D#DAY#        C4INDT
      *
      *          ------------------------------------------
      *          Get message data texts
      *          ------------------------------------------
     C                   EXSR      SRGVAR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
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
      *          Close programs when needed
      *          ------------------------------------------
     C                   IF        I#X128 = *ON                                 B01
     C                   CALLP     OMX128()
     C                   ENDIF                                                  E01
     C                   IF        I#X129 = *ON                                 B01
     C                   CALLP     OMX129()
     C                   ENDIF                                                  E01
     C                   IF        I#H012 = *ON                                 B01
     C                   CALLP     OMH012()
     C                   ENDIF                                                  E01
     C                   IF        I#X967 = *ON                                 B01
     C                   CALLP     OMX967()
     C                   ENDIF                                                  E01
     C                   IF        I#X961 = *ON                                 B01
     C                   CALLP     OMH961()
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGVAR  -  Get variable message text                         *
      *                                                              *
      * This subroutine gets the data needed for this program to     *
      * retrieve the data wich is filled in message texts.           *
      * It is called one time per program execution. If the system   *
      * choose to do so, it can be paged out. Thats why it is here.  *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGVAR        BEGSR
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 7                                  B01
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
      *
     C                   WHEN      D#FILE = 'OMCUSL1 '                          X02
     C                   MOVEL     CUSL1         P#FILE
      *
     C                   WHEN      D#FILE = 'OMCCOL1 '                          X02
     C                   MOVEL     CCOL1         P#FILE
      *
     C                   ENDSL                                                  E02
      *
      *          ------------------------------------------
      *          Call the error handler
      *          ------------------------------------------
     C                   CALL      'OMH961'                               99
     C                   PARM                    PGM
     C                   PARM                    P#FILE
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X961 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X961 = *ON
     C                   ENDIF
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Compile-time arrays                                          *
      * ------------------------------------------------------------ *
**
HLP0013  -  Customer                          1
HLP0014  -  Contact                           2
HLP0012  -  Telephone                         3
