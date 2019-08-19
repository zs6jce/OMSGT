     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ------------------------------------------------------------- *
      * Description                                                   *
      * ------------------------------------------------------------- *
      *                                                               *
      * Program ....: OMH037_2                                        *
      * Function ...: Check distribution                              *
      * Author .....: Remain Software                               *
      * Parameters .: P#DSID - Distribution identifier                *
      *               P#DSPI - Display indicator                      *
      *               P#STAT - Status                                 *
      *                                                               *
      * This program checks if the distribution id is ok. If not      *
      * then the status is set accordingly. If the display indicator  *
      * is set then the error messages are shown in a message subfile *
      * window.                                                       *
      *                                                               *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * File specifications                                           *
      * ------------------------------------------------------------- *
      *                                                               *
      * OMMTI       - Mail template input file  (override to member)  *
      * OMMTO       - Mail template output file (override to member)  *
      * OMH036W3    - Work file with data                             *
      * OMINC       - Incidents                                       *
      * OMTIH       - Incident headers                                *
      * OMTID       - Incident details                                *
      * OMCUS       - Customers                                       *
      * OMCCO       - Customer contacts                               *
      * OMCCA       - Customer calls                                  *
      *                                                               *
      * ------------------------------------------------------------- *
     FOMHMHL1   IF   E           K DISK
     F                                     INFDS(HMHL1)
     FOMHMQL1   IF   E           K DISK
     F                                     INFDS(HMQL1)
     FOMHDDL1   IF   E           K DISK
     F                                     INFDS(HDDL1)
     FOMINCL1   IF   E           K DISK
     F                                     INFDS(INCL1)
     FOMQLQL1   IF   E           K DISK
     F                                     INFDS(QLQL1)
     FOMQLAL1   IF   E           K DISK
     F                                     INFDS(QLAL1)
     FOMCUSL1   IF   E           K DISK
     F                                     INFDS(CUSL1)
     FOMSREL1   IF   E           K DISK
     F                                     INFDS(SREL1)
     FOMCCOL1   IF   E           K DISK
     F                                     INFDS(CCOL1)
     FOMH037D   CF   E             WORKSTN INFDS(INFO)
     F                                     USROPN
     F                                     INFSR(*PSSR)
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Extension specifications                                      *
      * ------------------------------------------------------------- *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(20)
     D MSD             S              7    DIM(20) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
      *COPY QCPYSRC,OMSREFREN
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Input specifications                                          *
      * ------------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Field reference file used to define fields
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          File information data structure
      *          ------------------------------------------
     D HMHL1           DS            57
     D HMQL1           DS            57
     D HDDL1           DS            57
     D INCL1           DS            57
     D CUSL1           DS            57
     D CCOL1           DS            57
     D SREL1           DS            57
     D QLQL1           DS            57
     D QLAL1           DS            57
     D INFO            DS            57
      *
      *          ------------------------------------------
      *          W3 file
      *          ------------------------------------------
     D P#W3          E DS                  EXTNAME(OMH036W3)
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  D#PARM           *PARMS
     D  PGMQ             *PROC
      *
      *          ------------------------------------------
      *          Binary number for message
      *          ------------------------------------------
     D D#BIN           DS
     D D#ANS#                         9B 0
     D D#QLS#                         9B 0
     D D#QSQ#                         9B 0
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
     D C#NEW           C                   CONST('*NEW ')
      * Prototype for OMH037_2
     D OMH037_2        PR
     D P#DSID_                             LIKE(P#DSID)
     D P#DSPI_                             LIKE(P#DSPI)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH901C'
     D OMH901C         PR                  EXTPGM('OMH901C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGDTA_                             LIKE(MSGDTA)
      *
      *          ------------------------------------------
      *          OMINCL1 - Incident file
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
      *          Close if no parameters passed.
      * *ENTRY Interface for Main Procedure
     D OMH037_2        PI
     D P#DSID                        20
     D P#DSPI                         1
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D EROR01          S                   LIKE(INDI)
     D I#PSSR          S              1
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#FILE          S             57
     D P#MSTA          S             40
      *----------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *----------------------------------------------------------------------
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Main line                                                   *
      * ------------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Entry parameters
      *          ------------------------------------------
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
      *          Close if no parameters passed.
      *          ------------------------------------------
     C                   IF        D#PARM = *ZEROS                              B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do the processing routine
      *          ------------------------------------------
     C                   EXSR      SRVERW
      *
      *          ------------------------------------------
      *          If error
      *          ------------------------------------------
     C                   IF        EROR01 = '1'                                 B01
     C                   EVAL      P#STAT = C#TERM
     C                   ELSE                                                   X01
     C                   EVAL      P#STAT = C#NORM
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If display
      *          ------------------------------------------
     C                   IF        P#DSPI = *ON                                 B01
     C                   OPEN      OMH037D
     C                   MOVE      P#DSID        FMDSID
     C                   MOVE      MHCUSC        FMCUSC
     C                   MOVE      MHCCTC        FMCCTC
     C                   EXFMT     MSGCTL
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Return to caller.
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
     C                   RETURN
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
     C                   IF        P#DSPI = *ON
     C                   EVAL      MSGF = 'OMHMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C  ( MSGID : MSGF : MSGLIB :
     C                             MSGDTA )
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRVERW - Processing                                           *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Check the header
      *          ------------------------------------------
     C     P#DSID        CHAIN     OMHMHL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *                    --------------------------------
      *                    Does the helpdesk exist
      *                    --------------------------------
     C     MHHEDC        SETLL     OMHDDL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 <> *ON                                 B01
     C                   EVAL      MSGID = 'HLPE002'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = '1'
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    Does the short description exist
      *                    --------------------------------
     C                   IF        MHSHID = *BLANKS                             B01
     C                   MOVEA     VAR(3)        MC(1)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = '1'
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    Does the incident exist
      *                    --------------------------------
     C                   IF        MHINC# <> *BLANKS                            B01
     C                             AND MHINC# <> '*GEN'
     C     $NC1K1        SETLL     OMINCL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 <> *ON                                 B02
     C                   EVAL      MSGID = 'INC0006'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = '1'
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    Does the customer exist
      *                    --------------------------------
     C     MHCUSC        SETLL     OMCUSL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 <> *ON                                 B01
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     MHCUSC        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = '1'
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    Does the contact exist
      *                    --------------------------------
     C     $CO1K1        SETLL     OMCCOL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 <> *ON                                 B01
     C                   MOVEA     VAR(2)        MC(1)
     C                   MOVEA     MHCCTC        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = '1'
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    Does the status route exist
      *                    --------------------------------
     C                   IF        MHSTRC <> *BLANKS                            B01
     C     $RE1K1        SETLL     OMSREL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 <> *ON                                 B02
     C                   MOVEA     VAR(4)        MC(1)
     C                   MOVEA     MHSTRC        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'EXI0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = '1'
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Does the status route entry exist
      *                    --------------------------------
     C     $RE1K2        SETLL     OMSREL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 <> *ON                                 B02
     C                   EVAL      MSGID = 'SEQE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = '1'
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    Is this a correct call source code?
      *                    --------------------------------
     C                   IF        MHCSRC < '0'
     C                             OR MHCSRC > '4'
     C                   MOVEA     MHCSRC        MC(1)
     C                   MOVEA     VAR(5)        MC(11)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'VAL0005'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = '1'
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read the question and answers
      *          ------------------------------------------
     C     P#DSID        CHAIN     OMHMQL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Q&A must exist for a new incident.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B01
     C                             AND MHINC# = *BLANKS
     C                   EVAL      MSGID = 'QUE0005'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = '1'
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          It exists, check if it is ok.
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *                    --------------------------------
      *                    Do the question and answer exist
      *                    --------------------------------
     C     $LQ1K1        CHAIN     OMQLQL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B03
     C     $LA1K1        SETLL     OMQLAL1
     C                   EVAL      *IN99 = %EQUAL
     C                   ELSE                                                   X03
     C                   EVAL      *IN99 = *OFF
     C                   ENDIF                                                  E03
      *
     C                   IF        *IN99 <> *ON                                 B03
     C                   Z-ADD     MQANS#        D#ANS#
     C                   Z-ADD     MQQLS#        D#QLS#
     C                   Z-ADD     MQQSQ#        D#QSQ#
     C                   MOVEA     D#BIN         MC(1)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'QUE0004'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = '1'
     C                   ENDIF                                                  E03
      *
     C     P#DSID        READE     OMHMQL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRTTLR - Exit processing                                      *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          Close if not enough parameters
      *          ------------------------------------------
     C                   IF        D#PARM <> 5                                  B01
     C                   EVAL      *INLR = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Return
      *          ------------------------------------------
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * *PSSR  - Error subroutine                                     *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
     C     *PSSR         BEGSR
      *
      *          ------------------------------------------
      *          Do if error not in this routine
      *          ------------------------------------------
     C                   IF        I#PSSR <> *ON                                B01
     C                   EVAL      I#PSSR = *ON
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
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * *INZSR - Program initialization                               *
      *                                                               *
      * ------------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Explicit numeric field definitions
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Other Workfields
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Indicators
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter definitions
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Save field definitions
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          API parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter lists
      *          ------------------------------------------
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
      *          OMINCL1 - Incident file
      *          ------------------------------------------
     C     $NC1K1        KLIST
     C                   KFLD                    MHHEDC
     C                   KFLD                    MHINC#
      *
      *          ------------------------------------------
      *          OMCCOL1 - Contact file
      *          ------------------------------------------
     C     $CO1K1        KLIST
     C                   KFLD                    MHCUSC
     C                   KFLD                    MHCCTC
      *
      *          ------------------------------------------
      *          OMQLQL1 - Question list questions
      *          ------------------------------------------
     C     $LQ1K1        KLIST
     C                   KFLD                    MHHEDC
     C                   KFLD                    MQQLS#
     C                   KFLD                    MQQSQ#
      *
      *          ------------------------------------------
      *          OMSREL1 - Status route entries
      *          ------------------------------------------
     C     $RE1K1        KLIST
     C                   KFLD                    MHHEDC
     C                   KFLD                    MHSTRC
      *
     C     $RE1K2        KLIST
     C                   KFLD                    MHHEDC
     C                   KFLD                    MHSTRC
     C                   KFLD                    MHRES#
      *
      *          ------------------------------------------
      *          OMQLAL1 - question list answers file
      *          ------------------------------------------
     C     $LA1K1        KLIST
     C                   KFLD                    MHHEDC
     C                   KFLD                    MQQLS#
     C                   KFLD                    MQQSQ#
     C                   KFLD                    LQQUE#
     C                   KFLD                    MQANS#
      *
      *          ------------------------------------------
      *          Get messages
      *          ------------------------------------------
     C                   EXSR      SRGVAR
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
      *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Compile time arrays                                           *
      * ------------------------------------------------------------- *
**
CUS0001      - customer         1
CUS0002      - contact          2
DSC0001      - description      3
STR0001      - status route     4
CSC0001      - call source code 5
