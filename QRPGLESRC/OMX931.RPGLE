     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMX931                                        *
      * Function ...: Calculate time difference escalation date/time*
      *               and current date time in real hours           *
      * Author .....: Remain Software                               *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators.  0=false  1=true                           *
      * ----------------------------------------------------------- *
      *                                                             *
      * 99 - General purpose indicator.                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Array definitions                                           *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Active days array
      *          ------------------------------------------
      *COPY QCPYSRC,OMSREFREN
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Input specifications                                        *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  D#PGM                  1     10
     D  E#FILE               201    208
     D  D#JOBN               244    253
     D  D#USID               254    263
     D  D#JOB#               264    269  0
      *
      *          ------------------------------------------
      *          Numeric workfield definitions
      *          ------------------------------------------
     D                 DS                  INZ
     D  D#DOWA                 1      7
     D  DOW                    1      7
     D                                     DIM(7)
      *
      *          ------------------------------------------
      *          Basic program information:
      *          ------------------------------------------
     D                 DS                  INZ
      *
      *          ------------------------------------------
      *          Hours from first week till end of week
      *          ------------------------------------------
     D D#HTO#                         5  2
      *
      *          ------------------------------------------
      *          Hours passed in last week from the beginning
      *          ------------------------------------------
     D D#HFM#                         5  2
      *
      *          ------------------------------------------
      *          Active hours per week
      *          ------------------------------------------
     D D#HPW#                         5  2
      *
      *          ------------------------------------------
      *          Number of complete weeks between the dates;
      *          -1 means the same week
      *          ------------------------------------------
     D D#WEEK                         5  0
      *
      *          ------------------------------------------
      *          Number of hours
      *          ------------------------------------------
     D D#HOUR                        10  2
      *
      *          ------------------------------------------
      *          Field used for hour counting
      *          ------------------------------------------
     D                 DS                  INZ
     D D#EPTM                         6  0
     D  D#EPMN                        2  0 OVERLAY(D#EPTM:3)
     D D#SPTM                         6  0
     D  D#SPMN                        2  0 OVERLAY(D#SPTM:3)
     D D#HRA#                         4  2
     D D#WRK#                         6  0
     D D#TIM#                         6  0
     D  D#MIN#                        2  0 OVERLAY(D#TIM#:3)
     D D#HLP#                         3  0
      *
      *          ------------------------------------------
      *          Gotten values from calls
      *          ------------------------------------------
     D                 DS                  INZ
     D D#DIF#                         6  0
     D D#TLF#                         1  0
     D D#TLT#                         1  0
      *
      *          ------------------------------------------
      *          The field reference file is used for field definitions
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
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
      * Prototype for OMX931
     D OMX931          PR
     D P#DATF_                             LIKE(P#DATF)
     D P#TIMF_                             LIKE(P#TIMF)
     D P#DATT_                             LIKE(P#DATT)
     D P#TIMT_                             LIKE(P#TIMT)
     D P#SPTM_                             LIKE(P#SPTM)
     D P#EPTM_                             LIKE(P#EPTM)
     D P#DOWA_                             LIKE(P#DOWA)
     D P#HDF#_                             LIKE(P#HDF#)
      * Prototype for 'OMX920C'
     D OMX920C         PR                  EXTPGM('OMX920C')
     D P1DATF_                             LIKE(P1DATF)
     D P1DATT_                             LIKE(P1DATT)
     D P1TEL#_                             LIKE(P1TEL#)
     D P1STAT_                             LIKE(P1STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMX929C (determine daynumber)
      * Prototype for 'OMX929C'
     D OMX929C         PR                  EXTPGM('OMX929C')
     D P2DATT_                             LIKE(P2DATT)
     D P2TEL#_                             LIKE(P2TEL#)
     D P2STAT_                             LIKE(P2STAT)
      *
      *          ------------------------------------------
      *          End of subroutine
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
      * *ENTRY Interface for Main Procedure
     D OMX931          PI
     D P#DATF                         7
     D P#TIMF                         6  0
     D P#DATT                         7
     D P#TIMT                         6  0
     D P#SPTM                         6  0
     D P#EPTM                         6  0
     D P#DOWA                         7
     D P#HDF#                        10  2
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D ix              S              2P 0
     D iy              S              2P 0
     D p1tel#          S              6P 0
     D p2tel#          S              6P 0
     D DO_X            S              7  0
     D DO_X2           S              7  0
     D DTA#            S              7
     D P#FILE          S             57
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#PMQC          S             10
     D P#STAT          S                   LIKE(STAT)
     D P1DATF          S                   LIKE(DTA#)
     D P1DATT          S                   LIKE(DTA#)
     D P1STAT          S                   LIKE(STAT)
     D P2DATT          S                   LIKE(DTA#)
     D P2STAT          S                   LIKE(STAT)
      *----------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *----------------------------------------------------------------------
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      *          «««  mainline  »»»                                 *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Program parameters
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Exit if no parameters passed
      *          ------------------------------------------
     C                   IF        D#PARM = *ZEROS                              B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Process
      *          ------------------------------------------
     C                   EXSR      SRVERW
      *
      *          ------------------------------------------
      *          Return
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRVERW - Processing                                          * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Get the difference in weeks (WEEK)
      *          ------------------------------------------
     C                   EXSR      SRWEEK
      *
      *          ------------------------------------------
      *          Get the number of active hours a week (HPW)
      *          ------------------------------------------
     C                   EXSR      SRHPW
      *
      *          ------------------------------------------
      *          Get the number of hours to go fromtime (HFM)
      *          ------------------------------------------
     C                   EXSR      SRHFM
      *
      *          ------------------------------------------
      *          Get the number of hours passed from totime (FTH)
      *          ------------------------------------------
     C                   EXSR      SRHTO
      *
      *          ------------------------------------------
      *          Calculate the lenght of the total hour gap
      *          ------------------------------------------
     C                   ADD       D#HFM#        D#HTO#
     C                   EVAL      D#HOUR = D#HPW# * D#WEEK
     C     D#HOUR        ADD       D#HTO#        P#HDF#
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRWEEK - Number of weeks                                     * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRWEEK        BEGSR
      *
      *          ------------------------------------------
      *          Get the difference in days
      *          ------------------------------------------
     C                   EVAL      P1DATF = P#DATF
     C                   EVAL      P1DATT = P#DATT
     C                   CALLP     OMX920C ( P1DATF : P1DATT : P1TEL# :
     C                             P1STAT )
     C                   EVAL      D#DIF# = P1TEL#
      *
      *          ------------------------------------------
      *          Get the day from number
      *          ------------------------------------------
     C                   EVAL      P2DATT = P#DATF
     C                   CALLP     OMX929C ( P2DATT : P2TEL# : P2STAT )
     C                   Z-ADD     P2TEL#        D#TLF#
      *
      *          ------------------------------------------
      *          Get the day to number
      *          ------------------------------------------
     C                   EVAL      P2DATT = P#DATT
     C                   CALLP     OMX929C ( P2DATT : P2TEL# : P2STAT )
     C                   Z-ADD     P2TEL#        D#TLT#
      *
      *          ------------------------------------------
      *          Evaluate coded number of weeks
      *          ------------------------------------------
     C     D#DIF#        DIV       7             D#WEEK
     C                   IF        D#TLF# <= D#TLT#
     C                   SUB       1             D#WEEK
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRHPW  - Hours per week                                      * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRHPW         BEGSR
      *
      *          ------------------------------------------
      *          Active hours a day, expressed in hours
      *          with two decimals
      *          ------------------------------------------
     C                   EVAL      D#EPTM = P#EPTM
     C                   EVAL      D#SPTM = P#SPTM
     C                   EVAL      D#HLP# = D#SPMN * 5
     C     D#HLP#        DIV       3             D#SPMN
     C                   EVAL      D#HLP# = D#EPMN * 5
     C     D#HLP#        DIV       3             D#EPMN
     C     D#EPTM        SUB       D#SPTM        D#WRK#
     C     D#WRK#        DIV       10000         D#HRA#
      *
      *          ------------------------------------------
      *          Active hours a week (also *DEC 4 2)
      *          ------------------------------------------
     C                   EVAL      D#DOWA = P#DOWA
     C                   EVAL      IX = 1
     C                   EVAL      IY = 0
     C                   FOR       IX = 1 TO 7
     C                   IF        DOW(IX) = '1'
     C                   ADD       1             IY
     C                   ENDIF
     C                   ENDFOR
     C     D#HRA#        MULT      IY            D#HPW#
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRHFM  - Hours from                                          * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRHFM         BEGSR
      *
      *          ------------------------------------------
      *          Calculate: start, remember: day # from 0
      *          to 6, so add 1 to the counter (IX)
      *          ------------------------------------------
     C                   EVAL      D#HFM# = 0
     C                   EVAL      IX = D#TLT# + 1
     C                   EVAL      IY = IX
      *
      *          ------------------------------------------
      *          Repeat till day # < 1
      *          ------------------------------------------
     C                   FOR       DO_X = 1 TO IX
      *
      *          ------------------------------------------
      *          First time of day checking;
      *          before beginning of range: ignore day
      *          ------------------------------------------
     C                   IF        DOW(IY) = '1'
     C                             AND IY = IX
     C                   SELECT
      *
      *          ------------------------------------------
      *          after ending of range: add all active hours
      *          ------------------------------------------
     C                   WHEN      P#TIMT >= P#EPTM
     C                   ADD       D#HRA#        D#HFM#
      *
      *          ------------------------------------------
      *          in range of active hours: count
      *          ------------------------------------------
     C                   WHEN      P#TIMT > P#SPTM
     C                   EVAL      D#TIM# = P#TIMT
     C                   EVAL      D#HLP# = D#MIN# * 5
     C     D#HLP#        DIV       3             D#MIN#
     C     D#TIM#        SUB       D#SPTM        D#WRK#
     C     D#WRK#        DIV       10000         D#HFM#
     C                   ENDSL
      *
      *          ------------------------------------------
      *          All next times till beginning of week: add or ignore
      *          ------------------------------------------
     C                   ELSE
     C                   IF        DOW(IY) = '1'
     C                   ADD       D#HRA#        D#HFM#
     C                   ENDIF
     C                   ENDIF
     C                   SUB       1             IY
     C                   ENDFOR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRHTO  - Hours to                                            * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRHTO         BEGSR
      *
      *          ------------------------------------------
      *          Calculate: start, remember: day # from 0
      *          to 6, so add 1 to the counter (IX)
      *          ------------------------------------------
     C                   EVAL      D#HTO# = 0
     C                   EVAL      IX = D#TLF# + 1
     C                   EVAL      IY = IX
      *
      *          ------------------------------------------
      *          Repeat till day # > 7
      *          ------------------------------------------
     C                   FOR       DO_X2 = IX TO 7
      *
      *          ------------------------------------------
      *          First time of day checking;
      *          after ending of range: ignore
      *          ------------------------------------------
     C                   IF        DOW(IY) = '1'
     C                             AND IY = IX
     C                   SELECT
      *
      *          ------------------------------------------
      *          before beginning of range: add all active hours
      *          ------------------------------------------
     C                   WHEN      P#TIMF <= P#SPTM
     C                   ADD       D#HRA#        D#HTO#
      *
      *          ------------------------------------------
      *          in range of active hours: count
      *          ------------------------------------------
     C                   WHEN      P#TIMF < P#EPTM
     C                   EVAL      D#TIM# = P#TIMF
     C                   EVAL      D#HLP# = D#MIN# * 5
     C     D#HLP#        DIV       3             D#MIN#
     C     D#EPTM        SUB       D#TIM#        D#WRK#
     C     D#WRK#        DIV       10000         D#HTO#
     C                   ENDSL
      *
      *          ------------------------------------------
      *          All next times till end of week: add or ignore
      *          ------------------------------------------
     C                   ELSE
     C                   IF        DOW(IY) = '1'
     C                   ADD       D#HRA#        D#HTO#
     C                   ENDIF
     C                   ENDIF
     C                   ADD       1             IY
     C                   ENDFOR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      /EJECT
      * ----------------------------------------------------------- *
      * *PSSR  - Error handler                                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *PSSR         BEGSR
      *
      *          ------------------------------------------
      *          Set status
      *          ------------------------------------------
     C                   EVAL      P#STAT = '*TERM'
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
      *          Else exit immediately
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
     C                   RETURN
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
      *          Seton last record, close program
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * *INZSR - Initialization program                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Define fields
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter list for OMX920C (substract dates)
      *          ------------------------------------------
     C     $X920C        PLIST
     C                   PARM                    P1DATF
     C                   PARM                    P1DATT
     C                   PARM                    P1TEL#
     C                   PARM                    P1STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMX929C (determine daynumber)
      *          ------------------------------------------
     C     $X929C        PLIST
     C                   PARM                    P2DATT
     C                   PARM                    P2TEL#
     C                   PARM                    P2STAT
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
