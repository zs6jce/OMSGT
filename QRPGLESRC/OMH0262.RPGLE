     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH0262                                       *
      * Function ...: Start escalation                              *
      * Author .....: Remain Software                               *
      *                                                             *
      * Input ......: P#HEDC - HelpDesk code                        *
      *                                                             *
      * This program starts the escalation                          *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators.  0=false  1=true                           *
      * ----------------------------------------------------------- *
      *                                                             *
      * 99 - General purpose indicator.                             *
      *                                                             *
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMEPRL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(EPRL1)
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * Array definitions                                           *
      * ----------------------------------------------------------- *
      *
     D TEL             S              1    DIM(4)
     D HLP             S              1    DIM(4)
      *COPY QCPYSRC,OMSREFREN
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Input specifications                                        *
      * ----------------------------------------------------------- *
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
      *
      *          ------------------------------------------
      *          Program status data structure
      *          ------------------------------------------
     D PGM            SDS
     D  D#PGM                  1     10
     D  E#FILE               201    208
     D  D#USID               254    263
     D  D#JOB#               264    269  0
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
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
      *          Counters
      *          ------------------------------------------
     D                 DS                  INZ
     D D#CN1#                         4  0
     D  D#CA1#                        4    OVERLAY(D#CN1#)
     D D#CN2#                         4  0
     D  D#CA2#                        4    OVERLAY(D#CN2#)
     D D#DTA                          8
      *
      *          ------------------------------------------
      *          Command keyfields
      *          ------------------------------------------
     D CMDDS           DS                  INZ
     D  CMD                    1     77
     D                                     DIM(1) CTDATA PERRCD(1)
     D  PRHEDC                34     38
     D  P#STAT                40     44
     D  W#JOBD                53     74
      *
      *          ------------------------------------------
      *          Named constants for status field
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#MSGF          C                   CONST('OMHMSG    *LIBL     ')
      * Prototype for OMH0262
     D OMH0262         PR
     D P#HEDC_                             LIKE(P#HEDC)
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
      *          Parameter list
      * Prototype for 'QCMDEXC'
     D QCMDEXC         PR                  EXTPGM('QCMDEXC')
     D P#CMD_                              LIKE(P#CMD)
     D P#CMD#_                             LIKE(P#CMD#)
      *
      *          ------------------------------------------
      *          End of subroutine
      * Prototype for 'OMH957C'
     D OMH957C         PR                  EXTPGM('OMH957C')
     D P1JOB#_                             LIKE(P1JOB#)
     D P1JOBN_                             LIKE(P1JOBN)
     D P1USID_                             LIKE(P1USID)
     D P1STAT_                             LIKE(P1STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH029 (Get info)
      * Prototype for 'QMHSNDPM'
     D QMHSNDPM        PR                  EXTPGM('QMHSNDPM')
     D MSGID_                              LIKE(MSGID)
     D P#MSGQ_                             LIKE(P#MSGQ)
     D MSGDTA_                             LIKE(MSGDTA)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#MSTC_                             LIKE(P#MSTC)
     D P#PMQC_                             LIKE(P#PMQC)
     D P#PSC#_                             LIKE(P#PSC#)
     D P#MSKC_                             LIKE(P#MSKC)
     D P#ERR_                              LIKE(P#ERR)
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
     D OMH0262         PI
     D P#HEDC                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D I#PSSR          S              1
     D IX              S              2  0
     D MA_X            S              5  0
     D MSGDTA          S            256
     D MSGID           S              7
     D MSGTYP          S             11
     D P#CMD           S             77
     D P#CMD#          S             15  5
     D P#FILE          S             57
     D P#MSGQ          S             20
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#MSTC          S             10
     D P#OPTI          S                   LIKE(INDI)
     D P#PMQC          S             10
     D P#SUL#          S                   LIKE(SUL#)
     D P1JOB#          S              6
     D P1JOBN          S             10
     D P1STAT          S              5
     D P1USID          S             10
     D W#ESF#          S              6
     D X1              S              1  0
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
      *          Process
      *          ------------------------------------------
     C                   EXSR      SRVERW
      *
      *          ------------------------------------------
      *          Return
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRVERW - Processing                                          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Initialization
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
      *
      *          ------------------------------------------
      *          Is *ALL allowed?
      *          ------------------------------------------
     C                   IF        P#HEDC = '*ALL '
     C                   EVAL      D#CN1# = 0
     C                   EVAL      D#CN2# = 0
     C                   MOVE      *LOVAL        PRHEDC
     C                   MOVE      *ZEROS        PRJOB#
     C     PRHEDC        SETLL     OMEPRR
     C                   READ      OMEPRR
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Only if records in property file and no monitor active
      *          ------------------------------------------
     C                   IF        *IN99 = *ON
      *
      *          ------------------------------------------
      *          "Property file is empty"
      *          ------------------------------------------
     C                   EVAL      MSGID = 'MH02604'
     C                   EXSR      SNDMSG
     C                   ELSE
      *
      *          ------------------------------------------
      *          Only submit whwn job is not active or on jobqueue
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      PRJOB# <> 0
     C                   EXSR      SRACTJ
     C                   ENDSL
     C                   IF        PRJOB# = 0
     C                   ADD       1             D#CN1#
     C                   EXSR      SRSUBM
     C                   ELSE
     C                   ADD       1             D#CN2#
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Submit for every valid Helpdesk
      *          ------------------------------------------
     C                   DOW       *IN99 <> *ON
     C                   READ      OMEPRR
     C                   EVAL      *IN99 = %EOF
     C                   IF        *IN99 = *OFF
      *
      *          ------------------------------------------
      *          Only submit when job is not active or on jobqueue
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      PRJOB# <> 0
     C                   EXSR      SRACTJ
     C                   ENDSL
     C                   IF        PRJOB# = 0
     C                   ADD       1             D#CN1#
     C                   EXSR      SRSUBM
     C                   ELSE
     C                   ADD       1             D#CN2#
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
      *
      *          ------------------------------------------
      *          Format the counters
      *          ------------------------------------------
     C                   EXSR      SRCVT
     C                   EVAL      %SUBST(MSGDTA:1:8) = D#DTA
      *
      *          ------------------------------------------
      *          "x submitted, y not submitted"
      *          ------------------------------------------
     C                   EVAL      MSGID = 'MH02605'
     C                   EXSR      SNDMSG
     C                   ENDIF
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Get HelpDesk when required, if P#HEDC is not *ALL
      *          ------------------------------------------
     C                   IF        P#HEDC <> '*ALL '
     C                   IF        P#HEDC = '*CURR'
     C                   EVAL      P#HEDC = *BLANKS
     C                   EVAL      P#OPTI = '3'
     C                   EVAL      P#SUL# = '0'
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD )
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Check whether valid
      *          ------------------------------------------
     C                   IF        P#STAT = '*TERM'
      *
      *          ------------------------------------------
      *          "No HelpDesk selected"
      *          ------------------------------------------
     C                   EVAL      MSGID = 'MH02603'
     C                   EXSR      SNDMSG
     C                   ELSE
     C     P#HEDC        CHAIN     OMEPRR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON
      *
      *          ------------------------------------------
      *          "No properties defined"
      *          ------------------------------------------
     C                   EVAL      MSGID = 'MH02601'
     C                   EXSR      SNDMSG
     C                   ELSE
      *
      *          ------------------------------------------
      *          Only submit whwn job is not active or on jobqueue
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      PRJOB# <> 0
     C                   EXSR      SRACTJ
     C                   ENDSL
     C                   IF        PRJOB# <> 0
      *
      *          ------------------------------------------
      *          "Monitor already active"
      *          ------------------------------------------
     C                   EVAL      MSGID = 'MH02602'
     C                   EXSR      SNDMSG
     C                   ELSE
     C                   EXSR      SRSUBM
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      /EJECT
      * ------------------------------------------------------------ *
      * SRSUBM - Submit job                                          * /
      *                                                              * /
      * This routine submits the job                                 * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSUBM        BEGSR
      *
      *          ------------------------------------------
      *          Continue if allowed
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
     C                   EVAL      W#JOBD = *BLANKS
     C     PRJBDL        CAT       '/':0         W#JOBD
     C                   CAT       PRJOBD:0      W#JOBD
     C                   EVAL      W#JOBD = %TRIMR(W#JOBD) + ')'
     C                   EVAL      P#CMD = CMDDS
     C                   EVAL      P#CMD# = 77
     C                   CALLP     QCMDEXC ( P#CMD : P#CMD# )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      /EJECT
      * ------------------------------------------------------------ *
      * SRACTJ - Check active job                                    * /
      *                                                              * /
      * This routine checks whether the job is really active         * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRACTJ        BEGSR
      *
      *          ------------------------------------------
      *          Call the checking program
      *          ------------------------------------------
     C                   MOVE      PRJOB#        P1JOB#
     C                   MOVEL     PRJOBN        P1JOBN
     C                   MOVEL     PRUSID        P1USID
     C                   EVAL      P1STAT = '*NORM'
     C                   CALLP     OMH957C ( P1JOB# : P1JOBN : P1USID :
     C                             P1STAT )
     C                   IF        P1STAT = '*TERM'
     C                   EVAL      PRJOB# = 0
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      /EJECT
      * ------------------------------------------------------------ *
      * SRCVT - Convert                                             *  /
      *                                                              * /
      * This routine converts numeric to alpha                       * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRCVT         BEGSR
      *
      *          ------------------------------------------
      *          Convert number submitted
      *          ------------------------------------------
     C                   FOR       MA_X = 1 TO %ELEM(HLP)
     C                   EVAL      HLP(MA_X) = *BLANKS
     C                   ENDFOR
     C                   MOVEA     D#CA1#        TEL(1)
     C                   EXSR      SREVAL
     C                   MOVEA     TEL(1)        D#CA1#
     C                   EVAL      %SUBST(D#DTA:1:4) = D#CA1#
      *
      *          ------------------------------------------
      *          Convert number not submitted
      *          ------------------------------------------
     C                   FOR       MA_X = 1 TO %ELEM(HLP)
     C                   EVAL      HLP(MA_X) = *BLANKS
     C                   ENDFOR
     C                   MOVEA     D#CA2#        TEL(1)
     C                   EXSR      SREVAL
     C                   MOVEA     TEL(1)        D#CA2#
     C                   EVAL      %SUBST(D#DTA:5:4) = D#CA2#
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      /EJECT
      * ------------------------------------------------------------ *
      * SREVAL- Evaluate                                            *  /
      *                                                              * /
      * This routine checks preceding zeros                          * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SREVAL        BEGSR
      *
      *          ------------------------------------------
      *          Evaluate
      *          ------------------------------------------
     C                   FOR       X1 = 1 TO 3
     C                   IF        TEL(1) = '0'
     C                   MOVEA     TEL(2)        HLP(1)
     C                   MOVEA     HLP(1)        TEL(1)
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDFOR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
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
      *          Escape or completion
      *          ------------------------------------------
     C                   IF        MSGID = 'MH02605'
     C                             AND TEL(1) = '0'
     C                   EVAL      %SUBST(P#MSTC:1:5) = '*COMP'
     C                   ELSE
     C                   EVAL      %SUBST(P#MSTC:1:7) = '*ESCAPE'
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Call program sender
      *          ------------------------------------------
     C                   EVAL      P#MSGQ = C#MSGF
     C                   EVAL      P#LOB# = 256
     C                   EVAL      P#PMQC = '*'
     C                   EVAL      P#PSC# = 1
     C                   CALLP     QMHSNDPM ( MSGID : P#MSGQ : MSGDTA :
     C                             P#LOB# : P#MSTC : P#PMQC : P#PSC# :
     C                             P#MSKC : P#ERR )
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
     C                   WHEN      E#FILE = 'OMEPRL1 '                          X02
     C                   MOVEL     EPRL1         P#FILE
      *
     C                   ENDSL                                                  E02
      *
      *          ------------------------------------------
      *          Set status
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#TERM
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
      *          Explicit field definitions
      *          ------------------------------------------
     C                   EVAL      W#ESF# = '000000'
     C                   EVAL      IX = *ZEROS
     C                   EVAL      D#CN1# = 0
     C                   EVAL      D#CN2# = 0
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter list for OMH945C (send message)
      *          ------------------------------------------
     C     $X945C        PLIST
     C                   PARM                    MSGID
     C                   PARM                    MSGDTA
     C                   PARM      '*INFO'       MSGTYP
      *
      *          ------------------------------------------
      *          Parameter list for OMH957C (check active)
      *          ------------------------------------------
     C     $X957C        PLIST
     C                   PARM                    P1JOB#
     C                   PARM                    P1JOBN
     C                   PARM                    P1USID
     C                   PARM      '*NORM'       P1STAT
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
      *          Parameter list
      *          ------------------------------------------
     C     $MDEXC        PLIST
     C                   PARM      CMDDS         P#CMD
     C                   PARM      77            P#CMD#
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
**CTDATA CMD
SBMJOB CMD(CALL PGM(OMH026) PARM(xxxxx xxxxx)) JOBD(
