     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMX929                                        *
      * Function ...: Add incident log entry (helpdesk)             *
      * Author .....: Remain Software                               *
      *                                                             *
      * Input ......: P#HEDC - Help desk code                       *
      *               P#ACTC - Action code                          *
      *               P#INC# - Incident number                      *
      *               P#MSID - Message id                           *
      *               P#MSGD - Message data                         *
      *               P#MSGF - Message file                         *
      *               P#STAT - Status                               *
      *                                                             *
      * This program adds entries to the incident log file. The     *
      * action code '00' is a general action code. Other action     *
      * codes map to the actual OMQCHGIN actions which is           *
      * documented in the OMQCHGIN command.                         *
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
     FOMHIL     O  A E           K DISK    INFSR(*PSSR)
     F                                     INFDS(HIL)
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Input specifications                                        *
      * ----------------------------------------------------------- *
      *COPY QCPYSRC,OMSREFREN
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  D#PGM                  1     10
     D  E#FILE               201    208
     D  D#USID               254    263
     D  D#JOB#               264    269  0
     D HIL             DS            57
      *
      *          ------------------------------------------
      *          Date and time
      *          ------------------------------------------
     D                 DS                  INZ
     D W#DATE                        14  0
     D  ILINTM                        6  0 OVERLAY(W#DATE)
     D  W#DAY#                        2  0 OVERLAY(W#DATE:7)
     D  W#MON#                        2  0 OVERLAY(W#DATE:9)
     D  W#CEN#                        2  0 OVERLAY(W#DATE:11)
     D  W#YEA#                        2  0 OVERLAY(W#DATE:13)
     D ILINDT                         7  0
     D  W1CEN#                        1  0 OVERLAY(ILINDT)
     D  W1YEA#                        2  0 OVERLAY(ILINDT:2)
     D  W1MON#                        2  0 OVERLAY(ILINDT:4)
     D  W1DAY#                        2  0 OVERLAY(ILINDT:6)
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
      *
      *          ------------------------------------------
      *          Named constants for status field
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
      * Prototype for OMX929
     D OMX929          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#INC#_                             LIKE(P#INC#)
     D P#ACTC_                             LIKE(P#ACTC)
     D P#MSID_                             LIKE(P#MSID)
     D P#MSGD_                             LIKE(P#MSGD)
     D P#MSGF_                             LIKE(P#MSGF)
     D P#STAT_                             LIKE(P#STAT)
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
     D OMX929          PI
     D P#HEDC                         5
     D P#INC#                        10
     D P#ACTC                         2
     D P#MSID                         7
     D P#MSGD                     99999C
     D P#MSGF                        10
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D I#PSSR          S              1
     D P#FILE          S             57
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#PMQC          S             10
      *----------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *----------------------------------------------------------------------
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
     C                   RETURN
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRVERW - Processing                                          * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Add to file
      *          ------------------------------------------
     C                   MOVEL     P#HEDC        ILHEDC
     C                   MOVEL     P#INC#        ILINC#
     C                   MOVEL     P#ACTC        ILACTC
     C                   MOVEL     P#MSID        ILMSID
     C                   MOVEL     P#MSGF        ILMSGF
     C                   MOVEL     P#MSGD        ILMSGD
     C                   MOVEL     D#USID        ILUSID
     C                   TIME                    W#DATE
     C                   EVAL      W1DAY# = W#DAY#
     C                   EVAL      W1MON# = W#MON#
     C                   EVAL      W1YEA# = W#YEA#
     C                   IF        W#CEN# = 19
     C                   EVAL      W1CEN# = 0
     C                   ELSE
     C                   IF        W#CEN# = 20
     C                   EVAL      W1CEN# = 1
     C                   ENDIF
     C                   ENDIF
     C                   WRITE(E)  OMHILR
     C                   EVAL      *IN99 = %ERROR
      *
      *          ------------------------------------------
      *          Problems writing.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON
     C                   EVAL      P#STAT = C#TERM
     C                   ELSE
     C                   EVAL      P#STAT = C#NORM
     C                   ENDIF
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
     C                   WHEN      E#FILE = 'OMHIL   '                          X02
     C                   MOVEL     HIL           P#FILE
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
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
