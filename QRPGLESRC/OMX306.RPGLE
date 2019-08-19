     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMX306.                                       *
      * Function ...: Format characterstring                        *
      * Author .....: Remain Software                               *
      * Parameters .: P#QUAD, string               I/O              *
      *               P#POS#, end screen position  I                *
      *               P#STAT, status               O                *
      *                                                             *
      * This routine formats a 256 character string so it can be    *
      * displayed. The string and the string position of the first  *
      * "end of screen" are required; only for 24x80; the starting  *
      * position on the screen should not exceed 64.                *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators                                             *
      * ----------------------------------------------------------- *
      *                                                             *
      * 99 - General purpose indicator.                             *
      *                                                             *
      * ------------------------------------------------------------ *
      *                                                              *
      * Q1    Used for wordprocessing                                *
      * Q2    Used for wordprocessing                                *
      * ------------------------------------------------------------ *
     D Q1              S              1    DIM(256)
     D Q2              S              1    DIM(300)
      *COPY QCPYSRC,OMSREFREN
      *
      *          ------------------------------------------
      *          The field reference file is used for field definitions
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
     D PGM            SDS
     D  PGMQ             *PROC
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
      *          Workfields
      *          ------------------------------------------
     D                 DS
     D X1                             3  0
      *
      *          ------------------------------------------
      *          Constants
      *          ------------------------------------------
     D C#NORM          C                   CONST('*NORM')
     D C#TERM          C                   CONST('*TERM')
      * Prototype for OMX306
     D OMX306          PR
     D P#QUAD_                             LIKE(P#QUAD)
     D P#POS#_                             LIKE(P#POS#)
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
     D OMX306          PI
     D P#QUAD                       256
     D P#POS#                         2  0
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D I#PSSR          S              1
     D P#FILE          S             57
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#PMQC          S             10
     D X2              S              3  0
     D X3              S              3  0
     D Y1              S              3  0
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
      *          Assume normal ending
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
      *
      *          ------------------------------------------
      *          Check, shift when needed
      *          ------------------------------------------
     C                   CLEAR                   Q2
     C                   MOVEA     P#QUAD        Q1(1)
     C                   MOVEA     P#QUAD        Q2(1)
      *
      *          ------------------------------------------
      *          first line
      *          ------------------------------------------
     C                   EVAL      X3 = 1
     C                   EXSR      SRSHFT
     C                   MOVEA     Q2(1)         Q1(1)
      *
      *          ------------------------------------------
      *          second line
      *          ------------------------------------------
     C                   EVAL      X3 = X2
     C                   ADD       80            X1
     C                   EXSR      SRSHFT
     C                   MOVEA     Q2(1)         Q1(1)
      *
      *          ------------------------------------------
      *          third line
      *          ------------------------------------------
     C                   EVAL      X3 = X2
     C                   ADD       80            X1
     C                   EXSR      SRSHFT
      *
      *          ------------------------------------------
      *          Return the result
      *          ------------------------------------------
     C                   MOVEA     Q2(1)         P#QUAD
      *
      *          ------------------------------------------
      *          End
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSHFT - Execute the shifting of characters                 *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSHFT        BEGSR
      *
      *          ------------------------------------------
      *          Arrayhandling
      *          ------------------------------------------
     C     X1            ADD       1             X2
     C                   IF        Q1(X1) <> *BLANKS                            B01
     C                             AND Q1(X2) <> *BLANKS
     C                   EVAL      Y1 = X1
     C                   DOW       Y1 > X3                                      B02
     C                             AND Q1(Y1) <> *BLANKS
     C                   EVAL      Q2(Y1) = *BLANKS
     C                   SUB       1             Y1
     C                   ENDDO                                                  E02
     C                   IF        Q1(Y1) = *BLANKS                             B02
     C                   ADD       1             Y1
     C                   MOVEA     Q1(Y1)        Q2(X2)
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
     C                   IF        Q1(X2) = *BLANKS                             B02
     C     X2            ADD       1             X3
     C                   MOVEA     Q1(X3)        Q2(X2)
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
      * SRTTLR - Total last record processing                       *
      *                                                             *
      * This routine sets the last record indicator on.             *
      *                                                             *
      * ----------------------------------------------------------- *
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
      *          Indicate error
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#TERM
      *
      *          ------------------------------------------
      *          Do if error not in this routine.
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
      * *INZSR - Program initialisation                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      X1 = P#POS#
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
