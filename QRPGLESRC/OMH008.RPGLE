     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH008.                                       *
      * Function ...: System Select Question                        *
      * Author .....: Remain Software                               *
      * Parameters .: P#HEDC - Helpdesk code     I                  *
      *               P#QLS# - Question list     I/O                *
      *               P#SEQ# - Sequence number   I/O                *
      *               P#QUE# - Question number   I/O                *
      *               P#ANS# - Answer number     I                  *
      *               P#STAT - Status            O                  *
      *                                                             *
      * This program passes the next question to the caller.        *
      * If the database is corrupt, status *TERM is passed.         *
      * If the question number is zero, the program assumes that    *
      * the first question should be passed.                        *
      *                                                             *
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMQLQL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(QLQL1)
     FOMQLAL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(QLAL1)
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Array specifications                                        *
      * ----------------------------------------------------------- *
      *
      *COPY QCPYSRC,OMSREFREN
      *
      * ----------------------------------------------------------- *
      * ----------------------------------------------------------- *
      * Input specifications                                        *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          The field reference file is used for field definitions
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          File information data structures in case of error.
      *          ------------------------------------------
     D QLQL1           DS            57
     D QLAL1           DS            57
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  E#FILE               201    208
     D  D#QUE#               254    258  0
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
      * Prototype for OMH008
     D OMH008          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#QLS#_                             LIKE(P#QLS#)
     D P#SEQ#_                             LIKE(P#SEQ#)
     D P#QUE#_                             LIKE(P#QUE#)
     D P#ANS#_                             LIKE(P#ANS#)
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
     D OMH008          PI
     D P#HEDC                         5
     D P#QLS#                         5  0
     D P#SEQ#                         3  0
     D P#QUE#                         5  0
     D P#ANS#                         3  0
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
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Main line                                                   *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Program parameters
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Do the processing routine
      *          ------------------------------------------
     C                   EXSR      SRVERW
      *
      *          ------------------------------------------
      *          Do the total last record routine
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRVERW - Main program processing routine                    *
      *                                                             *
      * In this routine all the processing takes place              *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          If first question: lowest level
      *          ------------------------------------------
     C                   IF        P#QUE# = *ZEROS                              B01
      *
      *          ------------------------------------------
      *          If QLS# is *ZEROS, then find first ques-
      *          tion in first question list
      *          If QLS# is not *ZEROS, then find first
      *          question in the particular list
      *          ------------------------------------------
     C                   IF        P#QLS# = *ZEROS                              B02
     C     $LQ1K1        SETLL     OMQLQR
     C     $LQ1K1        READE     OMQLQR
     C                   EVAL      *IN99 = %EOF
     C                   ELSE                                                   X02
     C     $LQ1K2        SETLL     OMQLQR
     C     $LQ1K2        READE     OMQLQR
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill parameters depending on result of
      *          read operation
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      P#STAT = '*NOTF'
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X02
     C                   Z-ADD     LQQUE#        P#QUE#
     C                   Z-ADD     LQQSQ#        P#SEQ#
     C                   Z-ADD     LQQLS#        P#QLS#
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          If not first question get question list
      *          and sequence number
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C     $LA1K1        CHAIN     OMQLAR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      P#STAT = '*NOTF'
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          No more questions
      *          ------------------------------------------
     C                   IF        LAGLS# = 0                                   B03
     C                             AND LAGQS# = 0
     C                   EVAL      P#STAT = '*STOP'
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Fill parameters
      *          ------------------------------------------
     C                   Z-ADD     LAGLS#        P#QLS#
     C                   Z-ADD     LAGQS#        P#SEQ#
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Get question number
      *          ------------------------------------------
     C     $LQ1K3        CHAIN     OMQLQR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      P#STAT = '*NOTF'
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X02
     C                   Z-ADD     LQQUE#        P#QUE#
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
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          OMQLQL1 Question List Question File
      *          ------------------------------------------
     C     $LQ1K1        KLIST
     C                   KFLD                    P#HEDC
      *
     C     $LQ1K2        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#QLS#
      *
     C     $LQ1K3        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#QLS#
     C                   KFLD                    P#SEQ#
      *
      *          ------------------------------------------
      *          OMQLAL1 Answer File
      *          ------------------------------------------
     C     $LA1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#QLS#
     C                   KFLD                    P#SEQ#
     C                   KFLD                    P#QUE#
     C                   KFLD                    P#ANS#
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
     C                   WHEN      E#FILE = 'OMQLQL1 '                          X02
     C                   MOVEL     QLQL1         P#FILE
     C                   WHEN      E#FILE = 'OMQLAL1 '                          X02
     C                   MOVEL     QLAL1         P#FILE
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
     C                   EVAL      P#STAT = '*TERM'
     C                   EVAL      D#PARM = 0
     C                   EXSR      SRTTLR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
