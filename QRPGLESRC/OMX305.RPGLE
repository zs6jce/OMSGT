     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMX305.                                       *
      * Function ...: Display formatted character string            *
      * Author .....: Remain Software                               *
      * Parameters .: P#QOAI, 1=question, 2=answer, 3=both   I      *
      *               P#QUED, question                       I/O    *
      *               P#ANSD, question                       I/O    *
      *               P#STAT, status                         O      *
      *                                                             *
      * This routine has one or two strings formatted ans displays  *
      * them.                                                       *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators                                             *
      * ----------------------------------------------------------- *
      *                                                             *
      * 40 - Question                                               *
      * 41 - Answer                                                 *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMX305D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
      *COPY QCPYSRC,OMSREFREN
      * ------------------------------------------------------------ *
      *
      *          ------------------------------------------
      *          Screen information data structure
      *          ------------------------------------------
     D INFO            DS
     D  STATUS           *STATUS
     D  D#CRC#               378    379B 0
     D PGM            SDS
     D  E#FILE               201    208
     D  PGMQ             *PROC
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
      *          Workfields
      *          ------------------------------------------
     D                 DS
     D X1                             3  0
      *
      *          ------------------------------------------
      *          Constants
      *          ------------------------------------------
     D C#NORM          C                   CONST('*NORM')
     D C#CNCL          C                   CONST('*CNCL')
     D C#EXIT          C                   CONST('*EXIT')
     D C#TERM          C                   CONST('*TERM')
      * Prototype for OMX305
     D OMX305          PR
     D P#QOAI_                             LIKE(P#QOAI)
     D P#QUES_                             LIKE(P#QUES)
     D P#ANSC_                             LIKE(P#ANSC)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMX306'
     D OMX306          PR                  EXTPGM('OMX306')
     D P#QUAD_                             LIKE(P#QUAD)
     D P#POS#_                             LIKE(P#POS#)
     D P#STAT_                             LIKE(P#STAT)
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
     D OMX305          PI
     D P#QOAI                          N
     D P#QUES                       256
     D P#ANSC                       256
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D I#PSSR          S              1
     D P#FILE          S             57
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#PMQC          S             10
     D P#POS#          S              2  0
     D P#QUAD          S                   LIKE(QUES)
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
      *          Set indicators that control the text in
      *          the screen header
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      P#QOAI = '1'
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *OFF
     C                   WHEN      P#QOAI = '2'
     C                   EVAL      *IN(80) = *OFF
     C                   EVAL      *IN(81) = *ON
     C                   WHEN      P#QOAI = '3'
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *ON
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Process
      *          ------------------------------------------
     C                   IF        P#QOAI = '1'
     C                             OR P#QOAI = '3'
     C                   EVAL      *IN40 = *ON
     C                   EVAL      P#QUAD = P#QUES
     C                   EVAL      P#POS# = 53
     C                   CALLP     OMX306 ( P#QUAD : P#POS# : P#STAT )
     C                   EVAL      P#STAT = C#NORM
     C                   MOVEL     P#QUAD        FMQUES
     C                   ENDIF
     C                   IF        P#QOAI = '2'
     C                             OR P#QOAI = '3'
     C                   EVAL      *IN41 = *ON
     C                   EVAL      P#QUAD = P#ANSC
     C                   EVAL      P#POS# = 53
     C                   CALLP     OMX306 ( P#QUAD : P#POS# : P#STAT )
     C                   EVAL      P#STAT = C#NORM
     C                   MOVEL     P#QUAD        FMANSC
     C                   ENDIF
     C                   IF        P#QOAI = '2'
     C                   MOVEL     FMANSC        FMQUES
     C                   ENDIF
     C                   EXFMT     OMX30501
     C                   IF        *INKC = *ON
     C                   EVAL      P#STAT = C#EXIT
     C                   ENDIF
     C                   IF        *INKL = *ON
     C                   EVAL      P#STAT = C#CNCL
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End
      *          ------------------------------------------
     C                   EXSR      SRTTLR
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
      *          Do if error not in this routine.
      *          ------------------------------------------
     C                   IF        I#PSSR <> *ON                                B01
     C                   EVAL      I#PSSR = *ON
      *
      *          ------------------------------------------
      *          Post information about the last used file
      *          ------------------------------------------
     C                   SELECT                                                 B02
     C                   WHEN      E#FILE = 'OMX305D '                          X02
     C                   MOVEL     INFO          P#FILE
     C                   ENDSL
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
      *          Indicate error
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#TERM
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
      *          Parameter list for OMX306 (Format string)
      *          ------------------------------------------
     C     $MX306        PLIST
     C                   PARM                    P#QUAD
     C                   PARM      53            P#POS#
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
