     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMQRTVCU                                      *
      * Function ...: Retrieve Customer                             *
      * Module .....: HelpDesk                                      *
      * Author .....: Remain Software                               *
      *                                                             *
      * Input ......: P#CUSC  -  Customer                           *
      *                                                             *
      * Output .....: C@NAMD  -  Name                               *
      *               C@ADST  -  Street                             *
      *               C@ZIPC  -  Zip code                           *
      *               C@ADRE  -  Residence                          *
      *               C@COUD  -  Country                            *
      *               C@USF1  -  User field 1                       *
      *               C@USF2  -  User field 2                       *
      *               C@USF3  -  User field 3                       *
      *               C@ERR   -  Error buffer                       *
      *                                                             *
      * Input ......: C@PRCA  -  Processing array                   *
      *                                                             *
      * This program retrieves call data and returns it             *
      * to the calling program. The possible errors are stored      *
      * in the error buffer.                                        *
      *                                                             *
      * The error buffer consists of:                               *
      * 1 -   7  Messsage i.d.                                      *
      * 8 -   8  Message type                                       *
      * 9 - 136  Messsage data                                      *
      *                                                             *
      * The processing array consists of:                           *
      * 1 - 1  End of processing                                    *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used files                                                  *
      * ----------------------------------------------------------- *
      * OMCUSL1  -  Customer file                                   *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Files                                                       *
      * ----------------------------------------------------------- *
     FOMCUSL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(CUSL1)
      * ----------------------------------------------------------- *
      * Input specifications                                        *
      * ----------------------------------------------------------- *
      *
      *COPY QCPYSRC,OMSREFREN
      *          ------------------------------------------
      *          Reference file is used for field definition
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          File information data structures
      *          ------------------------------------------
     D CUSL1           DS            57
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  E#MSID                40     46
     D  E#FILE               201    208
     D  D#USID               254    263
      *
      *          ------------------------------------------
      *          Parameter: Error buffer
      *          ------------------------------------------
     D C@ERR           DS
     D C@MSID                         7
     D C@MSTP                         1
     D C@MSGD                       128
      *
      *          ------------------------------------------
      *          Parameter: processing array
      *          ------------------------------------------
     D C@PRCA          DS            16
     D C@EOP                          1
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
      * Prototype for 'OMH961'
     D OMH961          PR                  EXTPGM('OMH961')
     D PGM_                                LIKEDS(PGM)
     D FILE_                               LIKE(FILE)
      * Prototype for 'QMHMOVPM'
     D QMHMOVPM        PR                  EXTPGM('QMHMOVPM')
     D P#MSKC_                             LIKE(P#MSKC)
     D P#MSTA_                             LIKE(P#MSTA)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#PMQC_                             LIKE(P#PMQC)
     D P#PSC#_                             LIKE(P#PSC#)
     D P#ERR_                              LIKE(P#ERR)
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D C@ADRE          S                   LIKE(USADRE)
     D C@ADST          S                   LIKE(USADST)
     D C@COUD          S                   LIKE(USCOUD)
     D C@NAMD          S                   LIKE(USNAMD)
     D C@USF1          S                   LIKE(USUSF1)
     D C@USF2          S                   LIKE(USUSF2)
     D C@USF3          S                   LIKE(USUSF3)
     D C@ZIPC          S                   LIKE(USZIPC)
     D FILE            S             57
     D I#PSSR          S              1
     D P#CUSC          S                   LIKE(USCUSC)
     D P#MSTA          S             40
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
      *          Get the parameters.
      *          ------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    P#CUSC
     C                   PARM                    C@NAMD
     C                   PARM                    C@ADST
     C                   PARM                    C@ZIPC
     C                   PARM                    C@ADRE
     C                   PARM                    C@COUD
     C                   PARM                    C@USF1
     C                   PARM                    C@USF2
     C                   PARM                    C@USF3
      *
     C                   PARM                    C@ERR
     C                   PARM                    C@PRCA
      *
      *          ------------------------------------------
      *          Exit if not enough parameters
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      D#PARM <> 11
     C                   EXSR      SRTTLR
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Check if the customer exists
      *          ------------------------------------------
     C     P#CUSC        SETLL     OMCUSL1
     C                   EVAL      *IN99 = %EQUAL
      *
      *                    --------------------------------
      *                    Record not found.
      *                    --------------------------------
     C                   IF        *IN99 = *OFF                                 B01
     C                   EVAL      C@MSID = 'OMQA701'
      *
     C                   IF        P#CUSC = *BLANKS
     C                   EVAL      %SUBST(C@MSGD:1:2) = '*N'
     C                   ELSE
     C                   MOVEL     P#CUSC        C@MSGD
     C                   ENDIF
      *
     C                   EVAL      C@MSTP = 'E'
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the customer
      *          ------------------------------------------
     C                   READ      OMCUSL1
     C                   EVAL      *IN99 = %EOF
      *
      *                    --------------------------------
      *                    Fill the parameters.
      *                    --------------------------------
     C                   MOVE      USNAMD        C@NAMD
     C                   MOVE      USADST        C@ADST
     C                   MOVE      USZIPC        C@ZIPC
     C                   MOVE      USADRE        C@ADRE
     C                   MOVE      USCOUD        C@COUD
     C                   MOVE      USUSF1        C@USF1
     C                   MOVE      USUSF2        C@USF2
     C                   MOVE      USUSF3        C@USF3
      *
      *          ------------------------------------------
      *          No errors.
      *          ------------------------------------------
     C                   EVAL      C@MSTP = *BLANKS
      *
      *          ------------------------------------------
      *          Exit.
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * *INZSR - Program initialisation                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Exit if not enough parameters
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      D#PARM <> 11
     C                   EXSR      SRTTLR
     C                   ENDSL
      *
      *          ------------------------------------------
      *          *ENTRY parameter definitions
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          API parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
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
      * Standard RPG error handler                                  *
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
     C                   IF        E#FILE = 'OMCUSL1 '                          B02
     C                   MOVEL     CUSL1         FILE
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Call the error handler
      *          ------------------------------------------
     C                   CALLP     OMH961 ( PGM : FILE )
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
      *
     C                   EVAL      P#MSKC = *BLANKS
     C                   EVAL      P#LOB# = 4
     C                   EVAL      P#PMQC = '*'
     C                   EVAL      P#PSC# = 1
     C                   CALLP     QMHMOVPM ( P#MSKC : P#MSTA : P#LOB# :
     C                             P#PMQC : P#PSC# : P#ERR )
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If enough parameters, then fill the error buffer.
      *          ------------------------------------------
     C                   IF        D#PARM = 11                                  B01
     C                   EVAL      C@MSID = E#MSID
     C                   EVAL      C@MSTP = 'E'
     C                   EVAL      C@MSGD = *BLANKS
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Exit
      *          ------------------------------------------
     C                   EVAL      D#PARM = *ZEROS
     C                   EXSR      SRTTLR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRTTLR - Close the program                                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          If end of processing.
      *          ------------------------------------------
     C                   IF        D#PARM <> 11                                 B01
     C                             OR C@EOP = *ON
      *
      *          ------------------------------------------
      *          Set close indicator
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Return to caller
      *          ------------------------------------------
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
