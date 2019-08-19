     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMQRTVCO                                      *
      * Function ...: Retrieve Customer Contact                     *
      * Module .....: HelpDesk                                      *
      * Author .....: Remain Software                               *
      *                                                             *
      * Input ......: P#CUSC  -  Customer                           *
      *               P#CCTC  -  Contact                            *
      *                                                             *
      * Output .....: C@TITL  -  Title                              *
      *               C@NAMD  -  Name                               *
      *               C@TEL#  -  Telephone                          *
      *               C@EMAC  -  Email account                      *
      *               C@USF1  -  User field 1                       *
      *               C@USF2  -  User field 2                       *
      *               C@USF3  -  User field 3                       *
      *               C@ERR   -  Error buffer                       *
      *                                                             *
      * Input ......: C@PRCA  -  Processing array                   *
      *                                                             *
      * This program retrieves customer contact data and returns it *
      * to the calling program. The possible errors are stored      *
      * in the error buffer.                                        *
      *                                                             *
      * The error buffer consists of:                               *
      * 1 -   7  Message i.d.                                       *
      * 8 -   8  Message type                                       *
      * 9 - 136  Messsage data                                      *
      *                                                             *
      * The processing array consists of:                           *
      * 1 - 1  End of processing                                    *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used files                                                  *
      * ----------------------------------------------------------- *
      * OMCCOL1  -  Customer contacts file                          *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Files                                                       *
      * ----------------------------------------------------------- *
     FOMCCOL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     USROPN
     F                                     INFDS(CCOL1)
     FOMCCOL2   IF   E           K DISK    INFSR(*PSSR)
     F                                     USROPN
     F                                     RENAME(OMCCOR:CCOL2R)
     F                                     INFDS(CCOL2)
      * ----------------------------------------------------------- *
      * Input specifications                                        *
      * ----------------------------------------------------------- *
      *COPY QCPYSRC,OMSREFREN
      *
      *          ------------------------------------------
      *          Reference file is used for field definition
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          File information data structures
      *          ------------------------------------------
     D CCOL1           DS            57
     D CCOL2           DS            57
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
     D C@EMAC          S                   LIKE(COEMAC)
     D C@NAMD          S                   LIKE(CONAMD)
     D C@TEL#          S                   LIKE(COTEL#)
     D C@TITL          S                   LIKE(COTITL)
     D C@USF1          S                   LIKE(COUSF1)
     D C@USF2          S                   LIKE(COUSF2)
     D C@USF3          S                   LIKE(COUSF3)
     D FILE            S             57
     D I#COL1          S                   LIKE(INDI)
     D I#COL2          S                   LIKE(INDI)
     D I#PSSR          S              1
     D P#CCTC          S                   LIKE(COCCTC)
     D P#CUSC          S                   LIKE(COCUSC)
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
     C                   PARM                    P#CCTC
      *
     C                   PARM                    C@TITL
     C                   PARM                    C@NAMD
     C                   PARM                    C@TEL#
     C                   PARM                    C@EMAC
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
      *          Open the correct file.
      *          ------------------------------------------
     C                   IF        P#CCTC <> '*DFT'
      *
     C                   IF        I#COL1 <> *ON
     C                   OPEN      OMCCOL1
     C                   EVAL      I#COL1 = *ON
     C                   EVAL      *IN01 = *ON
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   IF        I#COL2 <> *ON
     C                   OPEN      OMCCOL2
     C                   EVAL      I#COL2 = *ON
     C                   MOVE      *ON           COCNTI
     C                   EVAL      *IN02 = *ON
     C                   ENDIF
      *
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Check if the contact exists
      *          ------------------------------------------
     C                   IF        *IN01 = *ON
     C     $CO1K1        SETLL     OMCCOL1
     C                   EVAL      *IN99 = %EQUAL
     C                   ENDIF
     C                   IF        *IN02 = *ON
     C     $CO2K1        SETLL     OMCCOL2
     C                   EVAL      *IN99 = %EQUAL
     C                   ENDIF
      *
      *                    --------------------------------
      *                    Record not found.
      *                    --------------------------------
     C                   IF        *IN99 = *OFF                                 B01
     C                   EVAL      C@MSID = 'OMQA700'
      *
     C                   IF        P#CCTC = *BLANKS
     C                   EVAL      %SUBST(C@MSGD:1:2) = '*N'
     C                   ELSE
     C                   MOVEL     P#CCTC        C@MSGD
     C                   ENDIF
      *
     C                   EVAL      C@MSTP = 'E'
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the call
      *          ------------------------------------------
     C                   IF        *IN01 = *ON
     C                   READ      OMCCOL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
     C                   IF        *IN02 = *ON
     C                   READ      OMCCOL2
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
      *
      *                    --------------------------------
      *                    Fill the parameters.
      *                    --------------------------------
     C                   MOVE      COTITL        C@TITL
     C                   MOVE      CONAMD        C@NAMD
     C                   MOVE      COTEL#        C@TEL#
     C                   MOVE      COEMAC        C@EMAC
     C                   MOVE      COUSF1        C@USF1
     C                   MOVE      COUSF2        C@USF2
     C                   MOVE      COUSF3        C@USF3
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
      *          Indicators
      *          ------------------------------------------
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
      *          Keylist for the object file
      *          ------------------------------------------
     C     $CO1K1        KLIST
     C                   KFLD                    P#CUSC
     C                   KFLD                    P#CCTC
      *
     C     $CO2K1        KLIST
     C                   KFLD                    P#CUSC
     C                   KFLD                    COCNTI
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
     C                   IF        E#FILE = 'OMCCOL1 '                          B02
     C                   MOVEL     CCOL1         FILE
     C                   ENDIF                                                  E02
     C                   IF        E#FILE = 'OMCCOL2 '                          B02
     C                   MOVEL     CCOL2         FILE
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
