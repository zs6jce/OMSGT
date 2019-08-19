     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMQRTVCC                                      *
      * Function ...: Retrieve customer call by API                 *
      * Module .....: HelpDesk                                      *
      * Author .....: Remain Software                               *
      *                                                             *
      * Input ......: P#HEDC  -  HelpDesk Code                      *
      *               P#CTN#  -  Incident number                    *
      *                                                             *
      * Output .....: C@CUSC  -  Call track number                  *
      *               C@CCTC  -  Accumulated inc. number            *
      *               C@INDT  -  Incoming date                      *
      *               C@INTM  -  Incoming time                      *
      *               C@RCTM  -  Completion time                    *
      *               C@USID  -  User id                            *
      *               C@CSRC  -  Call source code                   *
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
      * OMCCAL1  -  Call track file                                 *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Files                                                       *
      * ----------------------------------------------------------- *
     FOMCCAL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(CCAL1)
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
     D CCAL1           DS            57
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
     D  C@MSD1                       10    OVERLAY(C@MSGD:6)
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
     D C@CCTC          S                   LIKE(CACCTC)
     D C@CSRC          S                   LIKE(CACSRC)
     D C@CUSC          S                   LIKE(CACUSC)
     D C@INDT          S                   LIKE(CAINDT)
     D C@INTM          S                   LIKE(CAINTM)
     D C@RCTM          S                   LIKE(CARCTM)
     D C@USF1          S                   LIKE(CAUSF1)
     D C@USF2          S                   LIKE(CAUSF2)
     D C@USF3          S                   LIKE(CAUSF3)
     D C@USID          S                   LIKE(CAUSID)
     D FILE            S             57
     D I#PSSR          S              1
     D P#CTN#          S                   LIKE(CACTN#)
     D P#HEDC          S                   LIKE(CAHEDC)
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
     C                   PARM                    P#HEDC
     C                   PARM                    P#CTN#
     C                   PARM                    C@CUSC
     C                   PARM                    C@CCTC
     C                   PARM                    C@INDT
     C                   PARM                    C@INTM
     C                   PARM                    C@RCTM
     C                   PARM                    C@USID
     C                   PARM                    C@CSRC
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
     C                   WHEN      D#PARM <> 14
     C                   EXSR      SRTTLR
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Check if the incident exists
      *          ------------------------------------------
     C     $CA1K1        SETLL     OMCCAL1
     C                   EVAL      *IN99 = %EQUAL
      *
      *                    --------------------------------
      *                    Record not found.
      *                    --------------------------------
     C                   IF        *IN99 = *OFF                                 B01
     C                   EVAL      C@MSID = 'OMQA00B'
      *
     C                   IF        P#HEDC = *BLANKS
     C                   EVAL      %SUBST(C@MSGD:1:2) = '*N'
     C                   ELSE
     C                   MOVEL     P#HEDC        C@MSGD
     C                   ENDIF
      *
     C                   IF        P#CTN# = *BLANKS
     C                   EVAL      %SUBST(C@MSD1:1:2) = '*N'
     C                   ELSE
     C                   MOVEL     P#CTN#        C@MSD1
     C                   ENDIF
      *
     C                   EVAL      C@MSTP = 'E'
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the call
      *          ------------------------------------------
     C                   READ      OMCCAL1
     C                   EVAL      *IN99 = %EOF
      *
      *                    --------------------------------
      *                    Fill the parameters.
      *                    --------------------------------
     C                   MOVE      CACUSC        C@CUSC
     C                   MOVE      CACCTC        C@CCTC
     C                   MOVE      CAINDT        C@INDT
     C                   MOVE      CAINTM        C@INTM
     C                   MOVE      CARCTM        C@RCTM
     C                   MOVE      CAUSID        C@USID
     C                   MOVE      CACSRC        C@CSRC
     C                   MOVE      CAUSF1        C@USF1
     C                   MOVE      CAUSF2        C@USF2
     C                   MOVE      CAUSF3        C@USF3
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
     C                   WHEN      D#PARM <> 14
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
      *          Keylist for the object file
      *          ------------------------------------------
     C     $CA1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#CTN#
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
     C                   IF        E#FILE = 'OMCCAL1 '                          B02
     C                   MOVEL     CCAL1         FILE
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
     C                   IF        D#PARM = 18                                  B01
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
     C                   IF        D#PARM <> 30                                 B01
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
