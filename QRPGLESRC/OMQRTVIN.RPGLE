     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ------------------------------------------------------------ *
      * Description                                                  *
      * ------------------------------------------------------------ *
      *                                                              *
      * Program ....: OMQRTVIN                                       *
      * Function ...: Retrieve Incident                              *
      * Module .....: HelpDesk                                       *
      * Author .....: Remain Software                               *
      *                                                              *
      * Input ......: K#HEDC  -  HelpDesk Code                       *
      *               K#INC#  -  Incident number                     *
      *                                                              *
      * Output .....: I@CTN#  -  Call track number                   *
      *               I@AIN#  -  Accumulated inc. number             *
      *               I@INDT  -  Incoming date                       *
      *               I@INTM  -  Incoming time                       *
      *               I@RCDT  -  Completion date                     *
      *               I@RCTM  -  Completion time                     *
      *               I@STRC  -  Status route code                   *
      *               I@RES#  -  Route entry sequence                *
      *               I@SUL#  -  Support level                       *
      *               I@STAT  -  Status                              *
      *               I@SRDT  -  Status date                         *
      *               I@SRTM  -  Status time                         *
      *               I@SEV1  -  Severity 1                          *
      *               I@SEV2  -  Severity 2                          *
      *               I@SEV3  -  Severity 3                          *
      *               I@PRI#  -  Priority                            *
      *               I@ESC#  -  Escalation counter                  *
      *               I@IEDT  -  Escalation date                     *
      *               I@IETM  -  Escalation time                     *
      *               I@RUID  -  Registered by user                  *
      *               I@AUID  -  Allocated by user                   *
      *               I@ITPC  -  Type code                           *
      *               I@CFL#  -  Configuration list number           *
      *               I@CFIC  -  Configuration item                  *
      *               I@USF1  -  User field 1                        *
      *               I@USF2  -  User field 2                        *
      *               I@USF3  -  User field 3                        *
      *               I@SHID  -  Short description                   *
      *               I@ERR   -  Error buffer                        *
      *                                                              *
      * Input ......: I@PRCA  -  Processing array                    *
      *                                                              *
      * This program retrieves incident data and returns it          *
      * to the calling program. The possible errors are stored       *
      * in the error buffer.                                         *
      *                                                              *
      * The error buffer consists of:                                *
      * 1 -   7  Messsage i.d.                                       *
      * 8 -   8  Message type                                        *
      * 9 - 136  Messsage data                                       *
      *                                                              *
      * The processing array consists of:                            *
      * 1 - 1  End of processing.                                    *
      *                                                              *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Used files                                                   *
      * ------------------------------------------------------------ *
      * OMINCL1  -  Incident file                                    *
      *                                                              *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Files                                                        *
      * ------------------------------------------------------------ *
     FOMINCL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(INCL1)
      * ------------------------------------------------------------ *
      * Input specifications                                         *
      * ------------------------------------------------------------ *
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
     D INCL1           DS            57
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
     D I@ERR           DS
     D I@MSID                         7
     D I@MSTP                         1
     D I@MSGD                       128
     D  I@MSD1                       10    OVERLAY(I@MSGD:6)
      *
      *          ------------------------------------------
      *          Parameter: processing array
      *          ------------------------------------------
     D I@PRCA          DS            16
     D I@EOP                          1
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
     D FILE            S             57
     D I#PSSR          S              1
     D I@AIN#          S                   LIKE(NCAIN#)
     D I@AUID          S                   LIKE(NCAUID)
     D I@CFIC          S                   LIKE(NCCFIC)
     D I@CFL#          S                   LIKE(NCCFL#)
     D I@CTN#          S                   LIKE(NCCTN#)
     D I@ESC#          S                   LIKE(NCESC#)
     D I@IEDT          S                   LIKE(NCIEDT)
     D I@IETM          S                   LIKE(NCIETM)
     D I@INDT          S                   LIKE(NCINDT)
     D I@INTM          S                   LIKE(NCINTM)
     D I@ITPC          S                   LIKE(NCITPC)
     D I@PRI#          S                   LIKE(NCPRI#)
     D I@RCDT          S                   LIKE(NCRCDT)
     D I@RCTM          S                   LIKE(NCRCTM)
     D I@RES#          S                   LIKE(NCRES#)
     D I@RUID          S                   LIKE(NCRUID)
     D I@SEV1          S                   LIKE(NCSEV1)
     D I@SEV2          S                   LIKE(NCSEV2)
     D I@SEV3          S                   LIKE(NCSEV3)
     D I@SHID          S                   LIKE(NCSHID)
     D I@SRDT          S                   LIKE(NCSRDT)
     D I@SRTM          S                   LIKE(NCSRTM)
     D I@STAT          S                   LIKE(NCSTAT)
     D I@STRC          S                   LIKE(NCSTRC)
     D I@SUL#          S                   LIKE(NCSUL#)
     D I@USF1          S                   LIKE(NCUSF1)
     D I@USF2          S                   LIKE(NCUSF2)
     D I@USF3          S                   LIKE(NCUSF3)
     D K#HEDC          S                   LIKE(NCHEDC)
     D K#INC#          S                   LIKE(NCINC#)
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
     C                   PARM                    K#HEDC
     C                   PARM                    K#INC#
     C                   PARM                    I@CTN#
     C                   PARM                    I@AIN#
     C                   PARM                    I@INDT
     C                   PARM                    I@INTM
     C                   PARM                    I@RCDT
     C                   PARM                    I@RCTM
     C                   PARM                    I@STRC
     C                   PARM                    I@RES#
     C                   PARM                    I@SUL#
     C                   PARM                    I@STAT
     C                   PARM                    I@SRDT
     C                   PARM                    I@SRTM
     C                   PARM                    I@SEV1
     C                   PARM                    I@SEV2
     C                   PARM                    I@SEV3
     C                   PARM                    I@PRI#
     C                   PARM                    I@ESC#
     C                   PARM                    I@IEDT
     C                   PARM                    I@IETM
     C                   PARM                    I@RUID
     C                   PARM                    I@AUID
     C                   PARM                    I@ITPC
     C                   PARM                    I@CFL#
     C                   PARM                    I@CFIC
     C                   PARM                    I@USF1
     C                   PARM                    I@USF2
     C                   PARM                    I@USF3
     C                   PARM                    I@SHID
      *
     C                   PARM                    I@ERR
     C                   PARM                    I@PRCA
      *
      *          ------------------------------------------
      *          Exit if not enough parameters
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      D#PARM <> 32
     C                   EXSR      SRTTLR
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Check if the incident exists
      *          ------------------------------------------
     C     $INCK1        SETLL     OMINCL1
     C                   EVAL      *IN99 = %EQUAL
      *
      *                    --------------------------------
      *                    Record not found.
      *                    --------------------------------
     C                   IF        *IN99 = *OFF                                 B01
     C                   EVAL      I@MSID = 'OMQ3006'
      *
     C                   IF        K#HEDC = *BLANKS
     C                   EVAL      %SUBST(I@MSGD:1:2) = '*N'
     C                   ELSE
     C                   MOVEL     K#HEDC        I@MSGD
     C                   ENDIF
      *
     C                   IF        K#INC# = *BLANKS
     C                   EVAL      %SUBST(I@MSD1:1:2) = '*N'
     C                   ELSE
     C                   MOVEL     K#INC#        I@MSD1
     C                   ENDIF
      *
     C                   EVAL      I@MSTP = 'E'
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the request
      *          ------------------------------------------
     C                   READ      OMINCL1
     C                   EVAL      *IN99 = %EOF
      *
      *                    --------------------------------
      *                    Fill the parameters.
      *                    --------------------------------
     C                   MOVE      NCCTN#        I@CTN#
     C                   MOVE      NCAIN#        I@AIN#
     C                   MOVE      NCINDT        I@INDT
     C                   MOVE      NCINTM        I@INTM
     C                   MOVE      NCRCDT        I@RCDT
     C                   MOVE      NCRCTM        I@RCTM
     C                   MOVE      NCSTRC        I@STRC
     C                   MOVE      NCRES#        I@RES#
     C                   MOVE      NCSUL#        I@SUL#
     C                   MOVE      NCSTAT        I@STAT
     C                   MOVE      NCSRDT        I@SRDT
     C                   MOVE      NCSRTM        I@SRTM
     C                   MOVE      NCSEV1        I@SEV1
     C                   MOVE      NCSEV2        I@SEV2
     C                   MOVE      NCSEV3        I@SEV3
     C                   MOVE      NCPRI#        I@PRI#
     C                   MOVE      NCESC#        I@ESC#
     C                   MOVE      NCIEDT        I@IEDT
     C                   MOVE      NCIETM        I@IETM
     C                   MOVE      NCRUID        I@RUID
     C                   MOVE      NCAUID        I@AUID
     C                   MOVE      NCITPC        I@ITPC
     C                   MOVE      NCCFL#        I@CFL#
     C                   MOVE      NCCFIC        I@CFIC
     C                   MOVE      NCUSF1        I@USF1
     C                   MOVE      NCUSF2        I@USF2
     C                   MOVE      NCUSF3        I@USF3
     C                   MOVE      NCSHID        I@SHID
      *
      *          ------------------------------------------
      *          No errors.
      *          ------------------------------------------
     C                   EVAL      I@MSTP = *BLANKS
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
     C                   WHEN      D#PARM <> 32
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
     C     $INCK1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#INC#
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
     C                   IF        E#FILE = 'OMINCL1 '                          B02
     C                   MOVEL     INCL1         FILE
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
     C                   IF        D#PARM = 32                                  B01
     C                   EVAL      I@MSID = E#MSID
     C                   EVAL      I@MSTP = 'E'
     C                   EVAL      I@MSGD = *BLANKS
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
     C                   IF        D#PARM <> 32                                 B01
     C                             OR I@EOP = *ON
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
