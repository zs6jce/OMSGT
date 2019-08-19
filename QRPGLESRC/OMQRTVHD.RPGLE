     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMQRTVHD                                      *
      * Function ...: Retrieve helpdesk                             *
      * Module .....: HelpDesk                                      *
      * Author .....: Remain Software                               *
      *                                                             *
      * Input ......: P#HEDC  -  HelpDesk Code                      *
      *                                                             *
      * Output .....: H@HEDD  -  Description                        *
      *               H@HDMC  -  Helpdesk manager                   *
      *               H@CCTN  -  Last call number                   *
      *               H@ICTN  -  Increment                          *
      *               H@CINC  -  Last incident number               *
      *               H@IINC  -  Increment                          *
      *               H@ISFL  -  Incident spool file library        *
      *               H@CTFP  -  Call track free field program      *
      *               H@CTFL  -  Call track free field library      *
      *               H@INFP  -  Incidents  free field program      *
      *               H@INFL  -  Incidents  free field library      *
      *               H@ERR   -  Error buffer                       *
      *                                                             *
      * Input ......: H@PRCA  -  Processing array                   *
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
      * OMHDDL1  -  Call track file                                 *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Files                                                       *
      * ----------------------------------------------------------- *
     FOMHDDL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(HDDL1)
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
     D HDDL1           DS            57
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
     D H@ERR           DS
     D H@MSID                         7
     D H@MSTP                         1
     D H@MSGD                       128
      *
      *          ------------------------------------------
      *          Parameter: processing array
      *          ------------------------------------------
     D H@PRCA          DS            16
     D H@EOP                          1
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
     D H@CCTN          S                   LIKE(DDCCTN)
     D H@CINC          S                   LIKE(DDCINC)
     D H@CTFL          S                   LIKE(DDCTFL)
     D H@CTFP          S                   LIKE(DDCTFP)
     D H@HDMC          S                   LIKE(DDHDMC)
     D H@HEDD          S                   LIKE(DDHEDD)
     D H@ICTN          S                   LIKE(DDICTN)
     D H@IINC          S                   LIKE(DDIINC)
     D H@INFL          S                   LIKE(DDINFL)
     D H@INFP          S                   LIKE(DDINFP)
     D H@ISFL          S                   LIKE(DDISFL)
     D I#PSSR          S              1
     D P#HEDC          S                   LIKE(DDHEDC)
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
     C                   PARM                    H@HEDD
     C                   PARM                    H@HDMC
     C                   PARM                    H@CCTN
     C                   PARM                    H@ICTN
     C                   PARM                    H@CINC
     C                   PARM                    H@IINC
     C                   PARM                    H@ISFL
     C                   PARM                    H@CTFP
     C                   PARM                    H@CTFL
     C                   PARM                    H@INFP
     C                   PARM                    H@INFL
      *
     C                   PARM                    H@ERR
     C                   PARM                    H@PRCA
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
      *          Check if the desk exists
      *          ------------------------------------------
     C     P#HEDC        SETLL     OMHDDL1
     C                   EVAL      *IN99 = %EQUAL
      *
      *                    --------------------------------
      *                    Record not found.
      *                    --------------------------------
     C                   IF        *IN99 = *OFF                                 B01
     C                   EVAL      H@MSID = 'OMQB000'
      *
     C                   IF        P#HEDC = *BLANKS
     C                   EVAL      %SUBST(H@MSGD:1:2) = '*N'
     C                   ELSE
     C                   MOVEL     P#HEDC        H@MSGD
     C                   ENDIF
      *
     C                   IF        P#HEDC = *BLANKS
     C                   EVAL      %SUBST(H@MSGD:1:2) = '*N'
     C                   ELSE
     C                   MOVEL     P#HEDC        H@MSGD
     C                   ENDIF
      *
     C                   EVAL      H@MSTP = 'E'
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the call
      *          ------------------------------------------
     C                   READ      OMHDDL1
     C                   EVAL      *IN99 = %EOF
      *
      *                    --------------------------------
      *                    Fill the parameters.
      *                    --------------------------------
     C                   MOVE      DDHEDD        H@HEDD
     C                   MOVE      DDHDMC        H@HDMC
     C                   MOVE      DDCCTN        H@CCTN
     C                   MOVE      DDICTN        H@ICTN
     C                   MOVE      DDCINC        H@CINC
     C                   MOVE      DDIINC        H@IINC
     C                   MOVE      DDISFL        H@ISFL
     C                   MOVE      DDCTFP        H@CTFP
     C                   MOVE      DDCTFL        H@CTFL
     C                   MOVE      DDINFP        H@INFP
     C                   MOVE      DDINFL        H@INFL
      *
      *          ------------------------------------------
      *          No errors.
      *          ------------------------------------------
     C                   EVAL      H@MSTP = *BLANKS
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
     C                   IF        E#FILE = 'OMHDDL1 '                          B02
     C                   MOVEL     HDDL1         FILE
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
     C                   IF        D#PARM = 14                                  B01
     C                   EVAL      H@MSID = E#MSID
     C                   EVAL      H@MSTP = 'E'
     C                   EVAL      H@MSGD = *BLANKS
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
     C                   IF        D#PARM <> 14                                 B01
     C                             OR H@EOP = *ON
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
