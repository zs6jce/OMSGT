     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH031.                                       *
      * Function ...: Validate answer list.                         *
      * Author .....: Remain Software                               *
      * Module .....: HelpDesk                                      *
      * Parameters .: P#HEDC    -  HelpDesk code                    *
      *               P#INC#    -  Incident                         *
      *               P#ALSC    -  Answer list code                 *
      *               P#STAT    -  Status                           *
      *                                                             *
      * This program checks if the answer list is matching th       *
      * answers in the Q&A history.                                 *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMAZDL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(AZDL1)
     FOMQAHL3   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(QAHL3)
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * ARRAY specifications                                        *
      * ----------------------------------------------------------- *
      *
      *COPY QCPYSRC,OMSREFREN
      *
      * ----------------------------------------------------------- *
      /EJECT
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
      *          File information data structures
      *          ------------------------------------------
     D AZDL1           DS            57
     D QAHL3           DS            57
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  D#PARM           *PARMS
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
      * Prototype for OMH031
     D OMH031          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#INC#_                             LIKE(P#INC#)
     D P#ALSC_                             LIKE(P#ALSC)
     D P#STAT_                             LIKE(P#STAT)
      * *ENTRY Interface for Main Procedure
     D OMH031          PI
     D P#HEDC                         5
     D P#INC#                        10
     D P#ALSC                        10
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D I#OK            S                   LIKE(INDI)
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
      *          Get the parameters
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Do the init routine
      *          ------------------------------------------
     C                   EXSR      *INZSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * *INZSR - Program initialisation. This routine also does the *
      *          processing.                                        *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Indicator definitions
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          OMAZD Answer list details
      *          ------------------------------------------
     C     $ZD1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#ALSC
      *
      *          ------------------------------------------
      *          OMQAH Question & Answer History
      *          ------------------------------------------
     C     $AH3K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#INC#
     C                   KFLD                    ZDQUE#
      *
     C     $AH3K2        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#INC#
     C                   KFLD                    ZDQUE#
     C                   KFLD                    ZDANS#
      *
     C     $AH3K3        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#INC#
     C                   KFLD                    ZDQUE#
     C                   KFLD                    ZDANS#
     C                   KFLD                    ZDOQL#
      *
      *          ------------------------------------------
      *          If passed without parameters.
      *          ------------------------------------------
     C                   IF        D#PARM = *ZERO                               B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Read the answer list details
      *          ------------------------------------------
     C     $ZD1K1        CHAIN     OMAZDL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Check if this question is answered.
      *          ------------------------------------------
     C                   SELECT                                                 B02
      *
      *                    --------------------------------
      *                    All fields where used.
      *                    --------------------------------
     C                   WHEN      ZDQUE# <> *ZEROS                             X02
     C                             AND ZDANS# <> *ZEROS
     C                             AND ZDOQL# <> *ZEROS
      *
     C     $AH3K3        SETLL     OMQAHL3
     C                   EVAL      *IN99 = %EQUAL
     C                   EVAL      I#OK = *IN99
      *
      *                    --------------------------------
      *                    Only the question and the answer
      *                    --------------------------------
     C                   WHEN      ZDQUE# <> *ZEROS                             X02
     C                             AND ZDANS# <> *ZEROS
     C                             AND ZDOQL# = *ZEROS
      *
     C     $AH3K2        SETLL     OMQAHL3
     C                   EVAL      *IN99 = %EQUAL
     C                   EVAL      I#OK = *IN99
      *
      *                    --------------------------------
      *                    Only the question and the originatin
      *                    question list.
      *                    --------------------------------
     C                   WHEN      ZDQUE# <> *ZEROS                             X02
     C                             AND ZDANS# = *ZEROS
     C                             AND ZDOQL# <> *ZEROS
      *
     C                   EVAL      I#OK = *OFF
     C     $AH3K1        CHAIN     OMQAHL3
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B03
     C                   IF        AHOQL# = ZDOQL#                              B04
     C                   EVAL      I#OK = *ON
     C                   LEAVE
     C                   ENDIF                                                  E04
     C     $AH3K1        READE     OMQAHL3
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E03
      *
      *                    --------------------------------
      *                    Only the question.
      *                    --------------------------------
     C                   WHEN      ZDQUE# <> *ZEROS                             X02
     C                             AND ZDANS# = *ZEROS
     C                             AND ZDOQL# = *ZEROS
      *
     C     $AH3K1        SETLL     OMQAHL3
     C                   EVAL      *IN99 = %EQUAL
     C                   EVAL      I#OK = *IN99
     C                   ENDSL                                                  E02
      *
      *          ------------------------------------------
      *          Leave if not OK.
      *          ------------------------------------------
     C                   IF        I#OK = *OFF                                  B02
     C                   EVAL      P#STAT = C#TERM
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read the next answer list.
      *          ------------------------------------------
     C     $ZD1K1        READE     OMAZDL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Return to caller.
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
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
      * Quit without warning.                                       *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *PSSR         BEGSR
      *
      *          ------------------------------------------
      *          Do if error not in this routine.
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
      * SRTTLR - Total last record processing                       *
      *                                                             *
      * This routine sets the last record indicator on.             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          If passed without parameters.
      *          ------------------------------------------
     C                   IF        D#PARM = *ZERO                               B01
     C                   EVAL      *INLR = *ON
     C                   ENDIF                                                  E01
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
