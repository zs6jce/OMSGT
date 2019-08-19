     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH0072                                       *
      * Function ...: Asking questions; set attributes              *
      * Module .....: Help desk                                     *
      *                                                             *
      * Author .....: Remain Software                               *
      * Action .....: Creation.                                     *
      *                                                             *
      * Parameters .: P#HEDC - Helpdesk                             *
      *               P#INC# - Incident                             *
      *               P#ACTC - Action code                          *
      *               P#QUE  - Questions array                      *
      *               P#ANS  - Answers array                        *
      *               P#QLS  - Question lists array                 *
      *               P#QSQ  - Question sequence in QL array        *
      *               P#SEV1 - Severity 1                           *
      *               P#SEV2 - Severity 2                           *
      *               P#SEV3 - Severity 3                           *
      *               P#STRC - Status route code                    *
      *               P#RES# - Route entry sequence                 *
      *               P#PRI# - Priority                             *
      *               P#ITPC - Incident type code                   *
      *               P#USF1 - User field 1                         *
      *               P#USF2 - User field 2                         *
      *               P#USF3 - User field 2                         *
      *               P#STAT - Status                               *
      *                                                             *
      * This program is retrieving the incident attributes from the *
      * question lists array.                                       *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators                                             *
      * ----------------------------------------------------------- *
      *                                                             *
      * 50  Test numeric incoming substring                         *
      * 99 - General purpose indicator.                             *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                   ANSL1 *
      * ----------------------------------------------------------- *
     FOMQLAL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(QLAL1)
     FOMQAH     IF A E             DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(QAH)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      *                                                              *
      * QUE   Used to store question numbers                         *
      * ANS   Used to store answers                                  *
      * QLS   Used to store question lists                           *
      * QSQ   Used to store question seq in question list            *
      * ------------------------------------------------------------ *
      *COPY QCPYSRC,OMSREFREN
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Input pecifications                                         *
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
     D QLAL1           DS            57
     D QAH             DS            57
     D P#QUE           DS          5000
     D  QUE                    1   5000  0
     D                                     DIM(1000)
     D P#ANS           DS          3000
     D  ANS                    1   3000  0
     D                                     DIM(1000)
     D P#QLS           DS          5000
     D  QLS                    1   5000  0
     D                                     DIM(1000)
     D P#QSQ           DS          3000
     D  QSQ                    1   3000  0
     D                                     DIM(1000)
      *
      *          ------------------------------------------
      *          Workfields
      *          ------------------------------------------
     D                 DS                  INZ
     D IZ                             4  0
     D X                              4  0
     D Y                              4  0
     D Z                              4  0
      *
     D                 DS                  INZ
     D W#1                            5
     D W#2                            3
     D W#3                            5
     D W#4                            3
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  E#FILE               201    208
     D  D#PARM           *PARMS
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
      *          Named constants
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
      * Prototype for 'OMX929'
     D OMX929          PR                  EXTPGM('OMX929')
     D P#HEDC_                             LIKE(P#HEDC)
     D P#INC#_                             LIKE(P#INC#)
     D P#ACTC_                             LIKE(P#ACTC)
     D P#MSID_                             LIKE(P#MSID)
     D P#MSGD_                             LIKE(P#MSGD)
     D P#MSGF_                             LIKE(P#MSGF)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          OMQLAL1 Question list answer file
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
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D p#ans#          S              3P 0
     D p#pri#          S              3P 0
     D p#qls#          S              5P 0
     D p#qsq#          S              3P 0
     D p#que#          S              5P 0
     D p#res#          S              3P 0
     D p#usf3          S             15P 0
     D I#PSSR          S              1
     D MSGDTA          S            256
     D P#ACTC          S                   LIKE(ACTC)
     D P#ACTN          S                   LIKE(ACTN)
     D P#CTN#          S                   LIKE(CTN#)
     D P#FILE          S             57
     D P#HEDC          S                   LIKE(HEDC)
     D P#INC#          S                   LIKE(INC#)
     D P#ITPC          S                   LIKE(ITPC)
     D P#MSGD          S                   LIKE(MSGDTA)
     D P#MSGF          S                   LIKE(MSGF)
     D P#MSID          S                   LIKE(MSID)
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#PMQC          S             10
     D P#SEV1          S                   LIKE(SEV1)
     D P#SEV2          S                   LIKE(SEV2)
     D P#SEV3          S                   LIKE(SEV3)
     D P#STAT          S                   LIKE(STAT)
     D P#STRC          S                   LIKE(STRC)
     D P#USF1          S                   LIKE(USF1)
     D P#USF2          S                   LIKE(USF2)
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
     C     *ENTRY        PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    P#INC#
     C                   PARM                    P#ACTC
     C                   PARM                    P#QUE
     C                   PARM                    P#ANS
     C                   PARM                    P#QLS
     C                   PARM                    P#QSQ
     C                   PARM                    P#SEV1
     C                   PARM                    P#SEV2
     C                   PARM                    P#SEV3
     C                   PARM                    P#STRC
     C                   PARM                    P#RES#
     C                   PARM                    P#PRI#
     C                   PARM                    P#ITPC
     C                   PARM                    P#USF1
     C                   PARM                    P#USF2
     C                   PARM                    P#USF3
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Exit if no parameters
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
      *          Exit
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      /EJECT
      * ----------------------------------------------------------- *
      * SRVERW - Search attributes                                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Initialize
      *          ------------------------------------------
     C                   EVAL      Z = 1
     C                   EVAL      X = 1
     C                   EVAL      Y = 1
      *
      *          ------------------------------------------
      *          Initialize parameters
      *          ------------------------------------------
     C                   EVAL      P#SEV1 = *BLANKS
     C                   EVAL      P#SEV2 = *BLANKS
     C                   EVAL      P#SEV3 = *BLANKS
     C                   EVAL      P#STRC = *BLANKS
     C                   EVAL      P#RES# = 0
     C                   EVAL      P#PRI# = 0
     C                   EVAL      P#ITPC = *BLANKS
     C                   EVAL      P#USF1 = *BLANKS
     C                   EVAL      P#USF2 = *BLANKS
     C                   EVAL      P#USF3 = 0
     C                   EVAL      P#STAT = C#NORM
      *
      *          ------------------------------------------
      *          Do until the end of the array is reached
      *          ------------------------------------------
     C                   EVAL      IZ = 1
     C                   DOW       ANS(IZ) <> 0                                 B01
     C                             AND IZ <= 1000
     C                   EVAL      P#ANS# = ANS(IZ)
     C                   EVAL      P#QUE# = QUE(IZ)
     C                   EVAL      P#QSQ# = QSQ(IZ)
     C                   EVAL      P#QLS# = QLS(IZ)
      *
      *          ------------------------------------------
      *          Get the attributes belonging to these answers
      *          ------------------------------------------
     C     $LA1K2        CHAIN     OMQLAR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ADD       1             IZ
     C                   IF        LASEV1 <> *BLANKS                            B01
     C                   MOVEL     LASEV1        P#SEV1
     C                   ENDIF                                                  E01
     C                   IF        LASEV2 <> *BLANKS                            B01
     C                   MOVEL     LASEV2        P#SEV2
     C                   ENDIF                                                  E01
     C                   IF        LASEV3 <> *BLANKS                            B01
     C                   MOVEL     LASEV3        P#SEV3
     C                   ENDIF                                                  E01
     C                   IF        LASTRC <> *BLANKS                            B01
     C                   MOVEL     LASTRC        P#STRC
     C                   ENDIF                                                  E01
     C                   IF        LARES# <> *ZEROS                             B01
     C                   Z-ADD     LARES#        P#RES#
     C                   ENDIF                                                  E01
     C                   IF        LAPRI# <> *ZEROS                             B01
     C                   Z-ADD     LAPRI#        P#PRI#
     C                   ENDIF                                                  E01
     C                   IF        LAITPC <> *BLANKS                            B01
     C                   MOVEL     LAITPC        P#ITPC
     C                   ENDIF                                                  E01
     C                   IF        LAUSF1 <> *BLANKS                            B01
     C                   MOVEL     LAUSF1        P#USF1
     C                   ENDIF                                                  E01
     C                   IF        LAUSF2 <> *BLANKS                            B01
     C                   MOVEL     LAUSF2        P#USF2
     C                   ENDIF                                                  E01
     C                   IF        LAUSF3 <> *ZEROS                             B01
     C                   Z-ADD     LAUSF3        P#USF3
     C                   ENDIF                                                  E01
     C                   ENDDO
      *
      *          ------------------------------------------
      *          If 'Write to history' is on
      *          ------------------------------------------
     C                   IF        P#INC# <> *BLANKS                            B01
      *
      *          ------------------------------------------
      *          Update history file, fill parameters
      *          ------------------------------------------
     C                   MOVEL     P#HEDC        AHHEDC
     C                   MOVEL     P#INC#        AHINC#
     C                   Z-ADD     QLS(1)        AHOQL#
      *
      *          ------------------------------------------
      *          Initialize sequence field
      *          ------------------------------------------
     C                   EVAL      AHSEQ# = *ZEROS
      *
      *          ------------------------------------------
      *          Loop through all questions and answers
      *          ------------------------------------------
     C                   EVAL      IZ = 1
     C                   DOW       ANS(IZ) <> 0                                 B02
     C                             AND IZ <= 1000
     C                   Z-ADD     ANS(IZ)       AHANS#
     C                   Z-ADD     QUE(IZ)       AHQUE#
     C                   Z-ADD     QSQ(IZ)       AHQSQ#
     C                   Z-ADD     QLS(IZ)       AHQLS#
     C                   ADD       10            AHSEQ#
     C                   WRITE     OMQAHR
     C                   ADD       1             IZ
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Call OMX929 (fill log file)
      *          ------------------------------------------
     C                   EVAL      P#MSID = 'MH00701'
     C                   EVAL      P#MSGF = 'OMHMSG'
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMX929 ( P#HEDC : P#INC# : P#ACTC :
     C                             P#MSID : P#MSGD : P#MSGF : P#STAT )
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
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
      *          Parameter list for OMX929 (Fill log file)
      *          ------------------------------------------
     C     $MX929        PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    P#INC#
     C                   PARM                    P#ACTC
     C                   PARM                    P#MSID
     C                   PARM                    P#MSGD
     C                   PARM                    P#MSGF
     C                   PARM      C#NORM        P#STAT
      *
      *          ------------------------------------------
      *          OMQLAL1 Question list answer file
      *          ------------------------------------------
     C     $LA1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#QLS#
     C                   KFLD                    P#QSQ#
     C                   KFLD                    P#QUE#
      *
     C     $LA1K2        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#QLS#
     C                   KFLD                    P#QSQ#
     C                   KFLD                    P#QUE#
     C                   KFLD                    P#ANS#
      *
      *          ------------------------------------------
      *          Dummy read QAH
      *          ------------------------------------------
     C                   IF        1 = 2                                        B01
     C                   READ      OMQAHR
     C                   EVAL      *IN99 = %EOF
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
      *          If number of parameters is zero, or action
      *          is 'Write to history' is on, then the
      *          program can be closed
      *          ------------------------------------------
     C                   IF        D#PARM = *ZEROS                              B01
     C                   EVAL      *INLR = *ON
     C                   ELSE
     C                   EVAL      P#STAT = C#NORM
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Return to calling program
      *          ------------------------------------------
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
      *
     C                   WHEN      E#FILE = 'OMQLAL1 '                          X02
     C                   MOVEL     QLAL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMQAH   '                          X02
     C                   MOVEL     QAH           P#FILE
      *
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
      *
