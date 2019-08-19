     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMQCHGCC                                      *
      * Function ...: API Maintain customer call                    *
      *                                                             *
      * Author .....: Remain Software                               *
      * Parameters .: P4INTL  - Interface level         (I)         *
      *               P4ACTC  - Activity code           (I)         *
      *               P4BUF   - Buffer data             (I/O)       *
      *               P4PRC   - Processing array        (I)         *
      *               P4ERR   - Error data              (O)         *
      *               P4FLD   - Field error data        (I/O)       *
      *                                                             *
      * This program is used to add, change or delete an call.      *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Sub programs                                                *
      * ----------------------------------------------------------- *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators                                             *
      * ----------------------------------------------------------- *
      *                                                             *
      * 99 - General purpose indicator.                             *
      *                                                             *
      * I#XXXX - Indicator fields                                   *
      * C#XXXX - Predefined constants                               *
      * W#XXXX - Work fields                                        *
      * P#XXXX - Parameter fields                                   *
      * D#XXXX - Data structure fields                              *
      * C4XXXX - Input buffer data structure                        *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMHDDL1   IF   E           K DISK    INFDS(HDDL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMCUSL1   IF   E           K DISK    INFDS(CUSL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMCCOL1   IF   E           K DISK    INFDS(CCOL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMCCAL1   UF A E           K DISK    INFDS(CCAL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMINCJ5   IF   E           K DISK    INFDS(INCJ5)
     F                                     USROPN
     F                                     INFSR(*PSSR)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension spec's                                             *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * ERR   Max. 30 elements of field errors, during checking      *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(15)
     D MSD             S              7    DIM(15) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
     D ERR             S              4    DIM(30)
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
      *          The call file as data structure.
      *          ------------------------------------------
     D DSCCAR        E DS                  EXTNAME(OMCCA) INZ
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  D#PGM                  1     10
     D  E#FILE               201    208
     D  D#JOB                244    253
     D  D#USID               254    263
     D  D#JOB#               264    269  0
      *
      *          ------------------------------------------
      *          Structure for exception handling
      *          ------------------------------------------
     D HDDL1           DS            57
     D CUSL1           DS            57
     D CCOL1           DS            57
     D CCAL1           DS            57
     D INCJ5           DS            57
      *
      *          ------------------------------------------
      *          Call API buffer parameter
      *          DS for saving the buffer
      *          ------------------------------------------
     D P4BUF           DS           300
     D S#BUF           DS           300
      *
      *          ------------------------------------------
      *          Structure for comparing date
      *          ------------------------------------------
     D                 DS
     D D#CHKD                         7  0
     D  DCCHKD                        7    OVERLAY(D#CHKD)
      *
      *          ------------------------------------------
      *          Structure for numbers in messages
      *          ------------------------------------------
     D                 DS
     D D#BC4#                         9B 0
     D  D#BC4A                        4    OVERLAY(D#BC4#)
      *
      *          ------------------------------------------
      *          Date convertor.
      *          ------------------------------------------
     D                 DS
     D D#DATA                         7
     D  D#DATE                        7  0 OVERLAY(D#DATA)
     D   D#CC                         1  0 OVERLAY(D#DATE)
     D    D#CCC                       1    OVERLAY(D#CC)
     D   D#YY                         2  0 OVERLAY(D#DATE:2)
     D   D#MM                         2  0 OVERLAY(D#DATE:4)
     D   D#DD                         2  0 OVERLAY(D#DATE:6)
      *
      *          ------------------------------------------
      *          Field containing time
      *          ------------------------------------------
     D                 DS                  INZ
     D D#TIME                         6  0
     D  D#HMC#                        4  2 OVERLAY(D#TIME)
     D   D#MNC#                       2  0 OVERLAY(D#HMC#:3)
     D D#REST                         8  2
     D D#TIM#                         6  0
     D  D#HML#                        4  2 OVERLAY(D#TIM#)
     D   D#MNL#                       2  0 OVERLAY(D#HML#:3)
      *
      *          ------------------------------------------
      *          Key convertor for rebuilding status
      *          ------------------------------------------
     D                 DS
     D D#KEY                         16
     D  D#APPL                        5    OVERLAY(D#KEY)
     D  D#ROF#                       10    OVERLAY(D#KEY:6)
     D  D#ROF                         1    OVERLAY(D#KEY:16)
      *
      *          ------------------------------------------
      *          Helpdesk data structures.
      *          ------------------------------------------
      /COPY QCPYLESRC,OMHLPDS
      *
      *          ------------------------------------------
      *          All buffers for OMQCHGCC
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQCHGCCDS
      *
      *          ------------------------------------------
      *          Extra redefinitions of buffer
      *          ------------------------------------------
     D  C4USFA               101    108
      *
      *          ------------------------------------------
      *          DS for retrieving message description
      *          ------------------------------------------
     D D#USFA          DS
     D D#USF3                        15  0
      *
      *          ------------------------------------------
      *          DS for retrieving message description
      *          ------------------------------------------
     D D#RTVM          DS           156
     D D#MDTA                 25    156
      *
      *          ------------------------------------------
      *          IBM API Binary number definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFBINARY
      *
      *          ------------------------------------------
      *          IBM API error buffer
      *          ------------------------------------------
      /COPY QAPILESRC,ERR_BUFFER
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#API           C                   CONST('OMHAPI    *LIBL')
     D C#MSG1          C                   CONST('OMHMSG    *LIBL')
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#EXIT          C                   CONST('*EXIT')
     D C#AUTH          C                   CONST('*AUTH')
     D C#CMP           C                   CONST('*CMP ')
     D C#NEW           C                   CONST('*NEW ')
     D C#LOCK          C                   CONST('*LOCK')
     D C#ALL           C                   CONST('*ALL ')
     D C#SAME          C                   CONST('*SAME')
     D C#SA            C                   CONST('*SA')
     D C#S             C                   CONST('*S')
     D C#DFT           C                   CONST('*DFT ')
     D C#CURR          C                   CONST('*CURR')
     D C#FRST          C                   CONST('*FIRST')
     D C#GEN           C                   CONST('*GEN')
     D C#INTL          C                   CONST('INTL')
     D C#ACTC          C                   CONST('ACTC')
     D C#HEDC          C                   CONST('HEDC')
     D C#CTN#          C                   CONST('CTN#')
     D C#CUSC          C                   CONST('CUSC')
     D C#CCTC          C                   CONST('CCTC')
     D C#INDT          C                   CONST('INDT')
     D C#INTM          C                   CONST('INTM')
     D C#RCTM          C                   CONST('RCTM')
     D C#USID          C                   CONST('USID')
     D C#CSRC          C                   CONST('CSRC')
     D C#USF1          C                   CONST('USF1')
     D C#USF2          C                   CONST('USF2')
     D C#USF3          C                   CONST('USF3')
     D C#PAR#          C                   CONST(6)
      * Prototype for 'OMH029'
     D OMH029          PR                  EXTPGM('OMH029')
     D P#OPTI_                             LIKE(P#OPTI)
     D P#HEDC_                             LIKE(P#HEDC)
     D P#SUL#_                             LIKE(P#SUL#)
     D P#STAT_                             LIKE(P#STAT)
     D D#D7HD_                             LIKE(D#D7HD)
     D D#D8HD_                             LIKE(D#D8HD)
      *
      *          ------------------------------------------
      *          Check user fields
      * Prototype for W#OBJQ
     D W#OBJQ_001      PR                  EXTPGM(W#OBJQ)
     D P#HEDC_                             LIKE(P#HEDC)
     D P#CTN#_                             LIKE(P#CTN#)
     D P#ACTC_                             LIKE(P#ACTC)
     D P#USF1_                             LIKE(P#USF1)
     D P#F1ST_                             LIKE(P#F1ST)
     D P#USF2_                             LIKE(P#USF2)
     D P#F2ST_                             LIKE(P#F2ST)
     D P#USF3_                             LIKE(P#USF3)
     D P#F3ST_                             LIKE(P#F3ST)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          OMCUSL1 Customer database
      * Prototype for 'QMHSNDPM'
     D QMHSNDPM        PR                  EXTPGM('QMHSNDPM')
     D P#MIDC_                             LIKE(P#MIDC)
     D P#MSGQ_                             LIKE(P#MSGQ)
     D C4MSGD_                             LIKE(C4MSGD)
     D P#MDL#_                             LIKE(P#MDL#)
     D P#MSTC_                             LIKE(P#MSTC)
     D P#PMQC_                             LIKE(P#PMQC)
     D P#PSC#_                             LIKE(P#PSC#)
     D P#MSKC_                             LIKE(P#MSKC)
     D P#ERR_                              LIKE(P#ERR)
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
      * Prototype for 'OMX967'
     D OMX967          PR                  EXTPGM('OMX967')
      * Prototype for 'QMHRTVM'
     D QMHRTVM         PR                  EXTPGM('QMHRTVM')
     D D#RTVM_                             LIKEDS(D#RTVM)
     D P#MDL#_                             LIKE(P#MDL#)
     D P#FMTC_                             LIKE(P#FMTC)
     D P#MIDC_                             LIKE(P#MIDC)
     D P#MSQL_                             LIKE(P#MSQL)
     D P#DUM6_                             LIKE(P#DUM6)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#DUM7_                             LIKE(P#DUM7)
     D P#DUM8_                             LIKE(P#DUM8)
     D P#ERR_                              LIKE(P#ERR)
      *
      *          ------------------------------------------
      *          Parameter list for OMH029 (Get info)
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D p#usf3          S             15P 0
     D w#sew#          S              3P 0
     D w#sydt          S              7P 0
     D w#sytm          S              6P 0
     D D#1UL#          S                   LIKE(SUL#)
     D D#2UL#          S                   LIKE(SUL#)
     D I#CAL1          S              1
     D I#COL1          S              1
     D I#DDL1          S              1
     D I#NCJ5          S              1
     D I#PSSR          S              1
     D I#QUIT          S                   LIKE(INDI)
     D I#USL1          S              1
     D I#X967          S                   LIKE(INDI)
     D IE              S              5  0
     D IR              S              3  0
     D K#CCTC          S                   LIKE(CCTC)
     D K#CTN#          S                   LIKE(CTN#)
     D K#CUSC          S                   LIKE(CUSC)
     D K#HEDC          S                   LIKE(HEDC)
     D MS              S              3  0
     D P#ACTC          S                   LIKE(ACTC)
     D P#CHKD          S              7  0
     D P#CTN#          S                   LIKE(CTN#)
     D P#DUM6          S              1
     D P#DUM7          S             10
     D P#DUM8          S             10
     D P#FILE          S             57
     D P#F1ST          S                   LIKE(STAT)
     D P#F2ST          S                   LIKE(STAT)
     D P#F3ST          S                   LIKE(STAT)
     D P#GRCL          S                   LIKE(USID)
     D P#GRID          S                   LIKE(USID)
     D P#HEDC          S                   LIKE(HEDC)
     D P#MSGD          S                   LIKE(MSGD)
     D P#MSGF          S                   LIKE(MSGF)
     D P#MSGN          S                   LIKE(MSID)
     D P#MSID          S                   LIKE(MSID)
     D P#MSQL          S             20
     D P#MSTA          S             40
     D P#OPTI          S              1
     D P#RMTI          S                   LIKE(INDI)
     D P#STAT          S                   LIKE(STAT)
     D P#SUL#          S                   LIKE(SUL#)
     D P#USCL          S                   LIKE(USID)
     D P#USDC          S                   LIKE(USDC)
     D P#USF1          S                   LIKE(USF1)
     D P#USF2          S                   LIKE(USF2)
     D P#USID          S                   LIKE(USID)
     D P4ACTC          S              2
     D P4ERR           S            136
     D P4FLD           S            130
     D P4INTL          S              8
     D P4PRC           S             40
     D S#CTN#          S                   LIKE(CTN#)
     D W#EOFI          S                   LIKE(INDI)
     D W#ERR           S              3  0
     D W#FLD1          S              4
     D W#FLD2          S              4
     D W#FLD3          S              4
     D W#OBJQ          S             21
     D W#VALI          S                   LIKE(INDI)
     D W#50            S              5  0
      *----------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *----------------------------------------------------------------------
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Main line                                                   *
      * ----------------------------------------------------------- *
     C     *ENTRY        PLIST
     C                   PARM                    P4INTL
     C                   PARM                    P4ACTC
     C                   PARM                    P4BUF
     C                   PARM                    P4PRC
     C                   PARM                    P4ERR
     C                   PARM                    P4FLD
      *
      *          ------------------------------------------
      *          Determine the action.
      *          ------------------------------------------
     C                   IF        D#PARM >= C#PAR#                             B01
      *
      *          ------------------------------------------
      *          Do the init routine
      *          ------------------------------------------
     C                   EXSR      SRINIT
      *
      *          ------------------------------------------
      *          Fill buffer data structures
      *          ------------------------------------------
     C                   MOVEL     P4BUF         C4BUF
     C                   MOVEL     P4PRC         C4PRC
     C                   MOVEL     P4FLD         C4FLD
     C                   MOVE      *BLANKS       C4ERR
     C                   MOVE      *BLANKS       C4FLDA
      *
      *          ------------------------------------------
      *          Do the processing routine.
      *          ------------------------------------------
     C                   EXSR      SRVERW
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do the total last record routine
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRVERW - Main program processing routine                    *
      *                                                             *
      * This routine calls a routine depending on the change code   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Call subroutine depending on ACTC
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      P4ACTC = '20'                                Change
     C                   EXSR      SRCH20                                       Change
     C                   WHEN      P4ACTC = '40'                                Delete
     C                   EXSR      SRCH40                                       Delete
     C                   WHEN      P4ACTC = '60'                                Add
     C                   EXSR      SRCH60                                       Add
     C                   OTHER
     C                   EXSR      SRCHER
     C                   ENDSL
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCH20 - Change code 20 Change Call                         *
      *                                                             *
      * This routine is executed when the program has to change     *
      * a Call.                                                     *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCH20        BEGSR
      *
      *          ------------------------------------------
      *          Process special keywords.
      *          ------------------------------------------
     C                   EXSR      SRSPKW
      *
      *          ------------------------------------------
      *          Process the input.
      *          ------------------------------------------
     C                   EXSR      SRINPT
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        ERR(1) <> *BLANKS                            B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the call record.
      *          ------------------------------------------
     C                   MOVE      C4HEDC        K#HEDC
     C                   MOVE      C4CTN#        K#CTN#
     C     $CA1K1        CHAIN     OMCCAL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill the record and update
      *          ------------------------------------------
     C                   MOVE      C4CUSC        CACUSC
     C                   MOVE      C4CCTC        CACCTC
     C                   MOVE      C4RCTM        CARCTM
     C                   MOVE      C4CSRC        CACSRC
     C                   MOVE      C4USID        CAUSID
     C                   MOVE      C4USF1        CAUSF1
     C                   MOVE      C4USF2        CAUSF2
     C                   MOVE      C4USF3        CAUSF3
     C                   EXSR      SRIO
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCH40 - Change code 40 Delete Call                         *
      *                                                             *
      * This routine is executed when the program has to delete     *
      * a Call.                                                     *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCH40        BEGSR
      *
      *          ------------------------------------------
      *          Process special keywords.
      *          ------------------------------------------
     C                   EXSR      SRSPKW
      *
      *          ------------------------------------------
      *          Process the input.
      *          ------------------------------------------
     C                   EXSR      SRINPT
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        ERR(1) <> *BLANKS                            B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the call record.
      *          ------------------------------------------
     C     $CA1K1        CHAIN     OMCCAL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill the record and update
      *          ------------------------------------------
     C                   EXSR      SRIO
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCH60 - Change code 60 Add Call                            *
      *                                                             *
      * This routine is executed when the program has to add        *
      * a Call                                                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCH60        BEGSR
      *
      *          ------------------------------------------
      *          Process special keywords.
      *          ------------------------------------------
     C                   EXSR      SRSPKW
      *
      *          ------------------------------------------
      *          Process the input.
      *          ------------------------------------------
     C                   EXSR      SRINPT
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        ERR(1) <> *BLANKS                            B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get a new call number
      *          ------------------------------------------
     C                   IF        C4CTN# = '*GEN'                              B01
     C                   CALL      'OMX967'      $MX967                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X967 = *OFF
     C                   MOVE      *BLANKS       C4CTN#
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X967 = *ON
     C                   MOVE      P#CTN#        C4CTN#
     C                   ENDIF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Catch *TERM status.
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Fill the record and write
      *          ------------------------------------------
     C                   MOVE      C4HEDC        CAHEDC
     C                   MOVE      C4CTN#        CACTN#
     C                   MOVE      C4CUSC        CACUSC
     C                   MOVE      C4CCTC        CACCTC
     C                   MOVE      C4INDT        CAINDT
     C                   MOVE      C4INTM        CAINTM
     C                   MOVE      C4RCTM        CARCTM
     C                   MOVE      C4USID        CAUSID
     C                   MOVE      C4CSRC        CACSRC
     C                   MOVE      C4USF1        CAUSF1
     C                   MOVE      C4USF2        CAUSF2
     C                   MOVE      C4USF3        CAUSF3
     C                   EXSR      SRIO
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C     CH6099        TAG
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSPKW - Process the special keywords.                      *
      *                                                             *
      * It is possible to fill the buffer fields with special       *
      * keywords. In this routine these keywords are validated      *
      * and filled with the value needed.                           *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSPKW        BEGSR
      *
      *          ------------------------------------------
      *          The helpdesk code must exist.
      *          ------------------------------------------
     C                   EXSR      SRDDL1
     C     C4HEDC        SETLL     OMHDDL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B01
     C                   MOVEL     'OMQ5028'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     C4HEDC        MC(1)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#HEDC
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If add then reset the output record.
      *          ------------------------------------------
     C                   IF        P4ACTC = '60'                                B01
     C                   RESET                   OMCCAR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the record if this is other than add.
      *          ------------------------------------------
     C                   IF        P4ACTC <> '60'                               B01
     C                   MOVE      C4HEDC        K#HEDC
     C                   MOVE      C4CTN#        K#CTN#
     C                   EXSR      SRCAL1
     C                   MOVE      C4HEDC        K#HEDC
     C                   MOVE      C4CTN#        K#CTN#
     C     $CA1K1        CHAIN(N)  OMCCAL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Error if record not found.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEL     'OMQB702'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     C4HEDC        MC(1)
     C                   MOVEA     C4CTN#        MC(6)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#CTN#
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Call program to get support level
      *          ------------------------------------------
     C                   EVAL      P#OPTI = '3'
     C                   EVAL      P#SUL# = '0'
     C                   MOVE      CAHEDC        P#HEDC
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD )
      *
      *          ------------------------------------------
      *          If error inauthorization program.
      *          ------------------------------------------
     C                   IF        P#STAT <> C#NORM                             B03
     C                   MOVEL     'OMQB70C'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     C4CTN#        MC(1)
     C                   MOVEA     C4HEDC        MC(11)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#CTN#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the call track number if not add nor copy.
      *          ------------------------------------------
     C                   IF        P4ACTC <> '60'                               B01
     C                             AND C4CTN# = *BLANKS
     C                             OR C4CTN# = '*SAME'
     C                   MOVE      CACTN#        C4CTN#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Call, the special value *GEN is only
      *          valid when adding.
      *          ------------------------------------------
     C                   IF        C4CTN# = C#GEN                               B01
     C                             AND P4ACTC <> '60'
     C                   MOVEL     'OMQA003'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     C#GEN         MC(1)
     C                   MOVEA     C#CTN#        MC(33)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#CTN#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          *SAME is this customer, not allowed when adding.
      *          ------------------------------------------
     C                   IF        C4CUSC = '*SAME'                             B01
     C                   IF        P4ACTC = '60'                                B02
     C                   MOVEL     'OMQA003'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     C#SAME        MC(1)
     C                   MOVEA     C#CUSC        MC(33)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#CUSC
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
     C                   MOVE      CACUSC        C4CUSC
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The following controls only if request is not deletion
      *          ------------------------------------------
     C                   IF        P4ACTC <> '40'                               B01
      *
      *          ------------------------------------------
      *          The customer must exist.
      *          ------------------------------------------
     C                   EXSR      SRUSL1
     C     C4CUSC        SETLL     OMCUSL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEL     'OMQA701'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     C4CUSC        MC(1)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#CUSC
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Default contact
      *          ------------------------------------------
     C                   IF        C4CCTC = C#DFT                               B03
     C                   EXSR      SRCOL1
     C     C4CUSC        CHAIN     OMCCOL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B04
      *
     C                   IF        COCNTI = *ON                                 B05
     C                   MOVE      COCCTC        C4CCTC
     C                   LEAVE
     C                   ENDIF                                                  E05
     C     C4CUSC        READE     OMCCOL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E04
     C                   ENDIF                                                  E03
      *
     C                   IF        C4CCTC = C#DFT                               B03
     C                   MOVEL     'OMQA702'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     C4CUSC        MC(1)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#CCTC
     C                   EXSR      SRERR
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          *SAME is this contact, not allowed when adding.
      *          ------------------------------------------
     C                   IF        C4CCTC = '*SAME'                             B04
     C                   IF        P4ACTC = '60'                                B05
     C                   MOVEL     'OMQA003'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     C#SAME        MC(1)
     C                   MOVEA     C#CCTC        MC(33)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#CCTC
     C                   EXSR      SRERR
     C                   ELSE                                                   X05
     C                   MOVE      CACCTC        C4CCTC
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          The contact must exist.
      *          ------------------------------------------
     C                   EXSR      SRCOL1
     C                   MOVE      C4CUSC        K#CUSC
     C                   MOVE      C4CCTC        K#CCTC
     C     $CO1K1        SETLL     OMCCOL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B04
     C                   MOVEL     'OMQA700'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     C4CCTC        MC(1)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#CCTC
     C                   EXSR      SRERR
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Income date; 555555 is *CURRENT
      *          ------------------------------------------
     C                   IF        C4INDT = 555555                              B02
     C                             OR C4INDT = 5555555                          B02
     C                   Z-ADD     W#SYDT        C4INDT
     C                   ELSE                                                   X02
     C                   IF        P4ACTC = '60'                                B03
     C                   Z-ADD     C4INDT        C4INDT
     C                   ELSE                                                   X03
     C                   Z-ADD     CAINDT        C4INDT
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Income time; 555555 is *CURRENT
      *          ------------------------------------------
     C                   IF        C4INTM = 555555                              B02
     C                   Z-ADD     W#SYTM        C4INTM
     C                   ELSE                                                   X02
     C                   IF        P4ACTC = '60'                                B03
     C                   Z-ADD     C4INTM        C4INTM
     C                   ELSE                                                   X03
     C                   Z-ADD     CAINTM        C4INTM
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Completion time; 555555 is *CURRENT
      *          ------------------------------------------
     C                   IF        C4RCTM = 555555                              B02
     C                             OR C4RCTM = 5555555                          B02
     C                   Z-ADD     W#SYTM        C4RCTM
     C                   ELSE                                                   X02
     C                   IF        P4ACTC = '20'                                B03
     C                             AND C4RCTM = 000000                          B02
     C                   Z-ADD     CARCTM        C4RCTM
     C                   ELSE                                                   X03
     C                   Z-ADD     C4RCTM        C4RCTM
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Call source 0=tel, 1=email, 2=fax, 3=WWW, 4=Notes
      *          ------------------------------------------
     C                   IF        C4CSRC <> '0'                                B02
     C                             AND C4CSRC <> '1'
     C                             AND C4CSRC <> '2'
     C                             AND C4CSRC <> '3'
     C                             AND C4CSRC <> '4'
     C                   MOVEA     C4CSRC        MC(1)
     C                   MOVEA     C#CSRC        MC(33)
     C                   MOVEA     MC            C4MSGD
     C                   MOVEL     'OMQB703'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   EVAL      W#FLD1 = C#CSRC
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          User defined field 1
      *          ------------------------------------------
     C                   IF        C4USF1 = '*SAME'                             B02
     C                   IF        P4ACTC = '60'                                B03
     C                   MOVE      *BLANKS       C4USF1
     C                   ELSE                                                   X03
     C                   MOVE      CAUSF1        C4USF1
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          User defined field 2
      *          ------------------------------------------
     C                   IF        C4USF2 = '*SAME'                             B02
     C                   IF        P4ACTC = '60'                                B03
     C                   MOVE      *BLANKS       C4USF2
     C                   ELSE                                                   X03
     C                   MOVE      CAUSF2        C4USF2
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          User defined field 3
      *          ------------------------------------------
     C                   IF        C4USFA = '*SAME'                             B02
     C                   IF        P4ACTC = '60'                                B03
     C                   EVAL      C4USF3 = *ZEROS
     C                   ELSE                                                   X03
     C                   MOVE      CAUSF3        C4USF3
     C                   ENDIF                                                  E03
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
      * SRINPT - Edit the input.                                    *
      *                                                             *
      * This routine edits the users input. If no input has been    *
      * given, the input is set to default values.                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRINPT        BEGSR
      *
      *          ------------------------------------------
      *          Does the HelpDesk exist?
      *          All actions, but already checked in SRSPKW.
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Call track number
      *          ------------------------------------------
      *                    May not be blanks.
      *                    --------------------------------
     C                   IF        C4CTN# = *BLANKS                             B01
     C                   MOVEL     'OMQ600A'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     VAR(2)        MC(11)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#CTN#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    May not exist if add.
      *                    --------------------------------
     C                   MOVE      C4HEDC        K#HEDC
     C                   MOVE      C4CTN#        K#CTN#
     C                   EXSR      SRCAL1
     C                   MOVE      C4HEDC        K#HEDC
     C                   MOVE      C4CTN#        K#CTN#
     C     $CA1K1        SETLL     OMCCAL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B01
     C                             AND P4ACTC = '60'
     C                   MOVEL     'OMQB704'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     C4CTN#        MC(1)
     C                   MOVEA     C4HEDC        MC(11)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#CTN#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    May not be *GEN when not add.
      *                    --------------------------------
     C                   IF        C4CTN# = '*GEN'                              B01
     C                             AND P4ACTC <> '60'
     C                   MOVEL     'OMQB703'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   EVAL      MC(1) = '*'
     C                   EVAL      MC(2) = 'G'
     C                   EVAL      MC(3) = 'E'
     C                   EVAL      MC(4) = 'N'
     C                   MOVEA     C#CTN#        MC(33)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#CTN#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    Must exist otherwise.
      *                    --------------------------------
     C                   IF        *IN99 <> *ON                                 B01
     C                             AND P4ACTC <> '60'
     C                   MOVEL     'OMQB702'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     C4HEDC        MC(1)
     C                   MOVEA     C4CTN#        MC(6)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#CTN#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    No incidents connected if delete
      *                    --------------------------------
     C                   IF        P4ACTC = '40'                                B01
     C                   EXSR      SRINC5
     C                   MOVE      C4HEDC        K#HEDC
     C                   MOVE      C4CTN#        K#CTN#
     C     $NC5K1        SETLL     OMINCJ5
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEL     'OMQB706'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     C4CTN#        MC(1)
     C                   MOVEA     C4HEDC        MC(11)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#CTN#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Skip other controls if request is deletion
      *          ------------------------------------------
     C                   IF        P4ACTC <> '40'                               B01
      *
      *          ------------------------------------------
      *          The registered by user id is filled with this
      *          user if this is add mode.
      *          ------------------------------------------
     C                   IF        C4USID = '*CURRENT'                          B02
     C                   MOVE      D#USID        C4USID
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          User defined programs.
      *          ------------------------------------------
     C                   IF        D8CTFP <> *BLANKS                            B02
     C                   IF        P4ACTC = '60'                                B03
     C                             OR P4ACTC = '20'
      *
      *                    --------------------------------
      *                    fill the parameters.
      *                    --------------------------------
     C                   MOVE      C4HEDC        P#HEDC
     C                   MOVE      C4CTN#        P#CTN#
      *
     C                   IF        P4ACTC = '20'                                B04
     C                   EVAL      P#ACTC = '20'
     C                   ELSE                                                   X04
     C                   EVAL      P#ACTC = '60'
     C                   ENDIF                                                  E04
      *
     C                   MOVEL(P)  D8CTFL        W#OBJQ
     C                   EVAL      W#OBJQ = %TRIMR(W#OBJQ) + '/'
     C     W#OBJQ        CAT       D8CTFP:0      W#OBJQ
     C                   EVAL      P#USF1 = C4USF1
     C                   EVAL      P#F1ST = '*NORM'
     C                   EVAL      P#USF2 = C4USF2
     C                   EVAL      P#F2ST = '*NORM'
     C                   EVAL      P#USF3 = C4USF3
     C                   EVAL      P#F3ST = '*NORM'
     C                   EVAL      P#STAT = '*NORM'
     C                   CALLP(E)  W#OBJQ_001 ( P#HEDC : P#CTN# : P#ACTC :
     C                             P#USF1 : P#F1ST : P#USF2 : P#F2ST :
     C                             P#USF3 : P#F3ST : P#STAT )
     C                   EVAL      *IN99 = %ERROR
      *
      *          ------------------------------------------
      *          Skip other tests if program not found
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF                                 B04
      *
      *                    --------------------------------
      *                    If exit then exit.
      *                    --------------------------------
     C                   IF        P#STAT = C#EXIT                              B05
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E05
      *
      *                    --------------------------------
      *                    Field 1 in error?
      *                    --------------------------------
     C                   IF        P#F1ST <> C#NORM                             B05
     C                   MOVEL     'OMQB703'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     C4USF1        MC(1)
     C                   MOVEA     C#USF1        MC(33)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#USF1
     C                   EXSR      SRERR
     C                   ENDIF                                                  E05
      *
      *                    --------------------------------
      *                    Field 2 in error?
      *                    --------------------------------
     C                   IF        P#F2ST <> C#NORM                             B05
     C                   MOVEL     'OMQB703'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     C4USF2        MC(1)
     C                   MOVEA     C#USF2        MC(33)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#USF2
     C                   EXSR      SRERR
     C                   ENDIF                                                  E05
      *
      *                    --------------------------------
      *                    Field 3 in error?
      *                    --------------------------------
     C                   IF        P#F3ST <> C#NORM                             B05
     C                   MOVEL     'OMQB703'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   Z-ADD     C4USF3        D#USF3
     C                   MOVEA     D#USFA        MC(1)
     C                   MOVEA     C#USF3        MC(33)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#USF3
     C                   EXSR      SRERR
     C                   ENDIF                                                  E05
      *
      *                    --------------------------------
      *                    Fill the fields with their new values
      *                    --------------------------------
     C                   MOVE      P#USF1        C4USF1
     C                   MOVE      P#USF2        C4USF2
     C                   MOVE      P#USF3        C4USF3
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
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
      * SRCHER - Change code unknown                                *
      *                                                             *
      * This routine is executed when an invalid change code        *
      * is in the buffer.                                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCHER        BEGSR
      *
      *          ------------------------------------------
      *          Fill message
      *          ------------------------------------------
     C                   MOVEL     'OMQ6003'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     P4ACTC        MC(1)
     C                   MOVEA     D#PGM         MC(3)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#ACTC
     C                   EXSR      SRERR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C     CHER99        TAG
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRERR  - Error found during checking                        *
      *                                                             *
      * This routine is executed when an error or warning was       *
      * detected during checking. When the buffer contains a        *
      * message queue, the message is send to that queue and        *
      * processing will continue. Otherwise we will return to       *
      * the caller.                                                 *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRERR         BEGSR
      *
      *          ------------------------------------------
      *          Send message when C4MSQ is filled
      *          ------------------------------------------
     C                   IF        C4MSQ <> *BLANKS                             B01
     C     W#FLD1        LOOKUP    ERR(1)                                 99
     C                   IF        *IN99 <> *ON                                 B02
     C                   ADD       1             IR
     C                   EVAL      ERR(IR) = W#FLD1
     C                   ENDIF                                                  E02
     C                   IF        W#FLD2 <> *BLANKS                            B02
     C     W#FLD2        LOOKUP    ERR(1)                                 99
     C                   IF        *IN99 <> *ON                                 B03
     C                   ADD       1             IR
     C                   EVAL      ERR(IR) = W#FLD2
     C                   ENDIF                                                  E03
     C                   IF        W#FLD3 <> *BLANKS                            B03
     C     W#FLD3        LOOKUP    ERR(1)                                 99
     C                   IF        *IN99 <> *ON                                 B04
     C                   ADD       1             IR
     C                   EVAL      ERR(IR) = W#FLD3
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Send program message
      *          ------------------------------------------
     C                   EVAL      P#MIDC = C4MSID
     C                   EVAL      P#MSGQ = C#API
     C                   EVAL      P#MDL# = 125
     C                   EVAL      P#MSTC = '*DIAG'
     C                   EVAL      P#PMQC = C4MSQ
     C                   EVAL      P#PSC# = 0
     C                   CALLP     QMHSNDPM ( P#MIDC : P#MSGQ : C4MSGD :
     C                             P#MDL# : P#MSTC : P#PMQC : P#PSC# :
     C                             P#MSKC : P#ERR )
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do only when an error occured
      *          ------------------------------------------
     C                   IF        C4MSTP = 'E'                                 B01
      *
      *          ------------------------------------------
      *          Unlock a possible locked record
      *          ------------------------------------------
     C                   UNLOCK    OMCCAL1                              99
      *
      *          ------------------------------------------
      *          Count the error number
      *          ------------------------------------------
     C                   ADD       1             W#ERR
      *
      *          ------------------------------------------
      *          Execute TTLR when no message queue is used
      *          ------------------------------------------
     C                   IF        C4MSQ = *BLANKS                              B02
     C                             OR I#QUIT = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Reset the message type
      *          ------------------------------------------
     C                   MOVE      *BLANK        C4MSTP
     C                   MOVE      *BLANK        C4MSGD
     C                   EVAL      MC = *BLANK
     C                   EVAL      W#FLD1 = *BLANK
     C                   EVAL      W#FLD2 = *BLANK
     C                   EVAL      W#FLD3 = *BLANK
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C     ERR99         TAG
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SR---- - Various subroutines to open files.                 *
      *                                                             *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Open OMHDDL1
      *          ------------------------------------------
     C     SRDDL1        BEGSR
     C                   IF        I#DDL1 <> *ON                                B01
     C                   EVAL      I#DDL1 = *ON
     C                   OPEN      OMHDDL1
     C                   ENDIF                                                  E01
     C                   ENDSR
      *
      *          ------------------------------------------
      *          Open OMCUSL1.
      *          ------------------------------------------
     C     SRUSL1        BEGSR
     C                   IF        I#USL1 <> *ON                                B01
     C                   OPEN      OMCUSL1
     C                   EVAL      I#USL1 = *ON
     C                   ENDIF                                                  E01
     C                   ENDSR
      *
      *          ------------------------------------------
      *          Open OMCCOL1
      *          ------------------------------------------
     C     SRCOL1        BEGSR
     C                   IF        I#COL1 <> *ON                                B01
     C                   EVAL      I#COL1 = *ON
     C                   OPEN      OMCCOL1
     C                   ENDIF                                                  E01
     C                   ENDSR
      *
      *          ------------------------------------------
      *          Open OMCCAL1
      *          ------------------------------------------
     C     SRCAL1        BEGSR
     C                   IF        I#CAL1 <> *ON                                B01
     C                   EVAL      I#CAL1 = *ON
     C                   OPEN      OMCCAL1
     C                   ENDIF                                                  E01
     C                   ENDSR
      *
      *          ------------------------------------------
      *          Open OMINCJ5
      *          ------------------------------------------
     C     SRINC5        BEGSR
     C                   IF        I#NCJ5 <> *ON                                B01
     C                   EVAL      I#NCJ5 = *ON
     C                   OPEN      OMINCJ5
     C                   ENDIF                                                  E01
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIO   - I/O to OMCCAR.                                     *
      *                                                             *
      * This routine does the actual update. It also updates the    *
      * buffer depending on the indicator.                          *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIO          BEGSR
      *
      *          ------------------------------------------
      *          Must the buffer be updated.
      *          ------------------------------------------
     C                   IF        C4UPB = *ON                                  B01
     C                   MOVEL     DSCCAR        P4BUF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Must the database be updated.
      *          ------------------------------------------
     C                   IF        C4UPD = *ON                                  B01
      *
      *          ------------------------------------------
      *          Must the record be written.
      *          ------------------------------------------
     C                   SELECT                                                 B02
     C                   WHEN      P4ACTC = '60'                                X02
     C                   WRITE     OMCCAR
      *
      *          ------------------------------------------
      *          Must the record be removed.
      *          ------------------------------------------
     C                   WHEN      P4ACTC = '40'                                X02
     C                   DELETE    OMCCAR
      *
      *          ------------------------------------------
      *          Must the record be updated
      *          ------------------------------------------
     C                   OTHER                                                  X02
     C                   UPDATE    OMCCAR
     C                   ENDSL                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRINIT - Program initialisation                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRINIT        BEGSR
      *
      *          ------------------------------------------
      *          Check for valid Interface level
      *          ------------------------------------------
     C                   IF        P4INTL <> 'V3R0M0  '                         B01
     C                   MOVEL     'OMQ500B'     C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVEA     D#PGM         MC(1)
     C                   MOVEA     P4INTL        MC(11)
     C                   MOVEA     MC            C4MSGD
     C                   EVAL      W#FLD1 = C#INTL
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Init the error count and error array
      *          ------------------------------------------
     C                   EVAL      W#ERR = 0
     C                   EVAL      ERR = *BLANKS
     C                   EVAL      IR = 0
     C                   EVAL      I#QUIT = *OFF
      *
      *          ------------------------------------------
      *          Calculate the system date.
      *          ------------------------------------------
     C                   EVAL      D#DATE = *ZEROS
     C                   EVAL      D#YY = UYEAR
     C                   EVAL      D#MM = UMONTH
     C                   EVAL      D#DD = UDAY
     C                   IF        D#YY < 39                                    B01
     C                   EVAL      D#CC = 1
     C                   ENDIF                                                  E01
     C                   EVAL      W#SYDT = D#DATE
     C                   EVAL      W#SYTM = %DEC(%TIME)
     C                   EVAL      D#TIME = %DEC(%TIME)
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C     INIT99        TAG
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * *INZSR - Program initialisation                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Indicator definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
     C                   EVAL      P#OPTI = *BLANK
      *
      *          ------------------------------------------
      *          Savefield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Keyfield definition
      *          ------------------------------------------
      *
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      W#ERR = *ZEROS
     C                   EVAL      IR = *ZEROS
     C                   EVAL      IE = *ZEROS
     C                   EVAL      W#50 = *ZEROS
     C                   EVAL      W#FLD1 = *BLANK
     C                   EVAL      W#FLD2 = *BLANK
     C                   EVAL      W#FLD3 = *BLANK
     C                   EVAL      W#OBJQ = *BLANKS
      *
      *          ------------------------------------------
      *          IBM API field definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *
      *          ------------------------------------------
      *          Parameter list for OMX909 (Check date)
      *          ------------------------------------------
     C     $MX909        PLIST
     C                   PARM                    P#CHKD
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMX967 (Generate CTN#)
      *          ------------------------------------------
     C     $MX967        PLIST
     C                   PARM      C4HEDC        P#HEDC
     C                   PARM      '2'           P#OPTI
     C     C4CTN#        PARM                    P#CTN#
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Plist for API QMHRTVM (Rtv msg text)
      *          ------------------------------------------
     C     $MHRTV        PLIST
     C                   PARM                    D#RTVM
     C                   PARM                    P#MDL#
     C                   PARM                    P#FMTC
     C                   PARM                    P#MIDC
     C                   PARM                    P#MSQL
     C                   PARM                    P#DUM6
     C                   PARM                    P#LOB#
     C                   PARM                    P#DUM7
     C                   PARM                    P#DUM8
     C                   PARM                    P#ERR
      *
      *          ------------------------------------------
      *          Parameter list for OMH029 (Get info)
      *          ------------------------------------------
     C     $MH029        PLIST
     C                   PARM                    P#OPTI
     C                   PARM                    P#HEDC
     C                   PARM                    P#SUL#
     C                   PARM                    P#STAT
     C                   PARM                    D#D7HD
     C                   PARM                    D#D8HD
      *
      *          ------------------------------------------
      *          Check user fields
      *          ------------------------------------------
     C     $OBJQ         PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    P#CTN#
     C                   PARM                    P#ACTC
     C                   PARM      C4USF1        P#USF1
     C                   PARM      '*NORM'       P#F1ST
     C                   PARM      C4USF2        P#USF2
     C                   PARM      '*NORM'       P#F2ST
     C                   PARM      C4USF3        P#USF3
     C                   PARM      '*NORM'       P#F3ST
     C                   PARM      '*NORM'       P#STAT
      *
      *          ------------------------------------------
      *          OMCUSL1 Customer database
      *          ------------------------------------------
     C     $US1K1        KLIST
     C                   KFLD                    K#CUSC
      *
      *          ------------------------------------------
      *          OMCCOL1 Customer contact database
      *          ------------------------------------------
     C     $CO1K1        KLIST
     C                   KFLD                    K#CUSC
     C                   KFLD                    K#CCTC
      *
      *          ------------------------------------------
      *          OMINCJ5 Incident database
      *          ------------------------------------------
     C     $NC5K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#CTN#
      *
      *          ------------------------------------------
      *          OMCCAL1 Call track numbers
      *          ------------------------------------------
     C     $CA1K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#CTN#
      *
      *          ------------------------------------------
      *          No parameters!
      *          Set IE to high values to avoid a loop in TTLR
      *          ------------------------------------------
     C                   IF        D#PARM = *ZERO                               B01
     C                   EVAL      IE = 999
     C                   ELSE                                                   X01
     C                   EXSR      SRGVAR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * *PSSR  - Error handler                                      *
      *                                                             *
      * Standard RPG error handler.                                 *
      * Make sure the data structure PGM is defined as the          *
      * program status data structure (SDS) with a length of 429.   *
      * The file information data structure is also passed, but     *
      * not the complete data structure, only the relevant info.    *
      * After the program is called, the messages in the queue      *
      * of this program are passes back to the queue of the         *
      * calling program and the *CANCL procedure is invoked.        *
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
     C                   WHEN      E#FILE = 'OMHDDL1 '                          X02
     C                   MOVEL     HDDL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMCUSL1 '                          X02
     C                   MOVEL     CUSL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMCCOL1 '                          X02
     C                   MOVEL     CCOL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMCCAL1 '                          X02
     C                   MOVEL     CCAL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMINCJ5 '                          X02
     C                   MOVEL     INCJ5         P#FILE
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
     C                   EVAL      D#PARM = 0
     C                   MOVE      'E'           C4MSTP
     C                   MOVE      'OMQ5012'     C4MSID
     C                   MOVEL     C4ERR         P4ERR
     C                   EVAL      D#PARM = 0
     C                   EXSR      SRTTLR
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
      *          Fill output parameter fields
      *          ------------------------------------------
     C                   IF        D#PARM <> *ZERO                              B01
     C                   MOVEL     C4ERR         P4ERR
     C                   MOVEA     ERR           C4FLDA
     C                   MOVEL     C4FLD         P4FLD
     C                   IF        C4UPB <> *OFF                                B02
     C                   MOVEL     C4BUF         P4BUF
     C                   MOVEL     C4PRC         P4PRC
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          When multiple messages are send, fill 'E'
      *          ------------------------------------------
     C                   IF        W#ERR > *ZERO                                B02
     C                             AND C4MSQ <> *BLANK
     C                   MOVEL     *BLANK        C4MSID
     C                   MOVEL     'E'           C4MSTP
     C                   MOVE      *BLANKS       C4MSGD
     C                   MOVEL     C4ERR         P4ERR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          When no parm or C4EOP = '1' then close
      *          ------------------------------------------
     C                   IF        D#PARM = *ZERO                               B01
     C                             OR C4EOP = '1'
      *
      *          ------------------------------------------
      *          Close number generator
      *          ------------------------------------------
     C                   IF        I#X967 = *ON                                 B02
     C                   CALLP     OMX967()
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Seton LR
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Return to the caller
      *          ------------------------------------------
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGVAR  -  Get variable message text                         *
      *                                                              *
      * This subroutine gets the data needed for this program to     *
      * retrieve the data wich is filled in message texts.           *
      * It is called one time per program execution.                 *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGVAR        BEGSR
      *
      *          ------------------------------------------
      *          Initialize fields for message retrieve
      *          ------------------------------------------
     C                   Z-ADD     156           P#MDL#
     C                   MOVEL     'RTVM0100'    P#FMTC
     C                   EVAL      %SUBST(P#MSQL:1:15) = C#MSG1
     C                   EVAL      %SUBST(P#DUM7:1:3) = '*NO'
     C                   EVAL      %SUBST(P#DUM8:1:3) = '*NO'
     C                   Z-ADD     1             P#LOB#
      *
      *          ------------------------------------------
      *          Retrieve message variables
      *          ------------------------------------------
     C                   IF        VAR(1) = *BLANK                              B01
     C                   FOR       MS = 1 TO 7                                  B02
     C                   IF        MSD(MS) <> *BLANKS                           B03
     C                   EVAL      D#MDTA = *BLANK
     C                   MOVE      MSD(MS)       P#MIDC
     C                   CALLP     QMHRTVM ( D#RTVM : P#MDL# : P#FMTC :
     C                             P#MIDC : P#MSQL : P#DUM6 : P#LOB# :
     C                             P#DUM7 : P#DUM8 : P#ERR )
     C                   EVAL      VAR(MS) = D#MDTA
     C                   ENDIF                                                  E03
     C                   ENDFOR                                                 E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
**
BLA0002  -  *BLANK                 1
HLP0006  -  call track number      2
