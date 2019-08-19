     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMQCHGIN                                      *
      * Function ...: API Maintain Incident                         *
      *                                                             *
      * Author .....: Remain Software                               *
      * Parameters .: P4INTL  - Interface level         (I)         *
      *               P4ACTC  - Activity code           (I)         *
      *               P4BUF   - Buffer data             (I/O)       *
      *               P4PRC   - Processing array        (I)         *
      *               P4ERR   - Error data              (O)         *
      *               P4FLD   - Field error data        (I/O)       *
      *                                                             *
      * This program is used to add, change or delete an Incident.  *
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
      * I4XXXX - Input buffer data structure                        *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMHDDL1   IF   E           K DISK    INFDS(HDDL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMINCL1   UF A E           K DISK    INFDS(INCL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMINCL4   IF   E           K DISK    INFDS(INCL4)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     F                                     RENAME(OMINCR:INCL4R)
     FOMRIRL1   UF A E           K DISK    INFDS(RIRL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMSREL1   IF   E           K DISK    INFDS(SREL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMHDSL1   IF   E           K DISK    INFDS(HDSL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMHDSL3   IF   E           K DISK    INFDS(HDSL3)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     F                                     RENAME(OMHDSR:HDSL3R)
     FOMHITL1   IF   E           K DISK    INFDS(HITL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMCCAL1   IF   E           K DISK    INFDS(CCAL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMHDEL1   IF   E           K DISK    INFDS(HDEL1)
     F                                     INFSR(*PSSR)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension spec's                                             *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * STS   Contains APPC/FIX# or ERR# for resetting status        *
      * ERR   Max. 30 elements of field errors, during checking      *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(15)
     D MSD             S              7    DIM(15) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
     D STS             S             16    DIM(999)
     D ERR             S              4    DIM(30)
     D ENV             S              5    DIM(49)
     D EN#             S              5  0 DIM(49)
     D AUT             S              1    DIM(49)
     D LVC             S              1    DIM(49)
     D LV#             S              3  0 DIM(49)
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
      *          The incident file as data structure.
      *          ------------------------------------------
     D DSINCR        E DS                  EXTNAME(OMINC) INZ
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
     D INCL1           DS            57
     D INCL4           DS            57
     D ERRL1           DS            57
     D RIRL1           DS            57
     D SREL1           DS            57
     D HDSL1           DS            57
     D HDSL3           DS            57
     D HITL1           DS            57
     D CCAL1           DS            57
     D HDEL1           DS            57
      *
      *          ------------------------------------------
      *          Incident API buffer parameter
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
     D D#BI4#                         9B 0
     D  D#BI4A                        4    OVERLAY(D#BI4#)
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
      *          All buffers for OMQCHGIN
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQCHGINDS
      *
      *          ------------------------------------------
      *          Extra redefinitions of buffer
      *          ------------------------------------------
     D  I4RESA                72     73
     D  I4USFA               200    207
      *
      *          ------------------------------------------
      *          Data queue incident buffer
      *          ------------------------------------------
      /COPY QCPYLESRC,OMH044IN
      *
      *          ------------------------------------------
      *          Structure for holding delayed propagation.
      *          ------------------------------------------
     D D#PDI           DS          1024    OCCURS(25)
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
     D C#INC#          C                   CONST('INC#')
     D C#CTN#          C                   CONST('CTN#')
     D C#AIN#          C                   CONST('AIN#')
     D C#SHID          C                   CONST('SHID')
     D C#INDT          C                   CONST('INDT')
     D C#INTM          C                   CONST('INTM')
     D C#RCDT          C                   CONST('RCDT')
     D C#RCTM          C                   CONST('RCTM')
     D C#STRC          C                   CONST('ROUT')
     D C#RES#          C                   CONST('RSEQ')
     D C#SUL#          C                   CONST('SUL#')
     D C#STAT          C                   CONST('STAT')
     D C#SRDT          C                   CONST('SRDT')
     D C#SRTM          C                   CONST('SRTM')
     D C#SEV1          C                   CONST('SEV1')
     D C#SEV2          C                   CONST('SEV2')
     D C#SEV3          C                   CONST('SEV3')
     D C#PRI#          C                   CONST('PRI#')
     D C#ESC#          C                   CONST('ESC#')
     D C#RUID          C                   CONST('RUID')
     D C#AUID          C                   CONST('AUID')
     D C#ITPC          C                   CONST('ITPC')
     D C#CFL#          C                   CONST('CFL#')
     D C#CFIC          C                   CONST('CFIC')
     D C#USF1          C                   CONST('USF1')
     D C#USF2          C                   CONST('USF2')
     D C#USF3          C                   CONST('USF3')
     D C#THED          C                   CONST('THED')
     D C#TINC          C                   CONST('TINC')
     D C#TAPP          C                   CONST('TAPP')
     D C#TREQ          C                   CONST('TREQ')
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
      *          Parameter list for OMH044C (Write entry in data queue)
      * Prototype for 'QMHSNDPM'
     D QMHSNDPM        PR                  EXTPGM('QMHSNDPM')
     D P#MIDC_                             LIKE(P#MIDC)
     D P#MSGQ_                             LIKE(P#MSGQ)
     D I4MSGD_                             LIKE(I4MSGD)
     D P#MDL#_                             LIKE(P#MDL#)
     D P#MSTC_                             LIKE(P#MSTC)
     D P#PMQC_                             LIKE(P#PMQC)
     D P#PSC#_                             LIKE(P#PSC#)
     D P#MSKC_                             LIKE(P#MSKC)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'OMH044C'
     D OMH044C         PR                  EXTPGM('OMH044C')
     D P#OBJC_                             LIKE(P#OBJC)
     D P#OBJL_                             LIKE(P#OBJL)
     D P#OPTI_                             LIKE(P#OPTI)
     D D#44IN_                             LIKE(D#44IN)
     D P#LEN_                              LIKE(P#LEN)
     D P#WAIT_                             LIKE(P#WAIT)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Check user fields
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
      * Prototype for 'OMX929'
     D OMX929          PR                  EXTPGM('OMX929')
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
     D k#prl#          S              3P 0
     D k#res#          S              3P 0
     D p#usf3          S             15P 0
     D w#sew#          S              3P 0
     D w#sydt          S              7P 0
     D w#sytm          S              6P 0
     D APPC            S              5
     D ERR#            S             10
     D D#1UL#          S                   LIKE(SUL#)
     D D#2UL#          S                   LIKE(SUL#)
     D I#ACCU          S                   LIKE(INDI)
     D I#CAL1          S              1
     D I#DDL1          S              1
     D I#DSL1          S              1
     D I#DSL3          S              1
     D I#IRL1          S              1
     D I#ITL1          S              1
     D I#NCL1          S              1
     D I#NCL4          S              1
     D I#PSSR          S              1
     D I#QUIT          S                   LIKE(INDI)
     D I#REL1          S              1
     D I#RRL1          S              1
     D I#X929          S                   LIKE(INDI)
     D I#X967          S                   LIKE(INDI)
     D IE              S              5  0
     D IR              S              3  0
     D K#APPC          S                   LIKE(APPC)
     D K#CTN#          S                   LIKE(CTN#)
     D K#EOEI          S                   LIKE(EOEI)
     D K#ERR#          S                   LIKE(ERR#)
     D K#HEDC          S                   LIKE(HEDC)
     D K#INC#          S                   LIKE(INC#)
     D K#IRIT          S                   LIKE(IRIT)
     D K#IRTP          S                   LIKE(IRTP)
     D K#ITPC          S                   LIKE(ITPC)
     D K#SEVC          S                   LIKE(SEVC)
     D K#STAT          S                   LIKE(STAT)
     D K#STRC          S                   LIKE(STRC)
     D K#SUL#          S                   LIKE(SUL#)
     D K#TABC          S                   LIKE(TABC)
     D K#USID          S                   LIKE(USID)
     D MS              S              3  0
     D P#ACTC          S                   LIKE(ACTC)
     D P#CHKD          S              7  0
     D P#CTN#          S                   LIKE(CTN#)
     D P#DUM6          S              1
     D P#DUM7          S             10
     D P#DUM8          S             10
     D P#EOEI          S                   LIKE(EOEI)
     D P#FILE          S             57
     D P#F1ST          S                   LIKE(STAT)
     D P#F2ST          S                   LIKE(STAT)
     D P#F3ST          S                   LIKE(STAT)
     D P#GRCL          S                   LIKE(USID)
     D P#GRID          S                   LIKE(USID)
     D P#HEDC          S                   LIKE(HEDC)
     D P#INC#          S                   LIKE(INC#)
     D P#LEN           S              5  0
     D P#MSGD          S                   LIKE(MSGD)
     D P#MSGF          S                   LIKE(MSGF)
     D P#MSGN          S                   LIKE(MSID)
     D P#MSID          S                   LIKE(MSID)
     D P#MSQL          S             20
     D P#MSTA          S             40
     D P#OBJC          S                   LIKE(OBJC)
     D P#OBJL          S                   LIKE(OBJL)
     D P#OPTI          S              1
     D P#RMTI          S                   LIKE(INDI)
     D P#STAN          S                   LIKE(STAT)
     D P#STAO          S                   LIKE(STAT)
     D P#STAT          S                   LIKE(STAT)
     D P#STRN          S                   LIKE(STRC)
     D P#STRO          S                   LIKE(STRC)
     D P#SUL#          S                   LIKE(SUL#)
     D P#SULN          S                   LIKE(INDI)
     D P#SULO          S                   LIKE(INDI)
     D P#USCL          S                   LIKE(USID)
     D P#USDC          S                   LIKE(USDC)
     D P#USF1          S                   LIKE(USF1)
     D P#USF2          S                   LIKE(USF2)
     D P#USID          S                   LIKE(USID)
     D P#WAIT          S              5  0
     D PI              S              5  0
     D P4ACTC          S              2
     D P4ERR           S            136
     D P4FLD           S            130
     D P4INTL          S              8
     D P4PRC           S             40
     D S#AUID          S                   LIKE(AUID)
     D S#CTN#          S                   LIKE(CTN#)
     D S#PRC           S                   LIKE(P4PRC)
     D S#STAT          S                   LIKE(STAT)
     D S#STRC          S                   LIKE(STRC)
     D S#STS2          S                   LIKE(STAT)
     D S#SUL#          S                   LIKE(SUL#)
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
     C                   MOVEL     P4BUF         I4BUF
     C                   MOVEL     P4PRC         I4PRC
     C                   MOVEL     P4FLD         I4FLD
     C                   MOVE      *BLANKS       I4ERR
     C                   MOVE      *BLANKS       I4FLDA
      *
      *          ------------------------------------------
      *          Call program to get support level
      *          ------------------------------------------
     C                   EVAL      P#OPTI = '3'
     C                   EVAL      P#SUL# = '0'
     C                   MOVE      I4HEDC        P#HEDC
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD )
      *
      *          ------------------------------------------
      *          HelpDesk not found.
      *          ------------------------------------------
     C                   IF        P#STAT <> C#NORM                             B02
     C                   MOVEL     'OMQ5028'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4HEDC        MC(1)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#HEDC
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Do the processing routine if all is clear.
      *          ------------------------------------------
     C                   EVAL      I#ACCU = *OFF
     C                   EXSR      SRVERW
      *
      *          ------------------------------------------
      *          Check if this is an accumulated incident.
      *          ------------------------------------------
     C                   EVAL      I#ACCU = *ON
     C                   EXSR      SRACCU
     C                   ENDIF                                                  E02
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
     C                   WHEN      P4ACTC = '15'                                Allocate
     C                   EXSR      SRCH15                                       Allocate
     C                   WHEN      P4ACTC = '20'                                Change
     C                   EXSR      SRCH20                                       Change
     C                   WHEN      P4ACTC = '23'                                Connect
     C                   EXSR      SRCH23                                       Connect
     C                   WHEN      P4ACTC = '25'                                Accumulate
     C                   EXSR      SRCH25                                       Accumulate
     C                   WHEN      P4ACTC = '27'                                Disconnect
     C                   EXSR      SRCH27                                       Disconnect
     C                   WHEN      P4ACTC = '09'                                Re-Open  <--
     C                   EXSR      SRCH29                                       Re-Open  <--
     C                   WHEN      P4ACTC = '29'                                Status
     C                   EXSR      SRCH29                                       Status
     C                   WHEN      P4ACTC = '30'                                Copy
     C                   EXSR      SRCH30                                       Copy
     C                   WHEN      P4ACTC = '33'                                Escalate
     C                   EXSR      SRCH33                                       Escalate
     C                   WHEN      P4ACTC = '42'                                Complete
     C                   EXSR      SRCH42                                       Complete
     C                   WHEN      P4ACTC = '60'                                Add
     C                   EXSR      SRCH60                                       Add
     C                   WHEN      P4ACTC = '85'                                Deallocate
     C                   EXSR      SRCH85                                       Deallocate
     C                   WHEN      P4ACTC = '87'                                OMS
     C                   EXSR      SRCH87                                       OMS
     C                   OTHER
     C                   EXSR      SRCHER
     C                   ENDSL
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRACCU - Accumulated processing                             *
      *                                                             *
      * This routine determines if the incident is an accumulated   *
      * incident. If it is then the underlying incidents are        *
      * processed also. This is only true for the following action  *
      * codes: 29=Status, 33=Escalate, 42=Complete, 09=Re-open      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRACCU        BEGSR
      *
      *          ------------------------------------------
      *          Only if certain action codes
      *          ------------------------------------------
     C                   IF        P4ACTC = '09'                                B01
     C                             OR P4ACTC = '29'
     C                             OR P4ACTC = '33'
     C                             OR P4ACTC = '42'
      *
      *          ------------------------------------------
      *          Only if an accumulated request
      *          ------------------------------------------
     C                   IF        NCAIN# = *BLANKS                             B02
      *
      *          ------------------------------------------
      *          Save the current buffers.
      *          ------------------------------------------
     C                   MOVEL     I4BUF         S#BUF
     C                   MOVEL     I4PRC         S#PRC
      *
      *          ------------------------------------------
      *          Open the incident file.
      *          ------------------------------------------
     C                   EXSR      SRNCL4
      *
      *          ------------------------------------------
      *          Read all records and verwerk them.
      *          ------------------------------------------
     C                   MOVE      '3'           I4PCM                          *ABS
     C                   MOVE      '4'           I4CCM                          *ABS
     C                   MOVE      I4INC#        K#INC#
     C     $NC1K1        CHAIN     OMINCL4
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B03
     C                   MOVE      NCINC#        I4INC#
     C                   EXSR      SRVERW
     C                   MOVE      NCAIN#        K#INC#
     C     $NC1K1        READE     OMINCL4
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E03
      *
      *          ------------------------------------------
      *          Restore the current buffers.
      *          ------------------------------------------
     C                   MOVEL     S#BUF         I4BUF
     C                   MOVEL     S#PRC         I4PRC
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
      * SRCH15 - Allocate incident.                                 *
      *                                                             *
      * This routine is executed when the incident must be          *
      * allocated to an HelpDesk employee.                          *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCH15        BEGSR
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
      *          Get the record & update the allocated_by user.
      *          ------------------------------------------
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4INC#        K#INC#
     C     $NC1K1        CHAIN     OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      I4AUID        NCAUID
      *
      *          ------------------------------------------
      *          Save the status
      *          ------------------------------------------
     C                   MOVEL     NCSTAT        S#STS2
     C                   MOVEL     NCSUL#        S#SUL#
     C                   MOVEL     NCSTRC        S#STRC
      *
     C                   EXSR      SRIO
      *
      *          ------------------------------------------
      *          Write a log record.
      *          ------------------------------------------
     C                   IF        I4UPD = *ON                                  B01
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   EVAL      P#ACTC = '15'
     C                   EVAL      P#MSID = 'OMQA006'
     C                   MOVEA     NCINC#        MC(1)
     C                   MOVEA     NCHEDC        MC(11)
     C                   MOVEA     D#USID        MC(16)
     C                   MOVEA     NCAUID        MC(26)
     C                   MOVEA     MC            P#MSGD
     C                   CALL      'OMX929'      $MX929                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X929 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X929 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Write in queue
      *          ------------------------------------------
     C                   EXSR      SRWDAQ
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCH20 - Change code 20 Change Incident                     *
      *                                                             *
      * This routine is executed when the program has to change     *
      * an Incident.                                                *
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
      *          Get the incident record.
      *          ------------------------------------------
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4INC#        K#INC#
     C     $NC1K1        CHAIN     OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Save statusfield
      *          ------------------------------------------
     C                   MOVEL     NCSTAT        S#STS2
     C                   MOVEL     NCSUL#        S#SUL#
     C                   MOVEL     NCSTRC        S#STRC
      *
      *          ------------------------------------------
      *          Fill the record and update
      *          ------------------------------------------
     C                   MOVE      I4CTN#        NCCTN#
     C                   MOVE      I4AIN#        NCAIN#
     C                   MOVE      I4INDT        NCINDT
     C                   MOVE      I4INTM        NCINTM
     C                   MOVE      I4RCDT        NCRCDT
     C                   MOVE      I4RCTM        NCRCTM
     C                   MOVE      I4STRC        NCSTRC
     C                   MOVE      I4RES#        NCRES#
     C                   MOVE      I4SUL#        NCSUL#
     C                   MOVE      I4STAT        NCSTAT
     C                   MOVE      I4SRDT        NCSRDT
     C                   MOVE      I4SRTM        NCSRTM
     C                   MOVE      I4SEV1        NCSEV1
     C                   MOVE      I4SEV2        NCSEV2
     C                   MOVE      I4SEV3        NCSEV3
     C                   MOVE      I4PRI#        NCPRI#
     C                   MOVE      I4ESC#        NCESC#
     C                   MOVE      I4RUID        NCRUID
     C                   MOVE      I4AUID        NCAUID
     C                   MOVE      I4ITPC        NCITPC
     C                   MOVE      I4CFL#        NCCFL#
     C                   MOVE      I4CFIC        NCCFIC
     C                   MOVE      I4USF1        NCUSF1
     C                   MOVE      I4USF2        NCUSF2
     C                   MOVE      I4USF3        NCUSF3
     C                   MOVE      I4SHID        NCSHID
     C                   EXSR      SRIO
      *
      *          ------------------------------------------
      *          Write a log record.
      *          ------------------------------------------
     C                   IF        I4UPD = *ON                                  B01
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   EVAL      P#ACTC = '20'
     C                   EVAL      P#MSID = 'OMQA007'
     C                   MOVEA     NCINC#        MC(1)
     C                   MOVEA     NCHEDC        MC(11)
     C                   MOVEA     D#USID        MC(16)
     C                   MOVEA     MC            P#MSGD
     C                   CALL      'OMX929'      $MX929                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X929 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X929 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Write in queue
      *          ------------------------------------------
     C                   EXSR      SRWDAQ
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCH23 - Connect to a incident                              *
      *                                                             *
      * This routine is executed when an incident must be connected *
      * to the incident. A record is written in the incident        *
      * relationships file.                                         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCH23        BEGSR
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
      *          Write an incident relationship if update allowed.
      *          ------------------------------------------
     C                   IF        I4UPD = *ON                                  B01
     C                   MOVE      I4HEDC        IRHEDC
     C                   MOVE      I4INC#        IRINC#
     C                   MOVE      'I'           IRIRTP
     C                   MOVE      I4THED        IRIRAH
     C                   MOVEL(P)  I4TINC        IRIRIT
     C                   MOVE      IRESTS        IRESTS
     C                   WRITE     OMRIRR
      *
      *          ------------------------------------------
      *          Save the status
      *          ------------------------------------------
     C                   MOVEL     NCSTAT        S#STS2
     C                   MOVEL     NCSUL#        S#SUL#
     C                   MOVEL     NCSTRC        S#STRC
      *
      *          ------------------------------------------
      *          Write a log record.
      *          ------------------------------------------
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   EVAL      P#ACTC = '23'
     C                   EVAL      P#MSID = 'OMQA012'
     C                   MOVEA     NCINC#        MC(1)
     C                   MOVEA     NCHEDC        MC(11)
     C                   MOVEA     I4TINC        MC(16)
     C                   MOVEA     I4THED        MC(26)
     C                   MOVEA     MC            P#MSGD
     C                   CALL      'OMX929'      $MX929                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X929 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X929 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Write in queue
      *          ------------------------------------------
     C                   EXSR      SRWDAQ
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCH25 - Accumulate.                                        *
      *                                                             *
      * This routine is executed when an incident must be connected *
      * to the incident. This is the accumulation process which     *
      * binds this incident to the accumulated incident. If the     *
      * accumulated incident changes state, the bound incidents     *
      * also change state.                                          *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCH25        BEGSR
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
      *          Get the accumulated incident if *CONNECT
      *          ------------------------------------------
     C                   IF        I4RCM = '0'                                  B01
     C                   DOU       NCAIN# = *BLANKS                             B02
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4AIN#        K#INC#
     C     $NC1K1        CHAIN     OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        NCAIN# <> *BLANKS                            B03
     C                   MOVE      NCAIN#        I4AIN#
     C                   ENDIF                                                  E03
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the fields needed for conversion
      *          ------------------------------------------
     C                   IF        I4RCM = '0'                                  B01
     C                   MOVE      NCSTRC        I4STRC
     C                   MOVE      NCRES#        I4RES#
     C                   MOVE      NCSUL#        I4SUL#
     C                   MOVE      NCSTAT        I4STAT
     C                   MOVE      NCSRDT        I4SRDT
     C                   MOVE      NCSRTM        I4SRTM
     C                   MOVE      NCPRI#        I4PRI#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the record & update the status fields.
      *          ------------------------------------------
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4INC#        K#INC#
     C     $NC1K1        CHAIN     OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Save the status
      *          ------------------------------------------
     C                   MOVEL     NCSTAT        S#STS2
     C                   MOVEL     NCSUL#        S#SUL#
     C                   MOVEL     NCSTRC        S#STRC
      *
      *          ------------------------------------------
      *          Update the incident if *CONNECT.
      *          ------------------------------------------
     C                   IF        I4RCM = '0'                                  B01
     C                   MOVE      I4AIN#        NCAIN#
     C                   MOVE      I4STRC        NCSTRC
     C                   MOVE      I4RES#        NCRES#
     C                   MOVE      I4SUL#        NCSUL#
     C                   MOVE      I4STAT        NCSTAT
     C                   MOVE      I4SRDT        NCSRDT
     C                   MOVE      I4SRTM        NCSRTM
     C                   MOVE      I4PRI#        NCPRI#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Update the incident if *REMOVE.
      *          ------------------------------------------
     C                   IF        I4RCM = '1'                                  B01
     C                   MOVE      *BLANKS       NCAIN#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do the update.
      *          ------------------------------------------
     C                   EXSR      SRIO
      *
      *          ------------------------------------------
      *          Write a log record.
      *          ------------------------------------------
     C                   IF        I4UPD = *ON                                  B01
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   EVAL      P#ACTC = '25'
     C                   IF        I4RCM = '0'                                  B02
     C                   EVAL      P#MSID = 'OMQA019'
     C                   ENDIF                                                  E02
     C                   IF        I4RCM = '1'                                  B02
     C                   EVAL      P#MSID = 'OMQA01A'
     C                   ENDIF                                                  E02
     C                   MOVEA     NCINC#        MC(1)
     C                   MOVEA     NCHEDC        MC(11)
     C                   MOVEA     NCAIN#        MC(16)
     C                   MOVEA     MC            P#MSGD
     C                   CALL      'OMX929'      $MX929                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X929 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X929 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Write in queue
      *          ------------------------------------------
     C                   EXSR      SRWDAQ
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCH27 - Disconnect an incident                             *
      *                                                             *
      * This routine is executed when the incident relationship     *
      * must be removed.                                            *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCH27        BEGSR
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
      *          Remove the incident relationship if update allowed.
      *          ------------------------------------------
     C                   IF        I4UPD = *ON                                  B01
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4INC#        K#INC#
     C                   EVAL      K#IRTP = 'I'
     C                   MOVE      I4THED        K#APPC
     C                   MOVEL(P)  I4TINC        K#IRIT
     C     $IR1K1        CHAIN     OMRIRL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF
     C                   DELETE    OMRIRR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Write a log record.
      *          ------------------------------------------
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   EVAL      P#ACTC = '27'
     C                   EVAL      P#MSID = 'OMQA017'
     C                   MOVEA     NCINC#        MC(1)
     C                   MOVEA     NCHEDC        MC(11)
     C                   MOVEA     I4TINC        MC(16)
     C                   MOVEA     I4THED        MC(26)
     C                   MOVEA     MC            P#MSGD
     C                   CALL      'OMX929'      $MX929                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X929 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X929 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Save the status
      *          ------------------------------------------
     C                   MOVEL     NCSTAT        S#STS2
     C                   MOVEL     NCSUL#        S#SUL#
     C                   MOVEL     NCSTRC        S#STRC
      *
      *          ------------------------------------------
      *          Write in queue
      *          ------------------------------------------
     C                   EXSR      SRWDAQ
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCH29 - Status and re-open                                 *
      *                                                             *
      * This routine is executed when the incident status must be   *
      * updated or when the incident must be reopened.              *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCH29        BEGSR
      *
      *          ------------------------------------------
      *          Do not check if accumulated processing.
      *          ------------------------------------------
     C                   IF        I#ACCU <> *ON                                B01
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
     C                   IF        ERR(1) <> *BLANKS                            B02
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If the status is *CMP then change the action
      *          '42' (*COMPLETE)
      *          ------------------------------------------
     C                   IF        I4STAT = C#CMP                               B01
     C                   EVAL      P4ACTC = '42'
     C                   EXSR      SRCH42
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Get the record & update the status fields.
      *          ------------------------------------------
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4INC#        K#INC#
     C                   EVAL      S#STAT = *BLANKS
     C     $NC1K1        CHAIN     OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Save statusfield
      *          ------------------------------------------
     C                   MOVEL     NCSTAT        S#STS2
     C                   MOVE      NCSTAT        S#STAT
     C                   MOVE      NCSUL#        S#SUL#
     C                   MOVE      NCSTRC        S#STRC
      *
     C                   MOVE      I4STRC        NCSTRC
     C                   MOVE      I4RES#        NCRES#
     C                   MOVE      I4SUL#        NCSUL#
     C                   MOVE      I4STAT        NCSTAT
     C                   MOVE      I4SRDT        NCSRDT
     C                   MOVE      I4SRTM        NCSRTM
     C                   EXSR      SRIO
      *
      *          ------------------------------------------
      *          Write a log record.
      *          ------------------------------------------
     C                   IF        I4UPD = *ON                                  B02
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   EVAL      P#ACTC = P4ACTC
     C                   EVAL      P#MSID = 'OMQA00A'
     C                   MOVEA     NCINC#        MC(1)
     C                   MOVEA     NCHEDC        MC(11)
     C                   MOVEA     S#STAT        MC(16)
     C                   MOVEA     I4STAT        MC(21)
     C                   EVAL      MC(26) = S#SUL#
     C                   MOVE      I4SUL#        MC(27)
     C                   MOVEA     S#STRC        MC(28)
     C                   MOVEA     I4STRC        MC(38)
     C                   MOVEA     MC            P#MSGD
     C                   CALL      'OMX929'      $MX929                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X929 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X929 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Write in queue
      *          ------------------------------------------
     C                   EXSR      SRWDAQ
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
      * SRCH30 - Copy                                               *
      *                                                             *
      * This routine copies one incident to another.                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCH30        BEGSR
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
      *          Get the record.
      *          ------------------------------------------
     C                   MOVE      NCHEDC        K#HEDC
     C                   MOVE      NCINC#        K#INC#
     C     $NC1K1        CHAIN     OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Save statusfield
      *          ------------------------------------------
     C                   MOVEL     NCSTAT        S#STS2
     C                   MOVE      NCSUL#        S#SUL#
     C                   MOVE      NCSTRC        S#STRC
      *
      *          ------------------------------------------
      *          Get a new incident number
      *          ------------------------------------------
     C                   IF        I4INC# = '*GEN'                              B01
     C                   CALL      'OMX967'      $MX967                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X967 = *OFF
     C                   MOVE      *BLANKS       I4INC#
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X967 = *ON
     C                   MOVE      P#INC#        I4INC#
     C                   ENDIF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Catch *TERM status.
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Fill the record.
      *          ------------------------------------------
     C                   MOVE      I4HEDC        NCHEDC
     C                   MOVE      I4INC#        NCINC#
     C                   MOVE      I4CTN#        NCCTN#
     C                   MOVE      I4AIN#        NCAIN#
     C                   MOVE      I4INDT        NCINDT
     C                   MOVE      I4INTM        NCINTM
     C                   MOVE      I4RCDT        NCRCDT
     C                   MOVE      I4RCTM        NCRCTM
     C                   MOVE      I4STRC        NCSTRC
     C                   MOVE      I4RES#        NCRES#
     C                   MOVE      I4SUL#        NCSUL#
     C                   MOVE      I4STAT        NCSTAT
     C                   MOVE      I4SRDT        NCSRDT
     C                   MOVE      I4SRTM        NCSRTM
     C                   MOVE      I4SEV1        NCSEV1
     C                   MOVE      I4SEV2        NCSEV2
     C                   MOVE      I4SEV3        NCSEV3
     C                   MOVE      I4PRI#        NCPRI#
     C                   MOVE      I4ESC#        NCESC#
     C                   MOVE      I4RUID        NCRUID
     C                   MOVE      I4AUID        NCAUID
     C                   MOVE      I4ITPC        NCITPC
     C                   MOVE      I4CFL#        NCCFL#
     C                   MOVE      I4CFIC        NCCFIC
     C                   MOVE      I4USF1        NCUSF1
     C                   MOVE      I4USF2        NCUSF2
     C                   MOVE      I4USF3        NCUSF3
     C                   MOVE      I4SHID        NCSHID
     C                   EXSR      SRIO
      *
      *          ------------------------------------------
      *          Write a log record.
      *          ------------------------------------------
     C                   IF        I4UPD = *ON                                  B01
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   EVAL      P#ACTC = '30'
     C                   EVAL      P#MSID = 'OMQA005'
     C                   MOVEA     NCINC#        MC(1)
     C                   MOVEA     NCHEDC        MC(11)
     C                   MOVEA     D#USID        MC(16)
     C                   MOVEA     NCAUID        MC(26)
     C                   MOVEA     MC            P#MSGD
     C                   CALL      'OMX929'      $MX929                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X929 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X929 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Write in queue
      *          ------------------------------------------
     C                   EVAL      S#STS2 = *BLANKS
     C                   EVAL      S#SUL# = *BLANKS
     C                   EVAL      S#STRC = *BLANKS
     C                   EXSR      SRWDAQ
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCH33 - Escalate                                           *
      *                                                             *
      * This routine escalates an incident                          *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCH33        BEGSR
      *
      *          ------------------------------------------
      *          Do not check if accumulated processing.
      *          ------------------------------------------
     C                   IF        I#ACCU <> *ON                                B01
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
     C                   IF        ERR(1) <> *BLANKS                            B02
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the record
      *          ------------------------------------------
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4INC#        K#INC#
     C     $NC1K1        CHAIN     OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Save the status
      *          ------------------------------------------
     C                   MOVEL     NCSTAT        S#STS2
     C                   MOVEL     NCSUL#        S#SUL#
     C                   MOVEL     NCSTRC        S#STRC
      *
      *          ------------------------------------------
      *          Write the new value.
      *          ------------------------------------------
     C                   Z-ADD     I4PRI#        NCPRI#
     C                   Z-ADD     I4ESC#        NCESC#
     C                   Z-ADD     I4IEDT        NCIEDT
     C                   Z-ADD     I4IETM        NCIETM
     C                   EXSR      SRIO
      *
      *          ------------------------------------------
      *          Write a log record.
      *          ------------------------------------------
     C                   IF        I4UPD = *ON                                  B01
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   EVAL      P#ACTC = '33'
     C                   EVAL      P#MSID = 'OMQA00D'
     C                   MOVEA     NCINC#        MC(1)
     C                   MOVEA     NCHEDC        MC(11)
     C                   MOVEA     D#USID        MC(16)
     C                   MOVEA     MC            P#MSGD
     C                   CALL      'OMX929'      $MX929                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X929 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X929 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Write in queue
      *          ------------------------------------------
     C                   EXSR      SRWDAQ
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCH40 - Change code 40 Remove Incident                     *
      *                                                             *
      * This routine is executed when the Incident has to be removed*
      *                                                             *
      *  ** MAY NOT WORK, NOT YET IMPLEMENTED ***                   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCH40        BEGSR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCH42 - Complete incident.                                 *
      *                                                             *
      * This routine is executed when the incident is completed.    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCH42        BEGSR
      *
      *          ------------------------------------------
      *          Do not check if accumulated processing.
      *          ------------------------------------------
     C                   IF        I#ACCU <> *ON                                B01
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
     C                   IF        ERR(1) <> *BLANKS                            B02
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the record and set status to *CMP
      *          ------------------------------------------
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4INC#        K#INC#
     C     $NC1K1        CHAIN     OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Save statusfield
      *          ------------------------------------------
     C                   MOVEL     NCSTAT        S#STS2
     C                   MOVE      NCSUL#        S#SUL#
     C                   MOVE      NCSTRC        S#STRC
      *
      *          ------------------------------------------
      *          Fill the record and update
      *          ------------------------------------------
     C                   MOVE      I4RCDT        NCRCDT
     C                   MOVE      I4RCTM        NCRCTM
     C                   MOVE      C#CMP         NCSTAT
     C                   EXSR      SRIO
      *
      *          ------------------------------------------
      *          Write a log record.
      *          ------------------------------------------
     C                   IF        I4UPD = *ON                                  B01
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   EVAL      P#ACTC = '42'
     C                   EVAL      P#MSID = 'OMQA008'
     C                   MOVEA     NCINC#        MC(1)
     C                   MOVEA     NCHEDC        MC(11)
     C                   MOVEA     D#USID        MC(16)
     C                   MOVEA     MC            P#MSGD
     C                   CALL      'OMX929'      $MX929                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X929 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X929 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Write in queue
      *          ------------------------------------------
     C                   EXSR      SRWDAQ
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCH60 - Change code 60 Add Incident                        *
      *                                                             *
      * This routine is executed when the program has to add        *
      * an Incident                                                 *
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
      *          Get a new incident number
      *          ------------------------------------------
     C                   IF        I4INC# = '*GEN'                              B01
     C                   CALL      'OMX967'      $MX967                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X967 = *OFF
     C                   MOVE      *BLANKS       I4INC#
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X967 = *ON
     C                   MOVE      P#INC#        I4INC#
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
     C                   MOVE      I4HEDC        NCHEDC
     C                   MOVE      I4INC#        NCINC#
     C                   MOVE      I4CTN#        NCCTN#
     C                   MOVE      I4AIN#        NCAIN#
     C                   MOVE      I4INDT        NCINDT
     C                   MOVE      I4INTM        NCINTM
     C                   MOVE      I4RCDT        NCRCDT
     C                   MOVE      I4RCTM        NCRCTM
     C                   MOVE      I4STRC        NCSTRC
     C                   MOVE      I4RES#        NCRES#
     C                   MOVE      I4SUL#        NCSUL#
     C                   MOVE      I4STAT        NCSTAT
     C                   MOVE      I4SRDT        NCSRDT
     C                   MOVE      I4SRTM        NCSRTM
     C                   MOVE      I4SEV1        NCSEV1
     C                   MOVE      I4SEV2        NCSEV2
     C                   MOVE      I4SEV3        NCSEV3
     C                   MOVE      I4PRI#        NCPRI#
     C                   MOVE      I4ESC#        NCESC#
     C                   MOVE      I4RUID        NCRUID
     C                   MOVE      I4AUID        NCAUID
     C                   MOVE      I4ITPC        NCITPC
     C                   MOVE      I4CFL#        NCCFL#
     C                   MOVE      I4CFIC        NCCFIC
     C                   MOVE      I4USF1        NCUSF1
     C                   MOVE      I4USF2        NCUSF2
     C                   MOVE      I4USF3        NCUSF3
     C                   MOVE      I4SHID        NCSHID
     C                   EXSR      SRIO
      *
      *          ------------------------------------------
      *          Write a log record.
      *          ------------------------------------------
     C                   IF        I4UPD = *ON                                  B01
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   EVAL      P#ACTC = '60'
     C                   EVAL      P#MSID = 'OMQA005'
     C                   MOVEA     NCINC#        MC(1)
     C                   MOVEA     NCHEDC        MC(11)
     C                   MOVEA     D#USID        MC(16)
     C                   MOVEA     MC            P#MSGD
     C                   CALL      'OMX929'      $MX929                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X929 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X929 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Write in queue
      *          ------------------------------------------
     C                   EVAL      S#STS2 = *BLANKS
     C                   EVAL      S#SUL# = *BLANKS
     C                   EVAL      S#STRC = *BLANKS
     C                   EXSR      SRWDAQ
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C     CH6099        TAG
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCH85 - Deallocate incident.                               *
      *                                                             *
      * This routine is executed when the incident must be          *
      * deallocated from an HelpDesk employee.                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCH85        BEGSR
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
      *          Get the record & update the allocated_by user.
      *          ------------------------------------------
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4INC#        K#INC#
     C     $NC1K1        CHAIN     OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      NCAUID        S#AUID
     C                   MOVE      I4AUID        NCAUID
      *
      *          ------------------------------------------
      *          Save the status
      *          ------------------------------------------
     C                   MOVEL     NCSTAT        S#STS2
     C                   MOVEL     NCSUL#        S#SUL#
     C                   MOVEL     NCSTRC        S#STRC
     C                   EXSR      SRIO
      *
      *          ------------------------------------------
      *          Write a log record.
      *          ------------------------------------------
     C                   IF        I4UPD = *ON                                  B01
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   EVAL      P#ACTC = '85'
     C                   EVAL      P#MSID = 'OMQA009'
     C                   MOVEA     NCINC#        MC(1)
     C                   MOVEA     NCHEDC        MC(11)
     C                   MOVEA     D#USID        MC(16)
     C                   MOVEA     S#AUID        MC(26)
     C                   MOVEA     MC            P#MSGD
     C                   CALL      'OMX929'      $MX929                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X929 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X929 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Write in queue
      *          ------------------------------------------
     C                   EXSR      SRWDAQ
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCH87 - Connect to a request                               *
      *                                                             *
      * This routine is executed when a request must be connected   *
      * to the incident. A record is written in the incident        *
      * relationships file.                                         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCH87        BEGSR
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
      *          If update allowed.
      *          ------------------------------------------
     C                   IF        I4UPD = *ON                                  B01
      *
      *          ------------------------------------------
      *          Write a request relationship if I4RCM = *CONNECT
      *          ------------------------------------------
     C                   IF        I4RCM = '0'                                  B02
     C                   MOVE      I4HEDC        IRHEDC
     C                   MOVE      I4INC#        IRINC#
     C                   MOVE      'R'           IRIRTP
     C                   MOVE      I4TAPP        IRIRAH
     C                   MOVEL(P)  I4TREQ        IRIRIT
     C                   MOVE      IRESTS        IRESTS
     C                   WRITE     OMRIRR
      *
      *          ------------------------------------------
      *          Remove a request relationship if I4RCM = *DISCONNECT
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4INC#        K#INC#
     C                   EVAL      K#IRTP = 'R'
     C                   MOVE      I4TAPP        K#APPC
     C                   MOVEL(P)  I4TREQ        K#IRIT
     C     $IR1K1        CHAIN     OMRIRL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF
     C                   DELETE    OMRIRR
     C                   ENDIF
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Write a log record.
      *          ------------------------------------------
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   EVAL      P#ACTC = '87'
     C                   IF        I4RCM = *OFF                                 B02
     C                   EVAL      P#MSID = 'OMQA011'
     C                   ELSE                                                   X02
     C                   EVAL      P#MSID = 'OMQA016'
     C                   ENDIF                                                  E02
     C                   MOVEA     NCINC#        MC(1)
     C                   MOVEA     NCHEDC        MC(11)
     C                   MOVEA     I4TREQ        MC(16)
     C                   MOVEA     I4TAPP        MC(26)
     C                   MOVEA     MC            P#MSGD
     C                   CALL      'OMX929'      $MX929                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X929 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X929 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Save the status
      *          ------------------------------------------
     C                   MOVEL     NCSTAT        S#STS2
     C                   MOVEL     NCSUL#        S#SUL#
     C                   MOVEL     NCSTRC        S#STRC
      *
      *          ------------------------------------------
      *          Write in queue
      *          ------------------------------------------
     C                   EXSR      SRWDAQ
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
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
     C     I4HEDC        SETLL     OMHDDL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B01
     C                   MOVEL     'OMQ5028'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4HEDC        MC(1)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#HEDC
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The target helpdesk code must exist (if copy)
      *          ------------------------------------------
     C                   IF        P4ACTC = '30'                                B01
     C                   EXSR      SRDDL1
     C     I4THED        SETLL     OMHDDL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEL     'OMQ5028'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4THED        MC(1)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#THED
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If add then reset the output record.
      *          ------------------------------------------
     C                   IF        P4ACTC = '60'                                B01
     C                   RESET                   OMINCR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the record if this is other than add.
      *          ------------------------------------------
     C                   IF        P4ACTC <> '60'                               B01
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4INC#        K#INC#
     C                   EXSR      SRNCL1
     C     $NC1K1        CHAIN(N)  OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Error if record not found.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEL     'OMQA002'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4HEDC        MC(1)
     C                   MOVEA     I4INC#        MC(6)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#INC#
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Only allowed to incident if same or higher
      *          support level (display is always allowed).
      *          ------------------------------------------
     C                   IF        P#SUL# < NCSUL#                              B03
     C                             AND P4ACTC <> '50'
     C                   MOVEL     'OMQA00C'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4INC#        MC(1)
     C                   MOVEA     I4HEDC        MC(11)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#INC#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Save statusfield
      *          ------------------------------------------
     C                   MOVEL     NCSTAT        S#STS2
      *
      *                    --------------------------------
      *                    Record completed.
      *                    --------------------------------
     C                   IF        NCSTAT = '*CMP'                              B03
     C                   IF        P4ACTC = '15'                                B04
     C                             OR P4ACTC = '20'                             Change
     C                             OR P4ACTC = '25'                             Accumulate
     C                             OR P4ACTC = '29'                             Status
     C                             OR P4ACTC = '33'                             Escalate
     C                             OR P4ACTC = '40'                             Delete
     C                             OR P4ACTC = '42'                             Complete
     C                             OR P4ACTC = '85'                             Deallocate
     C                   MOVEL     'OMQA00F'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4HEDC        MC(1)
     C                   MOVEA     I4INC#        MC(6)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      I#QUIT = *ON
     C                   EXSR      SRERR
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the call track number if not add nor copy.
      *          ------------------------------------------
     C                   IF        P4ACTC <> '60'                               B01
     C                             AND P4ACTC <> '30'
     C                             AND I4CTN# = *BLANKS
     C                             OR I4CTN# = '*SAME'
     C                   MOVE      NCCTN#        I4CTN#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the accumulated incident if not add nor copy.
      *          ------------------------------------------
     C                   IF        P4ACTC <> '60'                               B01
     C                             AND P4ACTC <> '30'
     C                             AND I4AIN# = *BLANKS
     C                             OR I4AIN# = '*SAME'
     C                   MOVE      NCAIN#        I4AIN#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Incident, the special value *GEN is only
      *          valid when adding.
      *          ------------------------------------------
     C                   IF        I4INC# = C#GEN                               B01
     C                             AND P4ACTC <> '60'
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     C#GEN         MC(1)
     C                   MOVEA     C#INC#        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#INC#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          To incident, the special value *GEN is only
      *          valid when copying.
      *          ------------------------------------------
     C                   IF        I4TINC = C#GEN                               B01
     C                             AND P4ACTC <> '30'
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     C#GEN         MC(1)
     C                   MOVEA     C#INC#        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#INC#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Short description, blank is *SAME.
      *          ------------------------------------------
     C                   IF        I4SHID = *BLANKS                             B01
     C                   MOVE      NCSHID        I4SHID
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Income date. Zero is *CURRENT or *SAME.
      *          ------------------------------------------
     C                   IF        P4ACTC = '60'                                B01
     C                             OR P4ACTC = '30'                             B02
     C                   IF        I4INDT = *ZERO                               B02
     C                   Z-ADD     W#SYDT        I4INDT
     C                   ELSE                                                   X02
     C                   Z-ADD     I4INDT        I4INDT
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
     C                   Z-ADD     NCINDT        I4INDT
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Income time. Zero is *CURRENT or *SAME.
      *          ------------------------------------------
     C                   IF        P4ACTC = '60'                                B01
     C                             OR P4ACTC = '30'                             B02
     C                   IF        I4INTM = *ZERO                               B02
     C                   Z-ADD     W#SYTM        I4INTM
     C                   ELSE                                                   X02
     C                   Z-ADD     I4INTM        I4INTM
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
     C                   Z-ADD     NCINTM        I4INTM
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Status route, *FIRST is first
      *          *SAME is *FIRST when adding.
      *          ------------------------------------------
     C                   IF        I4STRC = C#SAME                              B01
     C                             AND P4ACTC = '60'
     C                   MOVEL     C#FRST        I4STRC
     C                   ENDIF                                                  E01
      *
     C                   IF        I4STRC = C#FRST                              B01
     C                   EXSR      SRREL1
     C     I4HEDC        SETLL     OMSREL1
     C     I4HEDC        READE     OMSREL1
     C                   EVAL      *IN99 = %EOF
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     C#FRST        MC(1)
     C                   MOVEA     C#STRC        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#STRC
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
     C                   MOVE      RESTRC        I4STRC
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
      *
      *                    --------------------------------
      *                    *SAME
      *                    --------------------------------
     C                   IF        I4STRC = C#SAME                              B02
     C                   MOVE      NCSTRC        I4STRC
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Status route sequence.
      *          ------------------------------------------
      *                    *SAME is *FIRST when add.
      *                    --------------------------------
     C                   IF        I4RESA = '*S'                                B01
     C                             AND P4ACTC = '60'
     C                   EVAL      I4RESA = '*F'
     C                   ENDIF                                                  E01
      *
     C                   IF        I4RESA = '*S'                                B01
     C                   Z-ADD     NCRES#        I4RES#
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    *FIRST then a route entry must exist.
      *                    --------------------------------
     C                   IF        I4RESA = '*F'                                B01
     C                             OR I4RESA = X'000F'
     C                   IF        P4ACTC = '60'                                B02
     C                             OR P4ACTC = '29'
     C                             OR P4ACTC = '20'
     C                             OR P4ACTC = '09'
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4STRC        K#STRC
     C                   EXSR      SRREL1
     C     $RE1K1        CHAIN     OMSREL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B03
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     C#FRST        MC(1)
     C                   MOVEA     C#RES#        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#RES#
     C                   EXSR      SRERR
     C                   ELSE                                                   X03
     C                   MOVE      RERES#        I4RES#
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
      *
      *                    --------------------------------
      *                    *NEXT (valid for action *STATUS)
      *                    --------------------------------
     C                   IF        I4RESA = '*N'                                B02
      *
     C                   IF        P4ACTC = '29'                                B03
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4STRC        K#STRC
     C                   MOVE      NCRES#        K#RES#
     C                   EXSR      SRREL1
     C     $RE1K2        SETGT     OMSREL1
     C     $RE1K1        READE     OMSREL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF                                                  E03
      *
     C                   IF        *IN99 = *ON                                  B03
     C                             OR P4ACTC <> '29'
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   EVAL      MC(1) = '*'
     C                   EVAL      MC(2) = 'N'
     C                   EVAL      MC(3) = 'E'
     C                   EVAL      MC(4) = 'X'
     C                   EVAL      MC(5) = 'T'
     C                   MOVEA     C#RES#        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#RES#
     C                   EXSR      SRERR
     C                   ELSE                                                   X03
     C                   MOVE      RERES#        I4RES#
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Support level. Start with '0', updated by the
      *          status route.
      *          ------------------------------------------
     C                   IF        P4ACTC = '60'                                B01
     C                   MOVE      '0'           I4SUL#
     C                   ELSE                                                   X01
     C                   MOVE      NCSUL#        I4SUL#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Severity 1
      *          ------------------------------------------
      *                    *SAME allowed when not add.
      *                    --------------------------------
     C                   IF        I4SEV1 = '*SAME'                             B01
     C                   IF        P4ACTC = '60'                                B02
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     C#SAME        MC(1)
     C                   MOVEA     C#SEV1        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#SEV1
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
     C                   MOVE      NCSEV1        I4SEV1
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
      *
      *                    --------------------------------
      *                    *DFT
      *                    --------------------------------
     C                   IF        I4SEV1 = '*DFT'                              B02
     C                   MOVE      I4HEDC        K#HEDC
     C                   EVAL      K#TABC = '01'
     C                   EXSR      SRDSL3
     C     $DS3K1        CHAIN     OMHDSL3
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B03
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   EVAL      MC(1) = '*'
     C                   EVAL      MC(2) = 'D'
     C                   EVAL      MC(3) = 'F'
     C                   EVAL      MC(4) = 'T'
     C                   MOVEA     C#SEV1        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#SEV1
     C                   EXSR      SRERR
     C                   ELSE                                                   X03
     C                   MOVE      DSSEVC        I4SEV1
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Severity 2
      *          ------------------------------------------
      *                    *SAME allowed when not add.
      *                    --------------------------------
     C                   IF        I4SEV2 = '*SAME'                             B01
     C                   IF        P4ACTC = '60'                                B02
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     C#SAME        MC(1)
     C                   MOVEA     C#SEV2        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#SEV2
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
     C                   MOVE      NCSEV2        I4SEV2
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
      *
      *                    --------------------------------
      *                    *DFT
      *                    --------------------------------
     C                   IF        I4SEV2 = '*DFT'                              B02
     C                   MOVE      I4HEDC        K#HEDC
     C                   EVAL      K#TABC = '02'
     C                   EXSR      SRDSL3
     C     $DS3K1        CHAIN     OMHDSL3
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B03
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   EVAL      MC(1) = '*'
     C                   EVAL      MC(2) = 'D'
     C                   EVAL      MC(3) = 'F'
     C                   EVAL      MC(4) = 'T'
     C                   MOVEA     C#SEV2        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#SEV2
     C                   EXSR      SRERR
     C                   ELSE                                                   X03
     C                   MOVE      DSSEVC        I4SEV2
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Severity 3
      *          ------------------------------------------
      *                    *SAME allowed when not add.
      *                    --------------------------------
     C                   IF        I4SEV3 = '*SAME'                             B01
     C                   IF        P4ACTC = '60'                                B02
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     C#SAME        MC(1)
     C                   MOVEA     C#SEV3        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#SEV3
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
     C                   MOVE      NCSEV3        I4SEV3
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
      *
      *                    --------------------------------
      *                    *DFT
      *                    --------------------------------
     C                   IF        I4SEV3 = '*DFT'                              B02
     C                   MOVE      I4HEDC        K#HEDC
     C                   EVAL      K#TABC = '03'
     C                   EXSR      SRDSL3
     C     $DS3K1        CHAIN     OMHDSL3
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B03
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   EVAL      MC(1) = '*'
     C                   EVAL      MC(2) = 'D'
     C                   EVAL      MC(3) = 'F'
     C                   EVAL      MC(4) = 'T'
     C                   MOVEA     C#SEV3        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#SEV3
     C                   EXSR      SRERR
     C                   ELSE                                                   X03
     C                   MOVE      DSSEVC        I4SEV3
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Incident type
      *          ------------------------------------------
      *                    *SAME allowed when not add.
      *                    --------------------------------
     C                   IF        I4ITPC = '*SAME'                             B01
     C                   IF        P4ACTC = '60'                                B02
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     C#SAME        MC(1)
     C                   MOVEA     C#ITPC        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#ITPC
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
     C                   MOVE      NCITPC        I4ITPC
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Allocated by user.
      *          ------------------------------------------
     C                   IF        I4AUID = '*CURRENT'                          B01
     C                   IF        P4ACTC = '15'                                B02
     C                   MOVE      D#USID        I4AUID
     C                   ELSE                                                   X02
     C                   MOVE      NCAUID        I4AUID
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          User defined field 1
      *          ------------------------------------------
     C                   IF        I4USF1 = '*SAME'                             B01
     C                   IF        P4ACTC = '60'                                B02
     C                   MOVE      *BLANKS       I4USF1
     C                   ELSE                                                   X02
     C                   MOVE      NCUSF1        I4USF1
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          User defined field 2
      *          ------------------------------------------
     C                   IF        I4USF2 = '*SAME'                             B01
     C                   IF        P4ACTC = '60'                                B02
     C                   MOVE      *BLANKS       I4USF2
     C                   ELSE                                                   X02
     C                   MOVE      NCUSF2        I4USF2
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          User defined field 3
      *          ------------------------------------------
     C                   IF        I4USFA = '*SAME'                             B01
     C                   IF        P4ACTC = '60'                                B02
     C                   EVAL      I4USF3 = *ZEROS
     C                   ELSE                                                   X02
     C                   MOVE      NCUSF3        I4USF3
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
      *          May not accumulate if accumulated request.
      *          ------------------------------------------
     C                   IF        NCAIN# <> *BLANKS                            B01
     C                             AND P4ACTC = '25'
     C                             AND I4RCM = '0'
      *
      *          ------------------------------------------
      *          Source incident may not be an accumulated incident.
      *          ------------------------------------------
     C                   MOVE      I4HEDC        K#HEDC                         B01
     C                   MOVE      I4INC#        K#INC#
     C                   EXSR      SRNCL4
     C     $NC1K1        SETLL     OMINCL4
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEL     'OMQA01D'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4INC#        MC(1)
     C                   MOVEA     I4HEDC        MC(11)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#INC#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Incident number
      *          ------------------------------------------
      *                    May not be blanks.
      *                    --------------------------------
     C                   IF        I4INC# = *BLANKS                             B01
     C                   MOVEL     'OMQ600A'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     VAR(2)        MC(11)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#INC#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    May not exist if add.
      *                    --------------------------------
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4INC#        K#INC#
     C                   EXSR      SRNCL1
     C     $NC1K1        SETLL     OMINCL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B01
     C                             AND P4ACTC = '60'
     C                   MOVEL     'OMQA004'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4INC#        MC(1)
     C                   MOVEA     I4HEDC        MC(11)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#INC#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    May not be *GEN when not add.
      *                    --------------------------------
     C                   IF        I4INC# = '*GEN'                              B01
     C                             AND P4ACTC <> '60'
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   EVAL      MC(1) = '*'
     C                   EVAL      MC(2) = 'G'
     C                   EVAL      MC(3) = 'E'
     C                   EVAL      MC(4) = 'N'
     C                   MOVEA     C#INC#        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#INC#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    Must exist otherwise.
      *                    --------------------------------
     C                   IF        *IN99 <> *ON                                 B01
     C                             AND P4ACTC <> '60'
     C                   MOVEL     'OMQA002'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4HEDC        MC(1)
     C                   MOVEA     I4INC#        MC(6)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#INC#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Target incident
      *          ------------------------------------------
      *                    May not be blanks if copy.
      *                    --------------------------------
     C                   IF        I4TINC = *BLANKS                             B01
     C                   IF        P4ACTC = '30'                                B02
     C                   MOVEL     'OMQ600A'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     VAR(2)        MC(11)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#TINC
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    May not exist if copy.
      *                    --------------------------------
     C                   MOVE      I4THED        K#HEDC
     C                   MOVE      I4TINC        K#INC#
     C                   EXSR      SRNCL1
     C     $NC1K1        SETLL     OMINCL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B01
     C                   IF        P4ACTC = '30'                                B02
     C                   MOVEL     'OMQA004'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4TINC        MC(1)
     C                   MOVEA     I4THED        MC(11)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#INC#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Incident may not be connected to itself
      *                    --------------------------------
     C                   IF        I4THED = NCHEDC                              B02
     C                             AND I4TINC = NCINC#
     C                             AND P4ACTC = '23'
     C                   MOVEL     'OMQA020'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4TINC        MC(1)
     C                   MOVEA     I4THED        MC(11)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#INC#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Must exist if *CONNECT or *ACCUMULATE
      *                    --------------------------------
     C                   ELSE                                                   X01
     C                   IF        P4ACTC = '23'                                B02
     C                   MOVEL     'OMQA002'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4THED        MC(1)
     C                   MOVEA     I4TINC        MC(6)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#INC#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Accumulated incident.
      *          ------------------------------------------
      *                    May not be blanks if accumulate
      *                    --------------------------------
     C                   IF        P4ACTC = '25'                                B01
     C                             AND I4RCM = '0'
      *
     C                   IF        I4AIN# = *BLANKS                             B02
     C                   MOVEL     'OMQ600A'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     VAR(8)        MC(11)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#AIN#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Must exist.
      *                    --------------------------------
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4AIN#        K#INC#
     C                   EXSR      SRNCL1
     C     $NC1K1        CHAIN     OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEL     'OMQA002'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4HEDC        MC(1)
     C                   MOVEA     I4AIN#        MC(6)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#AIN#
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
      *
      *                    --------------------------------
      *                    Accumulated incident may not be
      *                    accumulated to another incident.
      *                    --------------------------------
     C                   IF        NCAIN# <> *BLANKS                            B03
     C                   MOVEL     'OMQA01C'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4AIN#        MC(1)
     C                   MOVEA     I4HEDC        MC(11)
     C                   MOVEA     NCAIN#        MC(16)
     C                   MOVEA     I4HEDC        MC(26)
     C                   MOVEA     I4INC#        MC(36)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#AIN#
     C                   EXSR      SRERR
      *
      *                    --------------------------------
      *                    Incident may not be connected to itself
      *                    --------------------------------
     C                   ELSE                                                   X03
      *
      *                    --------------------------------
      *                    Get the incident record again.
      *                    --------------------------------
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4INC#        K#INC#
     C                   EXSR      SRNCL1
     C     $NC1K1        CHAIN(N)  OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *                    --------------------------------
      *                    Incident may not be connected to itself
      *                    --------------------------------
     C                   IF        NCINC# = I4AIN#                              B04
     C                   MOVEL     'OMQA020'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4AIN#        MC(1)
     C                   MOVEA     I4HEDC        MC(11)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#INC#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    Must exist when unaccumulating
      *                    --------------------------------
     C                   IF        P4ACTC = '25'                                B01
     C                             AND I4RCM = '1'
     C                             AND NCAIN# <> I4TINC
     C                   MOVEL     'OMQA014'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4TINC        MC(1)
     C                   MOVEA     I4HEDC        MC(11)
     C                   MOVEA     I4INC#        MC(16)
     C                   MOVEA     I4HEDC        MC(26)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#TINC
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Target application
      *          ------------------------------------------
      *                    May not be blanks if *OMS
      *                    --------------------------------
     C                   IF        I4TAPP = *BLANKS                             B01
     C                             AND P4ACTC = '87'
     C                   MOVEL     'OMQ600A'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     VAR(7)        MC(11)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#TREQ
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Target request
      *          ------------------------------------------
      *                    May not be blanks if *OMS
      *                    --------------------------------
     C                   IF        I4TREQ = *BLANKS                             B01
     C                             AND P4ACTC = '87'
     C                   MOVEL     'OMQ600A'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     VAR(6)        MC(11)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#TREQ
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    Must exist if *OMS
      *                    *IN99 from SRRRL1 is on if no OMS
      *                    was found.
      *                    --------------------------------
     C                   MOVE      I4TAPP        K#APPC
     C                   MOVE      I4TREQ        K#ERR#
     C                   IF        P4ACTC = '87'                                B01
     C                             AND I4RCM = '0'                              Connect
      *
      *
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEL     'OMQ3006'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4TAPP        MC(1)
     C                   MOVEA     I4TREQ        MC(6)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#INC#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Connection with incident
      *          ------------------------------------------
      *                    If action is *CONNECT/*DISCONNECT
      *                    --------------------------------
     C                   IF        P4ACTC = '23'                                B01
     C                             OR P4ACTC = '27'                             B01
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVEL     I4INC#        K#INC#
     C                   MOVE      I4THED        K#APPC
     C                   MOVEL(P)  I4TINC        K#IRIT
     C                   EVAL      K#IRTP = 'I'
     C                   EXSR      SRIRL1
     C     $IR1K1        SETLL     OMRIRL1
     C                   EVAL      *IN99 = %EQUAL
      *
      *                    --------------------------------
      *                    May not exist if adding
      *                    --------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                             AND P4ACTC = '23'
     C                   MOVEL     'OMQA010'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4TINC        MC(1)
     C                   MOVEA     I4THED        MC(11)
     C                   MOVEA     I4INC#        MC(16)
     C                   MOVEA     I4HEDC        MC(26)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#TREQ
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Must exist when removing
      *                    --------------------------------
     C                   IF        *IN99 = *OFF                                 B02
     C                             AND P4ACTC = '27'
     C                   MOVEL     'OMQA014'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4TINC        MC(1)
     C                   MOVEA     I4THED        MC(11)
     C                   MOVEA     I4INC#        MC(16)
     C                   MOVEA     I4HEDC        MC(26)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#TREQ
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Connection with request
      *          ------------------------------------------
      *                    If action is *OMS
      *                    --------------------------------
     C                   IF        P4ACTC = '87'                                B01
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVEL     I4INC#        K#INC#
     C                   MOVE      I4TAPP        K#APPC
     C                   MOVEL(P)  I4TREQ        K#IRIT
     C                   EVAL      K#IRTP = 'R'
     C                   EXSR      SRIRL1
     C     $IR1K1        SETLL     OMRIRL1
     C                   EVAL      *IN99 = %EQUAL
      *
      *                    --------------------------------
      *                    May not exist if *ADD
      *                    --------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                             AND I4RCM = *OFF
     C                   MOVEL     'OMQA010'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4TREQ        MC(1)
     C                   MOVEA     I4TAPP        MC(11)
     C                   MOVEA     I4INC#        MC(16)
     C                   MOVEA     I4HEDC        MC(26)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#TREQ
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Must exist if *REMOVE
      *                    --------------------------------
     C                   IF        *IN99 = *OFF                                 B02
     C                             AND I4RCM <> *OFF
     C                   MOVEL     'OMQA015'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4TREQ        MC(1)
     C                   MOVEA     I4TAPP        MC(11)
     C                   MOVEA     I4INC#        MC(16)
     C                   MOVEA     I4HEDC        MC(26)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#TREQ
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If copy.
      *          ------------------------------------------
     C                   IF        P4ACTC = '30'                                B01
     C                   MOVE      NCCTN#        S#CTN#
     C                   MOVEL     DSINCR        I4BUF
     C                   MOVE      I4THED        I4HEDC
     C                   MOVE      I4TINC        I4INC#
     C                   MOVE      S#CTN#        I4CTN#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Call track number.
      *          ------------------------------------------
      *                    Must exist when add.
      *                    --------------------------------
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4CTN#        K#CTN#
     C                   EXSR      SRCAL1
     C     $CA1K1        SETLL     OMCCAL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B01
     C                             AND P4ACTC = '60'
     C                             OR *IN99 = *OFF                              B01
     C                             AND P4ACTC = '30'
     C                   MOVEL     'OMQ6018'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     VAR(3)        MC(1)
     C                   MOVEA     I4CTN#        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#CTN#
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Realized completed date. Fill with current if complete
      *          ------------------------------------------
     C                   IF        P4ACTC = '42'                                B01
     C                   MOVE      W#SYDT        I4RCDT
     C                   MOVE      W#SYTM        I4RCTM
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Status route and route code must exist.
      *          ------------------------------------------
     C                   IF        P4ACTC = '60'                                B01
     C                             OR P4ACTC = '29'
     C                             OR P4ACTC = '09'
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4STRC        K#STRC
     C                   MOVE      I4RES#        K#RES#
     C                   EXSR      SRREL1
     C     $RE1K2        CHAIN     OMSREL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   Z-ADD     I4RES#        D#BI4#
     C                   MOVEA     I4STRC        MC(1)
     C                   MOVEA     D#BI4A        MC(11)
     C                   MOVEL     'OMQ6019'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#STRC
     C                   EVAL      W#FLD2 = C#RES#
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
      *
      *                    --------------------------------
      *                    If the status route is filled, the
      *                    first entry of the this route is used.
      *                    This route can not point to another route.
      *                    --------------------------------
     C                   IF        RENSRC <> *BLANKS                            B03
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      RENSRC        K#STRC
     C                   MOVE      RENSRC        I4STRC
     C                   EXSR      SRREL1
     C     $RE1K2        CHAIN     OMSREL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B04
     C                   EVAL      D#BI4# = *ZEROS
     C                   MOVEA     I4STRC        MC(1)
     C                   MOVEA     D#BI4A        MC(11)
     C                   MOVEL     'OMQ6019'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#STRC
     C                   EVAL      W#FLD2 = C#RES#
     C                   EXSR      SRERR
      *
      *                    --------------------------------
      *                    The support level is found.
      *                    --------------------------------
     C                   ELSE                                                   X04
     C                   MOVE      RERES#        I4RES#
     C                   MOVE      RESUL#        I4SUL#
     C                   MOVE      RESTAT        I4STAT
     C                   ENDIF                                                  E04
      *
      *                    --------------------------------
      *                    The support level is found.
      *                    --------------------------------
     C                   ELSE                                                   X03
     C                   MOVE      RERES#        I4RES#
     C                   MOVE      RESUL#        I4SUL#
     C                   MOVE      RESTAT        I4STAT
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          If the status was not filled and this is not add
      *          nor *status then fill from old value.
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   IF        I4STAT = *BLANKS                             B02
     C                   MOVE      NCRES#        I4RES#
     C                   MOVE      NCSUL#        I4SUL#
     C                   MOVE      NCSTAT        I4STAT
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the incident type
      *          ------------------------------------------
     C                   IF        P4ACTC = '60'                                B01
     C                             OR P4ACTC = '30'
     C                             OR P4ACTC = '20'
      *
     C                   MOVE      I4HEDC        K#HEDC
     C                   MOVE      I4ITPC        K#ITPC
     C                   EXSR      SRITL1
     C     $IT1K1        CHAIN     OMHITL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEL     'OMQ6018'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     VAR(5)        MC(1)
     C                   MOVEA     I4ITPC        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#ITPC
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Change the status date and time if the status changed.
      *          ------------------------------------------
     C                   IF        P4ACTC = '60'                                B01
     C                             OR P4ACTC = '30'
     C                             OR P4ACTC = '29'
     C                             OR P4ACTC = '09'
      *
     C                   IF        I4STAT <> NCSTAT                             B02
     C                             OR I4STRC <> NCSTRC
     C                             OR I4RES# <> NCRES#
     C                   MOVE      W#SYDT        I4SRDT
     C                   MOVE      W#SYTM        I4SRTM
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If complete. Fill with current if completed via
      *          the next step in the status route (NCRCDT not filled)
      *          ------------------------------------------
     C                   IF        I4STAT = C#CMP                               B01
     C                             AND NCRCDT = *ZEROS
     C                   MOVE      W#SYDT        I4RCDT
     C                   MOVE      W#SYTM        I4RCTM
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the first severity. The severities influence
      *          the priority.
      *          If a severity changes, then the priority weigth
      *          of the old severity is subtracted from the priority.
      *          ------------------------------------------
     C                   IF        P4ACTC = '60'                                B01
     C                             OR P4ACTC = '30'
     C                             OR P4ACTC = '20'
      *
      *                    --------------------------------
      *                    The old priority also counts.
      *                    --------------------------------
     C                   IF        P4ACTC = '20'                                B02
     C                   Z-ADD     NCPRI#        I4PRI#
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Get the old severity if change.
      *                    --------------------------------
     C                   EVAL      W#SEW# = *ZEROS
     C                   IF        P4ACTC = '20'                                B02
     C                   MOVE      I4HEDC        K#HEDC
     C                   EVAL      K#TABC = '01'
     C                   MOVE      NCSEV1        K#SEVC
     C                   EXSR      SRDSL1
     C     $DS1K1        CHAIN     OMHDSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B03
     C                   Z-ADD     DSSEW#        W#SEW#
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Get the new severity.
      *                    --------------------------------
     C                   MOVE      I4HEDC        K#HEDC
     C                   EVAL      K#TABC = '01'
     C                   MOVE      I4SEV1        K#SEVC
     C                   EXSR      SRDSL1
     C     $DS1K1        CHAIN     OMHDSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEL     'OMQ6018'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     VAR(4)        MC(1)
     C                   MOVEA     I4SEV1        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#SEV1
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
      *
      *                    --------------------------------
      *                    Calculate the priority.
      *                    --------------------------------
     C     I4PRI#        ADD       DSSEW#        W#50
     C     W#50          SUB       W#SEW#        W#50
     C                   IF        W#50 > 999                                   B03
     C                   Z-ADD     999           I4PRI#
     C                   ELSE                                                   X03
     C                   IF        W#50 < *ZEROS                                B04
     C                   EVAL      I4PRI# = *ZEROS
     C                   ELSE                                                   X04
     C                   Z-ADD     W#50          I4PRI#
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Get the second severity.
      *          ------------------------------------------
      *
      *                    --------------------------------
      *                    Get the old severity if change.
      *                    --------------------------------
     C                   EVAL      W#SEW# = *ZEROS
     C                   IF        P4ACTC = '20'                                B02
     C                   MOVE      I4HEDC        K#HEDC
     C                   EVAL      K#TABC = '02'
     C                   MOVE      NCSEV2        K#SEVC
     C                   EXSR      SRDSL1
     C     $DS1K1        CHAIN     OMHDSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B03
     C                   Z-ADD     DSSEW#        W#SEW#
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Get the new severity.
      *                    --------------------------------
     C                   MOVE      I4HEDC        K#HEDC
     C                   EVAL      K#TABC = '02'
     C                   MOVE      I4SEV2        K#SEVC
     C                   EXSR      SRDSL1
     C     $DS1K1        CHAIN     OMHDSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEL     'OMQ6018'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     VAR(4)        MC(1)
     C                   MOVEA     I4SEV2        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#SEV2
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
      *
      *                    --------------------------------
      *                    Calculate the priority.
      *                    --------------------------------
     C     I4PRI#        ADD       DSSEW#        W#50
     C     W#50          SUB       W#SEW#        W#50
     C                   IF        W#50 > 999                                   B03
     C                   Z-ADD     999           I4PRI#
     C                   ELSE                                                   X03
     C                   IF        W#50 < *ZEROS                                B04
     C                   EVAL      I4PRI# = *ZEROS
     C                   ELSE                                                   X04
     C                   Z-ADD     W#50          I4PRI#
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Get the third severity.
      *          ------------------------------------------
      *
      *                    --------------------------------
      *                    Get the old severity if change.
      *                    --------------------------------
     C                   EVAL      W#SEW# = *ZEROS
     C                   IF        P4ACTC = '20'                                B02
     C                   MOVE      I4HEDC        K#HEDC
     C                   EVAL      K#TABC = '03'
     C                   MOVE      NCSEV3        K#SEVC
     C                   EXSR      SRDSL1
     C     $DS1K1        CHAIN     OMHDSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B03
     C                   Z-ADD     DSSEW#        W#SEW#
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Get the new severity.
      *                    --------------------------------
     C                   MOVE      I4HEDC        K#HEDC
     C                   EVAL      K#TABC = '03'
     C                   MOVE      I4SEV3        K#SEVC
     C                   EXSR      SRDSL1
     C     $DS1K1        CHAIN     OMHDSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEL     'OMQ6018'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     VAR(4)        MC(1)
     C                   MOVEA     I4SEV3        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#SEV3
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
      *
      *                    --------------------------------
      *                    Calculate the priority.
      *                    --------------------------------
     C     I4PRI#        ADD       DSSEW#        W#50
     C     W#50          SUB       W#SEW#        W#50
     C                   IF        W#50 > 999                                   B03
     C                   Z-ADD     999           I4PRI#
     C                   ELSE                                                   X03
     C                   IF        W#50 < *ZEROS                                B04
     C                   EVAL      I4PRI# = *ZEROS
     C                   ELSE                                                   X04
     C                   Z-ADD     W#50          I4PRI#
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Priority.
      *          ------------------------------------------
     C                   IF        P4ACTC = '33'                                B01
     C                   IF        I4PRI# = *ZERO                               B02
     C                   Z-ADD     NCPRI#        I4PRI#
     C                   MOVE      '3'           I4PCM
     C                   ENDIF                                                  E02
      *
     C                   SELECT                                                 B02
     C                   WHEN      I4PCM = '1'                                  X02
     C     NCPRI#        ADD       I4PRI#        W#50
     C                   IF        W#50 > 999                                   B03
     C                   Z-ADD     999           I4PRI#
     C                   ELSE                                                   X03
     C                   Z-ADD     W#50          I4PRI#
     C                   ENDIF                                                  E03
      *
     C                   WHEN      I4PCM = '2'                                  X02
     C     NCPRI#        SUB       I4PRI#        I4PRI#                 99
     C                   IF        *IN99 = *ON
     C                   EVAL      I4PRI# = *ZEROS
     C                   ENDIF
      *
     C                   WHEN      I4PCM = '3'                                  X02
     C                   Z-ADD     I4PRI#        I4PRI#
     C                   ENDSL                                                  E02
      *
      *          ------------------------------------------
      *          Escalation counter
      *          ------------------------------------------
     C                   IF        I4ESC# = *ZERO                               B02
     C                   Z-ADD     NCESC#        I4ESC#
     C                   MOVE      '4'           I4CCM
     C                   ENDIF                                                  E02
      *
     C                   SELECT                                                 B02
     C                   WHEN      I4CCM = '1'                                  X02
     C     NCESC#        ADD       I4ESC#        W#50
     C                   IF        W#50 > 999                                   B03
     C                   Z-ADD     999           I4ESC#
     C                   ELSE                                                   X03
     C                   Z-ADD     W#50          I4ESC#
     C                   ENDIF                                                  E03
      *
     C                   WHEN      I4CCM = '2'                                  X02
     C     NCESC#        SUB       I4ESC#        I4ESC#                 99
     C                   IF        *IN99 = *ON
     C                   EVAL      I4ESC# = *ZEROS
     C                   ENDIF
      *
     C                   WHEN      I4CCM = '3'                                  X02
     C                   Z-ADD     I4ESC#        I4ESC#
     C                   ENDSL                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The escalation counter is zero for add.
      *          Incremented for *ESCALATE.
      *          Unchanged for other actions.
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      P4ACTC = '60'                                X01
     C                             OR P4ACTC = '30'
     C                   EVAL      I4ESC# = *ZEROS
      *
     C                   WHEN      P4ACTC = '33'                                X01
     C                   IF        I4CCM = '4'                                  B02
     C     NCESC#        ADD       1             I4ESC#
     C                   ENDIF                                                  E02
      *
     C                   OTHER                                                  X01
     C                   Z-ADD     NCESC#        I4ESC#
      *
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          The escalation date is *SAME or *CURRENT
      *          ------------------------------------------
     C                   IF        P4ACTC = '33'                                B01
     C                   IF        I4IEDT = 999999                              B02
     C                             OR I4IEDT = 9999999                          B02
     C                   Z-ADD     NCIEDT        I4IEDT
     C                   ELSE                                                   X02
     C                   EVAL      D#YY = UYEAR
     C                   EVAL      D#MM = UMONTH
     C                   EVAL      D#DD = UDAY
     C                   EVAL      *IN99 = (D#YY <= 40)
     C                   EVAL      D#CCC = *IN99
     C                   Z-ADD     D#DATE        I4IEDT
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          The escalation time is *SAME, *CURRENT or passed.
      *          ------------------------------------------
     C                   IF        I4IETM = 999999                              B02
     C                             OR I4IETM = 9999999                          B02
     C                   Z-ADD     NCIETM        I4IETM
     C                   ELSE                                                   X02
     C                   IF        I4IETM = 888888                              B03
     C                             OR I4IETM = 8888888                          B03
     C                   TIME                    I4IETM
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          If not add then use the old values.
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   IF        P4ACTC <> '60'                               B02
     C                             AND P4ACTC <> '30'
     C                             AND P4ACTC <> '20'
     C                   Z-ADD     NCPRI#        I4PRI#
     C                   Z-ADD     NCESC#        I4ESC#
     C                   Z-ADD     NCIEDT        I4IEDT
     C                   Z-ADD     NCIETM        I4IETM
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The registered by user id is filled with this
      *          user if this is add mode.
      *          ------------------------------------------
     C                   IF        P4ACTC = '60'                                B01
     C                             OR P4ACTC = '30'
     C                   IF        I4RUID = *BLANKS                             B02
     C                             OR I4RUID = C#SAME
     C                   MOVE      D#USID        I4RUID
     C                   ENDIF                                                  E02
      *
     C                   ELSE                                                   X01
     C                   MOVE      NCRUID        I4RUID
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The allocated by user.
      *          ------------------------------------------
      *                    No allocation. Stays the same.
      *                    --------------------------------
     C                   IF        P4ACTC <> '15'                               B01
     C                             AND P4ACTC <> '85'
     C                   MOVE      NCAUID        I4AUID
     C                   ELSE                                                   X01
      *
      *                    --------------------------------
      *                    You can only allocate for other users if
      *                    you are '9' or allow allocate indicator
      *                    is on.
      *                    --------------------------------
     C     $HDEL1        CHAIN     OMHDEL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        I4AUID <> D#USID                             B02
     C                             AND D7SUL# <> '9'
     C                             AND P4ACTC = '15'
     C                             AND DEALOC <> '1'
      *
     C                             OR NCAUID <> D#USID
     C                             AND D7SUL# <> '9'
     C                             AND P4ACTC = '85'
     C                             AND DEALOC <> '1'
      *
     C                   MOVEL     'OMQ502A'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4INC#        MC(1)
     C                   MOVEA     I4HEDC        MC(11)
     C                   MOVEA     I4AUID        MC(16)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#AUID
     C                   EXSR      SRERR
     C                   ELSE                                                   X02
      *
      *                    --------------------------------
      *                    Allocate only if not allocated by another
      *                    user.
      *                    --------------------------------
     C                   IF        P4ACTC = '15'                                B03
     C                             AND NCAUID <> *BLANKS
     C                             AND NCAUID <> D#USID
     C                   MOVEL     'OMQ502B'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4INC#        MC(1)
     C                   MOVEA     I4HEDC        MC(11)
     C                   MOVEA     NCAUID        MC(16)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#AUID
     C                   EXSR      SRERR
     C                   ELSE                                                   X03
      *
     C                   IF        P4ACTC = '85'                                B04
     C                   MOVE      *BLANKS       I4AUID
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Short incident description must be filled.
      *          ------------------------------------------
     C                   IF        I4SHID = *BLANKS                             B01
     C                   MOVEL     'OMQ502C'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   EVAL      W#FLD1 = C#SHID
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Configuration not supported yet.
      *          ------------------------------------------
     C                   EVAL      I4CFL# = *ZERO
     C                   MOVE      *BLANKS       I4CFIC
      *
      *          ------------------------------------------
      *          User defined programs.
      *          ------------------------------------------
     C                   IF        D8INFP <> *BLANKS                            B01
     C                   IF        P4ACTC = '60'                                B02
     C                             OR P4ACTC = '30'
     C                             OR P4ACTC = '20'
      *
      *                    --------------------------------
      *                    fill the parameters.
      *                    --------------------------------
     C                   IF        P4ACTC = '30'                                B03
     C                   MOVE      I4THED        P#HEDC
     C                   MOVE      I4TINC        P#INC#
     C                   ELSE                                                   X03
     C                   MOVE      I4HEDC        P#HEDC
     C                   MOVE      I4INC#        P#INC#
     C                   ENDIF                                                  E03
      *
     C                   IF        P4ACTC = '20'                                B03
     C                   EVAL      P#ACTC = '20'
     C                   ELSE                                                   X03
     C                   EVAL      P#ACTC = '60'
     C                   ENDIF                                                  E03
      *
     C                   MOVEL(P)  D8INFL        W#OBJQ
     C                   EVAL      W#OBJQ = %TRIMR(W#OBJQ) + '/'
     C     W#OBJQ        CAT       D8INFP:0      W#OBJQ
     C                   CALL      W#OBJQ        $OBJQ                  9999
      *
      *          ------------------------------------------
      *          Skip other tests if program not found
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF                                 B03
      *
      *                    --------------------------------
      *                    If exit then exist.
      *                    --------------------------------
     C                   IF        P#STAT = C#EXIT                              B04
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E04
      *
      *                    --------------------------------
      *                    Field 1 in error?
      *                    --------------------------------
     C                   IF        P#F1ST <> C#NORM                             B04
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4USF1        MC(1)
     C                   MOVEA     C#USF1        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#USF1
     C                   EXSR      SRERR
     C                   ENDIF                                                  E04
      *
      *                    --------------------------------
      *                    Field 2 in error?
      *                    --------------------------------
     C                   IF        P#F2ST <> C#NORM                             B04
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     I4USF2        MC(1)
     C                   MOVEA     C#USF2        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#USF2
     C                   EXSR      SRERR
     C                   ENDIF                                                  E04
      *
      *                    --------------------------------
      *                    Field 3 in error?
      *                    --------------------------------
     C                   IF        P#F3ST <> C#NORM                             B04
     C                   MOVEL     'OMQA003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   Z-ADD     I4USF3        D#USF3
     C                   MOVEA     D#USFA        MC(1)
     C                   MOVEA     C#USF3        MC(33)
     C                   MOVEA     MC            I4MSGD
     C                   EVAL      W#FLD1 = C#USF3
     C                   EXSR      SRERR
     C                   ENDIF                                                  E04
      *
      *                    --------------------------------
      *                    Fill the fields with their new values
      *                    --------------------------------
     C                   MOVE      P#USF1        I4USF1
     C                   MOVE      P#USF2        I4USF2
     C                   MOVE      P#USF3        I4USF3
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
     C                   MOVEL     'OMQ6003'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     P4ACTC        MC(1)
     C                   MOVEA     D#PGM         MC(3)
     C                   MOVEA     MC            I4MSGD
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
      *          Send message when I4MSQ is filled
      *          ------------------------------------------
     C                   IF        I4MSQ <> *BLANKS                             B01
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
     C                   EVAL      P#MIDC = I4MSID
     C                   EVAL      P#MSGQ = C#API
     C                   EVAL      P#MDL# = 125
     C                   EVAL      P#MSTC = '*DIAG'
     C                   EVAL      P#PMQC = I4MSQ
     C                   EVAL      P#PSC# = 0
     C                   CALLP     QMHSNDPM ( P#MIDC : P#MSGQ : I4MSGD :
     C                             P#MDL# : P#MSTC : P#PMQC : P#PSC# :
     C                             P#MSKC : P#ERR )
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do only when an error occured
      *          ------------------------------------------
     C                   IF        I4MSTP = 'E'                                 B01
      *
      *          ------------------------------------------
      *          Unlock a possible locked record
      *          ------------------------------------------
     C                   UNLOCK    OMINCL1                              99
      *
      *          ------------------------------------------
      *          Count the error number
      *          ------------------------------------------
     C                   ADD       1             W#ERR
      *
      *          ------------------------------------------
      *          Execute TTLR when no message queue is used
      *          ------------------------------------------
     C                   IF        I4MSQ = *BLANKS                              B02
     C                             OR I#QUIT = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Reset the message type
      *          ------------------------------------------
     C                   MOVE      *BLANK        I4MSTP
     C                   MOVE      *BLANK        I4MSGD
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
      *          Open OMRIRL1.
      *          ------------------------------------------
     C     SRIRL1        BEGSR
     C                   IF        I#IRL1 <> *ON                                B01
     C                   OPEN      OMRIRL1
     C                   EVAL      I#IRL1 = *ON
     C                   ENDIF                                                  E01
     C                   ENDSR
      *
      *          ------------------------------------------
      *          Open OMINCL1
      *          ------------------------------------------
     C     SRNCL1        BEGSR
     C                   IF        I#NCL1 <> *ON                                B01
     C                   EVAL      I#NCL1 = *ON
     C                   OPEN      OMINCL1
     C                   ENDIF                                                  E01
     C                   ENDSR
      *
      *          ------------------------------------------
      *          Open OMINCL4
      *          ------------------------------------------
     C     SRNCL4        BEGSR
     C                   IF        I#NCL4 <> *ON                                B01
     C                   EVAL      I#NCL4 = *ON
     C                   OPEN      OMINCL4
     C                   ENDIF                                                  E01
     C                   ENDSR
      *
      *          ------------------------------------------
      *          Open OMSREL1
      *          ------------------------------------------
     C     SRREL1        BEGSR
     C                   IF        I#REL1 <> *ON                                B01
     C                   EVAL      I#REL1 = *ON
     C                   OPEN      OMSREL1
     C                   ENDIF                                                  E01
     C                   ENDSR
      *
      *          ------------------------------------------
      *          Open OMHDSL1
      *          ------------------------------------------
     C     SRDSL1        BEGSR
     C                   IF        I#DSL1 <> *ON                                B01
     C                   EVAL      I#DSL1 = *ON
     C                   OPEN      OMHDSL1
     C                   ENDIF                                                  E01
     C                   ENDSR
      *
      *          ------------------------------------------
      *          Open OMHDSL3
      *          ------------------------------------------
     C     SRDSL3        BEGSR
     C                   IF        I#DSL3 <> *ON                                B01
     C                   EVAL      I#DSL3 = *ON
     C                   OPEN      OMHDSL3
     C                   ENDIF                                                  E01
     C                   ENDSR
      *
      *          ------------------------------------------
      *          Open OMHITL1
      *          ------------------------------------------
     C     SRITL1        BEGSR
     C                   IF        I#ITL1 <> *ON                                B01
     C                   EVAL      I#ITL1 = *ON
     C                   OPEN      OMHITL1
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
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIO   - I/O to OMINCR.                                     *
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
     C                   IF        I4UPB = *ON                                  B01
     C                   MOVEL     DSINCR        P4BUF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Must the database be updated.
      *          ------------------------------------------
     C                   IF        I4UPD = *ON                                  B01
      *
      *          ------------------------------------------
      *          Must the record be written.
      *          ------------------------------------------
     C                   SELECT                                                 B02
     C                   WHEN      P4ACTC = '60'                                X02
     C                             OR P4ACTC = '30'
     C                   EVAL      NCIEDT = 0
     C                   EVAL      NCIETM = 0
     C                   WRITE     OMINCR
      *
      *          ------------------------------------------
      *          Must the record be removed.
      *          ------------------------------------------
     C                   WHEN      P4ACTC = '40'                                X02
     C                   DELETE    OMINCR
      *
      *          ------------------------------------------
      *          Must the record be updated
      *          ------------------------------------------
     C                   OTHER                                                  X02
      *
      *          ------------------------------------------
      *          Reset date/time escalation?
      *          ------------------------------------------
     C                   IF        NCSTAT <> S#STS2                             B03
     C                             AND NCSTAT <> C#CMP                          B03
     C                   EVAL      NCIEDT = 0
     C                   EVAL      NCIETM = 0
     C                   ENDIF                                                  E03
      *
     C                   UPDATE    OMINCR
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
     C                   MOVEL     'OMQ500B'     I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVEA     D#PGM         MC(1)
     C                   MOVEA     P4INTL        MC(11)
     C                   MOVEA     MC            I4MSGD
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
      * SRWDAQ - Write entry in the data queue                      *
      *                                                             *
      * This routine is called after actions where performed        *
      * on the incident. An entry is written in the incident        *
      * monitor data queue. This program executes the status        *
      * trigger programs and does mail processing.                  *
      *                                                             *
      * The propagation delay incidator determines wether the       *
      * action must be propagated immediately or if it must wait.   *
      * If it must wait (I4PDI = '1'), the incident is not placed   *
      * in the data queue but stored in an array.                   *
      *                                                             *
      * If no delay is wanted, the contents of the array is         *
      * moved to the data queue.                                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRWDAQ        BEGSR
      *
      *          ------------------------------------------
      *          Empty the array if out of entries, or
      *          propagation delay no longer wanted.
      *          ------------------------------------------
     C                   IF        PI = 25                                      B01
     C                             OR I4PDI <> *ON
     C                   EXSR      SRPDAQ
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the data queue or the array.
      *          ------------------------------------------
     C                   MOVE      D#USID        I44USI
     C                   MOVE      D#JOB         I44JNM
     C                   Z-ADD     D#JOB#        I44JNR
     C                   MOVE      'IN'          I44ENT
     C                   MOVE      P4ACTC        I44ACT
     C                   MOVE      *BLANKS       I44R1
      *
     C                   MOVE      NCHEDC        I44HED
     C                   MOVE      NCINC#        I44INC
     C                   MOVE      S#SUL#        I44OSL
     C                   MOVE      S#STS2        I44OST
     C                   MOVE      S#STRC        I44OSR
     C                   MOVE      NCSUL#        I44NSL
     C                   MOVE      NCSTAT        I44NST
     C                   MOVE      NCSTRC        I44NSR
      *
      *          ------------------------------------------
      *          Write in data queue or store local.
      *          ------------------------------------------
     C                   IF        I4PDI <> *ON                                 B01
     C                   EVAL      P#OBJC = 'OMH044C'
     C                   EVAL      P#OBJL = '*LIBL'
     C                   EVAL      P#OPTI = '1'
     C                   EVAL      P#LEN = 1024
     C                   EVAL      P#WAIT = 0
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH044C ( P#OBJC : P#OBJL : P#OPTI :
     C                             D#44IN : P#LEN : P#WAIT : P#STAT )
     C                   ELSE                                                   X01
     C                   ADD       1             PI
     C                   EVAL      %OCCUR(D#PDI) = PI
     C                   MOVEL     D#44IN        D#PDI
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPDAQ - Propagate to data queue                            *
      *                                                             *
      * This routine moves the contents of the propagation delay    *
      * array to the data queue.                                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPDAQ        BEGSR
      *
      *          ------------------------------------------
      *          Empty the array
      *          ------------------------------------------
     C                   DOW       PI > *ZEROS                                  B01
     C                   EVAL      %OCCUR(D#PDI) = PI
     C                   MOVEL(P)  D#PDI         D#44IN
      *
      *          ------------------------------------------
      *          Write in data queue.
      *          ------------------------------------------
     C                   EVAL      P#OBJC = 'OMH044C'
     C                   EVAL      P#OBJL = '*LIBL'
     C                   EVAL      P#OPTI = '1'
     C                   EVAL      P#LEN = 1024
     C                   EVAL      P#WAIT = 0
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH044C ( P#OBJC : P#OBJL : P#OPTI :
     C                             D#44IN : P#LEN : P#WAIT : P#STAT )
     C                   SUB       1             PI
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
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
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      W#ERR = *ZEROS
     C                   EVAL      PI = *ZEROS
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
      *          ------------------------------------------
      *          Parameter list for OMX905C (Get user profile)
      *          ------------------------------------------
     C     $X905C        PLIST
     C                   PARM                    P#USID
     C                   PARM                    P#USCL
     C                   PARM                    P#GRID
     C                   PARM                    P#GRCL
     C                   PARM                    P#USDC
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMX909 (Check date)
      *          ------------------------------------------
     C     $MX909        PLIST
     C                   PARM                    P#CHKD
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMX929 (write incident log)
      *          ------------------------------------------
     C     $MX929        PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    P#INC#
     C                   PARM                    P#ACTC
     C                   PARM                    P#MSID
     C                   PARM                    P#MSGD
     C                   PARM      'OMHAPI'      P#MSGF
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMX967 (Generate INC#)
      *          ------------------------------------------
     C     $MX967        PLIST
     C                   PARM      I4HEDC        P#HEDC
     C                   PARM      '1'           P#OPTI
     C     I4INC#        PARM                    P#INC#
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
      *          Parameter list for OMH044C (Write entry in data queue)
      *          ------------------------------------------
     C     $H044C        PLIST
     C                   PARM      'OMH044C'     P#OBJC
     C                   PARM      '*LIBL'       P#OBJL
     C                   PARM      '1'           P#OPTI
     C                   PARM                    D#44IN
     C                   PARM      1024          P#LEN
     C                   PARM      0             P#WAIT
     C                   PARM      C#NORM        P#STAT
      *
      *          ------------------------------------------
      *          Check user fields
      *          ------------------------------------------
     C     $OBJQ         PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    P#INC#
     C                   PARM                    P#ACTC
     C                   PARM      I4USF1        P#USF1
     C                   PARM      '*NORM'       P#F1ST
     C                   PARM      I4USF2        P#USF2
     C                   PARM      '*NORM'       P#F2ST
     C                   PARM      I4USF3        P#USF3
     C                   PARM      '*NORM'       P#F3ST
     C                   PARM      '*NORM'       P#STAT
      *
      *          ------------------------------------------
      *          OMINCLn key list Incident File
      *          ------------------------------------------
     C     $NC1K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#INC#
      *
      *          ------------------------------------------
      *          OMRIRL1 Incident relations file
      *          ------------------------------------------
     C     $IR1K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#INC#
     C                   KFLD                    K#IRTP
     C                   KFLD                    K#APPC
     C                   KFLD                    K#IRIT
      *
      *          ------------------------------------------
      *          OMERRL1 OMS request file
      *          ------------------------------------------
     C     $RR1K1        KLIST
     C                   KFLD                    K#APPC
     C                   KFLD                    K#ERR#
      *
      *          ------------------------------------------
      *          OMSREL1 Status route entries.
      *          ------------------------------------------
     C     $RE1K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#STRC
      *
     C     $RE1K2        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#STRC
     C                   KFLD                    K#RES#
      *
      *          ------------------------------------------
      *          OMHDSL1 Helpdesk severities
      *          ------------------------------------------
     C     $DS1K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#TABC
     C                   KFLD                    K#SEVC
      *
      *          ------------------------------------------
      *          OMHDSL3 Helpdesk severities
      *          ------------------------------------------
     C     $DS3K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#TABC
      *
      *          ------------------------------------------
      *          OMHITL1 Incident Types
      *          ------------------------------------------
     C     $IT1K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#ITPC
      *
      *          ------------------------------------------
      *          OMCCAL1 Call track numbers
      *          ------------------------------------------
     C     $CA1K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#CTN#
      *
      *          ------------------------------------------
      *          OMCCAL1 Call track numbers
      *          ------------------------------------------
     C     $HDEL1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    D#USID
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
     C                   WHEN      E#FILE = 'OMINCL1 '                          X02
     C                   MOVEL     INCL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMINCL4 '                          X02
     C                   MOVEL     INCL4         P#FILE
      *
     C                   WHEN      E#FILE = 'OMERRL1 '                          X02
     C                   MOVEL     ERRL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMRIRL1 '                          X02
     C                   MOVEL     RIRL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMSREL1 '                          X02
     C                   MOVEL     SREL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHDSL1 '                          X02
     C                   MOVEL     HDSL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHDSL3 '                          X02
     C                   MOVEL     HDSL3         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHITL1 '                          X02
     C                   MOVEL     HITL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMCCAL1 '                          X02
     C                   MOVEL     CCAL1         P#FILE
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
      *          No error processing possible, exit immediately
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Exit.
      *          ------------------------------------------
     C                   EVAL      D#PARM = 0
     C                   MOVE      'E'           I4MSTP
     C                   MOVE      'OMQ5012'     I4MSID
     C                   MOVEL     I4ERR         P4ERR
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
     C                   MOVEL     I4ERR         P4ERR
     C                   MOVEA     ERR           I4FLDA
     C                   MOVEL     I4FLD         P4FLD
     C                   IF        I4UPB <> *OFF                                B02
     C                   MOVEL     I4BUF         P4BUF
     C                   MOVEL     I4PRC         P4PRC
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          When multiple messages are send, fill 'E'
      *          ------------------------------------------
     C                   IF        W#ERR > *ZERO                                B02
     C                             AND I4MSQ <> *BLANK
     C                   MOVEL     *BLANK        I4MSID
     C                   MOVEL     'E'           I4MSTP
     C                   MOVE      *BLANKS       I4MSGD
     C                   MOVEL     I4ERR         P4ERR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          When no parm or I4EOP = '1' then close
      *          ------------------------------------------
     C                   IF        D#PARM = *ZERO                               B01
     C                             OR I4EOP = '1'
      *
      *          ------------------------------------------
      *          Close number generator
      *          ------------------------------------------
     C                   IF        I#X967 = *ON                                 B02
     C                   CALLP     OMX967()
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Close log writer.
      *          ------------------------------------------
     C                   IF        I#X929 = *ON                                 B02
     C                   CALLP     OMX929()
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Dequeue data queue array
      *          ------------------------------------------
     C                   EXSR      SRPDAQ
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
INC0003  -  incident               2
HLP0006  -  call track number      3
SEV0001  -  severity code          4
INC0001  -  incident type          5
ERR0001  -  request                6
APP0001  -  application            7
INC0007  -  accumulated incident   8
