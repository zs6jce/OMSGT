     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMQPRTIN                                      *
      * Function ...: API print request                             *
      *                                                             *
      * Author .....: Remain Software                               *
      *                                                             *
      * This program is used to print incidents                     *
      *                                                             *
      * ----------------------------------------------------------- *
      *                                                             *
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMINCL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(INCL1)
     FOMRIRL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(RIRL1)
     FOMQAHL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(QAHL1)
     FOMCCAL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(CCAL1)
     FOMCCOL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(CCOL1)
     FOMHQUL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(HQUL1)
     FOMHQLL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(HQLL1)
     FOMANSL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(ANSL1)
     FOMTIHL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(TIHL1)
     FOMTIDL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(TIDL1)
     FOMQPRTIN  O    E             PRINTER INFSR(*PSSR)
     F                                     OFLIND(*IN01)
     F                                     INFDS(QPRTIN)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension spec's                                             *
      *                                                              *
      * DAT   For subtracting two dates       (compile time array)   *
      * GEN   Message id general headers      (compile time array)   *
      * COV   Message id cover page           (compile time array)   *
      * END   Message id end page             (compile time array)   *
      * RSL   Message id request short list   (compile time array)   *
      * FSL   Message id fix     short list   (compile time array)   *
      * SSL   Message id solut.  short list   (compile time array)   *
      * FSC   Message id fix     short compnd (compile time array)   *
      * SSC   Message id solut.  short compnd (compile time array)   *
      * RSQ   Message id request short qualit.(compile time array)   *
      * RQS   Message id request detail       (compile time array)   *
      * FIX   Message id fix     detail       (compile time array)   *
      * SOL   Message id sol     detail       (compile time array)   *
      * VAR   Message text retrieved                                 *
      * N01   Table 01 Priority numeric                              *
      * C01   Table 01 Priority code                                 *
      * N11   Table 01 Reason   numeric                              *
      * C11   Table 01 Reason   code                                 *
      * N21   Table 01 Req/Fix type numeric                          *
      * C21   Table 01 Req/Fix type code                             *
      * MC    Used to fill message variables in message data parm.   *
      * ERR   Max. 30 elements of field errors, during checking      *
      * ------------------------------------------------------------ *
     D DAT             S              3  0 DIM(12) CTDATA PERRCD(1)
     D GEN             S              7    DIM(2) CTDATA PERRCD(1)
     D COV             S              7    DIM(25) CTDATA PERRCD(1)
     D END             S              7    DIM(8) CTDATA PERRCD(1)
     D RSL             S              7    DIM(1) CTDATA PERRCD(1)
     D FSL             S              7    DIM(1) CTDATA PERRCD(1)
     D SSL             S              7    DIM(1) CTDATA PERRCD(1)
     D FSC             S              7    DIM(1) CTDATA PERRCD(1)
     D SSC             S              7    DIM(1) CTDATA PERRCD(1)
     D RSQ             S              7    DIM(2) CTDATA PERRCD(1)
     D RQS             S              7    DIM(11) CTDATA PERRCD(1)
     D FIX             S              7    DIM(22) CTDATA PERRCD(1)
     D SOL             S              7    DIM(17) CTDATA PERRCD(1)
     D ISL             S              7    DIM(3) CTDATA PERRCD(1)
     D INC             S              7    DIM(22) CTDATA PERRCD(1)
     D TYP             S              7    DIM(5) CTDATA PERRCD(1)
     D TXT             S             25    DIM(5)
     D VAR             S            132    DIM(25)
     D N01             S              1    DIM(10)
     D C01             S              5    DIM(10)
     D N11             S              1    DIM(10)
     D C11             S              5    DIM(10)
     D N21             S              1    DIM(10)
     D C21             S              5    DIM(10)
     D MC              S              1    DIM(256)
     D ERR             S              4    DIM(30)
     D ENV             S              5    DIM(49)
     D EN#             S              5  0 DIM(49)
     D AUT             S              1    DIM(49)
     D LVC             S              1    DIM(49)
     D LV#             S              3  0 DIM(49)
     D FLT             S             15    DIM(9)
      *
      *
     D AVG             S              1    DIM(7)
     D AV#             S              1    DIM(7)
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
     D ERRL2           DS            57
     D ERRL3           DS            57
     D ERRL4           DS            57
     D ERDL1           DS            57
     D RSRL1           DS            57
     D ERSL1           DS            57
     D ESDL1           DS            57
     D EROL1           DS            57
     D EODL1           DS            57
     D OBJL8           DS            57
     D MBRL1           DS            57
     D TABL1           DS            57
     D SOVL1           DS            57
     D SOVL2           DS            57
     D INCL1           DS            57
     D RIRL1           DS            57
     D QAHL1           DS            57
     D CCAL1           DS            57
     D CCOL1           DS            57
     D HQUL1           DS            57
     D HQLL1           DS            57
     D ANSL1           DS            57
     D TIHL1           DS            57
     D TIDL1           DS            57
      *
      *          ------------------------------------------
      *          Helpdesk data structures.
      *          ------------------------------------------
      /COPY QCPYLESRC,OMHLPDS
      *
      *          ------------------------------------------
      *          Current print line
      *          ------------------------------------------
     D QPRTIN          DS           380
     D D#REG#                367    368B 0
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  E#STAT           *STATUS
     D  D#PGM                  1     10
     D  E#FILE               201    208
     D  D#USID               254    263
      *
      *          ------------------------------------------
      *          Convertor alpha to array counter
      *          ------------------------------------------
     D                 DS
     D IHITXT                         1
     D  Z1                            1  0 OVERLAY(IHITXT)
      *
      *          ------------------------------------------
      *          Date convertor input
      *          ------------------------------------------
     D                 DS
     D P#DTIN                        16
     D  D#IDAT                        7  0 OVERLAY(P#DTIN)
     D   D#CIN                        1  0 OVERLAY(D#IDAT)
     D   D#DTIN                       6  0 OVERLAY(D#IDAT:2)
     D  D#TMIN                        6  0 OVERLAY(P#DTIN:8)
     D  D#MSIN                        3  0 OVERLAY(P#DTIN:14)
      *
      *          ------------------------------------------
      *          Date convertor output
      *          ------------------------------------------
     D                 DS
     D P#DTOT                        16
     D  D#ODAT                        7  0 OVERLAY(P#DTOT)
     D   D#COT                        1  0 OVERLAY(D#ODAT)
     D   D#DTOT                       6  0 OVERLAY(D#ODAT:2)
     D  D#TMOT                        6  0 OVERLAY(P#DTOT:8)
     D  D#MSOT                        3  0 OVERLAY(P#DTOT:14)
      *
      *          ------------------------------------------
      *          DS for subtracting two dates
      *          ------------------------------------------
     D                 DS
     D D#DT1                          7  0
     D  D#Y1                          3  0 OVERLAY(D#DT1)
     D  D#M1                          2  0 OVERLAY(D#DT1:4)
     D  D#D1                          2  0 OVERLAY(D#DT1:6)
     D D#DT2                          7  0
     D  D#Y2                          3  0 OVERLAY(D#DT2)
     D  D#M2                          2  0 OVERLAY(D#DT2:4)
     D  D#D2                          2  0 OVERLAY(D#DT2:6)
      *
      *          ------------------------------------------
      *          Copy member for buffer from CL - OMQPRTINDS
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQPRTINDS
      *
      *
      *          ------------------------------------------
      *          Input buffer data structure - Redefined fields
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Input buffer data structure for all codes
      *          ------------------------------------------
     D  BCINFR                65     71
     D  BCINTO                72     78
     D  BCWCFR                83     89
     D  BCWCTO                90     96
     D  BCRCFR                97    103
     D  BCRCTO               104    110
      *
      *          ------------------------------------------
      *          DS for lower selection values character
      *          ------------------------------------------
     D D#LOWC          DS
     D D#PDLL                         5
     D D#RQTL                         1
     D D#PRIL                         1
     D D#REAL                         1
     D D#USIL                        10
     D D#TELL                        15
     D D#ADDL                         8
     D D#USRL                         8
      *
      *          ------------------------------------------
      *          DS for lower selection numeric
      *          ------------------------------------------
     D D#LOWN          DS
     D D#INFR                         7  0
     D D#WCFR                 15     21  0
     D D#RCFR                 29     35  0
      *
      *          ------------------------------------------
      *          DS for higher selection values character
      *          ------------------------------------------
     D D#HIGC          DS
     D D#PDLH                         5
     D D#RQTH                         1
     D D#PRIH                         1
     D D#REAH                         1
     D D#USIH                        10
     D D#TELH                        15
     D D#ADDH                         8
     D D#USRH                         8
      *
      *          ------------------------------------------
      *          DS for higher selection numeric
      *          ------------------------------------------
     D D#HIGN          DS
     D D#INTO                         7  0
     D D#WCTO                 15     21  0
     D D#RCTO                 29     35  0
      *
      *          ------------------------------------------
      *          Data structure for Follower/Leader Sol. table
      *          ------------------------------------------
     D                 DS
     D FMFAPA                       135
     D  FMFAP1                        5    OVERLAY(FMFAPA)
     D  FMFIX1                       10    OVERLAY(FMFAPA:6)
     D  FMFAP2                        5    OVERLAY(FMFAPA:16)
     D  FMFIX2                       10    OVERLAY(FMFAPA:21)
     D  FMFAP3                        5    OVERLAY(FMFAPA:31)
     D  FMFIX3                       10    OVERLAY(FMFAPA:36)
     D  FMFAP4                        5    OVERLAY(FMFAPA:46)
     D  FMFIX4                       10    OVERLAY(FMFAPA:51)
     D  FMFAP5                        5    OVERLAY(FMFAPA:61)
     D  FMFIX5                       10    OVERLAY(FMFAPA:66)
     D  FMFAP6                        5    OVERLAY(FMFAPA:76)
     D  FMFIX6                       10    OVERLAY(FMFAPA:81)
     D  FMFAP7                        5    OVERLAY(FMFAPA:91)
     D  FMFIX7                       10    OVERLAY(FMFAPA:96)
     D  FMFAP8                        5    OVERLAY(FMFAPA:106)
     D  FMFIX8                       10    OVERLAY(FMFAPA:111)
     D  FMFAP9                        5    OVERLAY(FMFAPA:121)
     D  FMFIX9                       10    OVERLAY(FMFAPA:126)
     D D#FLT                         15
     D  D#FAP                         5    OVERLAY(D#FLT)
     D  D#FIX                        10    OVERLAY(D#FLT:6)
      *
      *          ------------------------------------------
      *          Workfield line counting
      *          ------------------------------------------
     D                 DS
     D D#HLP#                         4  0
      *
     D D#AVG           DS
     D D#AVG#                         7  2
     D  D#AVGA                        7    OVERLAY(D#AVG#)
      *
      *          ------------------------------------------
      *          Version numbers - Data structure
      *          ------------------------------------------
     D                 DS
     D FMVRSB                         7
     D  VB                            1    INZ('V')
     D                                     OVERLAY(FMVRSB)
     D  FMVB2#                        2    OVERLAY(FMVRSB:2)
     D   FMVB1#                       1    OVERLAY(FMVB2#)
     D  SB                            1    INZ(' ')
     D                                     OVERLAY(FMVRSB:4)
     D  MB                            1    INZ('M')
     D                                     OVERLAY(FMVRSB:5)
     D  FMMB2#                        2    OVERLAY(FMVRSB:6)
     D   FMMB1#                       1    OVERLAY(FMMB2#)
     D FMVRS#                         7
     D  VR                            1    INZ('V')
     D                                     OVERLAY(FMVRS#)
     D  FMVR2#                        2    OVERLAY(FMVRS#:2)
     D   FMVR1#                       1    OVERLAY(FMVR2#)
     D  SR                            1    INZ(' ')
     D                                     OVERLAY(FMVRS#:4)
     D  MR                            1    INZ('M')
     D                                     OVERLAY(FMVRS#:5)
     D  FMMO2#                        2    OVERLAY(FMVRS#:6)
     D   FMMO1#                       1    OVERLAY(FMMO2#)
      *
      *          ------------------------------------------
      *          Extra DS with 1 pos codes
      *          ------------------------------------------
     D D#XTR           DS
     D R3ERRT                         1
     D R3REAC                         1
     D R3PRIN                         1
      *
      *          ------------------------------------------
      *          DS for retrieving overflow line number
      *          ------------------------------------------
     D D#RCV           DS           440
     D D#OFLW                437    440B 0
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
      *          DS for date conversion
      *          ------------------------------------------
     D  D#DAT#                 1      6  0
     D  D#YEA#                 1      2  0
     D  D#MON#                 3      4  0
     D  D#DAY#                 5      6  0
      *
      *          ------------------------------------------
      *          DS for single parameter values
      *          ------------------------------------------
     D                 DS
     D D#RIRI                        19
     D  D#RIII                        4    OVERLAY(D#RIRI)
     D  D#APPL                        5    OVERLAY(D#RIRI:5)
     D  D#USER                       10    OVERLAY(D#RIRI:10)
      *
      *          ------------------------------------------
      *          DS for numeric conversion
      *          ------------------------------------------
     D                 DS
     D  D#DTA#                 1      6
     D  D#DTN#                 1      6  0
     D  D#TMA#                 7     12
     D  D#TMN#                 7     12  0
     D  D#SUA#                13     13
     D  D#SUN#                13     13
     D  D#CA4#                14     17
     D  D#CA3#                14     16
     D  D#CA2#                14     15
     D  D#CA1#                14     14
     D  D#CN3#                14     16  0
     D  D#CN2#                14     15  0
     D  D#CN1#                14     14  0
     D  UFA                   18     32
     D                                     DIM(15)
     D  D#USF3                18     32  0
     D  CVA                   33     35
     D                                     DIM(3)
     D  D#CVN#                33     35  0
     D  D#ESCD                36     43
     D  D#OPRC                38     40
     D  D#CVA#                41     43
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#API           C                   CONST('OMHAPI    *LIBL')
     D C#MSG1          C                   CONST('OMHDSP    *LIBL')
     D C#TERM          C                   CONST('*TERM')
     D C#NOTF          C                   CONST('*NOTF')
     D C#LAST          C                   CONST('*LAST')
     D C#FRST          C                   CONST('*FIRST')
     D C#FND           C                   CONST('*FND')
     D C#NORM          C                   CONST('*NORM')
     D C#ACT           C                   CONST('*ACT ')
     D C#CMP           C                   CONST('*CMP ')
     D C#CMPT          C                   CONST('*COMPLETE')
     D C#OPN           C                   CONST('*OPEN')
     D C#NOMS          C                   CONST('*NOOMS')
     D C#OMS           C                   CONST('*OMS')
     D C#ALL           C                   CONST('*ALL ')
     D C#LIST          C                   CONST('*LIST    ')
     D C#CPND          C                   CONST('*COMPOUND')
     D C#NONE          C                   CONST('*NONE  ')
     D C#SHRT          C                   CONST('*SHORT ')
     D C#DETL          C                   CONST('*DETAIL')
     D C#NINE          C                   CONST('0999999')
     D C#APPL          C                   CONST('APPL')
     D C#REQ           C                   CONST('REQ ')
     D C#WCFR          C                   CONST('WCFR')
     D C#WCTO          C                   CONST('WCTO')
     D C#RCFR          C                   CONST('RCFR')
     D C#RCTO          C                   CONST('RCTO')
     D C#INFR          C                   CONST('INFR')
     D C#INTO          C                   CONST('INTO')
     D C#INTL          C                   CONST('INTL')
     D C#YES           C                   CONST('*YES')
     D C#NO            C                   CONST('*NO')
     D C#FOLW          C                   CONST('*FOLLOW')
     D C#LEAD          C                   CONST('*LEAD')
     D C#NSRC          C                   CONST('*NO SRC')
     D C#CHG           C                   CONST('*CHANGE')
     D C#CMPL          C                   CONST('*COMPILE')
     D C#COPY          C                   CONST('*COPY')
     D C#OS4           C                   CONST('*OS400')
     D C#GL4           C                   CONST('*GL4')
     D C#HFSO          C                   CONST('*HFSOBJ')
     D C#PAR#          C                   CONST(6)
      *
     D C#0TM9          C                   CONST('0123456789')
     D C#LT            C                   CONST('*LT')
     D C#LE            C                   CONST('*LE')
     D C#GT            C                   CONST('*GT')
     D C#GE            C                   CONST('*GE')
     D C#EQ            C                   CONST('*EQ')
      *
     D C#CAL           C                   CONST('Invalid calculation -
     D                                     range')
     D C#INV           C                   CONST('Calculation range no-
     D                                     t available')
      * Prototype for 'OMQPRTSH'
     D OMQPRTSH        PR                  EXTPGM('OMQPRTSH')
     D NCHEDC_                             LIKE(NCHEDC)
     D NCINC#_                             LIKE(NCINC#)
     D I3STRC_                             LIKE(I3STRC)
     D I3STAT_                             LIKE(I3STAT)
     D P1STAT_                             LIKE(P1STAT)
      *
      *          ------------------------------------------
      *          OMCCAL1 key list customer call
      * Prototype for 'OMH031'
     D OMH031          PR                  EXTPGM('OMH031')
     D NCHEDC_                             LIKE(NCHEDC)
     D NCINC#_                             LIKE(NCINC#)
     D I3ALSC_                             LIKE(I3ALSC)
     D P1STAT_                             LIKE(P1STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMQPRTHS
      * Prototype for 'OMQPRTHS'
     D OMQPRTHS        PR                  EXTPGM('OMQPRTHS')
     D NCHEDC_                             LIKE(NCHEDC)
     D NCINC#_                             LIKE(NCINC#)
     D P#AVGT_                             LIKE(P#AVGT)
     D NCSTRC_                             LIKE(NCSTRC)
     D NCSTAT_                             LIKE(NCSTAT)
     D I3FSTR_                             LIKE(I3FSTR)
     D I3FSTA_                             LIKE(I3FSTA)
     D I3TSTR_                             LIKE(I3TSTR)
     D I3TSTA_                             LIKE(I3TSTA)
     D P1STAT_                             LIKE(P1STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMQPRTSH
      * Prototype for 'QMHSNDPM'
     D QMHSNDPM        PR                  EXTPGM('QMHSNDPM')
     D P#MIDC_                             LIKE(P#MIDC)
     D P#MSGQ_                             LIKE(P#MSGQ)
     D R3MSGD_                             LIKE(MSGD)
     D P#MDL#_                             LIKE(P#MDL#)
     D P#MSTC_                             LIKE(P#MSTC)
     D P#PMQC_                             LIKE(P#PMQC)
     D P#PSC#_                             LIKE(P#PSC#)
     D P#MSKC_                             LIKE(P#MSKC)
     D P#ERR_                              LIKE(P#ERR)
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
      *          Parameter list for OMH029 (Get system info)
      * Prototype for 'QUSRSPLA'
     D QUSRSPLA        PR                  EXTPGM('QUSRSPLA')
     D D#RCV_                              LIKEDS(D#RCV)
     D P#LEN#_                             LIKE(P#LEN#)
     D P#FMTC_                             LIKE(P#FMTC)
     D P#JOBN_                             LIKE(P#JOBN)
     D P#IJOB_                             LIKE(P#IJOB)
     D P#ISPL_                             LIKE(P#ISPL)
     D P#SPLN_                             LIKE(P#SPLN)
     D P#STP#_                             LIKE(P#STP#)
     D P#ERR_                              LIKE(P#ERR)
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
      *          Parameter list for OMH031
      * Prototype for 'OMX961'
     D OMX961          PR                  EXTPGM('OMX961')
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

      *
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D k#seq#          S              5P 0
     D w#oql#          S              5P 0
     D FILE            S             57
     D FOL001          S             15
     D APPC            S              5
     D PDLC            S              5
     D ERR#            S             10
     D FIX#            S             10
     D MBRC            S             10
     D OBJA            S             10
     D I#ABL1          S                   LIKE(INDI)
     D I#BJL8          S                   LIKE(INDI)
     D I#COVG          S                   LIKE(INDI)
     D I#ODL1          S                   LIKE(INDI)
     D I#OVL1          S                   LIKE(INDI)
     D I#PRRE          S                   LIKE(INDI)
     D I#PSSR          S              1
     D I#RDL1          S                   LIKE(INDI)
     D I#RRL2          S                   LIKE(INDI)
     D I#RRL3          S                   LIKE(INDI)
     D I#RRL4          S                   LIKE(INDI)
     D I#RSL1          S                   LIKE(INDI)
     D I#RST           S                   LIKE(INDI)
     D I#SDL1          S                   LIKE(INDI)
     D I#SLCT          S                   LIKE(INDI)
     D I#TOLH          S                   LIKE(INDI)
     D I#TOVG          S                   LIKE(INDI)
     D I#X002          S                   LIKE(INDI)
     D I#X965          S                   LIKE(INDI)
     D ID              S              3  0
     D IR              S              3  0
     D IX              S              3  0
     D K#APPC          S                   LIKE(APPC)
     D K#IRTP          S                   LIKE(IRTP)
     D LEA001          S             15
     D MS              S              5  0
     D P#APPC          S                   LIKE(APPC)
     D P#AVGT          S              7  2
     D P#CTN           S                   LIKE(CTN#)
     D P#DUM6          S              1
     D P#DUM7          S             10
     D P#DUM8          S             10
     D P#EPLC          S                   LIKE(PDLC)
     D P#ERR#          S                   LIKE(ERR#)
     D P#FIX#          S                   LIKE(FIX#)
     D P#GRCL          S                   LIKE(USID)
     D P#GRID          S                   LIKE(USID)
     D P#HEDC          S                   LIKE(HEDC)
     D P#HPLC          S                   LIKE(PDLC)
     D P#IJOB          S             16
     D P#INC           S                   LIKE(INC#)
     D P#ISPL          S             16
     D P#JOBN          S             36
     D P#MSQL          S             20
     D P#MSTA          S             40
     D P#OPTI          S                   LIKE(INDI)
     D P#SPLN          S             10
     D P#STAT          S                   LIKE(STAT)
     D P#STSI          S                   LIKE(NCSTAT)
     D P#SUL#          S                   LIKE(SUL#)
     D P1STAT          S                   LIKE(STAT)
     D S#INC#          S                   LIKE(INC#)
     D W#DATE          S              7  0
     D W#DD#           S              7  0
     D W#DDIF          S              7  0
     D W#DTMI          S                   LIKE(INDI)
     D W#EOFI          S                   LIKE(INDI)
     D W#ERR           S              3  0
     D W#FILI          S                   LIKE(INDI)
     D W#FIX#          S              5  0
     D W#FLD1          S              4
     D W#FLD2          S              4
     D W#FLD3          S              4
     D W#HDR           S              3  0
     D W#HEDC          S                   LIKE(HEDC)
     D W#HISI          S                   LIKE(INDI)
     D W#HRS           S              7  0
     D W#INCI          S                   LIKE(INDI)
     D W#LEAP          S              5  0
     D W#LINE          S              3  0
     D W#LNFD          S              3  0
     D W#LNRD          S              3  0
     D W#LNSD          S              3  0
     D W#MIN           S              7  2
     D W#MVR           S              3  0
     D W#N01           S              2  0
     D W#N11           S              2  0
     D W#N21           S              2  0
     D W#PRTI          S                   LIKE(INDI)
     D W#QAEI          S                   LIKE(INDI)
     D W#REQ#          S              5  0
     D W#SOL#          S              5  0
     D W#SOLT          S                   LIKE(INDI)
     D W#TEL           S              7  0
     D W#TEL#          S              7  0
     D W#VALI          S                   LIKE(INDI)
     D X               S              3  0
     D Y               S              3  0
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
     C     *ENTRY        PLIST
     C                   PARM                    P#INTL            8
     C                   PARM                    P#ACTC            2
     C                   PARM                    P#BUF           252
     C                   PARM                    P#PRC            40
     C                   PARM                    P#ERD           136
     C                   PARM                    P#FLD           130
      *
      *          ------------------------------------------
      *          Skip processing when D#PARM 0
      *          ------------------------------------------
     C                   IF        D#PARM >= C#PAR#                             B01
      *
      *          ------------------------------------------
      *          Do the init routine
      *          ------------------------------------------
     C                   EXSR      SRINIT
      *
      *          ------------------------------------------
      *          Write first header
      *          ------------------------------------------
     C                   WRITE     PAGEH
     C                   WRITE     COMM
     C                   WRITE     INCHR
      *
      *          ------------------------------------------
      *          Define the way of reading
      *          ------------------------------------------
     C                   MOVEL     *BLANKS       W#FILI
     C                   MOVEL     *LOVAL        NCHEDC
     C                   MOVEL     *LOVAL        NCINC#
     C                   IF        I3HEDC <> C#ALL                              B02
     C                   MOVEL     I3HEDC        NCHEDC
     C                   EVAL      W#FILI = '1'
     C                   ENDIF                                                  E02
     C                   IF        I3INC <> C#ALL                               B02
     C                   MOVEL     I3INC         NCINC#
     C                   IF        I3INC = '1'                                  B03
     C                   EVAL      W#FILI = '2'
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C     $NC1K2        SETLL     OMINCR
      *
      *          ------------------------------------------
      *          Read the incidents till the end
      *          ------------------------------------------
     C                   MOVE      *BLANKS       W#EOFI
     C                   EVAL      W#INCI = *OFF
     C                   DOW       W#EOFI <> 'E'                                B02
     C                   SELECT                                                 B03
     C                   WHEN      W#FILI = ' '                                 X03
     C                   READ      OMINCR
     C                   EVAL      *IN99 = %EOF
     C                   WHEN      W#FILI = '1'                                 X03
     C     $NC1K1        READE     OMINCR
     C                   EVAL      *IN99 = %EOF
     C                   WHEN      W#FILI = '2'                                 X03
     C     $NC1K2        READE     OMINCR
     C                   EVAL      *IN99 = %EOF
     C                   ENDSL                                                  E03
     C                   IF        *IN99 = *ON                                  B03
     C                   EVAL      W#EOFI = 'E'
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Check authority
      *          ------------------------------------------
     C                   IF        NCHEDC <> W#HEDC                             B04
     C                   EXSR      SRSUL#
     C                   MOVEL     NCHEDC        W#HEDC
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          process per incident
      *          ------------------------------------------
     C                   EXSR      SRVWIN
     C                   ENDIF                                                  E03
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          No matching incidents
      *          ------------------------------------------
     C                   IF        W#INCI = *OFF                                B02
     C                   WRITE     INCND
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Print total page when necessary
      *          ------------------------------------------
     C                   IF        I3PEP = *ON                                  B02
     C                   EXSR      SRENDP
     C                   ENDIF                                                  E02
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do the total last record routine
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRVWIN - Process incidents                                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRVWIN        BEGSR
      *
      *          ------------------------------------------
      *          Validate, process when valid
      *          ------------------------------------------
     C                   EXSR      SRVALD
     C                   IF        W#VALI = *ON                                 B01
     C                   EVAL      W#INCI = *ON
      *
      *          ------------------------------------------
      *          Overflow?
      *          ------------------------------------------
     C                   IF        I3LOD = *ON                                  B02
     C     D#OFLW        SUB       D#REG#        D#HLP#
     C                   IF        D#HLP# <= 12                                 B03
     C                   EVAL      *IN01 = *ON
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
     C                   IF        *IN01 = *ON                                  B02
     C                   WRITE     PAGEH
     C                   WRITE     INCHR
     C                   EVAL      *IN01 = *OFF
     C                   EVAL      W#HISI = *OFF
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Header again?
      *          ------------------------------------------
     C                   IF        W#HISI = *ON                                 B02
     C                   WRITE     INCHR
     C                   EVAL      W#HISI = *OFF
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Print incident
      *          ------------------------------------------
     C                   EXSR      SRFPRT
     C                   IF        P1STAT <> C#NOTF
      *
     C                   IF        I3LOD = *OFF                                 B02
     C                   WRITE     INCDR
     C                   ELSE                                                   X02
     C                   WRITE     INCDD
     C                   ENDIF                                                  E02
     C                   ADD       1             FMPRTI
      *
      *          ------------------------------------------
      *          Print text bloc, when needed
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      I3PIT <> '0'
     C                   EXSR      SRTEXT
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Print answer and question history, when needed
      *          ------------------------------------------
     C                   EVAL      W#HISI = *OFF
     C                   SELECT
     C                   WHEN      I3QAH = '1'
     C                   EXSR      SRPRQA
     C                   ENDSL
      *
      *
      *          ------------------------------------------
      *          Print related incidents, when needed
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      I3PIR = '1'
     C                   EXSR      SRPRIN
     C                   ENDSL
      *
     C                   ENDIF                                                  E01
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRVALD - Validate                                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRVALD        BEGSR
      *
      *          ------------------------------------------
      *          Assume valid, check each field subsequently
      *          ------------------------------------------
     C                   EVAL      W#VALI = *ON
     C                   EVAL      D#DAY# = *DAY
     C                   EVAL      D#MON# = *MONTH
     C                   Z-ADD     *YEAR         D#YEA#
      *
      *          ------------------------------------------
      *          HelpDesk code
      *          ------------------------------------------
     C                   IF        I3HEDC <> C#ALL                              B01
     C                             AND I3HEDC <> NCHEDC
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Incident number
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3INC <> C#ALL
     C                             AND I3INC <> C#CMP
     C                             AND I3INC <> C#CMPT
     C                             AND I3INC <> C#NOMS
     C                             AND I3INC <> C#OPN
     C                             AND I3INC <> NCINC#
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3INC = C#OPN
     C                             AND NCSTAT = C#CMP
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3INC = C#NOMS
     C                             AND NCSTAT = C#CMP
     C                             OR I3INC = C#NOMS
     C                             AND NCSTAT = C#OMS
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3INC = C#CMP
     C                             AND NCSTAT <> C#CMP
     C                             OR W#VALI = *ON                              B01
     C                             AND I3INC = C#CMPT
     C                             AND NCSTAT <> C#CMP
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Call track number
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3CTN <> C#ALL
     C                             AND I3CTN <> NCCTN#
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Customer
      *          ------------------------------------------
     C     $CA1K1        CHAIN(N)  OMCCAR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3CUSC <> C#ALL
     C                   IF        *IN99 = *ON                                  B02
     C                             OR I3CUSC <> CACUSC
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Contact
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3CCTC <> C#ALL
     C     $CO1K1        CHAIN(N)  OMCCOR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                             OR I3CCTC <> CACCTC
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Incoming date
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                   IF        I3IDTF > NCINDT                              B02
     C                             OR I3IDTT < NCINDT
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Incoming time
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                   IF        I3ITMF > NCINTM                              B02
     C                             OR I3ITMT < NCINTM
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Completion date
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                   IF        I3CDTF > NCRCDT                              B02
     C                             OR I3CDTT < NCRCDT
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Status route code
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3HST = '1'
     C                             AND I3STRC <> C#ALL
     C                             AND I3STRC <> NCSTRC
     C                   CALLP     OMQPRTSH ( NCHEDC : NCINC# : I3STRC :
     C                             I3STAT : P1STAT )
     C                   IF        P1STAT = C#NOTF
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3STRC <> C#ALL
     C                             AND I3STRC <> NCSTRC
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Support level
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3SULC <> '*'
     C                   MOVEL     I3SULC        D#SUA#
     C                   TESTN                   D#SUA#               98
     C                   IF        *IN98 = *ON                                  B02
     C                             AND D#SUN# <> NCSUL#
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Status
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3STAT <> C#ALL
     C                             AND I3STAT <> NCSTAT
     C                             AND I3HST = '0'
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Severity one
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3SEV1 <> C#ALL
     C                             AND I3SEV1 <> NCSEV1
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Severity two
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3SEV2 <> C#ALL
     C                             AND I3SEV2 <> NCSEV2
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Severity three
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3SEV3 <> C#ALL
     C                             AND I3SEV3 <> NCSEV3
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Priority (numeric, length 3)
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                   IF        I3PRIF > NCPRI#                              B02
     C                             OR I3PRIT < NCPRI#
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Registered by
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3RUID <> C#ALL
     C                             AND I3RUID <> NCRUID
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Allocated by
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3AUID <> C#ALL
     C                             AND I3AUID <> NCAUID
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Incident type
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3ITPC <> C#ALL
     C                             AND I3ITPC <> NCITPC
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Answer list
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3ALSC <> C#NONE
     C                   CALLP     OMH031 ( NCHEDC : NCINC# : I3ALSC :
     C                             P1STAT )
     C                   IF        P1STAT <> C#NORM                             B02
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          User field 1
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3USF1 <> C#ALL
     C                             AND I3USF1 <> NCUSF1
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          User field 2
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3USF2 <> C#ALL
     C                             AND I3USF2 <> NCUSF2
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          User field 3
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                             AND I3USF3 <> -1
     C                             AND I3USF3 <> NCUSF3                         B02
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Escalation counter
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                   SELECT                                                 B02
     C                   WHEN      I3ESCD = C#LT                                X02
     C                   IF        I3ESC# <= NCESC#                             B03
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E03
     C                   WHEN      I3ESCD = C#LE                                X02
     C                   IF        I3ESC# < NCESC#                              B03
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E03
     C                   WHEN      I3ESCD = C#GT                                X02
     C                   IF        I3ESC# >= NCESC#                             B03
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E03
     C                   WHEN      I3ESCD = C#GE                                X02
     C                   IF        I3ESC# > NCESC#                              B03
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E03
     C                   WHEN      I3ESCD = C#EQ                                X02
     C                   IF        I3ESC# <> NCESC#                             B03
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E03
     C                   ENDSL                                                  E02
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Escalated since
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
     C                   IF        NCIEDT < I3ESDT                              B02
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR                                                  UNDERFLW
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRFPRT   Fill printerfile                                   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRFPRT        BEGSR
      *
      *          ------------------------------------------
      *          Retrieve
      *          ------------------------------------------
     C                   CLEAR                   PRCUSC
     C     $CA1K1        CHAIN     OMCCAR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B01
     C                   MOVEL     CACUSC        PRCUSC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill
      *          ------------------------------------------
     C                   MOVEL     NCHEDC        PRHEDC
     C                   MOVEL     NCINC#        PRINC#
     C                   MOVEL     NCAIN#        PRAIN#
     C                   MOVEL     NCCTN#        PRCTN#
     C                   Z-ADD     NCINDT        D#DAT#
     C                   Z-ADD     D#YEA#        PRYEA#
     C                   Z-ADD     D#MON#        PRMON#
     C                   Z-ADD     D#DAY#        PRDAY#
     C                   Z-ADD     NCINDT        PRINDT
     C                   Z-ADD     NCINTM        PRINTM
     C                   Z-ADD     NCRCTM        PRRCTM
     C                   Z-ADD     NCRCDT        PRRCDT
     C                   Z-ADD     NCRCDT        D#DAT#
      *****                Z-ADDD#YEA#    PRYEC#
      *****                Z-ADDD#MON#    PRMOC#
      ******               Z-ADDD#DAY#    PRDAC#
     C                   MOVEL     NCSTRC        PRSTRC
     C                   Z-ADD     NCSRTM        PRSRTM
     C                   Z-ADD     NCSRDT        PRSRDT
     C                   MOVE      NCSUL#        PRSUL#
     C                   MOVEL     NCSTAT        PRSTAT
     C                   MOVEL     NCSEV1        PRSEV1
     C                   MOVEL     NCSEV2        PRSEV2
     C                   MOVEL     NCSEV3        PRSEV3
     C                   MOVEL     NCAUID        PRAUID
     C                   MOVEL     NCRUID        PRRUID
     C                   Z-ADD     NCPRI#        PRPRI#
     C                   Z-ADD     NCESC#        PRESC#
     C                   Z-ADD     NCIETM        PRIETM
     C                   Z-ADD     NCIEDT        PRIEDT
     C                   MOVEL     NCITPC        PRITPC
     C                   MOVEL     NCCFIC        PRCFIC
     C                   Z-ADD     NCCFL#        PRCFL#
     C                   MOVEL     NCUSF1        PRUSF1
     C                   MOVEL     NCUSF2        PRUSF2
     C                   Z-ADD     NCUSF3        PRUSF3
      *
     C                   MOVE      *BLANKS       PRSHID
     C                   IF        I3AVE = '0'
     C                   MOVEL     NCSHID        PRSHID
     C                   ELSE
     C                   CALLP     OMQPRTHS ( NCHEDC : NCINC# : P#AVGT :
     C                             NCSTRC : NCSTAT : I3FSTR : I3FSTA :
     C                             I3TSTR : I3TSTA : P1STAT )
      *          ------------------------------------------
      *          If record found
      *          ------------------------------------------
     C                   IF        P1STAT = C#FND
     C                   EVAL      D#AVGA = %EDITC(P#AVGT:'X')
     C                   MOVEA     D#AVGA        AVG
      *
     C                   EVAL      X = 1
     C                   DOW       X <= 4
     C                   IF        AVG(X) = '0'
     C                   EVAL      AVG(X) = *BLANK
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             X
     C                   ENDDO
      *
     C                   EVAL      X = 7
     C                   EVAL      Y = 7
     C                   DOW       X >= 1
     C                             AND Y >= 1
     C                   IF        X <> 5
     C                   EVAL      AV#(Y) = AVG(X)
     C                   ELSE
     C                   EVAL      AV#(Y) = '.'
     C                   SUB       1             Y
     C                   EVAL      AV#(Y) = AVG(X)
     C                   ENDIF
     C                   SUB       1             X
     C                   SUB       1             Y
     C                   ENDDO
      *
     C     ' Hours'      CAT       PRSHID        PRSHID
     C                   MOVEA     AV#           D#AVGA
     C     D#AVGA        CAT       PRSHID        PRSHID
      *
      *          ---------------------------------------------
      *          Add record time to total time and add counter
      *          ---------------------------------------------
     C     P#AVGT        DIV       1             W#HRS
     C                   MVR                     W#MIN
     C                   MULT      100           W#MIN
     C                   MULT      60            W#HRS
     C                   ADD       W#HRS         W#MIN
      *
     C                   ADD       W#MIN         FMPAVG
     C                   ADD       1             W#TEL
      *
     C                   ELSE
     C                   IF        P1STAT = C#NOTF
     C                   MOVEL     C#INV         PRSHID
     C                   ELSE
     C                   MOVEL     C#CAL         PRSHID
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPRQA   Print questions and answers                        *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPRQA        BEGSR
      *
      *          ------------------------------------------
      *          Write Q&A header
      *          ------------------------------------------
     C                   EVAL      W#PRTI = *ON
      *
      *          ------------------------------------------
      *          Retrieve history
      *          ------------------------------------------
     C     $AH1K1        SETLL     OMQAHR
     C                   EVAL      W#QAEI = *OFF
     C                   DOW       W#QAEI = *OFF                                B01
     C     $AH1K1        READE     OMQAHR
     C                   EVAL      *IN99 = %EOF
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      W#QAEI = *ON
     C                   EVAL      W#PRTI = *OFF
     C                   ELSE                                                   X02
     C     $QL1K1        CHAIN     OMHQLR
     C                   EVAL      *IN99 = NOT %FOUND
     C     $QU1K1        CHAIN     OMHQUR
     C                   EVAL      *IN99 = NOT %FOUND
     C     $NS1K1        CHAIN     OMANSR
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill printerfields
      *          ------------------------------------------
     C                   MOVEL     QUQUES        PRQUES
     C                   MOVEL     NSANSC        PRANSC
      *
      *          ------------------------------------------
      *          Write, include headers in case of overflow
      *          ------------------------------------------
     C                   IF        W#OQL# <> AHOQL#                             B03
     C                   MOVEL     QLDESC        PRDESC
     C                   WRITE     QAAHR
     C                   Z-ADD     AHOQL#        W#OQL#
     C                   EVAL      W#PRTI = *OFF
     C                   ENDIF                                                  E03
     C                   IF        W#PRTI = *ON                                 B03
     C                   WRITE     QAAHR
     C                   EVAL      W#PRTI = *OFF
     C                   EVAL      W#HISI = *ON
     C                   ENDIF                                                  E03
     C                   IF        *IN01 = *ON                                  B03
     C                   WRITE     PAGEH
     C                   WRITE     QAAHR
     C                   EVAL      *IN01 = *OFF
     C                   ENDIF                                                  E03
     C                   WRITE     QAADR
      *
     C                   ENDIF                                                  E02
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRTEXT   Print questions and answers                        *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRTEXT        BEGSR
      *
      *          ------------------------------------------
      *          Get this users support level.
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Write  bloc header
      *          ------------------------------------------
     C                   EVAL      IHITH# = 0
     C                   MOVEL     TXT(5)        TYP007
     C                   EVAL      *IN99 = *OFF
     C     $IH1K2        SETLL     OMTIHR
     C     $IH1K1        READE     OMTIHR
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 <> *ON                                 B01
     C                   IF        I3PIT = '9'                                  B02
     C                             OR I3PIT = IHITXT
     C                   MOVEL     TXT(Z1)       PRTYPD
     C                   MOVEL     IHSITD        PRTXTH
     C                   IF        *IN01 = *ON                                  B03
     C                   WRITE     PAGEH
     C                   WRITE     INCHR
     C                   EVAL      *IN01 = *OFF
     C                   EVAL      W#HISI = *OFF
     C                   ENDIF                                                  E03
     C                   WRITE     TXTHR
     C                   EVAL      *IN98 = *OFF
     C                   EVAL      IDSEQ# = 0
      *
      *          ------------------------------------------
      *          Write details if authorized
      *          ------------------------------------------
     C                   IF        D7SUL# >= IHSUL#                             B03
      *
     C     $ID1K2        SETLL     OMTIDR
     C     $ID1K1        READE     OMTIDR
     C                   EVAL      *IN98 = %EOF
     C                   DOW       *IN98 <> *ON                                 B04
     C                   MOVEL     IDINCD        PRTXTD
      *
      *          ------------------------------------------
      *          Handle overflow, *IN50 PRTF indicator text detail
      *          ------------------------------------------
     C                   IF        *IN01 = *ON                                  B05
     C                   WRITE     PAGEH
     C                   WRITE     INCHR
     C                   EVAL      *IN01 = *OFF
     C                   EVAL      W#HISI = *OFF
     C                   EVAL      *IN50 = *ON
     C                   ENDIF                                                  E05
     C                   WRITE     TXTDR
     C                   EVAL      *IN50 = *OFF
     C     $ID1K1        READE     OMTIDR
     C                   EVAL      *IN98 = %EOF
     C                   ENDDO                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C     $IH1K1        READE     OMTIHR
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPRIN   Print related incident                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPRIN        BEGSR
      *
      *          ------------------------------------------
      *          Process when possible
      *          ------------------------------------------
     C                   IF        NCAIN# <> *BLANKS                            B01
     C                   EVAL      *IN52 = *ON
     C                   MOVEL     NCINC#        S#INC#
     C                   MOVEL     NCAIN#        NCINC#
     C                   MOVEL     NCAIN#        PRAIN#
     C     $NC1K2        CHAIN     OMINCR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Overflow?
      *          ------------------------------------------
     C                   IF        I3LOD = *ON                                  B03
     C     D#OFLW        SUB       D#REG#        D#HLP#
     C                   IF        D#HLP# <= 12                                 B04
     C                   EVAL      *IN01 = *ON
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
     C                   IF        *IN01 = *ON                                  B03
     C                   WRITE     PAGEH
     C                   EVAL      *IN01 = *OFF
     C                   ENDIF                                                  E03
     C                   WRITE     RINHR
     C                   EVAL      W#HISI = *ON
     C                   EXSR      SRFPRT
     C                   IF        *IN01 = *ON                                  B03
     C                   WRITE     PAGEH
     C                   EVAL      *IN01 = *OFF
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Print incident
      *          ------------------------------------------
     C                   IF        I3LOD = *OFF                                 B03
     C                   WRITE     INCDR
     C                   ELSE                                                   X03
     C                   WRITE     INCDD
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Print text bloc, when needed
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      I3PIT <> '0'
     C                   EXSR      SRTEXT
     C                   ENDSL
      *
     C                   IF        *IN01 = *ON                                  B03
     C                   WRITE     PAGEH
     C                   EVAL      *IN01 = *OFF
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Reposition file
      *          ------------------------------------------
     C                   MOVEL     S#INC#        NCINC#
     C     $NC1K2        CHAIN     OMINCR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   EVAL      *IN52 = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRINIT - Program initialization                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRINIT        BEGSR
      *
      *          ------------------------------------------
      *          Fill buffer data structures
      *          ------------------------------------------
     C                   MOVEL     P#BUF         I3BUF
     C                   MOVEL     P#PRC         I3PRC
     C                   MOVEL     P#FLD         I3FLD
     C                   MOVE      *BLANKS       I3ERR
      *
      *          ------------------------------------------
      *          Redefine
      *          ------------------------------------------
     C                   MOVEL     I3ESCD        D#ESCD
      *
      *          ------------------------------------------
      *          Initialize
      *          ------------------------------------------
     C                   EVAL      P#INTL = *BLANKS
     C                   EVAL      P#BUF = *BLANKS
     C                   EVAL      P#PRC = *BLANKS
     C                   EVAL      P#ERD = *BLANKS
     C                   EVAL      P#FLD = *BLANKS
     C                   EVAL      W#HISI = *OFF
      *
      *          ------------------------------------------
      *          Define printing of subsets; determine current date
      *          ------------------------------------------
     C     *YEAR         MULT      10000         W#DATE
     C                   EVAL      W#TEL# = *MONTH * 100
     C                   IF        *MONTH < 40                                  B01
     C                   ADD       1000000       W#DATE
     C                   ENDIF                                                  E01
     C                   ADD       W#TEL#        W#DATE
     C                   ADD       *DAY          W#DATE
      *
      *          ------------------------------------------
      *          Clear indicators for printerfile
      *          ------------------------------------------
     C                   EVAL      *IN(21) = *OFF
     C                   EVAL      *IN(22) = *OFF
     C                   EVAL      *IN(23) = *OFF
     C                   EVAL      *IN(24) = *OFF
     C                   EVAL      *IN(25) = *OFF
     C                   EVAL      *IN(26) = *OFF
     C                   EVAL      *IN(27) = *OFF
     C                   EVAL      *IN(28) = *OFF
     C                   EVAL      *IN(29) = *OFF
     C                   EVAL      *IN(30) = *OFF
     C                   EVAL      *IN(31) = *OFF
     C                   EVAL      *IN(32) = *OFF
     C                   EVAL      *IN(33) = *OFF
     C                   EVAL      *IN(34) = *OFF
     C                   EVAL      *IN(35) = *OFF
     C                   EVAL      *IN(36) = *OFF
     C                   EVAL      *IN(37) = *OFF
     C                   EVAL      *IN(38) = *OFF
     C                   EVAL      *IN(39) = *OFF
     C                   EVAL      *IN(40) = *OFF
     C                   EVAL      *IN(41) = *OFF
     C                   EVAL      *IN(42) = *OFF
     C                   EVAL      *IN(43) = *OFF
     C                   EVAL      *IN(44) = *OFF
      *
      *          ------------------------------------------
      *          Action if not default
      *          ------------------------------------------
     C                   IF        I3HEDC <> C#ALL                              B01
     C                   EVAL      *IN21 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3HEDC        PPHEDC
     C                   ENDIF                                                  E01
     C                   IF        I3INC <> C#OPN                               B01
     C                   EVAL      *IN22 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3INC         PPINC
     C                   ENDIF                                                  E01
     C                   IF        I3CTN <> C#ALL                               B01
     C                   EVAL      *IN23 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3CTN         PPCTN
     C                   ENDIF                                                  E01
     C                   IF        I3CUSC <> C#ALL                              B01
     C                   EVAL      *IN24 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3CUSC        PPCUSC
     C                   ENDIF                                                  E01
     C                   IF        I3CCTC <> C#ALL                              B01
     C                   EVAL      *IN25 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3CCTC        PPCCTC
     C                   ENDIF                                                  E01
     C                   IF        I3HST <> '0'                                 B01
     C                   EVAL      *IN29 = *ON
     C                   EVAL      *IN31 = *ON
     C                   MOVEL     I3STRC        PPSTRC
     C                   MOVEL     I3STAT        PPSTAT
     C                   EVAL      *IN44 = *ON
     C                   EVAL      *IN46 = *ON
     C                   MOVEL     C#YES         PPHIST
     C                   ENDIF                                                  E01
     C                   IF        I3AVE <> '0'                                 B01
     C                   EVAL      *IN29 = *ON
     C                   EVAL      *IN31 = *ON
     C                   MOVEL     I3STRC        PPSTRC
     C                   MOVEL     I3STAT        PPSTAT
     C                   EVAL      *IN45 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     '*YES'        PPPAVE
     C                   SELECT
     C                   WHEN      I3FSTR = '0'
     C                   MOVEL     C#FRST        PPFSTR
     C                   WHEN      I3FSTR = '1'
     C                   MOVEL     C#ALL         PPFSTR
     C                   OTHER
     C                   MOVEL     I3FSTR        PPFSTR
     C                   ENDSL
     C                   IF        I3FSTA = '0'
     C                   MOVEL     C#FRST        PPFSTA
     C                   ELSE
     C                   MOVEL     I3FSTA        PPFSTA
     C                   ENDIF
     C                   SELECT
     C                   WHEN      I3TSTR = '0'
     C                   MOVEL     C#NONE        PPTSTR
     C                   WHEN      I3TSTR = '1'
     C                   MOVEL     C#LAST        PPTSTR
     C                   OTHER
     C                   MOVEL     I3TSTR        PPTSTR
     C                   ENDSL
     C                   SELECT
     C                   WHEN      I3TSTA = '0'
     C                   MOVEL     C#NONE        PPTSTA
     C                   WHEN      I3TSTA = '1'
     C                   MOVEL     C#CMPT        PPTSTA
     C                   OTHER
     C                   MOVEL     I3TSTA        PPTSTA
     C                   ENDSL
     C                   ENDIF                                                  E01
     C                   IF        I3IDTF <> 0                                  B01
     C                             OR I3IDTT <> W#DATE
     C                   EVAL      *IN26 = *ON
     C                   EVAL      *IN44 = *ON
     C                   Z-ADD     I3IDTF        PPIDTF
     C                   Z-ADD     I3IDTT        PPIDTT
     C                   ENDIF                                                  E01
     C                   IF        I3ITMF <> 0                                  B01
     C                             OR I3ITMT <> 235959
     C                   EVAL      *IN27 = *ON
     C                   EVAL      *IN44 = *ON
     C                   Z-ADD     I3ITMF        PPITMF
     C                   Z-ADD     I3ITMT        PPITMT
     C                   ENDIF                                                  E01
     C                   IF        I3CDTF <> 0                                  B01
     C                             OR I3CDTT <> W#DATE
     C                   EVAL      *IN28 = *ON
     C                   EVAL      *IN44 = *ON
     C                   Z-ADD     I3CDTF        PPCDTF
     C                   Z-ADD     I3CDTT        PPCDTT
     C                   ENDIF                                                  E01
     C                   IF        I3STRC <> C#ALL                              B01
     C                   EVAL      *IN29 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3STRC        PPSTRC
     C                   ENDIF                                                  E01
     C                   IF        I3SULC <> '*'                                B01
     C                   EVAL      *IN30 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3SULC        PPSULC
     C                   ENDIF                                                  E01
     C                   IF        I3STAT <> C#ALL                              B01
     C                   EVAL      *IN31 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3STAT        PPSTAT
     C                   ENDIF                                                  E01
     C                   IF        I3SEV1 <> C#ALL                              B01
     C                   EVAL      *IN32 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3SEV1        PPSEV1
     C                   ENDIF                                                  E01
     C                   IF        I3SEV2 <> C#ALL                              B01
     C                   EVAL      *IN33 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3CTN         PPSEV2
     C                   ENDIF                                                  E01
     C                   IF        I3SEV3 <> C#ALL                              B01
     C                   EVAL      *IN34 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3SEV3        PPSEV3
     C                   ENDIF                                                  E01
     C                   IF        I3PRIF <> 0                                  B01
     C                             OR I3PRIT <> 999
     C                   EVAL      *IN35 = *ON
     C                   EVAL      *IN44 = *ON
     C                   Z-ADD     I3PRIF        PPPRIF
     C                   Z-ADD     I3PRIT        PPPRIT
     C                   ENDIF                                                  E01
     C                   IF        I3RUID <> C#ALL                              B01
     C                   EVAL      *IN36 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3RUID        PPRUID
     C                   ENDIF                                                  E01
     C                   IF        I3AUID <> C#ALL                              B01
     C                   EVAL      *IN37 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3AUID        PPAUID
     C                   ENDIF                                                  E01
     C                   IF        I3ITPC <> C#ALL                              B01
     C                   EVAL      *IN38 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3ITPC        PPITYP
     C                   ENDIF                                                  E01
     C                   IF        I3USF1 <> C#ALL                              B01
     C                   EVAL      *IN39 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3USF1        PPUSF1
     C                   ENDIF                                                  E01
     C                   IF        I3USF2 <> C#ALL                              B01
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3USF2        PPUSF2
     C                   ENDIF                                                  E01
     C                   IF        I3USF3 <> -1                                 B01
     C                   EVAL      *IN41 = *ON
     C                   EVAL      *IN44 = *ON
     C                   Z-ADD     I3USF3        PPUSF3
     C                   ENDIF                                                  E01
     C                   IF        I3ESCD <> C#GE                               B01
     C                             OR I3ESC# <> 0
     C                   EVAL      *IN42 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3ESCD        PPESCD
     C                   Z-ADD     I3ESC#        PPESC#
     C                   ENDIF                                                  E01
     C                   IF        I3ESDT <> 000000                             B01
     C                   EVAL      *IN43 = *ON
     C                   EVAL      *IN44 = *ON
     C                   MOVEL     I3ESDT        PPESDT
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Reset error parameter for API calls
      *          ------------------------------------------
     C                   RESET                   P#ERR
      *
      *          ------------------------------------------
      *          Init the error count and error array
      *          ------------------------------------------
     C                   EVAL      W#ERR = 0
     C                   EVAL      ERR = *BLANKS
     C                   EVAL      IR = 0
      *
      *          ------------------------------------------
      *          Check and set the subset parameters
      *          Use cover page to store fields
      *          ------------------------------------------
     C                   EVAL      I#SLCT = *OFF
     C                   EVAL      *IN(61) = *OFF
     C                   EVAL      *IN(62) = *OFF
     C                   EVAL      *IN(63) = *OFF
     C                   EVAL      *IN(64) = *OFF
     C                   EVAL      *IN(65) = *OFF
     C                   EVAL      *IN(66) = *OFF
     C                   EVAL      *IN(67) = *OFF
     C                   EVAL      *IN(68) = *OFF
     C                   EVAL      *IN(69) = *OFF
     C                   EVAL      *IN(70) = *OFF
     C                   EVAL      *IN(71) = *OFF
     C                   EVAL      *IN(72) = *OFF
     C                   EVAL      *IN(73) = *OFF
     C                   EVAL      *IN(74) = *OFF
     C                   EVAL      *IN(75) = *OFF
     C                   EVAL      *IN(76) = *OFF
     C                   EVAL      *IN(77) = *OFF
     C                   EVAL      *IN(78) = *OFF
     C                   MOVE      *LOVAL        D#LOWC
     C                   MOVE      *ZERO         D#LOWN
     C                   MOVE      *HIVAL        D#HIGC
     C                   MOVE      *ALL'9'       D#HIGN
      *
      *
      *          ------------------------------------------
      *          Check if general text is available
      *          ------------------------------------------
     C                   IF        TRE001 = *BLANK                              B01
     C                   FOR       MS = 1 TO 2                                  B02
     C                   IF        GEN(MS) <> *BLANKS                           B03
     C                   EVAL      D#MDTA = *BLANK
     C                   MOVE      GEN(MS)       P#MIDC
     C                   CALLP     QMHRTVM ( D#RTVM : P#MDL# : P#FMTC :
     C                             P#MIDC : P#MSQL : P#DUM6 : P#LOB# :
     C                             P#DUM7 : P#DUM8 : P#ERR )
     C                   EVAL      VAR(MS) = D#MDTA
     C                   ENDIF                                                  E03
     C                   ENDFOR                                                 E02
     C                   MOVEL     VAR(1)        TRE001
     C                   MOVEL     VAR(2)        TRE007
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the incident text header
      *          ------------------------------------------
     C                   IF        TRE011 = *BLANK                              B01
     C                   IF        I3LOD = '0'                                  B02
     C                   IF        I3AVE = '0'                                  B02
     C                   EVAL      MS = 1
     C                   ELSE                                                   X02
     C                   EVAL      MS = 3
     C                   ENDIF
     C                   ELSE                                                   X02
     C                   EVAL      MS = 2
     C                   ENDIF                                                  E02
     C                   IF        ISL(MS) <> *BLANKS                           B02
     C                   EVAL      D#MDTA = *BLANK
     C                   MOVE      ISL(MS)       P#MIDC
     C                   CALLP     QMHRTVM ( D#RTVM : P#MDL# : P#FMTC :
     C                             P#MIDC : P#MSQL : P#DUM6 : P#LOB# :
     C                             P#DUM7 : P#DUM8 : P#ERR )
     C                   EVAL      VAR(MS) = D#MDTA
     C                   ENDIF                                                  E02
     C                   MOVEL     VAR(MS)       TRE011
     C                   ENDIF                                                  E01
      *
      *
      *
      *          ------------------------------------------
      *          Retrieve cover messages
      *          ------------------------------------------
     C                   IF        TRE106 = *BLANK                              B01
     C                   FOR       MS = 1 TO 8                                  B02
     C                   IF        END(MS) <> *BLANKS                           B03
     C                   EVAL      D#MDTA = *BLANK
     C                   MOVE      END(MS)       P#MIDC
     C                   CALLP     QMHRTVM ( D#RTVM : P#MDL# : P#FMTC :
     C                             P#MIDC : P#MSQL : P#DUM6 : P#LOB# :
     C                             P#DUM7 : P#DUM8 : P#ERR )
     C                   EVAL      VAR(MS) = D#MDTA
     C                   ENDIF                                                  E03
     C                   ENDFOR                                                 E02
     C                   MOVEL     VAR(1)        TRE106
     C                   MOVEL     VAR(2)        TRE107
     C                   MOVEL     VAR(3)        TRE108
     C                   MOVEL     VAR(4)        TRE109
     C                   MOVEL     VAR(5)        TRE110
     C                   MOVEL     VAR(6)        END001
     C                   MOVEL     VAR(7)        TRE111
     C                   MOVEL     VAR(8)        TRE112
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Open the printfile when not open
      *          ------------------------------------------
     C                   IF        I#PRRE <> *ON                                B01
     C                   EVAL      I#PRRE = *ON
     C                   EVAL      W#LINE = 900
      *
      *          ------------------------------------------
      *          Retrieve the overflow line number
      *          ------------------------------------------
     C                   MOVE      'SPLA0100'    P#FMTC
     C                   EVAL      P#JOBN = '*'
     C                   EVAL      P#SPLN = 'OMQPRTIN'
     C                   Z-ADD     440           P#LEN#
     C                   Z-ADD     -1            P#STP#
     C                   CALLP     QUSRSPLA ( D#RCV : P#LEN# : P#FMTC :
     C                             P#JOBN : P#IJOB : P#ISPL : P#SPLN :
     C                             P#STP# : P#ERR )
      *
      *          ------------------------------------------
      *          Use default when not succesfull
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZERO                              B02
     C                   EVAL      D#OFLW = 60
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C     INIT99        TAG
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSUL#  -  Get Authorisation for helpdesk                    *
      *                                                              *
      * Input   -  P#HEDC - Help desk code   to check                *
      *            P#USID - Employee                                 *
      *                                                              *
      * Output  -  P#SUL# - Assigned support level.                  *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSUL#        BEGSR
      *
      *          ------------------------------------------
      *          Retrieve authorisation
      *          ------------------------------------------
     C                   IF        NCHEDC <> D8HEDC                             B01
     C                   EVAL      P#OPTI = '3'
     C                   MOVE      NCHEDC        P#HEDC
     C                   EVAL      P#SUL# = '0'
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD )
     C                   ENDIF                                                  E01
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
     C                   CLEAR                   W#OQL#
     C                   EVAL      X = 0
     C                   EVAL      Y = 0
     C                   EVAL      IX = 0
     C                   EVAL      ID = 0
     C                   EVAL      W#REQ# = 0
     C                   EVAL      W#FIX# = 0
     C                   EVAL      W#SOL# = 0
     C                   EVAL      W#HDR = 0
     C                   EVAL      W#DD# = 0
     C                   EVAL      W#TEL = 0
     C                   EVAL      W#HRS = 0
     C                   EVAL      W#MIN = 0
     C                   EVAL      W#DDIF = 0
     C                   EVAL      W#DATE = 0
     C                   EVAL      W#TEL# = 0
     C                   EVAL      W#LEAP = 0
     C                   EVAL      W#MVR = 0
     C                   EVAL      W#LINE = 0
     C                   EVAL      W#LNRD = 0
     C                   EVAL      W#LNFD = 0
     C                   EVAL      W#LNSD = 0
     C                   EVAL      W#N01 = 0
     C                   EVAL      W#N11 = 0
     C                   EVAL      W#N21 = 0
     C                   EVAL      W#FLD1 = *BLANK
     C                   EVAL      W#FLD2 = *BLANK
     C                   EVAL      W#FLD3 = *BLANK
      *
      *          ------------------------------------------
      *          IBM API field definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
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
      *          Parameter list for OMH029 (Get system info)
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
      *          Parameter list for OMH031
      *          ------------------------------------------
     C     $MH031        PLIST
     C                   PARM                    NCHEDC
     C                   PARM                    NCINC#
     C                   PARM                    I3ALSC
     C                   PARM                    P1STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMQPRTHS
      *          ------------------------------------------
     C     $PRTHS        PLIST
     C                   PARM                    NCHEDC
     C                   PARM                    NCINC#
     C                   PARM                    P#AVGT
     C                   PARM                    NCSTRC
     C                   PARM                    NCSTAT
     C                   PARM                    I3FSTR
     C                   PARM                    I3FSTA
     C                   PARM                    I3TSTR
     C                   PARM                    I3TSTA
     C                   PARM                    P1STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMQPRTSH
      *          ------------------------------------------
     C     $PRTSH        PLIST
     C                   PARM                    NCHEDC
     C                   PARM                    NCINC#
     C                   PARM                    I3STRC
     C                   PARM                    I3STAT
     C                   PARM                    P1STAT
      *
      *          ------------------------------------------
      *          OMCCAL1 key list customer call
      *          ------------------------------------------
     C     $CA1K1        KLIST
     C                   KFLD                    NCHEDC
     C                   KFLD                    NCCTN#
      *
      *          ------------------------------------------
      *          OMCCOL1 key list contacts
      *          ------------------------------------------
     C     $CO1K1        KLIST
     C                   KFLD                    CACUSC
     C                   KFLD                    CACCTC
      *
      *          ------------------------------------------
      *          OMRIRL1 key list incident/request file
      *          ------------------------------------------
     C     $IR1K1        KLIST
     C                   KFLD                    NCHEDC
     C                   KFLD                    NCINC#
     C                   KFLD                    K#IRTP
      *
      *          ------------------------------------------
      *          OMHQL   Question list file
      *          ------------------------------------------
     C     $QL1K1        KLIST
     C                   KFLD                    AHHEDC
     C                   KFLD                    AHOQL#
      *
      *          ------------------------------------------
      *          OMQAH   Q&A history file
      *          ------------------------------------------
     C     $AH1K1        KLIST
     C                   KFLD                    NCHEDC
     C                   KFLD                    NCINC#
      *
      *          ------------------------------------------
      *          OMHQU   Question file
      *          ------------------------------------------
     C     $QU1K1        KLIST
     C                   KFLD                    AHHEDC
     C                   KFLD                    AHQUE#
      *
      *          ------------------------------------------
      *          OMANS   Answer file
      *          ------------------------------------------
     C     $NS1K1        KLIST
     C                   KFLD                    AHHEDC
     C                   KFLD                    AHQUE#
     C                   KFLD                    AHANS#
      *
      *          ------------------------------------------
      *          OMTIH   Text header file
      *          ------------------------------------------
     C     $IH1K1        KLIST
     C                   KFLD                    NCHEDC
     C                   KFLD                    NCINC#
     C     $IH1K2        KLIST
     C                   KFLD                    NCHEDC
     C                   KFLD                    NCINC#
     C                   KFLD                    IHITH#
      *
      *          ------------------------------------------
      *          OMTID   Text detail file
      *          ------------------------------------------
     C     $ID1K1        KLIST
     C                   KFLD                    NCHEDC
     C                   KFLD                    NCINC#
     C                   KFLD                    IHITH#
     C     $ID1K2        KLIST
     C                   KFLD                    NCHEDC
     C                   KFLD                    NCINC#
     C                   KFLD                    IHITH#
     C                   KFLD                    IDSEQ#
      *
      *          ------------------------------------------
      *          OMINCL1 key list incident file
      *          ------------------------------------------
     C     $NC1K1        KLIST
     C                   KFLD                    NCHEDC
      *
      *          ------------------------------------------
      *          OMINCL1 key list incident file
      *          ------------------------------------------
     C     $NC1K2        KLIST
     C                   KFLD                    NCHEDC
     C                   KFLD                    NCINC#
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
      *          Get the incident text *DETAIL
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 22                                 B01
     C                   IF        INC(MS) <> *BLANKS                           B02
     C                   EVAL      D#MDTA = *BLANK
     C                   MOVE      INC(MS)       P#MIDC
     C                   CALLP     QMHRTVM ( D#RTVM : P#MDL# : P#FMTC :
     C                             P#MIDC : P#MSQL : P#DUM6 : P#LOB# :
     C                             P#DUM7 : P#DUM8 : P#ERR )
     C                   EVAL      VAR(MS) = D#MDTA
     C                   ENDIF                                                  E02
     C                   ENDFOR                                                 E01
     C                   MOVEL     VAR(1)        INC001
     C                   MOVEL     VAR(2)        INC002
     C                   MOVEL     VAR(3)        INC003
     C                   MOVEL     VAR(4)        INC004
     C                   MOVEL     VAR(5)        INC005
     C                   MOVEL     VAR(6)        INC006
     C                   MOVEL     VAR(7)        INC007
     C                   MOVEL     VAR(8)        INC008
     C                   MOVEL     VAR(9)        INC009
     C                   MOVEL     VAR(10)       INC010
     C                   MOVEL     VAR(11)       INC011
     C                   MOVEL     VAR(12)       INC012
     C                   MOVEL     VAR(13)       INC013
     C                   MOVEL     VAR(14)       INC014
     C                   MOVEL     VAR(15)       INC015
     C                   MOVEL     VAR(16)       INC016
     C                   MOVEL     VAR(17)       INC017
     C                   IF        I3AVE = '1'
     C                   MOVEL     VAR(18)       INC018
     C                   ELSE
     C                   MOVEL     VAR(22)       INC018
     C                   ENDIF
     C                   MOVEL     VAR(19)       INC019
     C                   MOVEL     VAR(20)       INC020
     C                   MOVEL     VAR(21)       INC021
      *
      *          ------------------------------------------
      *          Get the "text" text
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 5                                  B01
     C                   IF        TYP(MS) <> *BLANKS                           B02
     C                   EVAL      D#MDTA = *BLANK
     C                   MOVE      TYP(MS)       P#MIDC
     C                   CALLP     QMHRTVM ( D#RTVM : P#MDL# : P#FMTC :
     C                             P#MIDC : P#MSQL : P#DUM6 : P#LOB# :
     C                             P#DUM7 : P#DUM8 : P#ERR )
     C                   EVAL      TXT(MS) = D#MDTA
     C                   ENDIF                                                  E02
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          Get the rest of the text
      *          ------------------------------------------
     C                   EVAL      D#MDTA = *BLANK
     C                   IF        I3LOD = '0'                                  B01
     C                   MOVEL     'TRE0015'     P#MIDC
     C                   ELSE                                                   X01
     C                   MOVEL     'TRE0017'     P#MIDC
     C                   ENDIF                                                  E01
     C                   CALLP     QMHRTVM ( D#RTVM : P#MDL# : P#FMTC :
     C                             P#MIDC : P#MSQL : P#DUM6 : P#LOB# :
     C                             P#DUM7 : P#DUM8 : P#ERR )
     C                   MOVEL     D#MDTA        TRE015
     C                   EVAL      D#MDTA = *BLANK
     C                   MOVEL     'TRE0013'     P#MIDC
     C                   CALLP     QMHRTVM ( D#RTVM : P#MDL# : P#FMTC :
     C                             P#MIDC : P#MSQL : P#DUM6 : P#LOB# :
     C                             P#DUM7 : P#DUM8 : P#ERR )
     C                   MOVEL     D#MDTA        TRE013
     C                   EVAL      D#MDTA = *BLANK
     C                   MOVEL     'TRE0014'     P#MIDC
     C                   CALLP     QMHRTVM ( D#RTVM : P#MDL# : P#FMTC :
     C                             P#MIDC : P#MSQL : P#DUM6 : P#LOB# :
     C                             P#DUM7 : P#DUM8 : P#ERR )
     C                   MOVEL     D#MDTA        TRE014
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRENDP - Print end page                                     *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRENDP        BEGSR
      *
      *          ------------------------------------------
      *          Retrieve cover messages
      *          ------------------------------------------
     C                   IF        TRE106 = *BLANK                              B01
     C                   FOR       MS = 1 TO 7                                  B02
     C                   IF        END(MS) <> *BLANKS                           B03
     C                   EVAL      D#MDTA = *BLANK
     C                   MOVE      END(MS)       P#MIDC
     C                   CALLP     QMHRTVM ( D#RTVM : P#MDL# : P#FMTC :
     C                             P#MIDC : P#MSQL : P#DUM6 : P#LOB# :
     C                             P#DUM7 : P#DUM8 : P#ERR )
     C                   EVAL      VAR(MS) = D#MDTA
     C                   ENDIF                                                  E03
     C                   ENDFOR                                                 E02
     C                   MOVEL     VAR(1)        TRE106
     C                   MOVEL     VAR(2)        TRE107
     C                   MOVEL     VAR(3)        TRE108
     C                   MOVEL     VAR(4)        TRE109
     C                   MOVEL     VAR(5)        TRE110
     C                   MOVEL     VAR(6)        END001
     C                   MOVEL     VAR(7)        TRE112
     C                   ENDIF                                                  E01
      *
     C                   IF        FMPRTI <> 0
     C                             AND FMPAVG <> 0
     C                             AND W#TEL <> 0
     C     FMPAVG        DIV       60            FMPAVG
     C     FMPAVG        DIV       W#TEL         FMPAVG
     C     FMPAVG        DIV       1             W#HRS
     C                   MVR                     W#MIN
     C                   MULT      0.60          W#MIN
     C                   ADD       W#HRS         W#MIN
     C                   Z-ADD     W#MIN         FMPAVG
     C                   ELSE
     C                   EVAL      FMPAVG = *ZERO
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Write page header (do not reset page indic)
      *          ------------------------------------------
     C                   WRITE     PAGEH
      *
      *          ------------------------------------------
      *          Write end page
      *          ------------------------------------------
     C                   WRITE     ENDPG
      *
      *          ------------------------------------------
      *          Init the count fields
      *          ------------------------------------------
     C                   EVAL      FMPRTI = 0
     C                   EVAL      FMPRTR = 0
     C                   EVAL      FMPRTF = 0
     C                   EVAL      FMPRTS = 0
     C                   EVAL      FMPRQR = 0
     C                   EVAL      FMPRQF = 0
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
     C                   MOVEL     I3ERR         P#ERD
     C                   MOVEL     I3FLD         P#FLD
     C                   MOVEL     I3BUF         P#BUF
     C                   MOVEL     I3PRC         P#PRC
     C                   ENDIF
      *
      *
      *          ------------------------------------------
      *          When no parm or I3EOP = '1' then close
      *          ------------------------------------------
     C                   IF        D#PARM = *ZERO                               B01
     C                             OR I3EOP = '1'
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
     C                   EVAL      P#STAT = '*TERM'
     C                   IF        I#PSSR <> *ON                                B01
     C                   EVAL      I#PSSR = *ON
      *
      *          ------------------------------------------
      *          Post information about the last used file
      *          ------------------------------------------
     C                   SELECT                                                 B02
      *
     C                   WHEN      E#FILE = 'OMERRL2 '                          X02
     C                   MOVEL     ERRL2         FILE
      *
     C                   WHEN      E#FILE = 'OMERRL3 '                          X02
     C                   MOVEL     ERRL3         FILE
      *
     C                   WHEN      E#FILE = 'OMERRL4 '                          X02
     C                   MOVEL     ERRL4         FILE
      *
     C                   WHEN      E#FILE = 'OMRSRL1 '                          X02
     C                   MOVEL     RSRL1         FILE
      *
     C                   WHEN      E#FILE = 'OMESDL1 '                          X02
     C                   MOVEL     ESDL1         FILE
      *
     C                   WHEN      E#FILE = 'OMEROL1 '                          X02
     C                   MOVEL     EROL1         FILE
      *
     C                   WHEN      E#FILE = 'OMEODL1 '                          X02
     C                   MOVEL     EODL1         FILE
      *
     C                   WHEN      E#FILE = 'OMOBJL8 '                          X02
     C                   MOVEL     OBJL8         FILE
      *
     C                   WHEN      E#FILE = 'OMMBRL1 '                          X02
     C                   MOVEL     MBRL1         FILE
      *
     C                   WHEN      E#FILE = 'OMTABL1 '                          X02
     C                   MOVEL     TABL1         FILE
      *
     C                   WHEN      E#FILE = 'OMERRL2 '                          X02
     C                   MOVEL     ERRL2         FILE
      *
     C                   WHEN      E#FILE = 'OMERDL1 '                          X02
     C                   MOVEL     ERDL1         FILE
      *
     C                   WHEN      E#FILE = 'OMQPRTIN'                          X02
     C                   MOVEL     QPRTIN        FILE
      *
     C                   WHEN      E#FILE = 'OMSOVL1 '                          X02
     C                   MOVEL     SOVL1         FILE
      *
     C                   WHEN      E#FILE = 'OMSOVL2 '                          X02
     C                   MOVEL     SOVL2         FILE
      *
     C                   WHEN      E#FILE = 'OMINCL1 '                          X02
     C                   MOVEL     INCL1         FILE
      *
     C                   WHEN      E#FILE = 'OMRIRL1 '                          X02
     C                   MOVEL     RIRL1         FILE
      *
     C                   WHEN      E#FILE = 'OMQAHL1 '                          X02
     C                   MOVEL     QAHL1         FILE
      *
     C                   WHEN      E#FILE = 'OMCCAL1 '                          X02
     C                   MOVEL     CCAL1         FILE
      *
     C                   WHEN      E#FILE = 'OMHQUL1 '                          X02
     C                   MOVEL     HQUL1         FILE
      *
     C                   WHEN      E#FILE = 'OMHQLL1 '                          X02
     C                   MOVEL     HQLL1         FILE
      *
     C                   WHEN      E#FILE = 'OMANSL1 '                          X02
     C                   MOVEL     ANSL1         FILE
      *
     C                   WHEN      E#FILE = 'OMTIHL1 '                          X02
     C                   MOVEL     TIHL1         FILE
      *
     C                   WHEN      E#FILE = 'OMTIDL1 '                          X02
     C                   MOVEL     TIDL1         FILE
      *
     C                   ENDSL                                                  E02
      *
      *          ------------------------------------------
      *          Call the error handler
      *          ------------------------------------------
     C                   CALLP     OMX961 ( PGM : FILE )
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
      *          Send the general escape message
      *          ------------------------------------------
     C                   EVAL      P#MIDC = 'OMQ5012'
     C                   EVAL      P#MSGQ = C#API
     C                   EVAL      MSGD = 'OMQPRTIN'
     C                   EVAL      P#LOB# = 8
     C                   EVAL      P#MSTC = '*ESCAPE'
     C                   EVAL      P#PMQC = '*'
     C                   EVAL      P#PSC# = 1
     C                   CALLP     QMHSNDPM ( P#MIDC : P#MSGQ : MSGD :
     C                             P#LOB# : P#MSTC : P#PMQC : P#PSC# :
     C                             P#MSKC : P#ERR )
      *
      *          ------------------------------------------
      *          Else exit immediately ...
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   EVAL      *INLR = *ON
     C                   RETURN
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
     C     PSSR99        TAG
     C                   ENDSR
      * ------------------------------------------------------------ *
      * Compile time arrays                                          *
      * ------------------------------------------------------------ *
**    DAT
000      -  Januari                              -  1
031      -  Februari                             -  2
059      -  Maart                                -  3
090      -  April                                -  4
120      -  Mei                                  -  5
151      -  Juni                                 -  6
181      -  Juli                                 -  7
212      -  Augustus                             -  8
243      -  September                            -  9
273      -  Oktober                              - 10
304      -  November                             - 11
334      -  December                             - 12
**    GEN
TRE0010  -  Page header normal report            -  1
TRE0010  -  Page header quality report           -  2
**    COV
TRE0101  -                                       -  1
APP0001  -                                       -  2
ERR0001  -                                       -  3
ERR0012  -                                       -  4
ERR0013  -                                       -  5
FIX0009  -                                       -  6
FIX0010  -                                       -  7
SOL0001  -                                       -  8
TRE0103  -                                       -  9
TRE0104  -                                       - 10
PDL0003  -                                       - 11
ERR0014  -                                       - 12
PRI0001  -                                       - 13
ERR0011  -                                       - 14
USR0003  -                                       - 15
TEL0001  -                                       - 16
ADR0005  -                                       - 17
USR0011  -                                       - 18
DAT0001  -                                       - 19
DAT0012  -                                       - 20
DAT0013  -                                       - 21
** End
TRE0106  -  Requests printed                     -  1
TRE0107  -  Fixes    printed                     -  2
TRE0108  -  Solutions printed                    -  3
TRE0109  -  Requests quality                     -  4
TRE0110  -  Fixes quality                        -  5
END0001  -  End of report                        -  6
TRE0111  -  Incidents printed                    -  7
TRE0112  -  Average of printed                   -  8
**
TRE0002  -  Request  *SHORT *LIST header         -  1
**
TRE0003  -  Fix      *SHORT *LIST header         -  1
**
TRE0004  -  Solution *SHORT *LIST header         -  1
**
TRE0005  -  Fix      *SHORT *COMPOUND header     -  1
**
TRE0006  -  Solution *SHORT *COMPOUND header     -  1
**
TRE0008  -  Quality short report header          -  1
TRE0009  -  Quality short total line             -  2
**
ERR0001  -  Request number                       -  1
ERR0002  -  Short description                    -  2
ERR0006  -  Request status                       -  3
ERR0014  -  Request type                         -  4
USR0003  -  User id                              -  5
USR0011  -  Remote user                          -  6
DAT0004  -  Expected compl-date                  -  7
DAT0005  -  Realized compl-date                  -  8
DAT0006  -  Income date                          -  9
PRI0001  -  Priority                             - 10
TEL0001  -  Telephone                            - 11
**
ERR0001  -  Request number                       -  1
FIX0001  -  Fix number                           -  2
FIX0002  -  Fix status                           -  3
FIX0003  -  Fix short description                -  4
FIX0011  -  Fix type                             -  5
PGM0001  -  Programmer                           -  6
RLS0002  -  Release                              -  7
PRI0001  -  Priority                             -  8
DAT0014  -  Expected start date                  -  9
DAT0015  -  Realized start date                  - 10
DAT0016  -  Expected dev-end date                - 11
DAT0017  -  Realized dev-end date                - 12
DAT0018  -  Expected completion date             - 13
DAT0019  -  Realized completion date             - 14
HRS0001  -  Expected hours                       - 15
HRS0002  -  Realized hours                       - 16
REA0001  -  Reason code                          - 17
DEX0001  -  Development exit count               - 18
DAT0020  -  Start dates                          - 19
DAT0021  -  Developmend end dates                - 20
DAT0022  -  Completion dates                     - 21
HRS0003  -  Hours                                - 22
**
FIX0001  -  Fix number                           -  1
OBJ0001  -  Object code                          -  2
OBJ0002  -  Object description                   -  3
OBJ0003  -  Object type                          -  4
OBJ0004  -  Object attribute                     -  5
OBJ0008  -  Object library                       -  6
DET0001  -  Detail code                          -  7
SOL0002  -  Solution type                        -  8
OVR0001  -  Override code                        -  9
PDL0003  -  Environment code                     - 10
STA0001  -  Status                               - 11
VRS0007  -  Version                              - 12
VRS0008  -  Version based on                     - 13
SRC0006  -  Process source                       - 14
OBJ0022  -  Object class                         - 15
FOL0001  -  Followers are                        - 16
LEA0001  -  Leader is                            - 17
**
TRE0011  -  Incident  *SHORT *LIST header        -  1
TRE0016  -  Incident  *DETAIL header             -  2
TRE0211  -  Incident  *SHORT *LIST header average-  3
**
INC0006  -  Incident                             -  1
CTN0001  -  Call track                           -  2
ACC0007  -  Accumulated incident                 -  3
STA0004  -  Status route                         -  4
INC0007  -  Inc date/time                        -  5
DAT0034  -  Cmp date/time                        -  6
STA0005  -  Status/support level                 -  7
STA0006  -  Status date/time                     -  8
SEV0006  -  Severity                             -  9
PRI0001  -  Priority                             - 10
ESC0002  -  Escalation counter                   - 11
ESC0003  -  Esc. date/time                       - 12
USR0012  -  Registered by                        - 13
USR0013  -  Allocated by                         - 14
INC0005  -  Incident type                        - 15
CFG0002  -  Cfg item/list                        - 16
USR9021  -  User field 1                         - 17
DSC0004  -  Description                          - 18
USR9022  -  User field 2                         - 19
USR9023  -  User field 3                         - 20
CUS0005  -  Customer code                        - 21
AVG0002  -  Average hours                        - 22
**
TXT0002  -  symptom                              -  1
TXT0003  -  cause                                -  2
TXT0004  -  solution                             -  3
TXT0005  -  other                                -  4
TYP0007  -  Type:                                -  4
