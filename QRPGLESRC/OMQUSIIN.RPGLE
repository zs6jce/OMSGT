     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMQUSIIN                                      *
      * Function ...: Interface with Incident through API           *
      * Module .....: HelpDesk                                      *
      * Author .....: Remain Software                               *
      * Parameters .: I2INTL  - Interface level                     *
      *               I2ACTC  - Action code                         *
      *               I2BUF   - File buffer                         *
      *               I2PRCA  - Processing array                    *
      *               I2ERR   - Error buffer                        *
      *               I2FUN#  - Number of function keys             *
      *               I2FUN   - Function key table                  *
      *               I2FKU#  - Function key used                   *
      *               I2CFLC  - Field of cursor                     *
      *               I2KEY   - Key buffer                          *
      *                                                             *
      * This option is the user interface API for the Inc. The      *
      * change/add/display/copy and delete actions are presented    *
      * to the user and handled in this program.                    *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Files                                                       *
      * ----------------------------------------------------------- *
     FOMCCAL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(CCAL1)
     FOMHDDL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     USROPN
     F                                     INFDS(HDDL1)
     FOMINCL1   UF   E           K DISK    INFSR(*PSSR)
     F                                     USROPN
     F                                     INFDS(INCL1)
     FOMTIHL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     USROPN
     F                                     INFDS(TIHL1)
     FOMCCOL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     USROPN
     F                                     INFDS(CCOL1)
     FOMMPXL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     USROPN
     F                                     INFDS(MPXL1)
     FOMQUSIIND CF   E             WORKSTN INFSR(*PSSR)
     F                                     USROPN
     F                                     INFDS(INFO)
     F                                     SFILE(QUSIIN01:RRN01)
     F                                     SFILE(QUSIIN03:RRN02)
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Extension specifications                                    *
      *                                                             *
      * VAR   Contains substitution text                            *
      * MSD   Contains variable message id (compile time array)     *
      * MC    Used to fill message variables in message data parm.  *
      * TXT   Contains screen texts in the available language       *
      * TXD   Contains variable message id (compile time array)     *
      *                                                             *
      * PRA   Processing array to work with                         *
      *                                                             *
      * FA    Active function keys array                            *
      * FT    Active function keys type array                       *
      * LIN   Contains all the function key lines                   *
      * FUN   Used to format the function key lines                 *
      * FIN   Function key array input                              *
      *                                                             *
      * T05   The text of 'F5=Refresh'                              *
      * T06   The text of 'F6=Add text'                             *
      * T07   The text of 'F6=Show Customer'                        *
      * T11   The text of 'F11=View Item/Text'                      *
      * T24   The text of 'F24=More keys'                           *
      *                                                             *
      * INK   The screen function keys  (F1-F24, RU & RD)           *
      *                                                             *
      * FLD   Fields in error.                                      *
      * FL    Fields                                                *
      * FP    Position of the error indicator for field in FL       *
      * FB    Array with the data buffer                            *
      *                                                             *
      * ABH   After or before list entry handle codes               *
      * ABI   After or before indicators.                           *
      * CMH   Copy or move list entry handle codes                  *
      * CMI   Copy or move indicators.                              *
      *                                                             *
      * ITH   Incident text headers used to roll.                   *
      * ----------------------------------------------------------- *
     D VAR             S            256    DIM(20)
     D MSD             S              7    DIM(20) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
     D TXT             S             50    DIM(20)
     D TXD             S              7    DIM(20) CTDATA PERRCD(1)
      *
     D PRA             S              1    DIM(16)
      *
     D FA              S              1    DIM(24)
     D FT              S              1    DIM(24)
     D LIN             S             78    DIM(12)
     D FIN             S             34    DIM(23)
      *
     D T05             S              1    DIM(30)
     D T06             S              1    DIM(30)
     D T07             S              1    DIM(30)
     D T11             S              1    DIM(30)
      *
     D INK             S              1    DIM(26)
      *
     D FLD             S              4    DIM(30)
     D FL              S              4    DIM(30) CTDATA PERRCD(1)
     D FP              S              3  0 DIM(30) CTDATA PERRCD(1)
     D FB              S              1    DIM(30)
      *
     D ABH             S              4    DIM(100)
     D ABI             S              1    DIM(100)
     D CMH             S              4    DIM(100)
     D CMI             S              1    DIM(100)
      *COPY QCPYSRC,OMSREFREN
      * ----------------------------------------------------------- *
      * Input specifications                                        *
      * ----------------------------------------------------------- *
      *
      *
      *
      *          ------------------------------------------
      *          Reference file is used for field definition
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          Screen information data structure
      *          ------------------------------------------
     D INFO            DS
     D D#LOC#                370    371B 0
     D D#CRC#                378    379B 0
      *
      *          ------------------------------------------
      *          File information data structures
      *          ------------------------------------------
     D IOBL1           DS            57
     D COBL1           DS            57
     D CCAL1           DS            57
     D HDDL1           DS            57
     D INCL1           DS            57
     D TIHL1           DS            57
     D CCOL1           DS            57
     D MPXL1           DS            57
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  D#PGM                  1     10
     D  E#MSID                40     46
     D  E#FILE               201    208
     D  D#USID               254    263
      *
      *          ------------------------------------------
      *          System date
      *          ------------------------------------------
     D                 DS
     D D#SYSC                         6  0
     D  D#SYSY                        2  0 OVERLAY(D#SYSC)
     D  D#SYSM                        2  0 OVERLAY(D#SYSC:3)
     D  D#SYSD                        2  0 OVERLAY(D#SYSC:5)
      *
      *          ------------------------------------------
      *          Structure to compare array's.
      *          ------------------------------------------
     D                 DS
     D  FUN                    1     30
     D                                     DIM(30)
     D  FUNFLD                 1     30
     D  T24                   31     60
     D                                     DIM(30)
     D  T24FLD                31     60
      *
      *          ------------------------------------------
      *          Structure to convert numbers.
      *          ------------------------------------------
     D                 DS
     D D#BI2#                         4B 0
     D  D#BI2A                        2    OVERLAY(D#BI2#)
     D D#BI4#                         9B 0
     D  D#BI4A                        4    OVERLAY(D#BI4#)
     D D#D2#                          2  0
     D  D#D2A                         2    OVERLAY(D#D2#)
     D   D#D21A                       1    OVERLAY(D#D2A:2)
     D D#D22A                         1
      *
      *          ------------------------------------------
      *          Parameter: Error buffer
      *          ------------------------------------------
     D F#ERR           DS
     D F#MSID                         7
     D F#MSTP                         1
     D F#MSGD                       128
      *
      *          ------------------------------------------
      *          Helpdesk data structures.
      *          ------------------------------------------
      /COPY QCPYLESRC,OMHLPDS
      *
      *          ------------------------------------------
      *          OMQUSIIN data strucutures.
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQUSIINDS
      *
      *          ------------------------------------------
      *          Parameter: Function key buffer
      *          ------------------------------------------
     D I2FUN           DS            34
     D I2KEY#                         4B 0
     D I2TYPC                         1
     D I2TOMC                         1
     D I2KEYD                        30
     D  I2KMID                        7    OVERLAY(I2KEYD)
     D  I2KMFC                       10    OVERLAY(I2KEYD:8)
     D  I2KMLC                       10    OVERLAY(I2KEYD:18)
      *
      *          ------------------------------------------
      *          OMQCHGIN data strucutures.
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQCHGINDS
      *
      *          ------------------------------------------
      *          OMQCHGIN extra redefinitions
      *          ------------------------------------------
     D  NCHEDC                 1      5
     D  NCINC#                 6     15
     D  NCCTN#                16     25
     D  NCAIN#                26     35
     D  NCINDT                36     42  0
     D  NCINTM                43     48  0
     D  NCRCDT                49     55  0
     D  NCRCTM                56     61  0
     D  NCSTRC                62     71
     D  NCRES#                72     73P 0
     D  NCSUL#                74     74
     D  NCSTAT                75     79
     D  NCSRDT                80     86  0
     D  NCSRTM                87     92  0
     D  NCSEV1                93     97
     D  NCSEV2                98    102
     D  NCSEV3               103    107
     D  NCPRI#               108    109P 0
     D  NCESC#               110    111P 0
     D  NCIEDT               112    118  0
     D  NCIETM               119    124  0
     D  NCRUID               125    134
     D  NCAUID               135    144
     D  NCITPC               145    149
     D  NCCFL#               150    152P 0
     D  NCCFIC               153    164
     D  NCUSF1               165    174
     D  NCUSF2               175    199
     D  NCUSF3               200    207P 0
     D  NCSHID               208    257
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
      *          Input buffer (7Kb)
      *          ------------------------------------------
     D P#BUF           DS          7168
      *
      *          ------------------------------------------
      *          Date and time stamp data structure
      *          ------------------------------------------
     D D#STMP          DS
     D D#STDT                         7  0
     D D#STTM                         6  0
      *
      *          ------------------------------------------
      *          Data structure to convert the edit quantity
      *          ------------------------------------------
     D                 DS
     D FMQTY#                         1  0
     D  D#FMQY                        1    OVERLAY(FMQTY#)
      *
      *          ------------------------------------------
      *          Date converter input
      *          ------------------------------------------
     D                 DS
     D P#DTIN                        16
     D  D#IDAT                        7  0 OVERLAY(P#DTIN)
     D   D#CIN                        1  0 OVERLAY(D#IDAT)
     D   D#DTIN                       6  0 OVERLAY(D#IDAT:2)
     D    D#YY                        2  0 OVERLAY(D#DTIN)
     D    D#MM                        2  0 OVERLAY(D#DTIN:3)
     D    D#DD                        2  0 OVERLAY(D#DTIN:5)
     D  D#TMIN                        6  0 OVERLAY(P#DTIN:8)
     D  D#MSIN                        3  0 OVERLAY(P#DTIN:14)
      *
      *          ------------------------------------------
      *          Date converter output
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
      *          Constants used in program
      *          ------------------------------------------
     D C#CNCL          C                   CONST('*CNCL')
     D C#EXIT          C                   CONST('*EXIT')
     D C#NORM          C                   CONST('*NORM')
     D C#TERM          C                   CONST('*TERM')
     D C#NONE          C                   CONST('*NONE')
     D C#NEW           C                   CONST('*NEW ')
     D C#OMSG          C                   CONST('OMHMSG    ')
     D C#ODSP          C                   CONST('OMHDSP    ')
     D C#LIBL          C                   CONST('*LIBL     ')
     D C#201C          C                   CONST('OMX201C   ')
     D C#PGM           C                   CONST('*PGM     ')
     D C#FIX           C                   CONST('FIX       ')
     D C#INTL          C                   CONST('V3R0M0  ')
     D C#EMAI          C                   CONST('(e-mail)')
     D C#MRS1          C                   CONST(25)
      *
      *          ------------------------------------------
      *          Constants used for display file override
      *          ------------------------------------------
     D C#OVR1          C                   CONST('OVRDBF FILE(OMQUSIIN-
     D                                     ) TOFILE(*LIBL/OMQUS-
     D                                     IIND)')
      * Prototype for 'OMX105'
     D OMX105          PR                  EXTPGM('OMX105')
     D FMHEDC_                             LIKE(FMHEDC)
     D P#HEDD_                             LIKE(P#HEDD)
      * Prototype for 'OMX126'
     D OMX126          PR                  EXTPGM('OMX126')
     D P#HEDC_                             LIKE(P#HEDC)
     D FMSTRC_                             LIKE(FMSTRC)
     D STRD_                               LIKE(STRD)
      * Prototype for 'OMX125'
     D OMX125          PR                  EXTPGM('OMX125')
     D P#HEDC_                             LIKE(P#HEDC)
     D FMITPC_                             LIKE(FMITPC)
     D ITPD_                               LIKE(ITPD)
      * Prototype for 'OMX106'
     D OMX106          PR                  EXTPGM('OMX106')
     D P#HEDC_                             LIKE(P#HEDC)
     D TABC_                               LIKE(TABC)
     D FMSEV1_                             LIKE(FMSEV1)
     D DESC_                               LIKE(DESC)
      * Prototype for 'QWCCVTDT'
     D QWCCVTDT        PR                  EXTPGM('QWCCVTDT')
     D P#INFM_                             LIKE(P#INFM)
     D P#DTIN_                             LIKE(P#DTIN)
     D P#OTFM_                             LIKE(P#OTFM)
     D P#DTOT_                             LIKE(P#DTOT)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'OMQCHGIN'
     D OMQCHGIN        PR                  EXTPGM('OMQCHGIN')
     D I4INTL_                             LIKE(I4INTL)
     D I4ACTC_                             LIKE(I4ACTC)
     D I4BUF_                              LIKE(I4BUF)
     D I4PRC_                              LIKE(I4PRC)
     D I4ERR_                              LIKE(I4ERR)
     D I4FLD_                              LIKE(I4FLD)
      * Prototype for 'OMQUSICI'
     D OMQUSICI        PR                  EXTPGM('OMQUSICI')
     D P2INTL_                             LIKE(P2INTL)
     D P2ACTC_                             LIKE(P2ACTC)
     D FMOTID_                             LIKE(FMOTID)
     D FMOCID_                             LIKE(FMOCID)
     D FMSNBR_                             LIKE(FMSNBR)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH054C'
     D OMH054C         PR                  EXTPGM('OMH054C')
     D P#ACTC_                             LIKE(P#ACTC)
     D FMHEDC_                             LIKE(FMHEDC)
     D FMCUSC_                             LIKE(FMCUSC)
     D FMCCTC_                             LIKE(FMCCTC)
     D FMINC#_                             LIKE(FMINC#)
     D FMOTID_                             LIKE(FMOTID)
     D FMOCID_                             LIKE(FMOCID)
     D FMSNBR_                             LIKE(FMSNBR)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH024'
     D OMH024          PR                  EXTPGM('OMH024')
     D P#CUSC_                             LIKE(P#CUSC)
     D P#CCTC_                             LIKE(P#CCTC)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH029 (Get system info)
      * Prototype for 'OMH902C'
     D OMH902C         PR                  EXTPGM('OMH902C')
     D PGMQ_                               LIKE(PGMQ)
      *
      *          ------------------------------------------
      *          Parameter list for OMH903C (Get system parameters)
      * Prototype for 'OMH901C'
     D OMH901C         PR                  EXTPGM('OMH901C')
     D D#MSID_                             LIKE(D#MSID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D D#MSGD_                             LIKE(D#MSGD)
      *
      *          ------------------------------------------
      *          Parameter list for OMH902C (Clear message queue)
      * Prototype for 'OMX201C'
     D OMX201C         PR                  EXTPGM('OMX201C')
     D FMHEDC_                             LIKE(FMHEDC)
     D P#CLS#_                             LIKE(P#CLS#)
     D P#EXWP_                             LIKE(P#EXWP)
     D P#EXTX_                             LIKE(P#EXTX)
     D P#ACTC_                             LIKE(P#ACTC)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for QCMDEXEC
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
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
      *          Keylist for the incident file
      * Prototype for 'OMH904C'
     D OMH904C         PR                  EXTPGM('OMH904C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGTXT_                             LIKE(MSGTXT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH905C (Get user profile)
      * Prototype for 'OMH905C'
     D OMH905C         PR                  EXTPGM('OMH905C')
     D P#USID_                             LIKE(P#USID)
     D P#USCL_                             LIKE(P#USCL)
     D P#GRID_                             LIKE(P#GRID)
     D P#GRCL_                             LIKE(P#GRCL)
     D P#USDC_                             LIKE(P#USDC)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH909C (Check Object)
      * Prototype for 'OMH909C'
     D OMH909C         PR                  EXTPGM('OMH909C')
     D P#OBJL_                             LIKE(P#OBJL)
     D P#OBJC_                             LIKE(P#OBJC)
     D P#MATM_                             LIKE(P#MATM)
     D P#OBJT_                             LIKE(P#OBJT)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH949C (Change application).
      * Prototype for 'QCMDEXC'
     D QCMDEXC         PR                  EXTPGM('QCMDEXC')
     D P#EXE_                              LIKE(P#EXE)
     D P#LEN_                              LIKE(P#LEN)
      *
      *          ------------------------------------------
      *          Parameter list for QCMDEXEC
      * Prototype for 'OMQCHGIN'
     D OMQCHGIN_001    PR                  EXTPGM('OMQCHGIN')
      * Prototype for 'OMH023'
     D OMH023          PR                  EXTPGM('OMH023')
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
     D p#ith#          S              3P 0
     D s#rr01          S              5P 0
     D s#rr02          S              5P 0
     D w#rr01          S              5P 0
     D w#rr02          S              5P 0
     D AB              S              5  0
     D CL              S              5  0
     D CM              S              5  0
     D EROR01          S                   LIKE(INDI)
     D EROR02          S                   LIKE(INDI)
     D FILE            S             57
     D I#ADD           S                   LIKE(INDI)
     D I#CHG           S                   LIKE(INDI)
     D I#CPY           S                   LIKE(INDI)
     D I#DEL           S                   LIKE(INDI)
     D I#DSP           S                   LIKE(INDI)
     D I#F11           S                   LIKE(INDI)
     D I#F11U          S                   LIKE(INDI)
     D I#F24           S                   LIKE(INDI)
     D I#F5            S                   LIKE(INDI)
     D I#F5U           S                   LIKE(INDI)
     D I#F6            S                   LIKE(INDI)
     D I#F6U           S                   LIKE(INDI)
     D I#F7            S                   LIKE(INDI)
     D I#F7U           S                   LIKE(INDI)
     D I#HDE           S                   LIKE(INDI)
     D I#HEDC          S                   LIKE(INDI)
     D I#H023          S                   LIKE(INDI)
     D I#IHL1          S                   LIKE(INDI)
     D I#IN27          S                   LIKE(INDI)
     D I#OBL1          S                   LIKE(INDI)
     D I#OPEN          S                   LIKE(INDI)
     D I#PSSR          S              1
     D I#SHID          S                   LIKE(INDI)
     D I#VIEW          S                   LIKE(INDI)
     D I#WP            S                   LIKE(INDI)
     D I#WRT           S                   LIKE(INDI)
     D I#502E          S                   LIKE(INDI)
     D IX              S              5  0
     D IY              S              5  0
     D PDLC            S              5
     D K#              S              5  0
     D LOOP01          S                   LIKE(INDI)
     D LOOP02          S                   LIKE(INDI)
     D MA_X            S              5  0
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D OC              S              5  0
     D P#ACTC          S              2
     D P#CCTC          S                   LIKE(CCTC)
     D P#CLS#          S             10
     D P#CUSC          S                   LIKE(CUSC)
     D P#EXE           S             60
     D P#APNM          S             30
     D P#CONM          S             50
     D P#EXTX          S                   LIKE(INC#)
     D P#EXWP          S              1
     D P#GRCL          S                   LIKE(USID)
     D P#GRID          S                   LIKE(USID)
     D P#HEDC          S                   LIKE(HEDC)
     D P#HEDD          S                   LIKE(HEDD)
     D P#IB            S             16
     D P#INC#          S                   LIKE(INC#)
     D P#INFM          S             10
     D P#LEN           S             15  5
     D P#MATM          S                   LIKE(MATM)
     D P#MSTA          S             40
     D P#OBJC          S                   LIKE(OBJC)
     D P#OBJL          S                   LIKE(OBJL)
     D P#OBJT          S                   LIKE(OBJT)
     D P#OPTI          S                   LIKE(INDI)
     D P#OTFM          S             10
     D P#PDLC          S                   LIKE(PDLC)
     D P#STAT          S                   LIKE(STAT)
     D P#SUL#          S                   LIKE(SUL#)
     D P#USCL          S                   LIKE(USID)
     D P#USDC          S                   LIKE(USDC)
     D P#USID          S                   LIKE(USID)
     D P2ACTC          S                   LIKE(ACTC)
     D P2INTL          S                   LIKE(INTL)
     D RRN01           S              5  0
     D RRN02           S              5  0
     D S#CR01          S                   LIKE(CRRN01)
     D S#CR02          S                   LIKE(CRRN02)
     D S#LEHC          S                   LIKE(P#LEHC)
     D W#CHA8          S              8
     D W#F24#          S              5  0
     D W#TKL#          S              5  0
     D W#TLN#          S              5  0
     D W#TX30          S             30
     D W#20A           S             20
      *----------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *----------------------------------------------------------------------
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Main line   No SRVERW in this program                       *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Get the parameters.
      *          ------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    I2INTL
     C                   PARM                    I2ACTC
     C                   PARM                    I2INC
     C                   PARM                    I2PRCA
     C                   PARM                    I2ERR
     C                   PARM                    I2FDS#
     C                   PARM                    FIN
     C                   PARM                    I2KDS#
     C                   PARM                    I2CFLC
     C                   PARM                    I2KEY
      *
      *          ------------------------------------------
      *          General API handler (do this first)
      *          ------------------------------------------
     C                   EXSR      SRFRST
      *
      *          ------------------------------------------
      *          Start screen processing.
      *          ------------------------------------------
     C                   EXSR      SRSC01
      *
      *          ------------------------------------------
      *          Close the program
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPRMT - Prompt a field                                      * /
      *                                                              * /
      * This routine prompts a field.                                  /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPRMT        BEGSR
      *
      *          ------------------------------------------
      *          Helpdesk?
      *          ------------------------------------------
     C                   SELECT                                                 B01
      *
     C                   WHEN      FMFLDC = 'FMHEDC'                            X01
     C                             AND *IN47 = *OFF
     C                             AND I#ADD = *ON
     C                   EVAL      EROR01 = *ON
     C                   CALLP     OMX105 ( FMHEDC : P#HEDD )
      *
      *          ------------------------------------------
      *          Status route code
      *          ------------------------------------------
     C                   WHEN      FMFLDC = 'FMSTRC'                            X01
     C                             AND *IN41 = *OFF
     C                   EVAL      EROR01 = *ON
     C                   EVAL      P#HEDC = FMHEDC
     C                   CALLP     OMX126 ( P#HEDC : FMSTRC : STRD )
      *
      *          ------------------------------------------
      *          Incident type
      *          ------------------------------------------
     C                   WHEN      FMFLDC = 'FMITPC'                            X01
     C                             AND *IN42 = *OFF
     C                   EVAL      EROR01 = *ON
     C                   EVAL      P#HEDC = FMHEDC
     C                   CALLP     OMX125 ( P#HEDC : FMITPC : ITPD )
      *
      *          ------------------------------------------
      *          Severity 1
      *          ------------------------------------------
     C                   WHEN      FMFLDC = 'FMSEV1'                            X01
     C                             AND *IN43 = *OFF
     C                   EVAL      EROR01 = *ON
     C                   EVAL      P#HEDC = FMHEDC
     C                   EVAL      TABC = '01'
     C                   CALLP     OMX106 ( P#HEDC : TABC : FMSEV1 : DESC )
      *
      *          ------------------------------------------
      *          Severity 2
      *          ------------------------------------------
     C                   WHEN      FMFLDC = 'FMSEV2'                            X01
     C                             AND *IN44 = *OFF
     C                   EVAL      EROR01 = *ON
     C                   EVAL      P#HEDC = FMHEDC
     C                   EVAL      TABC = '02'
     C                   CALLP     OMX106 ( P#HEDC : TABC : FMSEV2 : DESC )
      *
      *          ------------------------------------------
      *          Severity 3
      *          ------------------------------------------
     C                   WHEN      FMFLDC = 'FMSEV3'                            X01
     C                             AND *IN45 = *OFF
     C                   EVAL      EROR01 = *ON
     C                   EVAL      P#HEDC = FMHEDC
     C                   EVAL      TABC = '03'
     C                   CALLP     OMX106 ( P#HEDC : TABC : FMSEV3 : DESC )
      *
      *          ------------------------------------------
      *          User field 1
      *          ------------------------------------------
     C                   WHEN      FMFLDC = 'FMUSF1'                            X01
     C                             AND *IN48 = *OFF
     C                   EVAL      EROR01 = *ON
      *
      *          ------------------------------------------
      *          User field 2
      *          ------------------------------------------
     C                   WHEN      FMFLDC = 'FMUSF2'                            X01
     C                             AND *IN49 = *OFF
     C                   EVAL      EROR01 = *ON
      *
      *          ------------------------------------------
      *          User field 3
      *          ------------------------------------------
     C                   WHEN      FMFLDC = 'FMUSF3'                            X01
     C                             AND *IN50 = *OFF
     C                   EVAL      EROR01 = *ON
      *
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRFYTU - Convert date From YYMMDD format to user format      * /
      *                                                              * /
      * This routine calls a subprogram which converts a date to     * /
      * user format.                                                 * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRFYTU        BEGSR
      *
      *          ------------------------------------------
      *          Convert date when not zero
      *          ------------------------------------------
     C                   IF        D#DTIN <> *ZEROS                             B01
     C                   EVAL      P#INFM = '*YMD'
     C                   EVAL      P#OTFM = '*JOB'
     C                   CALLP     QWCCVTDT ( P#INFM : P#DTIN : P#OTFM :
     C                             P#DTOT : P#ERR )
      *
      *          ------------------------------------------
      *          Zeroes when error
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B02
     C                   EVAL      P#DTOT = '0000000000000000'
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
     C                   EVAL      P#DTOT = '0000000000000000'
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC01 - Screen 1 handling.                                  *
      *                                                              *
      * With this display the incident can be maintained.            *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSC01        BEGSR
      *
      *          ------------------------------------------
      *          Init display 1
      *          ------------------------------------------
     C                   EXSR      SRIN01
     C                   EVAL      EROR01 = *ON                                 (cursor pos)
      *
      *          ------------------------------------------
      *          Do until end of display
      *          ------------------------------------------
     C                   DOU       LOOP01 = '0'                                 B01
      *
      *          ------------------------------------------
      *          Do until no more errors in entry fields
      *          ------------------------------------------
     C                   DOU       EROR01 = '0'                                 B02
      *
      *          ------------------------------------------
      *          Display the messages
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     QUSIIN05
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        I#VIEW <> '1'                                B03
      *
     C                   IF        RRN01 = *ZEROS                               B04
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X04
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E04
      *
     C                   ELSE                                                   X03
      *
     C                   IF        RRN02 = *ZEROS                               B04
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X04
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Display the screen.
      *          ------------------------------------------
     C                   IF        I#VIEW <> '1'                                B03
     C                   EXFMT     QUSIIN02
     C                   ELSE                                                   X03
     C                   EXFMT     QUSIIN04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If fields where changed then force update.
      *          ------------------------------------------
     C                   IF        *IN27 = *ON                                  B03
     C                   EVAL      I#IN27 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Clear the programs message queue.
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Set control fields
      *          ------------------------------------------
     C                   EVAL      EROR01 = *OFF
     C                   EVAL      LOOP01 = *OFF
      *
      *          ------------------------------------------
      *          Do not allow scrolling of subfile
      *          ------------------------------------------
     C                   IF        FMCR01 <> *ZEROS                             B03
     C                   Z-ADD     FMCR01        CRRN01
     C                   ENDIF                                                  E03
      *
     C                   IF        FMCR02 <> *ZEROS                             B03
     C                   Z-ADD     FMCR02        CRRN02
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          F4=Prompt. If EROR01 = '1' upon return then
      *          F4 was used and a window was displayed. We must
      *          assume that a field was changed.
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   EXSR      SRPRMT
     C                   IF        EROR01 = *ON                                 B04
     C                   EVAL      I#IN27 = *ON                                 F4 was valid
     C                   EVAL      *INKD = *OFF
     C                   EVAL      *IN04 = *OFF
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Check if more keys fix
      *          ------------------------------------------
     C                   IF        *INKY = *ON                                  B03
     C                             AND I#F24 = *ON
     C                   ADD       1             CL
     C                   IF        LIN(CL) = *BLANKS                            B04
     C                   EVAL      CL = 1
     C                   ENDIF                                                  E04
      *
      *                    --------------------------------
      *                    Put the fields.
      *                    --------------------------------
     C                   MOVE      LIN(CL)       FMFUND
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F5=is refresh
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B04
     C                             AND I#F5 = *ON
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          F6=Add text  or Add Object
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B05
     C                             AND I#F6 = *ON
      *
     C                   IF        I#VIEW <> '1'                                B06
     C                   EXSR      SRSC02
     C                   ELSE                                                   X06
     C                   EXSR      SRSC03
     C                   ENDIF                                                  E06
      *
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          F7=Show customer
      *          ------------------------------------------
     C                   IF        *INKG = *ON                                  B06
     C                             AND I#F7 = *ON
     C                   EXSR      SRSC04
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X06
      *
      *          ------------------------------------------
      *          F11 = Toggle View
      *          ------------------------------------------
     C                   IF        *INKK = *ON                                  B07
     C                             AND I#F11 = *ON
      *
     C                   IF        I#VIEW <> '1'                                B08
     C                   EVAL      I#VIEW = '1'
     C                   ELSE                                                   X08
     C                   EVAL      I#VIEW = '0'
     C                   ENDIF                                                  E08
      *
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X07
      *
      *          ------------------------------------------
      *          Rollup.
      *          ------------------------------------------
     C                   IF        *IN25 = *ON                                  B08
     C                   IF        FMFMTC = 'QUSIIN02'                          B09
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *ON
     C                   ENDIF                                                  E09
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X08
      *
      *          ------------------------------------------
      *          RollDown.
      *          ------------------------------------------
     C                   IF        *IN26 = *ON                                  B09
     C                   IF        FMFMTC = 'QUSIIN02'                          B10
     C                   EVAL      *IN31 = *ON
     C                   EVAL      *IN32 = *OFF
     C                   ENDIF                                                  E10
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X09
      *
      *          ------------------------------------------
      *          A user function key.
      *          ------------------------------------------
     C                   MOVEA     *IN           INK
     C                   EVAL      K# = 1
     C                   EVAL      K# = %LOOKUP('1':INK:K#)
     C                   IF        K# > 0
     C                   EVAL      *IN99 = *ON
     C                   ELSE
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      K# = 1
     C                   ENDIF
     C                   IF        *IN99 = *ON                                  B10
      *
      *          ------------------------------------------
      *          Not allowed (no entry in FA)
      *          ------------------------------------------
     C                   IF        FA(K#) <> *ON                                B11
     C                   IF        *IN04 = *OFF
     C                   EVAL      MSGID = 'OMQ502D'
     C                   ENDIF
     C                   IF        *IN04 = *ON
     C                   EVAL      MSGID = 'OMQ500D'
     C                   ENDIF
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   ITER
     C                   ELSE                                                   X11
      *
      *          ------------------------------------------
      *          Allowed, but no screen must be processed (in FT)
      *          ------------------------------------------
     C                   IF        FT(K#) = *OFF                                B12
     C                   Z-ADD     K#            I2FKU#
     C                   MOVE      'OMQ5008'     I2MSID
     C                   MOVE      *BLANKS       I2MSGD
     C                   MOVEL     I2KDS#        I2MSGD
     C                   MOVE      'W'           I2MSTP
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E12
     C                   ENDIF                                                  E11
     C                   ENDIF                                                  E10
      *
      *          ------------------------------------------
      *          Edit
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      EROR01 = *OFF
     C                   EXSR      SRED01
     C                   ENDSL
     C                   ENDIF                                                  E09
     C                   ENDIF                                                  E08
     C                   ENDIF                                                  E07
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If errors in display the re-enter error loop
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Process. I#IN27 is used to indicate a change in the
      *          display. Force update if this field is set.
      *          ------------------------------------------
     C                   IF        I#IN27 = *ON                                 B02
     C                   EVAL      *IN27 = *ON
     C                   ENDIF                                                  E02
     C                   EXSR      SRPR01
      *
      *          ------------------------------------------
      *          End screen loop
      *          ------------------------------------------
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRIN01 - Screen 1 init.                                      *
      *                                                              *
      * This routine initialises the first screen. This is only      *
      * done if the re-display option was not set or the program was *
      * not yet called before.                                       *
      *                                                             *
      * ------------------------------------------------------------ *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Reset the action options.
      *          ------------------------------------------
     C                   EVAL      I#ADD = *OFF
     C                   EVAL      I#CHG = *OFF
     C                   EVAL      I#DSP = *OFF
     C                   EVAL      I#CPY = *OFF
     C                   EVAL      I#DEL = *OFF
      *
      *          ------------------------------------------
      *          The first screen.
      *          ------------------------------------------
     C                   EVAL      *IN31 = *ON                                  Screen 1
     C                   EVAL      *IN32 = *OFF                                 Screen 2
      *
      *          ------------------------------------------
      *          Determine the action.
      *          ------------------------------------------
     C                   IF        I2ACTC = '20'                                B01
     C                   EVAL      I#CHG = *ON
     C                   MOVEL     TXT(1)        FMPG1D
     C                   ELSE                                                   X01
      *
     C                   IF        I2ACTC = '30'                                B02
     C                   EVAL      I#CPY = *ON
     C                   MOVEL     TXT(2)        FMPG1D
     C                   ELSE                                                   X02
      *
     C                   IF        I2ACTC = '40'                                B03
     C                   EVAL      I#DEL = *ON
     C                   MOVEL     TXT(5)        FMPG1D
     C                   ELSE                                                   X03
      *
     C                   IF        I2ACTC = '50'                                B04
     C                   EVAL      I#DSP = *ON
     C                   MOVEL     TXT(4)        FMPG1D
     C                   ELSE                                                   X04
      *
     C                   IF        I2ACTC = '60'                                B05
     C                             OR I2ACTC = '61'                             B05
     C                   EVAL      I#ADD = *ON
     C                   MOVEL     TXT(3)        FMPG1D
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do the refresh
      *          ------------------------------------------
     C                   EXSR      SRRF01
      *
      *          ------------------------------------------
      *          The program is open.
      *          ------------------------------------------
     C                   EVAL      I#OPEN = *ON
      *
      *          ------------------------------------------
      *          If delete, then display the correct text.
      *          ------------------------------------------
     C                   IF        I2ACTC = '40'                                B01
     C                   EVAL      MSGID = 'OMQ5005'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Some fields wear protection.
      *          ------------------------------------------
     C                   IF        I#DSP = *ON                                  B01
     C                             OR I#DEL = *ON
     C                   MOVE      *ALL'1'       W#20A
     C                   MOVEA     W#20A         *IN(40)
     C                   ELSE                                                   X01
     C                   IF        I#CHG <> *ON                                 B02
     C                   MOVE      *ALL'0'       W#20A
     C                   MOVEA     W#20A         *IN(40)
     C                   MOVEL     I2PHPD        *IN(47)
     C                   ELSE                                                   X02
     C                   MOVE      *ALL'0'       W#20A
     C                   EVAL      %SUBST(W#20A:1:2) = '11'
     C                   MOVEA     W#20A         *IN(40)
     C                   EVAL      *IN(47) = *ON
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRF01 - Screen 1 refresh.                                   *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRF01        BEGSR
      *
      *          ------------------------------------------
      *          Prefill with database values if not add.
      *          ------------------------------------------
     C                   IF        I#ADD <> *ON                                 B01
     C                   CLEAR                   I4BUF
     C                   MOVE      I2KHED        NCHEDC
     C                   MOVE      I2KINC        NCINC#
     C     $NC1K1        CHAIN(N)  OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Update the screen buffer from the record.
      *          Then update the screen buffer from the user input.
      *          ------------------------------------------
     C                   IF        I2INTL = 'V3R0M0'                            B02
     C                   EXSR      SRR2F1
     C                   EXSR      SRI2F1
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If change then get the authorisation code
      *          ------------------------------------------
     C                   IF        I#CHG = *ON                                  B01
     C                   MOVE      FMHEDC        P#HEDC
     C                   EVAL      P#USID = D#USID
     C                   EXSR      SRSUL#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          First clear it from the record and then from the
      *          user input.
      *          ------------------------------------------
     C                   IF        I#ADD = *ON                                  B01
     C                   IF        I2INTL = 'V3R0M0'                            B02
     C                   CLEAR                   I4BUF
     C                   EXSR      SRR2F1
     C                   EXSR      SRI2F1
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Add text if required and no addition.
      *          ------------------------------------------
     C                   IF        I2ATXT = *ON                                 B02
     C                   EVAL      *INKF = *ON
     C                   EXSR      SRSC02
     C                   MOVE      *OFF          I2ATXT
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the text subfile.
      *          ------------------------------------------
     C                   EXSR      SRPGTX
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPGTX - Fill a page with text block                         *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPGTX        BEGSR
      *
      *          ------------------------------------------
      *          Open the file
      *          ------------------------------------------
     C                   IF        I#IHL1 <> *ON                                B01
     C                   OPEN      OMTIHL1
     C                   EVAL      I#IHL1 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Set the key.
      *          ------------------------------------------
     C     $IH1K1        SETLL     OMTIHL1
      *
      *          ------------------------------------------
      *          Initialize the subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     QUSIIN02
     C                   EVAL      *IN91 = *OFF
     C                   EVAL      RRN01 = *ZEROS
     C                   Z-ADD     1             CRRN01
      *
      *          ------------------------------------------
      *          Read all records
      *          ------------------------------------------
     C                   IF        I#CHG = *ON                                  B01
     C                             OR I#DEL = *ON
     C                             OR I#DSP = *ON
     C     $IH1K1        READE     OMTIHL1
     C                   EVAL      *IN99 = %EOF
      *
      *                    --------------------------------
      *                    If this is the first record and the
      *                    short incident description is *NONE
      *                    then this header line is copied into
      *                    the incident description.
      *                    --------------------------------
     C                   IF        *IN99 = *OFF                                 B02
     C                             AND FMSHID = '*NONE'
     C                             AND FMSTAT <> '*CMP'
     C                   MOVEL(P)  IHSITD        FMSHID
     C                   EVAL      I#SHID = *ON
     C                   IF        P2ACTC = '61'                                B03
     C                   EVAL      P2ACTC = '60'
     C                   EVAL      MSGID = 'OMQA022'
     C                   ELSE                                                   X03
     C                   EVAL      MSGID = 'OMQA018'
     C                   ENDIF                                                  E03
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E02
      *
     C                   DOW       *IN99 = *OFF                                 B02
     C                   ADD       1             RRN01
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      IHITH#        F2ITH#
     C                   MOVE      IHSUL#        F2SUL#
     C                   MOVE      IHUSID        F2USID
     C                   MOVE      IHITXT        F2ITXT
      *
     C                   Z-ADD     IHINDT        D#IDAT
     C                   EVAL      D#TMIN = *ZEROS
     C                   EVAL      D#MSIN = *ZEROS
     C                   EXSR      SRFYTU
     C                   Z-ADD     D#DTOT        F2INDT
      *
     C                   MOVEL     IHSITD        F2SITD
     C                   WRITE     QUSIIN01
      *
      *          ------------------------------------------
      *          Read the next record.
      *          ------------------------------------------
     C     $IH1K1        READE     OMTIHL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Leave if the maximum number of records in subfile 1
      *          was reached.
      *          ------------------------------------------
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If end of file then set the SFLEND indicator
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B01
     C                   EVAL      *IN92 = *OFF
     C                   ELSE                                                   X01
     C                   EVAL      *IN92 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRED01 - Check screen 1 input                                *
      *                                                              *
      * This routine checks the screen 1 fields.                     *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          Do not check for a display only action.
      *          ------------------------------------------
     C                   IF        I#DSP <> *ON                                 B01
      *
      *          ------------------------------------------
      *          Only check if add or fields where changed.
      *          ------------------------------------------
     C                   IF        I#ADD = *ON                                  B02
     C                             OR I#IN27 = *ON
     C                             OR I#SHID = *ON
      *
      *          ------------------------------------------
      *          Reset error fields (do 'm all)
      *          ------------------------------------------
     C                   EVAL      *IN(60) = *OFF
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
     C                   EVAL      *IN(79) = *OFF
      *
      *          ------------------------------------------
      *          Check blank values.
      *          ------------------------------------------
     C                   EVAL      *IN99 = *ON
      *
     C                   IF        FMHEDC = *BLANKS                             B03
     C                             AND PRA(4) = *OFF
     C                   EVAL      I#HEDC = *ON
     C                   EVAL      *IN99 = (I#HEDC = *BLANKS)
     C                   ENDIF                                                  E03
      *
     C                   IF        FMINC# = *BLANKS                             B03
     C                   EVAL      *IN60 = *ON
     C                   EVAL      *IN99 = (*IN60 = *BLANKS)
     C                   EVAL      *IN61 = *ON
     C                   EVAL      *IN99 = (*IN61 = *BLANKS)
     C                   ENDIF                                                  E03
      *
     C                   IF        *IN99 = *OFF                                 B03
     C                   EVAL      EROR01 = *ON
     C                   EVAL      MSGID = 'OMQ6016'
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          If add and the application can be changed then
      *          check if the user is authorized to the existing Desk.
      *          ------------------------------------------
     C                   IF        I#ADD = *ON                                  B04
     C                             OR I#CPY = *ON
      *
     C                   IF        PRA(4) <> *ON                                B05
      *
     C     FMHEDC        SETLL     OMHDDL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B06
     C                   EVAL      MSGID = 'OMQ6001'
     C                   MOVEL     FMHEDC        MSGDTA
     C                   EVAL      I#HEDC = *ON
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X06
      *
     C                   MOVE      FMHEDC        P#HEDC
     C                   EVAL      P#USID = D#USID
     C                   EXSR      SRSUL#
      *
     C                   IF        P#SUL# < I4SUL#                              B07
     C                   EVAL      MSGID = 'OMQ5001'
     C                   MOVEL     FMHEDC        MSGDTA
     C                   EVAL      *IN67 = *ON
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X07
     C                   MOVE      P#SUL#        FMSUL#
     C                   ENDIF                                                  E07
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If no errors, then check the other input
      *          ------------------------------------------
     C                   IF        EROR01 = *OFF                                B04
      *
      *          ------------------------------------------
      *          Update the record from the screen buffer
      *          ------------------------------------------
     C                   IF        I2INTL = 'V3R0M0'                            B05
     C                   EXSR      SRF2R1
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          Fill the action code.
      *          ------------------------------------------
     C                   IF        I#ADD = *ON                                  B05
     C                   MOVE      '60'          I4ACTC
     C                   ELSE                                                   X05
     C                   IF        I#DEL = *ON                                  B06
     C                   MOVE      '40'          I4ACTC
     C                   ELSE                                                   X06
     C                   IF        I#CHG = *ON                                  B07
     C                   MOVE      '20'          I4ACTC
     C                   ENDIF                                                  E07
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          Call the change API to check the input
      *          ------------------------------------------
     C                   MOVE      '0'           I4UPD
     C                   MOVE      '0'           I4UPB
     C                   MOVE      PRA(1)        I4EOP
      *
     C                   CALLP     OMQCHGIN ( I4INTL : I4ACTC : I4BUF :
     C                             I4PRC : I4ERR : I4FLD )
      *
      *          ------------------------------------------
      *          Fill field error buffer from Parameter
      *          ------------------------------------------
     C                   MOVEA     I4FLDA        FLD
      *
      *          ------------------------------------------
      *          Seton error indicator if errors.
      *          ------------------------------------------
     C                   IF        FLD(1) <> *BLANKS                            B05
     C                             OR I4MSTP = 'E'
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Seton the fields in error.
      *          ------------------------------------------
     C                   EVAL      IX = 1
     C                   DOW       FLD(IX) <> *BLANKS                           B04
     C                   EVAL      IY = 1
     C                   EVAL      IY = %LOOKUP(FLD(IX):FL:IY)
     C                   IF        IY > 0
     C                   EVAL      *IN99 = *ON
     C                   ELSE
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      IY = 1
     C                   ENDIF
     C                   IF        *IN99 = *ON
     C                   EVAL      IY = FP(IY)
     C                   EVAL      *IN(IY) = *ON
     C                   ENDIF
     C                   ADD       1             IX
     C                   ENDDO                                                  E04
      *
      *          ------------------------------------------
      *          Seton error indicator if errors.
      *          ------------------------------------------
     C                   IF        FLD(1) <> *BLANKS                            B04
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check the subfile options.
      *          ------------------------------------------
     C                   IF        RRN01 > *ZEROS                               B02
     C                             AND I#VIEW <> '1'
      *
      *                    --------------------------------
      *                    Get the current date
      *                    --------------------------------
     C                   EVAL      D#DD = UDAY
     C                   EVAL      D#YY = UYEAR
     C                   EVAL      D#MM = UMONTH
     C                   EXSR      SRFYTU
      *
      *                    --------------------------------
      *                    Read the subfile
      *                    --------------------------------
     C                   Z-ADD     CRRN01        S#CR01
     C                   EVAL      CRRN01 = *ZEROS
     C                   EVAL      S#RR01 = RRN01
     C                   EVAL      I#502E = *OFF
     C                   DOW       1 = 1                                        B03
     C                   READC(E)  QUSIIN01
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   IF        *IN99 = *ON
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOOP01 = *ON
      *
      *                    --------------------------------
      *                    Send error if not the right option
      *                    --------------------------------
     C                   IF        FMOPTI <> '5'                                B04
     C                             AND FMOPTI <> '2'
     C                             AND FMOPTI <> *BLANKS
     C                   EVAL      *IN70 = *ON
     C                   IF        I#502E <> *ON                                B05
     C                   EVAL      MSGID = 'OMQ502E'
     C                   EXSR      SNDMSG
     C                   EVAL      I#502E = *ON
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
      *                    --------------------------------
      *                    If option '2' and not this user and
      *                    this date then error.
      *                    --------------------------------
     C                   IF        FMOPTI = '2'                                 B04
     C                             AND F2INDT <> D#DTOT
     C                   EVAL      *IN70 = *ON
     C                   EVAL      MSGID = 'OMQ5032'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E04
      *
      *                    --------------------------------
      *                    Update the subfile
      *                    --------------------------------
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    QUSIIN01
      *
     C                   IF        *IN70 = *ON                                  B04
     C                   EVAL      EROR01 = *IN70
     C                   IF        CRRN01 = *ZEROS                              B05
     C                   Z-ADD     RRN01         CRRN01
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
     C                   EVAL      *IN70 = *OFF
     C                   EVAL      *IN93 = *OFF
     C                   ENDDO                                                  E03
      *
     C                   EVAL      RRN01 = S#RR01
     C                   IF        CRRN01 = *ZEROS                              B03
     C                   Z-ADD     S#CR01        CRRN01
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check the subfile options.
      *          ------------------------------------------
     C                   IF        RRN02 > *ZEROS                               B02
     C                             AND I#VIEW = '1'
      *
      *                    --------------------------------
      *                    Read the subfile
      *                    --------------------------------
     C                   Z-ADD     CRRN02        S#CR02
     C                   EVAL      CRRN02 = *ZEROS
     C                   EVAL      S#RR02 = RRN02
     C                   EVAL      I#502E = *OFF
     C                   DOW       1 = 1                                        B03
     C                   READC(E)  QUSIIN03
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   IF        *IN99 = *ON
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOOP01 = *ON
      *
      *                    --------------------------------
      *                    Send error if not the right option
      *                    --------------------------------
     C                   IF        FMOPTI <> '5'                                B04
     C                             AND FMOPTI <> '4'                            B04
     C                             AND FMOPTI <> *BLANKS
     C                   EVAL      *IN70 = *ON
     C                   IF        I#502E <> *ON                                B05
     C                   EVAL      MSGID = 'OMQ502E'
     C                   EXSR      SNDMSG
     C                   EVAL      I#502E = *ON
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
      *                    --------------------------------
      *                    Update the subfile
      *                    --------------------------------
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    QUSIIN03
      *
     C                   IF        *IN70 = *ON                                  B04
     C                   EVAL      EROR02 = *IN70
     C                   IF        CRRN02 = *ZEROS                              B05
     C                   Z-ADD     RRN02         CRRN02
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
     C                   EVAL      *IN70 = *OFF
     C                   EVAL      *IN93 = *OFF
     C                   ENDDO                                                  E03
      *
     C                   EVAL      RRN02 = S#RR02
     C                   IF        CRRN02 = *ZEROS                              B03
     C                   Z-ADD     S#CR02        CRRN02
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPR01 - Screen 1 processing.                                *
      *                                                              *
      * This routine calls the change fix API to do the desired      *
      * action.                                                      *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          Fill the processing options.
      *          ------------------------------------------
     C                   MOVE      '1'           I4UPD
     C                   MOVE      '1'           I4UPB
      *
      *          ------------------------------------------
      *          If addition.
      *          ------------------------------------------
     C                   IF        I#ADD = *ON                                  B01
     C                             OR I#CPY = *ON
      *
      *                    --------------------------------
      *                    Delay propagation
      *                    --------------------------------
     C                   MOVE      *ON           I4PDI
      *
      *          ------------------------------------------
      *          Call OMQCHGIN.
      *          ------------------------------------------
     C                   EVAL      I4ACTC = '60'
     C                   CALLP     OMQCHGIN ( I4INTL : I4ACTC : I4BUF :
     C                             I4PRC : I4ERR : I4FLD )
      *
      *                    --------------------------------
      *                    Do not delay propagation
      *                    --------------------------------
     C                   MOVE      *OFF          I4PDI
      *
      *          ------------------------------------------
      *          Loop again
      *          ------------------------------------------
     C                   EVAL      LOOP01 = *ON
      *
      *          ------------------------------------------
      *          Update the screen buffer from the fix record.
      *          ------------------------------------------
     C                   IF        I2INTL = 'V3R0M0'                            B02
     C                   EXSR      SRR2F1
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Update the data and the key buffer.
      *          ------------------------------------------
     C                   IF        I2INTL = 'V3R0M0'                            B02
     C                   MOVEL     I4BUF         I2INC
     C                   MOVE      I2HEDC        I2KHED
     C                   MOVE      I2INC#        I2KINC
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Enter change mode.
      *          ------------------------------------------
     C                   EVAL      I#CPY = *OFF
     C                   EVAL      I#ADD = *OFF
     C                   EVAL      I#CHG = *ON
     C                   EVAL      I2ACTC = '20'
     C                   MOVEL     TXT(1)        FMPG1D
      *
      *          ------------------------------------------
      *          Send added message
      *          ------------------------------------------
     C                   FOR       MA_X = 1 TO %ELEM(MC)
     C                   EVAL      MC(MA_X) = *BLANK
     C                   ENDFOR
     C                   MOVEA     FMINC#        MC(1)
     C                   MOVEA     FMHEDC        MC(11)
     C                   MOVEA     D#USID        MC(16)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'OMQA005'
     C                   EXSR      SNDMSG
      *
      *          ------------------------------------------
      *          Get the new function keys.
      *          ------------------------------------------
     C                   EXSR      SRFUNK
      *
      *          ------------------------------------------
      *          Else (no addition or copy)
      *          ------------------------------------------
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          If action is change
      *          ------------------------------------------
     C                   IF        I#CHG = *ON                                  B02
      *
      *          ------------------------------------------
      *          If fields were changed or text was changed
      *          ------------------------------------------
     C                   IF        I#IN27 = *ON                                 B03
     C                             OR I#SHID = *ON
      *
      *          ------------------------------------------
      *          Force loop only if fields were changed
      *          ------------------------------------------
     C                   IF        I#IN27 = *ON                                 B04
     C                   EVAL      LOOP01 = *ON
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Reset change indicators
      *          ------------------------------------------
     C                   EVAL      I#IN27 = *OFF
     C                   EVAL      I#SHID = *OFF
      *
      *          ------------------------------------------
      *          Call OMQCHGIN.
      *          ------------------------------------------
     C                   EVAL      I4ACTC = '20'
     C                   CALLP     OMQCHGIN ( I4INTL : I4ACTC : I4BUF :
     C                             I4PRC : I4ERR : I4FLD )
      *
      *          ------------------------------------------
      *          Fill the fix buffer
      *          ------------------------------------------
     C                   IF        I2INTL = 'V3R0M0'                            B04
     C                   MOVEL     I4BUF         I2INC
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Update the screen buffer from the fix record.
      *          ------------------------------------------
     C                   IF        I2INTL = 'V3R0M0'                            B04
     C                   EXSR      SRR2F1
     C                   ENDIF                                                  E04
      *
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If this is delete.
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   IF        I#DEL = *ON                                  B03
      *
      *          ------------------------------------------
      *          Call OMQCHGIN.
      *          ------------------------------------------
     C                   EVAL      I4ACTC = '40'
     C                   CALLP     OMQCHGIN ( I4INTL : I4ACTC : I4BUF :
     C                             I4PRC : I4ERR : I4FLD )
      *
      *          ------------------------------------------
      *          If error during processing force screen loop.
      *          ------------------------------------------
     C                   IF        I4MSTP = 'E'                                 B04
     C                   EVAL      LOOP01 = *ON
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Fill the fix buffer
      *          ------------------------------------------
     C                   IF        I2INTL = 'V3R0M0'                            B04
     C                   MOVEL     I4BUF         I2INC
     C                   ENDIF                                                  E04
      *
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          A user function key, exit.
      *          ------------------------------------------
     C                   IF        K# > 1                                       B01
     C                   Z-ADD     K#            I2FKU#
     C                   MOVE      'OMQ5010'     I2MSID
     C                   MOVE      *BLANKS       I2MSGD
     C                   MOVEL     I2KDS#        I2MSGD
     C                   MOVE      'W'           I2MSTP
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check the text if change or display.
      *          ------------------------------------------
     C                   IF        I#CHG = *ON                                  B01
     C                             OR I#DEL = *ON
     C                             OR I#DSP = *ON
      *
     C                   IF        I#VIEW <> '1'                                B02
     C                   IF        RRN01 > 0                                    B03
     C                   EVAL      S#RR01 = RRN01
     C                   DOW       1 = 1                                        B04
     C                   READC(E)  QUSIIN01
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   IF        *IN99 = *ON
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOOP01 = *ON
     C                   EVAL      I#WRT = *OFF
     C                   SELECT
     C                   WHEN      FMOPTI <> *BLANKS
     C                   EXSR      SRSC02
     C                   ENDSL
     C                   EVAL      *IN93 = *OFF
     C                   MOVE      *BLANKS       FMOPTI
     C                   IF        I#WRT = *OFF
     C                   UPDATE    QUSIIN01
     C                   ELSE
     C                   EXSR      SRRF01
     C                   ENDIF
     C                   IF        P#STAT = C#CNCL                              B05
     C                   LEAVE
     C                   ENDIF                                                  E05
     C                   ENDDO                                                  E04
     C                   EVAL      RRN01 = S#RR01
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check the item if view or remove
      *          ------------------------------------------
     C                   IF        I#VIEW = '1'                                 B02
     C                   IF        RRN02 > 0                                    B03
     C                   EVAL      S#RR02 = RRN02
     C                   DOW       1 = 1                                        B04
     C                   READC(E)  QUSIIN03
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   IF        *IN99 = *ON
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOOP01 = *ON
     C                   SELECT
     C                   WHEN      FMOPTI <> *BLANKS
     C                   EXSR      SRSC03
     C                   ENDSL
     C                   EVAL      *IN93 = *OFF
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    QUSIIN03
      *
     C                   IF        P#STAT = C#CNCL                              B05
     C                   LEAVE
     C                   ENDIF                                                  E05
      *
     C                   IF        P#STAT = C#EXIT                              B05
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E05
      *
     C                   ENDDO                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC02 - Text processing                                     *
      *                                                              *
      * This routine calls the text API to add or display incident   *
      * text.                                                        *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSC02        BEGSR
      *
      *          ------------------------------------------
      *          Fill the processing options.
      *          ------------------------------------------
     C                   Z-ADD     F2ITH#        P#ITH#
     C                   EVAL      P#STAT = C#NORM
      *
      *          ------------------------------------------
      *          Fill the processing options.
      *          ------------------------------------------
     C                   IF        *INKF = *ON
     C                   EVAL      P2ACTC = '60'
     C                   ENDIF
      *
     C                   IF        *INKF = *OFF                                 B01
     C                   IF        FMOPTI = '5'                                 B02
     C                   EVAL      P2ACTC = '50'
     C                   ELSE                                                   X02
     C                   EVAL      P2ACTC = '55'
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
     C                   CALL      'OMH023'      $MH023                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#H023 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#H023 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If add then add a record to the subfile if
      *          this page is not yet full.
      *          ------------------------------------------
      *          RRN01     IFLT 5                          B01
     C                   IF        P2ACTC = '60'                                B01
     C                             OR P2ACTC = '61'                             B01
     C                   IF        I#IHL1 <> *ON                                B02
     C                   OPEN      OMTIHL1
     C                   EVAL      I#IHL1 = *ON
     C                   ENDIF                                                  E02
     C     $IH1K2        CHAIN     OMTIHL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *                    --------------------------------
      *                    If this is the first record and the
      *                    short incident description is *NONE
      *                    then this header line is copied into
      *                    the incident description.
      *                    --------------------------------
     C                   IF        *IN99 = *OFF                                 B02
     C                             AND FMSHID = '*NONE'
     C                             AND RRN01 = *ZEROS
     C                             AND FMSTAT <> '*CMP'
     C                   MOVEL(P)  IHSITD        FMSHID
     C                   EVAL      I#SHID = *ON
      *
      *                    --------------------------------
      *                    Also Update the short incident desc.
      *                    for the Incident Maintenance screen,
      *                    this prevents a '*NONE' text description
      *                    on the IM screen after pressing F12.
      *                    --------------------------------
     C     $NC1K1        CHAIN     OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL(P)  IHSITD        NCSHID                           AJU
     C                   UPDATE    OMINCR
      *
     C                   IF        P2ACTC = '61'                                B03
     C                   EVAL      P2ACTC = '60'
     C                   EVAL      MSGID = 'OMQA022'
     C                   ELSE                                                   X03
     C                   EVAL      MSGID = 'OMQA018'
     C                   ENDIF                                                  E03
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Add the record.
      *                    --------------------------------
     C                   IF        *IN99 = *OFF                                 B02
     C                   ADD       1             RRN01
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      IHITH#        F2ITH#
     C                   MOVE      IHSUL#        F2SUL#
     C                   MOVE      IHUSID        F2USID
     C                   MOVE      IHITXT        F2ITXT
      *
     C                   Z-ADD     IHINDT        D#IDAT
     C                   EVAL      D#TMIN = *ZEROS
     C                   EVAL      D#MSIN = *ZEROS
     C                   EXSR      SRFYTU
     C                   Z-ADD     D#DTOT        F2INDT
      *
     C                   MOVEL     IHSITD        F2SITD
     C                   WRITE     QUSIIN01
     C                   EVAL      I#WRT = *ON
     C                   Z-ADD     RRN01         CRRN01
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC03 - Item processing                                     *
      *                                                              *
      * This routine calls the item API to add or display incident   *
      * items.                                                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSC03        BEGSR
      *
      *          ------------------------------------------
      *          Fill the processing options.
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
      *
      *          ------------------------------------------
      *          Fill the processing options.
      *          ------------------------------------------
     C                   IF        *INKF = *ON
     C                   MOVE      *BLANKS       FMOPTI
     C                   EVAL      P2ACTC = '60'
     C                   ENDIF
      *
     C                   IF        FMOPTI = '5'                                 B01
     C                   EVAL      P2ACTC = '50'
     C                   ENDIF                                                  E01
      *
     C                   IF        FMOPTI = '4'                                 B01
     C                   EVAL      P2ACTC = '40'
     C                   ENDIF                                                  E01
      *
     C                   IF        *INKF = *OFF                                 B01
     C                   IF        FMOPTI = '5'                                 B02
     C                   EVAL      P2ACTC = '50'
      *
     C                   EVAL      P2INTL = 'V3R0M0'
     C                   CALLP     OMQUSICI ( P2INTL : P2ACTC : FMOTID :
     C                             FMOCID : FMSNBR : P#STAT )
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If add then call the item selection program.
      *          ------------------------------------------
     C                   IF        P2ACTC = '60'                                B01
     C                   EVAL      P#ACTC = '60'
     C                   CALLP     OMH054C ( P#ACTC : FMHEDC : FMCUSC :
     C                             FMCCTC : FMINC# : FMOTID : FMOCID :
     C                             FMSNBR : P#STAT )
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If remove then call the item selection program.
      *          ------------------------------------------
     C                   IF        P2ACTC = '40'                                B01
     C                   EVAL      P#ACTC = '40'
     C                   CALLP     OMH054C ( P#ACTC : FMHEDC : FMCUSC :
     C                             FMCCTC : FMINC# : FMOTID : FMOCID :
     C                             FMSNBR : P#STAT )
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC04 - Show customer                                       *
      *                                                              *
      * This routine calls the show customer window                  *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSC04        BEGSR
      *
      *          ------------------------------------------
      *          Call
      *          ------------------------------------------
     C                   EVAL      P#CUSC = FMCUSC
     C                   EVAL      P#CCTC = FMCCTC
     C                   CALLP     OMH024 ( P#CUSC : P#CCTC : P#STAT )
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRF2R1 - Update fields from Format to Record (V3R0M0)        *
      *                                                              *
      * This routine updates for V3R0M0 the fix record from          *
      * the display fields. The fields that can be changed.          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRF2R1        BEGSR
      *
      *          ------------------------------------------
      *          Do it.
      *          ------------------------------------------
     C                   MOVE      FMHEDC        I4HEDC
     C                   MOVE      FMINC#        I4INC#
     C                   MOVE      FMCTN#        I4CTN#
      ***                  MOVE FMAIN#    I4AIN#
      ***                  MOVE FMINDT    I4INDT
      ***                  MOVE FMINTM    I4INTM
      ***                  MOVE FMRCDT    I4RCDT
      ***                  MOVE FMRCTM    I4RCTM
     C                   MOVE      FMSTRC        I4STRC
      ***                  MOVE FMRES#    I4RES#
     C                   MOVE      FMSUL#        I4SUL#
      ***                  MOVE FMSTAT    I4STAT
      ***                  MOVE FMSRDT    I4SRDT
      ***                  MOVE FMSRTM    I4SRTM
     C                   MOVE      FMSEV1        I4SEV1
     C                   MOVE      FMSEV2        I4SEV2
     C                   MOVE      FMSEV3        I4SEV3
      ***                  MOVE FMPRI#    I4PRI#
      ***                  MOVE FMESC#    I4ESC#
      ***                  MOVE FMRUID    I4RUID
      ***                  MOVE FMAUID    I4AUID
     C                   MOVE      FMITPC        I4ITPC
      ***                  MOVE FMCFL#    I4CFL#
      ***                  MOVE FMCFIC    I4CFIC
     C                   MOVE      FMUSF1        I4USF1
     C                   MOVE      FMUSF2        I4USF2
     C                   MOVE      FMUSF3        I4USF3
     C                   MOVE      FMSHID        I4SHID
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRR2F1 - Update fields from Record to Format (V3R0M0)        *
      *                                                              *
      * This routine updates for V3R0M0 the display fields from the  *
      * record fields.                                               *
      *                                                              *
      * Because of field remapping, the field values are not in the  *
      * record format fields but in the I4 fields instead.           *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRR2F1        BEGSR
      *
      *          ------------------------------------------
      *          Get the call track number for the customer
      *          ------------------------------------------
     C                   MOVE      I4HEDC        CAHEDC
     C                   MOVE      I4CTN#        CACTN#
     C     $CA1K1        CHAIN     OMCCAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B01
     C                   MOVE      CACUSC        FMCUSC
     C                   MOVE      CACCTC        FMCCTC
      *
      *                    --------------------------------
      *                    Get the phone number
      *                    --------------------------------
     C     $CO1K1        CHAIN     OMCCOL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF
     C                   MOVE      COTEL#        FMTEL#
     C                   ENDIF
     C                   IF        *IN99 = *ON
     C                   MOVE      *ALL'?*'      FMTEL#
     C                   ENDIF
     C                   IF        COEMAC <> *BLANKS                            B02
     C     $CO1K1        CHAIN     OMMPXL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        PXMTDI <> '9'                                B03
     C                   MOVEL     C#EMAI        FMEMAI
     C                   ELSE                                                   X03
     C                   MOVE      *BLANKS       FMEMAI
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the incident fields  (the ones used in the dsp)
      *          ------------------------------------------
     C                   MOVE      I4HEDC        FMHEDC
     C                   MOVE      I4INC#        FMINC#
     C                   MOVE      I4CTN#        FMCTN#
     C                   MOVEL     I4AIN#        FMAIN2
     C                   IF        I4AIN# = *BLANKS                             B01
     C                   MOVEL     '*NONE'       FMAIN2
     C                   ENDIF                                                  E01
      ***                  MOVE I4AIN#    FMAIN#
      *
     C                   Z-ADD     I4INDT        D#IDAT
     C                   EVAL      D#TMIN = *ZEROS
     C                   EVAL      D#MSIN = *ZEROS
     C                   EXSR      SRFYTU
     C                   Z-ADD     D#DTOT        FMINDT
      *
      ***                  MOVE I4INTM    FMINTM
      ***                  MOVE I4RCDT    FMRCDT
      ***                  MOVE I4RCTM    FMRCTM
     C                   MOVE      I4STRC        FMSTRC
      ***                  MOVE I4RES#    FMRES#
     C                   MOVE      I4SUL#        FMSUL#
     C                   MOVE      I4STAT        FMSTAT
      ***                  MOVE I4SRDT    FMSRDT
      ***                  MOVE I4SRTM    FMSRTM
     C                   MOVE      I4SEV1        FMSEV1
     C                   MOVE      I4SEV2        FMSEV2
     C                   MOVE      I4SEV3        FMSEV3
     C                   MOVE      I4PRI#        FMPRI#
      ***                  MOVE I4ESC#    FMESC#
     C                   MOVEL     I4RUID        FMRUI2
      ***                  MOVE I4RUID    FMRUID
      ***                  MOVE I4AUID    FMAUID
     C                   MOVE      I4ITPC        FMITPC
      ***                  MOVE I4CFL#    FMCFL#
      ***                  MOVE I4CFIC    FMCFIC
     C                   MOVE      I4USF1        FMUSF1
     C                   MOVE      I4USF2        FMUSF2
     C                   MOVE      I4USF3        FMUSF3
     C                   MOVE      I4SHID        FMSHID
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRI2F1 - Update fields from Input to format  (V3R0M0)        *
      *                                                              *
      * This routine updates for V3R0M0 the display fields from the  *
      * user input.                                                  *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRI2F1        BEGSR
      *
      *          ------------------------------------------
      *          Do not do this if the redisplay option is
      *          set and the program is already open.
      *          ------------------------------------------
     C                   IF        I#OPEN <> *ON                                B01
     C                             OR I#OPEN = *ON
     C                             AND PRA(2) = *OFF
      *
      *          ------------------------------------------
      *          Get the call track number for the customer
      *          ------------------------------------------
     C                   IF        I2HEDC <> *BLANKS                            B02
     C                             AND I2CTN# <> *BLANKS
     C                   MOVE      I2HEDC        CAHEDC
     C                   MOVE      I2CTN#        CACTN#
     C     $CA1K1        CHAIN     OMCCAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B03
     C                   MOVE      CACUSC        FMCUSC
     C                   MOVE      CACCTC        FMCCTC
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          If buffer prefill requested
      *          ------------------------------------------
     C                   IF        PRA(3) = '1'                                 B02
      *
      *          ------------------------------------------
      *          Force update when fields passed.
      *          ------------------------------------------
     C                   EVAL      I#IN27 = *ON
      *
      *          ------------------------------------------
      *          If ADD/COPY then keyfields can also be prefilled.
      *          ------------------------------------------
     C                   IF        I#ADD = *ON                                  B03
     C                             OR I#CPY = *ON
     C                   MOVE      I2HEDC        FMHEDC
     C                   MOVE      I2INC#        FMINC#
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Fill the other fields.
      *          ------------------------------------------
     C                   MOVE      I2CTN#        FMCTN#
     C                   MOVE      I2STRC        FMSTRC
     C                   MOVE      I2SEV1        FMSEV1
     C                   MOVE      I2SEV2        FMSEV2
     C                   MOVE      I2SEV3        FMSEV3
     C                   MOVE      I2PRI#        FMPRI#
     C                   MOVE      I2ITPC        FMITPC
     C                   MOVE      I2USF1        FMUSF1
     C                   MOVE      I2USF2        FMUSF2
     C                   MOVE      I2USF3        FMUSF3
     C                   MOVE      I2SHID        FMSHID
      *
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * CLRMSG - Clear message subfile                               * /
      *                                                              * /
      * This routine calls a subprogram wich clears the message      * /
      * subfile.                                                     * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     CLRMSG        BEGSR
      *
      *          ------------------------------------------
      *          Call message queue clearing program
      *          ------------------------------------------
     C                   CALLP     OMH902C ( PGMQ )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRCVTD - Convert date and time format                        *
      *                                                              *
      * The convert date and time format API (QWCCVTDT) converts     *
      * date and time values from one format to another. It can      *
      * also be used to retrieve the current machine clock time and  *
      * return it in a specific format.                              *
      *                                                              *
      * Input      - P#FMIC  Input format code                       *
      *              P#IB    Input buffer                            *
      *              P#FMOC  Output format code                      *
      *                                                              *
      * Output     - P#BUF   Output buffer                           *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRCVTD        BEGSR
      *
      *          ------------------------------------------
      *          Convert Date and Time Format
      *          ------------------------------------------
     C                   CALLP     QWCCVTDT ( P#FMIC : P#IB : P#FMOC :
     C                             P#BUF : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C  ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRMEXT - More extended text.                                *
      *                                                             *
      * This routine calls the interface program to the more        *
      * extended text word processor.                               *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRMEXT        BEGSR
      *
      *          ------------------------------------------
      *          Set the change/display action code.
      *          ------------------------------------------
     C                   IF        I#CHG = *ON                                  B01
     C                   EVAL      P#ACTC = '20'
     C                   ELSE                                                   X01
     C                   EVAL      P#ACTC = '50'
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Call the interface Program
      *          ------------------------------------------
     C                   EVAL      P#CLS# = C#FIX
     C                   EVAL      P#EXWP = '1'
     C                   EVAL      P#EXTX = FMINC#
     C                   CALLP     OMX201C ( FMHEDC : P#CLS# : P#EXWP :
     C                             P#EXTX : P#ACTC : P#STAT )
      *
      *          ------------------------------------------
      *          Check if everything was cool.
      *          ------------------------------------------
     C                   IF        P#STAT = C#TERM                              B01
     C                   MOVEA     FMHEDC        MC(1)
     C                   MOVEA     FMINC#        MC(6)
     C                   EVAL      MC(16) = 'F'
     C                   EVAL      MC(17) = 'I'
     C                   EVAL      MC(18) = 'X'
     C                   EVAL      MC(19) = ' '
     C                   EVAL      MC(20) = ' '
     C                   EVAL      MC(21) = ' '
     C                   EVAL      MC(22) = ' '
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'OMQ5014'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SNDMSG - Send message                                        * /
      *                                                              * /
      * This routine displays an error wich occured when the user    * /
      * keyed data validation.                                       * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SNDMSG        BEGSR
      *
      *          ------------------------------------------
      *          Call send message program
      *          ------------------------------------------
     C                   EVAL      MSGF = 'OMHAPI '
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C  ( MSGID : MSGF : MSGLIB :
     C                             MSGDTA )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSUL#  -  Get Authorisation for helpdesk                    *
      *                                                              *
      * Input   -  P#HEDC - Application code to check                *
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
     C                   EVAL      P#OPTI = '3'
     C                   EVAL      P#SUL# = '0'
     C                   EVAL      P#STAT = *BLANKS
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRFUNK - Handle required function keys.                     *
      *                                                             *
      * This routine checks the requested function keys. There are  *
      * some keys which can not be used or with restrictions.       *
      *                                                             *
      * F1    - This key will always be used to display help.       *
      * F03   - When not issued then this will be the exit key      *
      * F4    - This key will first be used to prompt the           *
      *         fields determined by this program. If the cursor    *
      *         is not on a promptable field and it is assigned by  *
      *         the user, then it is returned.                      *
      * F05   - When not issued then this will be the refresh key   *
      * F06   - When not issued then this will be the add text key  *
      * F07   - When not issued then this will be the dsp customer  *
      * F12   - When not issued then this will be the cancel key    *
      * F24   - If the program detects more keys then will fit on   *
      *         one line, the user F24 key will be replaced by the  *
      *         'F24=More keys' standard key.                       *
      *                                                             *
      * If an invalid key was found, then the error buffer is       *
      * filled and the program is terminated.                       *
      *                                                             *
      * This routine calls SRDFTF to fill the function key lines if *
      * no keys where passed and SRUSRF to fill the user function   *
      * key lines.                                                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRFUNK        BEGSR
      *
      *          ------------------------------------------
      *          Init
      *          ------------------------------------------
     C                   EVAL      LIN = *BLANKS
     C                   EVAL      FA = '0'
     C                   EVAL      FT = '0'
      *
      *                    --------------------------------
      *                    I#Fxx  indicates if the key was processed
      *                    I#FxxU indicates if the key was supplied
      *                    --------------------------------
     C                   EVAL      I#F5 = *OFF
     C                   EVAL      I#F5U = *OFF
     C                   EVAL      I#F6 = *OFF
     C                   EVAL      I#F6U = *OFF
     C                   EVAL      I#F7 = *OFF
     C                   EVAL      I#F7U = *OFF
     C                   EVAL      I#F11 = *OFF
     C                   EVAL      I#F11U = *OFF
     C                   EVAL      I#F24 = *OFF
      *
      *          ------------------------------------------
      *          Get the text of F5=Refresh
      *          ------------------------------------------
     C                   EVAL      MSGID = 'KEYF501'
     C                   EVAL      MSGF = C#ODSP
     C                   EVAL      MSGLIB = C#LIBL
     C                   EVAL      MSGTXT = *BLANKS
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   MOVEA     MSGTXT        T05
      *
      *          ------------------------------------------
      *          Get the text of F6=Add
      *          ------------------------------------------
     C                   EVAL      MSGID = 'KEYF601'
     C                   EVAL      MSGF = C#ODSP
     C                   EVAL      MSGLIB = C#LIBL
     C                   EVAL      MSGTXT = *BLANKS
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   MOVEA     MSGTXT        T06
      *
      *          ------------------------------------------
      *          Get the text of F7=Show customer
      *          ------------------------------------------
     C                   EVAL      MSGID = 'KEYF701'
     C                   EVAL      MSGF = C#ODSP
     C                   EVAL      MSGLIB = C#LIBL
     C                   EVAL      MSGTXT = *BLANKS
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   MOVEA     MSGTXT        T07
      *
      *          ------------------------------------------
      *          Get the text of F11=View Text/Items
      *          ------------------------------------------
     C                   EVAL      MSGID = 'KEYF11F'
     C                   EVAL      MSGF = C#ODSP
     C                   EVAL      MSGLIB = C#LIBL
     C                   EVAL      MSGTXT = *BLANKS
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   MOVEA     MSGTXT        T11
      *
      *          ------------------------------------------
      *          Get the text & length of F24=More keys
      *          ------------------------------------------
     C                   EVAL      MSGID = 'KEYF241'
     C                   EVAL      MSGTXT = *BLANKS
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   MOVEA     MSGTXT        T24
      *
     C                   EVAL      FUN = T24
     C                   EXSR      SRTLEN
     C                   EVAL      W#F24# = IX
      *
      *          ------------------------------------------
      *          If no keys passed then do the default routine else
      *          do the user routine.
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      I2FUN# = *ZEROS
     C                   EXSR      SRDFTF
     C                   OTHER
     C                   EXSR      SRUSRF
     C                   ENDSL
      *
      *          ------------------------------------------
      *          If more then one line then fill F24=more keys in the
      *          last line.
      *          ------------------------------------------
     C                   IF        CL > 1                                       B01
     C                   EVAL      FA(24) = *ON
     C                   EVAL      I#F24 = *ON
     C                   EVAL      FUN = T24
     C                   EXSR      SRFLIN
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Display the first function key line.
      *          ------------------------------------------
     C                   EVAL      CL = 1                                       Current line
     C                   MOVE      LIN(CL)       FMFUND
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRDFTF - Default function key handler.                      *
      *                                                             *
      * This routine is called from SRFUNK. It handles the default  *
      * function keys.                                              *
      *                                                             *
      * The default function keys are: F3=Exit                      *
      *                                F5=Refresh                   *
      *                                F12=Cancel                   *
      *                                                             *
      * Input  - W#F24#   The length of the 'F24=More keys' text    *
      *                                                             *
      * Output - LIN      Contains the function key lines           *
      *          W#TKL#   Total key lines (F24 toggles)             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRDFTF        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      MSGF = C#ODSP
     C                   EVAL      MSGLIB = C#LIBL
      *
      *          ------------------------------------------
      *          Current line is line 1
      *          Current line length is zero
      *          ------------------------------------------
     C                   EVAL      CL = 1
     C                   EVAL      W#TLN# = *ZEROS
      *
      *          ------------------------------------------
      *          F3=Exit.
      *          ------------------------------------------
     C                   EVAL      MSGID = 'KEYF301'
     C                   EVAL      MSGTXT = *BLANKS
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   MOVEA     MSGTXT        FUN
     C                   EXSR      SRFLIN
     C                   EVAL      FA(3) = *ON
     C                   EVAL      FT(3) = *OFF
      *
      *          ------------------------------------------
      *          F5=Refresh
      *          ------------------------------------------
     C                   EVAL      FUN = T05
     C                   EXSR      SRFLIN
     C                   EVAL      FA(5) = *ON
     C                   EVAL      FT(5) = *ON
     C                   EVAL      I#F5 = *ON
      *
      *          ------------------------------------------
      *          F6=Add
      *          ------------------------------------------
     C                   IF        I2ACTC <> '40'                               B01
     C                             AND I2ACTC <> '50'
     C                             AND I2ACTC <> '60'
     C                             AND I2ACTC <> '61'
     C                   EVAL      FUN = T06
     C                   EXSR      SRFLIN
     C                   EVAL      FA(06) = *ON
     C                   EVAL      FT(06) = *ON
     C                   EVAL      I#F6 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          F7=Show customer
      *          ------------------------------------------
     C                   EVAL      FUN = T07
     C                   EXSR      SRFLIN
     C                   EVAL      FA(7) = *ON
     C                   EVAL      FT(7) = *ON
     C                   EVAL      I#F7 = *ON
      *
      *          ------------------------------------------
      *          F11=View Item/Text
      *          ------------------------------------------
     C                   IF        I#VIEW <> 'X'                                B01
     C                   EVAL      FUN = T11
     C                   EXSR      SRFLIN
     C                   EVAL      FA(11) = *ON
     C                   EVAL      FT(11) = *ON
     C                   EVAL      I#F11 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          F12=Cancel
      *          ------------------------------------------
     C                   EVAL      MSGID = 'KEYF121'
     C                   EVAL      MSGTXT = *BLANKS
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   MOVEA     MSGTXT        FUN
     C                   EXSR      SRFLIN
     C                   EVAL      FA(12) = *ON
     C                   EVAL      FT(12) = *OFF
      *
      *          ------------------------------------------
      *          Check if 'F24=More keys' is required.
      *          ------------------------------------------
     C                   IF        CL > 1                                       B01
     C                   EVAL      FA(24) = *ON
     C                   EVAL      FT(24) = *ON
     C                   EVAL      I#F24 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRUSRF - User function key handler.                         *
      *                                                             *
      * This routine is called from SRFUNK. It handles the user     *
      * supplied function keys.                                     *
      *                                                             *
      * Input  - W#F24#   The length of the 'F24=More keys' text    *
      *          I2FUN    Function key table                        *
      *          I2FUN#   Number of keys supplied                   *
      *                                                             *
      * Output - LIN      Contains the function key lines           *
      *          W#TKL#   Total key lines (F24 toggles)             *
      *                                                             *
      * This routine also performs data checking on the supplied    *
      * function key table. If an error was found, then te program  *
      * will terminate with the close option set unless the program *
      * was already open before this call was issued.               *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRUSRF        BEGSR
      *
      *          ------------------------------------------
      *          Current line is line 1
      *          Current line length is zero
      *          ------------------------------------------
     C                   EVAL      CL = 1
     C                   EVAL      W#TLN# = *ZEROS
      *
      *          ------------------------------------------
      *          Check if F5 is supplied.
      *          ------------------------------------------
     C                   FOR       OC = 1 TO I2FUN#                             B01
     C                   MOVE      FIN(OC)       I2FUN
     C                   IF        I2KEY# = 5                                   B02
     C                   EVAL      I#F5U = *ON
     C                   ENDIF                                                  E02
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          Check if F06 is supplied.
      *          ------------------------------------------
     C                   FOR       OC = 1 TO I2FUN#                             B01
     C                   MOVE      FIN(OC)       I2FUN
     C                   IF        I2KEY# = 6                                   B02
     C                   EVAL      I#F6U = *ON
     C                   ENDIF                                                  E02
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          Check if F07 is supplied.
      *          ------------------------------------------
     C                   FOR       OC = 1 TO I2FUN#                             B01
     C                   MOVE      FIN(OC)       I2FUN
     C                   IF        I2KEY# = 7                                   B02
     C                   EVAL      I#F7U = *ON
     C                   ENDIF                                                  E02
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          Check if F11 is supplied.
      *          ------------------------------------------
     C                   FOR       OC = 1 TO I2FUN#                             B01
     C                   MOVE      FIN(OC)       I2FUN
     C                   IF        I2KEY# = 11                                  B02
     C                   EVAL      I#F11U = *ON
     C                   ENDIF                                                  E02
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          Do the number of function keys.
      *          ------------------------------------------
     C                   FOR       OC = 1 TO I2FUN#                             B01
     C                   MOVE      FIN(OC)       I2FUN
      *
      *          ------------------------------------------
      *          Check if the data is ok. If not then exit.
      *          ------------------------------------------
     C                   IF        I2TOMC <> '1'                                B02
     C                             AND I2TOMC <> '0'
     C                             OR I2KEY# < 1
     C                             OR I2KEY# > 24
     C                             OR I2KEY# = 1
     C                   MOVE      'OMQ5009'     I2MSID
     C                   MOVE      'E'           I2MSTP
     C                   MOVE      *BLANKS       I2MSGD
     C                   Z-ADD     OC            D#BI2#
     C                   MOVEL     D#BI2A        I2MSGD
     C                   IF        I#OPEN <> *ON                                B03
     C                   EVAL      D#PARM = *ZEROS
     C                   ENDIF                                                  E03
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          If this key is greater then F5 and F5 is not
      *          supplied then fill the defaul 'F5=Refresh' text.
      *          ------------------------------------------
     C                   IF        I2KEY# > 5                                   B02
     C                             AND I#F5U = *OFF
     C                   EVAL      FUN = T05
     C                   EXSR      SRFLIN
     C                   EVAL      I#F5 = *ON
     C                   EVAL      I#F5U = *ON
     C                   EVAL      FA(5) = *ON
     C                   EVAL      FT(5) = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          If this key is greater then F06 and F06 is not
      *          supplied then fill the defaul 'F06=Add text'
      *          ------------------------------------------
     C                   IF        I2KEY# > 6                                   B02
     C                             AND I#F6U = *OFF
     C                             AND I2ACTC <> '40'
     C                             AND I2ACTC <> '50'
     C                   EVAL      FUN = T06
     C                   EXSR      SRFLIN
     C                   EVAL      I#F6 = *ON
     C                   EVAL      I#F6U = *ON
     C                   EVAL      FA(06) = *ON
     C                   EVAL      FT(06) = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          If this key is greater then F07 and F07 is not
      *          supplied then fill the defaul 'F07=Show Customer'
      *          ------------------------------------------
     C                   IF        I2KEY# > 7                                   B02
     C                             AND I#F7U = *OFF
     C                   EVAL      FUN = T07
     C                   EXSR      SRFLIN
     C                   EVAL      I#F7 = *ON
     C                   EVAL      I#F7U = *ON
     C                   EVAL      FA(07) = *ON
     C                   EVAL      FT(07) = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          If this key is greater then F11 and F11 is not
      *          supplied then fill the defaul 'F11=View Item/Text'
      *          ------------------------------------------
     C                   IF        I2KEY# > 11                                  B02
     C                             AND I#F11U = *OFF
     C                             AND I2ACTC <> '40'
     C                             AND I2ACTC <> '50'
     C                   EVAL      FUN = T11
     C                   EXSR      SRFLIN
     C                   EVAL      I#F11 = *ON
     C                   EVAL      I#F11U = *ON
     C                   EVAL      FA(11) = *ON
     C                   EVAL      FT(11) = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Special function keys are processed, fill this key.
      *          ------------------------------------------
     C                   IF        I2TOMC = '0'                                 B02
     C                   EVAL      MSGID = I2KMID
     C                   EVAL      MSGF = I2KMFC
     C                   EVAL      MSGLIB = I2KMLC
     C                   EVAL      MSGTXT = *BLANKS
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   MOVEA     MSGTXT        FUN
      *
      *          ------------------------------------------
      *          If no text in the message id.
      *          ------------------------------------------
     C                   IF        MSGTXT = *BLANKS                             B03
     C                   Z-ADD     I2KEY#        D#D2#
     C                   EVAL      FUN(1) = 'F'
     C                   IF        I2KEY# < 10                                  B04
     C                   EVAL      FUN(2) = D#D22A
     C                   EVAL      FUN(3) = '='
     C                   EVAL      FUN(4) = '?'
     C                   EVAL      FUN(5) = '?'
     C                   ELSE                                                   X04
     C                   MOVEA     D#D2A         FUN(2)
     C                   EVAL      FUN(4) = '='
     C                   EVAL      FUN(5) = '?'
     C                   EVAL      FUN(6) = '?'
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   EXSR      SRFLIN
      *
      *          ------------------------------------------
      *          Fill plain text if no text found, then do not
      *          fill this line.
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   IF        I2KEYD <> *BLANKS                            B03
     C                   MOVEA     I2KEYD        FUN
     C                   EXSR      SRFLIN
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Activate this key
      *          ------------------------------------------
     C                   EVAL      IX = I2KEY#
     C                   EVAL      FA(IX) = *ON
     C                   EVAL      FT(IX) = I2TYPC
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          Check if the special function keys are supplied.
      *          'F5=Refresh'.
      *          ------------------------------------------
     C                   IF        I#F5 <> *ON                                  B01
     C                             AND I#F5U <> *ON
     C                   EVAL      FUN = T05
     C                   EXSR      SRFLIN
     C                   EVAL      I#F5 = *ON
     C                   EVAL      I#F5U = *ON
     C                   EVAL      FA(5) = *ON
     C                   EVAL      FT(5) = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          'F6=Add text'
      *          ------------------------------------------
     C                   IF        I#F6 <> *ON                                  B01
     C                             AND I#F6U <> *ON
     C                             AND I2ACTC <> '40'
     C                             AND I2ACTC <> '50'
     C                   EVAL      FUN = T06
     C                   EXSR      SRFLIN
     C                   EVAL      I#F6 = *ON
     C                   EVAL      I#F6U = *ON
     C                   EVAL      FA(06) = *ON
     C                   EVAL      FT(06) = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          'F6=Show Customer'
      *          ------------------------------------------
     C                   IF        I#F7 <> *ON                                  B01
     C                             AND I#F7U <> *ON
     C                   EVAL      FUN = T07
     C                   EXSR      SRFLIN
     C                   EVAL      I#F7 = *ON
     C                   EVAL      I#F7U = *ON
     C                   EVAL      FA(07) = *ON
     C                   EVAL      FT(07) = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          'F11=View Item/Text'
      *          ------------------------------------------
     C                   IF        I#F11 <> *ON                                 B01
     C                             AND I#F11U <> *ON
     C                             AND I2ACTC <> '40'
     C                             AND I2ACTC <> '50'
     C                   EVAL      FUN = T11
     C                   EXSR      SRFLIN
     C                   EVAL      I#F11 = *ON
     C                   EVAL      I#F11U = *ON
     C                   EVAL      FA(11) = *ON
     C                   EVAL      FT(11) = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRTLEN - Get the length of the text in FUN                  *
      *                                                             *
      * This routine calculates the length of the text in FUN.      *
      *                                                             *
      * Input  - FUN   (array of 30(1))                             *
      *                                                             *
      * Output - IX    the length of the text.                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRTLEN        BEGSR
      *
      *          ------------------------------------------
      *          Do it.
      *          ------------------------------------------
     C                   EVAL      IX = 30
     C                   DOW       FUN(IX) = *BLANKS                            B01
     C                   SUB       1             IX
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Each key is seperated by 2 spaces.
      *          ------------------------------------------
     C                   ADD       2             IX
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRFLIN - Fill the function key text in the function key line*
      *                                                             *
      * This routine checks if the function key in FUN will fit     *
      * in the current line of array LIN which is addressed by      *
      * CL (current line). If the text will not fit, it is moved    *
      * to the next line and the current line will end with the     *
      * F24=More keys text.                                         *
      *                                                             *
      * Input  - FUN   (array of 30(1))                             *
      *                                                             *
      * Output - CL    The current text line                        *
      *          LIN   Array LIN containing the lines.              *
      *                                                             *
      * Kills  - IX                                                 *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRFLIN        BEGSR
      *
      *          ------------------------------------------
      *          Get the length of the text in FUN
      *          ------------------------------------------
     C                   EXSR      SRTLEN
      *
      *          ------------------------------------------
      *          If this is the first key then fill it.
      *          ------------------------------------------
     C                   IF        W#TLN# = *ZEROS                              B01
     C                   MOVEA     FUN           LIN(CL)
     C                   EVAL      W#TLN# = IX
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          If the length of this key plus the length of the
      *          'F24=More keys' text will not fit on the current
      *          line, then fill the more keys text and place the
      *          current text on the next line. Else fill it in this.
      *          ------------------------------------------
     C     IX            ADD       W#F24#        IY
     C                   ADD       IY            W#TLN#
      *
     C                   IF        W#TLN# > 80                                  B02
     C                             AND FUNFLD <> T24FLD
     C                   MOVEA     T24           W#TX30
     C                   EVAL      LIN(CL) = %TRIMR(LIN(CL)) + '  ' + W#TX30
     C                   ADD       1             CL
     C                   EVAL      W#TLN# = IX
     C                   MOVEA     FUN           LIN(CL)
      *
     C                   ELSE                                                   X02
     C                   MOVEA     FUN           W#TX30
     C                   EVAL      LIN(CL) = %TRIMR(LIN(CL)) + '  ' + W#TX30
     C                   SUB       W#F24#        W#TLN#
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
      * SRFRST - General API handler (do this first)                *
      *                                                             *
      * This routine checks some general dependencies which without *
      * the program will surely not work. If such a conflict was    *
      * found then the program will terminate with the close        *
      * option set unless the program was already open before this  *
      * call was issued.                                            *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRFRST        BEGSR
      *
      *          ------------------------------------------
      *          If not required number of parameters, or the close
      *          option is set then exit.
      *          ------------------------------------------
     C                   IF        D#PARM <> 10                                 B01
     C                             OR I2ACTC = '99'
     C                   EVAL      D#PARM = *ZEROS
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Exit if no parameters passed.
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      D#PARM = *ZERO
     C                   EXSR      SRTTLR
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Check interface level
      *          ------------------------------------------
     C                   IF        I2INTL <> C#INTL                             B01
     C                   MOVE      'OMQ500B'     I2MSID
     C                   MOVE      'E'           I2MSTP
     C                   EVAL      MC = *BLANKS
     C                   MOVEA     D#PGM         MC(1)
     C                   MOVEA     I2INTL        MC(11)
     C                   MOVEA     MC            I2MSGD
     C                   EVAL      D#PARM = *ZEROS
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Move the processing array.
      *          ------------------------------------------
     C                   MOVEA     I2PRCA        PRA
      *
      *          ------------------------------------------
      *          Clear output parameters
      *          ------------------------------------------
     C                   EVAL      I2FKU# = *ZEROS
     C                   MOVE      *BLANKS       I2MSID
     C                   MOVE      *BLANKS       I2MSTP
     C                   MOVE      *BLANKS       I2MSGD
      *
      *          ------------------------------------------
      *          Do not check this if the redisplay option is
      *          set and the program is already open.
      *          ------------------------------------------
     C                   IF        I#OPEN <> *ON                                B01
     C                             OR I#OPEN = *ON
     C                             AND PRA(2) = *OFF
      *
      *          ------------------------------------------
      *          If more then 23 function keys supplied then exit
      *          ------------------------------------------
     C                   IF        I2FUN# > 23                                  B02
     C                   MOVE      'OMQ5009'     I2MSID
     C                   MOVE      'E'           I2MSTP
     C                   EVAL      D#BI2# = 0
     C                   MOVE      *BLANKS       I2MSGD
     C                   MOVEL     D#BI2A        I2MSGD
     C                   EVAL      D#PARM = *ZEROS
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          If not add, then the incident must exist.
      *          ------------------------------------------
     C                   IF        I2ACTC <> '60'                               B02
     C                             AND I2ACTC <> '61'                           B02
     C                   MOVE      I2KHED        NCHEDC
     C                   MOVE      I2KINC        NCINC#
     C     $NC1K1        SETLL     OMINCL1
     C     $NC1K1        READE(N)  OMINCL1
     C                   EVAL      *IN99 = %EOF
     C                   IF        *IN99 = *ON                                  B03
     C                   MOVE      'OMQA002'     I2MSID
     C                   MOVE      'E'           I2MSTP
     C                   EVAL      MC = *BLANKS
     C                   MOVEA     NCHEDC        MC(1)
     C                   MOVEA     NCINC#        MC(6)
     C                   MOVEA     MC            I2MSGD
     C                   EVAL      D#PARM = *ZEROS
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Get the call track number for the customer if add/copy
      *          ------------------------------------------
     C                   IF        I2ACTC = '60'                                B02
     C                             OR I2ACTC = '61'
     C                             OR I2ACTC = '30'
     C                   MOVE      I2HEDC        CAHEDC
     C                   MOVE      I2CTN#        CACTN#
     C     $CA1K1        SETLL     OMCCAL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B03
     C                   MOVE      'OMQA00B'     I2MSID
     C                   MOVE      'E'           I2MSTP
     C                   EVAL      MC = *BLANKS
     C                   MOVEA     I2CTN#        MC(1)
     C                   MOVEA     I2HEDC        MC(11)
     C                   MOVEA     MC            I2MSGD
     C                   EVAL      D#PARM = *ZEROS
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Delete and display can not use prefill.
      *          Also fill the screen buffer from the record.
      *          ------------------------------------------
     C                   IF        I2ACTC = '40'                                B02
     C                             OR I2ACTC = '50'
     C                   EVAL      PRA(3) = '0'
     C                   MOVEL     I4BUF         I2INC
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Not add nor copy is protect helpdesk.
      *          ------------------------------------------
     C                   IF        I2ACTC <> '60'                               B02
     C                             AND I2ACTC <> '61'                           B02
     C                             AND I2ACTC <> '30'
     C                   EVAL      PRA(4) = '1'
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          If the application is protect and this is add
      *          then fill the key application with the passed HEDC.
      *          ------------------------------------------
     C                   IF        I2ACTC = '60'                                B02
     C                             AND I2ACTC <> '61'                           B02
     C                             AND PRA(4) = *ON
     C                   MOVE      I2HEDC        I2KHED
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          If the helpdesk is protected then retrieve auth.
      *          The user must at least be '1' to add. Other auth.
      *          depend on the incident.
      *          ------------------------------------------
     C                   IF        PRA(4) = *ON                                 B02
     C                             OR I2ACTC = '30'
     C     I2KHED        SETLL     OMHDDL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B03
     C                   MOVE      'OMQ601A'     I2MSID
     C                   MOVE      'E'           I2MSTP
     C                   MOVE      *BLANKS       I2MSGD
     C                   MOVEL     I2KHED        I2MSGD
     C                   EVAL      D#PARM = *ZEROS
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X03
      *
     C                   MOVE      I2KHED        P#HEDC
     C                   EVAL      P#USID = D#USID
     C                   EXSR      SRSUL#
      *
     C                   IF        I2ACTC = '60'                                B04
     C                             OR I2ACTC = '61'                             B04
     C                             OR I2ACTC = '50'
     C                             OR P#SUL# >= I4SUL#
     C                   ELSE                                                   X04
      *
     C                   MOVE      'OMQA00C'     I2MSID
     C                   MOVE      'E'           I2MSTP
     C                   MOVE      *BLANKS       I2MSGD
     C                   MOVEA     I2INC#        MC(1)
     C                   MOVEA     P#HEDC        MC(11)
     C                   MOVEA     MC            I2MSGD
     C                   EVAL      D#PARM = *ZEROS
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill the *SAME values.
      *          ------------------------------------------
     C                   IF        I2STRC = '*SAME'                             B02
     C                   MOVE      NCSTRC        I2STRC
     C                   ENDIF                                                  E02
      *
     C                   IF        I2ITPC = '*SAME'                             B02
     C                   MOVE      NCITPC        I2ITPC
     C                   ENDIF                                                  E02
      *
     C                   IF        I2SEV1 = '*SAME'                             B02
     C                   MOVE      NCSEV1        I2SEV1
     C                   ENDIF                                                  E02
      *
     C                   IF        I2SEV2 = '*SAME'                             B02
     C                   MOVE      NCSEV2        I2SEV2
     C                   ENDIF                                                  E02
      *
     C                   IF        I2SEV3 = '*SAME'                             B02
     C                   MOVE      NCSEV3        I2SEV3
     C                   ENDIF                                                  E02
      *
     C                   IF        I2USF1 = '*SAME'                             B02
     C                   MOVE      NCUSF1        I2USF1
     C                   ENDIF                                                  E02
      *
     C                   IF        I2USF2 = '*SAME'                             B02
     C                   MOVE      NCUSF2        I2USF2
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill the function keys.
      *          ------------------------------------------
     C                   EXSR      SRFUNK
     C                   ENDIF                                                  E01
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
      *          Indicator definitions
      *          ------------------------------------------
     C                   EVAL      I#VIEW = '0'
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      AB = *ZEROS
     C                   EVAL      CM = *ZEROS
     C                   EVAL      IX = *ZEROS
     C                   EVAL      K# = *ZEROS
     C                   EVAL      IY = *ZEROS
     C                   EVAL      W#TKL# = *ZEROS                              Tot. F-lines
     C                   EVAL      W#TLN# = *ZEROS                              Text length
     C                   EVAL      W#F24# = *ZEROS                              F24 text len
     C                   EVAL      W#TX30 = *BLANKS
     C                   EVAL      CL = *ZEROS                                  Current line
     C                   EVAL      OC = *ZEROS                                  Occurence
      *
     C                   EVAL      RRN01 = *ZEROS
     C                   EVAL      RRN02 = *ZEROS
      *
      *          ------------------------------------------
      *          Savefield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parmeterfield definition
      *          ------------------------------------------
     C                   EVAL      P#ACTC = P#ACTC
      *
      *          ------------------------------------------
      *          API parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      *          ------------------------------------------
     C     $X901C        PLIST
     C                   PARM                    MSGID
     C                   PARM      'OMHAPI '     MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGDTA
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      *          ------------------------------------------
     C     $901C2        PLIST
     C                   PARM                    D#MSID
     C                   PARM      'QCPFMSG'     MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    D#MSGD
      *
      *          ------------------------------------------
      *          Parameter list for OMH902C (Clear message queue)
      *          ------------------------------------------
     C     $X902C        PLIST
     C                   PARM                    PGMQ
      *
      *          ------------------------------------------
      *          Parameter list for OMH903C (Get system parameters)
      *          ------------------------------------------
     C     $X903C        PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    P#APNM
     C                   PARM                    P#CONM
      *
      *          ------------------------------------------
      *          Parameter list for OMH904C (Get message data text)
      *          ------------------------------------------
     C     $X904C        PLIST
     C                   PARM                    MSGID
     C                   PARM                    MSGF
     C                   PARM                    MSGLIB
     C                   PARM                    MSGTXT
      *
      *          ------------------------------------------
      *          Parameter list for OMH905C (Get user profile)
      *          ------------------------------------------
     C     $X905C        PLIST
     C                   PARM      D#USID        P#USID
     C                   PARM                    P#USCL
     C                   PARM                    P#GRID
     C                   PARM                    P#GRCL
     C                   PARM                    P#USDC
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH909C (Check Object)
      *          ------------------------------------------
     C     $X909C        PLIST
     C                   PARM                    P#OBJL
     C                   PARM                    P#OBJC
     C                   PARM                    P#MATM
     C                   PARM                    P#OBJT
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH949C (Change application).
      *          ------------------------------------------
     C     $X949C        PLIST
     C                   PARM                    FMHEDC
     C                   PARM                    P#SUL#
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMX201C (more extended text)
      *          ------------------------------------------
     C     $X201C        PLIST
     C                   PARM                    FMHEDC
     C                   PARM      C#FIX         P#CLS#
     C                   PARM      '1'           P#EXWP
     C                   PARM      FMINC#        P#EXTX
     C                   PARM                    P#ACTC
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for QCMDEXEC
      *          ------------------------------------------
     C     $QCMDE        PLIST
     C                   PARM                    P#EXE
     C                   PARM      60            P#LEN
      *
      *          ------------------------------------------
      *          Parameter list for QCMDEXEC
      *          ------------------------------------------
     C     $MH023        PLIST
     C                   PARM      FMHEDC        P#HEDC
     C                   PARM      FMINC#        P#INC#
     C                   PARM                    P#ITH#
     C                   PARM                    P2ACTC
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH024
      *          ------------------------------------------
     C     $MH024        PLIST
     C                   PARM      FMCUSC        P#CUSC
     C                   PARM      FMCCTC        P#CCTC
     C                   PARM                    P#STAT
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
      *          Keylist for the incident file
      *          ------------------------------------------
     C     $NC1K1        KLIST
     C                   KFLD                    NCHEDC
     C                   KFLD                    NCINC#
      *
      *          ------------------------------------------
      *          Keylist for the employee file
      *          ------------------------------------------
     C     $DE1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#USID
      *
      *          ------------------------------------------
      *          Keylist for the calltrack file
      *          ------------------------------------------
     C     $CA1K1        KLIST
     C                   KFLD                    CAHEDC
     C                   KFLD                    CACTN#
      *
      *          ------------------------------------------
      *          Keylist for the customer contacts
      *          ------------------------------------------
     C     $CO1K1        KLIST
     C                   KFLD                    FMCUSC
     C                   KFLD                    FMCCTC
      *
      *          ------------------------------------------
      *          Keylist for the items per incident
      *          ------------------------------------------
     C     $OB1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMINC#
      *
      *          ------------------------------------------
      *          Keylist for the header text file
      *          ------------------------------------------
     C     $IH1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMINC#
      *
     C     $IH1K2        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMINC#
     C                   KFLD                    P#ITH#
      *
      *          ------------------------------------------
      *          Exit if no parameters.
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      D#PARM = *ZEROS
     C                   EXSR      SRTTLR
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Open files when parms passed
      *          ------------------------------------------
     C                   OPEN      OMHDDL1
     C                   OPEN      OMINCL1
     C                   OPEN      OMCCOL1
     C                   OPEN      OMMPXL1
      *
      *          ------------------------------------------
      *          Get the language text.
      *          ------------------------------------------
     C                   EXSR      SRGVAR                                       OMHMSG
     C                   EXSR      SRGTXT                                       OMHDSP
      *
      *          ------------------------------------------
      *          Initialize buffer OMQCHGIN
      *          ------------------------------------------
     C                   MOVE      *OFF          I4EOP
     C                   MOVE      *OFF          I4UPB
     C                   MOVEL     PGMQ          I4MSQ
     C                   MOVEL     C#INTL        I4INTL
      *
      *          ------------------------------------------
      *          Get the user information.
      *          ------------------------------------------
     C                   EVAL      P#USID = D#USID
     C                   CALLP     OMH905C ( P#USID : P#USCL : P#GRID :
     C                             P#GRCL : P#USDC : P#STAT )
      *
      *          ------------------------------------------
      *          Check Word Processing interface exists
      *          ------------------------------------------
     C                   EVAL      P#OBJL = C#LIBL
     C                   EVAL      P#OBJC = C#201C
     C                   EVAL      P#MATM = *BLANKS
     C                   EVAL      P#OBJT = C#PGM
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH909C ( P#OBJL : P#OBJC : P#MATM :
     C                             P#OBJT : P#STAT )
     C                   IF        P#STAT = C#NORM                              B01
     C                   EVAL      I#WP = *ON
     C                   ELSE                                                   X01
     C                   EVAL      I#WP = *OFF
     C                   ENDIF                                                  E01
     C                   EVAL      P#STAT = C#NORM
      *
      *          ------------------------------------------
      *          Initialize system date
      *          ------------------------------------------
     C                   Z-ADD     *YEAR         D#SYSY
     C                   EVAL      D#SYSM = *MONTH
     C                   EVAL      D#SYSD = *DAY
      *
      *          ------------------------------------------
      *          Open the display file.
      *          ------------------------------------------
     C                   MOVEL     C#OVR1        P#EXE
     C                   EVAL      P#LEN = 60
     C                   CALLP     QCMDEXC ( P#EXE : P#LEN )
     C                   OPEN      OMQUSIIND                            99
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
     C                   IF        D#PARM = *ZEROS                              B01
     C                             OR PRA(1) = *ON
      *
      *          ------------------------------------------
      *          Close the fix API
      *          ------------------------------------------
     C                   CALLP     OMQCHGIN_001()
      *
      *          ------------------------------------------
      *          Close the text changer
      *          ------------------------------------------
     C                   IF        I#H023 = *ON                                 B02
     C                   CALLP     OMH023()
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Set close indicator
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If parameters then fill the cursor field.
      *          ------------------------------------------
     C                   IF        D#PARM <> *ZEROS                             B01
     C                   MOVE      FMFLDC        W#CHA8
     C                   MOVEL(P)  W#CHA8        I2CFLC
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
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGTXT  -  Get display file text                             *
      *                                                              *
      * This subroutine gets the data needed for this program to     *
      * display on the panels.                                       *
      * It is called one time per program invocation.                *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGTXT        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init.
      *          ------------------------------------------
     C                   EVAL      MSGLIB = C#LIBL
     C                   EVAL      MSGF = C#ODSP
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 10                                 B01
     C                   EVAL      MSGID = TXD(MS)
      *
      *          ------------------------------------------
      *          Call subprogram
      *          ------------------------------------------
     C                   IF        MSGID <> *BLANKS                             B02
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill message arrays
      *          ------------------------------------------
     C                   EVAL      TXT(MS) = MSGTXT
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGVAR  -  Get variable message text                         *
      *                                                              *
      * This subroutine gets the data needed for this program to     *
      * substitute in message texts.                                 *
      * It is called one time per program invocation.                *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGVAR        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init.
      *          ------------------------------------------
     C                   EVAL      MSGLIB = C#LIBL
     C                   EVAL      MSGF = C#OMSG
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 10                                 B01
     C                   EVAL      MSGID = MSD(MS)
      *
      *          ------------------------------------------
      *          Call subprogram
      *          ------------------------------------------
     C                   IF        MSGID <> *BLANKS                             B02
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill message arrays
      *          ------------------------------------------
     C                   EVAL      %SUBST(VAR(MS):1:132) = MSGTXT
     C                   ENDFOR                                                 E01
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
     C                   MOVEL     HDDL1         FILE
      *
     C                   WHEN      E#FILE = 'OMCCAL1 '                          X02
     C                   MOVEL     CCAL1         FILE
      *
     C                   WHEN      E#FILE = 'OMQUSIIN'                          X02
     C                   MOVEL     INFO          FILE
      *
     C                   WHEN      E#FILE = 'OMINCL1 '                          X02
     C                   MOVEL     INCL1         FILE
      *
     C                   WHEN      E#FILE = 'OMTIHL1 '                          X02
     C                   MOVEL     TIHL1         FILE
      *
     C                   WHEN      E#FILE = 'OMCCOL1 '                          X02
     C                   MOVEL     CCOL1         FILE
      *
     C                   WHEN      E#FILE = 'OMMPXL1 '                          X02
     C                   MOVEL     MPXL1         FILE
     C                   ENDSL                                                  E02
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
     C                   EVAL      P#MSKC = *BLANKS
     C                   EVAL      P#LOB# = 4
     C                   EVAL      P#PMQC = '*'
     C                   EVAL      P#PSC# = 1
     C                   CALLP     QMHMOVPM ( P#MSKC : P#MSTA : P#LOB# :
     C                             P#PMQC : P#PSC# : P#ERR )
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
     C                   MOVE      'E'           I2MSTP
     C                   MOVEL     'OMQ5012'     I2MSID
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
      * Array specifications                                        *
      * ----------------------------------------------------------- *
**      MSD
DSC0001    description                         1
VRS0004    OS/400 version                      2
DEV0001    device                              3
JBD0001    job description                     4
LIB0001    library                             5
OPT0006    '0' or '1'                          6
MON0001    monitor option                      7
ADR0002    remote location                     8
**      TXD
IIN0001    Maintain Incident                   1
IIN0002    Copy Incident                       2
IIN0003    Add Incident                        3
IIN0004    Display Incident                    4
IIN0005    Confirm delete of Incident          5
DAT0026    Realized start date                 6
DAT0028    Realized completion date            7
DAT0030    Realized development end date       8
HRS0005    Realized number of hours            9
**      FL
HEDC
INC#
ROUT
ITPC
USF1
USF2
USF3
SEV1
SEV2
SEV3
SHID
**      FP
067
060
061
062
068
069
070
063
064
065
076
