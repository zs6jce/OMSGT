     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ------------------------------------------------------------ *
      * Description                                                  *
      * ------------------------------------------------------------ *
      *                                                              *
      * Program ....: OMH018                                         *
      * Function ...: Incident Maintenance                           *
      * Author .....: Remain Software                               *
      * Module .....: HelpDesk                                       *
      * Parameters .: P#HEDC    -  HelpDesk Code                 I   *
      *               P#CTN#    -  Call Track Number             I   *
      *               P#CUSC    -  Customer code subset          I   *
      *               P#CCTC    -  Contact code subset           I   *
      *               P#STAT    -  Return Status                 O   *
      *                                                              *
      * This program is used as a starting point for all kinds of    *
      * actions on incidents.                                        *
      * ------------------------------------------------------------ *
      * ------------------------------------------------------------ *
      * File specifications                                          *
      * ------------------------------------------------------------ *
      *
     FOMINCJ1   IF   E           K DISK    INFDS(INCJ1)
     F                                     INFSR(*PSSR)
     F                                     RENAME(OMINCR:OMINC1)
     FOMINCJ2   IF   E           K DISK    INFDS(INCJ2)
     F                                     INFSR(*PSSR)
     F                                     RENAME(OMINCR:OMINC2)
     FOMINCJ3   IF   E           K DISK    INFDS(INCJ3)
     F                                     INFSR(*PSSR)
     F                                     RENAME(OMINCR:OMINC3)
     FOMINCJ4   IF   E           K DISK    INFDS(INCJ4)
     F                                     INFSR(*PSSR)
     F                                     RENAME(OMINCR:OMINC4)
     FOMINCJ5   IF   E           K DISK    INFDS(INCJ5)
     F                                     INFSR(*PSSR)
     F                                     RENAME(OMINCR:OMINC5)
     FOMINCJ6   IF   E           K DISK    INFDS(INCJ6)
     F                                     INFSR(*PSSR)
     F                                     RENAME(OMINCR:OMINC6)
     FOMINCL4   IF   E           K DISK    INFDS(INCL4)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     F                                     RENAME(OMINCR:OMINL4)
     FOMHDDL1   IF   E           K DISK    INFDS(HDDL1)
     F                                     INFSR(*PSSR)
     FOMHTVL1   UF A E           K DISK    INFDS(HTVL1)
     F                                     INFSR(*PSSR)
     FOMRIRL1   IF   E           K DISK    INFDS(RIRL1)
     F                                     INFSR(*PSSR)
     FOMRIRL2   IF   E           K DISK    INFDS(RIRL2)
     F                                     INFSR(*PSSR)
     F                                     RENAME(OMRIRR:OMRIR2)
     FOMHUOL1   IF   E           K DISK    INFDS(HUOL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMQAHL1   IF   E           K DISK    INFDS(QAHL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMHQUL1   IF   E           K DISK    INFDS(HQUL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMANSL1   IF   E           K DISK    INFDS(ANSL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMCCAL1   IF   E           K DISK    INFDS(CCAL1)
     F                                     INFSR(*PSSR)
     FOMCCOL1   IF   E           K DISK    INFDS(CCOL1)
     F                                     INFSR(*PSSR)
     FOMESAL1   IF   E           K DISK    INFDS(ESAL1)
     F                                     INFSR(*PSSR)
     FOMSTRL1   IF   E           K DISK    INFDS(STRL1)
     F                                     INFSR(*PSSR)
     FOMHDEL1   IF   E           K DISK    INFDS(HDEL1)
     F                                     INFSR(*PSSR)
     FOMH018D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
     F                                     SFILE(OMH01801:RRN01)
     F                                     SFILE(OMH01804:RRN01)
     F                                     SFILE(OMH01807:RRN01)
     F                                     SFILE(OMH01809:RRN03)
     F                                     SFILE(OMH01812:RRN04)
     F                                     SFILE(OMH01818:RRN01)
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension specifications                                     *
      * ------------------------------------------------------------ *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm    *
      * MT    Message text from QMHRTVM API                          *
      * STR   Array to store helpdesks / status routes from OMESA    *
      * QUE   Used to store question numbers                         *
      * ANS   Used to store answers                                  *
      * QLS   Used to store question lists                           *
      * QSQ   Used to store question seq in question list            *
      * ------------------------------------------------------------ *
      *
     D VAR             S            256    DIM(6)
     D MSD             S              7    DIM(6) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
     D STR             S             15    DIM(1000)
      *COPY QCPYSRC,OMSREFREN
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Input specifications                                         *
      * ------------------------------------------------------------ *
      *
      *          ------------------------------------------
      *          Field reference file used to define fields
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          File information data structures
      *          ------------------------------------------
     D INCJ1           DS            57
     D INCJ2           DS            57
     D INCJ3           DS            57
     D INCJ4           DS            57
     D INCJ5           DS            57
     D INCJ6           DS            57
     D INCL4           DS            57
     D HDDL1           DS            57
     D HUOL1           DS            57
     D HTVL1           DS            57
     D RIRL1           DS            57
     D RIRL2           DS            57
     D ERRL1           DS            57
     D QAHL1           DS            57
     D HQUL1           DS            57
     D ANSL1           DS            57
     D CCAL1           DS            57
     D CCOL1           DS            57
     D ESAL1           DS            57
     D STRL1           DS            57
     D HDEL1           DS            57
      *
      *          ------------------------------------------
      *          Question & answer arrays.
      *          ------------------------------------------
     D P#QUE           DS
     D  QUE                    1   5000  0
     D                                     DIM(1000)
     D P#ANS           DS
     D  ANS                    1   3000  0
     D                                     DIM(1000)
     D P#QLS           DS
     D  QLS                    1   5000  0
     D                                     DIM(1000)
     D P#QSQ           DS
     D  QSQ                    1   3000  0
     D                                     DIM(1000)
      *
      *          ------------------------------------------
      *          Screen information data structure
      *          ------------------------------------------
     D INFO            DS
     D  STATUS           *STATUS
     D  D#CRC#               378    379B 0
      *
      *          ------------------------------------------
      *          Program status data structure
      *          ------------------------------------------
     D PGM            SDS
     D  E#FILE               201    208
     D  D#USID               254    263
     D  PGMQ             *PROC
      *
      *          ------------------------------------------
      *          User options, incident buffer
      *          ------------------------------------------
     D D#INC           DS
     D D#HEDC                         5
     D D#INC#                        10
      *
      *          ------------------------------------------
      *          Date converter input
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
      *          Data structure to build a date field
      *          ------------------------------------------
     D                 DS
     D D#DATE                         7  0
     D  D#CC                          1  0 OVERLAY(D#DATE)
     D  D#YY                          2  0 OVERLAY(D#DATE:2)
     D  D#MM                          2  0 OVERLAY(D#DATE:4)
     D  D#DD                          2  0 OVERLAY(D#DATE:6)
      *
      *          ------------------------------------------
      *          Data structures used for positioning
      *          FMPS1S = HelpDesk code + priority
      *          FMPS2S = HelpDesk code + incident number
      *          ------------------------------------------
     D FMPS1S          DS             8
     D FMPS1A                         5
     D FMPS1B                         3  0
      *
     D FMPS2S          DS            15
     D FMPS2A                         5
     D FMPS2B                        10
      *
      *          ------------------------------------------
      *          Map selection fields to data structure
      *          ------------------------------------------
     D                 DS
     D D#FMT1                       188
     D  FFHEDC                        5    OVERLAY(D#FMT1)
     D  XXXXXX                        5    OVERLAY(D#FMT1:6)
     D  FFINC#                       10    OVERLAY(D#FMT1:11)
     D  FTINC#                       10    OVERLAY(D#FMT1:21)
     D  FFCTN#                       10    OVERLAY(D#FMT1:31)
     D  FTCTN#                       10    OVERLAY(D#FMT1:41)
     D  FFPRI#                        3  0 OVERLAY(D#FMT1:51)
     D  FTPRI#                        3  0 OVERLAY(D#FMT1:54)
     D  FFCUSC                       10    OVERLAY(D#FMT1:57)
     D  FFSTA1                        5    OVERLAY(D#FMT1:67)
     D  FFSTRC                       10    OVERLAY(D#FMT1:72)
     D  FFITPC                        5    OVERLAY(D#FMT1:82)
     D  FFRUID                       10    OVERLAY(D#FMT1:87)
     D  FFAUID                       10    OVERLAY(D#FMT1:97)
     D  FFACIN                        1    OVERLAY(D#FMT1:107)
     D  FFALSC                       10    OVERLAY(D#FMT1:108)
     D  FFUSF1                       10    OVERLAY(D#FMT1:118)
     D  FFUSF2                       25    OVERLAY(D#FMT1:128)
     D  FFUSF3                       15  0 OVERLAY(D#FMT1:153)
     D  FFCCTC                       10    OVERLAY(D#FMT1:168)
     D  FFSTA2                        5    OVERLAY(D#FMT1:178)
     D  FFSTA3                        5    OVERLAY(D#FMT1:183)
     D  FFVIEI                        1    OVERLAY(D#FMT1:188)
      *
      *          ------------------------------------------
      *          P#PGI - PGMI0100 Format
      *
      *          The following table lists the fields in the PGMI0100
      *          format.
      *          It is used as a parameter for the QCLRPGMI API.
      *
      *          DP1BR# (B4)   Bytes returned to buffer
      *          DP1BA# (B4)   Bytes available for buffer
      *
      * Program creation information
      *
      *          DP1PGC (A10)  Program
      *          DP1PLC (A10)  Program library
      *          DP1OWC (A10)  Owner
      *          DP1ATC (A10)  Attribute
      *          DP1PCD (A13)  Creation date and time
      *          DP1SFC (A10)  Source file
      *          DP1SLC (A10)  Source library
      *          DP1SMC (A10)  Source member
      *          DP1SXD (A13)  Source change date and time
      *          DP1OIC (A1)   Observable info
      *          DP1USC (A1)   User profile option
      *          DP1UAC (A1)   Use adopted authority
      *          DP1LCC (A1)   Log commands
      *          DP1ARC (A1)   Allow RTVCLSRC
      *          DP1IDC (A1)   Ignore decimal data error
      *          DP1TXD (A50)  Text description
      *
      *          DP1R1  (A60)  Reserved
      *
      * Program statistics information
      *
      *          DP1MI# (B4)   Minimum number of parameters
      *          DP1MA# (B4)   Maximum number of parameters
      *          DP1PS# (B4)   Program size
      *          DP1AS# (B4)   Associated space size
      *          DP1SS# (B4)   Static storage size
      *          DP1AU# (B4)   Automatic storage size
      *          DP1NI# (B4)   Number of MI instructions
      *          DP1NO# (B4)   Number of MI ODT entries
      *          DP1PSC (A1)   Program state
      *          DP1CIC (A14)  Compiler identification
      *          DP1ERC (A6)   Earliest release that program can run
      *
      *          DP1R2  (A52)  Reserved
      *
      * Program performance information
      *
      *          DP1OPC (A1)   Optimization
      *          DP1PPC (A1)   Paging pool
      *          DP1UPC (A1)   Update PASA
      *          DP1CPC (A1)   Clear PASA
      *          DP1PAC (A1)   Paging amount
      *
      *          DP1R3  (A18)  Reserved
      *          ------------------------------------------
     D P#PMI           DS
     D DP1BR#                         4  0
     D DP1BA#                         4  0
     D DP1PGC                        10
     D DP1PLC                        10
     D DP1OWC                        10
     D DP1ATC                        10
     D DP1PCD                        13
     D DP1SFC                        10
     D DP1SLC                        10
     D DP1SMC                        10
     D DP1SXD                        13
     D DP1OIC                         1
     D DP1USC                         1
     D DP1UAC                         1
     D DP1LCC                         1
     D DP1ARC                         1
     D DP1IDC                         1
     D DP1TXD                        50
     D DP1R1                         60
     D DP1MI#                         4  0
     D DP1MA#                         4  0
     D DP1PS#                         4  0
     D DP1AS#                         4  0
     D DP1SS#                         4  0
     D DP1AU#                         4  0
     D DP1NI#                         4  0
     D DP1NO#                         4  0
     D DP1PSC                         1
     D DP1CIC                        14
     D DP1ERC                267    273
     D DP1R2                         52
     D DP1OPC                         1
     D DP1PPC                         1
     D DP1UPC                         1
     D DP1CPC                         1
     D DP1PAC                         1
     D DP1R3                         18
      *
     D                 DS
     D D#PMI#                         9B 0 INZ(348)
     D D#PMIC                        20
     D D#PMIF                         8    INZ('PGMI0100')
      *
      *          ------------------------------------------
      *          Helpdesk data structures
      *          ------------------------------------------
      /COPY QCPYLESRC,OMHLPDS
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
      *
      *          ------------------------------------------
      *          Copy member for OMQCHGIN - Change incident
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQCHGINDS
      *
      *          ------------------------------------------
      *          Copy member for OMQUSIIN - Incident user interface
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQUSIINDS
      *
      *          ------------------------------------------
      *          Copy member for OMQCHGCC - Change call track
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQCHGCCDS
      *
      *          ------------------------------------------
      *          Copy member for OMQPRTIN - Print incident
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQPRTINDS
      *
      *          ------------------------------------------
      *          Copy member for OMQUSIRE - Request user interface
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQUSIREDS
      *
      *          ------------------------------------------
      *          Parameter: Function key buffer OMQUSIIN
      *          ------------------------------------------
     D I2FUN           DS            34    OCCURS(24)
     D I2KEY#                         4B 0
     D I2TYPC                         1
     D I2TOMC                         1
     D I2KEYD                        30
     D  I2KMID                        7    OVERLAY(I2KEYD)
     D  I2KMFC                       10    OVERLAY(I2KEYD:8)
     D  I2KMLC                       10    OVERLAY(I2KEYD:18)
      *
      *          ------------------------------------------
      *          Parameter: Function key buffer OMQUSIRE
      *          ------------------------------------------
     D R2FUN           DS            34    OCCURS(24)
     D R2KEY#                         4B 0
     D R2TYPC                         1
     D R2TOMC                         1
     D R2KEYD                        30
     D  R2KMID                        7    OVERLAY(R2KEYD)
     D  R2KMFC                       10    OVERLAY(R2KEYD:8)
     D  R2KMLC                       10    OVERLAY(R2KEYD:18)
      *
      *          ------------------------------------------
      *          P#MSI - RTVM0200 Format
      *          ------------------------------------------
     D P#MSI           DS
     D  DM2BR#                 1      4B 0
     D  DM2BA#                 5      8B 0
     D  DM2MS#                 9     12B 0
     D  DM2AI#                13     16B 0
     D  DM2AOC                17     25
     D  DM2LIC                26     26
     D  DM2R1                 27     28
     D  DM2RR#                29     32B 0
     D  DM2RA#                33     36B 0
     D  DM2MR#                37     40B 0
     D  DM2MA#                41     44B 0
     D  DM2HR#                45     48B 0
     D  DM2HA#                49     52B 0
     D  MT                    53    130
     D                                     DIM(78)
      *
      *          ------------------------------------------
      *          Structure for array STR
      *          ------------------------------------------
     D @STR            DS
     D @STRC#                         4  0 INZ(1000)
      *
      *          ------------------------------------------
      *          Structures for accessing array STR
      *          ------------------------------------------
     D D#STR           DS
     D NCHEDC                         5
     D NCSTRC                        10
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#INTL          C                   CONST('V3R0M0')
     D C#NONE          C                   CONST('*NONE')
     D C#GEN           C                   CONST('*GEN')
     D C#CHG           C                   CONST('CHANGE  ')
     D C#CHGA          C                   CONST('CHANGEA ')
     D C#ADD           C                   CONST('ADD     ')
     D C#PS1S          C                   CONST('     000')
     D C#PS2S          C                   CONST('               ')
     D C#QUES          C                   CONST('*QUES')
     D C#HIST          C                   CONST('*HIST')
     D C#LIBL          C                   CONST('*LIBL     ')
      *
     D C#ALL           C                   CONST('*ALL ')
     D C#CMP           C                   CONST('*CMP ')
     D C#OPEN          C                   CONST('*OPEN')
     D C#PRF           C                   CONST('*EMPPRF   ')
      *
     D C#NOTF          C                   CONST('*NOT FOUND')
     D C#NOMS          C                   CONST('*OMS NOT INSTALLED')
      *
     D C#HGTM          C                   CONST(235959)
      *
     D C#REQ           C                   CONST('*REQUEST')
     D C#INC           C                   CONST('*INCIDENT')
     D C#ACC           C                   CONST('*ACCUMUL')
      *
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#EXIT          C                   CONST('*EXIT')
     D C#CNCL          C                   CONST('*CNCL')
      * Prototype for 'OMH902C'
     D OMH902C         PR                  EXTPGM('OMH902C')
     D PGMQ_                               LIKE(PGMQ)
      *
      *          ------------------------------------------
      *          Parameter list for OMH904C
      * Prototype for 'OMH901C'
     D OMH901C         PR                  EXTPGM('OMH901C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGDTA_                             LIKE(MSGDTA)
      *
      * Prototype for 'OMH908C'
     D OMH908C         PR                  EXTPGM('OMH908C')
     D P#OPTI_                             LIKE(P#OPTI)
     D P#INDT_                             LIKE(P#INDT)
     D P#OTDT_                             LIKE(P#OTDT)
     D P#FMIN_                             LIKE(P#FMIN)
     D P#FMOT_                             LIKE(P#FMOT)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      * Prototype for 'QWCCVTDT'
     D QWCCVTDT        PR                  EXTPGM('QWCCVTDT')
     D P#INFM_                             LIKE(P#INFM)
     D P#DTIN_                             LIKE(P#DTIN)
     D P#OTFM_                             LIKE(P#OTFM)
     D P#DTOT_                             LIKE(P#DTOT)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'OMX105'
     D OMX105          PR                  EXTPGM('OMX105')
     D FFHEDC_                             LIKE(FFHEDC)
     D P#HEDD_                             LIKE(P#HEDD)
      *
      *          ------------------------------------------
      *          Parameter list for OMX107
      * Prototype for 'OMX128'
     D OMX128          PR                  EXTPGM('OMX128')
     D FFCUSC_                             LIKE(FFCUSC)
     D P#NAMD_                             LIKE(P#NAMD)
      *
      *          ------------------------------------------
      *          Parameter list for OMX129 (get contact)
      * Prototype for 'OMX123'
     D OMX123          PR                  EXTPGM('OMX123')
     D FFHEDC_                             LIKE(FFHEDC)
     D FFSTA1_                             LIKE(FFSTA1)
     D P#STAD_                             LIKE(P#STAD)
      *
      *          ------------------------------------------
      *          Parameter list for OMX123
      *
      *          ------------------------------------------
      *          Parameter list for OMX123
      *          ------------------------------------------
      *          Aroen inserted code
      *          ------------------------------------------
      *                    PARM           FFSTA2
      *                    PARM           FFSTA3
      *          ------------------------------------------
      *          Parameter list for OMX125
      * Prototype for 'OMX126'
     D OMX126          PR                  EXTPGM('OMX126')
     D FFHEDC_                             LIKE(FFHEDC)
     D FFSTRC_                             LIKE(FFSTRC)
     D P#STRD_                             LIKE(P#STRD)
      *
      *          ------------------------------------------
      *          Parameter list for OMX128
      * Prototype for 'OMX125'
     D OMX125          PR                  EXTPGM('OMX125')
     D FFHEDC_                             LIKE(FFHEDC)
     D FFITPC_                             LIKE(FFITPC)
     D P#ITPD_                             LIKE(P#ITPD)
      *
      *          ------------------------------------------
      *          Parameter list for OMX126
      * Prototype for 'OMX1192'
     D OMX1192         PR                  EXTPGM('OMX1192')
     D P#HEDC_                             LIKE(P#HEDC)
     D P#USID_                             LIKE(P#USID)
     D P#USDC_                             LIKE(P#USDC)
      *
      *          ------------------------------------------
      *          Parameter list for OMX120
      * Prototype for 'OMX131'
     D OMX131          PR                  EXTPGM('OMX131')
     D FFHEDC_                             LIKE(FFHEDC)
     D FFALSC_                             LIKE(FFALSC)
     D P#ALSD_                             LIKE(P#ALSD)
      *
      *          ------------------------------------------
      *          Parameter list for OMX203 (Select user option)
      * Prototype for 'OMX203'
     D OMX203          PR                  EXTPGM('OMX203')
     D UOSUL#_                             LIKE(UOSUL#)
     D P#UOPC_                             LIKE(P#UOPC)
      *
      *          ------------------------------------------
      *          Parameter list for OMX305
      * Prototype for 'OMX120'
     D OMX120          PR                  EXTPGM('OMX120')
     D P#HEDC_                             LIKE(P#HEDC)
     D P#OQL#_                             LIKE(P#OQL#)
     D P#DESC_                             LIKE(P#DESC)
      *
      *          ------------------------------------------
      *          Parameter list for OMX123
      * Prototype for 'OMH027'
     D OMH027          PR                  EXTPGM('OMH027')
     D P#HEDC_                             LIKE(P#HEDC)
     D P#INC#_                             LIKE(P#INC#)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH029 (Get info)
      * Prototype for 'OMH022'
     D OMH022          PR                  EXTPGM('OMH022')
     D P#HEDC_                             LIKE(P#HEDC)
     D P1STRC_                             LIKE(P1STRC)
     D P1RES#_                             LIKE(P1RES#)
     D P1SUL#_                             LIKE(P1SUL#)
     D P1STAT_                             LIKE(P1STAT)
     D P2STRC_                             LIKE(P2STRC)
     D P2RES#_                             LIKE(P2RES#)
     D P2SUL#_                             LIKE(P2SUL#)
     D P2STAT_                             LIKE(P2STAT)
      *
      *          ------------------------------------------
      *          Parameter list for QCMDEXEC
      * Prototype for 'OMQPRTIN'
     D OMQPRTIN        PR                  EXTPGM('OMQPRTIN')
     D I3INTL_                             LIKE(I3INTL)
     D I3ACTC_                             LIKE(I3ACTC)
     D I3BUF_                              LIKE(I3BUF)
     D I3PRC_                              LIKE(I3PRC)
     D I3ERR_                              LIKE(I3ERR)
     D I3FLD_                              LIKE(I3FLD)
      *
      *          ------------------------------------------
      *          Parameter list for OMQUSIRE
      * Prototype for 'OMQPRTIN'
     D OMQPRTIN_001    PR                  EXTPGM('OMQPRTIN')
      * Prototype for 'OMX305'
     D OMX305          PR                  EXTPGM('OMX305')
     D P#QOAI_                             LIKE(P#QOAI)
     D P#QUES_                             LIKE(P#QUES)
     D P#ANSC_                             LIKE(P#ANSC)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH0071
      * Prototype for 'OMH906C'
     D OMH906C         PR                  EXTPGM('OMH906C')
     D P#OBJC_                             LIKE(P#OBJC)
     D P#OBJL_                             LIKE(P#OBJL)
     D P#OBJT_                             LIKE(P#OBJT)
     D P#OBJD_                             LIKE(P#OBJD)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH908C
      * Prototype for 'OMH020C'
     D OMH020C         PR                  EXTPGM('OMH020C')
     D P#OBJL_                             LIKE(P#OBJL)
     D P#OBJC_                             LIKE(P#OBJC)
     D P#STAT_                             LIKE(P#STAT)
      *
      *
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter list for OMX1192
      * Prototype for 'QCLRPGMI'
     D QCLRPGMI        PR                  EXTPGM('QCLRPGMI')
     D P#PMI_                              LIKEDS(P#PMI)
     D D#PMI#_                             LIKE(D#PMI#)
     D D#PMIF_                             LIKE(D#PMIF)
     D D#PMIC_                             LIKE(D#PMIC)
     D P#ERR_                              LIKE(P#ERR)
      *
      *          ------------------------------------------
      *          Parameter list for OMH902C
      * Prototype for P#OPGM
     D P#OPGM_001      PR                  EXTPGM(P#OPGM)
      * Prototype for P#OPGM
     D P#OPGM_002      PR                  EXTPGM(P#OPGM)
     D P#ENTC_                             LIKE(P#ENTC)
     D D#INC_                              LIKEDS(D#INC)
     D P#OPT1_                             LIKE(P#OPT1)
     D P#STAT_                             LIKE(P#STAT)
      *
      *
      *          ------------------------------------------
      *          Parameter list for OMH949C
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
      * Prototype for 'OMH961'
     D OMH961          PR                  EXTPGM('OMH961')
     D PGM_                                LIKEDS(PGM)
     D P#FILE_                             LIKE(P#FILE)
      *
      *          ------------------------------------------
      *          Parameter list for OMO004  (Work with Fixes)
      * Prototype for 'QMHMOVPM'
     D QMHMOVPM        PR                  EXTPGM('QMHMOVPM')
     D P#MSKC_                             LIKE(P#MSKC)
     D P#MSTA_                             LIKE(P#MSTA)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#PMQC_                             LIKE(P#PMQC)
     D P#PSC#_                             LIKE(P#PSC#)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'OMH021'
     D OMH021          PR                  EXTPGM('OMH021')
      * Prototype for 'OMX203'
     D OMX203_001      PR                  EXTPGM('OMX203')
      * Prototype for 'OMH0071'
     D OMH0071         PR                  EXTPGM('OMH0071')
      * Prototype for 'OMH0072'
     D OMH0072         PR                  EXTPGM('OMH0072')
      * Prototype for 'OMH015'
     D OMH015          PR                  EXTPGM('OMH015')
      * Prototype for 'OMH031'
     D OMH031          PR                  EXTPGM('OMH031')
      * Prototype for 'OMX129'
     D OMX129          PR                  EXTPGM('OMX129')
      * Prototype for 'OMQCHGIN'
     D OMQCHGIN        PR                  EXTPGM('OMQCHGIN')
      * Prototype for 'OMQUSIIN'
     D OMQUSIIN        PR                  EXTPGM('OMQUSIIN')
      * Prototype for 'OMQUSIRE'
     D OMQUSIRE        PR                  EXTPGM('OMQUSIRE')
      * Prototype for 'OMH904C'
     D OMH904C         PR                  EXTPGM('OMH904C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGTXT_                             LIKE(MSGTXT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH906C
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D @S              S              5  0
     D ftusf3          S             15P 0
     D k#pri#          S              3P 0
     D k#rtv#          S              5P 0
     D p#ith#          S              3P 0
     D p#oql#          S              5P 0
     D p#pri#          S              3P 0
     D p#qls#          S              5P 0
     D p#res#          S              3P 0
     D p#usf3          S             15P 0
     D p1res#          S              3P 0
     D p2res#          S              3P 0
     D s#pri#          S              3P 0
     D s#rr01          S              5P 0
     D w#crc1          S              5P 0
     D w#crc3          S              5P 0
     D w#crc4          S              5P 0
     D w#valc          S              5P 0
     D DO_X            S              7  0
     D DO_X2           S              7  0
     D FIX#            S             10A
     D CONM            S             50A
     D APNM            S             30A
     D AUTC            S              1A
     D EOF1            S                   LIKE(INDI)
     D EROR01          S                   LIKE(INDI)
     D EROR02          S                   LIKE(INDI)
     D EROR03          S                   LIKE(INDI)
     D EROR04          S                   LIKE(INDI)
     D EROR05          S                   LIKE(INDI)
     D EROR06          S                   LIKE(INDI)
     D FTAUID          S                   LIKE(AUID)
     D FTCCTC          S                   LIKE(CCTC)
     D FTCUSC          S                   LIKE(CUSC)
     D FTITPC          S                   LIKE(ITPC)
     D FTRUID          S                   LIKE(RUID)
     D FTSTRC          S                   LIKE(STRC)
     D FTUSF1          S                   LIKE(USF1)
     D FTUSF2          S                   LIKE(USF2)
     D I#ANS1          S                   LIKE(INDI)
     D I#CHIN          S                   LIKE(INDI)
     D I#ERR1          S                   LIKE(INDI)
     D I#HQU1          S                   LIKE(INDI)
     D I#H015          S                   LIKE(INDI)
     D I#H021          S                   LIKE(INDI)
     D I#H031          S                   LIKE(INDI)
     D I#INL4          S                   LIKE(INDI)
     D I#MSGI          S                   LIKE(INDI)
     D I#OPTI          S                   LIKE(INDI)
     D I#PRTI          S                   LIKE(INDI)
     D I#PSSR          S              1
     D I#QAH1          S                   LIKE(INDI)
     D I#SC02          S                   LIKE(INDI)
     D I#UOL1          S                   LIKE(INDI)
     D I#USII          S                   LIKE(INDI)
     D I#USIR          S                   LIKE(INDI)
     D I#VIEW          S                   LIKE(INDI)
     D I#X129          S              1
     D I#X203          S                   LIKE(INDI)
     D I#0071          S                   LIKE(INDI)
     D I#0072          S                   LIKE(INDI)
     D IP              S              2  0
     D IX              S              5  0
     D K#FMTC          S                   LIKE(FMTC)
     D K#HEDC          S                   LIKE(HEDC)
     D K#INC#          S                   LIKE(INC#)
     D K#IRAH          S                   LIKE(IRAH)
     D K#IRIT          S                   LIKE(IRIT)
     D K4HEDC          S                   LIKE(HEDC)
     D K4INC#          S                   LIKE(INC#)
     D LOOP01          S                   LIKE(INDI)
     D LOOP02          S                   LIKE(INDI)
     D LOOP03          S                   LIKE(INDI)
     D LOOP04          S                   LIKE(INDI)
     D LOOP05          S                   LIKE(INDI)
     D LOOP06          S                   LIKE(INDI)
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#APNM          S                   LIKE(APNM)
     D P#CONM          S                   LIKE(CONM)
     D P#ACTC          S                   LIKE(ACTC)
     D P#AUTC          S                   LIKE(AUTC)
     D P#ACTN          S                   LIKE(ACTN)
     D P#ALSD          S                   LIKE(ALSD)
     D P#ANSC          S                   LIKE(ANSC)
     D P#CCTC          S                   LIKE(CCTC)
     D P#CDE#          S                   LIKE(CTN#)
     D P#CTN#          S                   LIKE(CTN#)
     D P#CUSC          S                   LIKE(CUSC)
     D P#DESC          S                   LIKE(DESC)
     D P#ENTC          S              3
     D P#FILE          S             57
     D P#FIX#          S                   LIKE(FIX#)
     D P#FMIN          S              1
     D P#FMOT          S              1
     D P#HEDD          S                   LIKE(HEDD)
     D P#HST           S              1
     D P#INC#          S                   LIKE(INC#)
     D P#INDT          S              6  0
     D P#INFM          S             10
     D P#INKC          S                   LIKE(INDI)
     D P#ITPC          S                   LIKE(ITPC)
     D P#ITPD          S                   LIKE(ITPD)
     D P#LIB           S                   LIKE(ISFL)
     D P#MSD           S             20
     D P#MSTA          S             40
     D P#NAMD          S                   LIKE(NAMD)
     D P#OBJC          S                   LIKE(OBJC)
     D P#OBJD          S                   LIKE(OBJD)
     D P#OBJL          S                   LIKE(OBJL)
     D P#OBJT          S                   LIKE(OBJT)
     D P#OPGM          S             21
     D P#OPTI          S              1
     D P#OPT1          S              2
     D P#OTDT          S              6  0
     D P#OTFM          S             10
     D P#OUTC          S              6
     D P#OUTQ          S                   LIKE(INC#)
     D P#QOAI          S                   LIKE(INDI)
     D P#QUES          S                   LIKE(QUES)
     D P#SEV1          S                   LIKE(SEV1)
     D P#SEV2          S                   LIKE(SEV2)
     D P#SEV3          S                   LIKE(SEV3)
     D P#STAD          S                   LIKE(STAD)
     D P#STAT          S                   LIKE(STAT)
     D P#STRC          S                   LIKE(STRC)
     D P#STRD          S                   LIKE(STRD)
     D P#SUL#          S                   LIKE(SUL#)
     D P#UOPC          S                   LIKE(UOPC)
     D P#USDC          S                   LIKE(USDC)
     D P#USF1          S                   LIKE(USF1)
     D P#USF2          S                   LIKE(USF2)
     D P#USID          S                   LIKE(USID)
     D P1STAT          S                   LIKE(STAT)
     D P1STRC          S                   LIKE(STRC)
     D P1SUL#          S                   LIKE(SUL#)
     D P2STAT          S                   LIKE(STAT)
     D P2STRC          S                   LIKE(STRC)
     D P2SUL#          S                   LIKE(SUL#)
     D RRN01           S              5  0
     D RRN03           S              5  0
     D RRN04           S              5  0
     D S#END           S                   LIKE(INDI)
     D S#FMT1          S                   LIKE(D#FMT1)
     D S#HEDC          S                   LIKE(HEDC)
     D S#INC#          S                   LIKE(INC#)
     D S#POS1          S                   LIKE(HEDC)
     D S#SDTA          S                   LIKE(SDTA)
     D S#SUL#          S                   LIKE(SUL#)
     D S#VIEI          S                   LIKE(INDI)
     D SEL1            S                   LIKE(INDI)
     D S1INC#          S                   LIKE(INC#)
     D W#EOFI          S                   LIKE(INDI)
     D W#ERR           S                   LIKE(INDI)
     D W#INC#          S                   LIKE(INC#)
     D W#INCA          S                   LIKE(INDI)
     D W#INCF          S                   LIKE(INDI)
     D W#MODE          S              8
     D W#OMS           S                   LIKE(INDI)
     D W#OPTI          S                   LIKE(FMOPTI)
     D W#OPTS          S                   LIKE(FMOPTS)
     D W#STR           S             15
     D W#TEMP          S             50
     D W#VIEI          S                   LIKE(INDI)
     D W#WRKI          S              1
      *----------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *----------------------------------------------------------------------
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Main line                                                    *
      * ------------------------------------------------------------ *
      *
      *          ------------------------------------------
      *          Entry parameters
      *          ------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    P#CTN#
     C                   PARM                    P#CUSC
     C                   PARM                    P#CCTC
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Do the processing routine if authorised
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B01
     C                   EXSR      SRVERW
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do the total last record routine
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * CLRMSG - Clear message subfile                               *
      *                                                              *
      * This routine calls a subprogram wich clears the message      *
      * subfile.                                                     *
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SNDMSG - Send message                                        *
      *                                                              *
      * This routine calls a subprogram which sends a message to the *
      * program message queue.                                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SNDMSG        BEGSR
      *
      *          ------------------------------------------
      *          Call message sender
      *          ------------------------------------------
     C                   EVAL      MSGF = 'OMHMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C  ( MSGID : MSGF : MSGLIB :
     C                             MSGDTA )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SNDAPI - Send message from OMHAPI                            *
      *                                                              *
      * This routine calls a subprogram which sends a message to the *
      * program message queue.                                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SNDAPI        BEGSR
      *
      *          ------------------------------------------
      *          Call message sender
      *          ------------------------------------------
     C                   EVAL      MSGF = 'OMHAPI'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   EVAL      MSGDTA = I2MSGD
     C                   CALLP     OMH901C  ( I2MSID : MSGF : MSGLIB :
     C                             MSGDTA )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRFUTY - Convert date From User To YYMMDD format             * /
      *                                                              * /
      * This routine calls a subprogram which converts a date to     * /
      * YYMMDD format.                                               * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRFUTY        BEGSR
      *
      *          ------------------------------------------
      *          Convert dates
      *          ------------------------------------------
     C                   EVAL      P#OTDT = *ZEROS
     C                   EVAL      P#FMIN = '?'
     C                   EVAL      P#FMOT = 'Y'
     C                   EVAL      P#OPTI = ' '
     C                   CALLP     OMH908C ( P#OPTI : P#INDT : P#OTDT :
     C                             P#FMIN : P#FMOT : P#STAT )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * PROMPT - Prompt                                             *
      *                                                             *
      * This routine calls the appropriate select routines.         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     PROMPT        BEGSR
      *
      *          ------------------------------------------
      *          Select selection program
      *          ------------------------------------------
     C                   SELECT                                                 B01
      *
      *          ------------------------------------------
      *          Field Helpdesk
      *
     C                   WHEN      FMFMTC = 'OMH01806'                          X01
     C                             AND FMFLDC = 'FFHEDC'
     C                   CALLP     OMX105 ( FFHEDC : P#HEDD )
      *
      *          ------------------------------------------
      *          Field Customer
      *          ------------------------------------------
     C                   WHEN      FMFMTC = 'OMH01806'                          X01
     C                             AND FMFLDC = 'FFCUSC'
     C                             AND *IN61 = *OFF
     C                   CALLP     OMX128 ( FFCUSC : P#NAMD )
      *
      *          ------------------------------------------
      *          Field Contact
      *          ------------------------------------------
     C                   WHEN      FMFMTC = 'OMH01806'                          X01
     C                             AND FMFLDC = 'FFCCTC'
     C                             AND *IN62 = *OFF
      *
     C                   CALL      'OMX129'      $MX129                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#X129 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#X129 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Field Status
      *          ------------------------------------------
     C                   WHEN      FMFMTC = 'OMH01806'                          X01
     C                             AND FMFLDC = 'FFSTA1'
      *
     C                   IF        FFHEDC = *BLANKS                             B02
      *
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR02 = *ON
     C                   EVAL      MSGID = 'MH01803'
     C                   EXSR      SNDMSG
      *
     C                   ELSE                                                   X02
      *
     C                   CALLP     OMX123 ( FFHEDC : FFSTA1 : P#STAD )
      *
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Field Status
      *          ------------------------------------------
     C                   WHEN      FMFMTC = 'OMH01806'                          X01
     C                             AND FMFLDC = 'FFSTA2'
      *
     C                   IF        FFHEDC = *BLANKS                             B02
      *
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR02 = *ON
     C                   EVAL      MSGID = 'MH01803'
     C                   EXSR      SNDMSG
      *
     C                   ELSE                                                   X02
      *
     C                   CALLP     OMX123 ( FFHEDC : FFSTA2 : P#STAD )
      *
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Field Status
      *          ------------------------------------------
     C                   WHEN      FMFMTC = 'OMH01806'                          X01
     C                             AND FMFLDC = 'FFSTA3'
      *
     C                   IF        FFHEDC = *BLANKS                             B02
      *
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR02 = *ON
     C                   EVAL      MSGID = 'MH01803'
     C                   EXSR      SNDMSG
      *
     C                   ELSE                                                   X02
      *
     C                   CALLP     OMX123 ( FFHEDC : FFSTA3 : P#STAD )
      *
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Field Status Route
      *          ------------------------------------------
     C                   WHEN      FMFMTC = 'OMH01806'                          X01
     C                             AND FMFLDC = 'FFSTRC'
      *
     C                   IF        FFHEDC = *BLANKS                             B02
      *
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR02 = *ON
     C                   EVAL      MSGID = 'MH01803'
     C                   EXSR      SNDMSG
      *
     C                   ELSE                                                   X02
      *
     C                   CALLP     OMX126 ( FFHEDC : FFSTRC : P#STRD )
      *
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Field Incident type
      *          ------------------------------------------
     C                   WHEN      FMFMTC = 'OMH01806'                          X01
     C                             AND FMFLDC = 'FFITPC'
      *
     C                   IF        FFHEDC = *BLANKS                             B02
      *
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR02 = *ON
     C                   EVAL      MSGID = 'MH01803'
     C                   EXSR      SNDMSG
      *
     C                   ELSE                                                   X02
      *
     C                   CALLP     OMX125 ( FFHEDC : FFITPC : P#ITPD )
      *
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Field Registered by user
      *          ------------------------------------------
     C                   WHEN      FMFMTC = 'OMH01816'                          X01
     C                             AND FMFLDC = 'FFRUID'
     C                   EVAL      P#USID = FFRUID
     C                   EVAL      P#USID = *BLANKS
     C                   EVAL      P#USDC = *BLANKS
     C                   CALLP     OMX1192 ( P#HEDC : P#USID : P#USDC )
     C                   EVAL      FFRUID = P#USID
      *
      *          ------------------------------------------
      *          Field Allocated by user
      *          ------------------------------------------
     C                   WHEN      FMFMTC = 'OMH01816'                          X01
     C                             AND FMFLDC = 'FFAUID'
     C                   EVAL      P#USID = FFAUID
     C                   EVAL      P#USID = *BLANKS
     C                   EVAL      P#USDC = *BLANKS
     C                   CALLP     OMX1192 ( P#HEDC : P#USID : P#USDC )
     C                   EVAL      FFAUID = P#USID
      *
      *          ------------------------------------------
      *          Field Answer list
      *          ------------------------------------------
     C                   WHEN      FMFMTC = 'OMH01816'                          X01
     C                             AND FMFLDC = 'FFALSC'
      *
     C                   IF        FFHEDC = *BLANKS                             B02
      *
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR02 = *ON
     C                   EVAL      MSGID = 'MH01803'
     C                   EXSR      SNDMSG
      *
     C                   ELSE                                                   X02
      *
     C                   CALLP     OMX131 ( FFHEDC : FFALSC : P#ALSD )
      *
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Wrong cursor position for prompt
      *          ------------------------------------------
     C                   OTHER                                                  X01
      *
     C                   EVAL      MSGID = 'PMT0002'
     C                   EXSR      SNDMSG
      *
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRVERW - Main program processing routine                     *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Enter loop to call screen 1 processing
      *          ------------------------------------------
     C                   EXSR      SRSC01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC01 - Work with Incidents                                 *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSC01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EXSR      SRIN01
      *
      *          ------------------------------------------
      *          Do while no end
      *          ------------------------------------------
     C                   DOU       LOOP01 = *OFF                                B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR01 = *OFF                                B02
      *
      *          ------------------------------------------
      *          If subfile is empty, then write record to
      *          clear some lines on the display
      *          ------------------------------------------
     C                   IF        RRN01 = *ZEROS                               B03
     C                   Z-ADD     1             LIN
     C                   Z-ADD     1             COL
     C                   WRITE     OMH018CL
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH01803
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if
      *          records found
      *          ------------------------------------------
     C                   IF        RRN01 = *ZEROS                               B03
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   EXSR      SRSFEF
      *
      *          ------------------------------------------
      *          CMD-3 and CMD-12 is exit
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B03
     C                             OR *INKL = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   EVAL      W#CRC1 = D#CRC#
      *
      *          ------------------------------------------
      *          Clear message queue
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Setof screen error indicators
      *          ------------------------------------------
     C                   EVAL      *IN70 = *OFF
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      EROR01 = *OFF
     C                   EVAL      LOOP01 = *OFF
      *
      *          ------------------------------------------
      *          CMD-5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B03
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F6=Add incident
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B04
     C                   EXSR      SRINAD
     C                   IF        W#INCA = *ON                                 B05
     C                   EXSR      SRSC05
     C                   EXSR      SRRF01
     C                   ENDIF                                                  E05
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          F11=View
      *          ------------------------------------------
     C                   IF        *INKK = *ON                                  B05
     C                   IF        RRN01 > *ZEROS                               B06
     C                   EXSR      SRVW01
     C                   ENDIF                                                  E06
     C                   IF        CCRRN > *ZEROS                               B06
     C                   Z-ADD     CCRRN         CRRN01
     C                   ELSE                                                   X06
     C                   Z-ADD     W#CRC1        CRRN01
     C                   ENDIF                                                  E06
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          F17=Change selection
      *          ------------------------------------------
     C                   IF        *INKR = *ON                                  B06
     C                   EXSR      SRSC02
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X06
      *
      *          ------------------------------------------
      *          F18=Add new call
      *          ------------------------------------------
     C                   IF        *INKS = *ON                                  B07
     C                   EXSR      SRCAAD
      *
     C                   EXSR      SRSC05
     C                   EXSR      SRIN01
      *
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X07
      *
      *          ------------------------------------------
      *          F23=More options
      *          ------------------------------------------
     C                   IF        *INKX = *ON                                  B08
     C                   SELECT                                                 B09
     C                   WHEN      *IN36 = *OFF                                 X09
     C                             AND *IN37 = *OFF
     C                   EVAL      *IN37 = *ON
     C                   WHEN      *IN36 = *OFF                                 X09
     C                             AND *IN37 = *ON
     C                   EVAL      *IN36 = *ON
     C                   WHEN      *IN36 = *ON                                  X09
     C                             AND *IN37 = *ON
     C                   EVAL      *IN36 = *OFF
     C                   EVAL      *IN37 = *OFF
     C                   ENDSL                                                  E09
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X08
      *
      *          ------------------------------------------
      *          F24=More keys
      *          ------------------------------------------
     C                   IF        *INKY = *ON                                  B09
     C                   IF        *IN38 = *ON                                  B10
     C                   EVAL      *IN38 = *OFF
     C                   ELSE                                                   X10
     C                   EVAL      *IN38 = *ON
     C                   ENDIF                                                  E10
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X09
      *
      *          ------------------------------------------
      *          F24=Roll up
      *          ------------------------------------------
     C                   IF        *IN22 = *ON                                  B10
     C                   EXSR      SRRU01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X10
      *
      *          ------------------------------------------
      *          Edit
      *          ------------------------------------------
     C                   EXSR      SRED01
     C                   ENDIF                                                  E10
     C                   ENDIF                                                  E09
     C                   ENDIF                                                  E08
     C                   ENDIF                                                  E07
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors or CMD key
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do processing
      *          ------------------------------------------
     C                   EXSR      SRPR01
      *
      *          ------------------------------------------
      *          End process loop
      *          ------------------------------------------
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRIN01 - Work with Incidents - Initialization                *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Clear positioning fields
      *          ------------------------------------------
     C                   MOVE      C#PS1S        FMPS1S
     C                   MOVE      C#PS2S        FMPS2S
      *
      *          ------------------------------------------
      *          Set indicator 'OMS installed Y/N' to blank
      *          ------------------------------------------
     C                   EVAL      W#OMS = *BLANKS
      *
      *          ------------------------------------------
      *          If call track number is empty (i.e. called
      *          directly from menu)
      *          ------------------------------------------
     C                   IF        P#CTN# = *BLANKS                             B01
      *
      *          ------------------------------------------
      *          Retrieve the subset fields
      *          ------------------------------------------
     C                   EVAL      %SUBST(K#FMTC:1:8) = 'OMH01806'
     C                   EXSR      SRRF02
      *
      *          ------------------------------------------
      *          Check if the list is subsetted and send a
      *          message if so
      *          ------------------------------------------
     C                   IF        FFINC# <> *BLANKS                            B02
     C                             OR FTINC# <> *ALL'9'
     C                             OR FFCTN# <> *BLANKS
     C                             OR FTCTN# <> *ALL'9'
     C                             OR FFPRI# <> *ZEROS
     C                             OR FTPRI# <> *ALL'9'
     C                             OR FFCUSC <> *BLANKS
     C                             OR FFCCTC <> *BLANKS
     C                             OR FFSTRC <> *BLANKS
     C                             OR FFITPC <> *BLANKS
     C                             OR FFRUID <> *BLANKS
     C                             OR FFAUID <> *BLANKS
     C                             OR FFACIN <> *OFF
     C                             OR FFALSC <> *BLANKS
     C                             OR FFUSF1 <> *BLANKS
     C                             OR FFUSF2 <> *BLANKS
     C                             OR FFUSF3 <> *ZEROS
     C                   EXSR      CLRMSG
     C                   EVAL      MSGID = 'SUB0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E02
      *
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Reset the selection criteria
      *          ------------------------------------------
     C                   EXSR      SRRS02
      *
      *          ------------------------------------------
      *          Set selection limits equal to parameter
      *          ------------------------------------------
     C                   EVAL      FFCTN# = P#CTN#
     C                   EVAL      FTCTN# = P#CTN#
      *
      *          ------------------------------------------
      *          Default status to *ALL
      *          ------------------------------------------
     C                   EVAL      FFSTA1 = C#ALL
     C                   EVAL      FFSTA2 = C#ALL
     C                   EVAL      FFSTA3 = C#ALL
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If parameters are filled then set display
      *          indicator to protect the passed subset fields.
      *          ------------------------------------------
     C                   IF        P#CTN# <> *BLANKS                            B01
     C                   EVAL      *IN60 = *ON
     C                   ELSE                                                   X01
     C                   EVAL      *IN60 = *OFF
     C                   ENDIF                                                  E01
      *
     C                   IF        P#CUSC <> *BLANKS                            B01
     C                   EVAL      *IN61 = *ON
     C                   ELSE                                                   X01
     C                   EVAL      *IN61 = *OFF
     C                   ENDIF                                                  E01
      *
     C                   IF        P#CCTC <> *BLANKS                            B01
     C                   EVAL      *IN62 = *ON
     C                   ELSE                                                   X01
     C                   EVAL      *IN62 = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the key fields
      *          ------------------------------------------
     C                   EVAL      K#HEDC = FFHEDC
     C                   MOVE      *HIVAL        K#PRI#
     C                   MOVE      *HIVAL        K#INC#
      *
      *          ------------------------------------------
      *          Put helpdesk code in save field
      *          ------------------------------------------
     C                   EVAL      S#POS1 = FFHEDC
      *
      *          ------------------------------------------
      *          Set subfile view to 3
      *          ------------------------------------------
     C                   EVAL      I#VIEW = '3'
      *
      *          ------------------------------------------
      *          Fill sort view
      *          ------------------------------------------
     C                   MOVE      FFVIEI        FMVIEI
     C                   EVAL      S#VIEI = FFVIEI
      *
      *          ------------------------------------------
      *          Set *IN40 equal to value of FMVIEI
      *          ------------------------------------------
     C                   MOVE      FMVIEI        *IN40
      *
      *          ------------------------------------------
      *          Get allowed status routes
      *          ------------------------------------------
     C                   EXSR      SRSTR
      *
      *          ------------------------------------------
      *          Do refresh subroutine
      *          ------------------------------------------
     C                   EXSR      SRRF01
      *
      *          ------------------------------------------
      *          If parameter call track number is filled
      *          and no incidents were found, then start
      *          with subroutine to add an incident if this was
      *          call was not added in this program with F18 (*INKS)
      *          ------------------------------------------
     C                   IF        *IN60 = *ON                                  B01
     C                             AND W#INCF = *OFF
     C                             AND *INKS = *OFF
     C                   EXSR      SRINAD
     C                   IF        W#INCA = *ON                                 B02
     C                   EXSR      SRSC05
     C                   EXSR      SRRF01
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRF01 - Work with Incidents - Refresh                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRF01        BEGSR
      *
      *          ------------------------------------------
      *          Fill the customer and contact subsets.
      *          ------------------------------------------
     C                   IF        P#CUSC <> *BLANKS                            B01
     C                   EVAL      FFCUSC = P#CUSC
     C                   EVAL      FTCUSC = P#CUSC
     C                   ENDIF                                                  E01
      *
     C                   IF        P#CCTC <> *BLANKS                            B01
     C                   EVAL      FFCCTC = P#CCTC
     C                   EVAL      FTCCTC = P#CCTC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Set 'Incidents added' to *OFF
      *          ------------------------------------------
     C                   EVAL      W#INCA = *OFF
      *
      *          ------------------------------------------
      *          Set 'Incidents found' to *OFF
      *          ------------------------------------------
     C                   EVAL      W#INCF = *OFF
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      RRN01 = *ZEROS
     C                   EVAL      IX = *ZEROS
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH01802
     C                   WRITE     OMH01805
     C                   WRITE     OMH01808
     C                   WRITE     OMH01819
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Reset current sort view
      *          ------------------------------------------
     C                   MOVE      S#VIEI        FMVIEI
      *
      *          ------------------------------------------
      *          Determine File to be used
      *          ------------------------------------------
     C                   SELECT                                                 B01
      *
     C                   WHEN      FMVIEI = '0'                                 X01
      *
     C                   IF        P#CTN# = *BLANKS                             B02
      *
     C                   IF        FFSTA1 = C#ALL                               B03
     C                             OR FFSTA1 = C#CMP
     C                             OR FFSTA2 = C#ALL                            B03
     C                             OR FFSTA2 = C#CMP
     C                             OR FFSTA3 = C#ALL                            B03
     C                             OR FFSTA3 = C#CMP
     C                   EVAL      W#VIEI = '2'
     C                   ELSE                                                   X03
     C                   EVAL      W#VIEI = '4'
     C                   ENDIF                                                  E03
      *
     C                   ELSE                                                   X02
      *
     C                   EVAL      W#VIEI = '6'
      *
     C                   ENDIF                                                  E02
      *
     C                   WHEN      FMVIEI = '1'                                 X01
      *
     C                   IF        P#CTN# = *BLANKS                             B02
      *
     C                   IF        FFSTA1 = C#ALL                               B03
     C                             OR FFSTA1 = C#CMP                            B03
     C                             OR FFSTA2 = C#ALL                            B03
     C                             OR FFSTA2 = C#CMP
     C                             OR FFSTA3 = C#ALL                            B03
     C                             OR FFSTA3 = C#CMP
     C                   EVAL      W#VIEI = '1'
     C                   ELSE                                                   X03
     C                   EVAL      W#VIEI = '3'
     C                   ENDIF                                                  E03
      *
     C                   ELSE                                                   X02
      *
     C                   EVAL      W#VIEI = '5'
      *
     C                   ENDIF                                                  E02
      *
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          Position to first record
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      W#VIEI = '1'                                 X01
     C     $NC1K1        SETLL     OMINCJ1
     C                   WHEN      W#VIEI = '2'                                 X01
     C     $NC2K1        SETLL     OMINCJ2
     C                   WHEN      W#VIEI = '3'                                 X01
     C     $NC3K1        SETLL     OMINCJ3
     C                   WHEN      W#VIEI = '4'                                 X01
     C     $NC4K1        SETLL     OMINCJ4
     C                   WHEN      W#VIEI = '5'                                 X01
     C     $NC5K1        SETLL     OMINCJ5
     C                   WHEN      W#VIEI = '6'                                 X01
     C     $NC6K1        SETLL     OMINCJ6
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          Read first record
      *          ------------------------------------------
     C                   EXSR      SRRD01
      *
      *          ------------------------------------------
      *          Save HelpDesk code of first subfile record
      *          ------------------------------------------
     C                   IF        EOF1 <> *ON                                  B01
     C                   EVAL      S#POS1 = NCHEDC
     C                   ELSE                                                   X01
     C                   EVAL      S#POS1 = *BLANKS
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       EOF1 = *OFF                                  B01
     C                             AND IX < 9
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   EXSR      SRFR01
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C                   EXSR      SRRD01
      *
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Fill current subfile relative record
      *          number to display first subfile page
      *          ------------------------------------------
     C                   Z-ADD     1             CRRN01
      *
      *          ------------------------------------------
      *          Save overall relative record number
      *          ------------------------------------------
     C                   EVAL      S#RR01 = RRN01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRFR01 - Fill subfile records for screen 1                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRFR01        BEGSR
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      NCHEDC        FMHEDC
     C                   MOVE      NCINC#        FMINC#
     C                   Z-ADD     NCPRI#        FMPRI#
     C                   MOVE      NCCTN#        FMCTN#
     C                   MOVE      NCSUL#        FMSUL#
     C                   MOVE      NCSTAT        FMSTAT
     C                   MOVE      NCSTRC        FMSTRC
     C                   MOVE      NCRUID        FMRUID
     C                   MOVE      NCAUID        FMAUID
     C                   MOVE      NCITPC        FMITPC
     C                   Z-ADD     NCINDT        FMINDT
     C                   Z-ADD     NCINTM        FMINTM
     C                   MOVE      NCSHID        FMSHID
     C                   Z-ADD     NCINDT        D#IDAT
     C                   EXSR      SRFYTU
     C                   Z-ADD     D#DTOT        FMIND$
     C                   MOVE      CACUSC        FMCUSC
     C                   MOVE      CACCTC        FMCCTC
     C                   MOVEL     CONAMD        FMNAMD
     C                   MOVEL     COTEL#        FMTEL#
     C                   ADD       1             RRN01
     C                   ADD       1             IX
     C                   WRITE     OMH01801
     C                   WRITE     OMH01804
     C                   WRITE     OMH01807
     C                   WRITE     OMH01818
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRD01 - Screen 1 read                                      *
      *                                                             *
      * This routine is called from the subfile refresh routine.    *
      * It reads an correct record, within the user defined limits  *
      * for the incid file OMINC. If no record could be found, then *
      * the subroutine returns a '1' in the EOF field, indicating   *
      * end of file.                                                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRD01        BEGSR
      *
      *          ------------------------------------------
      *          Do until good record found
      *          ------------------------------------------
     C                   DOU       SEL1 = *ON                                   B01
     C                             OR EOF1 = *ON
      *
      *          ------------------------------------------
      *          Read the file OMINC depending on sort view
      *          ------------------------------------------
     C                   SELECT                                                 B02
     C                   WHEN      W#VIEI = '1'                                 X02
     C                   READ      OMINCJ1
     C                   EVAL      *IN99 = %EOF
     C                   WHEN      W#VIEI = '2'                                 X02
     C                   READ      OMINCJ2
     C                   EVAL      *IN99 = %EOF
     C                   WHEN      W#VIEI = '3'                                 X02
     C                   READ      OMINCJ3
     C                   EVAL      *IN99 = %EOF
     C                   WHEN      W#VIEI = '4'                                 X02
     C                   READ      OMINCJ4
     C                   EVAL      *IN99 = %EOF
     C                   WHEN      W#VIEI = '5'                                 X02
     C                   READ      OMINCJ5
     C                   EVAL      *IN99 = %EOF
     C                   WHEN      W#VIEI = '6'                                 X02
     C                   READ      OMINCJ6
     C                   EVAL      *IN99 = %EOF
     C                   ENDSL                                                  E02
      *
      *          ------------------------------------------
      *          Set control indicators.
      *          ------------------------------------------
     C                   EVAL      EOF1 = *IN99
     C                   EVAL      SEL1 = *OFF
      *
      *          ------------------------------------------
      *          If end of file not yet reached
      *          ------------------------------------------
     C                   IF        EOF1 = *OFF                                  B02
      *
      *          ------------------------------------------
      *          If help desk greater than upper limit,
      *          then force end of read loop
      *          ------------------------------------------
     C                   IF        NCHEDC > FFHEDC                              B03
     C                   EVAL      EOF1 = *ON
     C                   LEAVE
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If view is 5 or 6, and call track number
      *          greater than parameter, then force end of
      *          read loop
      *          ------------------------------------------
     C                   IF        W#VIEI = '5'                                 B03
     C                             AND NCCTN# > P#CTN#
     C                             OR W#VIEI = '6'
     C                             AND NCCTN# > P#CTN#
     C                   EVAL      EOF1 = *ON
     C                   LEAVE
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Set 'Incidents found' to *ON
      *          ------------------------------------------
     C                   EVAL      W#INCF = *ON
      *
      *          ------------------------------------------
      *          Check if the record is within user limits
      *          ------------------------------------------
      *
      *                    --------------------------------
      *                    If Registered By must not be selected
      *                    --------------------------------
     C                   IF        FFAUID = C#NONE                              B03
     C                             AND NCAUID = *BLANKS
     C                             OR FFAUID <> C#NONE
     C                             AND NCAUID >= FFAUID
     C                             AND NCAUID <= FTAUID
      *
      *                    --------------------------------
      *                    Test the rest of the subset
      *                    --------------------------------
     C                   IF        NCHEDC = FFHEDC                              B04
     C                             AND NCINC# >= FFINC#
     C                             AND NCINC# <= FTINC#
     C                             AND NCCTN# >= FFCTN#
     C                             AND NCCTN# <= FTCTN#
     C                             AND NCPRI# >= FFPRI#
     C                             AND NCPRI# <= FTPRI#
     C                             AND CACUSC >= FFCUSC
     C                             AND CACUSC <= FTCUSC
     C                             AND CACCTC >= FFCCTC
     C                             AND CACCTC <= FTCCTC
     C                             AND NCITPC >= FFITPC
     C                             AND NCITPC <= FTITPC
     C                             AND NCRUID >= FFRUID
     C                             AND NCRUID <= FTRUID
     C                             AND NCUSF1 >= FFUSF1
     C                             AND NCUSF1 <= FTUSF1
     C                             AND NCUSF2 >= FFUSF2
     C                             AND NCUSF2 <= FTUSF2
     C                             AND NCUSF3 >= FFUSF3
     C                             AND NCUSF3 <= FTUSF3
      *
      *          ------------------------------------------
      *          If status not OK, then reject incident
      *          If status OK, then SEL1 is set to *ON
      *          ------------------------------------------
     C                   IF        FFSTA1 = C#ALL                               B05
     C                             OR FFSTA2 = C#ALL
     C                             OR FFSTA3 = C#ALL
     C                             OR NCSTAT = FFSTA1
     C                             OR NCSTAT = FFSTA2
     C                             OR NCSTAT = FFSTA3
      *
     C                   EVAL      SEL1 = *ON
      *
     C                   ELSE                                                   X05
      *
     C                   IF        FFSTA1 = C#OPEN                              B06
     C                             OR FFSTA2 = C#OPEN
     C                             OR FFSTA3 = C#OPEN
     C                             AND NCSTAT <> C#CMP
      *
     C                   EVAL      SEL1 = *ON
     C                   ELSE                                                   X06
     C                   ITER
     C                   ENDIF                                                  E06
      *
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          If we only need accumulated incidents, and
      *          this incident is itself accumulated to
      *          another incident, then reject incident
      *          ------------------------------------------
     C                   IF        FFACIN = *ON                                 B05
     C                             AND NCAIN# <> *BLANKS
     C                   EVAL      SEL1 = *OFF
     C                   ITER
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          Check status routes
      *          ------------------------------------------
     C                   IF        FFSTRC = C#PRF                               B05
     C                   EVAL      @S = @STRC#
     C                   EVAL      @S = %LOOKUP(D#STR:STR:@S)
     C                   IF        @S > 0
     C                   EVAL      *IN99 = *ON
     C                   ELSE
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      @S = 1
     C                   ENDIF
     C                   IF        *IN99 = *OFF                                 B06
     C                   IF        NCRUID <> D#USID                             B07
     C                             AND NCAUID <> D#USID                         B07
     C                   EVAL      SEL1 = *OFF
     C                   ITER
     C                   ENDIF                                                  E07
     C                   ENDIF                                                  E06
     C                   ELSE                                                   X05
     C                   IF        FFSTRC <> *BLANKS                            B06
     C                             AND FFSTRC <> NCSTRC
     C                   EVAL      SEL1 = *OFF
     C                   ITER
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          If an answer list was selected, then call
      *          program to test this incident
      *          If test fails, then reject incident
      *          ------------------------------------------
     C                   IF        FFALSC <> *BLANKS                            B05
     C                   CALL      'OMH031'      $MH031                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#H031 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#H031 = *ON
     C                   ENDIF
     C                   IF        P#STAT <> C#NORM                             B06
     C                   EVAL      SEL1 = *OFF
     C                   ITER
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          Save fields that are used for positioning
      *          ------------------------------------------
     C                   EVAL      S#HEDC = NCHEDC
     C                   MOVE      NCPRI#        S#PRI#
     C                   MOVE      NCINC#        S#INC#
      *
     C                   ENDIF                                                  E04
      *
     C                   ENDIF                                                  E03
      *
     C                   ENDIF                                                  E02
      *
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          If end of file then no more records (SFLEND)
      *          ------------------------------------------
     C                   IF        EOF1 = *ON                                   B01
     C                   EVAL      *IN92 = *OFF
     C                   ELSE                                                   X01
     C                   EVAL      *IN92 = *ON
      *
      *          ------------------------------------------
      *          Get the contact record
      *          ------------------------------------------
     C     $CO1K1        CHAIN     OMCCOL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRU01 - Roll up next screen                                *
      *                                                             *
      * This routine is called if the user pressed the roll up key  *
      * in the screen. The next ten records are read.               *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRU01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      RRN01 = S#RR01
     C                   EVAL      IX = *ZEROS
      *
      *          ------------------------------------------
      *          Position to record that was last added
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      W#VIEI = '1'                                 X01
     C     $NC1K2        SETLL     OMINCJ1
     C                   WHEN      W#VIEI = '2'                                 X01
     C     $NC2K2        SETLL     OMINCJ2
     C                   WHEN      W#VIEI = '3'                                 X01
     C     $NC3K2        SETLL     OMINCJ3
     C                   WHEN      W#VIEI = '4'                                 X01
     C     $NC4K2        SETLL     OMINCJ4
     C                   WHEN      W#VIEI = '5'                                 X01
     C     $NC5K2        SETLL     OMINCJ5
     C                   WHEN      W#VIEI = '6'                                 X01
     C     $NC6K2        SETLL     OMINCJ6
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          Read record
      *          ------------------------------------------
     C                   EXSR      SRRD01
      *
      *          ------------------------------------------
      *          Save HelpDesk code of first subfile record
      *          ------------------------------------------
     C                   IF        EOF1 <> *ON                                  B01
     C                   EVAL      S#POS1 = NCHEDC
     C                   ELSE                                                   X01
     C                   EVAL      S#POS1 = *BLANKS
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Read the first ten records if available
      *          ------------------------------------------
     C                   DOW       EOF1 = *OFF                                  B01
     C                             AND IX < 9
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   EXSR      SRFR01
      *
      *          ------------------------------------------
      *          Read Next Record
      *          ------------------------------------------
     C                   EXSR      SRRD01
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   EVAL      S#RR01 = RRN01
      *
      *          ------------------------------------------
      *          Fill current subfile relative record number
      *          to display the page just filled
      *          ------------------------------------------
     C                   IF        IX > *ZEROS                                  B01
     C     W#CRC1        ADD       9             CRRN01
     C                   IF        CRRN01 > RRN01                               B02
     C                   Z-ADD     RRN01         CRRN01
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If end of file then no more records (SFLEND)
      *          ------------------------------------------
     C                   IF        EOF1 = *ON                                   B01
     C                   EVAL      *IN92 = *OFF
     C                   ELSE                                                   X01
     C                   EVAL      *IN92 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRED01 - Work with Incidents - Edit                          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
     C                   EVAL      W#VALC = *ZEROS
     C                   EVAL      CRRN01 = *ZEROS
     C                   EVAL      I#MSGI = *OFF
     C                   EVAL      P#UOPC = *BLANKS
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   EXSR      SRSFRC
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Do if option is not blank
      *          ------------------------------------------
     C                   IF        FMOPTI <> *BLANKS                            B02
      *
     C                   MOVE      FMOPTI        W#WRKI
     C                   IF        W#WRKI = *BLANKS                             B03
     C                   MOVEL     FMOPTI        W#WRKI
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      W#WRKI        FMOPTI
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If option is ' /' and the user option was not yet
      *          retrieved, then retrieve it. If nothing was returned
      *          then update the subfile and leave.
      *          ------------------------------------------
     C                   IF        FMOPTI = ' /'                                B03
     C                   Z-ADD     RRN01         CRRN01
     C                   EVAL      EROR01 = *ON
      *
     C                   IF        P#UOPC = *BLANKS                             B04
     C                   MOVE      P#SUL#        UOSUL#
     C                   CALLP     OMX203 ( UOSUL# : P#UOPC )
     C                   EVAL      I#X203 = *ON
     C                   ENDIF                                                  E04
      *
     C                   IF        P#UOPC = *BLANKS                             B04
     C                   EVAL      MSGID = 'OPTE015'
     C                   EXSR      SNDMSG
      *
     C                   EVAL      *IN70 = *ON
     C                   EVAL      *IN93 = *ON
      *
     C                   EXSR      SRSFUP
      *
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
     C                   LEAVE
      *
     C                   ELSE                                                   X04
     C                   MOVE      P#UOPC        FMOPTI
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Check if this is a user option.
      *          The authorisation code may not be '0' (disabled)
      *          and it must be less then or equal to the user auth.
      *          ------------------------------------------
     C                   IF        I#UOL1 <> *ON                                B03
     C                   OPEN      OMHUOL1
     C                   EVAL      I#UOL1 = *ON
     C                   ENDIF                                                  E03
     C     FMOPTI        CHAIN     OMHUOL1
     C                   EVAL      *IN98 = NOT %FOUND
     C                   IF        *IN98 = *OFF
     C                   EVAL      *IN98 = (UOSUL# = ' ')                        Equal
     C                   ENDIF
     C                   IF        *IN98 = *OFF
     C                   EVAL      *IN98 = (UOSUL# > P#SUL#)                     Higher
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If option is valid, then check the number
      *          of valid codes
      *          ------------------------------------------
     C                   IF        FMOPTI = ' 1'                                B03
     C                             OR FMOPTI = ' 2'
     C                             OR FMOPTI = ' 5'
     C                             OR FMOPTI = ' 6'
     C                             OR FMOPTI = ' 8'
     C                             OR FMOPTI = ' 9'
     C                             OR FMOPTI = '10'
     C                             OR FMOPTI = '12'
     C                             OR FMOPTI = '14'
     C                             OR FMOPTI = '16'
     C                             OR FMOPTI = '18'
     C                             OR FMOPTI = '20'
     C                             OR FMOPTI = '22'
     C                             OR *IN98 = *OFF
      *
      *          ------------------------------------------
      *          For some options, we need current informa-
      *          tion, so get the record from the file
      *          ------------------------------------------
     C                   IF        FMOPTI = ' 1'                                B04
     C                             OR FMOPTI = ' 2'
      *
     C     $NC1K3        CHAIN     OMINCJ1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          If the incident is allocated to a user,
      *          which is not the current user, then error
      *          ------------------------------------------
     C                   IF        NCAUID <> *BLANKS                            B05
     C                             AND NCAUID <> D#USID
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   MOVEL(P)  NCAUID        MSGDTA
     C                   EVAL      MSGID = 'INC0005'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          If the status is *CMP, then changing the
      *          status is no longer allowed ---> error
      *          ------------------------------------------
     C                   IF        FMOPTI = ' 1'                                B05
     C                             AND NCSTAT = C#CMP                           B05
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   MOVEA     FMHEDC        MC(1)
     C                   MOVEA     FMINC#        MC(6)
     C                   MOVEA     MC            I2MSGD
     C                   MOVE      'OMQA00F'     I2MSID
     C                   EXSR      SNDAPI
     C                   ENDIF                                                  E05
      *
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If no error found so far, then continue
      *          ------------------------------------------
     C                   IF        *IN70 = *OFF                                 B04
      *
      *          ------------------------------------------
      *          If API needs to be called for validity
      *          checking, then do so in non-update mode
      *          ------------------------------------------
     C                   MOVE      *OFF          I4UPD
      *
      *          ------------------------------------------
      *          Clear fields that are used to signal errors
      *          ------------------------------------------
     C                   MOVE      *BLANKS       I4FLDA
     C                   EVAL      W#ERR = *BLANKS
      *
      *          ------------------------------------------
      *          Call subroutine to do validity checking
      *          ------------------------------------------
     C                   SELECT                                                 B05
      *
     C                   WHEN      FMOPTI = '14'                                X05
     C                   EXSR      SRINSP
      *
     C                   WHEN      FMOPTI = '22'                                X05
     C                   IF        W#OMS = *BLANKS                              B06
     C                   EXSR      SROMS
     C                   ENDIF                                                  E06
     C                   IF        W#OMS = *OFF                                 B06
     C                   EVAL      MSGID = 'OMS0005'
     C                   EXSR      SNDMSG
     C                   EVAL      W#ERR = *ON
     C                   ENDIF                                                  E06
      *
     C                   ENDSL                                                  E05
      *
      *          ------------------------------------------
      *          If I4FLDA is not blanks or W#ERR is on,
      *          then error
      *          ------------------------------------------
     C                   IF        I4FLDA <> *BLANKS                            B05
     C                             OR W#ERR = *ON
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
     C                   ADD       1             W#VALC
     C                   ENDIF                                                  E05
      *
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If option is other than valid, then error.
      *          Reverse screen fields to indicate this.
      *          ------------------------------------------
     C                   ELSE                                                   X03
      *
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
      *
      *          ------------------------------------------
      *          Send the message if not yet sent
      *          ------------------------------------------
     C                   IF        I#MSGI = *OFF                                B04
     C                   MOVEL(P)  FMOPTI        MSGDTA
     C                   EVAL      MSGID = 'OPTE021'
     C                   EXSR      SNDMSG
     C                   EVAL      I#MSGI = *ON
     C                   ENDIF                                                  E04
      *
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If an error was found, then position on
      *          the page of the first error
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B03
     C                             AND *IN70 = *ON
     C                   Z-ADD     RRN01         CRRN01
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Update subfile
      *          ------------------------------------------
     C                   EVAL      *IN93 = *ON
     C                   EXSR      SRSFUP
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, just update subfile
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   EXSR      SRSFUP
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   EXSR      SRSFRC
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          If there has been no error
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B01
      *
      *          ------------------------------------------
      *          If position-to was filled, or sort view
      *          was changed, then reposition / change view
      *          ------------------------------------------
     C                   IF        FMPS1S <> C#PS1S                             B02
     C                             OR FMPS2S <> C#PS2S                          B02
     C                             OR FMVIEI <> S#VIEI
      *
      *          ------------------------------------------
      *          Force loop
      *          ------------------------------------------
     C                   EVAL      EROR01 = *ON
      *
      *          ------------------------------------------
      *          Check value of sort view indicator
      *          ------------------------------------------
     C                   IF        FMVIEI <> '0'                                B03
     C                             AND FMVIEI <> '1'
     C                   EVAL      MSGID = 'MH01801'
     C                   EVAL      *IN71 = *ON
     C                   EXSR      SNDMSG
     C                   MOVE      C#PS1S        FMPS1S
     C                   MOVE      C#PS2S        FMPS2S
     C                   MOVE      S#VIEI        FMVIEI
     C                   Z-ADD     W#CRC1        CRRN01
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          If position-to was filled AND sort view
      *          was changed, then error
      *          ------------------------------------------
     C                   IF        FMPS1S <> C#PS1S                             B04
     C                             AND FMVIEI <> S#VIEI
     C                             OR FMPS2S <> C#PS2S                          B04
     C                             AND FMVIEI <> S#VIEI
     C                   EVAL      MSGID = 'VIE0004'
     C                   EXSR      SNDMSG
      **********           MOVE C#PS1S    FMPS1S
      **********           MOVE C#PS2S    FMPS2S
      **********           MOVE S#VIEI    FMVIEI
     C                   Z-ADD     W#CRC1        CRRN01
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          If view was changed, then fill save field
      *          and set *IN40 to show the correct selec-
      *          tion fields
      *          ------------------------------------------
     C                   IF        FMVIEI <> S#VIEI                             B05
     C                   MOVE      FMVIEI        S#VIEI
     C                   SELECT                                                 B06
     C                   WHEN      FMVIEI = '0'                                 X06
     C                   EVAL      *IN40 = *OFF
     C                   WHEN      FMVIEI = '1'                                 X06
     C                   EVAL      *IN40 = *ON
     C                   ENDSL                                                  E06
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          Process position-to
      *          ------------------------------------------
     C                   IF        FMPS1S <> C#PS1S                             B06
      *
     C                   IF        FMPS1A <> *BLANKS                            B07
     C                   EVAL      K#HEDC = FMPS1A
     C                   EVAL      FFHEDC = FMPS1A
     C                   EVAL      FMPS1A = *BLANKS
     C                   ELSE                                                   X07
     C                   EVAL      K#HEDC = S#POS1
     C                   ENDIF                                                  E07
      *
     C                   IF        FMPS1B <> *ZEROS                             B07
     C                   EVAL      K#PRI# = FMPS1B
     C                   EVAL      FMPS1B = *ZEROS
     C                   ELSE                                                   X07
     C                   MOVE      *HIVAL        K#PRI#
     C                   ENDIF                                                  E07
      *
     C                   ELSE                                                   X06
      *
     C                   IF        FMPS2S <> C#PS2S                             B07
      *
     C                   IF        FMPS2A <> *BLANKS                            B08
     C                   EVAL      K#HEDC = FMPS2A
     C                   EVAL      FFHEDC = FMPS2A
     C                   EVAL      FMPS2A = *BLANKS
     C                   ELSE                                                   X08
     C                   EVAL      K#HEDC = S#POS1
     C                   ENDIF                                                  E08
      *
      *          ------------------------------------------
      *          Special processing: if field FMPS2B is
      *          filled with 'INC', then K#INC# should be
      *          filled with 'INC99999999' (where the 9's
      *          should be read as hexadecimal FF's)
      *          ------------------------------------------
     C                   IF        FMPS2B <> *BLANKS                            B08
     C                   EVAL      IP = %CHECKR(' ':FMPS2B)
     C                   IF        IP = *ZEROS                                  B09
     C                   EVAL      IP = 10
     C                   ENDIF                                                  E09
     C                   EVAL      %SUBST(K#INC#:1:IP) = %SUBST(FMPS2B:1:IP)
     C                   EVAL      FMPS2B = *BLANKS
     C                   ELSE                                                   X08
     C                   MOVE      *HIVAL        K#INC#
     C                   ENDIF                                                  E08
      *
     C                   ENDIF                                                  E07
      *
     C                   ENDIF                                                  E06
      *
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          Call refresh routine
      *          ------------------------------------------
     C                   EXSR      SRRF01
      *
     C                   ENDIF                                                  E04
      *
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          No positioning requested
      *          ------------------------------------------
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Fill the current relative record on the
      *          page to display
      *          ------------------------------------------
     C                   Z-ADD     W#CRC1        CRRN01
      *
     C                   ENDIF                                                  E02
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRVW01 - Change the subfile view.                           *
      *                                                             *
      * This routine switches the subfile view. Two subfiles are    *
      * being maintained at the same time, and the user can switch  *
      * from one view to another without having to rebuild anything *
      *                                                             *
      * If options where keyed in one subfile and the user pressed  *
      * the change view key without enter first, then the options   *
      * are moved from one subfile to another.                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRVW01        BEGSR
      *
      *          ------------------------------------------
      *          Copy subfile 1 options to subfiles 2 + 3 + 4
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      I#VIEW = '1'                                 X01
     C                   READC(E)  OMH01801
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   DOW       *IN99 = *OFF                                 B02
      *
     C                   MOVE      FMOPTI        W#OPTI
     C                   EVAL      *IN93 = *OFF
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    OMH01801
      *
     C     RRN01         CHAIN     OMH01804
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      W#OPTI        FMOPTI
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH01804
      *
     C     RRN01         CHAIN     OMH01807
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      W#OPTI        FMOPTI
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH01807
      *
     C     RRN01         CHAIN     OMH01818
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      W#OPTI        FMOPTI
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH01818
      *
     C                   READC(E)  OMH01801
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   ENDDO                                                  E02
      *
     C                   EVAL      I#VIEW = '2'
      *
      *          ------------------------------------------
      *          Copy subfile 2 options to subfiles 1 + 3
      *          ------------------------------------------
     C                   WHEN      I#VIEW = '2'                                 X01
     C                   READC(E)  OMH01804
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   DOW       *IN99 = *OFF                                 B02
      *
     C                   MOVE      FMOPTI        W#OPTI
     C                   EVAL      *IN93 = *OFF
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    OMH01804
      *
     C     RRN01         CHAIN     OMH01801
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      W#OPTI        FMOPTI
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH01801
      *
     C     RRN01         CHAIN     OMH01807
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      W#OPTI        FMOPTI
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH01807
      *
     C     RRN01         CHAIN     OMH01818
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      W#OPTI        FMOPTI
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH01818
      *
     C                   READC(E)  OMH01804
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   ENDDO                                                  E02
      *
     C                   EVAL      I#VIEW = '3'
      *
      *          ------------------------------------------
      *          Copy subfile 3 options to subfiles 1 + 2 + 4
      *          ------------------------------------------
     C                   WHEN      I#VIEW = '3'                                 X01
     C                   READC(E)  OMH01807
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   DOW       *IN99 = *OFF                                 B02
      *
     C                   MOVE      FMOPTI        W#OPTI
     C                   EVAL      *IN93 = *OFF
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    OMH01807
      *
     C     RRN01         CHAIN     OMH01801
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      W#OPTI        FMOPTI
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH01801
      *
     C     RRN01         CHAIN     OMH01804
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      W#OPTI        FMOPTI
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH01804
      *
     C     RRN01         CHAIN     OMH01818
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      W#OPTI        FMOPTI
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH01818
      *
     C                   READC(E)  OMH01807
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   ENDDO                                                  E02
      *
     C                   EVAL      I#VIEW = '4'
      *
      *          ------------------------------------------
      *          Copy subfile 4 options to subfiles 1 + 2 + 3
      *          ------------------------------------------
     C                   WHEN      I#VIEW = '4'                                 X01
     C                   READC(E)  OMH01818
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   DOW       *IN99 = *OFF                                 B02
      *
     C                   MOVE      FMOPTI        W#OPTI
     C                   EVAL      *IN93 = *OFF
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    OMH01818
      *
     C     RRN01         CHAIN     OMH01801
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      W#OPTI        FMOPTI
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH01801
      *
     C     RRN01         CHAIN     OMH01804
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      W#OPTI        FMOPTI
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH01804
      *
     C     RRN01         CHAIN     OMH01807
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      W#OPTI        FMOPTI
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH01807
      *
     C                   READC(E)  OMH01818
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   ENDDO                                                  E02
      *
     C                   EVAL      I#VIEW = '1'
      *
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          Restore record numbers
      *          ------------------------------------------
     C                   EVAL      RRN01 = S#RR01
     C                   Z-ADD     W#CRC1        CRRN01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPR01 - Work with Incidents - Process                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          Read subfile next changed record if
      *          records in subfile and valid codes found
      *          ------------------------------------------
     C                   IF        RRN01 > *ZEROS                               B01
     C                             AND W#VALC > *ZEROS
     C                   EXSR      SRSFRC
      *
      *          ------------------------------------------
      *          Read subfile until valid option found
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Force loop of screen 1
      *          ------------------------------------------
     C                   EVAL      LOOP01 = *ON
     C                   EVAL      W#CRC1 = RRN01
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPTI        W#OPTI
     C                   MOVE      *BLANKS       FMOPTI
     C                   EXSR      SRSFUP
      *
      *          ------------------------------------------
      *          If API needs to be called for processing,
      *          then do so in update mode
      *          ------------------------------------------
     C                   MOVE      *ON           I4UPD
      *
      *          ------------------------------------------
      *          Do action on option
      *          ------------------------------------------
     C                   SELECT                                                 B03
      *
      *          ------------------------------------------
      *          Set incident status
      *          ------------------------------------------
     C                   WHEN      W#OPTI = ' 1'                                X03
     C                   EXSR      SRINST
      *
      *          ------------------------------------------
      *          Change incident
      *          ------------------------------------------
     C                   WHEN      W#OPTI = ' 2'                                X03
     C                   EVAL      W#MODE = C#CHG
     C                   EXSR      SRINCH
      *
      *          ------------------------------------------
      *          Display incident
      *          ------------------------------------------
     C                   WHEN      W#OPTI = ' 5'                                X03
     C                   EXSR      SRINDS
      *
      *          ------------------------------------------
      *          Print incident
      *          ------------------------------------------
     C                   WHEN      W#OPTI = ' 6'                                X03
     C                   EXSR      SRINPR
      *
      *          ------------------------------------------
      *          Display log
      *          ------------------------------------------
     C                   WHEN      W#OPTI = ' 8'                                X03
     C                   EXSR      SRINLG
      *
      *          ------------------------------------------
      *          Allocate incident
      *          ------------------------------------------
     C                   WHEN      W#OPTI = ' 9'                                X03
     C                   EXSR      SRINAL
      *
      *          ------------------------------------------
      *          Link incident
      *          ------------------------------------------
     C                   WHEN      W#OPTI = '10'                                X03
     C                   EXSR      SRSC06
      *
      *          ------------------------------------------
      *          Display relations
      *          ------------------------------------------
     C                   WHEN      W#OPTI = '12'                                X03
     C                   EXSR      SRSC03
      *
      *          ------------------------------------------
      *          Work with spooled files
      *          ------------------------------------------
     C                   WHEN      W#OPTI = '14'                                X03
     C                   EXSR      SRINSP
      *
      *          ------------------------------------------
      *          Start Question & Answer
      *          ------------------------------------------
     C                   WHEN      W#OPTI = '16'                                X03
     C                   EXSR      SRQUES
      *
      *          ------------------------------------------
      *          Work with Q & A history.
      *          ------------------------------------------
     C                   WHEN      W#OPTI = '18'                                X03
      *
      *                    --------------------------------
      *                    Get the question list.
      *                    --------------------------------
     C                   EVAL      P#HEDC = FMHEDC
     C                   EVAL      P#OQL# = *ZEROS
     C                   CALLP     OMX120 ( P#HEDC : P#OQL# : P#DESC )
      *
      *                    --------------------------------
      *                    Display data if list was selected
      *                    --------------------------------
     C                   IF        P#OQL# <> *ZEROS                             B04
     C                   EXSR      SRSC04
     C                   ELSE                                                   X04
     C                   EVAL      *INKL = *ON
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Search & find
      *          ------------------------------------------
     C                   WHEN      W#OPTI = '20'                                X03
     C                   EVAL      P#INC# = FMINC#
     C                   CALLP     OMH027 ( P#HEDC : P#INC# : P#STAT )
     C                   IF        P#STAT = C#EXIT                              B04
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X04
     C                   IF        P#STAT = C#CNCL                              B05
     C                   EVAL      *INKL = *ON
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Connect request
      *          ------------------------------------------
     C                   WHEN      W#OPTI = '22'                                X03
      *
      *          ------------------------------------------
      *          User options.
      *          ------------------------------------------
     C                   WHEN      W#OPTI <> *BLANKS                            X03
     C                   EXSR      USROPT
      *
     C                   IF        I#OPTI = *ON                                 B04
     C                   EXSR      SRSFCH
     C                   MOVE      W#OPTI        FMOPTI
     C                   EVAL      *INKL = *ON
     C                   EVAL      *IN70 = *ON
     C                   EVAL      *IN93 = *ON
     C                   EXSR      SRSFUP
     C                   ENDIF                                                  E04
     C                   ENDSL                                                  E03
      *
      *          ------------------------------------------
      *          Exit if CMD-3 pressed on screen 2
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B03
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Leave loop if CMD-12 pressed on screen 2
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                   LEAVE
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next changed
      *          ------------------------------------------
     C                   EXSR      SRSFRC
      *
      *          ------------------------------------------
      *          If end of file found, then end loop
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Reset subfile relative record number
      *          ------------------------------------------
     C                   Z-ADD     W#CRC1        CRRN01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC02 - Screen 2 processing routine                        *
      *                                                             *
      * This routine displays the second screen. The second screen  *
      * is used to change the incident selection criteria. These    *
      * are used to select specific records from the incident file  *
      * OMINC.                                                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC02        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EXSR      SRIN02
      *
      *          ------------------------------------------
      *          Do while no end
      *          ------------------------------------------
     C                   DOU       LOOP02 = *OFF                                B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR02 = *OFF                                B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the screen
      *          ------------------------------------------
     C                   IF        I#SC02 = *OFF                                B03
     C                   EXFMT     OMH01806
     C                   ELSE                                                   X03
     C                   EXFMT     OMH01816
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Clear message queue
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Setof screen error indicators
      *          ------------------------------------------
     C                   EVAL      *IN(70) = *OFF
     C                   EVAL      *IN(71) = *OFF
     C                   EVAL      *IN(72) = *OFF
     C                   EVAL      *IN(73) = *OFF
     C                   EVAL      *IN(74) = *OFF
     C                   EVAL      *IN(75) = *OFF
     C                   EVAL      *IN(76) = *OFF
     C                   EVAL      *IN(77) = *OFF
     C                   EVAL      *IN(80) = *OFF
     C                   EVAL      *IN(81) = *OFF
     C                   EVAL      *IN(82) = *OFF
     C                   EVAL      *IN(83) = *OFF
     C                   EVAL      *IN(84) = *OFF
     C                   EVAL      *IN(85) = *OFF
     C                   EVAL      *IN(86) = *OFF
     C                   EVAL      *IN(87) = *OFF
     C                   EVAL      EROR02 = *OFF
     C                   EVAL      LOOP02 = *OFF
      *
      *          ------------------------------------------
      *          If F4 was pressed then retain the cursor
      *          locations else do normal positioning
      *          ------------------------------------------
     C                   IF        *INKD = *OFF                                 B03
     C                   EVAL      FMROW# = *ZEROS
     C                   EVAL      FMCOL# = *ZEROS
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Roll up and down
      *          ------------------------------------------
     C                   IF        *IN25 = *ON                                  B03
     C                             OR *IN26 = *ON                               B03
     C                   IF        *IN25 = *ON
     C                   EVAL      I#SC02 = *ON
     C                   ENDIF
     C                   IF        *IN26 = *ON
     C                   EVAL      I#SC02 = *OFF
     C                   ENDIF
     C                   EVAL      *IN25 = *OFF
     C                   EVAL      *IN26 = *OFF
     C                   EVAL      EROR02 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F4=Prompt
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B04
     C                   EXSR      PROMPT
     C                   IF        EROR02 = *ON                                 B05
     C                   EVAL      FMROW# = *ZEROS
     C                   EVAL      FMCOL# = *ZEROS
     C                   ELSE                                                   X05
     C                   EVAL      EROR02 = *ON
     C                   ENDIF                                                  E05
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          CMD-5=Refresh
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B05
     C                   EXSR      SRRF02
     C                   EVAL      EROR02 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          CMD-9=Retrieve
      *          ------------------------------------------
     C                   IF        *INKI = *ON                                  B06
     C                   EVAL      %SUBST(TVSDTA:1:188) = D#FMT1
     C                   EXSR      SRRTV
     C                   EVAL      D#FMT1 = TVSDTA
     C                   EXSR      SRFL02
     C                   EVAL      EROR02 = *ON
     C                   ELSE                                                   X06
      *
      *          ------------------------------------------
      *          CMD-14=Restore defaults
      *          ------------------------------------------
     C                   IF        *INKN = *ON                                  B07
     C                   EXSR      SRRF02
     C                   EVAL      EROR02 = *ON
     C                   ELSE                                                   X07
      *
      *          ------------------------------------------
      *          CMD-12=Cancel
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B08
     C                   EVAL      D#FMT1 = S#FMT1
     C                   ELSE                                                   X08
      *
      *          ------------------------------------------
      *          Do edit routine if not CMD-12 and selec-
      *          tion values changed or if not CMD-12 and
      *          Save as default subset is not '0'
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B09
     C                             AND D#FMT1 <> S#FMT1
     C                             OR *INKL = *OFF
     C                             AND FMDEFI <> *OFF
     C                   EXSR      SRED02
     C                   ENDIF                                                  E09
      *
     C                   ENDIF                                                  E08
     C                   ENDIF                                                  E07
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do processing if not CMD-12 and selection
      *          values changed or if not CMD-12 and Save
      *          as default subset is not '0'
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B02
     C                             AND D#FMT1 <> S#FMT1
     C                             OR *INKL = *OFF
     C                             AND FMDEFI <> *OFF
     C                   EXSR      SRPR02
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End process loop
      *          ------------------------------------------
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIN02 - Screen 2 initialization                            *
      *                                                             *
      * This routine initializes the processing of screen 2.        *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN02        BEGSR
      *
      *          ------------------------------------------
      *          Fill FFVIEI with current value of FMVIEI
      *          (This should be done before D#FMT1 is
      *          moved to S#FMT1. Otherwise, a change to
      *          FFVIEI might go unnoticed.)
      *          ------------------------------------------
     C                   MOVE      FMVIEI        FFVIEI
      *
      *          ------------------------------------------
      *          Save current selection values
      *          ------------------------------------------
     C                   EVAL      S#FMT1 = D#FMT1
      *
      *          ------------------------------------------
      *          Start with the first screen
      *          ------------------------------------------
     C                   EVAL      I#SC02 = *OFF
      *
      *          ------------------------------------------
      *          Fill FFVIEI with current value of FMVIEI
      *          ------------------------------------------
     C                   MOVE      FMVIEI        FFVIEI
      *
      *          ------------------------------------------
      *          Initialize error field for status route field
      *          ------------------------------------------
     C                   EVAL      *IN59 = *OFF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF02 - Screen 2 refresh                                   *
      *                                                             *
      * This screen resets the selection criteria by placing the    *
      * initial values in the fields.                               *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF02        BEGSR
      *
      *          ------------------------------------------
      *          Reset the selection criteria
      *          ------------------------------------------
     C                   EXSR      SRRS02
      *
      *          ------------------------------------------
      *          If not F5 pressed, then restore defaults.
      *          ------------------------------------------
     C                   IF        *INKE = *OFF                                 B01
     C                   EVAL      K#RTV# = *ZEROS
     C     $RTVK2        CHAIN(N)  OMHTVL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B02
     C                   EVAL      D#FMT1 = TVSDTA
     C                   EXSR      SRFL02
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Set selection limits equal to parameter if
      *          one was supplied.
      *          ------------------------------------------
     C                   IF        *IN60 = *ON
     C                   EVAL      FFCTN# = P#CTN#
     C                   EVAL      FTCTN# = P#CTN#
     C                   ENDIF
      *
     C                   IF        *IN61 = *ON
     C                   EVAL      FFCUSC = P#CUSC
     C                   EVAL      FTCUSC = P#CUSC
     C                   ENDIF
      *
     C                   IF        *IN62 = *ON
     C                   EVAL      FFCCTC = P#CCTC
     C                   EVAL      FTCCTC = P#CCTC
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Save as defaults is default '0'
      *          ------------------------------------------
     C                   MOVE      '0'           FMDEFI
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSTR  - Fill array with allowed status routes              *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSTR         BEGSR
      *
      *          ------------------------------------------
      *          Clear array with allowed status routes
      *          ------------------------------------------
     C                   EVAL      STR = *BLANKS
      *
      *          ------------------------------------------
      *          Set @S and @STRC# to 1000
      *          ------------------------------------------
     C                   EVAL      @STRC# = 1000
     C                   EVAL      @S = 1000
      *
      *          ------------------------------------------
      *          Check if there is a record for the user
      *          ------------------------------------------
     C     $SA1K1        SETLL     OMESAR
     C                   EVAL      *IN99 = %EQUAL
      *
      *          ------------------------------------------
      *          If there are no records for this user
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Fill array with all status routes for
      *          the helpdesk in field FFHEDC
      *          ------------------------------------------
     C     $TR1K1        SETLL     OMSTRR
     C     $TR1K1        READE     OMSTRR
     C                   EVAL      *IN99 = %EOF
      *
     C                   DOW       *IN99 = *OFF                                 B02
     C                             AND @STRC# <> *ZEROS
      *
     C                   EVAL      @S = @STRC#
      *
     C                   MOVEL     TRHEDC        STR(@S)
     C                   MOVE      TRSTRC        STR(@S)
      *
     C     @STRC#        SUB       1             @STRC#
      *
     C     $TR1K1        READE     OMSTRR
     C                   EVAL      *IN99 = %EOF
      *
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Else (there are records for this user)
      *          ------------------------------------------
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Fill array with status routes from OMESA
      *          ------------------------------------------
     C     $SA1K1        READE     OMESAR
     C                   EVAL      *IN99 = %EOF
      *
     C                   DOW       *IN99 = *OFF                                 B02
     C                             AND @STRC# <> *ZEROS
      *
     C                   EVAL      @S = @STRC#
      *
     C                   MOVEL     SAHEDC        STR(@S)
     C                   MOVE      SASTRC        STR(@S)
      *
     C     @STRC#        SUB       1             @STRC#
      *
     C     $SA1K1        READE     OMESAR
     C                   EVAL      *IN99 = %EOF
      *
     C                   ENDDO                                                  E02
      *
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRS02 - Screen 2 reset limits                              *
      *                                                             *
      * This routine resets the limits for the selection screen     *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRS02        BEGSR
      *
      *          ------------------------------------------
      *          Reset all lower and upper limits
      *          ------------------------------------------
     C                   MOVE      P#HEDC        FFHEDC
     C                   EVAL      FFINC# = *BLANKS
     C                   MOVE      *ALL'9'       FTINC#
     C                   EVAL      FFCTN# = *BLANKS
     C                   MOVE      *ALL'9'       FTCTN#
     C                   EVAL      FFPRI# = *ZEROS
     C                   MOVE      *ALL'9'       FTPRI#
     C                   EVAL      FFCUSC = *BLANKS
     C                   MOVE      *ALL'9'       FTCUSC
     C                   EVAL      FFCCTC = *BLANKS
     C                   MOVE      *ALL'9'       FTCCTC
     C                   EVAL      FFSTA1 = C#OPEN
     C                   EVAL      FFSTRC = C#PRF
     C                   EVAL      FTSTRC = C#PRF
     C                   EVAL      FFITPC = *BLANKS
     C                   MOVE      *ALL'9'       FTITPC
     C                   EVAL      FFRUID = *BLANKS
     C                   MOVE      *ALL'9'       FTRUID
     C                   EVAL      FFAUID = *BLANKS
     C                   MOVE      *ALL'9'       FTAUID
     C                   EVAL      FFACIN = *OFF
     C                   EVAL      FFALSC = *BLANKS
     C                   EVAL      FFUSF1 = *BLANKS
     C                   MOVE      *ALL'9'       FTUSF1
     C                   EVAL      FFUSF2 = *BLANKS
     C                   MOVE      *ALL'9'       FTUSF2
     C                   EVAL      FFUSF3 = *ZEROS
     C                   MOVE      *ALL'9'       FTUSF3
     C                   MOVE      '0'           FMDEFI
     C                   EVAL      FFVIEI = '0'
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRFL02 - Screen 2 fill upper limits                         *
      *                                                             *
      * This routine fills the to-fields that are used on the       *
      * selection screen.                                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRFL02        BEGSR
      *
      *          ------------------------------------------
      *          If a from-field is filled, then copy the
      *          value to the to-field
      *          ------------------------------------------
     C                   IF        FFCUSC <> *BLANK                             B01
     C                   EVAL      FTCUSC = FFCUSC
     C                   ELSE                                                   X01
     C                   MOVE      *ALL'9'       FTCUSC
     C                   ENDIF                                                  E01
      *
     C                   IF        FFCCTC <> *BLANK                             B01
     C                   EVAL      FTCCTC = FFCCTC
     C                   ELSE                                                   X01
     C                   MOVE      *ALL'9'       FTCCTC
     C                   ENDIF                                                  E01
      *
     C                   IF        FFSTRC <> *BLANK                             B01
     C                   EVAL      FTSTRC = FFSTRC
     C                   ELSE                                                   X01
     C                   MOVE      *ALL'9'       FTSTRC
     C                   ENDIF                                                  E01
      *
     C                   IF        FFITPC <> *BLANK                             B01
     C                   EVAL      FTITPC = FFITPC
     C                   ELSE                                                   X01
     C                   MOVE      *ALL'9'       FTITPC
     C                   ENDIF                                                  E01
      *
     C                   IF        FFRUID <> *BLANK                             B01
     C                   EVAL      FTRUID = FFRUID
     C                   ELSE                                                   X01
     C                   MOVE      *ALL'9'       FTRUID
     C                   ENDIF                                                  E01
      *
     C                   IF        FFAUID <> *BLANK                             B01
     C                   EVAL      FTAUID = FFAUID
     C                   ELSE                                                   X01
     C                   MOVE      *ALL'9'       FTAUID
     C                   ENDIF                                                  E01
      *
     C                   IF        FFUSF1 <> *BLANK                             B01
     C                   EVAL      FTUSF1 = FFUSF1
     C                   ELSE                                                   X01
     C                   MOVE      *ALL'9'       FTUSF1
     C                   ENDIF                                                  E01
      *
     C                   IF        FFUSF2 <> *BLANK                             B01
     C                   EVAL      FTUSF2 = FFUSF2
     C                   ELSE                                                   X01
     C                   MOVE      *ALL'9'       FTUSF2
     C                   ENDIF                                                  E01
      *
     C                   IF        FFUSF3 <> *ZEROS                             B01
     C                   EVAL      FTUSF3 = FFUSF3
     C                   ELSE                                                   X01
     C                   MOVE      *ALL'9'       FTUSF3
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If View On happens to be empty, then
      *          default to value 0 (this might happen
      *          because this field was added later)
      *          ------------------------------------------
     C                   IF        FFVIEI = *BLANKS
     C                   EVAL      FFVIEI = '0'
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED02 - Screen 2 edit                                      *
      *                                                             *
      * This routine checks if the from value does not exceed the   *
      * to value, if it does, then a message is sent, and the field *
      * contents are swapped.                                       *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED02        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      W#TEMP = *BLANKS
      *
      *          ------------------------------------------
      *          Use HIVAL when FT is blank
      *          ------------------------------------------
     C                   IF        FTINC# = *BLANK                              B01
     C                   MOVE      *ALL'9'       FTINC#
     C                   ENDIF                                                  E01
     C                   IF        FTCTN# = *BLANK                              B01
     C                   MOVE      *ALL'9'       FTCTN#
     C                   ENDIF                                                  E01
     C                   IF        FTPRI# = *ZEROS                              B01
     C                   MOVE      *ALL'9'       FTPRI#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill upper limits for fields with only
      *          one selection value
      *          ------------------------------------------
     C                   EXSR      SRFL02
      *
      *          ------------------------------------------
      *          Check the Incident number
      *          ------------------------------------------
     C                   IF        FFINC# > FTINC#                              B01
     C                   EVAL      *IN71 = *ON
     C                   EVAL      *IN81 = *ON
     C                   EVAL      %SUBST(W#TEMP:1:10) = FFINC#
     C                   EVAL      FFINC# = FTINC#
     C                   EVAL      FTINC# = W#TEMP
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check the Call track number
      *          ------------------------------------------
     C                   IF        FFCTN# > FTCTN#                              B01
     C                   EVAL      *IN72 = *ON
     C                   EVAL      *IN82 = *ON
     C                   EVAL      %SUBST(W#TEMP:1:10) = FFCTN#
     C                   EVAL      FFCTN# = FTCTN#
     C                   EVAL      FTCTN# = W#TEMP
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check the Priority
      *          ------------------------------------------
     C                   IF        FFPRI# > FTPRI#                              B01
     C                   EVAL      *IN73 = *ON
     C                   EVAL      *IN83 = *ON
     C                   EVAL      %SUBST(W#TEMP:1:3) = %EDITC(FFPRI#:'X')
     C                   EVAL      FFPRI# = FTPRI#
     C                   MOVEL     W#TEMP        FTPRI#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Send message if error found
      *          ------------------------------------------
     C                   IF        W#TEMP <> *BLANKS                            B01
     C                   EVAL      EROR02 = *ON
     C                   EVAL      MSGID = 'SWA0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If selection field for status is blanks,
      *          then default to *OPEN and force loop
      *          ------------------------------------------
     C                   IF        FFSTA1 = *BLANKS                             B01
     C                   EVAL      FFSTA1 = C#OPEN
     C                   EVAL      EROR02 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check the Helpdesk code
      *          ------------------------------------------
     C                   IF        FFHEDC = *BLANKS                             B01
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR02 = *ON
     C                   EVAL      MSGDTA = VAR(5)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If status route code is filled with a
      *          value other than *EMPPRF, then check if
      *          user is authorized for the status route
      *          ------------------------------------------
     C                   IF        FFSTRC <> *BLANKS                            B01
     C                             AND FFSTRC <> C#PRF
      *
     C                   EVAL      %SUBST(W#STR:1:5) = FFHEDC
     C                   EVAL      %SUBST(W#STR:6:10) = FFSTRC
      *
     C                   EVAL      @S = @STRC#
      *
     C                   EVAL      @S = %LOOKUP(W#STR:STR:@S)
     C                   IF        @S > 0
     C                   EVAL      *IN99 = *ON
     C                   ELSE
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      @S = 1
     C                   ENDIF
      *
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEA     FFSTRC        MC(1)
     C                   MOVEA     FFHEDC        MC(11)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'MH01804'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN59 = *ON
     C                   EVAL      *IN77 = *ON
     C                   EVAL      EROR02 = *ON
     C                   ENDIF                                                  E02
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Save as default indicator should be
      *          '0' or '1'
      *          ------------------------------------------
     C                   IF        FMDEFI <> '0'                                B01
     C                             AND FMDEFI <> '1'
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     VAR(2)        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'OPTE002'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN75 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Show accumulated only indicator should
      *          be '0' or '1'
      *          ------------------------------------------
     C                   IF        FFACIN <> '0'                                B01
     C                             AND FFACIN <> '1'
     C                   MOVEA     VAR(3)        MC(1)
     C                   MOVEA     VAR(2)        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'OPTE002'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN76 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          View should be '0' or '1'
      *          ------------------------------------------
     C                   IF        FFVIEI <> '0'                                B01
     C                             AND FFVIEI <> '1'
     C                   MOVEA     VAR(6)        MC(1)
     C                   MOVEA     VAR(2)        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'OPTE002'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN87 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR02 - Process screen 2                                   *
      *                                                             *
      * This routine calls the first screen refresh routine to      *
      * rebuild the first subfile                                   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR02        BEGSR
      *
      *          ------------------------------------------
      *          Fill save field with subset data.
      *          ------------------------------------------
     C                   EVAL      S#SDTA = *BLANKS
     C                   EVAL      %SUBST(S#SDTA:1:188) = D#FMT1
      *
      *          ------------------------------------------
      *          If this must be saved as defaults then save
      *          as defaults and save selections.
      *          ------------------------------------------
     C                   IF        FMDEFI = '1'                                 B01
     C                   MOVE      '0'           FMDEFI
      *
      *          ------------------------------------------
      *          Save as the default values.
      *          ------------------------------------------
     C                   EVAL      K#RTV# = *ZEROS
     C     $RTVK2        CHAIN     OMHTVL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVE      P#HEDC        TVHEDC
     C                   EVAL      TVUSID = D#USID
     C                   EVAL      TVFMTC = K#FMTC
     C                   EVAL      TVRTV# = *ZEROS
     C                   EVAL      TVSDTA = S#SDTA
     C                   WRITE     OMHTVR
     C                   ELSE                                                   X02
     C                   EVAL      TVSDTA = S#SDTA
     C                   UPDATE    OMHTVR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Save selections
      *          ------------------------------------------
     C                   EXSR      SRRTV
      *
      *          ------------------------------------------
      *          Set key field for HelpDesk
      *          ------------------------------------------
     C                   EVAL      K#HEDC = FFHEDC
      *
      *          ------------------------------------------
      *          Set key field for incident
      *          ------------------------------------------
     C                   IF        FFINC# = *BLANKS                             B01
     C                             AND FTINC# = *ALL'9'
     C                   MOVE      *HIVAL        K#INC#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Set key field for priority
      *          ------------------------------------------
     C                   IF        FFPRI# = *ZEROS                              B01
     C                             AND FTPRI# = *ALL'9'
     C                   MOVE      *HIVAL        K#PRI#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Move view in S#VIEI (in SRRF01, this field
      *          will be moved to S#VIEI)
      *          ------------------------------------------
     C                   EVAL      S#VIEI = FFVIEI                              B01
      *
      *          ------------------------------------------
      *          Set *IN40 equal to value of FFVIEI
      *          ------------------------------------------
     C                   EVAL      *IN40 = FFVIEI
      *
      *          ------------------------------------------
      *          Get allowed status routes
      *          ------------------------------------------
     C                   EXSR      SRSTR
      *
      *          ------------------------------------------
      *          Execute refresh routine for screen 1
      *          ------------------------------------------
     C                   EXSR      SRRF01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRINAD - Add incident                                        *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRINAD        BEGSR
      *
      *          ------------------------------------------
      *          Set action to Ask questions
      *          Set question list to zeros
      *          Set moment to 0 (incident creation)
      *          ------------------------------------------
     C                   EVAL      P#ACTN = C#QUES
     C                   EVAL      P#QLS# = *ZEROS
     C                   EVAL      QLS = *ZEROS
     C                   EVAL      ANS = *ZEROS
     C                   EVAL      QSQ = *ZEROS
     C                   EVAL      QUE = *ZEROS
      *
      *          ------------------------------------------
      *          Call program to ask questions
      *          ------------------------------------------
     C                   CALL      'OMH0071'     $H0071                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#0071 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#0071 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If return status is *EXIT, then exit pgm
      *          ------------------------------------------
     C                   IF        P#STAT = C#EXIT                              B01
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          If return status is *NORM
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B02
      *
      *          ------------------------------------------
      *          Start typing text while the incident is
      *          beeing created.
      *          ------------------------------------------
     C                   CALL      'OMH023'      $MH023                   99
      *
      *          ------------------------------------------
      *          Call program to get question values
      *          ------------------------------------------
     C                   EVAL      P#INC# = *BLANKS
     C                   CALL      'OMH0072'     $H0072                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#0072 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#0072 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Fill all fields in the buffer
      *          ------------------------------------------
     C                   MOVE      P#HEDC        I4HEDC
     C                   MOVEL(P)  C#GEN         I4INC#
     C                   MOVE      P#CTN#        I4CTN#
     C                   MOVE      *BLANKS       I4AIN#
     C                   MOVE      *ZEROS        I4INDT
     C                   MOVE      *ZEROS        I4INTM
     C                   MOVE      *ZEROS        I4RCDT
     C                   MOVE      *ZEROS        I4RCTM
     C                   MOVE      P#STRC        I4STRC
     C                   MOVE      P#RES#        I4RES#
     C                   MOVE      '0'           I4SUL#
     C                   MOVE      *BLANKS       I4STAT
     C                   MOVE      *ZEROS        I4SRDT
     C                   MOVE      *ZEROS        I4SRTM
     C                   MOVE      P#SEV1        I4SEV1
     C                   MOVE      P#SEV2        I4SEV2
     C                   MOVE      P#SEV3        I4SEV3
     C                   MOVE      P#PRI#        I4PRI#
     C                   MOVE      *ZEROS        I4ESC#
     C                   MOVE      *ZEROS        I4IEDT
     C                   MOVE      *ZEROS        I4IETM
     C                   MOVE      *BLANKS       I4RUID
     C                   MOVE      *BLANKS       I4AUID
     C                   MOVE      P#ITPC        I4ITPC
     C                   MOVE      *ZEROS        I4CFL#
     C                   MOVE      *BLANKS       I4CFIC
     C                   MOVE      P#USF1        I4USF1
     C                   MOVE      P#USF2        I4USF2
     C                   MOVE      P#USF3        I4USF3
     C                   MOVEL(P)  C#NONE        I4SHID
      *
      *          ------------------------------------------
      *          Fill action code
      *          ------------------------------------------
     C                   MOVE      '60'          I4ACTC
      *
      *          ------------------------------------------
      *          Fill the processing options in I4PRC
      *          ------------------------------------------
     C                   MOVE      *OFF          I4EOP
     C                   MOVE      *ON           I4UPD
     C                   MOVE      *ON           I4UPB
     C                   MOVE      *ON           I4PDI
      *
      *          ------------------------------------------
      *          Instruct API to send messages
      *          ------------------------------------------
     C                   MOVE      PGMQ          I4MSQ
      *
      *          ------------------------------------------
      *          Call OMQCHGIN
      *          ------------------------------------------
     C                   CALL      'OMQCHGIN'    $CHGIN                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#CHIN = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#CHIN = *ON
     C                   ENDIF
      *
     C                   MOVE      *OFF          I4PDI
      *
      *          ------------------------------------------
      *          If OMQCHGIN ended without an error
      *          ------------------------------------------
     C                   IF        I4MSTP = *BLANKS                             B03
      *
      *          ------------------------------------------
      *          Write the history.
      *          ------------------------------------------
     C                   MOVE      I4INC#        P#INC#
     C                   CALL      'OMH0072'     $H0072                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#0072 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#0072 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Fill fields for OMQUSIIN  (I4INC# is the
      *          incident number generated by OMQCHGIN)
      *          ------------------------------------------
     C                   MOVEL     P#HEDC        FMHEDC
     C                   MOVEL     I4INC#        FMINC#
      *
      *          ------------------------------------------
      *          Call OMQUSIIN in '*CHANGE after add' mode
      *          Subroutine SRINCH is used for this purpose
      *          ------------------------------------------
     C                   EVAL      W#MODE = C#CHGA
     C                   EXSR      SRINCH
      *
      *          ------------------------------------------
      *          If indicator 'Incidents added during this
      *          loop' was not yet on, then turn it on now
      *          ------------------------------------------
     C                   IF        W#INCA <> *ON                                B04
     C                   EVAL      W#INCA = *ON
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If OMQCHGIN ended with an error
      *          ------------------------------------------
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Fill fields for OMQUSIIN. Instruct
      *          OMQUSIIN to generate incident number.
      *          ------------------------------------------
     C                   MOVEL     P#HEDC        FMHEDC
     C                   MOVEL(P)  C#GEN         FMINC#
      *
      *          ------------------------------------------
      *          Fill all fields in the buffer
      *          ------------------------------------------
     C                   MOVE      P#HEDC        I2HEDC
     C                   MOVEL(P)  C#GEN         I2INC#
     C                   MOVE      P#CTN#        I2CTN#
     C                   MOVE      *BLANKS       I2AIN#
     C                   MOVE      *ZEROS        I2INDT
     C                   MOVE      *ZEROS        I2INTM
     C                   MOVE      *ZEROS        I2RCDT
     C                   MOVE      *ZEROS        I2RCTM
     C                   MOVE      P#STRC        I2STRC
     C                   MOVE      P#RES#        I2RES#
     C                   MOVE      '0'           I2SUL#
     C                   MOVE      *BLANKS       I2STAT
     C                   MOVE      *ZEROS        I2SRDT
     C                   MOVE      *ZEROS        I2SRTM
     C                   MOVE      P#SEV1        I2SEV1
     C                   MOVE      P#SEV2        I2SEV2
     C                   MOVE      P#SEV3        I2SEV3
     C                   MOVE      P#PRI#        I2PRI#
     C                   MOVE      *ZEROS        I2ESC#
     C                   MOVE      *ZEROS        I2IEDT
     C                   MOVE      *ZEROS        I2IETM
     C                   MOVE      *BLANKS       I2RUID
     C                   MOVE      *BLANKS       I2AUID
     C                   MOVE      P#ITPC        I2ITPC
     C                   MOVE      *ZEROS        I2CFL#
     C                   MOVE      *BLANKS       I2CFIC
     C                   MOVE      P#USF1        I2USF1
     C                   MOVE      P#USF2        I2USF2
     C                   MOVE      P#USF3        I2USF3
     C                   MOVEL(P)  C#NONE        I2SHID
      *
      *          ------------------------------------------
      *          Call OMQUSIIN in *ADD mode
      *          Subroutine SRINCH is used for this purpose
      *          ------------------------------------------
     C                   EVAL      W#MODE = C#ADD
     C                   EXSR      SRINCH
      *
      *          ------------------------------------------
      *          If OMQUSIIN ended without an error
      *          ------------------------------------------
     C                   IF        I2MSTP = *BLANKS                             B04
      *
      *          ------------------------------------------
      *          Set action to 'Write to history' and
      *          supply the generated incident number
      *          Also fill the screen field FMINC#
      *          ------------------------------------------
     C                   MOVEL     I2INC#        P#INC#
     C                   MOVEL     I2INC#        FMINC#
      *
      *          ------------------------------------------
      *          Call program to write records to history
      *          ------------------------------------------
     C                   CALL      'OMH0072'     $H0072                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#0072 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#0072 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If indicator 'Incidents added during this
      *          loop' was not yet on, then turn it on now
      *          ------------------------------------------
     C                   IF        W#INCA <> *ON                                B05
     C                   EVAL      W#INCA = *ON
     C                   ENDIF                                                  E05
      *
     C                   ENDIF                                                  E04
      *
     C                   ENDIF                                                  E03
      *
     C                   ENDIF                                                  E02
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRCAAD - Add call                                            *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRCAAD        BEGSR
      *
      *          ------------------------------------------
      *          Add a call
      *          ------------------------------------------
     C                   EVAL      P#ACTC = '60'
     C                   MOVE      FMHEDC        P#HEDC
     C                   EVAL      P#CTN# = P#CTN#
     C                   EVAL      P#CUSC = FFCUSC
     C                   EVAL      P#CCTC = FFCCTC
     C                   EVAL      P#STAT = *BLANKS
      *
      *          ------------------------------------------
      *          Call program register a call.
      *          ------------------------------------------
     C                   CALL      'OMH015'      $MH015                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#H015 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#H015 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If return status is *EXIT, then exit pgm
      *          ------------------------------------------
     C                   IF        P#STAT = C#EXIT                              B01
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          If return status is *NORM
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B02
      *
      *          ------------------------------------------
      *          Add an incident
      *          ------------------------------------------
     C                   EXSR      SRINAD
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
      * SRINST - Set incident status                                 *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRINST        BEGSR
      *
      *          ------------------------------------------
      *          Get record from OMINCJ1 with latest data
      *          ------------------------------------------
     C     $NC1K3        CHAIN     OMINCJ1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Call program to display window 'Select
      *          next status'
      *          ------------------------------------------
     C                   EVAL      P#HEDC = FMHEDC
     C                   EVAL      P1STRC = NCSTRC
     C                   EVAL      P1RES# = NCRES#
     C                   EVAL      P1SUL# = NCSUL#
     C                   EVAL      P1STAT = NCSTAT
     C                   EVAL      P2STRC = *BLANKS
     C                   EVAL      P2RES# = *ZEROS
     C                   EVAL      P2SUL# = '0'
     C                   EVAL      P2STAT = *BLANKS
     C                   CALLP     OMH022 ( P#HEDC : P1STRC : P1RES# :
     C                             P1SUL# : P1STAT : P2STRC : P2RES# :
     C                             P2SUL# : P2STAT )
      *
      *          ------------------------------------------
      *          If a selection was made (output parameter
      *          P2STRC is used to test this)
      *          ------------------------------------------
     C                   IF        P2STRC <> *BLANKS                            B01
      *
      *          ------------------------------------------
      *          Fill required fields in I4BUF
      *          ------------------------------------------
     C                   CLEAR                   I4BUF
     C                   MOVEL     FMHEDC        I4HEDC
     C                   MOVEL     FMINC#        I4INC#
     C                   MOVEL     P2STRC        I4STRC
     C                   MOVEL     P2RES#        I4RES#
     C                   MOVEL     P2SUL#        I4SUL#
     C                   MOVEL     P2STAT        I4STAT
      *
      *          ------------------------------------------
      *          Build Status date
      *          ------------------------------------------
     C                   EVAL      D#YY = UYEAR
     C                   EVAL      D#MM = UMONTH
     C                   EVAL      D#DD = UDAY
      *
     C                   EVAL      *IN99 = (D#YY <= 40)
     C                   MOVE      *IN99         D#CC
      *
     C                   Z-ADD     D#DATE        I4SRDT
      *
      *          ------------------------------------------
      *          Fill Status time
      *          ------------------------------------------
     C                   TIME                    I4SRTM
      *
      *          ------------------------------------------
      *          Fill action code for action *STATUS
      *          ------------------------------------------
     C                   MOVE      '29'          I4ACTC
      *
      *          ------------------------------------------
      *          Fill the processing options in I4PRC
      *          ------------------------------------------
     C                   MOVE      *OFF          I4EOP
     C                   MOVE      *ON           I4UPB
      *
      *          ------------------------------------------
      *          Instruct API to send messages
      *          ------------------------------------------
     C                   MOVE      PGMQ          I4MSQ
      *
      *          ------------------------------------------
      *          Call OMQCHGIN
      *          ------------------------------------------
     C                   CALL      'OMQCHGIN'    $CHGIN                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#CHIN = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#CHIN = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If no errors in API
      *          ------------------------------------------
     C                   IF        I4MSTP = *BLANKS                             B02
      *
      *          ------------------------------------------
      *          Update subfile record
      *          ------------------------------------------
     C     RRN01         CHAIN     OMH01801
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL     P2STRC        FMSTRC
     C                   MOVEL     P2SUL#        FMSUL#
     C                   MOVEL     P2STAT        FMSTAT
     C                   UPDATE    OMH01801
     C                   EVAL      RRN01 = S#RR01
      *
      *          ------------------------------------------
      *          Else - Error in API
      *          ------------------------------------------
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Force end of processing loop
      *          ------------------------------------------
     C                   EVAL      *INKL = *ON
      *
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Else - No status was selected
      *          ------------------------------------------
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Force end of processing loop
      *          ------------------------------------------
     C                   EVAL      *INKL = *ON
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRINCH - Change incident                                     *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRINCH        BEGSR
      *
      *          ------------------------------------------
      *          Fill key fields in I2KEY
      *          ------------------------------------------
     C                   MOVEL     FMHEDC        I2KHED
     C                   MOVEL     FMINC#        I2KINC
      *
      *          ------------------------------------------
      *          Fill function keys
      *          ------------------------------------------
     C                   EVAL      I2FUN# = 0
      *
      *          ------------------------------------------
      *          Fill action code depending on mode
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      W#MODE = C#CHG                               X01
     C                             OR W#MODE = C#CHGA                           X01
     C                   MOVE      '20'          I2ACTC
     C                   WHEN      W#MODE = C#ADD                               X01
     C                   MOVE      '60'          I2ACTC
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          Fill the processing options in I2PRCA
      *          ------------------------------------------
     C                   MOVE      *ON           I2EOP
      *
     C                   MOVE      *OFF          I2RDSP
      *
     C                   SELECT                                                 B01
     C                   WHEN      W#MODE = C#CHG                               X01
     C                             OR W#MODE = C#CHGA                           X01
     C                   MOVE      *OFF          I2FILL
     C                   WHEN      W#MODE = C#ADD                               X01
     C                   MOVE      *ON           I2FILL
     C                   ENDSL                                                  E01
      *
     C                   MOVE      *ON           I2PHPD
      *
     C                   SELECT                                                 B01
     C                   WHEN      W#MODE = C#CHG                               X01
     C                   MOVE      *OFF          I2ATXT
     C                   WHEN      W#MODE = C#ADD                               X01
     C                             OR W#MODE = C#CHGA                           X01
     C                   MOVE      *ON           I2ATXT
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          Clear the buffer
      *          ------------------------------------------
     C                   IF        I2FILL <> *ON                                B01
     C                   MOVE      *BLANKS       I2INC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Call OMQUSIIN
      *          ------------------------------------------
     C                   CALL      'OMQUSIIN'    $USIIN                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#USII = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#USII = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          F3=Exit
      *          ------------------------------------------
     C                   IF        I2FKU# = 3                                   B01
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          F12=Cancel
      *          ------------------------------------------
     C                   IF        I2FKU# = 12                                  B02
     C                   EVAL      *INKL = *ON
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Send the API message if needed
      *          ------------------------------------------
     C                   IF        I2MSTP <> *BLANK                             B03
     C                   EXSR      SNDAPI
     C                   EVAL      *INKL = *ON
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRINDS - Display incident                                    *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRINDS        BEGSR
      *
      *          ------------------------------------------
      *          Fill key fields in I2KEY
      *          ------------------------------------------
     C                   MOVEL     FMHEDC        I2KHED
     C                   MOVEL     FMINC#        I2KINC
      *
      *          ------------------------------------------
      *          Fill function keys
      *          ------------------------------------------
     C                   EVAL      I2FUN# = 0
      *
      *          ------------------------------------------
      *          Fill action code for *DISPLAY
      *          ------------------------------------------
     C                   MOVE      '50'          I2ACTC
      *
      *          ------------------------------------------
      *          Fill the processing options in I2PRCA
      *          ------------------------------------------
     C                   MOVE      *ON           I2EOP
     C                   MOVE      *OFF          I2RDSP
     C                   MOVE      *OFF          I2FILL
     C                   MOVE      *ON           I2PHPD
     C                   MOVE      *OFF          I2ATXT
      *
      *          ------------------------------------------
      *          Clear the buffer
      *          ------------------------------------------
     C                   IF        I2FILL <> *ON                                B01
     C                   MOVE      *BLANKS       I2INC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Call OMQUSIIN
      *          ------------------------------------------
     C                   CALL      'OMQUSIIN'    $USIIN                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#USII = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#USII = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          F3=Exit
      *          ------------------------------------------
     C                   IF        I2FKU# = 3                                   B01
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          F12=Cancel
      *          ------------------------------------------
     C                   IF        I2FKU# = 12                                  B02
     C                   EVAL      *INKL = *ON
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Send the API message if needed.
      *          ------------------------------------------
     C                   IF        I2MSTP <> *BLANK                             B03
     C                   EXSR      SNDAPI
     C                   EVAL      *INKL = *ON
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRINPR - Print incident                                      *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRINPR        BEGSR
      *
      *          ------------------------------------------
      *          Fill action code
      *          ------------------------------------------
     C                   MOVE      '00'          I3ACTC
      *
      *          ------------------------------------------
      *          Fill fields in the processing array
      *          ------------------------------------------
     C                   IF        W#OMS = *BLANKS                              B01
     C                   EXSR      SROMS
     C                   ENDIF                                                  E01
     C                   IF        W#OMS = *ON                                  B01
     C                   MOVE      *ON           I3PRR
     C                   MOVE      *ON           I3PFI
     C                   MOVE      *ON           I3PSI
     C                   ELSE                                                   X01
     C                   IF        W#OMS = *OFF                                 B02
     C                   MOVE      *OFF          I3PRR
     C                   MOVE      *OFF          I3PFI
     C                   MOVE      *OFF          I3PSI
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
     C                   MOVE      *OFF          I3EOP
     C                   MOVE      *ON           I3PIR
     C                   MOVE      *ON           I3LOD
     C                   MOVE      *ON           I3PIP
     C                   MOVE      *OFF          I3PCP
     C                   MOVE      *OFF          I3PEP
     C                   MOVE      *ON           I3QAH
     C                   MOVE      '9'           I3PIT
     C                   MOVE      *ON           I3CLP
      *
      *          ------------------------------------------
      *          Instruct API to send messages
      *          ------------------------------------------
     C                   MOVEL(P)  PGMQ          I3MSQ
      *
      *          ------------------------------------------
      *          Build date of today
      *          ------------------------------------------
     C                   EVAL      D#YY = UYEAR
     C                   EVAL      D#MM = UMONTH
     C                   EVAL      D#DD = UDAY
      *
      *          ------------------------------------------
      *          Fill century digit
      *          ------------------------------------------
     C                   EVAL      *IN99 = (D#YY <= 40)
     C                   MOVE      *IN99         D#CC
      *
      *          ------------------------------------------
      *          Fill fields in the buffer
      *          ------------------------------------------
     C                   MOVEL(P)  FMHEDC        I3HEDC
     C                   MOVEL(P)  FMINC#        I3INC
     C                   MOVEL(P)  '*ALL'        I3CTN
     C                   EVAL      I3IDTF = *ZEROS
     C                   Z-ADD     D#DATE        I3IDTT
     C                   EVAL      I3ITMF = *ZEROS
     C                   Z-ADD     C#HGTM        I3ITMT
     C                   EVAL      I3CDTF = *ZEROS
     C                   Z-ADD     D#DATE        I3CDTT
     C                   MOVEL(P)  '*ALL'        I3STRC
     C                   MOVEL(P)  '*'           I3SULC
     C                   MOVEL(P)  '*ALL'        I3SEV1
     C                   MOVEL(P)  '*ALL'        I3SEV2
     C                   MOVEL(P)  '*ALL'        I3SEV3
     C                   EVAL      I3PRIF = *ZEROS
     C                   Z-ADD     *HIVAL        I3PRIT
     C                   MOVEL(P)  '*ALL'        I3RUID
     C                   MOVEL(P)  '*ALL'        I3AUID
     C                   MOVEL(P)  '*ALL'        I3ITPC
     C                   MOVEL(P)  '*ALL'        I3USF1
     C                   MOVEL(P)  '*ALL'        I3USF2
     C                   Z-ADD     -1            I3USF3
     C                   MOVEL(P)  '*GE'         I3ESCD
     C                   EVAL      I3ESC# = *ZEROS
     C                   EVAL      I3ESDT = *ZEROS
     C                   MOVEL(P)  C#NONE        I3ALSC
     C                   MOVEL(P)  C#ALL         I3STAT
     C                   MOVEL(P)  C#ALL         I3CUSC
     C                   MOVEL(P)  C#ALL         I3CCTC
      *
      *          ------------------------------------------
      *          Call the print API
      *          ------------------------------------------
     C                   EVAL      I3INTL = 'V3R0M0  '
     C                   CALLP(E)  OMQPRTIN ( I3INTL : I3ACTC : I3BUF :
     C                             I3PRC : I3ERR : I3FLD )
     C                   EVAL      *IN99 = %ERROR
     C                   IF        *IN99 = *ON
     C                   EVAL      I#PRTI = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#PRTI = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Send printed request if no error found
      *          If error found call OMQPRTIN again to
      *          close program and force end of processing
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF                                 B01
     C                             AND I3MSTP <> 'E'
     C                   MOVEA     FMINC#        MC(1)
     C                   MOVEA     FMHEDC        MC(11)
     C                   MOVEA     MC(1)         MSGDTA
     C                   EVAL      MSGID = 'PRT0011'
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X01
     C                   CALLP     OMQPRTIN_001()
     C                   EVAL      *INKL = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRINLG - Display incident log                                *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRINLG        BEGSR
      *
      *          ------------------------------------------
      *          Call program to display incident log
      *          ------------------------------------------
     C                   CALL      'OMH021'      $MH021                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#H021 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#H021 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Process return status
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      P#STAT = C#EXIT                              X01
     C                   EXSR      SRTTLR
     C                   WHEN      P#STAT = C#CNCL                              X01
     C                             OR P#STAT = C#TERM
     C                   EVAL      *INKL = *ON
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRINAL - Allocate incident                                   *
      *                                                              *
      * Option 9 works as a toggle. If the incident is not allocated *
      * then option 9 functions as an allocate request. If the inci- *
      * dent is allocated, then option 9 functions as a deallocate   *
      * request. Therefore, the value of the action code passed to   *
      * the API is dependent on the contents of field NCAUID.        *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRINAL        BEGSR
      *
      *          ------------------------------------------
      *          DO so we can LEAVE
      *          ------------------------------------------
     C                   FOR       DO_X = 1 TO 1                                B01
      *
      *          ------------------------------------------
      *          Get record from OMINCJ1 with latest data
      *          ------------------------------------------
     C     $NC1K3        CHAIN     OMINCJ1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill required fields in I4BUF
      *          ------------------------------------------
     C                   CLEAR                   I4BUF
     C                   MOVEL     FMHEDC        I4HEDC
     C                   MOVEL     FMINC#        I4INC#
     C                   MOVEL     NCAUID        I4AUID
      *
      *          ------------------------------------------
      *          Fill action code
      *          15 = Allocate  85 = Deallocate
      *          ------------------------------------------
     C                   IF        NCAUID = *BLANKS                             B02
     C                   MOVE      '15'          I4ACTC
     C                   ELSE                                                   X02
     C                   MOVE      '85'          I4ACTC
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if user is allowed to allocate incidents
      *          to other users.
      *          ------------------------------------------
     C     $HDEL1        CHAIN     OMHDEL1
     C                   EVAL      *IN99 = NOT %FOUND
      *          ------------------------------------------
      *          If allocate, then check if this person is
      *          allowed to allocate for others.
      *          ------------------------------------------
     C                   IF        I4UPD = *ON                                  B02
     C                   MOVE      *BLANKS       I4AUID
     C                   IF        I4ACTC = '15'                                B03
     C                   IF        P#SUL# <> '9'                                B04
     C                             AND DEALOC = '0'
     C                   MOVE      D#USID        I4AUID
     C                   ELSE                                                   X04
     C                   EVAL      P#USID = *BLANKS
     C                   EVAL      P#USDC = *BLANKS
     C                   CALLP     OMX1192 ( P#HEDC : P#USID : P#USDC )
     C                   IF        P#USID = *BLANKS                             B05
     C                   LEAVE
     C                   ELSE                                                   X05
     C                   MOVE      P#USID        I4AUID
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill the processing options in I4PRC
      *          ------------------------------------------
     C                   MOVE      *OFF          I4EOP
     C                   MOVE      *ON           I4UPB
      *
      *          ------------------------------------------
      *          Instruct API to send messages
      *          ------------------------------------------
     C                   MOVE      PGMQ          I4MSQ
      *          ------------------------------------------
      *          Call OMQCHGIN
      *          ------------------------------------------
     C                   CALL      'OMQCHGIN'    $CHGIN                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#CHIN = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#CHIN = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If subroutine was called in process mode
      *          then refresh subfile record
      *          ------------------------------------------
     C                   IF        I4UPD = *ON                                  B02
     C     RRN01         CHAIN     OMH01804
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      I4AUID        FMAUID
     C                   UPDATE    OMH01804
     C                   EVAL      RRN01 = S#RR01
     C                   ENDIF                                                  E02
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC03 - Display incident relations                          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSC03        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EXSR      SRIN03
      *
      *          ------------------------------------------
      *          Do while no end
      *          ------------------------------------------
     C                   DOU       LOOP03 = *OFF                                B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR03 = *OFF                                B02
      *
      *          ------------------------------------------
      *          If subfile is empty, then write record to
      *          clear some lines on the display
      *          ------------------------------------------
     C                   IF        RRN03 = *ZEROS                               B03
     C                   Z-ADD     1             LIN
     C                   Z-ADD     1             COL
     C                   WRITE     OMH018CL
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH01811
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if
      *          records found
      *          ------------------------------------------
     C                   IF        RRN03 = *ZEROS                               B03
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   EXFMT     OMH01810
      *
      *          ------------------------------------------
      *          CMD-3 is exit
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B03
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   EVAL      W#CRC3 = D#CRC#
      *
      *          ------------------------------------------
      *          Clear message queue
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Setof screen error indicators
      *          ------------------------------------------
     C                   EVAL      *IN70 = *OFF
     C                   EVAL      EROR03 = *OFF
     C                   EVAL      LOOP03 = *OFF
      *
      *          ------------------------------------------
      *          CMD-5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B03
     C                   EXSR      SRRF03
     C                   EVAL      EROR03 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F8=Connect request
      *          ------------------------------------------
     C                   IF        *INKH = *ON                                  B04
     C                   IF        W#OMS = *BLANKS                              B05
     C                   EXSR      SROMS
     C                   ENDIF                                                  E05
     C                   IF        W#OMS = *ON                                  B05
     C                   EXSR      SRRF03
     C                   EVAL      EROR03 = *ON
     C                   ELSE                                                   X05
     C                   EVAL      MSGID = 'OMS0006'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR03 = *ON
     C                   ENDIF                                                  E05
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Do edit routine if not CMD-12
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B05
     C                   EXSR      SRED03
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do processing
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B02
     C                   EXSR      SRPR03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End process loop
      *          ------------------------------------------
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Reset SFLEND indicator for last subfile
      *          ------------------------------------------
     C                   EVAL      *IN92 = S#END
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIN03 - Work with incident relations - initialization      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN03        BEGSR
      *
      *          ------------------------------------------
      *          Save SFLEND indicator for last subfile
      *          ------------------------------------------
     C                   EVAL      S#END = *IN92
      *
      *          ------------------------------------------
      *          Do refresh subroutine
      *          ------------------------------------------
     C                   EXSR      SRRF03
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF03 - Work with incident relations - refresh             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF03        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      RRN03 = *ZEROS
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH01810
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Check if this incident was accumulated to
      *          another incident
      *          ------------------------------------------
     C     $NC1K3        CHAIN     OMINCJ1
     C                   EVAL      *IN99 = NOT %FOUND
      *
     C                   IF        NCAIN# <> *BLANKS                            B01
      *
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      *BLANKS       FMRELC
     C                   MOVEL(P)  C#ACC         FMIRTP
     C                   MOVEL     FMHEDC        FMIRAH
     C                   MOVEL     NCAIN#        FMIRIT
      *
     C                   MOVEL     FMHEDC        K4HEDC
     C                   MOVEL     NCAIN#        K4INC#
      *
     C     $NC1K4        CHAIN     OMINCJ1
     C                   EVAL      *IN99 = NOT %FOUND
      *
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEL(P)  NCSHID        FMDESC
     C                   ELSE                                                   X02
     C                   MOVEL(P)  C#NONE        FMDESC
     C                   ENDIF                                                  E02
      *
     C                   ADD       1             RRN03
     C                   WRITE     OMH01809
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check if other incidents were accumulated
      *          to this incident
      *          ------------------------------------------
     C                   IF        I#INL4 <> *ON                                B01
     C                   OPEN      OMINCL4
     C                   EVAL      I#INL4 = *ON
     C                   ENDIF                                                  E01
      *
     C     $NC4L1        SETLL     OMINCL4
     C     $NC4L1        READE     OMINCL4
     C                   EVAL      *IN99 = %EOF
      *
     C                   DOW       *IN99 = *OFF                                 B01
      *
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      '<'           FMRELC
     C                   MOVEL(P)  C#ACC         FMIRTP
     C                   MOVEL     NCHEDC        FMIRAH
     C                   MOVEL     NCINC#        FMIRIT
      *
     C                   EVAL      K4HEDC = NCHEDC
     C                   MOVEL     NCINC#        K4INC#
      *
     C     $NC1K4        CHAIN     OMINCJ1
     C                   EVAL      *IN99 = NOT %FOUND
      *
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEL     NCSHID        FMDESC
     C                   ELSE                                                   X02
     C                   MOVEL(P)  C#NONE        FMDESC
     C                   ENDIF                                                  E02
      *
     C                   ADD       1             RRN03
     C                   WRITE     OMH01809
      *
     C     $NC4L1        READE     OMINCL4
     C                   EVAL      *IN99 = %EOF
      *
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Read first record from OMRIR
      *          ------------------------------------------
     C                   MOVEL     FMHEDC        K#IRAH
     C                   MOVEL     FMINC#        K#IRIT
      *
     C     $IR1K1        SETLL     OMRIRL1
     C     $IR2K1        SETLL     OMRIRL2
      *
     C     $IR1K1        READE     OMRIRL1
     C                   EVAL      *IN99 = %EOF
      *
     C                   IF        *IN99 = *OFF                                 B01
     C                   MOVE      *BLANKS       FMRELC
     C                   ELSE                                                   X01
     C                   MOVE      '<'           FMRELC
     C     $IR2K1        READE     OMRIRL2
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPTI
     C                   IF        FMRELC <> '<'                                B02
     C                   MOVEL     IRIRAH        FMIRAH
     C                   MOVEL     IRIRIT        FMIRIT
     C                   ELSE                                                   X02
     C                   MOVEL     IRHEDC        FMIRAH
     C                   MOVEL     IRINC#        FMIRIT
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Some actions depend on the relation type
      *          ------------------------------------------
     C                   SELECT                                                 B02
      *
      *          ------------------------------------------
      *          Relation type Request
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Relation type Incident
      *          ------------------------------------------
     C                   WHEN      IRIRTP = 'I'                                 X02
     C                   MOVEL(P)  C#INC         FMIRTP
     C                   IF        FMRELC <> '<'                                B03
     C                   MOVEL     IRIRAH        K4HEDC
     C                   MOVEL     IRIRIT        K4INC#
     C                   ELSE                                                   X03
     C                   MOVEL     IRHEDC        K4HEDC
     C                   MOVEL     IRINC#        K4INC#
     C                   ENDIF                                                  E03
     C     $NC1K4        CHAIN     OMINCJ1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B03
     C                   MOVEL     NCSHID        FMDESC
     C                   ELSE                                                   X03
     C                   MOVEL(P)  C#NONE        FMDESC
     C                   ENDIF                                                  E03
      *
     C                   ENDSL                                                  E02
      *
      *          ------------------------------------------
      *          Increase record number and write record
      *          ------------------------------------------
     C                   ADD       1             RRN03
     C                   WRITE     OMH01809
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C                   IF        FMRELC <> '<'                                B02
     C     $IR1K1        READE     OMRIRL1
     C                   EVAL      *IN99 = %EOF
     C                   IF        *IN99 = *ON                                  B03
     C                   MOVE      '<'           FMRELC
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
     C                   IF        FMRELC = '<'                                 B02
     C     $IR2K1        READE     OMRIRL2
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF                                                  E02
      *
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Fill current subfile relative record number
      *          to display first subfile page
      *          ------------------------------------------
     C                   Z-ADD     1             CRRN03
      *
      *          ------------------------------------------
      *          Set SFLEND indicator off
      *          ------------------------------------------
     C                   EVAL      *IN92 = *OFF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED03 - Work with incident relations - edit                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED03        BEGSR
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
     C                   EVAL      CRRN03 = *ZEROS
     C                   EVAL      W#VALC = *ZEROS
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   IF        RRN03 > *ZERO                                B01
     C                   READC     OMH01809
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Do if option is not blank
      *          ------------------------------------------
     C                   IF        FMOPTI <> *BLANKS                            B03
      *
      *          ------------------------------------------
      *          Format
      *          ------------------------------------------
     C                   MOVE      FMOPTI        W#WRKI
     C                   IF        W#WRKI = *BLANKS                             B04
     C                   MOVEL     FMOPTI        W#WRKI
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      W#WRKI        FMOPTI
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If option is valid, then check the number of valid
      *          codes. *IN93 is the subfile next change indicator
      *          ------------------------------------------
     C                   IF        FMOPTI = ' 5'                                B04
     C                             OR FMOPTI = ' 4'                             B04
     C                             OR FMOPTI = '12'                             B04
      *
     C                   EVAL      W#ERR = *OFF
      *
     C                   IF        FMOPTI = ' 4'                                B05
     C                             AND FMIRTP = C#REQ
     C                             OR FMOPTI = ' 5'                             B05
     C                             AND FMIRTP = C#REQ
      *
     C                   IF        W#OMS = *BLANKS                              B06
     C                   EXSR      SROMS
     C                   ENDIF                                                  E06
     C                   IF        W#OMS = *OFF                                 B06
     C                   EVAL      MSGID = 'OMS0005'
     C                   EXSR      SNDMSG
     C                   EVAL      W#ERR = *ON
     C                   ENDIF                                                  E06
      *
     C                   ENDIF                                                  E05
      *
     C                   IF        W#ERR = *ON                                  B05
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR03 = *ON
     C                   ELSE                                                   X05
     C                   ADD       1             W#VALC
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          Option not valid
      *          ------------------------------------------
     C                   ELSE                                                   X04
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR03 = *ON
     C                   MOVEL(P)  FMOPTI        MSGDTA
     C                   EVAL      MSGID = 'OPTE021'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If an error was found, then position on the page of
      *          the first error
      *          ------------------------------------------
     C                   IF        CRRN03 = *ZEROS                              B04
     C                             AND *IN70 = *ON
     C                   Z-ADD     RRN03         CRRN03
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Update subfile
      *          ------------------------------------------
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH01809
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   UPDATE    OMH01809
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH01809
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Position subfile
      *          ------------------------------------------
     C                   IF        CRRN03 = *ZEROS                              B01
     C                   Z-ADD     W#CRC3        CRRN03
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR03 - Work with incident relations - process             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR03        BEGSR
      *
      *          ------------------------------------------
      *          Read subfile next changed record if
      *          records in subfile and valid codes found
      *          ------------------------------------------
     C                   IF        RRN03 > *ZEROS                               B01
     C                             AND W#VALC > *ZEROS
     C                   READC     OMH01809
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Read subfile until valid option found
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Loop again for options were typed
      *          ------------------------------------------
     C                   EVAL      LOOP03 = *ON
     C                   EVAL      W#CRC3 = RRN03
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPTI        W#OPTI
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    OMH01809
      *
      *          ------------------------------------------
      *          Do action
      *          ------------------------------------------
     C                   IF        W#OPTI = '12'                                B03
      *
      *          ------------------------------------------
      *          Different actions for different relations
      *          ------------------------------------------
     C                   SELECT                                                 B04
      *
      *          ------------------------------------------
      *          Relation type Request
      *          ------------------------------------------
     C                   WHEN      FMIRTP = C#REQ                               X04
     C                   ENDSL                                                  E04
      *
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Do action
      *          ------------------------------------------
     C                   IF        W#OPTI = ' 4'                                B03
      *
      *          ------------------------------------------
      *          Different actions for different relations
      *          ------------------------------------------
     C                   SELECT                                                 B04
      *
      *          ------------------------------------------
      *          Relation type Request
      *          ------------------------------------------
     C                   WHEN      FMIRTP = C#REQ                               X04
     C                   EXSR      SRDREQ
      *
      *          ------------------------------------------
      *          Relation type Incident
      *          ------------------------------------------
     C                   WHEN      FMIRTP = C#INC                               X04
     C                   EXSR      SRDINC
      *
      *          ------------------------------------------
      *          Relation type Accumulated incident
      *          ------------------------------------------
     C                   WHEN      FMIRTP = C#ACC                               X04
     C                   EXSR      SRDACC
     C                   ENDSL                                                  E04
      *
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Do action
      *          ------------------------------------------
     C                   IF        W#OPTI = ' 5'                                B03
      *
      *          ------------------------------------------
      *          Different actions for different relations
      *          ------------------------------------------
     C                   SELECT                                                 B04
      *
      *          ------------------------------------------
      *          Relation type Request
      *          ------------------------------------------
     C                   WHEN      FMIRTP = C#REQ                               X04
     C                   EXSR      SRREDS
      *
      *          ------------------------------------------
      *          Relation type Incident - use subroutine
      *          SRINDS to perform the required action
      *          Save original incident number and restore
      *          it after displaying the related incident
      *          ------------------------------------------
     C                   WHEN      FMIRTP = C#INC                               X04
     C                             OR FMIRTP = C#ACC                            X04
     C                   MOVEL     FMINC#        S1INC#
     C                   MOVEL     FMIRAH        FMHEDC
     C                   MOVEL     FMIRIT        FMINC#
     C                   EXSR      SRINDS
     C                   MOVEL     S1INC#        FMINC#
     C                   IF        I2MSTP = 'E'                                 B05
     C                   LEAVE
     C                   ENDIF                                                  E05
      *
     C                   ENDSL                                                  E04
      *
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next changed if no end requested.
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                   LEAVE
     C                   ELSE                                                   X03
     C                   READC     OMH01809
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If end of file found, then end loop
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Reset subfile relative record number
      *          ------------------------------------------
     C                   Z-ADD     W#CRC3        CRRN03
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC04 - Display Q & A history                               *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSC04        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EXSR      SRIN04
      *
      *          ------------------------------------------
      *          Do while no end
      *          ------------------------------------------
     C                   DOU       LOOP04 = *OFF                                B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR04 = *OFF                                B02
      *
      *          ------------------------------------------
      *          If subfile is empty, then write record to
      *          clear some lines on the display
      *          ------------------------------------------
     C                   IF        RRN04 = *ZEROS                               B03
     C                   Z-ADD     1             LIN
     C                   Z-ADD     1             COL
     C                   WRITE     OMH018CL
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH01814
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if
      *          records found
      *          ------------------------------------------
     C                   IF        RRN04 = *ZEROS                               B03
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   EXFMT     OMH01813
      *
      *          ------------------------------------------
      *          CMD-3 is exit
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B03
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   EVAL      W#CRC4 = D#CRC#
      *
      *          ------------------------------------------
      *          Clear message queue
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Setof screen error indicators
      *          ------------------------------------------
     C                   EVAL      *IN70 = *OFF
     C                   EVAL      EROR04 = *OFF
     C                   EVAL      LOOP04 = *OFF
      *
      *          ------------------------------------------
      *          CMD-5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B03
     C                   EXSR      SRRF04
     C                   EVAL      EROR04 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Do edit routine if not CMD-12
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B04
     C                   EXSR      SRED04
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do processing
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B02
     C                   EXSR      SRPR04
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End process loop
      *          ------------------------------------------
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Reset SFLEND indicator for last subfile
      *          ------------------------------------------
     C                   EVAL      *IN92 = S#END
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIN04 - Work with Q & A history - initialization           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN04        BEGSR
      *
      *          ------------------------------------------
      *          Open OMQAHL1, QMHQUL1 and OMANSL1 if not
      *          already open
      *          ------------------------------------------
     C                   IF        I#QAH1 <> *ON                                B01
     C                   OPEN      OMQAHL1
     C                   EVAL      I#QAH1 = *ON
     C                   ENDIF                                                  E01
     C                   IF        I#HQU1 <> *ON                                B01
     C                   OPEN      OMHQUL1
     C                   EVAL      I#HQU1 = *ON
     C                   ENDIF                                                  E01
     C                   IF        I#ANS1 <> *ON                                B01
     C                   OPEN      OMANSL1
     C                   EVAL      I#ANS1 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Save SFLEND indicator for last subfile
      *          ------------------------------------------
     C                   EVAL      S#END = *IN92
      *
      *          ------------------------------------------
      *          Do refresh subroutine
      *          ------------------------------------------
     C                   EXSR      SRRF04
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF04 - Work with Q & A history - refresh                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF04        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      RRN04 = *ZEROS
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH01813
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Read first record
      *          ------------------------------------------
     C     $AH1K1        SETLL     OMQAHL1
     C     $AH1K1        READE     OMQAHL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Fill option field
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPTS
      *
      *          ------------------------------------------
      *          Get question
      *          ------------------------------------------
     C     $QU1K1        CHAIN     OMHQUL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEL     QUQUES        FMQUE1
     C                   MOVEL     QUQUES        FMQUES
     C                   ELSE                                                   X02
     C                   MOVEL(P)  C#NOTF        FMQUE1
     C                   MOVEL(P)  C#NOTF        FMQUES
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Get answer
      *          ------------------------------------------
     C     $NS1K1        CHAIN     OMANSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEL     NSANSC        FMANS1
     C                   MOVEL     NSANSC        FMANSC
     C                   ELSE                                                   X02
     C                   MOVEL(P)  C#NOTF        FMANS1
     C                   MOVEL(P)  C#NOTF        FMANSC
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Increase record number and write record
      *          ------------------------------------------
     C                   ADD       1             RRN04
     C                   WRITE     OMH01812
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     $AH1K1        READE     OMQAHL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Fill current subfile relative record number
      *          to display first subfile page
      *          ------------------------------------------
     C                   Z-ADD     1             CRRN04
      *
      *          ------------------------------------------
      *          Set SFLEND indicator off
      *          ------------------------------------------
     C                   EVAL      *IN92 = *OFF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED04 - Work with Q & A history - edit                     *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED04        BEGSR
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
     C                   EVAL      CRRN04 = *ZEROS
     C                   EVAL      W#VALC = *ZEROS
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   IF        RRN04 > *ZERO                                B01
     C                   READC     OMH01812
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Do if option is not blank
      *          ------------------------------------------
     C                   IF        FMOPTS <> *BLANKS                            B03
      *
      *          ------------------------------------------
      *          If option is valid, then check the number of valid
      *          codes. *IN93 is the subfile next change indicator
      *          ------------------------------------------
     C                   IF        FMOPTS = '5'                                 B04
     C                   ADD       1             W#VALC
      *
      *          ------------------------------------------
      *          Option not valid
      *          ------------------------------------------
     C                   ELSE                                                   X04
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR04 = *ON
     C                   MOVEL(P)  FMOPTS        MSGDTA
     C                   EVAL      MSGID = 'OPTE021'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If an error was found, then position on the page of
      *          the first error
      *          ------------------------------------------
     C                   IF        CRRN04 = *ZEROS                              B04
     C                             AND *IN70 = *ON
     C                   Z-ADD     RRN04         CRRN04
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Update subfile
      *          ------------------------------------------
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH01812
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   UPDATE    OMH01812
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH01812
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Position subfile
      *          ------------------------------------------
     C                   IF        CRRN04 = *ZEROS                              B01
     C                   Z-ADD     W#CRC4        CRRN04
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR04 - Work with Q & A history - process                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR04        BEGSR
      *
      *          ------------------------------------------
      *          Read subfile next changed record if
      *          records in subfile and valid codes found
      *          ------------------------------------------
     C                   IF        RRN04 > *ZEROS                               B01
     C                             AND W#VALC > *ZEROS
     C                   READC     OMH01812
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Read subfile until valid option found
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Loop again for options were typed
      *          ------------------------------------------
     C                   EVAL      LOOP04 = *ON
     C                   EVAL      W#CRC4 = RRN04
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPTS        W#OPTS
     C                   MOVE      *BLANKS       FMOPTS
     C                   UPDATE    OMH01812
      *
      *          ------------------------------------------
      *          Do action
      *          ------------------------------------------
     C                   IF        W#OPTS = '5'                                 B03
     C                   EVAL      P#QOAI = '3'
     C                   EVAL      P#QUES = FMQUES
     C                   EVAL      P#ANSC = FMANSC
     C                   CALLP     OMX305 ( P#QOAI : P#QUES : P#ANSC :
     C                             P#STAT )
      *
      *          ------------------------------------------
      *          Check for return status *EXIT and *CNCL
      *          ------------------------------------------
     C                   IF        P#STAT = C#EXIT                              B04
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X04
     C                   IF        P#STAT = C#CNCL                              B05
     C                   LEAVE
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next changed
      *          ------------------------------------------
     C                   READC     OMH01812
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If end of file found, then end loop
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Reset subfile relative record number
      *          ------------------------------------------
     C                   Z-ADD     W#CRC4        CRRN04
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC05 - Select continuation option                          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSC05        BEGSR
      *
      *          ------------------------------------------
      *          Close call
      *          ------------------------------------------
     C     $CA1K1        CHAIN     OMCCAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B01
     C                   MOVEL     CAHEDC        C4HEDC
     C                   MOVEL     CACTN#        C4CTN#
     C                   MOVEL     CACUSC        C4CUSC
     C                   MOVEL     CACCTC        C4CCTC
     C                   Z-ADD     CAINDT        C4INDT
     C                   Z-ADD     CAINTM        C4INTM
     C                   TIME                    C4RCTM
     C                   MOVEL     CAUSID        C4USID
     C                   MOVEL     CACSRC        C4CSRC
     C                   MOVEL     CAUSF1        C4USF1
     C                   MOVEL     CAUSF2        C4USF2
     C                   Z-ADD     CAUSF3        C4USF3
     C                   MOVEL     C#INTL        C4INTL
     C                   MOVEL     '20'          C4ACTC
     C                   MOVEL     '111'         C4PRC
      *
      *          ------------------------------------------
      *          Call OMQCHGCC
      *          ------------------------------------------
     C                   CALL      'OMQCHGCC'    $CHGCC                   99
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Send information message
      *          ------------------------------------------
     C                   MOVEA     FMINC#        MC(1)
     C                   MOVEA     P#CTN#        MC(11)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'INF0027'
     C                   EXSR      SNDMSG
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC06 - Link incident - Enter incident number               *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSC06        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EXSR      SRIN06
      *
      *          ------------------------------------------
      *          Do while no end
      *          ------------------------------------------
     C                   DOU       LOOP06 = *OFF                                B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR06 = *OFF                                B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the screen
      *          ------------------------------------------
     C                   EXFMT     OMH01817
      *
      *          ------------------------------------------
      *          Clear message queue
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Setof screen error indicators
      *          ------------------------------------------
     C                   EVAL      *IN70 = *OFF
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      EROR06 = *OFF
     C                   EVAL      LOOP06 = *OFF
      *
      *          ------------------------------------------
      *          CMD-05=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B03
     C                   EXSR      SRIN06
     C                   EVAL      EROR06 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Do edit routine if not CMD-12 and some-
      *          thing was entered
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B04
     C                             AND FMLNIN <> *BLANKS
     C                   EXSR      SRED06
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors or CMD key
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do processing if not CMD-12 and something
      *          was entered
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B02
     C                             AND FMLNIN <> *BLANKS
     C                   EXSR      SRPR06
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End process loop
      *          ------------------------------------------
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRIN06 - Link incident - Initialize screen                   *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRIN06        BEGSR
      *
      *          ------------------------------------------
      *          Fill screen fields
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMLNIN
     C                   MOVE      *BLANKS       FMLNAC
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRED06 - Link incident - Edit screen                         *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRED06        BEGSR
      *
      *          ------------------------------------------
      *          Fill key fields
      *          ------------------------------------------
     C                   MOVEL     FMHEDC        K4HEDC
     C                   MOVEL     FMLNIN        K4INC#
      *
      *          ------------------------------------------
      *          Check if incident number exists
      *          ------------------------------------------
     C     $NC1K4        SETLL     OMINCJ1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B01
     C                   EVAL      EROR06 = *ON
     C                   EVAL      *IN70 = *ON
     C                   EVAL      MSGID = 'INC0006'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check value of field 'Link or allocate'
      *          ------------------------------------------
     C                   IF        FMLNAC <> '0'                                B01
     C                             AND FMLNAC <> '1'
     C                   EVAL      EROR06 = *ON
     C                   EVAL      *IN71 = *ON
     C                   MOVEA     VAR(4)        MC(1)
     C                   MOVEA     VAR(2)        MC(33)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'OPTE002'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPR06 - Link incident - Process screen                      *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPR06        BEGSR
      *
      *          ------------------------------------------
      *          If option is Link, then SRINLN
      *          If option is Accumulate, then SRINAC
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      FMLNAC = '0'                                 X01
     C                   EXSR      SRINLN
     C                   WHEN      FMLNAC = '1'                                 X01
     C                   EXSR      SRINAC
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRINLN - Link incident                                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRINLN        BEGSR
      *
      *          ------------------------------------------
      *          Fill all fields in the buffer
      *          ------------------------------------------
     C                   MOVE      FMHEDC        I4HEDC
     C                   MOVEL     FMINC#        I4INC#
     C                   MOVE      FMHEDC        I4THED
     C                   MOVE      FMLNIN        I4TINC
      *
      *          ------------------------------------------
      *          Fill action code
      *          ------------------------------------------
     C                   MOVE      '23'          I4ACTC
      *
      *          ------------------------------------------
      *          Fill the processing options in I4PRC
      *          ------------------------------------------
     C                   MOVE      *OFF          I4EOP
     C                   MOVE      *ON           I4UPD
     C                   MOVE      *OFF          I4UPB
     C                   MOVE      *BLANKS       I4PCM
     C                   MOVE      *BLANKS       I4CCM
     C                   MOVE      *BLANKS       I4RCM
      *
      *          ------------------------------------------
      *          Instruct API to send messages
      *          ------------------------------------------
     C                   MOVE      PGMQ          I4MSQ
      *
      *          ------------------------------------------
      *          Call OMQCHGIN
      *          ------------------------------------------
     C                   CALL      'OMQCHGIN'    $CHGIN                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#CHIN = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#CHIN = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If OMQCHGIN ended without an error
      *          ------------------------------------------
     C                   IF        I4MSTP = *BLANKS                             B01
      *
      *          ------------------------------------------
      *          Send message that incident was success-
      *          fully linked
      *          ------------------------------------------
     C                   MOVEA     FMINC#        MC(1)
     C                   MOVEA     FMLNIN        MC(11)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'LNK0016'
     C                   EXSR      SNDMSG
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRINAC - Accumulate incident                                 *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRINAC        BEGSR
      *
      *          ------------------------------------------
      *          Fill all fields in the buffer
      *          ------------------------------------------
     C                   MOVE      FMHEDC        I4HEDC
     C                   MOVEL     FMINC#        I4INC#
     C                   MOVE      FMLNIN        I4AIN#
      *
      *          ------------------------------------------
      *          Fill action code
      *          ------------------------------------------
     C                   MOVE      '25'          I4ACTC
      *
      *          ------------------------------------------
      *          Fill the processing options in I4PRC
      *          ------------------------------------------
     C                   MOVE      *OFF          I4EOP
     C                   MOVE      *ON           I4UPD
     C                   MOVE      *ON           I4UPB
     C                   MOVE      *BLANKS       I4PCM
     C                   MOVE      *BLANKS       I4CCM
     C                   MOVE      '0'           I4RCM
      *
      *          ------------------------------------------
      *          Instruct API to send messages
      *          ------------------------------------------
     C                   MOVE      PGMQ          I4MSQ
      *
      *          ------------------------------------------
      *          Call OMQCHGIN
      *          ------------------------------------------
     C                   CALL      'OMQCHGIN'    $CHGIN                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#CHIN = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#CHIN = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If OMQCHGIN ended without an error
      *          ------------------------------------------
     C                   IF        I4MSTP = *BLANKS                             B01
      *
      *          ------------------------------------------
      *          If actual accumulated incident number is
      *          equal to the selected incident number,
      *          then send 'normal' message
      *          ------------------------------------------
     C                   IF        I4AIN# = FMLNIN                              B02
     C                   MOVEA     FMINC#        MC(1)
     C                   MOVEA     FMLNIN        MC(11)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'ACC0002'
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          If actual accumulated incident number is
      *          not equal to the selected incident number,
      *          then send 'special' message
      *          ------------------------------------------
     C                   MOVEA     FMINC#        MC(1)
     C                   MOVEA     FMLNIN        MC(11)
     C                   MOVEA     I4AIN#        MC(21)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'ACC0003'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E02
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRDINC - Disconnect incident                                 *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRDINC        BEGSR
      *
      *          ------------------------------------------
      *          Fill all fields in the buffer
      *          ------------------------------------------
     C                   MOVE      FMHEDC        I4HEDC
     C                   MOVEL     FMINC#        I4INC#
     C                   MOVEL     FMIRAH        I4THED
     C                   MOVEL     FMIRIT        I4TINC
      *
      *          ------------------------------------------
      *          Fill action code
      *          ------------------------------------------
     C                   MOVE      '27'          I4ACTC
      *
      *          ------------------------------------------
      *          Fill the processing options in I4PRC
      *          ------------------------------------------
     C                   MOVE      *OFF          I4EOP
     C                   MOVE      *ON           I4UPD
     C                   MOVE      *OFF          I4UPB
     C                   MOVE      *BLANKS       I4PCM
     C                   MOVE      *BLANKS       I4CCM
     C                   MOVE      *BLANKS       I4RCM
      *
      *          ------------------------------------------
      *          Instruct API to send messages
      *          ------------------------------------------
     C                   MOVE      PGMQ          I4MSQ
      *
      *          ------------------------------------------
      *          Call OMQCHGIN
      *          ------------------------------------------
     C                   CALL      'OMQCHGIN'    $CHGIN                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#CHIN = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#CHIN = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If OMQCHGIN ended with an error
      *          ------------------------------------------
     C                   IF        I4MSTP <> *BLANKS                            B01
      *
      *          ------------------------------------------
      *          Send message and force end of loop
      *          ------------------------------------------
     C                   EXSR      SNDAPI
     C                   EVAL      *INKL = *ON
      *
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Send message to confirm that incident was
      *          disconnected from request
      *          ------------------------------------------
     C                   MOVEA     FMINC#        MC(1)
     C                   MOVEA     FMHEDC        MC(11)
     C                   MOVEA     I4TINC        MC(16)
     C                   MOVEA     I4THED        MC(26)
     C                   MOVEA     MC            I2MSGD
     C                   MOVE      'OMQA017'     I2MSID
     C                   EXSR      SNDAPI
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRDACC - Disconnect accumulated incident                     *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRDACC        BEGSR
      *
      *          ------------------------------------------
      *          Fill all fields in the buffer
      *          ------------------------------------------
     C                   MOVE      FMHEDC        I4HEDC
     C                   MOVEL     FMINC#        I4INC#
     C                   MOVEL     FMIRAH        I4THED
     C                   MOVEL     FMIRIT        I4TINC
      *
      *          ------------------------------------------
      *          Fill action code
      *          ------------------------------------------
     C                   MOVE      '25'          I4ACTC
      *
      *          ------------------------------------------
      *          Fill the processing options in I4PRC
      *          ------------------------------------------
     C                   MOVE      *OFF          I4EOP
     C                   MOVE      *ON           I4UPD
     C                   MOVE      *OFF          I4UPB
     C                   MOVE      *BLANKS       I4PCM
     C                   MOVE      *BLANKS       I4CCM
     C                   MOVE      *ON           I4RCM                          Disconnect
      *
      *          ------------------------------------------
      *          Instruct API to send messages
      *          ------------------------------------------
     C                   MOVE      PGMQ          I4MSQ
      *
      *          ------------------------------------------
      *          Call OMQCHGIN
      *          ------------------------------------------
     C                   CALL      'OMQCHGIN'    $CHGIN                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#CHIN = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#CHIN = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If OMQCHGIN ended with an error
      *          ------------------------------------------
     C                   IF        I4MSTP <> *BLANKS                            B01
      *
      *          ------------------------------------------
      *          Send message and force end of loop
      *          ------------------------------------------
     C                   EXSR      SNDAPI
     C                   EVAL      *INKL = *ON
      *
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Send message to confirm that incident was
      *          disconnected from incident
      *          ------------------------------------------
     C                   MOVEA     FMINC#        MC(1)
     C                   MOVEA     FMHEDC        MC(11)
     C                   MOVEA     I4TINC        MC(16)
     C                   MOVEA     I4THED        MC(26)
     C                   MOVEA     MC            I2MSGD
     C                   MOVE      'OMQA017'     I2MSID
     C                   EXSR      SNDAPI
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRREDS - Request display                                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRREDS        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   MOVE      *BLANKS       R2MSID
     C                   MOVE      *BLANKS       R2MSTP
     C                   MOVE      *BLANKS       R2MSGD
      *
      *          ------------------------------------------
      *          Fill function keys
      *          ------------------------------------------
     C                   EVAL      R2FUN# = 0
      *
      *          ------------------------------------------
      *          Fill the request key
      *          ------------------------------------------
     C                   MOVE      *BLANKS       R2KEY
     C                   MOVEL     FMIRAH        R2KAPP
     C                   MOVEL     FMIRIT        R2KERR
      *
      *          ------------------------------------------
      *          Fill the processing array
      *          ------------------------------------------
     C                   MOVE      *OFF          R2EOP
     C                   MOVE      *OFF          R2RDSP
     C                   MOVE      *OFF          R2FILL
     C                   MOVE      *OFF          R2PAPC
      *
      *          ------------------------------------------
      *          Fill the action code
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      W#OPTI = ' 5'                                X01
     C                   MOVE      '50'          R2ACTC
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          Call the API
      *          ------------------------------------------
     C                   CALL      'OMQUSIRE'    $USIRE                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#USIR = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#USIR = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          F3=Exit
      *          ------------------------------------------
     C                   IF        R2FKU# = 3                                   B01
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          F12=Cancel
      *          ------------------------------------------
     C                   IF        R2FKU# = 12                                  B02
     C                   EVAL      *INKL = *ON
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Error?
      *          ------------------------------------------
     C                   IF        R2MSTP = 'E'                                 B03
     C                   EVAL      MSGID = R2MSID
     C                   EVAL      MSGF = 'OMHAPI'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   EVAL      MSGDTA = R2MSGD
     C                   CALLP     OMH901C  ( MSGID : MSGF : MSGLIB : MSGDTA
     C                             )
     C                   ENDIF                                                  E03
      *
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Send the completion message
      *          ------------------------------------------
     C                   IF        R2MSID <> *BLANKS                            B01
     C                   EVAL      MSGF = 'OMHAPI'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   EVAL      D#MSGD = R2MSGD
     C                   CALLP     OMH901C  ( R2MSID : MSGF : MSGLIB :
     C                             D#MSGD )
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRINSP - Work with spooled files                             *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRINSP        BEGSR
      *
      *          ------------------------------------------
      *          Get incident spool file library
      *          ------------------------------------------
     C     FMHEDC        CHAIN     OMHDDL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B01
     C                   MOVEL     DDISFL        P#LIB
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If subroutine is called in edit mode
      *          ------------------------------------------
     C                   IF        I4UPD = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Check if output queue exists
      *          ------------------------------------------
     C                   MOVEL     FMINC#        P#OBJC
     C                   MOVE      DDISFL        P#OBJL
     C                   EVAL      P#OBJT = '*OUTQ'
     C                   CALLP     OMH906C ( P#OBJC : P#OBJL : P#OBJT :
     C                             P#OBJD : P#STAT )
      *
      *          ------------------------------------------
      *          Error if not found
      *          ------------------------------------------
     C                   IF        P#STAT <> C#NORM                             B02
     C                   MOVEA     FMINC#        MC(11)
     C                   MOVEA     DDISFL        MC(21)
     C                   MOVEA     P#OBJT        MC(31)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'LNK0002'
     C                   EXSR      SNDMSG
     C                   EVAL      W#ERR = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Subroutine called in process mode
      *          ------------------------------------------
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Call 'Work with incident spool files'
      *          ------------------------------------------
     C                   CALLP     OMH020C ( P#OBJL : P#OBJC : P#STAT )
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRQUES - Start Question & Answer                             *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRQUES        BEGSR
      *
      *          ------------------------------------------
      *          Open OMQAHL1 if not already open
      *          ------------------------------------------
     C                   IF        I#QAH1 <> *ON                                B01
     C                   OPEN      OMQAHL1
     C                   EVAL      I#QAH1 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Call program to select question list
      *          ------------------------------------------
     C                   EVAL      P#HEDC = FMHEDC
     C                   EVAL      P#OQL# = *ZEROS
     C                   CALLP     OMX120 ( P#HEDC : P#OQL# : P#DESC )
      *
      *          ------------------------------------------
      *          Cancel if no list was selected
      *          ------------------------------------------
     C                   IF        P#OQL# = *ZEROS                              B01
     C                   EVAL      *INKL = *ON
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Check if this question list has
      *          already been processed
      *          ------------------------------------------
     C     $AH1K1        SETLL     OMQAHL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      MSGID = 'QUL0001'
     C                   EXSR      SNDMSG
     C                   EXSR      SRSC04
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Supply helpdesk code, incident number and
      *          the question list that should be started
      *          ------------------------------------------
     C                   MOVE      FMHEDC        P#HEDC
     C                   MOVE      FMINC#        P#INC#
     C                   EVAL      P#QLS# = P#OQL#
     C                   EVAL      QLS = *ZEROS
     C                   EVAL      ANS = *ZEROS
     C                   EVAL      QSQ = *ZEROS
     C                   EVAL      QUE = *ZEROS
      *
      *          ------------------------------------------
      *          Set action to Ask questions
      *          ------------------------------------------
     C                   EVAL      P#ACTN = C#QUES
     C                   MOVE      '20'          I4ACTC
      *
      *          ------------------------------------------
      *          Call program to ask questions
      *          ------------------------------------------
     C                   CALL      'OMH0071'     $H0071                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#0071 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#0071 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If return status is *EXIT, then exit pgm
      *          ------------------------------------------
     C                   IF        P#STAT = C#EXIT                              B03
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          If return status is *NORM
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B04
      *
      *          ------------------------------------------
      *          Call program to write records to history
      *          ------------------------------------------
     C                   CALL      'OMH0072'     $H0072                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#0072 = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#0072 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Force end of processing loop when return
      *          status was not *NORM
      *          ------------------------------------------
     C                   ELSE                                                   X04
     C                   EVAL      *INKL = *ON
     C                   ENDIF                                                  E04
      *
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRDREQ - Disconnect request                                  *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRDREQ        BEGSR
      *
      *          ------------------------------------------
      *          Fill required fields in I4BUF
      *          ------------------------------------------
     C                   CLEAR                   I4BUF
     C                   MOVE      FMHEDC        I4HEDC
     C                   MOVE      FMINC#        I4INC#
     C                   MOVEL     FMIRAH        I4TAPP
     C                   MOVEL     FMIRIT        I4TREQ
      *
      *          ------------------------------------------
      *          Fill action code
      *          ------------------------------------------
     C                   MOVE      '87'          I4ACTC
      *
      *          ------------------------------------------
      *          Fill the processing options in I4PRC
      *          ------------------------------------------
     C                   MOVE      *OFF          I4EOP
     C                   MOVE      *ON           I4UPD
     C                   MOVE      *ON           I4UPB
     C                   MOVE      '1'           I4RCM                          Disconnect
      *
      *          ------------------------------------------
      *          Instruct API to send messages
      *          ------------------------------------------
     C                   MOVE      PGMQ          I4MSQ
      *
      *          ------------------------------------------
      *          Call OMQCHGIN
      *          ------------------------------------------
     C                   CALL      'OMQCHGIN'    $CHGIN                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#CHIN = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#CHIN = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If OMQCHGIN ended with an error
      *          ------------------------------------------
     C                   IF        I4MSTP <> *BLANKS                            B01
      *
      *          ------------------------------------------
      *          Send message and force end of loop
      *          ------------------------------------------
     C                   EXSR      SNDAPI
     C                   EVAL      *INKL = *ON
      *
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Send message to confirm that incident was
      *          disconnected from request
      *          ------------------------------------------
     C                   MOVEA     FMINC#        MC(1)
     C                   MOVEA     FMHEDC        MC(11)
     C                   MOVEA     I4TREQ        MC(16)
     C                   MOVEA     I4TAPP        MC(26)
     C                   MOVEA     MC            I2MSGD
     C                   MOVE      'OMQA016'     I2MSID
     C                   EXSR      SNDAPI
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * USROPT - User option processing                              *
      *                                                              *
      * This routine handles possible user options.                  *
      *                                                              *
      * Input  - W#OPTI - User option                                *
      *                                                              *
      * Output - *INKL  - Set to *ON if an error occured.            *
      *          I#OPTI - Set if an error occured.                   *
      *                                                              *
      * ------------------------------------------------------------ *
     C     USROPT        BEGSR
      *
      *          ------------------------------------------
      *          Get the user option.
      *          ------------------------------------------
     C     W#OPTI        CHAIN     OMHUOL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *                    --------------------------------
      *                    User option not found.
      *                    --------------------------------
     C                   IF        *IN99 = *ON                                  B01
     C                   MOVEL     W#OPTI        MSGDTA
     C                   EVAL      MSGID = 'OPTE021'
     C                   EXSR      SNDMSG
     C                   EVAL      *INKL = *ON
     C                   EVAL      I#OPTI = *ON
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Check if the program is not adopting authority.
      *          ------------------------------------------
     C                   MOVEL     UOPGMN        D#PMIC
     C                   MOVE      UOPGML        D#PMIC
     C                   CALLP     QCLRPGMI ( P#PMI : D#PMI# : D#PMIF :
     C                             D#PMIC : P#ERR )
      *
      *                    --------------------------------
      *                    Error while retrieving program information.
      *                    --------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B02
     C                   MOVEL     W#OPTI        MSGDTA
     C                   EVAL      MSGID = 'OPTE018'
     C                   EXSR      SNDMSG
      *
     C                   MOVE      D#MSID        MSGID
     C                   MOVEL     D#MSGD        MSGDTA
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C  ( MSGID : MSGF : MSGLIB : MSGDTA
     C                             )
     C                   EVAL      *INKL = *ON
     C                   EVAL      I#OPTI = *ON
     C                   ELSE                                                   X02
      *
      *                    --------------------------------
      *                    Check if the program is adopting
      *                    --------------------------------
     C                   EVAL      %SUBST(D#PMIC:11:10) = DP1PLC
     C                   IF        DP1UAC <> 'N'                                B03
     C                   MOVEA     D#PMIC        MC(1)
     C                   MOVEA     W#OPTI        MC(21)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'OPTE019'
     C                   EXSR      SNDMSG
     C                   EVAL      *INKL = *ON
     C                   EVAL      I#OPTI = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Call the program.
      *          ------------------------------------------
      *
      *                    --------------------------------
      *                    Without parameters.
      *                    --------------------------------
     C                   EVAL      P#OPGM = *BLANKS
     C     DP1PLC        CAT       '/':0         P#OPGM
     C                   EVAL      P#OPGM = %TRIMR(P#OPGM) + DP1PGC
     C                   EVAL      P#STAT = C#NORM
      *
     C                   IF        UOPRMI = *OFF                                B04
     C                   CALLP(E)  P#OPGM_001()
     C                   EVAL      *IN99 = %ERROR
     C                   ELSE                                                   X04
      *
      *                    --------------------------------
      *                    With parameters.
      *                    --------------------------------
     C                   EVAL      P#ENTC = 'INC'
     C                   MOVE      FMHEDC        D#HEDC
     C                   MOVE      FMINC#        D#INC#
     C                   MOVE      W#OPTI        P#OPT1
      *
     C                   IF        UOINTL = '*V1R1M0'                           B05
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP(E)  P#OPGM_002 ( P#ENTC : D#INC : P#OPT1 :
     C                             P#STAT )
     C                   EVAL      *IN99 = %ERROR
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
      *                    --------------------------------
      *                    Error.
      *                    --------------------------------
     C                   IF        *IN99 = *ON                                  B04
     C                             OR P#STAT = C#TERM
     C                   MOVEL     W#OPTI        MSGDTA
     C                   EVAL      MSGID = 'OPTE020'
     C                   EXSR      SNDMSG
     C                   EVAL      *INKL = *ON
     C                   EVAL      I#OPTI = *ON
     C                   ELSE                                                   X04
      *
      *                    --------------------------------
      *                    Cancel
      *                    --------------------------------
     C                   IF        P#STAT = C#CNCL                              B05
     C                   EVAL      *INKL = *ON
     C                   ELSE                                                   X05
      *
      *                    --------------------------------
      *                    Exit
      *                    --------------------------------
     C                   IF        P#STAT = C#EXIT                              B06
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSFxx - Subfile routines.                                   *
      *                                                              *
      * SRSFEF   Execute format                                      *
      * SRSFCH   Chain subfile record                                *
      * SRSFRC   REad changed subfile record                         *
      * SRSFUP   Update subfile record                               *
      *                                                              *
      * Returns normal *IN99 processing                              *
      *                                                              *
      * ------------------------------------------------------------ *
      *
      *          ------------------------------------------
      *          Execute format
      *          ------------------------------------------
     C     SRSFEF        BEGSR
     C                   SELECT                                                 B01
     C                   WHEN      I#VIEW = '1'                                 X01
     C                   EXFMT     OMH01802
     C                   WHEN      I#VIEW = '2'                                 X01
     C                   EXFMT     OMH01805
     C                   WHEN      I#VIEW = '3'                                 X01
     C                   EXFMT     OMH01808
     C                   WHEN      I#VIEW = '4'                                 X01
     C                   EXFMT     OMH01819
     C                   ENDSL                                                  E01
     C                   ENDSR
      *
      *          ------------------------------------------
      *          Chain
      *          ------------------------------------------
     C     SRSFCH        BEGSR
     C                   SELECT                                                 B01
     C                   WHEN      I#VIEW = '1'                                 X01
     C     RRN01         CHAIN     OMH01801
     C                   EVAL      *IN99 = NOT %FOUND
     C                   WHEN      I#VIEW = '2'                                 X01
     C     RRN01         CHAIN     OMH01804
     C                   EVAL      *IN99 = NOT %FOUND
     C                   WHEN      I#VIEW = '3'                                 X01
     C     RRN01         CHAIN     OMH01807
     C                   EVAL      *IN99 = NOT %FOUND
     C                   WHEN      I#VIEW = '4'                                 X01
     C     RRN01         CHAIN     OMH01818
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDSL                                                  E01
     C                   ENDSR
      *
      *          ------------------------------------------
      *          Read changed
      *          ------------------------------------------
     C     SRSFRC        BEGSR
     C                   SELECT                                                 B01
     C                   WHEN      I#VIEW = '1'                                 X01
     C                   READC(E)  OMH01801
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   WHEN      I#VIEW = '2'                                 X01
     C                   READC(E)  OMH01804
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   WHEN      I#VIEW = '3'                                 X01
     C                   READC(E)  OMH01807
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   WHEN      I#VIEW = '4'                                 X01
     C                   READC(E)  OMH01818
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   ENDSL                                                  E01
     C                   ENDSR
      *
      *          ------------------------------------------
      *          Update
      *          ------------------------------------------
     C     SRSFUP        BEGSR
     C                   SELECT                                                 B01
     C                   WHEN      I#VIEW = '1'                                 X01
     C                   UPDATE    OMH01801
     C                   WHEN      I#VIEW = '2'                                 X01
     C                   UPDATE    OMH01804
     C                   WHEN      I#VIEW = '3'                                 X01
     C                   UPDATE    OMH01807
     C                   WHEN      I#VIEW = '4'                                 X01
     C                   UPDATE    OMH01818
     C                   ENDSL                                                  E01
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SROMS  - Check if OMS is installed on the system             *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SROMS         BEGSR
      *
      *          ------------------------------------------
      *          Call program to check if OMS is installed
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          If return status is not normal, then OMS
      *          is not installed.
      *          ------------------------------------------
     C                   EVAL      W#OMS = *OFF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * *INZSR - Program initialisation                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Explicit numeric field definitions
      *          ------------------------------------------
     C                   EVAL      RRN01 = *ZEROS
     C                   EVAL      RRN03 = *ZEROS
     C                   EVAL      RRN04 = *ZEROS
     C                   EVAL      MS = *ZEROS
     C                   EVAL      IP = *ZEROS
     C                   EVAL      IX = *ZEROS
     C                   EVAL      @S = *ZEROS
      *
      *          ------------------------------------------
      *          Explicit character field definitions
      *          ------------------------------------------
     C                   EVAL      W#WRKI = *BLANKS
     C                   EVAL      P#OUTC = *BLANKS
     C                   EVAL      W#MODE = *BLANKS
     C                   EVAL      P#MSD = *BLANKS
     C                   EVAL      W#STR = *BLANKS
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
      *          Keyfield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Savefields
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Indicator definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Miscellaneous definitions
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          API parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *
      *          ------------------------------------------
      *          Parameter list for OMX105
      *          ------------------------------------------
     C     $MX105        PLIST
     C                   PARM                    FFHEDC
     C                   PARM                    P#HEDD
      *
      *
      *          ------------------------------------------
      *          Parameter list for OMX1192
      *          ------------------------------------------
     C     $MX119        PLIST
     C                   PARM                    P#HEDC
     C                   PARM      *BLANKS       P#USID
     C                   PARM      *BLANKS       P#USDC
      *
      *          ------------------------------------------
      *          Parameter list for OMX120
      *          ------------------------------------------
     C     $MX120        PLIST
     C                   PARM      FMHEDC        P#HEDC
     C                   PARM      *ZEROS        P#OQL#
     C                   PARM                    P#DESC
      *
      *          ------------------------------------------
      *          Parameter list for OMX123
      *          ------------------------------------------
     C     $AX123        PLIST
     C                   PARM                    FFHEDC
     C                   PARM                    FFSTA1
     C                   PARM                    P#STAD
      *
      *          ------------------------------------------
      *          Parameter list for OMX123
      *          ------------------------------------------
     C     $BX123        PLIST
     C                   PARM                    FFHEDC
     C                   PARM                    FFSTA2
     C                   PARM                    P#STAD
      *
      *          ------------------------------------------
      *          Parameter list for OMX123
      *          ------------------------------------------
     C     $CX123        PLIST
     C                   PARM                    FFHEDC
     C                   PARM                    FFSTA3
     C                   PARM                    P#STAD
      *          ------------------------------------------
      *          Aroen inserted code
      *          ------------------------------------------
      *                    PARM           FFSTA2
      *                    PARM           FFSTA3
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter list for OMX125
      *          ------------------------------------------
     C     $MX125        PLIST
     C                   PARM                    FFHEDC
     C                   PARM                    FFITPC
     C                   PARM                    P#ITPD
      *
      *          ------------------------------------------
      *          Parameter list for OMX126
      *          ------------------------------------------
     C     $MX126        PLIST
     C                   PARM                    FFHEDC
     C                   PARM                    FFSTRC
     C                   PARM                    P#STRD
      *
      *          ------------------------------------------
      *          Parameter list for OMX128
      *          ------------------------------------------
     C     $MX128        PLIST
     C                   PARM                    FFCUSC
     C                   PARM                    P#NAMD
      *
      *          ------------------------------------------
      *          Parameter list for OMX129 (get contact)
      *          ------------------------------------------
     C     $MX129        PLIST
     C                   PARM                    FFCUSC
     C                   PARM                    FFCCTC
     C                   PARM                    P#NAMD
      *
      *          ------------------------------------------
      *          Parameter list for OMX131
      *          ------------------------------------------
     C     $MX131        PLIST
     C                   PARM                    FFHEDC
     C                   PARM                    FFALSC
     C                   PARM                    P#ALSD
      *
      *          ------------------------------------------
      *          Parameter list for OMX203 (Select user option)
      *          ------------------------------------------
     C     $MX203        PLIST
     C                   PARM                    UOSUL#
     C                   PARM                    P#UOPC
      *
      *          ------------------------------------------
      *          Parameter list for OMX305
      *          ------------------------------------------
     C     $MX305        PLIST
     C                   PARM      '3'           P#QOAI
     C                   PARM      FMQUES        P#QUES
     C                   PARM      FMANSC        P#ANSC
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C
      *          ------------------------------------------
     C     $X901C        PLIST
     C                   PARM                    MSGID
     C                   PARM      'OMHMSG'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGDTA
      *
     C     $X901$        PLIST
     C                   PARM                    I2MSID
     C                   PARM      'OMHAPI'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    I2MSGD
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      *          ------------------------------------------
     C     $X9012        PLIST
     C                   PARM                    MSGID
     C                   PARM      'QCPFMSG'     MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGDTA
      *
      *          ------------------------------------------
      *          Parameter list for OMH902C
      *          ------------------------------------------
     C     $X902C        PLIST
     C                   PARM                    PGMQ
      *
      *          ------------------------------------------
      *          Parameter list for OMH904C
      *          ------------------------------------------
     C     $X904C        PLIST
     C                   PARM                    MSGID
     C                   PARM                    MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGTXT
      *
      *          ------------------------------------------
      *          Parameter list for OMH906C
      *          ------------------------------------------
     C     $X906C        PLIST
     C                   PARM                    P#OBJC
     C                   PARM                    P#OBJL
     C                   PARM                    P#OBJT
     C                   PARM                    P#OBJD
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH908C
      *          ------------------------------------------
     C     $X908C        PLIST
     C                   PARM      ' '           P#OPTI
     C                   PARM                    P#INDT
     C                   PARM                    P#OTDT
     C                   PARM                    P#FMIN
     C                   PARM                    P#FMOT
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH933C
      *          ------------------------------------------
     C     $X933C        PLIST
     C                   PARM                    P#STAT
      *
      *
      *          ------------------------------------------
      *          Parameter list for OMH961
      *          ------------------------------------------
     C     $MX961        PLIST
     C                   PARM                    PGM
     C                   PARM                    P#FILE
      *
      *
      *          ------------------------------------------
      *          Parameter list for OMH0071
      *          ------------------------------------------
     C     $H0071        PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    P#CTN#
     C                   PARM                    P#INC#
     C                   PARM                    P#QLS#
     C                   PARM                    P#ACTN
     C                   PARM                    P#QUE
     C                   PARM                    P#ANS
     C                   PARM                    P#QLS
     C                   PARM                    P#QSQ
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH0072
      *          ------------------------------------------
     C     $H0072        PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    P#INC#
     C                   PARM      I4ACTC        P#ACTC
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
      *          Parameter list for OMH015
      *          ------------------------------------------
     C     $MH015        PLIST
     C                   PARM                    P#ACTC
     C                   PARM                    P#HEDC
     C                   PARM                    P#CTN#
     C                   PARM                    FFCUSC
     C                   PARM                    FFCCTC
     C                   PARM      C#NORM        P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH020C
      *          ------------------------------------------
     C     $H020C        PLIST
     C                   PARM                    P#OBJL
     C                   PARM                    P#OBJC
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH021
      *          ------------------------------------------
     C     $MH021        PLIST
     C                   PARM      FMHEDC        P#HEDC
     C                   PARM      FMINC#        P#INC#
     C                   PARM      '*     '      P#OUTC
     C                   PARM      *BLANKS       P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH022
      *          ------------------------------------------
     C     $MH022        PLIST
     C                   PARM      FMHEDC        P#HEDC
     C                   PARM      NCSTRC        P1STRC
     C                   PARM      NCRES#        P1RES#
     C                   PARM      NCSUL#        P1SUL#
     C                   PARM      NCSTAT        P1STAT
     C                   PARM      *BLANKS       P2STRC
     C                   PARM      *ZEROS        P2RES#
     C                   PARM      '0'           P2SUL#
     C                   PARM      *BLANKS       P2STAT
      *
      *          ------------------------------------------
      *          Parameter list for QCMDEXEC
      *          ------------------------------------------
     C     $MH023        PLIST
     C                   PARM                    P#HEDC
     C                   PARM      *BLANKS       P#INC#
     C                   PARM      0             P#ITH#
     C                   PARM      '60'          ACTC
     C                   PARM      '*NORM'       P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH027
      *          ------------------------------------------
     C     $MH027        PLIST
     C                   PARM                    P#HEDC
     C                   PARM      FMINC#        P#INC#
     C                   PARM                    P#STAT
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
      *          Parameter list for OMH031
      *          ------------------------------------------
     C     $MH031        PLIST
     C                   PARM                    NCHEDC
     C                   PARM                    NCINC#
     C                   PARM                    FFALSC
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMQCHGCC
      *          ------------------------------------------
     C     $CHGCC        PLIST
     C                   PARM      C#INTL        C4INTL
     C                   PARM                    C4ACTC
     C                   PARM                    C4BUF
     C                   PARM                    C4PRC
     C                   PARM                    C4ERR
     C                   PARM                    C4FLD
      *
      *          ------------------------------------------
      *          Parameter list for OMQCHGIN
      *          ------------------------------------------
     C     $CHGIN        PLIST
     C                   PARM      C#INTL        I4INTL
     C                   PARM                    I4ACTC
     C                   PARM                    I4BUF
     C                   PARM                    I4PRC
     C                   PARM      *BLANKS       I4ERR
     C                   PARM                    I4FLD
      *
      *          ------------------------------------------
      *          Parameter list for OMQUSIIN
      *          ------------------------------------------
     C     $USIIN        PLIST
     C                   PARM      C#INTL        I2INTL
     C                   PARM                    I2ACTC
     C                   PARM                    I2INC
     C                   PARM                    I2PRCA
     C                   PARM                    I2ERR
     C                   PARM                    I2FUN#
     C                   PARM                    I2FUN
     C                   PARM                    I2FKU#
     C                   PARM                    I2CFLC
     C                   PARM                    I2KEY
      *
      *          ------------------------------------------
      *          Parameter list for OMQPRTIN
      *          ------------------------------------------
     C     $PRTIN        PLIST
     C                   PARM      'V3R0M0  '    I3INTL
     C                   PARM                    I3ACTC
     C                   PARM                    I3BUF
     C                   PARM                    I3PRC
     C                   PARM                    I3ERR
     C                   PARM                    I3FLD
      *
      *          ------------------------------------------
      *          Parameter list for OMQUSIRE
      *          ------------------------------------------
     C     $USIRE        PLIST
     C                   PARM      C#INTL        R2INTL
     C                   PARM                    R2ACTC
     C                   PARM                    R2REQ
     C                   PARM                    R2PRCA
     C                   PARM                    R2ERR
     C                   PARM                    R2FUN#
     C                   PARM                    R2FUN
     C                   PARM                    R2FKU#
     C                   PARM                    R2CFLC
     C                   PARM                    R2KEY
      *
      *          ------------------------------------------
      *          Parameter list for user option programs (V3R0M0)
      *          ------------------------------------------
     C     $OPGM1        PLIST
     C                   PARM                    P#ENTC
     C                   PARM                    D#INC
     C                   PARM                    P#OPT1
     C                   PARM      C#NORM        P#STAT
      *
      *          ------------------------------------------
      *          OMINCJ1 Incident file
      *          ------------------------------------------
     C     $NC1K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#INC#
      *
     C     $NC1K2        KLIST
     C                   KFLD                    S#HEDC
     C                   KFLD                    S#INC#
      *
     C     $NC1K3        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMINC#
      *
     C     $NC1K4        KLIST
     C                   KFLD                    K4HEDC
     C                   KFLD                    K4INC#
      *
      *          ------------------------------------------
      *          OMINCJ2 Incident file
      *          ------------------------------------------
     C     $NC2K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#PRI#
      *
     C     $NC2K2        KLIST
     C                   KFLD                    S#HEDC
     C                   KFLD                    S#PRI#
     C                   KFLD                    S#INC#
      *
     C     $NC2K3        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMPRI#
     C                   KFLD                    FMINC#
      *
      *          ------------------------------------------
      *          OMINCJ3 Incident file - NE *CMP
      *          ------------------------------------------
     C     $NC3K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#INC#
      *
     C     $NC3K2        KLIST
     C                   KFLD                    S#HEDC
     C                   KFLD                    S#INC#
      *
      *          ------------------------------------------
      *          OMINCJ4 Incident file - NE *CMP
      *          ------------------------------------------
     C     $NC4K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#PRI#
      *
     C     $NC4K2        KLIST
     C                   KFLD                    S#HEDC
     C                   KFLD                    S#PRI#
     C                   KFLD                    S#INC#
      *
      *          ------------------------------------------
      *          OMINCJ5 Incident file - with CTN#
      *          ------------------------------------------
     C     $NC5K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    P#CTN#
     C                   KFLD                    K#INC#
      *
     C     $NC5K2        KLIST
     C                   KFLD                    S#HEDC
     C                   KFLD                    P#CTN#
     C                   KFLD                    S#INC#
      *
      *          ------------------------------------------
      *          OMINCJ6 Incident file - with CTN#
      *          ------------------------------------------
     C     $NC6K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    P#CTN#
     C                   KFLD                    K#PRI#
      *
     C     $NC6K2        KLIST
     C                   KFLD                    S#HEDC
     C                   KFLD                    P#CTN#
     C                   KFLD                    S#PRI#
     C                   KFLD                    S#INC#
      *
      *          ------------------------------------------
      *          OMINCL4 Incident file - on AIN#
      *          ------------------------------------------
     C     $NC4L1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMINC#
      *
      *          ------------------------------------------
      *          OMHTVL1 Selections and default subset
      *          ------------------------------------------
     C     $RTVK1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    D#USID
     C                   KFLD                    K#FMTC
      *
     C     $RTVK2        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    D#USID
     C                   KFLD                    K#FMTC
     C                   KFLD                    K#RTV#
      *
      *          ------------------------------------------
      *          OMRIRL1 Incident relations
      *          ------------------------------------------
     C     $IR1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMINC#
      *
     C     $IR2K1        KLIST
     C                   KFLD                    K#IRAH
     C                   KFLD                    K#IRIT
      *
      *
      *          ------------------------------------------
      *          OMQAHL1 Q & A history
      *          ------------------------------------------
     C     $AH1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMINC#
     C                   KFLD                    P#OQL#
      *
      *          ------------------------------------------
      *          OMHQUL1 Questions
      *          ------------------------------------------
     C     $QU1K1        KLIST
     C                   KFLD                    AHHEDC
     C                   KFLD                    AHQUE#
      *
      *          ------------------------------------------
      *          OMANSL1 Answers
      *          ------------------------------------------
     C     $NS1K1        KLIST
     C                   KFLD                    AHHEDC
     C                   KFLD                    AHQUE#
     C                   KFLD                    AHANS#
      *
      *          ------------------------------------------
      *          OMCCAL1 Customer calls
      *          ------------------------------------------
     C     $CA1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#CTN#
      *
      *          ------------------------------------------
      *          OMCCOL1 Customer contacts
      *          ------------------------------------------
     C     $CO1K1        KLIST
     C                   KFLD                    CACUSC
     C                   KFLD                    CACCTC
      *
      *          ------------------------------------------
      *          OMESAL1 Employee status route access
      *          ------------------------------------------
     C     $SA1K1        KLIST
     C                   KFLD                    FFHEDC
     C                   KFLD                    D#USID
      *          ------------------------------------------
      *          OMHDEL1 Employee allocation
      *          ------------------------------------------
     C     $HDEL1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    D#USID
      *
      *          ------------------------------------------
      *          OMSTRL1 Status routes
      *          ------------------------------------------
     C     $TR1K1        KLIST
     C                   KFLD                    FFHEDC
      *
      *          ------------------------------------------
      *          Retrieve authorization
      *          ------------------------------------------
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '0'
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD )
      *
      *          ------------------------------------------
      *          Continue if authorisation ok
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B01
      *
      *          ------------------------------------------
      *          Fill screen fields from parameters
      *          ------------------------------------------
     C                   MOVE      P#HEDC        FMHEDC
     C                   MOVEL     D8HEDD        FMHEDD
      *
      *          ------------------------------------------
      *          Get message data
      *          ------------------------------------------
     C                   EXSR      SRGVAR
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * *PSSR  - Standard RPG error handler                          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     *PSSR         BEGSR
      *
      *          ------------------------------------------
      *          Do if error not in this routine
      *          ------------------------------------------
     C                   IF        I#PSSR <> *ON                                B01
     C                   EVAL      I#PSSR = *ON
      *
      *          ------------------------------------------
      *          Post information about the last used file
      *          ------------------------------------------
     C                   SELECT                                                 B02
     C                   WHEN      E#FILE = 'OMH018D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMINCJ1 '                          X02
     C                   MOVEL     INCJ1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMINCJ2 '                          X02
     C                   MOVEL     INCJ2         P#FILE
      *
     C                   WHEN      E#FILE = 'OMINCJ3 '                          X02
     C                   MOVEL     INCJ3         P#FILE
      *
     C                   WHEN      E#FILE = 'OMINCJ4 '                          X02
     C                   MOVEL     INCJ4         P#FILE
      *
     C                   WHEN      E#FILE = 'OMINCJ5 '                          X02
     C                   MOVEL     INCJ5         P#FILE
      *
     C                   WHEN      E#FILE = 'OMINCJ6 '                          X02
     C                   MOVEL     INCJ6         P#FILE
      *
     C                   WHEN      E#FILE = 'OMINCL4 '                          X02
     C                   MOVEL     INCL4         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHDDL1 '                          X02
     C                   MOVEL     HDDL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHUOL1 '                          X02
     C                   MOVEL     HUOL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHTVL1 '                          X02
     C                   MOVEL     HTVL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMRIRL1 '                          X02
     C                   MOVEL     RIRL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMERRL1 '                          X02
     C                   MOVEL     ERRL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMQAHL1 '                          X02
     C                   MOVEL     QAHL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHQUL1 '                          X02
     C                   MOVEL     HQUL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMANSL1 '                          X02
     C                   MOVEL     ANSL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMCCAL1 '                          X02
     C                   MOVEL     CCAL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMCCOL1 '                          X02
     C                   MOVEL     CCOL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMESAL1 '                          X02
     C                   MOVEL     ESAL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMSTRL1 '                          X02
     C                   MOVEL     STRL1         P#FILE
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
      *          Exit
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRTTLR - Total last record processing                        *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          Close called programs if necessary
      *          ------------------------------------------
     C                   IF        I#H021 = *ON                                 B01
     C                   CALLP     OMH021()
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Close OMX203 if called
      *          ------------------------------------------
     C                   IF        I#X203 = *ON                                 B01
     C                   CALLP     OMX203_001()
     C                   ENDIF                                                  E01
      *
     C                   IF        I#0071 = *ON                                 B01
     C                   CALLP     OMH0071()
     C                   ENDIF                                                  E01
      *
     C                   IF        I#0072 = *ON                                 B01
     C                   CALLP     OMH0072()
     C                   ENDIF                                                  E01
      *
     C                   IF        I#H015 = *ON                                 B01
     C                   CALLP     OMH015()
     C                   ENDIF                                                  E01
      *
     C                   IF        I#H031 = *ON                                 B01
     C                   CALLP     OMH031()
     C                   ENDIF                                                  E01
      *
     C                   IF        I#X129 = *ON                                 B01
     C                   CALLP     OMX129()
     C                   ENDIF                                                  E01
      *
     C                   IF        I#CHIN = *ON                                 B01
     C                   CALLP     OMQCHGIN()
     C                   ENDIF                                                  E01
      *
     C                   IF        I#USII = *ON                                 B01
     C                   CALLP     OMQUSIIN()
     C                   ENDIF                                                  E01
      *
     C                   IF        I#PRTI = *ON                                 B01
     C                   CALLP     OMQPRTIN_001()
     C                   ENDIF                                                  E01
      *
     C                   IF        I#USIR = *ON                                 B01
     C                   CALLP     OMQUSIRE()
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill return status
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      *INKC = *ON                                  X01
     C                   EVAL      P#STAT = C#EXIT
     C                   WHEN      *INKL = *ON                                  X01
     C                   EVAL      P#STAT = C#CNCL
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          Seton last record and return
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRTV   -  Retrieve selections                               *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRTV         BEGSR
      *
      *          ------------------------------------------
      *          If new screen, then start from last stored
      *          selection
      *          ------------------------------------------
     C                   IF        K#FMTC <> TVFMTC                             B01
     C                   EVAL      TVFMTC = K#FMTC
     C                   EVAL      K#RTV# = *ZEROS
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Retrieve values if F9 is pressed
      *          ------------------------------------------
     C                   IF        *INKI = *ON                                  B01
      *
      *          ------------------------------------------
      *          If no selections for this format then send msg
      *          ------------------------------------------
     C     $RTVK1        SETLL     OMHTVL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B02
     C                   EVAL      MSGID = 'RTV0002'
     C                   EXSR      SNDMSG
      *
      *          ------------------------------------------
      *          If already last retrieved then send msg.
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   ADD       1             K#RTV#
     C                   IF        K#RTV# > 10                                  B03
     C                   EVAL      K#RTV# = *ZEROS
     C                   EVAL      MSGID = 'RTV0001'
     C                   EXSR      SNDMSG
      *
      *          ------------------------------------------
      *          Read the next selections if none found then error
      *          else the sequence number read now is used for next F9
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C     $RTVK2        SETLL     OMHTVL1
     C     $RTVK1        READE(N)  OMHTVL1
     C                   EVAL      *IN99 = %EOF
     C                   IF        *IN99 = *ON                                  B04
     C                   EVAL      MSGID = 'RTV0002'
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X04
     C                   EVAL      K#RTV# = TVRTV#
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          The screen was accepted, save this selection
      *          Check highest free number
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C     $RTVK1        SETLL     OMHTVL1
     C     $RTVK1        READE     OMHTVL1
     C                   EVAL      *IN99 = %EOF
     C                   IF        TVRTV# = *ZEROS                              B02
     C                             AND *IN99 = *OFF
     C     $RTVK1        READE     OMHTVL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          If no record found then this is the first
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      K#RTV# = 10
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Decrement the number is the next.
      *          ------------------------------------------
     C     TVRTV#        SUB       1             K#RTV#
      *
      *          ------------------------------------------
      *          If it is zero then no more available.
      *          Shift the selections up.
      *          ------------------------------------------
     C                   IF        K#RTV# = *ZERO                               B03
      *
     C                   EVAL      K#RTV# = 10
     C     $RTVK2        DELETE    OMHTVL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
     C                   FOR       DO_X2 = 1 TO 9                               B04
     C                   SUB       1             K#RTV#
     C     $RTVK2        CHAIN     OMHTVL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B05
     C                   ADD       1             TVRTV#
     C                   UPDATE    OMHTVR
     C                   ENDIF                                                  E05
     C                   ENDFOR                                                 E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Write this selection.
      *          ------------------------------------------
     C                   MOVE      FMHEDC        TVHEDC
     C                   EVAL      TVUSID = D#USID
     C                   EVAL      TVFMTC = K#FMTC
     C                   EVAL      TVRTV# = K#RTV#
     C                   EVAL      K#RTV# = *ZEROS
     C                   EVAL      TVSDTA = S#SDTA
     C                   WRITE     OMHTVR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGVAR  -  Get variable message text                         *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGVAR        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      %SUBST(MSGF:1:6) = 'OMHMSG'
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 6                                  B01
     C                   EVAL      MSGID = MSD(MS)
      *
      *          ------------------------------------------
      *          Call subprogram
      *          ------------------------------------------
     C                   IF        MSGID <> *BLANKS                             B02
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill message array
      *          ------------------------------------------
     C                   EVAL      %SUBST(VAR(MS):1:132) = MSGTXT
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          Fill message gotten indicator
      *          ------------------------------------------
     C                   EVAL      MSGGET = *ON
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Compile-time arrays                                          *
      * ------------------------------------------------------------ *
**
SAV0003 - Save as default indicator             1
OPT0006 -'0' or '1'                             2
SHW0001 - Show accumulated only indicator       3
LNK0017 - Link or accumulate indicator          4
HLP0002 - Helpdesk code                         5
VIE0005 - View                                  6
