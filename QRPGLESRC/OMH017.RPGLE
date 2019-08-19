     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH017                                        *
      * Function ...: Customer call maintenance                     *
      * Module .....: Help desk                                     *
      *                                                             *
      * Author .....: Remain Software                               *
      * Action .....: Creation.                                     *
      *                                                             *
      * Parameters .: P#HEDC, P#STAT                                *
      *                                                             *
      * This program maintains the incoming calls                   *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators                                             *
      * ----------------------------------------------------------- *
      *                                                             *
      * 70-77 - Screen field error indicators                       *
      *                                                             *
      * 92 - subfile end indicator                                  *
      * 99 - General purpose indicator.                             *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMCCAL2   IF   E           K DISK    INFDS(CCAL2)
     F                                     INFSR(*PSSR)
     FOMCCOL1   IF   E           K DISK    INFDS(CCOL1)
     F                                     INFSR(*PSSR)
     FOMHTVL1   UF A E           K DISK    INFDS(HTVL1)
     F                                     INFSR(*PSSR)
     FOMHUOL1   IF   E           K DISK    INFDS(HUOL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMINCJ5   IF   E           K DISK    INFDS(INCJ5)
     F                                     INFSR(*PSSR)
     FOMH017D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
     F                                     SFILE(OMH01701:RRN01)
     F                                     SFILE(OMH01706:RRN01)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(10)
     D MSD             S              7    DIM(7) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
     D ST3             S              1    DIM(15)
      *COPY QCPYSRC,OMSREFREN
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
     D CCAL2           DS            57
     D CCOL1           DS            57
     D HTVL1           DS            57
     D HUOL1           DS            57
     D INCJ5           DS            57
      *
      *          ------------------------------------------
      *          Screen information data structure
      *          ------------------------------------------
     D INFO            DS
     D  STATUS           *STATUS
     D  D#CRC#               378    379B 0
      *
      *          ------------------------------------------
      *          Format subfile option
      *          ------------------------------------------
     D                 DS
     D W#OPTI                         2
     D  D#OPP1                        1    OVERLAY(W#OPTI)
     D  D#OPP2                        1    OVERLAY(W#OPTI:2)
      *
      *          ------------------------------------------
      *          Map selection fields to data structure
      *          ------------------------------------------
     D                 DS                  INZ
     D D#FMT1                       115
     D  F8CTN#                       10    OVERLAY(D#FMT1)
     D  F8CUSC                       10    OVERLAY(D#FMT1:11)
     D  F8CCTC                       10    OVERLAY(D#FMT1:21)
     D  F8INDT                        6  0 OVERLAY(D#FMT1:31)
     D  F8TMDT                        6  0 OVERLAY(D#FMT1:37)
     D  F8INTM                        6  0 OVERLAY(D#FMT1:43)
     D  F8TMTM                        6  0 OVERLAY(D#FMT1:49)
     D  F8USID                       10    OVERLAY(D#FMT1:55)
     D  F8CSRC                        1    OVERLAY(D#FMT1:65)
     D  F8USF1                       10    OVERLAY(D#FMT1:66)
     D  F8USF2                       25    OVERLAY(D#FMT1:76)
     D  F8USF3                       15    OVERLAY(D#FMT1:101)
     D D#FMT2                       115
     D  D#CTN#                       10    OVERLAY(D#FMT2)
     D  D#CUSC                       10    OVERLAY(D#FMT2:11)
     D  D#CCTC                       10    OVERLAY(D#FMT2:21)
     D  D#INDT                        6  0 OVERLAY(D#FMT2:31)
     D  D#TMDT                        6  0 OVERLAY(D#FMT2:37)
     D  D#INTM                        6  0 OVERLAY(D#FMT2:43)
     D  D#TMTM                        6  0 OVERLAY(D#FMT2:49)
     D  E#USID                       10    OVERLAY(D#FMT2:55)
     D  D#CSRC                        1    OVERLAY(D#FMT2:65)
     D  D#USF1                       10    OVERLAY(D#FMT2:66)
     D  D#USF2                       25    OVERLAY(D#FMT2:76)
     D  D#USF3                       15    OVERLAY(D#FMT2:101)
     D D1DAT#                         7  0
     D D2DAT#                         7  0
     D D#HLP#                         4  0
      *
      *          ------------------------------------------
      *          User options, customer call buffer
      *          ------------------------------------------
     D D#CCA           DS
     D D#HEDC                         5
     D D2CTN#                        10
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
     D                 DS
     D D#PMI#                         9B 0 INZ(348)
     D D#PMIC                        20
     D D#PMIF                         8    INZ('PGMI0100')
      *
      *          ------------------------------------------
      *          Workfields
      *          ------------------------------------------
     D                 DS
     D  ST1                    1     25
     D                                     DIM(25)
     D  D#ST1                  1     25
     D  ST2                   26     50
     D                                     DIM(25)
     D  D#ST2                 26     50
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  E#FILE               201    208
     D  D#USID               254    263
     D  PGMQ             *PROC
      *
      *          ------------------------------------------
      *          Helpdesk data structures.
      *          ------------------------------------------
      /COPY QCPYLESRC,OMHLPDS
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
     D C#CNCL          C                   CONST('*CNCL')
     D C#EXIT          C                   CONST('*EXIT')
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#ALL           C                   CONST('*ALL      ')
     D C#CMP           C                   CONST('*CMP ')
     D C#ALLN          C                   CONST('           *ALL')
     D C#SFL1          C                   CONST(10)
      * Prototype for OMH017
     D OMH017          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH902C'
     D OMH902C         PR                  EXTPGM('OMH902C')
     D PGMQ_                               LIKE(PGMQ)
      *
      *          ------------------------------------------
      * Prototype for 'OMH901C'
     D OMH901C         PR                  EXTPGM('OMH901C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGDTA_                             LIKE(MSGDTA)
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
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
      *          Parameter list for OMX1192 (get user)
      * Prototype for 'OMH015'
     D OMH015          PR                  EXTPGM('OMH015')
     D P#ACTC_                             LIKE(P#ACTC)
     D P#HEDC_                             LIKE(P#HEDC)
     D P#CTN#_                             LIKE(P#CTN#)
     D P#CUSC_                             LIKE(P#CUSC)
     D P#CCTC_                             LIKE(P#CCTC)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH018 (maintain incidents)
      * Prototype for 'OMH018'
     D OMH018          PR                  EXTPGM('OMH018')
     D P#HEDC_                             LIKE(P#HEDC)
     D P#CTN#_                             LIKE(P#CTN#)
     D P#CUSC_                             LIKE(P#CUSC)
     D P#CCTC_                             LIKE(P#CCTC)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMX203 (Select user option)
      * Prototype for 'OMX203'
     D OMX203          PR                  EXTPGM('OMX203')
     D UOSUL#_                             LIKE(UOSUL#)
     D P#UOPC_                             LIKE(P#UOPC)
      *
      *          ------------------------------------------
      * Prototype for 'QCLRPGMI'
     D QCLRPGMI        PR                  EXTPGM('QCLRPGMI')
     D P#PMI_                              LIKEDS(P#PMI)
     D D#PMI#_                             LIKE(D#PMI#)
     D D#PMIF_                             LIKE(D#PMIF)
     D D#PMIC_                             LIKE(D#PMIC)
     D P#ERR_                              LIKE(P#ERR)
      *
      *          ------------------------------------------
      * Prototype for P#OPGM
     D P#OPGM_001      PR                  EXTPGM(P#OPGM)
      * Prototype for P#OPGM
     D P#OPGM_002      PR                  EXTPGM(P#OPGM)
     D P#ENTC_                             LIKE(P#ENTC)
     D D#CCA_                              LIKEDS(D#CCA)
     D P#OPT1_                             LIKE(P#OPT1)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Retrieve authorization
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
      *          Parameter list for OMH015 (add call track)
      * Prototype for 'OMX203'
     D OMX203_001      PR                  EXTPGM('OMX203')
      * Prototype for 'OMH904C'
     D OMH904C         PR                  EXTPGM('OMH904C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGTXT_                             LIKE(MSGTXT)
      *
      *          ------------------------------------------
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
      * *ENTRY Interface for Main Procedure
     D OMH017          PI
     D P#HEDC                         5
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D k#rtv#          S              5P 0
     D s#rr01          S              5P 0
     D DO_X            S              7  0
     D EROR01          S                   LIKE(INDI)
     D EROR03          S                   LIKE(INDI)
     D F8USFN          S             15  0
     D I#IN40          S                   LIKE(*IN40)
     D I#OPTI          S                   LIKE(INDI)
     D I#PSSR          S              1
     D I#UOL1          S                   LIKE(INDI)
     D I#X203          S                   LIKE(INDI)
     D IX              S              5  0
     D IY              S              2  0
     D K#CTN#          S                   LIKE(CTN#)
     D K#FMTC          S                   LIKE(FMTC)
     D K#HEDC          S                   LIKE(HEDC)
     D LOOP01          S                   LIKE(INDI)
     D LOOP03          S                   LIKE(INDI)
     D MA_X            S              5  0
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#ACTC          S                   LIKE(ACTC)
     D P#CCTC          S                   LIKE(CCTC)
     D P#CTN#          S                   LIKE(CTN#)
     D P#CUSC          S                   LIKE(CUSC)
     D P#ENTC          S              3
     D P#FILE          S             57
     D P#FMIN          S              1
     D P#FMOT          S              1
     D P#GRCL          S             10
     D P#GRID          S             10
     D P#HEDD          S                   LIKE(HEDD)
     D P#INDT          S              6  0
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#NAMD          S                   LIKE(NAMD)
     D P#OPGM          S             21
     D P#OPTI          S                   LIKE(INDI)
     D P#OPT1          S              2
     D P#OTDT          S              6  0
     D P#PMQC          S             10
     D P#SUL#          S                   LIKE(SUL#)
     D P#UOPC          S                   LIKE(UOPC)
     D P#USCL          S             10
     D P#USDC          S             50
     D P#USID          S             10
     D RRN01           S              5  0
     D S#SDTA          S                   LIKE(SDTA)
     D W#CRC1          S              5  0
     D W#EOFI          S                   LIKE(INDI)
     D W#F4I           S                   LIKE(INDI)
     D W#OPT1          S                   LIKE(FMOPT1)
     D W#SAV#          S              5  0
     D W#VALC          S              5  0
     D W#VALI          S                   LIKE(INDI)
     D X1              S              2  0
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
      *
      *          ------------------------------------------
      *          Work with customer calls
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B01
     C                   EXSR      SRSC01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Exit
      *          ------------------------------------------
     C                   EXSR      SRTTLR
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
      * SNDMSG - Send message                                        * /
      *                                                              * /
      * This routine displays an error wich occured during user      * /
      * keyed data validation.                                       * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SNDMSG        BEGSR
      *
      *          ------------------------------------------
      *          Call Program sender
      *          ------------------------------------------
     C                   EVAL      MSGF = 'OMHMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C  ( MSGID : MSGF : MSGLIB :
     C                             MSGDTA )
     C                   FOR       MA_X = 1 TO %ELEM(MC)
     C                   EVAL      MC(MA_X) = *BLANK
     C                   ENDFOR
     C                   EVAL      MSGDTA = *BLANK
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRFUTY - Convert date fields                                *   *
      *                                                             *
      * This routine converts the date fields to YMD format         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRFUTY        BEGSR
      *
      *          ------------------------------------------
      *          Convert dates
      *          ------------------------------------------
     C                   EVAL      P#OTDT = *ZEROS
     C                   EVAL      P#FMIN = '?'
     C                   EVAL      P#FMOT = 'Y'
     C                   EVAL      P#OPTI = 'C'
     C                   CALLP     OMH908C ( P#OPTI : P#INDT : P#OTDT :
     C                             P#FMIN : P#FMOT : P#STAT )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRFYTU - Convert date fields                                *   *
      *                                                             *
      * This routine converts the date fields to user format.       *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRFYTU        BEGSR
      *
      *          ------------------------------------------
      *          Convert dates
      *          ------------------------------------------
     C                   EVAL      P#OTDT = *ZEROS
     C                   EVAL      P#FMIN = 'Y'
     C                   EVAL      P#FMOT = '?'
     C                   EVAL      P#OPTI = 'C'
     C                   CALLP     OMH908C ( P#OPTI : P#INDT : P#OTDT :
     C                             P#FMIN : P#FMOT : P#STAT )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC01 - Work with customer call                            *
      *                                                             *
      * This routine displays the first screen where the user can   *
      * select a call track for maintenance actions                 *   *
      *                                                             *
      * ----------------------------------------------------------- *
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
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        RRN01 = *ZEROS                               B03
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH01703
      *
      *          ------------------------------------------
      *          Display: view1, view2, no records (in that sequence)
      *          ------------------------------------------
     C                   IF        *IN90 = *ON
     C                             AND *IN43 = *OFF
     C                   EXFMT     OMH01702
     C                   ENDIF
     C                   IF        *IN90 = *ON
     C                             AND *IN43 = *ON
     C                   EXFMT     OMH01707
     C                   ENDIF
     C                   IF        *IN90 = *OFF
     C                   EXFMT     OMH01705
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Meet several end requests
      *          ------------------------------------------
     C                   IF        *INKC = *ON
     C                   EVAL      P#STAT = '*EXIT'
     C                   EXSR      SRTTLR
     C                   ENDIF
     C                   IF        *INKL = *ON
     C                   EVAL      P#STAT = '*CNCL'
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   IF        *IN42 = *OFF
NB   C                   MOVE      *BLANKS       FMPOSI
     C                   ENDIF
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
     C                   EVAL      *IN(70) = *OFF
     C                   EVAL      *IN(71) = *OFF
     C                   EVAL      *IN(72) = *OFF
     C                   EVAL      *IN(73) = *OFF
     C                   EVAL      *IN(74) = *OFF
     C                   EVAL      *IN(75) = *OFF
     C                   EVAL      *IN(76) = *OFF
     C                   EVAL      *IN(77) = *OFF
     C                   EVAL      EROR01 = *OFF
     C                   EVAL      LOOP01 = *OFF
      *
      *          ------------------------------------------
      *          F5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B03
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F11=Change view
      *          ------------------------------------------
     C                   IF        *INKK = *ON                                  B04
     C                   Z-ADD     W#CRC1        CRRN01                         Don't forget!
     C                   IF        *IN43 = *OFF                                 B05
     C                   EVAL      *IN43 = *ON
     C                   ELSE                                                   X05
     C                   EVAL      *IN43 = *OFF
     C                   ENDIF                                                  E05
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          F6=Add Function
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B05
     C                   EVAL      P#ACTC = '60'
     C                   EVAL      P#CTN# = *BLANKS
     C                   EVAL      P#CUSC = *BLANKS
     C                   EVAL      P#CCTC = *BLANKS
     C                   EVAL      P#STAT = '*NORM'
     C                   CALLP     OMH015 ( P#ACTC : P#HEDC : P#CTN# :
     C                             P#CUSC : P#CCTC : P#STAT )
      *
     C                   SELECT
     C                   WHEN      P#STAT = '*EXIT'
     C                   EXSR      SRTTLR
     C                   ENDSL
      *
     C                   IF        P#STAT <> '*CNCL'                            B06
     C                   CALLP     OMH018 ( P#HEDC : P#CTN# : P#CUSC :
     C                             P#CCTC : P#STAT )
      *
     C                   ENDIF                                                  E06
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          F17=Subset
      *          ------------------------------------------
     C                   IF        *IN17 = *ON                                  B06
     C                   EXSR      SRSC03
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X06
      *
      *          ------------------------------------------
      *          22=Roll up
      *          ------------------------------------------
     C                   IF        *IN22 = *ON                                  B07
     C                   EXSR      SRRU01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X07
      *
      *          ------------------------------------------
      *          Do edit routine
      *          ------------------------------------------
     C                   EXSR      SRED01
     C                   ENDIF                                                  E07
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors or command key
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do processing if subfile records
      *          ------------------------------------------
     C                   IF        RRN01 <> *ZERO                               B02
     C                   EXSR      SRPR01
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
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIN01 - Init                                               *   *
      *                                                             *
      * Initialize the display                                      *   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Get the subset defaults
      *          ------------------------------------------
     C                   EXSR      SRRF03
      *
      *          ------------------------------------------
      *          Do refresh subroutine
      *          ------------------------------------------
     C                   EXSR      SRRF01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRD01 - Read next record                                   *   *
      *                                                             *
      * This routine reads the next record                          *   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRD01        BEGSR
      *
      *          ------------------------------------------
      *          Come on, do it
      *          ------------------------------------------
     C                   READ      OMCCAL2
     C                   EVAL      *IN99 = %EOF
     C                   EVAL      W#EOFI = *IN99
      *
      *          ------------------------------------------
      *          Save the key.
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF
     C                   MOVEL     CAHEDC        K#HEDC
     C                   MOVEL     CACTN#        K#CTN#
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSF01 - Build subfile                                      *   *
      *                                                             *
      * This routine builds one page of subfile records             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSF01        BEGSR
      *
      *          ------------------------------------------
      *          Read first interface record
      *          ------------------------------------------
     C                   EVAL      K#HEDC = P#HEDC
     C     $CA2K1        SETLL     OMCCAL2
     C                   EXSR      SRRD01
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   EVAL      IX = *ZEROS
     C                   DOW       W#EOFI = *OFF                                B01
     C                             AND IX <= C#SFL1
      *
      *          ------------------------------------------
      *          Check valid
      *          ------------------------------------------
     C                   EXSR      SRCHK1
     C                   IF        W#VALI = '1'                                 B02
      *
      *          ------------------------------------------
      *          More valid records at the next page
      *          ------------------------------------------
     C                   IF        IX = C#SFL1                                  B03
     C                   ADD       1             IX
     C                   READP     OMCCAR
     C                   EVAL      *IN99 = %EOF
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Fill subfile record: date
      *          ------------------------------------------
     C                   Z-ADD     CAINDT        P#INDT
     C                   EXSR      SRFYTU
     C                   Z-ADD     P#OTDT        FMINDT
      *
      *          ------------------------------------------
      *          Check if open incidents.
      *          ------------------------------------------
     C     $NJ5K1        CHAIN     OMINCJ5
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       NCSTAT = C#CMP                               B04
     C                   IF        *IN99 = *ON
     C                   LEAVE
     C                   ENDIF
     C     $NJ5K1        READE     OMINCJ5
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E04
      *
     C                   IF        *IN99 = *OFF                                 B04
     C                   MOVE      '*'           FMINOI
     C                   ELSE                                                   X04
     C                   MOVE      *BLANKS       FMINOI
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          and the rest
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPT1
     C                   MOVEL     CACUSC        FMCUSC
     C                   MOVEL     CACCTC        FMCCTC
     C     $CO1K1        CHAIN     OMCCOR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF
     C                   MOVEL     COTEL#        FMTEL#
     C                   ENDIF
     C                   IF        *IN99 = *ON
     C                   MOVE      *BLANKS       FMTEL#
     C                   ENDIF
     C                   MOVEL     CACTN#        FMCTN#
     C                   MOVEL     CAUSID        FMUSID
     C                   MOVEL     CAUSF1        FMUSF1
     C                   MOVEL     CAUSF2        FMUSF2
     C                   Z-ADD     CAUSF3        FMUSF3
     C                   ADD       1             RRN01
     C                   ADD       1             IX
     C                   WRITE     OMH01701
     C                   WRITE     OMH01706
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C                   EXSR      SRRD01
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          If end of file then no more records (SFLEND)
      *          ------------------------------------------
     C                   IF        W#EOFI = *ON                                 B01
     C                   EVAL      *IN92 = *OFF
     C                   ELSE                                                   X01
     C                   EVAL      *IN92 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCHK1 - Prepare check                                          *
      *                                                             *
      * This routine prepares the validity check, according to the  *
      * defaults set; in case of no defaults: everything is selected*
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCHK1        BEGSR
      *
      *          ------------------------------------------
      *          Check the fields when needed
      *          ------------------------------------------
     C                   CLEAR                   D#ST1
     C                   CLEAR                   D#ST2
     C                   EVAL      W#VALI = '0'
     C                   IF        P#HEDC = CAHEDC                              B01
     C                   MOVEL     CACTN#        D#ST1
     C                   EVAL      %SUBST(D#ST2:1:10) = F8CTN#
     C                   EXSR      SRCHK2
     C                   IF        W#VALI = '1'                                 B02
     C                   MOVEL     CACUSC        D#ST1
     C                   EVAL      %SUBST(D#ST2:1:10) = F8CUSC
     C                   EXSR      SRCHK2
     C                   ENDIF                                                  E02
     C                   IF        W#VALI = '1'                                 B02
     C                   CLEAR                   D#ST1
     C                   CLEAR                   D#ST2
     C                   MOVEL     CACCTC        D#ST1
     C                   EVAL      %SUBST(D#ST2:1:10) = F8CCTC
     C                   EXSR      SRCHK2
     C                   ENDIF                                                  E02
     C                   IF        W#VALI = '1'                                 B02
     C                   MOVEL     CAUSID        D#ST1
     C                   EVAL      %SUBST(D#ST2:1:10) = F8USID
     C                   EXSR      SRCHK2
     C                   ENDIF                                                  E02
     C                   IF        W#VALI = '1'                                 B02
     C                   MOVEL     CAUSF1        D#ST1
     C                   EVAL      %SUBST(D#ST2:1:10) = F8USF1
     C                   EXSR      SRCHK2
     C                   ENDIF                                                  E02
     C                   IF        W#VALI = '1'                                 B02
     C                   MOVEL     CAUSF2        D#ST1
     C                   EVAL      D#ST2 = F8USF2
     C                   EXSR      SRCHK2
     C                   ENDIF                                                  E02
     C                   IF        W#VALI = '1'                                 B02
     C                   IF        F8USF3 <> C#ALL                              B03
     C                             AND F8USF3 <> C#ALLN
     C                   EVAL      IY = 1
     C                   MOVEA     F8USF3        ST3(1)
     C                   FOR       IY = 1 TO 15                                 B04
     C                   IF        ST3(IY) = ' '                                B05
     C                   EVAL      ST3(IY) = '0'
     C                   ENDIF                                                  E05
     C                   ENDFOR                                                 E04
     C                   MOVEA     ST3(1)        F8USF3
     C                   TESTN                   F8USF3               99
     C                   IF        *IN99 = *OFF                                 B04
     C                   EVAL      W#VALI = '0'
     C                   ELSE                                                   X04
     C                   MOVEL     F8USF3        F8USFN
     C                   IF        CAUSF3 <> F8USFN                             B05
     C                   EVAL      W#VALI = '0'
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   IF        W#VALI = '1'                                 B02
     C                   IF        F8CSRC <> '9'                                B03
     C                             AND F8CSRC <> CACSRC
     C                   EVAL      W#VALI = '0'
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   IF        W#VALI = '1'                                 B02
     C                   IF        CAINDT < D1DAT#                              B03
     C                             OR CAINDT > D2DAT#                           B02
     C                   EVAL      W#VALI = '0'
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   IF        W#VALI = '1'                                 B02
     C                   IF        CAINTM < F8INTM                              B03
     C                             OR CAINTM > F8TMTM
     C                   EVAL      W#VALI = '0'
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Subroutine end
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCHK2 - Validity check                                     *   *
      *                                                             *
      * This routine checks the validity: *ALL, name, generic*      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCHK2        BEGSR
      *
      *          ------------------------------------------
      *          Check
      *          ------------------------------------------
     C                   EVAL      W#VALI = '0'
     C                   IF        D#ST2 = '*ALL'                               B01
     C                   EVAL      W#VALI = '1'
     C                   ELSE                                                   X01
     C     '*'           SCAN      D#ST2         X1                       99
     C                   IF        *IN99 = *OFF                                 B02
     C                   IF        D#ST1 = D#ST2                                B03
     C                   EVAL      W#VALI = '1'
     C                   ENDIF                                                  E03
     C                   ELSE                                                   X02
     C                   FOR       MA_X = X1 TO %ELEM(ST1)
     C                   EVAL      ST1(MA_X) = *BLANKS
     C                   ENDFOR
     C                   FOR       MA_X = X1 TO %ELEM(ST2)
     C                   EVAL      ST2(MA_X) = *BLANKS
     C                   ENDFOR
     C                   IF        D#ST1 = D#ST2                                B03
     C                   EVAL      W#VALI = '1'
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Subroutine end
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRLIST - Control F4                                         *   *
      *                                                             *
      * This routine controls the F4 key                            *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRLIST        BEGSR
      *
      *          ------------------------------------------
      *          List of customers
      *          ------------------------------------------
     C                   IF        FMFLDC = 'F8CUSC'                            B01
     C                   EVAL      P#CUSC = F8CUSC
     C                   CALL      'OMX128'      $MX128                   99
     C                   IF        P#CUSC <> *BLANKS                            B02
     C                   EVAL      F8CUSC = P#CUSC
     C                   EVAL      W#F4I = '1'
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          List of contacts
      *          ------------------------------------------
     C                   IF        FMFLDC = 'F8USID'                            B02
     C                   EVAL      P#USID = *BLANKS
     C                   MOVEL     FMHEDC        P#HEDC
     C                   CALL      'OMX1192'     $MX119                   99
     C                   IF        P#USID <> *BLANKS                            B03
     C                   EVAL      F8USID = P#USID
     C                   EVAL      W#F4I = '1'
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Subroutine end
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF01 - Refresh the screen                                     *
      *                                                             *
      * This routine is called in the screen initialisation, and    *
      * when the user pressed F5 from the first screen              *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      RRN01 = *ZEROS
     C                   IF        *INKE = *ON
     C                   MOVE      *HIVAL        FMPOSI
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Convert selection dates to system format
      *          ------------------------------------------
     C                   IF        F8INDT <> *ZEROS                             B01
     C                   EVAL      P#INDT = F8INDT
     C                   EXSR      SRFUTY
     C                   EVAL      D1DAT# = P#OTDT
     C                   IF        D1DAT# < 400000                              B02
     C                   ADD       1000000       D1DAT#
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
     C                   EVAL      D1DAT# = *ZEROS
     C                   ENDIF                                                  E01
      *
     C                   IF        F8TMDT <> *HIVAL                             B01
     C                   EVAL      P#INDT = F8TMDT
     C                   EXSR      SRFUTY
     C                   EVAL      D2DAT# = P#OTDT
     C                   IF        D2DAT# < 400000                              B02
     C                   ADD       1000000       D2DAT#
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
     C                   Z-ADD     *HIVAL        D2DAT#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the key, reset screenfield
      *          ------------------------------------------
     C                   MOVEL     FMPOSI        K#CTN#
     C                   MOVE      *BLANKS       FMPOSI
      *
      *          ------------------------------------------
      *          Initialize subfiles
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH01702
     C                   WRITE     OMH01707
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Build the subfiles
      *          ------------------------------------------
     C                   EXSR      SRSF01
      *
      *          ------------------------------------------
      *          Fill current subfile relative record number
      *          to display first subfile page
      *          ------------------------------------------
     C                   Z-ADD     1             CRRN01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRU01 - Roll up next screen                                *
      *                                                             *
      * This routine is called if the user presses the roll up key  *
      * in the screen. The next bunch of records is read            *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRU01        BEGSR
      *
      *          ------------------------------------------
      *          Build the subfile
      *          ------------------------------------------
     C                   EXSR      SRSF01
      *
      *          ------------------------------------------
      *          Calculate current record number
      *          ------------------------------------------
     C     W#CRC1        ADD       C#SFL1        CRRN01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED01 - Edit the screen                                    *   *
      *                                                             *
      * This routine checks if the option fields in the subfile     *
      * where correctly filled. A call can be viewed (5), or one    *   *
      * can work with incidents (12)                                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
     C                   EVAL      CRRN01 = *ZEROS
     C                   EVAL      W#SAV# = RRN01
     C                   EVAL      W#VALC = *ZEROS
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   IF        RRN01 > *ZERO                                B01
     C                   IF        *IN43 = *OFF
     C                   READC     OMH01701
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
     C                   IF        *IN43 = *ON
     C                   READC     OMH01706
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Do if option is not blank
      *          ------------------------------------------
     C                   IF        FMOPT1 <> *BLANKS                            B03
      *
      *          ------------------------------------------
      *          Format the subfile option.
      *          ------------------------------------------
     C                   MOVEL     FMOPT1        W#OPTI
     C                   IF        D#OPP2 = *BLANKS                             B04
     C                   EVAL      D#OPP2 = D#OPP1
     C                   EVAL      D#OPP1 = *BLANKS
     C                   MOVEL     W#OPTI        FMOPT1
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If option is ' /' and the user option was not yet
      *          retrieved, then retrieve it. If nothing was returned
      *          then update the subfile and leave.
      *          ------------------------------------------
     C                   IF        FMOPT1 = ' /'                                B04
     C                   Z-ADD     RRN01         CRRN01
     C                   EVAL      EROR01 = *ON
      *
     C                   IF        P#UOPC = *BLANKS                             B05
     C                   MOVE      P#SUL#        UOSUL#
     C                   CALLP     OMX203 ( UOSUL# : P#UOPC )
     C                   EVAL      I#X203 = *ON
     C                   ENDIF                                                  E05
      *
     C                   IF        P#UOPC = *BLANKS                             B05
     C                   EVAL      MSGID = 'OPTE015'
     C                   EXSR      SNDMSG
      *
     C                   EVAL      *IN70 = *ON
     C                   EVAL      *IN93 = *ON
      *
     C                   SELECT                                                 B06
     C                   WHEN      *IN43 = *OFF                                 X06
     C                   UPDATE    OMH01701
     C                   WHEN      *IN43 = *ON                                  X06
     C                   UPDATE    OMH01706
     C                   ENDSL                                                  E06
      *
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
     C                   LEAVE
      *
     C                   ELSE                                                   X05
     C                   MOVE      P#UOPC        FMOPT1
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Check if this is a user option.
      *          The authorisation code may not be '0' (disabled)
      *          and it must be less then or equal to the user auth.
      *          ------------------------------------------
     C                   IF        I#UOL1 <> *ON                                B04
     C                   OPEN      OMHUOL1
     C                   EVAL      I#UOL1 = *ON
     C                   ENDIF                                                  E04
     C     FMOPT1        CHAIN     OMHUOL1
     C                   EVAL      *IN98 = NOT %FOUND
     C                   IF        *IN98 = *OFF
     C                   EVAL      *IN98 = (UOSUL# = ' ')                        Equal
     C                   ENDIF
     C                   IF        *IN98 = *OFF
     C                   EVAL      *IN98 = (UOSUL# > P#SUL#)                     Higher
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If option is valid, then check the number of valid
      *          codes. *IN93 is the subfile next change indicator
      *          ------------------------------------------
     C                   IF        FMOPT1 = ' 5'                                B04
     C                             OR FMOPT1 = '12'
     C                             OR *IN98 = *OFF
      *
      *          ------------------------------------------
      *          Option is valid
      *          ------------------------------------------
     C     $CA2K2        SETLL     OMCCAL2
     C                   EVAL      *IN99 = %EQUAL
      *
      *          ------------------------------------------
      *          Record no longer active
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF                                 B05
     C                   EVAL      MSGID = 'FILE002'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X05
     C                   ADD       1             W#VALC
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          Option not valid
      *          ------------------------------------------
     C                   ELSE                                                   X04
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EVAL      MSGDTA = VAR(1)
     C                   EVAL      MSGID = 'OPTE001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If an error was found, then position on the page of
      *          the first error
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B04
     C                             AND *IN70 = *ON
     C                   Z-ADD     RRN01         CRRN01
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Update subfile
      *          ------------------------------------------
     C                   EVAL      *IN93 = *ON
     C                   IF        *IN43 = *OFF
     C                   UPDATE    OMH01701
     C                   ENDIF
     C                   IF        *IN43 = *ON
     C                   UPDATE    OMH01706
     C                   ENDIF
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   IF        *IN43 = *OFF
     C                   UPDATE    OMH01701
     C                   ENDIF
     C                   IF        *IN43 = *ON
     C                   UPDATE    OMH01706
     C                   ENDIF
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   IF        *IN43 = *OFF
     C                   READC     OMH01701
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
     C                   IF        *IN43 = *ON
     C                   READC     OMH01706
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If subfile must be positioned, and no errors occured
      *          then position it
      *          ------------------------------------------
     C                   EVAL      RRN01 = W#SAV#
     C                   IF        CRRN01 = *ZEROS                              B01
     C                             AND FMPOSI <> *BLANKS
      *
      *          ------------------------------------------
      *          Fill the correct key and call the refresh routine
      *          ------------------------------------------
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
      *
      *          ------------------------------------------
      *          Fill the current relative record on the page to
      *          display
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   IF        CRRN01 = *ZEROS                              B02
     C                   Z-ADD     W#CRC1        CRRN01
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
      * SRPR01 - Process display                                    *   *
      *                                                             *
      * This routine processes the first display                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      S#RR01 = RRN01
      *
      *          ------------------------------------------
      *          Read subfile next changed record
      *          If records in subfile and valid codes found
      *          ------------------------------------------
     C                   IF        RRN01 > *ZEROS                               B01
     C                             AND W#VALC > *ZEROS
     C                   IF        *IN43 = *OFF
     C                   READC     OMH01701
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
     C                   IF        *IN43 = *ON
     C                   READC     OMH01706
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Read subfile until valid option found
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          If options typed then reloop.
      *          ------------------------------------------
     C                   EVAL      LOOP01 = *ON
     C                   EVAL      W#CRC1 = RRN01
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPT1        W#OPT1
     C                   MOVE      *BLANKS       FMOPT1
     C                   IF        *IN43 = *OFF
     C                   UPDATE    OMH01701
     C                   ENDIF
     C                   IF        *IN43 = *ON
     C                   UPDATE    OMH01706
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Display incident
      *          ------------------------------------------
     C                   IF        W#OPT1 = ' 5'                                B03
     C                   EVAL      P#STAT = '*NORM'
     C                   EVAL      P#ACTC = '50'
     C                   MOVE      FMCTN#        P#CTN#
     C                   CALLP     OMH015 ( P#ACTC : P#HEDC : P#CTN# :
     C                             P#CUSC : P#CCTC : P#STAT )
      *
     C                   SELECT
     C                   WHEN      P#STAT = '*EXIT'
     C                   EXSR      SRTTLR
     C                   ENDSL
      *
     C                   IF        P#STAT = '*CNCL'                             B04
     C                   LEAVE
     C                   ENDIF                                                  E04
      *
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Action if option is 12
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      W#OPT1 = '12'
     C                   EXSR      SRCONT
     C                   WHEN      W#OPT1 <> *BLANKS
     C                   EXSR      USROPT
     C                   ENDSL
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Error option field.
      *          ------------------------------------------
     C                   IF        I#OPTI = *ON                                 B03
     C                   IF        *IN43 = *OFF
     C     RRN01         CHAIN     OMH01701
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF
     C                   IF        *IN43 = *ON
     C     RRN01         CHAIN     OMH01706
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF
     C                   MOVE      W#OPT1        FMOPT1
     C                   EVAL      *INKL = *ON
     C                   EVAL      *IN70 = *ON
     C                   EVAL      *IN93 = *ON
     C                   IF        *IN43 = *OFF
     C                   UPDATE    OMH01701
     C                   ENDIF
     C                   IF        *IN43 = *ON
     C                   UPDATE    OMH01706
     C                   ENDIF
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next changed if no end requested
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                   LEAVE
     C                   ELSE                                                   X03
     C                   IF        *IN43 = *OFF
     C                   READC     OMH01701
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
     C                   IF        *IN43 = *ON
     C                   READC     OMH01706
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
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
     C                   Z-ADD     W#CRC1        CRRN01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Reset record pointers
      *          ------------------------------------------
     C                   EVAL      RRN01 = S#RR01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC03 - Screen 3, Subset                                   *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
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
     C                   DOU       LOOP03 = '0'                                 B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR03 = '0'                                 B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the screen
      *          ------------------------------------------
     C                   EXFMT     OMH01708
      *
      *          ------------------------------------------
      *          Quit if required.
      *          ------------------------------------------
     C                   IF        *INKC = *ON
     C                   EVAL      P#STAT = '*EXIT'
     C                   EXSR      SRTTLR
     C                   ENDIF
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
     C                   EVAL      *IN(78) = *OFF
     C                   EVAL      *IN(79) = *OFF
     C                   EVAL      *IN(61) = *OFF
     C                   EVAL      *IN(62) = *OFF
     C                   EVAL      *IN(63) = *OFF
     C                   EVAL      *IN(64) = *OFF
     C                   EVAL      *IN(65) = *OFF
     C                   EVAL      *IN(66) = *OFF
     C                   EVAL      EROR03 = '0'
     C                   EVAL      LOOP03 = '0'
      *
      *          ------------------------------------------
      *          Fill change indicator pending on saved indicator
      *          ------------------------------------------
     C                   IF        I#IN40 = *ON                                 B03
     C                   EVAL      *IN40 = *ON
     C                   EVAL      I#IN40 = *OFF
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          F4=Prompt
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   EXSR      SRLIST
     C                   EVAL      EROR03 = '1'
     C                   EVAL      I#IN40 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F5=Refresh
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B04
     C                             OR F8CTN# = *BLANKS
     C                             OR F8CUSC = *BLANKS
     C                             OR F8CCTC = *BLANKS
     C                             OR F8USID = *BLANKS
     C                             OR F8CSRC = *BLANKS
     C                             OR F8USF1 = *BLANKS
     C                             OR F8USF2 = *BLANKS
     C                             OR F8USF3 = *ZEROS
     C                   EXSR      SRRF03
     C                   EVAL      EROR03 = '1'
     C                   EVAL      I#IN40 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          F9=Retrieve subset.
      *          ------------------------------------------
     C                   IF        *INKI = *ON                                  B05
     C                   EVAL      %SUBST(TVSDTA:1:115) = D#FMT1
     C                   EXSR      SRRTV
     C                   EVAL      D#FMT1 = TVSDTA
     C                   EVAL      EROR03 = *ON
     C                   EVAL      I#IN40 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          F14=Restore defaults
      *          ------------------------------------------
     C                   IF        *INKN = *ON                                  B06
     C                   EXSR      SRRF03
     C                   EVAL      EROR03 = *ON
     C                   EVAL      I#IN40 = *ON
     C                   ELSE                                                   X06
      *
      *          ------------------------------------------
      *          Do edit routine if not F3 or F12.
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B07
     C                   EXSR      SRED03
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
      *          Do processing if not F3 or F12
      *          And fields where changed (*IN40 = *ON)
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B02
     C                             AND *IN40 = *ON
     C                   EXSR      SRPR03
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
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIN03 - Init                                               *
      *                                                             *
      * This routine is here for compatibliliy purposes only.       *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN03        BEGSR
      *
      *          ------------------------------------------
      *          Init
      *          ------------------------------------------
     C                   MOVE      *OFF          FMDEFI
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF03 - Subset refresh                                     *
      *                                                             *
      * This screen resets the selection criteria by placing the    *
      * initial values in the fields.                               *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF03        BEGSR
      *
      *          ------------------------------------------
      *          Initialize
      *          ------------------------------------------
     C                   EVAL      %SUBST(K#FMTC:1:8) = 'OMH01708'
     C                   EVAL      P#UOPC = *BLANKS
      *
      *          ------------------------------------------
      *          Refresh
      *          ------------------------------------------
     C                   EVAL      F8CTN# = C#ALL
     C                   EVAL      F8CUSC = C#ALL
     C                   EVAL      F8CCTC = C#ALL
     C                   EVAL      F8INDT = 0
     C                   EVAL      F8TMDT = 999999
     C                   EVAL      F8INTM = 0
     C                   EVAL      F8TMTM = 999999
     C                   EVAL      F8USID = C#ALL
     C                   EVAL      F8USF1 = C#ALL
     C                   EVAL      %SUBST(F8USF2:1:10) = C#ALL
     C                   EVAL      %SUBST(F8USF3:1:10) = C#ALL
     C                   EVAL      F8CSRC = '9'
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
      * SRED03 - Subset edit                                        *
      *                                                             *
      * This routine checks if the from value does not exceed the   *
      * to value, if it does, then a message is sent, and the field *
      * contents are swapped.                                       *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED03        BEGSR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR03 - Subset process                                     *
      *                                                             *
      * This routine calls the first screen refresh routine to      *
      * rebuild the first subfile                                   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR03        BEGSR
      *
      *          ------------------------------------------
      *          Fill save field with subset data.
      *          ------------------------------------------
     C                   EVAL      S#SDTA = *BLANKS
     C                   EVAL      %SUBST(S#SDTA:1:115) = D#FMT1
      *
      *          ------------------------------------------
      *          If this must be saved as defaults then save
      *          as defaults and save selections.
      *          ------------------------------------------
     C                   IF        FMDEFI = *ON                                 B01
      *
      *          ------------------------------------------
      *          Save as the default values.
      *          ------------------------------------------
     C                   EVAL      K#RTV# = *ZEROS
     C     $RTVK2        CHAIN     OMHTVL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON
     C                   MOVE      FMHEDC        TVHEDC
     C                   EVAL      TVUSID = D#USID
     C                   EVAL      TVFMTC = K#FMTC
     C                   EVAL      TVRTV# = *ZEROS
     C                   ENDIF
     C                   EVAL      TVSDTA = S#SDTA
     C                   IF        *IN99 = *ON
     C                   WRITE     OMHTVR
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMHTVR
     C                   ENDIF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Save selections.
      *          ------------------------------------------
     C                   EXSR      SRRTV
      *
      *          ------------------------------------------
      *          Refresh the subfile
      *          ------------------------------------------
     C                   EVAL      *INKE = *ON
     C                   EXSR      SRRF01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRTV   -  Retrieve selections.                              *
      *                                                              *
      * Input parameters. K#FMTC - Current screen format.            *
      *                   S#SDTA - Screen data.                      *
      *                                                              *
      * This routine is the retrieve selection server. If the        *
      * retrieve key (F9) is *ON, then the data is retrieved from    *
      * the next or the first seqeunce pending if F9 was used before *
      * on this screen.                                              *
      * If no selections where found or the last selection was       *
      * already retrieved then a message is send.                    *
      *                                                              *
      * If F9 is *OFF, then this routine is called from a process    *
      * routine when the program knows that the selections where     *
      * accepted. These selections are written in the highest        *
      * available sequence number. If sequence number 1 to 10 where  *
      * filled, then 10 is shifted out by replacing 2 for 1, 3 for   *
      * 2 etc..                                                      *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRTV         BEGSR
      *
      *          ------------------------------------------
      *          If new screen, then start from last stored selection.
      *          ------------------------------------------
     C                   IF        K#FMTC <> TVFMTC                             B01
     C                   EVAL      TVFMTC = K#FMTC
     C                   EVAL      K#RTV# = *ZEROS
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Retrieve values if F9 is pressed.
      *          ------------------------------------------
     C                   IF        *INKI = *ON                                  B01
      *
      *          ------------------------------------------
      *          If no selections for this format then send msg.
      *          ------------------------------------------
     C     $RTVK1        SETLL     OMHTVL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B02
     C                   EVAL      MSGID = 'RTV0002'
     C                   EXSR      SNDMSG
      *
      *          ------------------------------------------
      *          If allready last retrieved then send msg.
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   ADD       1             K#RTV#
     C                   IF        K#RTV# > 10                                  B03
     C                   EVAL      K#RTV# = *ZEROS
     C                   EVAL      MSGID = 'RTV0001'
     C                   EXSR      SNDMSG
      *
      *          ------------------------------------------
      *          Read the next selections if none found then error.
      *          else the sequence number read now is used for next F9.
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
      *          The screen was accepted, save this selection.
      *          Check highest free number.
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
      *          If no record found then this is the first.
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
     C                   FOR       DO_X = 1 TO 9                                B04
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
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCONT - Incidents maintenance                              *
      *                                                             *
      * This routine calls the incidents maintenance program        *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCONT        BEGSR
      *
      *          ------------------------------------------
      *          Call the program
      *          ------------------------------------------
     C                   MOVEL     FMHEDC        P#HEDC
     C                   MOVEL     FMCTN#        P#CTN#
     C                   EVAL      P#CCTC = *BLANKS
     C                   EVAL      P#CUSC = *BLANKS
     C                   CALLP     OMH018 ( P#HEDC : P#CTN# : P#CUSC :
     C                             P#CCTC : P#STAT )
     C                   SELECT
     C                   WHEN      P#STAT = '*EXIT'
     C                   EXSR      SRTTLR
     C                   ENDSL
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * USROPT - User option processing                              *
      *                                                              *
      * This routine handles possible user options.                  *
      *                                                              *
      * Input  - W#OPT1 - User option                                *
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
     C     W#OPT1        CHAIN     OMHUOL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *                    --------------------------------
      *                    User option not found.
      *                    --------------------------------
     C                   IF        *IN99 = *ON                                  B01
     C                   MOVEL     W#OPT1        MSGDTA
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
     C                   MOVEL     W#OPT1        MSGDTA
     C                   EVAL      MSGID = 'OPTE018'
     C                   EXSR      SNDMSG
      *
     C                   MOVE      D#MSID        MSGID
     C                   MOVEL     D#MSGD        MSGDTA
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( MSGID : MSGF : MSGLIB : MSGDTA
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
     C                   MOVEA     W#OPT1        MC(21)
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
     C                   EVAL      P#ENTC = 'CCA'
     C                   MOVE      FMHEDC        D#HEDC
     C                   MOVE      FMCTN#        D2CTN#
     C                   MOVE      W#OPT1        P#OPT1
      *
     C                   IF        UOINTL = '*V1R1M0'                           B05
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP(E)  P#OPGM_002 ( P#ENTC : D#CCA : P#OPT1 :
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
     C                   MOVEL     W#OPT1        MSGDTA
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
      * ----------------------------------------------------------- *
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
      *          Keyfield definition
      *          ------------------------------------------
     C                   EVAL      K#CTN# = *BLANKS
     C                   EVAL      K#HEDC = *BLANKS
     C                   MOVE      *BLANKS       FMPOSI
     C                   MOVEL     P#HEDC        FMHEDC
      *
      *          ------------------------------------------
      *          Savefield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      W#VALC = *ZERO
      *
      *          ------------------------------------------
      *          Keylist for OMCCOL1
      *          ------------------------------------------
     C     $CO1K1        KLIST
     C                   KFLD                    FMCUSC
     C                   KFLD                    FMCCTC
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
     C                   KFLD                    FMHEDC
     C                   KFLD                    D#USID
     C                   KFLD                    K#FMTC
     C                   KFLD                    K#RTV#
      *
      *          ------------------------------------------
      *          Keylist for OMCCA
      *          ------------------------------------------
     C     $CA2K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#CTN#
      *
     C     $CA2K2        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMCTN#
      *
      *          ------------------------------------------
      *          Keylist for OMINCJ5
      *          ------------------------------------------
     C     $NJ5K1        KLIST
     C                   KFLD                    CAHEDC
     C                   KFLD                    CACTN#
      *
      *          ------------------------------------------
      *          Parameter list for OMX1192 (get user)
      *          ------------------------------------------
     C     $MX119        PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    P#USID
     C                   PARM                    P#USDC
      *
      *          ------------------------------------------
      *          Parameter list for OMX128 (get customer)
      *          ------------------------------------------
     C     $MX128        PLIST
     C                   PARM                    P#CUSC
     C                   PARM                    P#NAMD
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
     C                   MOVEL     P#HEDD        FMHEDD
      *
      *          ------------------------------------------
      *          Get message data
      *          ------------------------------------------
     C                   EXSR      SRGVAR
      *
      *          ------------------------------------------
      *          Initialize some fields.
      *          ------------------------------------------
     C                   MOVE      *HIVAL        K#CTN#
     C                   MOVE      *HIVAL        FMPOSI
     C                   EVAL      K#HEDC = P#HEDC
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
      *          Close OMX203 if called
      *          ------------------------------------------
     C                   IF        I#X203 = *ON                                 B01
     C                   CALLP     OMX203_001()
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Seton last record
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
     C                   RETURN
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
      * retrieve the data wich is filled in message texts.           *
      * It is called one time per program execution. If the system   *
      * choose to do so, it can be paged out. Thats why it is here.  *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGVAR        BEGSR
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 7                                  B01
     C                   EVAL      MSGID = MSD(MS)
      *
      *          ------------------------------------------
      *          Call subprogram
      *          ------------------------------------------
     C                   IF        MSGID <> *BLANKS                             B02
     C                   EVAL      MSGF = 'OMHMSG'
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
     C                   WHEN      E#FILE = 'OMH017D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMCCAL2 '                          X02
     C                   MOVEL     CCAL2         P#FILE
      *
     C                   WHEN      E#FILE = 'OMCCOL1 '                          X02
     C                   MOVEL     CCOL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHTVL1 '                          X02
     C                   MOVEL     HTVL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHUOL1 '                          X02
     C                   MOVEL     HUOL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMINCJ5 '                          X02
     C                   MOVEL     INCJ5         P#FILE
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
**
OPT0018  -  '5' or '12'                       1
