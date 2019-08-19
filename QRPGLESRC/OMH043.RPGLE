     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH043                                        *
      * Function ...: Incident request processor                    *
      * Module .....: Helpdesk                                      *
      * Author .....: Remain Software                               *
      * Parameters .: *NONE                                         *
      *                                                             *
      * This program is the incident and mail monitor.              *
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
     FOMSRFL1   IF   E           K DISK    INFDS(SRFL1)
     F                                     INFSR(*PSSR)
     FOMSAFL1   IF   E           K DISK    INFDS(SAFL1)
     F                                     INFSR(*PSSR)
     FOMMHXL1   IF   E           K DISK    INFDS(MHXL1)
     F                                     INFSR(*PSSR)
     FOMMCXL1   IF   E           K DISK    INFDS(MCXL1)
     F                                     INFSR(*PSSR)
     FOMCCOL1   IF   E           K DISK    INFDS(CCOL1)
     F                                     INFSR(*PSSR)
     FOMMSX     IF   E             DISK    INFDS(MSX)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMINCL1   IF   E           K DISK    INFDS(INCL1)
     F                                     INFSR(*PSSR)
     FOMCCAL1   IF   E           K DISK    INFDS(CCAL1)
     F                                     INFSR(*PSSR)
     FOMRCFL2   IF   E           K DISK    INFDS(RCFL2)
     F                                     INFSR(*PSSR)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension spec's                                             *
      *                                                              *
      * MC    Used to fill message variables in message data parm.   *
      * ------------------------------------------------------------ *
     D MC              S              1    DIM(256)
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
     D SAFL1           DS            57
     D SRFL1           DS            57
     D MHXL1           DS            57
     D MCXL1           DS            57
     D CCOL1           DS            57
     D INCL1           DS            57
     D MSX             DS            57
     D CCAL1           DS            57
     D RCFL2           DS            57
      *
      *          ------------------------------------------
      *          Helpdesk data structures.
      *          ------------------------------------------
      /COPY QCPYLESRC,OMHLPDS
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
      *          Incident API data queue buffer
      *          ------------------------------------------
      /COPY QCPYLESRC,OMH044IN
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#NORM          C                   CONST('*NORM')
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
      *          Parameter list for OMH045 (Get contact mail template)
      * Prototype for 'OMH037_3'
     D OMH037_3        PR                  EXTPGM('OMH037_3')
     D P#DSID_                             LIKE(P#DSID)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH037C'
     D OMH037C         PR                  EXTPGM('OMH037C')
     D P#HUID_                             LIKE(P#HUID)
     D P#HADR_                             LIKE(P#HADR)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH043C (Send mail)
      * Prototype for W#OBJQ
     D W#OBJQ_001      PR                  EXTPGM(W#OBJQ)
     D P#HEDC_                             LIKE(P#HEDC)
     D P#INC#_                             LIKE(P#INC#)
     D P#CTN#_                             LIKE(P#CTN#)
     D P#EOEI_                             LIKE(P#EOEI)
     D P#STAO_                             LIKE(P#STAO)
     D P#STRO_                             LIKE(P#STRO)
     D P#SUL#_                             LIKE(P#SUL#)
     D P#STAN_                             LIKE(P#STAN)
     D P#STRN_                             LIKE(P#STRN)
     D SUL#_                               LIKE(SUL#)
     D P#ACTC_                             LIKE(P#ACTC)
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
      *          Parameter list for OMH037C (Get mail)
      * Prototype for 'OMH043C'
     D OMH043C         PR                  EXTPGM('OMH043C')
     D P#HEDC_                             LIKE(P#HEDC)
     D P#INC#_                             LIKE(P#INC#)
     D P#EXTI_                             LIKE(P#EXTI)
     D P#MSRV_                             LIKE(P#MSRV)
     D P#PORT_                             LIKE(P#PORT)
     D P#EMAC_                             LIKE(P#EMAC)
     D P#EEMC_                             LIKE(P#EEMC)
     D P#BCCE_                             LIKE(P#BCCE)
     D P#SUBJ_                             LIKE(P#SUBJ)
     D P#MATF_                             LIKE(P#MATF)
     D P#MATL_                             LIKE(P#MATL)
     D P#MATM_                             LIKE(P#MATM)
     D P#SUL#_                             LIKE(P#SUL#)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH044C (Get next data queue entry)
      * Prototype for 'OMH045'
     D OMH045          PR                  EXTPGM('OMH045')
     D P#HEDC_                             LIKE(P#HEDC)
     D P#CUSC_                             LIKE(P#CUSC)
     D P#CCTC_                             LIKE(P#CCTC)
     D P#ACTC_                             LIKE(P#ACTC)
     D P#MATL_                             LIKE(P#MATL)
     D P#MATF_                             LIKE(P#MATF)
     D P#MATM_                             LIKE(P#MATM)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          OMINCLn key list Incident File
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
      * Prototype for 'OMH037_3'
     D OMH037_3_001    PR                  EXTPGM('OMH037_3')
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D I#PSSR          S              1
     D K#ACTC          S              1
     D K#CCTC          S                   LIKE(CCTC)
     D K#CTN#          S                   LIKE(CTN#)
     D K#CUSC          S                   LIKE(CUSC)
     D K#EOEI          S                   LIKE(EOEI)
     D K#HEDC          S                   LIKE(HEDC)
     D K#INC#          S                   LIKE(INC#)
     D K#STAT          S                   LIKE(STAT)
     D K#STRC          S                   LIKE(STRC)
     D P#ACTC          S                   LIKE(ACTC)
     D P#HADR          S                   LIKE(HADR)
     D P#BCCE          S                   LIKE(EEMC)
     D P#CCTC          S                   LIKE(CCTC)
     D P#CTN#          S                   LIKE(CTN#)
     D P#CUSC          S                   LIKE(CUSC)
     D P#DSID          S                   LIKE(DSID)
     D P#EEMC          S                   LIKE(EEMC)
     D P#EMAC          S                   LIKE(EMAC)
     D P#EOEI          S                   LIKE(EOEI)
     D P#EXTI          S                   LIKE(EXTI)
     D P#FILE          S             57
     D P#HEDC          S                   LIKE(HEDC)
     D P#INC#          S                   LIKE(INC#)
     D P#LEN           S              5  0
     D P#MATF          S                   LIKE(MATF)
     D P#MATL          S                   LIKE(MATL)
     D P#MATM          S                   LIKE(MATM)
     D P#MSGD          S            256
     D P#MSGF          S                   LIKE(MSGF)
     D P#MSID          S                   LIKE(MSID)
     D P#MSRV          S             15
     D P#MSTA          S             40
     D P#OBJC          S                   LIKE(OBJC)
     D P#OBJL          S                   LIKE(OBJL)
     D P#OPTI          S                   LIKE(INDI)
     D P#PORT          S                   LIKE(PORT)
     D P#STAN          S                   LIKE(STAT)
     D P#STAO          S                   LIKE(STAT)
     D P#STAT          S                   LIKE(STAT)
     D P#STRN          S                   LIKE(STRC)
     D P#STRO          S                   LIKE(STRC)
     D P#SUBJ          S                   LIKE(SUBJ)
     D P#SUL#          S                   LIKE(SUL#)
     D P#HUID          S                   LIKE(HUID)
     D P#WAIT          S              5  0
     D PNAME           S             10
     D PVAR            S             50
     D SVSTRC          S                   LIKE(STRC)
     D W#OBJQ          S             21
     D WASTAT          S                   LIKE(STAT)
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
      *          Do the processing routine if all is clear.
      *          ------------------------------------------
     C                   EXSR      SRVERW
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
      * First check if there are any incidents waiting to be        *
      * processed. If so, then process them. If not, then check     *
      * for incoming mail. If no mail then wait for incoming        *
      * incidents.                                                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Get the first data queue message
      *          ------------------------------------------
     C                   EVAL      P#WAIT = *ZEROS
     C                   EVAL      P#OBJC = 'OMH044C'
     C                   EVAL      P#OBJL = '*LIBL'
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH044C ( P#OBJC : P#OBJL : P#OPTI :
     C                             D#44IN : P#LEN : P#WAIT : P#STAT )
      *
      *          ------------------------------------------
      *          Enter the never ending loop
      *          ------------------------------------------
     C                   DOW       1 = 1                                        B01
      *
      *          ------------------------------------------
      *          Exit if requested
      *          ------------------------------------------
     C                   IF        I44ENT = 'XX'                                B02
     C                             AND I44ACT = '99'
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Get the system mail extensions
      *          ------------------------------------------
     C                   OPEN      OMMSX
     C                   READ      OMMSX
     C                   EVAL      *IN99 = %EOF
     C                   CLOSE     OMMSX
     C                   IF        *IN99 = *ON
     C                   Z-ADD     5             SXMPT#
     C                   ENDIF
     C                   DOW       P#LEN <> *ZEROS                              B02
      *
      *                    --------------------------------
      *                    Get the incident
      *                    --------------------------------
     C                   MOVE      I44HED        K#HEDC
     C                   MOVE      I44INC        K#INC#
     C     $NC1K1        CHAIN     OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF
     C                   MOVE      NCCTN#        K#CTN#
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C     $CA1K1        CHAIN     OMCCAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF
     C                   IF        *IN99 = *OFF                                 B03
      *
      *                    --------------------------------
      *                    Do status routes
      *                    --------------------------------
     C                   EXSR      SRSRPG
     C                   EXSR      SRSTPG
      *
      *                    --------------------------------
      *                    Send it
      *                    --------------------------------
     C                   EXSR      SRSEND
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Check for shutdown
      *          ------------------------------------------
     C                   EXSR      SRSHUT
      *
      *          ------------------------------------------
      *          Get the next entry
      *          ------------------------------------------
     C                   EVAL      P#WAIT = *ZEROS
     C                   EVAL      P#OBJC = 'OMH044C'
     C                   EVAL      P#OBJL = '*LIBL'
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH044C ( P#OBJC : P#OBJL : P#OPTI :
     C                             D#44IN : P#LEN : P#WAIT : P#STAT )
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Check for shutdown
      *          ------------------------------------------
     C                   EXSR      SRSHUT
      *
      *          ------------------------------------------
      *          Check for distributions with the *PROC status.
      *          ------------------------------------------
     C                   EVAL      P#DSID = *BLANKS
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH037_3 ( P#DSID : P#STAT )
      *
      *          ------------------------------------------
      *          Check for shutdown
      *          ------------------------------------------
     C                   EXSR      SRSHUT
      *
      *          ------------------------------------------
      *          No more incidents, get mail from all desks.
      *          ------------------------------------------
     C     *LOVAL        SETLL     OMMHXL1
     C                   READ      OMMHXL1
     C                   EVAL      *IN99 = %EOF
      *
      *                    --------------------------------
      *                    Process all extensions.
      *                    --------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *                    --------------------------------
      *                    If a user id was specified.
      *                    --------------------------------
     C                   IF        HXHUID <> *BLANKS                            B03
     C                             AND HXHADR <> *BLANKS
     C                             AND HXEMAC <> *BLANKS
      *
      *                    --------------------------------
      *                    Get the mail
      *                    --------------------------------
     C                   MOVE      HXHUID        P#HUID
     C                   MOVE      HXHADR        P#HADR
     C                   CALLP     OMH037C ( P#HUID : P#HADR : P#STAT )
     C                   ENDIF                                                  E03
      *
      *                    --------------------------------
      *                    Read next mail extension
      *                    --------------------------------
     C                   READ      OMMHXL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Check for shutdown
      *          ------------------------------------------
     C                   EXSR      SRSHUT
      *
      *          ------------------------------------------
      *          Wait ? seconds for more messages
      *          ------------------------------------------
     C     SXMPT#        MULT      60            P#WAIT
     C                   EVAL      P#OBJC = 'OMH044C'
     C                   EVAL      P#OBJL = '*LIBL'
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH044C ( P#OBJC : P#OBJL : P#OPTI :
     C                             D#44IN : P#LEN : P#WAIT : P#STAT )
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSRPG - Status route program handling                      *
      *                                                             *
      * This routine is called after the incident status was        *
      * set or changed. The old incident status is in S#STAT, the   *
      * new status is in NCSTAT.                                    *
      *                                                             *
      * If S#STAT is blanks then the exit entries are not executed, *
      * else the exit entry programs are called. After these are    *
      * done the entry entries are called to deal with the new      *
      * status.                                                     *
      *                                                             *
      * In NCSTAT is blanks then no entry entries are called.       *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSRPG        BEGSR
      *
      *          ------------------------------------------
      *          Do nothing if no changes.
      *          ------------------------------------------
     C                   IF        I44NSR <> I44OSR                             B01
      *
      *          -----------------------------------------------
      *          If there was an old status ROUTE
      *          -----------------------------------------------
     C                   IF        I44OSR <> *BLANKS                            B02
     C                   MOVE      I44HED        K#HEDC
     C                   MOVE      I44OSR        K#STRC
     C                   EVAL      K#EOEI = '1'
      *
      *          -----------------------------------------------
      *          Read all exit programs and call the program.
      *          -----------------------------------------------
     C     $AF1K2        CHAIN     OMSRFL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B03
     C                   MOVEL(P)  RFSEPL        W#OBJQ
     C                   EVAL      W#OBJQ = %TRIMR(W#OBJQ) + '/'
     C     W#OBJQ        CAT       RFSEPG:0      W#OBJQ
      *
      *                    --------------------------------
      *                    Interface V1R1M0
      *                    --------------------------------
     C                   IF        RFINTL = '*V1R1M0'                           B04
     C                   EVAL      P#HEDC = NCHEDC
     C                   EVAL      P#INC# = NCINC#
     C                   EVAL      P#CTN# = NCCTN#
     C                   EVAL      P#EOEI = '1'
     C                   EVAL      P#STAO = I44OST
     C                   EVAL      P#STRO = I44OSR
     C                   EVAL      P#SUL# = I44OSL
     C                   EVAL      P#STAN = I44NST
     C                   EVAL      P#STRN = I44OST
     C                   EVAL      SUL# = I44NSL
     C                   EVAL      P#ACTC = I44ACT
     C                   CALLP(E)  W#OBJQ_001 ( P#HEDC : P#INC# : P#CTN# :
     C                             P#EOEI : P#STAO : P#STRO : P#SUL# :
     C                             P#STAN : P#STRN : SUL# : P#ACTC )
     C                   EVAL      *IN99 = %ERROR
      *
      *                    --------------------------------
      *                    Write a log entry.
      *                    --------------------------------
     C                   MOVEA     RFSEPG        MC(1)
     C                   MOVEA     RFSEPL        MC(11)
     C                   MOVEA     NCINC#        MC(21)
     C                   MOVEA     NCHEDC        MC(31)
     C                   MOVEA     I44OST        MC(36)
     C                   MOVEA     I44OSR        MC(41)
     C                   MOVEA     I44OSL        MC(51)
     C                   MOVEA     I44NST        MC(52)
     C                   MOVEA     I44NSR        MC(57)
     C                   MOVEA     I44NSL        MC(67)
     C                   MOVEA     MC            P#MSGD
     C                   EVAL      P#HEDC = NCHEDC
     C                   EVAL      P#INC# = NCINC#
     C                   EVAL      P#ACTC = I44ACT
     C                   EVAL      P#MSID = 'OMQA01E'
     C                   EVAL      P#MSGF = 'OMHAPI'
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMX929 ( P#HEDC : P#INC# : P#ACTC :
     C                             P#MSID : P#MSGD : P#MSGF : P#STAT )
     C                   ENDIF                                                  E04
      *
     C     $AF1K2        READE     OMSRFL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E03
     C                   ENDIF                                                  E02
      *
      *          -----------------------------------------------
      *          If there is a new status ROUTE
      *          -----------------------------------------------
     C                   IF        NCSTRC <> *BLANKS                            B02
     C                   MOVE      NCHEDC        K#HEDC
     C                   MOVE      NCSTRC        K#STRC
     C                   EVAL      K#EOEI = '0'
      *
      *          ------------------------------------------
      *          Read all entry programs and call the program.
      *          ------------------------------------------
     C     $AF1K2        CHAIN     OMSRFL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B03
     C                   MOVEL(P)  RFSEPL        W#OBJQ
     C                   EVAL      W#OBJQ = %TRIMR(W#OBJQ) + '/'
     C     W#OBJQ        CAT       RFSEPG:0      W#OBJQ
      *
      *                    --------------------------------
      *                    Interface V1R1M0
      *                    --------------------------------
     C                   IF        RFINTL = '*V1R1M0'                           B04
     C                   EVAL      P#HEDC = NCHEDC
     C                   EVAL      P#INC# = NCINC#
     C                   EVAL      P#CTN# = NCCTN#
     C                   EVAL      P#EOEI = '1'
     C                   EVAL      P#STAO = I44OST
     C                   EVAL      P#STRO = I44OSR
     C                   EVAL      P#SUL# = I44OSL
     C                   EVAL      P#STAN = I44NST
     C                   EVAL      P#STRN = I44OST
     C                   EVAL      SUL# = I44NSL
     C                   EVAL      P#ACTC = I44ACT
     C                   CALLP(E)  W#OBJQ_001 ( P#HEDC : P#INC# : P#CTN# :
     C                             P#EOEI : P#STAO : P#STRO : P#SUL# :
     C                             P#STAN : P#STRN : SUL# : P#ACTC )
     C                   EVAL      *IN99 = %ERROR
      *
      *                    --------------------------------
      *                    Write a log entry.
      *                    --------------------------------
     C                   MOVEA     RFSEPG        MC(1)
     C                   MOVEA     RFSEPL        MC(11)
     C                   MOVEA     NCINC#        MC(21)
     C                   MOVEA     NCHEDC        MC(31)
     C                   MOVEA     I44OST        MC(36)
     C                   MOVEA     I44OSR        MC(41)
     C                   MOVEA     I44OSL        MC(51)
     C                   MOVEA     I44NST        MC(52)
     C                   MOVEA     I44NSR        MC(57)
     C                   MOVEA     I44NSL        MC(67)
     C                   MOVEA     MC            P#MSGD
     C                   EVAL      P#HEDC = NCHEDC
     C                   EVAL      P#INC# = NCINC#
     C                   EVAL      P#ACTC = I44ACT
     C                   EVAL      P#MSID = 'OMQA01E'
     C                   EVAL      P#MSGF = 'OMHAPI'
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMX929 ( P#HEDC : P#INC# : P#ACTC :
     C                             P#MSID : P#MSGD : P#MSGF : P#STAT )
     C                   ENDIF                                                  E04
      *
     C     $AF1K2        READE     OMSRFL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSTPG - Status program handling                            *
      *                                                             *
      * This routine is called after the incident status was        *
      * set or changed. The old incident status is in S#STAT, the   *
      * new status is in NCSTAT.                                    *
      *                                                             *
      * If S#STAT is blanks then the exit entries are not executed, *
      * else the exit entry programs are called. After these are    *
      * done the entry entries are called to deal with the new      *
      * status.                                                     *
      *                                                             *
      * In NCSTAT is blanks then no entry entries are called.       *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSTPG        BEGSR
      *
      *          ------------------------------------------
      *          Do nothing if no changes.
      *          ------------------------------------------
     C                   IF        I44NSL <> I44OSL                             B01
     C                             OR I44NSR <> I44OSR
     C                             OR I44NST <> I44OST
      *
      *          -----------------------------------------------
      *          If there was an old status.
      *          -----------------------------------------------
     C                   IF        I44OST <> *BLANKS                            B02
     C                   MOVE      I44HED        K#HEDC
     C                   MOVE      I44OST        K#STAT
     C                   EVAL      K#EOEI = '1'
      *
      *          -----------------------------------------------
      *          Read all exit programs and call the program.
      *          -----------------------------------------------
     C     $AF1K1        CHAIN     OMSAFL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B03
     C                   MOVEL(P)  AFSEPL        W#OBJQ
     C                   EVAL      W#OBJQ = %TRIMR(W#OBJQ) + '/'
     C     W#OBJQ        CAT       AFSEPG:0      W#OBJQ
      *
      *                    --------------------------------
      *                    Interface V1R1M0
      *                    --------------------------------
     C                   IF        AFINTL = '*V1R1M0'                           B04
     C                   EVAL      P#HEDC = NCHEDC
     C                   EVAL      P#INC# = NCINC#
     C                   EVAL      P#CTN# = NCCTN#
     C                   EVAL      P#EOEI = '1'
     C                   EVAL      P#STAO = I44OST
     C                   EVAL      P#STRO = I44OSR
     C                   EVAL      P#SUL# = I44OSL
     C                   EVAL      P#STAN = I44NST
     C                   EVAL      P#STRN = I44OST
     C                   EVAL      SUL# = I44NSL
     C                   EVAL      P#ACTC = I44ACT
     C                   CALLP(E)  W#OBJQ_001 ( P#HEDC : P#INC# : P#CTN# :
     C                             P#EOEI : P#STAO : P#STRO : P#SUL# :
     C                             P#STAN : P#STRN : SUL# : P#ACTC )
     C                   EVAL      *IN99 = %ERROR
      *
      *                    --------------------------------
      *                    Write a log entry.
      *                    --------------------------------
     C                   MOVEA     AFSEPG        MC(1)
     C                   MOVEA     AFSEPL        MC(11)
     C                   MOVEA     NCINC#        MC(21)
     C                   MOVEA     NCHEDC        MC(31)
     C                   MOVEA     I44OST        MC(36)
     C                   MOVEA     I44OSR        MC(41)
     C                   MOVEA     I44OSL        MC(51)
     C                   MOVEA     I44NST        MC(52)
     C                   MOVEA     I44NSR        MC(57)
     C                   MOVEA     I44NSL        MC(67)
     C                   MOVEA     MC            P#MSGD
     C                   EVAL      P#HEDC = NCHEDC
     C                   EVAL      P#INC# = NCINC#
     C                   EVAL      P#ACTC = I44ACT
     C                   EVAL      P#MSID = 'OMQA01E'
     C                   EVAL      P#MSGF = 'OMHAPI'
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMX929 ( P#HEDC : P#INC# : P#ACTC :
     C                             P#MSID : P#MSGD : P#MSGF : P#STAT )
     C                   ENDIF                                                  E04
      *
     C     $AF1K1        READE     OMSAFL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E03
     C                   ENDIF                                                  E02
      *
      *          -----------------------------------------------
      *          If there is a new status
      *          -----------------------------------------------
     C                   IF        NCSTAT <> *BLANKS                            B02
     C                   MOVE      NCHEDC        K#HEDC
     C                   MOVE      NCSTAT        K#STAT
     C                   EVAL      K#EOEI = '0'
      *
      *          ------------------------------------------
      *          Read all entry programs and call the program.
      *          ------------------------------------------
     C     $AF1K1        CHAIN     OMSAFL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B03
     C                   MOVEL(P)  AFSEPL        W#OBJQ
     C                   EVAL      W#OBJQ = %TRIMR(W#OBJQ) + '/'
     C     W#OBJQ        CAT       AFSEPG:0      W#OBJQ
      *
      *                    --------------------------------
      *                    Interface V1R1M0
      *                    --------------------------------
     C                   IF        AFINTL = '*V1R1M0'                           B04
     C                   EVAL      P#HEDC = NCHEDC
     C                   EVAL      P#INC# = NCINC#
     C                   EVAL      P#CTN# = NCCTN#
     C                   EVAL      P#EOEI = '1'
     C                   EVAL      P#STAO = I44OST
     C                   EVAL      P#STRO = I44OSR
     C                   EVAL      P#SUL# = I44OSL
     C                   EVAL      P#STAN = I44NST
     C                   EVAL      P#STRN = I44OST
     C                   EVAL      SUL# = I44NSL
     C                   EVAL      P#ACTC = I44ACT
     C                   CALLP(E)  W#OBJQ_001 ( P#HEDC : P#INC# : P#CTN# :
     C                             P#EOEI : P#STAO : P#STRO : P#SUL# :
     C                             P#STAN : P#STRN : SUL# : P#ACTC )
     C                   EVAL      *IN99 = %ERROR
      *
      *                    --------------------------------
      *                    Write a log entry.
      *                    --------------------------------
     C                   MOVEA     AFSEPG        MC(1)
     C                   MOVEA     AFSEPL        MC(11)
     C                   MOVEA     NCINC#        MC(21)
     C                   MOVEA     NCHEDC        MC(31)
     C                   MOVEA     I44OST        MC(36)
     C                   MOVEA     I44OSR        MC(41)
     C                   MOVEA     I44OSL        MC(51)
     C                   MOVEA     I44NST        MC(52)
     C                   MOVEA     I44NSR        MC(57)
     C                   MOVEA     I44NSL        MC(67)
     C                   MOVEA     MC            P#MSGD
     C                   EVAL      P#HEDC = NCHEDC
     C                   EVAL      P#INC# = NCINC#
     C                   EVAL      P#ACTC = I44ACT
     C                   EVAL      P#MSID = 'OMQA01E'
     C                   EVAL      P#MSGF = 'OMHAPI'
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMX929 ( P#HEDC : P#INC# : P#ACTC :
     C                             P#MSID : P#MSGD : P#MSGF : P#STAT )
     C                   ENDIF                                                  E04
      *
     C     $AF1K1        READE     OMSAFL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSEND - Send this incident                                 *
      *                                                             *
      * This routine checks which user or databases want to receive *
      * incidents and how they want to receive it.                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSEND        BEGSR
      *
      *          ------------------------------------------
      *          Mail extension available?
      *          ------------------------------------------
     C                   OPEN      OMMSX
     C                   READ      OMMSX
     C                   EVAL      *IN99 = %EOF
     C                   CLOSE     OMMSX
      *
     C                   IF        *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          External database available for this helpdesk.
      *          ------------------------------------------
     C     NCHEDC        CHAIN     OMMHXL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *                    --------------------------------
      *                    Send it.
      *                    --------------------------------
     C                   IF        HXEXTI <> '0'                                B02
     C                             AND *IN99 = *OFF
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   MOVE      HXEXTI        P#EXTI
     C                   MOVE      HXEEMC        P#EEMC
     C                   EVAL      P#BCCE = *BLANKS
     C                   EVAL      %SUBST(P#MATF:1:5) = 'OMMTI'
     C                   EVAL      %SUBST(P#MATL:1:5) = '*LIBL'
     C                   EVAL      P#MATM = *BLANKS
     C                   EVAL      P#SUL# = '9'
     C                   EVAL      P#MSRV = '*DFT'
     C                   EVAL      P#PORT = '*DFT'
     C                   EVAL      P#EMAC = '*DFT'
     C                   EVAL      P#SUBJ = '*INC'
     C                   CALLP     OMH043C ( P#HEDC : P#INC# : P#EXTI :
     C                             P#MSRV : P#PORT : P#EMAC : P#EEMC :
     C                             P#BCCE : P#SUBJ : P#MATF : P#MATL :
     C                             P#MATM : P#SUL# : P#STAT )
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          External interface for the customer?
      *          ------------------------------------------
     C     CACUSC        CHAIN     OMMCXL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *                    --------------------------------
      *                    Send it.
      *                    --------------------------------
     C                   IF        CXEXTI <> '0'                                B02
     C                             AND *IN99 = *OFF
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   MOVE      CXEXTI        P#EXTI
     C                   MOVE      CXEEMC        P#EEMC
     C                   EVAL      P#BCCE = *BLANKS
     C                   EVAL      %SUBST(P#MATF:1:5) = 'OMMTI'
     C                   EVAL      %SUBST(P#MATL:1:5) = '*LIBL'
     C                   EVAL      P#MATM = *BLANKS
     C                   EVAL      P#SUL# = '0'
     C                   EVAL      P#MSRV = '*DFT'
     C                   EVAL      P#PORT = '*DFT'
     C                   EVAL      P#EMAC = '*DFT'
     C                   EVAL      P#SUBJ = '*INC'
     C                   CALLP     OMH043C ( P#HEDC : P#INC# : P#EXTI :
     C                             P#MSRV : P#PORT : P#EMAC : P#EEMC :
     C                             P#BCCE : P#SUBJ : P#MATF : P#MATL :
     C                             P#MATM : P#SUL# : P#STAT )
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Must the customer contact receive mail.
      *          ------------------------------------------
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      CACUSC        P#CUSC
     C                   MOVE      CACCTC        P#CCTC
     C                   MOVE      I44ACT        P#ACTC
     C                   EVAL      P#MATL = *BLANKS
     C                   EVAL      P#MATF = *BLANKS
     C                   EVAL      P#MATM = *BLANKS
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH045 ( P#HEDC : P#CUSC : P#CCTC :
     C                             P#ACTC : P#MATL : P#MATF : P#MATM :
     C                             P#STAT )
      *
      *                    --------------------------------
      *                    If found, send it to the customer contact
      *                    --------------------------------
     C                   IF        P#STAT = C#NORM                              B02
     C                   MOVE      CACUSC        K#CUSC
     C                   MOVE      CACCTC        K#CCTC
     C     $CO1K1        CHAIN     OMCCOL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B03
     C                             AND COEMAC <> *BLANKS
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   EVAL      P#EXTI = '0'
     C                   MOVE      COEMAC        P#EEMC
     C                   MOVE      SXBCCE        P#BCCE
     C                   EVAL      P#MATF = P#MATF
     C                   EVAL      P#MATL = P#MATL
     C                   EVAL      P#MATM = P#MATM
     C                   EVAL      P#SUL# = '0'
     C                   EVAL      P#MSRV = '*DFT'
     C                   EVAL      P#PORT = '*DFT'
     C                   EVAL      P#EMAC = '*DFT'
     C                   EVAL      P#SUBJ = '*INC'
     C                   CALLP     OMH043C ( P#HEDC : P#INC# : P#EXTI :
     C                             P#MSRV : P#PORT : P#EMAC : P#EEMC :
     C                             P#BCCE : P#SUBJ : P#MATF : P#MATL :
     C                             P#MATM : P#SUL# : P#STAT )
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          -------------------------------------------------
      *          Send the Status route e-mail
      *          -------------------------------------------------
      *
      *          -------------------------------------------------
      *          The incident has left a status route
      *          -------------------------------------------------
     C                   IF        I44NSR <> I44OSR                             B02
     C                             AND I44OSR <> *BLANKS
      *
      *                    --------------------------------
      *                    Read all exit entries
      *                    --------------------------------
     C                   MOVE      NCHEDC        K#HEDC
     C                   MOVEL(P)  I44OSR        K#STRC
     C                   EVAL      K#ACTC = '1'
     C     $CF2K1        CHAIN     OMRCFL2
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B03
      *
      *                    --------------------------------
      *                    Entry found, but is it OK?
      *                    --------------------------------
     C                   IF        RCSTAC = '*ALL'                              B04
     C                             OR RCSTAC = *BLANKS
     C                   EXSR      SREMAL
     C                   ENDIF                                                  E04
      *
      *                    --------------------------------
      *                    Read the next status route
      *                    --------------------------------
     C     $CF2K1        READE     OMRCFL2
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E03
     C                   ENDIF                                                  E02
      *
      *          -------------------------------------------------
      *          The incident has entered a status route
      *          -------------------------------------------------
     C                   IF        I44NSR <> I44OSR                             B02
      *
      *                    --------------------------------
      *                    Read all entry entries
      *                    --------------------------------
     C                   MOVE      NCHEDC        K#HEDC
     C                   MOVEL(P)  I44NSR        K#STRC
     C                   EVAL      K#ACTC = '0'
     C     $CF2K1        CHAIN     OMRCFL2
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B03
      *
      *                    --------------------------------
      *                    Entry found, but is it OK?
      *                    --------------------------------
     C                   IF        RCSTAC = '*ALL'                              B04
     C                             OR RCSTAC = *BLANKS
     C                   EXSR      SREMAL
     C                   ENDIF                                                  E04
      *
      *                    --------------------------------
      *                    Read the next status route
      *                    --------------------------------
     C     $CF2K1        READE     OMRCFL2
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E03
     C                   ENDIF                                                  E02
      *
      *          -------------------------------------------------
      *          The incident has left a status
      *          -------------------------------------------------
     C                   IF        I44OST <> *BLANKS                            B02
     C                             AND I44OST <> I44NST
     C                             OR I44OST <> *BLANKS
     C                             AND I44OSR <> I44NSR
      *
      *                    --------------------------------
      *                    Read all exit entries
      *                    --------------------------------
     C                   MOVE      NCHEDC        K#HEDC
     C                   MOVEL(P)  I44OSR        K#STRC
     C                   EVAL      K#ACTC = '1'
     C     $CF2K1        CHAIN     OMRCFL2
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B03
      *
      *                    --------------------------------
      *                    Entry found, but is it OK?
      *                    --------------------------------
     C                   IF        RCSTAC <> '*ALL'                             B04
     C                             AND RCSTAC <> *BLANKS
     C                             AND RCSTAC = I44OST
     C                   EXSR      SREMAL
     C                   ENDIF                                                  E04
      *
      *                    --------------------------------
      *                    Read the next record
      *                    --------------------------------
     C     $CF2K1        READE     OMRCFL2
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E03
     C                   ENDIF                                                  E02
      *
      *          -------------------------------------------------
      *          The incident has entered a status
      *          -------------------------------------------------
     C                   IF        I44OST <> I44NST                             B02
     C                             OR I44OSR <> I44NSR
      *
      *                    --------------------------------
      *                    Read all entry entries
      *                    --------------------------------
     C                   MOVE      NCHEDC        K#HEDC
     C                   MOVEL(P)  I44NSR        K#STRC
     C                   EVAL      K#ACTC = '0'
     C     $CF2K1        CHAIN     OMRCFL2
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B03
      *
      *                    --------------------------------
      *                    Entry found, but is it OK?
      *                    --------------------------------
     C                   IF        RCSTAC <> '*ALL'                             B04
     C                             AND RCSTAC <> *BLANKS
     C                             AND RCSTAC = I44NST
     C                   EXSR      SREMAL
     C                   ENDIF                                                  E04
      *
      *                    --------------------------------
      *                    Read the next record
      *                    --------------------------------
     C     $CF2K1        READE     OMRCFL2
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E03
     C                   ENDIF                                                  E02
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
      * SREMAL - SEND E-MAIL                                        *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SREMAL        BEGSR
      *
      *          ------------------------------------------
      *          Call the e-mail sender
      *          ------------------------------------------
     C                   MOVE      NCHEDC        P#HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   MOVE      RCEXTI        P#EXTI
     C                   MOVE      RCEEMC        P#EEMC
     C                   MOVE      RCBCCE        P#BCCE
     C                   MOVE      RCMATF        P#MATF
     C                   MOVE      RCMATL        P#MATL
     C                   MOVE      RCMATM        P#MATM
     C                   MOVE      RCSUL#        P#SUL#
     C                   EVAL      P#MSRV = '*DFT'
     C                   EVAL      P#PORT = '*DFT'
     C                   EVAL      P#EMAC = '*DFT'
     C                   EVAL      P#SUBJ = '*INC'
     C                   CALLP     OMH043C ( P#HEDC : P#INC# : P#EXTI :
     C                             P#MSRV : P#PORT : P#EMAC : P#EEMC :
     C                             P#BCCE : P#SUBJ : P#MATF : P#MATL :
     C                             P#MATM : P#SUL# : P#STAT )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSHUT - Check if it is time to leave                       *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSHUT        BEGSR
      *
      *          ------------------------------------------
      *          Test
      *          ------------------------------------------
     C                   EVAL      *IN99 = %SHTDN
     C                   IF        *IN99 = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
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
     C                   EVAL      P#LEN = *ZEROS
     C                   EVAL      P#WAIT = *ZEROS
     C                   EVAL      P#MSRV = *BLANKS
      *
      *          ------------------------------------------
      *          Savefield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Keyfield definition
      *          ------------------------------------------
     C                   EVAL      K#ACTC = *BLANKS
      *
     C                   EVAL      PNAME = *BLANKS
     C                   EVAL      PVAR = *BLANKS
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      W#OBJQ = *BLANKS
      *
      *          ------------------------------------------
      *          IBM API field definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter list for OMX929 (write incident log)
      *          ------------------------------------------
     C     $MX929        PLIST
     C                   PARM      NCHEDC        P#HEDC
     C                   PARM      NCINC#        P#INC#
     C                   PARM      I44ACT        P#ACTC
     C                   PARM      'OMQA01E'     P#MSID
     C                   PARM                    P#MSGD
     C                   PARM      'OMHAPI'      P#MSGF
     C                   PARM      C#NORM        P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH037C (Get mail)
      *          ------------------------------------------
     C     $H037C        PLIST
     C                   PARM                    P#HUID
     C                   PARM                    P#HADR
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH043C (Send mail)
      *          ------------------------------------------
     C     $H043C        PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    P#INC#
     C                   PARM                    P#EXTI
     C                   PARM      '*DFT'        P#MSRV
     C                   PARM      '*DFT'        P#PORT
     C                   PARM      '*DFT'        P#EMAC
     C                   PARM                    P#EEMC
     C                   PARM                    P#BCCE
     C                   PARM      '*INC'        P#SUBJ
     C                   PARM                    P#MATF
     C                   PARM                    P#MATL
     C                   PARM                    P#MATM
     C                   PARM                    P#SUL#
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH044C (Get next data queue entry)
      *          ------------------------------------------
     C     $H044C        PLIST
     C                   PARM      'OMH044C'     P#OBJC
     C                   PARM      '*LIBL'       P#OBJL
     C                   PARM      '2'           P#OPTI
     C                   PARM                    D#44IN
     C                   PARM                    P#LEN
     C                   PARM                    P#WAIT
     C                   PARM      C#NORM        P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH045 (Get contact mail template)
      *          ------------------------------------------
     C     $MH045        PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    P#CUSC
     C                   PARM                    P#CCTC
     C                   PARM                    P#ACTC
     C                   PARM                    P#MATL
     C                   PARM                    P#MATF
     C                   PARM                    P#MATM
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          OMINCLn key list Incident File
      *          ------------------------------------------
     C     $NC1K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#INC#
      *
      *          ------------------------------------------
      *          OMSAFL1 Status actions
      *          ------------------------------------------
     C     $AF1K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#STAT
     C                   KFLD                    K#EOEI
      *
      *          ------------------------------------------
      *          OMSRFL1 Status Route actions
      *          ------------------------------------------
     C     $AF1K2        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#STRC
     C                   KFLD                    K#EOEI
      *
      *          ------------------------------------------
      *          OMCCOL1 Customer contacts
      *          ------------------------------------------
     C     $CO1K1        KLIST
     C                   KFLD                    K#CUSC
     C                   KFLD                    K#CCTC
      *
      *          ------------------------------------------
      *          OMCCAL1 Customer calls
      *          ------------------------------------------
     C     $CA1K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#CTN#
      *          ------------------------------------------
      *          OMRCFL2 Status Route actions
      *          ------------------------------------------
     C     $CF2K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#STRC
     C                   KFLD                    K#ACTC
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
      *
     C                   WHEN      E#FILE = 'OMSAFL1 '                          X02
     C                   MOVEL     SAFL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMSRFL1 '                          X02
     C                   MOVEL     SRFL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMINCL1 '                          X02
     C                   MOVEL     INCL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMMCXL1 '                          X02
     C                   MOVEL     MCXL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMMHXL1 '                          X02
     C                   MOVEL     MHXL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMRCFL2 '                          X02
     C                   MOVEL     RCFL2         P#FILE
      *
     C                   WHEN      E#FILE = 'OMMSX   '                          X02
     C                   MOVEL     MSX           P#FILE
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
      *          Close incident processor
      *          ------------------------------------------
     C                   CALLP     OMH037_3_001()
      *
      *          ------------------------------------------
      *          Seton LR
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
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
