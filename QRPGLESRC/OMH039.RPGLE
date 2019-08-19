     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH039                                        *
      * Function ...: Distribute definitions                        *
      * Module .....: Helpdesk                                      *
      * Author .....: Remain Software                               *
      * Parameters .: P#HEDC - Helpdesk code, *NONE or *ALL         *
      *               P#HOPT - 0=Send definitions                   *
      *                        1=Send Incidents                     *
      *                        2=Send All                           *
      *                        9=Send Nothing                       *
      *               P#CUSC - Customer code, *NONE or *ALL         *
      *               P#COPT - 0=Send definitions                   *
      *                        1=Send Incidents                     *
      *                        2=Send All                           *
      *                        9=Send Nothing                       *
      *               P#EXTI - 1=Lotus notes                        *
      *               P#FRIN - From incident                        *
      *               P#TOIN - To incident                          *
      *               P#STAT - Status                               *
      *                                                             *
      * This program sends the definitions to the databases which   *
      * require remote replication of the definition. First, the    *
      * helpdesk mail extensions are read and distributed to the    *
      * correct database. Then the customer mail extensions are     *
      * distributed to the correct customer database.               *
      *                                                             *
      * This program is capable of serving all helpdesks, or a      *
      * specific helpdesk. This is true also for the customer.      *
      * The P#xOPT parameters can be set to also include all        *
      * incidents if this is a first time database installation.    *
      *                                                             *
      * The external interface indicator P#EXTI is set to define    *
      * the templates to be send.  Check the parameter description  *
      * to find out which external interfaces are supported.        *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMMHXL1   IF   E           K DISK    INFDS(MHXL1)
     F                                     INFSR(*PSSR)
     FOMCUSL1   IF   E           K DISK    INFDS(CUSL1)
     F                                     INFSR(*PSSR)
     FOMMCXL1   IF   E           K DISK    INFDS(MCXL1)
     F                                     INFSR(*PSSR)
     FOMINCL1   IF   E           K DISK    INFDS(INCL1)
     F                                     INFSR(*PSSR)
     FOMINCJ5   IF   E           K DISK    INFDS(INCJ5)
     F                                     RENAME(OMINCR:INCJ5R)
     F                                     INFSR(*PSSR)
     FOMCCAL3   IF   E           K DISK    INFDS(CCAL3)
     F                                     INFSR(*PSSR)
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * ARRAY pecifications                                         *
      * ----------------------------------------------------------- *
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
     D MHXL1           DS            57
     D MCXL1           DS            57
     D CUSL1           DS            57
     D INCL1           DS            57
     D INCJ5           DS            57
     D CCAL3           DS            57
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
      *          Named constants
      *          ------------------------------------------
     D C#NORM          C                   CONST('*NORM')
     D C#TERM          C                   CONST('*TERM')
      * Prototype for OMH039
     D OMH039          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#HOPT_                             LIKE(P#HOPT)
     D P#CUSC_                             LIKE(P#CUSC)
     D P#COPT_                             LIKE(P#COPT)
     D P#EXTI_                             LIKE(P#EXTI)
     D P#FRIN_                             LIKE(P#FRIN)
     D P#TOIN_                             LIKE(P#TOIN)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH039C2'
     D OMH039C2        PR                  EXTPGM('OMH039C2')
     D HXHEDC_                             LIKE(HXHEDC)
     D HXEEMC_                             LIKE(HXEEMC)
     D P#EXTI_                             LIKE(P#EXTI)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH039C3'
     D OMH039C3        PR                  EXTPGM('OMH039C3')
     D P#HEDC_                             LIKE(P#HEDC)
     D USCUSC_                             LIKE(USCUSC)
     D HXEEMC_                             LIKE(HXEEMC)
     D P#EXTI_                             LIKE(P#EXTI)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH043C (Send mail)
      * Prototype for 'OMH043C'
     D OMH043C         PR                  EXTPGM('OMH043C')
     D P2HEDC_                             LIKE(P2HEDC)
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
      *          OMINCJ5 key list Incident File
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
     D OMH039          PI
     D P#HEDC                         5
     D P#HOPT                         1
     D P#CUSC                        10
     D P#COPT                         1
     D P#EXTI                         1
     D P#FRIN                        10
     D P#TOIN                        10
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D I#PSSR          S              1
     D P#BCCE          S                   LIKE(BCCE)
     D P#EEMC          S                   LIKE(EEMC)
     D P#EMAC          S                   LIKE(EMAC)
     D P#EOEI          S                   LIKE(EOEI)
     D P#FILE          S             57
     D P#INC#          S                   LIKE(INC#)
     D P#MATF          S                   LIKE(MATF)
     D P#MATL          S                   LIKE(MATL)
     D P#MATM          S                   LIKE(MATM)
     D P#MSRV          S             15
     D P#MSTA          S             40
     D P#PORT          S                   LIKE(PORT)
     D P#SUBJ          S                   LIKE(SUBJ)
     D P#SUL#          S                   LIKE(SUL#)
     D P2HEDC          S                   LIKE(HEDC)
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
      *          Parameters
      *          ------------------------------------------
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
      * Send helpdesk definitions.                                  *
      * Send customer definitions.                                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Must we send helpdesk definitions?
      *          ------------------------------------------
     C                   IF        P#HEDC <> '*NONE'                            B01
     C                   EXSR      SRHEDC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Must we send customer definitions?
      *          ------------------------------------------
     C                   IF        P#CUSC <> '*NONE'                            B01
     C                   EXSR      SRCUSC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRHEDC - Send helpdesk definitions                          *
      *                                                             *
      * This routine sends the helpdesk definitions to the          *
      * correct database.                                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRHEDC        BEGSR
      *
      *          ------------------------------------------
      *          Must we send anything?
      *          ------------------------------------------
     C                   IF        P#HOPT <> '9'                                B01
      *
      *          ------------------------------------------
      *          Send a specific helpdesk ...
      *          ------------------------------------------
     C                   IF        P#HEDC <> '*ALL'                             B02
     C     P#HEDC        CHAIN     OMMHXL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
     C                   IF        *IN99 <> *ON                                 B03
     C                             AND HXEXTI = P#EXTI
     C                             AND HXEEMC <> *BLANKS
      *
      *          ------------------------------------------
      *          Must we send definitions?
      *          ------------------------------------------
     C                   IF        P#HOPT <> '1'                                B04
     C                   CALLP     OMH039C2 ( HXHEDC : HXEEMC : P#EXTI :
     C                             P#STAT )
     C                   EXSR      SRHCUS
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Must we send helpdesk incidents?
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      P#HOPT = '1'
     C                   EXSR      SRHINC
     C                   WHEN      P#HOPT = '2'
     C                   EXSR      SRHINC
     C                   ENDSL
      *
     C                   ENDIF                                                  E03
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Send all helpdesks
      *          ------------------------------------------
     C                   IF        P#HEDC = '*ALL'                              B03
     C     *LOVAL        SETLL     OMMHXL1
     C                   READ      OMMHXL1
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF                                 B04
      *
      *          ------------------------------------------
      *          Must we send definitions?
      *          ------------------------------------------
     C                   IF        P#HOPT <> '1'                                B05
     C                   CALLP     OMH039C2 ( HXHEDC : HXEEMC : P#EXTI :
     C                             P#STAT )
     C                   EXSR      SRHCUS
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          Must we send helpdesk incidents?
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      P#HOPT = '1'
     C                   EXSR      SRHINC
     C                   WHEN      P#HOPT = '2'
     C                   EXSR      SRHINC
     C                   ENDSL
      *
     C                   READ      OMMHXL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E04
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
      * ----------------------------------------------------------- *
      * SRHCUS - Send customer database for helpdesk                *
      *                                                             *
      * This routine sends the customers to the correct database.   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRHCUS        BEGSR
      *
      *          ------------------------------------------
      *          Read all customers
      *          ------------------------------------------
     C     *LOVAL        SETLL     OMCUSL1
     C                   READ      OMCUSL1
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF                                 B01
     C                   CALLP     OMH039C3 ( P#HEDC : USCUSC : HXEEMC :
     C                             P#EXTI : P#STAT )
     C                   READ      OMCUSL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCUSC - Send customer definitions                          *
      *                                                             *
      * This routine sends the customer definitions to the          *
      * correct customer database.                                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCUSC        BEGSR
      *
      *          ------------------------------------------
      *          Must we send anything?
      *          ------------------------------------------
     C                   IF        P#COPT <> '9'                                B01
      *
      *          ------------------------------------------
      *          Send a specific customer ...
      *          ------------------------------------------
     C                   IF        P#CUSC <> '*ALL'                             B02
     C     P#CUSC        CHAIN     OMCUSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF
     C     P#CUSC        CHAIN     OMMCXL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF
      *
     C                   IF        *IN99 <> *ON                                 B03
     C                             AND CXEXTI = P#EXTI
     C                             AND CXEEMC <> *BLANKS
      *
      *          ------------------------------------------
      *          Must we send definitions?
      *          ------------------------------------------
     C                   IF        P#COPT <> '1'                                B04
     C                   CALLP     OMH039C3 ( P#HEDC : CXCUSC : CXEEMC :
     C                             P#EXTI : P#STAT )
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Must we send customer incidents?
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      P#COPT = '1'
     C                   EXSR      SRCINC
     C                   WHEN      P#COPT = '2'
     C                   EXSR      SRCINC
     C                   ENDSL
      *
     C                   ENDIF                                                  E03
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Send all customer definitions
      *          ------------------------------------------
     C                   IF        P#CUSC = '*ALL'                              B03
     C     *LOVAL        SETLL     OMMCXL1
     C                   READ      OMMCXL1
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF                                 B04
      *
      *          ------------------------------------------
      *          If a customer record exists
      *          ------------------------------------------
     C     CXCUSC        SETLL     OMCUSL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B05
      *
      *          ------------------------------------------
      *          Must we send definitions?
      *          ------------------------------------------
     C                   IF        P#COPT <> '1'                                B06
     C                   CALLP     OMH039C3(P#HEDC : CXCUSC : CXEEMC : P#EXTI :
     C                             P#STAT )
     C                   ENDIF                                                  E06
      *
      *          ------------------------------------------
      *          Must we send customer incidents?
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      P#COPT = '1'
     C                   EXSR      SRCINC
     C                   WHEN      P#COPT = '2'
     C                   EXSR      SRCINC
     C                   ENDSL
     C                   ENDIF                                                  E05
      *
     C                   READ      OMMCXL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E04
      *
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
      * ----------------------------------------------------------- *
      * SRCINC - Send all incidents to the customer database        *
      *                                                             *
      * This routine sends all incidents to the helpdesk database.  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCINC        BEGSR
      *
      *                    --------------------------------
      *                    Send it.
      *                    --------------------------------
     C     CXCUSC        CHAIN     OMCCAL3
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B01
     C     $CJ5K1        CHAIN     OMINCJ5
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVE      NCHEDC        P2HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   EVAL      P#EXTI = P#EXTI
     C                   MOVE      CXEEMC        P#EEMC
     C                   EVAL      %SUBST(P#MATF:1:5) = 'OMMTI'
     C                   EVAL      %SUBST(P#MATL:1:5) = '*LIBL'
     C                   EVAL      P#MATM = *BLANKS
     C                   EVAL      P#SUL# = '0'
     C                   EVAL      P#MSRV = '*DFT'
     C                   EVAL      P#PORT = '*DFT'
     C                   EVAL      P#EMAC = '*DFT'
     C                   EVAL      P#SUBJ = '*INC'
     C                   CALLP     OMH043C ( P2HEDC : P#INC# : P#EXTI :
     C                             P#MSRV : P#PORT : P#EMAC : P#EEMC :
     C                             P#BCCE : P#SUBJ : P#MATF : P#MATL :
     C                             P#MATM : P#SUL# : P#STAT )
     C                   ENDIF                                                  E02
     C     CXCUSC        READE     OMCCAL3
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
      * SRHINC - Send all incidents to the database                 *
      *                                                             *
      * This routine sends all incidents to the helpdesk database.  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRHINC        BEGSR
      *
      *                    --------------------------------
      *                    Send it.
      *                    --------------------------------
     C     $NC1K1        SETLL     OMINCL1
     C     HXHEDC        READE     OMINCL1
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF                                 B01
     C                   MOVE      NCHEDC        P2HEDC
     C                   MOVE      NCINC#        P#INC#
     C                   EVAL      P#EXTI = P#EXTI
     C                   MOVE      HXEEMC        P#EEMC
     C                   EVAL      %SUBST(P#MATF:1:5) = 'OMMTI'
     C                   EVAL      %SUBST(P#MATL:1:5) = '*LIBL'
     C                   EVAL      P#MATM = *BLANKS
     C                   EVAL      P#SUL# = '9'
     C                   EVAL      P#MSRV = '*DFT'
     C                   EVAL      P#PORT = '*DFT'
     C                   EVAL      P#EMAC = '*DFT'
     C                   EVAL      P#SUBJ = '*INC'
     C                   CALLP     OMH043C ( P2HEDC : P#INC# : P#EXTI :
     C                             P#MSRV : P#PORT : P#EMAC : P#EEMC :
     C                             P#BCCE : P#SUBJ : P#MATF : P#MATL :
     C                             P#MATM : P#SUL# : P#STAT )
     C     HXHEDC        READE     OMINCL1
     C                   EVAL      *IN99 = %EOF
     C  N99              IF        NCINC# > P#TOIN
     C                   LEAVE
     C                   ENDIF
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
     C                   EVAL      P#MSRV = *BLANKS
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
      *
      *          ------------------------------------------
      *          IBM API field definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter list for OMH039C2 (Send definitions)
      *          ------------------------------------------
     C     $H39C2        PLIST
     C                   PARM                    HXHEDC
     C                   PARM                    HXEEMC
     C                   PARM                    P#EXTI
     C                   PARM                    P#STAT
      *
     C     $H39C3        PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    CXCUSC
     C                   PARM                    CXEEMC
     C                   PARM                    P#EXTI
     C                   PARM                    P#STAT
      *
     C     $H39C#        PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    USCUSC
     C                   PARM                    HXEEMC
     C                   PARM                    P#EXTI
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH043C (Send mail)
      *          ------------------------------------------
     C     $H043C        PLIST
     C                   PARM                    P2HEDC
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
      *          OMINCJ5 key list Incident File
      *          ------------------------------------------
     C     $CJ5K1        KLIST
     C                   KFLD                    CAHEDC
     C                   KFLD                    CACTN#
      *
     C     $NC1K1        KLIST
     C                   KFLD                    HXHEDC
     C                   KFLD                    P#FRIN
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
     C                   WHEN      E#FILE = 'OMINCJ5 '                          X02
     C                   MOVEL     INCJ5         P#FILE
      *
     C                   WHEN      E#FILE = 'OMINCL1 '                          X02
     C                   MOVEL     INCL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMCCAL3 '                          X02
     C                   MOVEL     CCAL3         P#FILE
      *
     C                   WHEN      E#FILE = 'OMMCXL1 '                          X02
     C                   MOVEL     MCXL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMMHXL1 '                          X02
     C                   MOVEL     MHXL1         P#FILE
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
