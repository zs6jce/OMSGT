     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ------------------------------------------------------------ *
      * COMPILATION PARAMETERS                                       *
      * ------------------------------------------------------------ *
      *                                                              *
      * This program should be compiled with the following options   *
      *                                                              *
      *      COMMIT(*NONE)                                           *
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMQCHGCU                                      *
      * Function ...: API Maintain customer                         *
      *                                                             *
      * Author .....: Remain Software                               *
      * Parameters .: P4INTL  - Interface level         (I)         *
      *               P4ACTC  - Activity code           (I)         *
      *               P4BUF   - Buffer data             (I/O)       *
      *               P4PRC   - Processing array        (I)         *
      *               P4ERR   - Error data              (O)         *
      *               P4FLD   - Field error data        (I/O)       *
      *                                                             *
      * This program is used to process changes on a customer.      *
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
      * K4XXXX - Input buffer data structure                        *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMCUSL1   UF A E           K DISK    INFDS(CUSL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMCCOL1   IF   E           K DISK    INFDS(CCOL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMCCAL3   IF   E           K DISK    INFDS(CCAL3)
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
      *
      *                   ---------------------------------
      *                   Actually used arrays
      *                   ---------------------------------
     D VAR             S            256    DIM(15)
     D MSD             S              7    DIM(15) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
     D ERR             S              4    DIM(30)
      *
      *                   ---------------------------------
      *                   Arrays to fool the compiler (to
      *                   prevent error message QRG4140)
      *                   ---------------------------------
      *COPY QCPYSRC,OMSREFREN
      *
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
      *          The customer file as a data structure
      *          ------------------------------------------
     D DSCUSR        E DS                  EXTNAME(OMCUS) INZ
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
     D CUSL1           DS            57
     D CCAL3           DS            57
     D CCOL1           DS            57
     D IOBL1           DS            57
      *
      *          ------------------------------------------
      *          Call API buffer parameter
      *          DS for saving the buffer
      *          ------------------------------------------
     D P4BUF           DS          1024
     D S#BUF           DS          1024
      *
      *          ------------------------------------------
      *          Helpdesk data structures.
      *          ------------------------------------------
      /COPY QCPYLESRC,OMHLPDS
      *
      *          ------------------------------------------
      *          All buffers for OMQCHGCU
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQCHGCUDS
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
     D C#BLNK          C                   CONST('*BLANKS')
     D C#SA            C                   CONST('*SA')
     D C#S             C                   CONST('*S')
     D C#DFT           C                   CONST('*DFT ')
     D C#CURR          C                   CONST('*CURR')
     D C#FRST          C                   CONST('*FIRST')
     D C#GEN           C                   CONST('*GEN')
     D C#INTL          C                   CONST('INTL')
     D C#ACTC          C                   CONST('ACTC')
     D C#CUSC          C                   CONST('CUSC')
     D C#NAMD          C                   CONST('NAMD')
     D C#ADST          C                   CONST('ADST')
     D C#ZIPC          C                   CONST('ZIPC')
     D C#ADRE          C                   CONST('ADRE')
     D C#COUD          C                   CONST('COUD')
     D C#USF1          C                   CONST('USF1')
     D C#USF2          C                   CONST('USF2')
     D C#USF3          C                   CONST('USF3')
     D C#NCUS          C                   CONST('NCUS')
     D C#PAR#          C                   CONST(6)
      * Prototype for 'QMHSNDPM'
     D QMHSNDPM        PR                  EXTPGM('QMHSNDPM')
     D P#MIDC_                             LIKE(P#MIDC)
     D P#MSGQ_                             LIKE(P#MSGQ)
     D K4MSGD_                             LIKE(K4MSGD)
     D P#MDL#_                             LIKE(P#MDL#)
     D P#MSTC_                             LIKE(P#MSTC)
     D P#PMQC_                             LIKE(P#PMQC)
     D P#PSC#_                             LIKE(P#PSC#)
     D P#MSKC_                             LIKE(P#MSKC)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'OMX047C'
     D OMX047C         PR                  EXTPGM('OMX047C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGDTA_                             LIKE(MSGDTA)
      *
      *          ------------------------------------------
      *          No parameters!
      *          Set IE to high values to avoid a loop in TTLR
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
     D I#CCA3          S              1
     D I#CCO1          S              1
     D I#CUS1          S              1
     D I#PSSR          S              1
     D I#QUIT          S                   LIKE(INDI)
     D I#X967          S                   LIKE(INDI)
     D IE              S              5  0
     D IR              S              3  0
     D K#CCTC          S                   LIKE(CCTC)
     D K#CTN#          S                   LIKE(CTN#)
     D K#CUSC          S                   LIKE(CUSC)
     D K#HEDC          S                   LIKE(HEDC)
     D MS              S              3  0
     D MSGDTA          S             50
     D MSGID           S              7
     D MSGLIB          S             10
     D P#ACTC          S                   LIKE(ACTC)
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
     C                   MOVEL     P4BUF         K4BUF
     C                   MOVEL     P4PRC         K4PRC
     C                   MOVEL     P4FLD         K4FLD
     C                   MOVE      *BLANKS       K4ERR
     C                   MOVE      *BLANKS       K4FLDA
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
     C                   WHEN      P4ACTC = '21'                                Rename
     C                   EXSR      SRCH21                                       Rename
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
      * SRCH20 - Change code 20 Change                              *
      *                                                             *
      * This routine is executed when the program has to change     *
      * the entity                                                  *
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
      *          Get the customer record
      *          ------------------------------------------
     C     K4CUSC        CHAIN     OMCUSL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill the record and update
      *          ------------------------------------------
     C                   MOVE      K4CUSC        USCUSC
     C                   MOVE      K4NAMD        USNAMD
     C                   MOVE      K4ADST        USADST
     C                   MOVE      K4ZIPC        USZIPC
     C                   MOVE      K4ADRE        USADRE
     C                   MOVE      K4COUD        USCOUD
     C                   MOVE      K4USF1        USUSF1
     C                   MOVE      K4USF2        USUSF2
     C                   MOVE      K4USF3        USUSF3
     C                   EXSR      SRIO
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCH21 - Change code 21 Rename                              *
      *                                                             *
      * This routine renames a customer                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCH21        BEGSR
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
      *          Rename
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
      * SRCH40 - Change code 40 Delete                              *
      *                                                             *
      * This routine is executed when the program has to delete     *
      * the entity                                                  *
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
      *          Get the entity
      *          ------------------------------------------
     C     K4CUSC        CHAIN     OMCUSL1
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
      * SRCH60 - Change code 60 Add                                 *
      *                                                             *
      * This routine is executed when the program has to add        *
      * the entity                                                  *
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
      *          Fill the record and write
      *          ------------------------------------------
     C                   MOVE      K4CUSC        USCUSC
     C                   MOVE      K4NAMD        USNAMD
     C                   MOVE      K4ADST        USADST
     C                   MOVE      K4ZIPC        USZIPC
     C                   MOVE      K4ADRE        USADRE
     C                   MOVE      K4COUD        USCOUD
     C                   MOVE      K4USF1        USUSF1
     C                   MOVE      K4USF2        USUSF2
     C                   MOVE      K4USF3        USUSF3
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
      *          If add then reset the output record.
      *          ------------------------------------------
     C                   IF        P4ACTC = '60'                                B01
     C                   RESET                   OMCUSR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the record if this is other than add.
      *          ------------------------------------------
     C                   IF        P4ACTC <> '60'                               B01
      *
      *          ------------------------------------------
      *          The customer must exist.
      *          ------------------------------------------
     C                   EXSR      SRCUS1
     C     K4CUSC        CHAIN     OMCUSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEL     'OMQA701'     K4MSID
     C                   MOVEL     'E'           K4MSTP
     C                   MOVEA     K4CUSC        MC(1)
     C                   MOVEA     MC            K4MSGD
     C                   EVAL      W#FLD1 = C#CUSC
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill defaults if add or change
      *          ------------------------------------------
     C                   IF        P4ACTC = '60'                                B01
     C                             OR P4ACTC = '20'
      *
      *          ------------------------------------------
      *          Name may not be blanks
      *          ------------------------------------------
     C                   IF        K4NAMD = *BLANKS                             B02
     C                   MOVEL     'OMQA003'     K4MSID
     C                   MOVEL     'E'           K4MSTP
     C                   MOVEA     C#BLNK        MC(1)
     C                   MOVEA     C#NAMD        MC(33)
     C                   MOVEA     MC            K4MSGD
     C                   EVAL      W#FLD1 = C#NAMD
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          *SAME is the name, not allowed when adding.
      *          ------------------------------------------
     C                   IF        K4NAMD = '*SAME'                             B02
     C                   IF        P4ACTC = '60'                                B03
     C                   MOVEL     'OMQA003'     K4MSID
     C                   MOVEL     'E'           K4MSTP
     C                   MOVEA     C#SAME        MC(1)
     C                   MOVEA     C#NAMD        MC(33)
     C                   MOVEA     MC            K4MSGD
     C                   EVAL      W#FLD1 = C#NAMD
     C                   EXSR      SRERR
     C                   ELSE                                                   X03
     C                   MOVE      USNAMD        K4NAMD
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          *SAME is the street.
      *          ------------------------------------------
     C                   IF        K4ADST = '*SAME'                             B02
     C                   IF        P4ACTC = '60'                                B03
     C                   MOVE      *BLANKS       K4ADST
     C                   ELSE                                                   X03
     C                   MOVE      USADST        K4ADST
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          *SAME is the zip code
      *          ------------------------------------------
     C                   IF        K4ZIPC = '*SAME'                             B02
     C                   IF        P4ACTC = '60'                                B03
     C                   MOVE      *BLANKS       K4ZIPC
     C                   ELSE                                                   X03
     C                   MOVE      USZIPC        K4ZIPC
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          *SAME is the residence
      *          ------------------------------------------
     C                   IF        K4ADRE = '*SAME'                             B02
     C                   IF        P4ACTC = '60'                                B03
     C                   MOVE      *BLANKS       K4ADRE
     C                   ELSE                                                   X03
     C                   MOVE      USADRE        K4ADRE
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          *SAME is the country
      *          ------------------------------------------
     C                   IF        K4COUD = '*SAME'                             B02
     C                   IF        P4ACTC = '60'                                B03
     C                   MOVE      *BLANKS       K4COUD
     C                   ELSE                                                   X03
     C                   MOVE      USCOUD        K4COUD
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          User defined field 1
      *          ------------------------------------------
     C                   IF        K4USF1 = '*SAME'                             B02
     C                   IF        P4ACTC = '60'                                B03
     C                   MOVE      *BLANKS       K4USF1
     C                   ELSE                                                   X03
     C                   MOVE      USUSF1        K4USF1
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          User defined field 2
      *          ------------------------------------------
     C                   IF        K4USF2 = '*SAME'                             B02
     C                   IF        P4ACTC = '60'                                B03
     C                   MOVE      *BLANKS       K4USF2
     C                   ELSE                                                   X03
     C                   MOVE      USUSF2        K4USF2
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          User defined field 3
      *          ------------------------------------------
     C                   IF        K4USFA = '*SAME'                             B02
     C                   IF        P4ACTC = '60'                                B03
     C                   EVAL      K4USF3 = *ZEROS
     C                   ELSE                                                   X03
     C                   MOVE      USUSF3        K4USF3
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
      *          Customer code
      *          ------------------------------------------
      *                    May not be blanks.
      *                    --------------------------------
     C                   IF        K4CUSC = *BLANKS                             B01
     C                   MOVEL     'OMQA003'     K4MSID
     C                   MOVEL     'E'           K4MSTP
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     C#CUSC        MC(33)
     C                   MOVEA     MC            K4MSGD
     C                   EVAL      W#FLD1 = C#CUSC
     C                   EXSR      SRERR
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    May not exist if add.
      *                    --------------------------------
     C                   EXSR      SRCUS1
     C     K4CUSC        SETLL     OMCUSL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B01
     C                             AND P4ACTC = '60'
     C                   MOVEL     'OMQA704'     K4MSID
     C                   MOVEL     'E'           K4MSTP
     C                   MOVEA     K4CUSC        MC(1)
     C                   MOVEA     MC            K4MSGD
     C                   EVAL      W#FLD1 = C#CUSC
     C                   EXSR      SRERR
     C                   ELSE                                                   X01
      *
      *                    --------------------------------
      *                    Must exist otherwise.
      *                    --------------------------------
     C                   IF        *IN99 <> *ON                                 B02
     C                             AND P4ACTC <> '60'
     C                   MOVEL     'OMQA705'     K4MSID
     C                   MOVEL     'E'           K4MSTP
     C                   MOVEA     K4CUSC        MC(1)
     C                   MOVEA     MC            K4MSGD
     C                   EVAL      W#FLD1 = C#CUSC
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    New customer may not exist if rename
      *                    --------------------------------
     C                   IF        P4ACTC = '21'                                B01
      *
     C                   EXSR      SRCUS1
     C     K4NCUS        SETLL     OMCUSL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEL     'OMQA704'     K4MSID
     C                   MOVEL     'E'           K4MSTP
     C                   MOVEA     K4CUSC        MC(1)
     C                   MOVEA     MC            K4MSGD
     C                   EVAL      W#FLD1 = C#NCUS
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
      *
     C                   IF        K4NCUS = *BLANKS                             B02
     C                   MOVEL     'OMQA003'     K4MSID
     C                   MOVEL     'E'           K4MSTP
     C                   MOVEA     VAR(1)        MC(1)
     C                   MOVEA     C#NCUS        MC(33)
     C                   MOVEA     MC            K4MSGD
     C                   EVAL      W#FLD1 = C#NCUS
     C                   EXSR      SRERR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *                    --------------------------------
      *                    No calls connected if delete
      *                    --------------------------------
     C                   IF        P4ACTC = '40'                                B01
     C                   EXSR      SRCCA3
     C                   EXSR      SRCCO1
     C     K4CUSC        SETLL     OMCCAL3
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF
     C     K4CUSC        SETLL     OMCCOL1
     C                   EVAL      *IN99 = %EQUAL
     C                   ENDIF
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVEL     'OMQA706'     K4MSID
     C                   MOVEL     'E'           K4MSTP
     C                   MOVEA     K4CUSC        MC(1)
     C                   MOVEA     MC            K4MSGD
     C                   EVAL      W#FLD1 = C#CUSC
     C                   EXSR      SRERR
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
     C                   MOVEL     'OMQ6003'     K4MSID
     C                   MOVEL     'E'           K4MSTP
     C                   MOVEA     P4ACTC        MC(1)
     C                   MOVEA     D#PGM         MC(3)
     C                   MOVEA     MC            K4MSGD
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
      *          Send message when K4MSQ is filled
      *          ------------------------------------------
     C                   IF        K4MSQ <> *BLANKS                             B01
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
     C                   EVAL      P#MIDC = K4MSID
     C                   EVAL      P#MSGQ = C#API
     C                   EVAL      P#MDL# = 125
     C                   EVAL      P#MSTC = '*DIAG'
     C                   EVAL      P#PMQC = K4MSQ
     C                   EVAL      P#PSC# = 0
     C                   CALLP     QMHSNDPM ( P#MIDC : P#MSGQ : K4MSGD :
     C                             P#MDL# : P#MSTC : P#PMQC : P#PSC# :
     C                             P#MSKC : P#ERR )
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do only when an error occured
      *          ------------------------------------------
     C                   IF        K4MSTP = 'E'                                 B01
      *
      *          ------------------------------------------
      *          Unlock a possible locked record
      *          ------------------------------------------
     C                   UNLOCK    OMCUSL1                              99
      *
      *          ------------------------------------------
      *          Count the error number
      *          ------------------------------------------
     C                   ADD       1             W#ERR
      *
      *          ------------------------------------------
      *          Execute TTLR when no message queue is used
      *          ------------------------------------------
     C                   IF        K4MSQ = *BLANKS                              B02
     C                             OR I#QUIT = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Reset the message type
      *          ------------------------------------------
     C                   MOVE      *BLANK        K4MSTP
     C                   MOVE      *BLANK        K4MSGD
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
      *          Open OMCUSL1
      *          ------------------------------------------
     C     SRCUS1        BEGSR
     C                   IF        I#CUS1 <> *ON                                B01
     C                   EVAL      I#CUS1 = *ON
     C                   OPEN      OMCUSL1
     C                   ENDIF                                                  E01
     C                   ENDSR
      *
      *          ------------------------------------------
      *          Open OMCCAL3
      *          ------------------------------------------
     C     SRCCA3        BEGSR
     C                   IF        I#CCA3 <> *ON                                B01
     C                   EVAL      I#CCA3 = *ON
     C                   OPEN      OMCCAL3
     C                   ENDIF                                                  E01
     C                   ENDSR
      *
      *          ------------------------------------------
      *          Open OMCCOL1
      *          ------------------------------------------
     C     SRCCO1        BEGSR
     C                   IF        I#CCO1 <> *ON                                B01
     C                   EVAL      I#CCO1 = *ON
     C                   OPEN      OMCCOL1
     C                   ENDIF                                                  E01
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIO   - I/O to OMCUSR.                                     *
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
     C                   IF        K4UPB = *ON                                  B01
     C                   MOVEL     DSCUSR        P4BUF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Must the database be updated.
      *          ------------------------------------------
     C                   IF        K4UPD = *ON                                  B01
      *
      *          ------------------------------------------
      *          Must the record be written.
      *          ------------------------------------------
     C                   SELECT                                                 B02
     C                   WHEN      P4ACTC = '60'                                X02
     C                   WRITE     OMCUSR
      *
      *          ------------------------------------------
      *          Must the record be removed.
      *          ------------------------------------------
     C                   WHEN      P4ACTC = '40'                                X02
     C                   DELETE    OMCUSR
      *
      *          ------------------------------------------
      *          Action 21 - rename
      *          ------------------------------------------
     C                   WHEN      P4ACTC = '21'                                X02
      *
      *                    --------------------------------
      *                    Send renaming message
      *                    --------------------------------
     C                   EVAL      MSGID = 'RNM0003'
     C                   EVAL      MSGF = 'OMHMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMX047C ( MSGID : MSGF : MSGLIB : MSGDTA
     C                             )
      *
      *                    --------------------------------
      *                    Rename
      *                    --------------------------------
     C/EXEC SQL
     C+                    WHENEVER SQLERROR GO TO SQLDMP
     C/END-EXEC
      *          --------- Save the rename command --------
     C/EXEC SQL
     C+                    INSERT INTO OMCRNL1
     C+                            (RNCUSC,  RNNCUS,  RNNNAM, RNCOMP)
     C+                     VALUES(:K4CUSC, :K4NCUS, ' ', '0')
     C/END-EXEC
      *
      *          --------- Update Customer Calls ----------
     C/EXEC SQL
     C+                    UPDATE OMCCA
     C+                    SET   CACUSC = :K4NCUS
     C+                    WHERE CACUSC = :K4CUSC
     C/END-EXEC
      *          --------- Update Customer Contacts -------
     C/EXEC SQL
     C+                    UPDATE OMCCO
     C+                    SET   COCUSC = :K4NCUS
     C+                    WHERE COCUSC = :K4CUSC
     C/END-EXEC
      *          --------- Update Helpdesk Mail Headers ---
     C/EXEC SQL
     C+                    UPDATE OMHMH
     C+                    SET   MHCUSC = :K4NCUS
     C+                    WHERE MHCUSC = :K4CUSC
     C/END-EXEC
      *          --------- Update Mail Extensions ---------
     C/EXEC SQL
     C+                    UPDATE OMMCX
     C+                    SET   CXCUSC = :K4NCUS
     C+                    WHERE CXCUSC = :K4CUSC
     C/END-EXEC
      *          --------- Update Person Mail Extensions --
     C/EXEC SQL
     C+                    UPDATE OMMPX
     C+                    SET   PXCUSC = :K4NCUS
     C+                    WHERE PXCUSC = :K4CUSC
     C/END-EXEC
      *          --------- Update Mail Templates ----------
     C/EXEC SQL
     C+                    UPDATE OMTEM
     C+                    SET   EMCUSC = :K4NCUS
     C+                    WHERE EMCUSC = :K4CUSC
     C/END-EXEC
      *          --------- Update Customers ---------------
     C/EXEC SQL
     C+                    UPDATE OMCUS
     C+                    SET   USCUSC = :K4NCUS
     C+                    WHERE USCUSC = :K4CUSC
     C/END-EXEC
      *
     C/EXEC SQL
     C+                    DELETE FROM OMCRNL1
     C+                    WHERE RNCUSC = :K4CUSC
     C/END-EXEC
      *
      *          ------------------------------------------
      *          Normal exit
      *          ------------------------------------------
     C                   LEAVESR
      *
      *          ------------------------------------------
      *          SQL Error
      *          ------------------------------------------
     C     SQLDMP        TAG
     C                   EXSR      *PSSR
      *
      *          ------------------------------------------
      *          Must the record be updated
      *          ------------------------------------------
     C                   OTHER                                                  X02
     C                   UPDATE    OMCUSR
     C                   ENDSL                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C     IO99          ENDSR
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
     C                   MOVEL     'OMQ500B'     K4MSID
     C                   MOVEL     'E'           K4MSTP
     C                   MOVEA     D#PGM         MC(1)
     C                   MOVEA     P4INTL        MC(11)
     C                   MOVEA     MC            K4MSGD
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
      *          Parameter list for OMX047C (Send Status Message)
      *          ------------------------------------------
     C     $X047C        PLIST
     C                   PARM                    MSGID
     C                   PARM      'OMHMSG'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGDTA
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
     C                   WHEN      E#FILE = 'OMCUSL1 '                          X02
     C                   MOVEL     CUSL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMCCOL1 '                          X02
     C                   MOVEL     CCOL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMCCAL3 '                          X02
     C                   MOVEL     CCAL3         P#FILE
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
     C                   MOVE      'E'           K4MSTP
     C                   MOVE      'OMQ5012'     K4MSID
     C                   MOVEL     K4ERR         P4ERR
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
     C                   MOVEL     K4ERR         P4ERR
     C                   MOVEA     ERR           K4FLDA
     C                   MOVEL     K4FLD         P4FLD
     C                   IF        K4UPB <> *OFF                                B02
     C                   MOVEL     K4BUF         P4BUF
     C                   MOVEL     K4PRC         P4PRC
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          When multiple messages are send, fill 'E'
      *          ------------------------------------------
     C                   IF        W#ERR > *ZERO                                B02
     C                             AND K4MSQ <> *BLANK
     C                   MOVEL     *BLANK        K4MSID
     C                   MOVEL     'E'           K4MSTP
     C                   MOVE      *BLANKS       K4MSGD
     C                   MOVEL     K4ERR         P4ERR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          When no parm or K4EOP = '1' then close
      *          ------------------------------------------
     C                   IF        D#PARM = *ZERO                               B01
     C                             OR K4EOP = '1'
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
     C                   FOR       MS = 1 TO 1                                  B02
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
