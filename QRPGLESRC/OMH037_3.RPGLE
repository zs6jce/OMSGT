     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ------------------------------------------------------------- *
      * Description                                                   *
      * ------------------------------------------------------------- *
      *                                                               *
      * Program ....: OMH037_3                                        *
      * Function ...: Process distribution                            *
      * Author .....: Remain Software                               *
      * Parameters .: P#DSID - Distribution identifier                *
      *               P#STAT - Status                                 *
      *                                                               *
      * This program processes a distribution. The distribution is    *
      * read and the incident is changed or added.                    *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * File specifications                                           *
      * ------------------------------------------------------------- *
     FOMHMHL1   UF   E           K DISK
     F                                     INFDS(HMHL1)
     FOMHMTL1   UF   E           K DISK
     F                                     INFDS(HMTL1)
     FOMHMQL1   UF   E           K DISK
     F                                     INFDS(HMQL1)
     FOMQLAL1   IF   E           K DISK
     F                                     INFDS(QLAL1)
     FOMHMOL1   UF   E           K DISK
     F                                     INFDS(HMOL1)
     FOMINCL1   IF   E           K DISK
     F                                     INFDS(INCL1)
     FOMTIDL1   UF A E           K DISK
     F                                     INFDS(TIDL1)
     FOMTIHL1   UF A E           K DISK
     F                                     INFDS(TIHL1)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension spec's                                             *
      *                                                              *
      * QUE   Used to store question numbers                         *
      * ANS   Used to store answers                                  *
      * QLS   Used to store question lists                           *
      * QSQ   Used to store question seq in question list            *
      * ------------------------------------------------------------ *
      *COPY QCPYSRC,OMSREFREN
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Input specifications                                          *
      * ------------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Field reference file used to define fields
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          File information data structure
      *          ------------------------------------------
     D HMHL1           DS            57
     D HMQL1           DS            57
     D QLAL1           DS            57
     D HMOL1           DS            57
     D HMTL1           DS            57
     D INCL1           DS            57
     D TIDL1           DS            57
     D TIHL1           DS            57
      *
      *          ------------------------------------------
      *          Question & answer arrays.
      *          ------------------------------------------
     D P#QUE           DS
     D  QUE                    1   5000  0 INZ(0)
     D                                     DIM(1000)
     D P#ANS           DS
     D  ANS                    1   3000  0 INZ(0)
     D                                     DIM(1000)
     D P#QLS           DS
     D  QLS                    1   5000  0 INZ(0)
     D                                     DIM(1000)
     D P#QSQ           DS
     D  QSQ                    1   3000  0 INZ(0)
     D                                     DIM(1000)
      *
      *          ------------------------------------------
      *          W3 file
      *          ------------------------------------------
     D P#W3          E DS                  EXTNAME(OMH036W3)
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  D#PARM           *PARMS
     D  PGMQ             *PROC
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D                 DS
     D D#DATE                         7  0
     D  D#CC                          1  0 OVERLAY(D#DATE)
     D  D#YY                          2  0 OVERLAY(D#DATE:2)
     D  D#MM                          2  0 OVERLAY(D#DATE:4)
     D  D#DD                          2  0 OVERLAY(D#DATE:6)
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
      *          Copy member for OMQCHGIN - Change incident
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQCHGINDS
      *
      *          ------------------------------------------
      *          Copy member for OMQCHGCC - Change call
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQCHGCCDS
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#NEW           C                   CONST('*NEW ')
     D C#PROC          C                   CONST('*PROC')
     D C#SAME          C                   CONST('*SAME    ')
     D C#GEN           C                   CONST('*GEN      ')
     D C#INTL          C                   CONST('V3R0M0  ')
      * Prototype for OMH037_3
     D OMH037_3        PR
     D P#DSID_                             LIKE(P#DSID)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH901C'
     D OMH901C         PR                  EXTPGM('OMH901C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGDTA_                             LIKE(MSGDTA)
      *
      *          ------------------------------------------
      *          Parameter list for OMQCHGIN
      * Prototype for 'OMH0072'
     D OMH0072         PR                  EXTPGM('OMH0072')
     D MHHEDC_                             LIKE(MHHEDC)
     D I4INC#_                             LIKE(I4INC#)
     D I4ACTC_                             LIKE(I4ACTC)
     D P#QUE_                              LIKEDS(P#QUE)
     D P#ANS_                              LIKEDS(P#ANS)
     D P#QLS_                              LIKEDS(P#QLS)
     D P#QSQ_                              LIKEDS(P#QSQ)
     D I4SEV1_                             LIKE(I4SEV1)
     D I4SEV2_                             LIKE(I4SEV2)
     D I4SEV3_                             LIKE(I4SEV3)
     D I4STRC_                             LIKE(I4STRC)
     D I4RES#_                             LIKE(I4RES#)
     D I4PRI#_                             LIKE(I4PRI#)
     D I4ITPC_                             LIKE(I4ITPC)
     D I4USF1_                             LIKE(I4USF1)
     D I4USF2_                             LIKE(I4USF2)
     D I4USF3_                             LIKE(I4USF3)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          OMINCL1 - Incident file
      * Prototype for 'OMQCHGIN'
     D OMQCHGIN        PR                  EXTPGM('OMQCHGIN')
      * Prototype for 'OMQCHGCC'
     D OMQCHGCC        PR                  EXTPGM('OMQCHGCC')
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
     D OMH037_3        PI
     D P#DSID                        20
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D EROR01          S                   LIKE(INDI)
     D I#CHCC          S                   LIKE(INDI)
     D I#CHIN          S                   LIKE(INDI)
     D I#PSSR          S              1
     D IQ              S              5  0
     D MSGDTA          S            256
     D MSGID           S              7
     D MSGLIB          S             10
     D P#DSPI          S                   LIKE(INDI)
     D P#FILE          S             57
     D P#MSTA          S             40
      *----------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *----------------------------------------------------------------------
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Main line                                                   *
      * ------------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Entry parameters
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Close if no parameters passed.
      *          ------------------------------------------
     C                   IF        D#PARM = *ZEROS                              B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do the processing routine
      *          ------------------------------------------
     C                   EXSR      SRVERW
      *
      *          ------------------------------------------
      *          Return to caller.
      *          ------------------------------------------
     C                   RETURN
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
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRVERW - Processing                                           *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Read the first header
      *          ------------------------------------------
     C                   IF        P#DSID = *BLANKS                             B01
     C     *ALL'9'       SETLL     OMHMHL1
     C                   READ(N)   OMHMHL1
     C                   EVAL      *IN99 = %EOF
     C                   ELSE                                                   X01
     C     P#DSID        CHAIN(N)  OMHMHL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF                                                  E01
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          If process flag set.
      *          ------------------------------------------
     C                   IF        MHSTAT = C#PROC                              B02
     C                             OR P#DSID <> *BLANKS
      *
      *          ------------------------------------------
      *          Update old incident or create a new one.
      *          ------------------------------------------
     C                   IF        MHINC# <> *BLANKS                            B03
     C                   EXSR      SRUPD
     C                   ELSE                                                   X03
     C                   EXSR      SRNEW
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Process the text block (if any)
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B03
     C                   EXSR      SRTXT
      *
      *          ------------------------------------------
      *          Delete the distribution if all went fine
      *          ------------------------------------------
     C     MHDSID        DELETE    OMHMQL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B04
     C     MHDSID        DELETE    OMHMQL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDDO                                                  E04
      *
     C     MHDSID        DELETE    OMHMTL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B04
     C     MHDSID        DELETE    OMHMTL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDDO                                                  E04
      *
     C     MHDSID        DELETE    OMHMOL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B04
     C     MHDSID        DELETE    OMHMOL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDDO                                                  E04
      *
     C     MHDSID        DELETE    OMHMHL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B04
     C     MHDSID        DELETE    OMHMHL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDDO                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read the next header
      *          ------------------------------------------
     C                   IF        P#DSID = *BLANKS                             B02
     C                   READ(N)   OMHMHL1
     C                   EVAL      *IN99 = %EOF
     C                   ELSE                                                   X02
     C                   EVAL      *IN99 = *ON
     C                   ENDIF                                                  E02
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRUPD  - Update the existing incident                         *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRUPD         BEGSR
      *
      *          ------------------------------------------
      *          Init
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
      *
      *          ------------------------------------------
      *          Get the incident.
      *          ------------------------------------------
     C     $NC1K1        CHAIN     OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Check the status route
      *          ------------------------------------------
     C                   IF        MHSTRC = *BLANKS                             B01
     C                   MOVE      NCSTRC        MHSTRC
     C                   Z-ADD     NCRES#        MHRES#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill all fields in the buffer
      *          ------------------------------------------
     C                   MOVE      MHHEDC        I4HEDC
     C                   MOVE      MHINC#        I4INC#
     C                   MOVEL(P)  C#SAME        I4CTN#
     C                   MOVEL     C#SAME        I4AIN#
     C                   MOVE      *ZEROS        I4INDT
     C                   MOVE      *ZEROS        I4INTM
     C                   MOVE      *ZEROS        I4RCDT
     C                   MOVE      *ZEROS        I4RCTM
     C                   MOVEL(P)  C#SAME        I4STRC
     C                   EVAL      I4RES# = *ZEROS
     C                   MOVE      '0'           I4SUL#
     C                   MOVE      *BLANKS       I4STAT
     C                   MOVE      *ZEROS        I4SRDT
     C                   MOVE      *ZEROS        I4SRTM
     C                   MOVEL(P)  C#SAME        I4SEV1
     C                   MOVEL(P)  C#SAME        I4SEV2
     C                   MOVEL(P)  C#SAME        I4SEV3
     C                   MOVE      *ZEROS        I4IEDT
     C                   MOVE      *ZEROS        I4IETM
     C                   MOVE      *BLANKS       I4AUID
     C                   MOVE      MHRUID        I4RUID
     C                   MOVEL     C#SAME        I4ITPC
     C                   MOVE      *ZEROS        I4CFL#
     C                   MOVE      *BLANKS       I4CFIC
     C                   MOVEL     C#SAME        I4USF1
     C                   MOVEL     C#SAME        I4USF2
     C                   MOVE      *ZEROS        I4USF3
      *
     C                   IF        MHSHID = *BLANKS                             B01
     C                   MOVEL     MHSUBJ        MHSHID
     C                   ENDIF                                                  E01
     C                   IF        MHSHID = *BLANKS                             B01
     C                   MOVEL     '*SAME'       MHSHID
     C                   ENDIF                                                  E01
     C                   MOVE      MHSHID        I4SHID
      *
      *          ------------------------------------------
      *          Fill action code
      *          ------------------------------------------
     C                   MOVE      '20'          I4ACTC
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
      *          ------------------------------------------
      *          Check the status route
      *          ------------------------------------------
     C                   IF        MHSTRC <> *BLANKS                            B01
     C                   MOVE      MHSTRC        I4STRC
     C                   Z-ADD     MHRES#        I4RES#
     C                   MOVE      '29'          I4ACTC
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
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Set error status
      *          ------------------------------------------
     C                   IF        I4MSTP = 'E'                                 B01
     C                   EVAL      P#STAT = C#TERM
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRNEW  - Create a new call & a incident.                      *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRNEW         BEGSR
      *
      *          ------------------------------------------
      *          Create a new call
      *          ------------------------------------------
     C                   CLEAR                   C4BUF
     C                   MOVE      MHHEDC        C4HEDC
     C                   MOVE      C#GEN         C4CTN#
     C                   MOVE      MHCUSC        C4CUSC
     C                   MOVE      MHCCTC        C4CCTC
     C                   Z-ADD     555555        C4INDT
     C                   Z-ADD     555555        C4INTM
     C                   Z-ADD     555555        C4RCTM
     C                   MOVE      MHRUID        C4USID
     C                   MOVE      MHCSRC        C4CSRC
     C                   MOVE      *BLANKS       C4USF1
     C                   MOVE      *BLANKS       C4USF2
     C                   EVAL      C4USF3 = *ZEROS
      *
     C                   CLEAR                   C4PRC
     C                   MOVE      *OFF          C4EOP
     C                   MOVE      *ON           C4UPD
     C                   MOVE      *ON           C4UPB
      *
     C                   MOVE      '60'          C4ACTC
      *
     C                   MOVE      PGMQ          C4MSQ
      *
      *          ------------------------------------------
      *          Call OMQCHGCC
      *          ------------------------------------------
     C                   CALL      'OMQCHGCC'    $CHGCC                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#CHCC = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#CHCC = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Exit if error.
      *          ------------------------------------------
     C                   IF        C4MSTP = 'E'                                 B01
     C                   EVAL      P#STAT = C#TERM
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Get the first question list
      *          ------------------------------------------
     C                   EVAL      QLS = *ZEROS
     C                   EVAL      ANS = *ZEROS
     C                   EVAL      QSQ = *ZEROS
     C                   EVAL      QUE = *ZEROS
      *
     C     MHDSID        CHAIN(N)  OMHMQL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   EVAL      IQ = 1
     C                   DOW       *IN99 = *OFF                                 B02
     C     $LA1K1        CHAIN     OMQLAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   Z-ADD     MQQLS#        QLS(IQ)
     C                   Z-ADD     MQANS#        ANS(IQ)
     C                   Z-ADD     MQQSQ#        QSQ(IQ)
     C                   Z-ADD     LAQUE#        QUE(IQ)
     C                   ADD       1             IQ
     C     MHDSID        READE     OMHMQL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Retrieve the Q&A attributes
      *          ------------------------------------------
     C                   MOVE      *BLANKS       I4INC#
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH0072 ( MHHEDC : I4INC# : I4ACTC :
     C                             P#QUE : P#ANS : P#QLS : P#QSQ : I4SEV1 :
     C                             I4SEV2 : I4SEV3 : I4STRC : I4RES# :
     C                             I4PRI# : I4ITPC : I4USF1 : I4USF2 :
     C                             I4USF3 : P#STAT )
      *
      *          ------------------------------------------
      *          Fill all fields in the buffer
      *          ------------------------------------------
     C                   MOVE      MHHEDC        I4HEDC
     C                   MOVE      C#GEN         I4INC#
     C                   MOVEL(P)  C4CTN#        I4CTN#
     C                   MOVE      *BLANKS       I4AIN#
     C                   MOVE      *ZEROS        I4INDT
     C                   MOVE      *ZEROS        I4INTM
     C                   MOVE      *ZEROS        I4RCDT
     C                   MOVE      *ZEROS        I4RCTM
     C                   MOVE      I4STRC        I4STRC
     C                   Z-ADD     I4RES#        I4RES#
     C                   MOVE      '0'           I4SUL#
     C                   MOVE      I4STAT        I4STAT
     C                   MOVE      *ZEROS        I4SRDT
     C                   MOVE      *ZEROS        I4SRTM
     C                   MOVE(P)   I4SEV1        I4SEV1
     C                   MOVE(P)   I4SEV2        I4SEV2
     C                   MOVE(P)   I4SEV3        I4SEV3
     C                   MOVE      *ZEROS        I4IEDT
     C                   MOVE      *ZEROS        I4IETM
     C                   MOVE      MHRUID        I4RUID
     C                   MOVEL(P)  C#SAME        I4AUID
     C                   MOVE      I4ITPC        I4ITPC
     C                   EVAL      I4CFL# = *ZEROS
     C                   MOVE      *BLANKS       I4CFIC
     C                   MOVE      I4USF1        I4USF1
     C                   MOVE      I4USF2        I4USF2
     C                   MOVE      I4USF3        I4USF3
      *
     C                   IF        MHSHID = *BLANKS                             B02
     C                   MOVEL     MHSUBJ        MHSHID
     C                   ENDIF                                                  E02
     C                   IF        MHSHID = *BLANKS                             B02
     C                   MOVEL     '*NONE'       MHSHID
     C                   ENDIF                                                  E02
     C                   MOVE      MHSHID        I4SHID
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
      *          Exit if error.
      *          ------------------------------------------
     C                   IF        I4MSTP = 'E'                                 B02
     C                   EVAL      P#STAT = C#TERM
      *
      *          ------------------------------------------
      *          Write the Q&A history
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH0072 ( MHHEDC : I4INC# : I4ACTC :
     C                             P#QUE : P#ANS : P#QLS : P#QSQ : I4SEV1 :
     C                             I4SEV2 : I4SEV3 : I4STRC : I4RES# :
     C                             I4PRI# : I4ITPC : I4USF1 : I4USF2 :
     C                             I4USF3 : P#STAT )
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRTXT  - Write the text.                                      *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRTXT         BEGSR
      *
      *          ------------------------------------------
      *          Determine the new text sequence
      *          ------------------------------------------
     C     $IH1K1        SETGT     OMTIHL1
     C     $IH1K1        READPE(N) OMTIHL1
     C                   EVAL      *IN99 = %EOF
     C                   IF        *IN99 = *ON                                  B01
     C                   Z-ADD     10            IHITH#
     C                   ELSE                                                   X01
     C                   ADD       10            IHITH#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the text
      *          ------------------------------------------
     C     MHDSID        CHAIN     OMHMTL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Write the header
      *          ------------------------------------------
     C                   MOVE      MHHEDC        IHHEDC
     C                   MOVE      I4INC#        IHINC#
     C                   Z-ADD     IHITH#        IHITH#
     C                   MOVE      MTITXT        IHITXT
     C                   IF        IHITXT < '0'                                 B02
     C                             OR IHITXT > '4'
     C                   MOVE      '1'           IHITXT
     C                   ENDIF                                                  E02
     C                   MOVE      D#DATE        IHINDT
     C                   TIME                    IHINTM
     C                   MOVE      MTSUL#        IHSUL#
     C                   IF        IHSUL# < '0'                                 B02
     C                             OR IHSUL# > '9'
     C                   MOVE      '0'           IHSUL#
     C                   ENDIF                                                  E02
     C                   MOVE(P)   MHRUID        IHUSID
     C                   MOVE      MTSITD        IHSITD
     C                   IF        IHSITD = *BLANKS                             B02
     C                   MOVE      I4SHID        IHSITD
     C                   ENDIF                                                  E02
     C                   WRITE     OMTIHR
      *
      *          ------------------------------------------
      *          Do while records
      *          ------------------------------------------
     C                   EVAL      IDSEQ# = *ZEROS
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Write the detail
      *          ------------------------------------------
     C                   MOVE      MHHEDC        IDHEDC
     C                   MOVE      I4INC#        IDINC#
     C                   MOVE      IHITH#        IDITH#
     C                   MOVE      IDITXT        IDITXT
     C                   ADD       10            IDSEQ#
     C                   MOVE      MTINCD        IDINCD
     C                   WRITE     OMTIDR
      *
      *          ------------------------------------------
      *          Get the next text record
      *          ------------------------------------------
     C     MHDSID        READE     OMHMTL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Remove blank text entries
      *          ------------------------------------------
     C     $ID1K1        SETGT     OMTIDL1
     C     $ID1K1        READPE    OMTIDL1
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF                                 B01
     C                   IF        IDINCD = *BLANKS                             B02
     C                   DELETE    OMTIDR
     C     $ID1K1        READPE    OMTIDL1
     C                   EVAL      *IN99 = %EOF
     C                   ELSE                                                   X02
     C                   LEAVE
     C                   ENDIF                                                  E02
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRTTLR - Exit processing                                      *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          Close OMQCHGIN
      *          ------------------------------------------
     C                   IF        *ON = I#CHIN                                 B01
     C                   CALLP     OMQCHGIN()
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Close OMQCHGCC
      *          ------------------------------------------
     C                   IF        *ON = I#CHCC                                 B01
     C                   CALLP     OMQCHGCC()
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Return
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * *PSSR  - Error subroutine                                     *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
     C     *PSSR         BEGSR
      *
      *          ------------------------------------------
      *          Do if error not in this routine
      *          ------------------------------------------
     C                   IF        I#PSSR <> *ON                                B01
     C                   EVAL      I#PSSR = *ON
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
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * *INZSR - Program initialization                               *
      *                                                               *
      * ------------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Close if no parameters passed.
      *          ------------------------------------------
     C                   IF        D#PARM = *ZEROS                              B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Explicit numeric field definitions
      *          ------------------------------------------
     C                   EVAL      IQ = *ZEROS
      *
      *          ------------------------------------------
      *          Other Workfields
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Indicators
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter definitions
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Save field definitions
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          API parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter lists
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (Send message)
      *          ------------------------------------------
     C     $X901C        PLIST
     C                   PARM                    MSGID
     C                   PARM      'OMHMSG'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGDTA
      *
      *          ------------------------------------------
      *          Parameter list for OMQCHGIN
      *          ------------------------------------------
     C     $CHGIN        PLIST
     C                   PARM      C#INTL        I4INTL
     C                   PARM                    I4ACTC
     C                   PARM                    I4BUF
     C                   PARM                    I4PRC
     C                   PARM                    I4ERR
     C                   PARM                    I4FLD
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
      *          Parameter list for OMH007 (Asking Questions)
      *          ------------------------------------------
     C     $H0072        PLIST
     C                   PARM                    MHHEDC
     C                   PARM                    I4INC#
     C                   PARM                    I4ACTC
     C                   PARM                    P#QUE
     C                   PARM                    P#ANS
     C                   PARM                    P#QLS
     C                   PARM                    P#QSQ
     C                   PARM                    I4SEV1
     C                   PARM                    I4SEV2
     C                   PARM                    I4SEV3
     C                   PARM                    I4STRC
     C                   PARM                    I4RES#
     C                   PARM                    I4PRI#
     C                   PARM                    I4ITPC
     C                   PARM                    I4USF1
     C                   PARM                    I4USF2
     C                   PARM                    I4USF3
     C                   PARM      C#NORM        P#STAT
      *
      *          ------------------------------------------
      *          OMINCL1 - Incident file
      *          ------------------------------------------
     C     $NC1K1        KLIST
     C                   KFLD                    MHHEDC
     C                   KFLD                    MHINC#
      *
      *          ------------------------------------------
      *          OMTIHL1 - Incident text header file
      *          ------------------------------------------
     C     $IH1K1        KLIST
     C                   KFLD                    MHHEDC
     C                   KFLD                    MHINC#
      *
      *          ------------------------------------------
      *          OMTIDL1 - Incident text detail file
      *          ------------------------------------------
     C     $ID1K1        KLIST
     C                   KFLD                    IHHEDC
     C                   KFLD                    IHINC#
     C                   KFLD                    IHITH#
      *
      *          ------------------------------------------
      *          Key list for OMQLAL1 (Question list answers)
      *          ------------------------------------------
     C     $LA1K1        KLIST
     C                   KFLD                    MHHEDC
     C                   KFLD                    MQQLS#
     C                   KFLD                    MQQSQ#
      *
      *          ------------------------------------------
      *          What date is it?
      *          ------------------------------------------
     C                   EVAL      D#YY = UYEAR
     C                   EVAL      D#MM = UMONTH
     C                   EVAL      D#DD = UDAY
     C                   IF        D#YY < 40                                    B01
     C                   EVAL      D#CC = 1
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
