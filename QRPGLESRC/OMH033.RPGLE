     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH033.                                       *
      * Function ...: Create HTML form.                             *
      * Author .....: Remain Software                               *
      * Module .....: HelpDesk                                      *
      * Parameters .: P#HEDC    -  HelpDesk                         *
      *               P#SRCF    -  Source file                      *
      *               P#SRCL    -  Source library                   *
      *               P#SRCM    -  Source member                    *
      *               P#CUDI    -  Customer pull down               *
      *               P#CODI    -  Contact pull down                *
      *               P#CONI    -  Contact name                     *
      *               P#COEI    -  Contact entry                    *
      *               P#HEDI    -  Employees indicator              *
      *               P#CSCI    -  Call source code                 *
      *                                                             *
      * This program creates a HTML page in the                     *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
     FQHTMLSRC  O  A F   92        DISK
     FOMCUSL1   IF   E           K DISK
     FOMCCOL1   IF   E           K DISK
     FOMQLAL1   IF   E           K DISK
     FOMHQUL1   IF   E           K DISK
     FOMANSL1   IF   E           K DISK
     FOMHDEL1   IF   E           K DISK
     FOMMHXL1   IF   E           K DISK
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension spec's                                             *
      *                                                              *
      * ------------------------------------------------------------ *
     D BO              S             80    DIM(180) CTDATA PERRCD(1)
     D QLQ             S              8    DIM(100)
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
      *          Source file fields
      *          ------------------------------------------
     D HTMLSR        E DS                  EXTNAME(QHTMLSRC)
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  D#PARM           *PARMS
      *
      *          ------------------------------------------
      *          Next option.
      *          ------------------------------------------
     D QLQKEY          DS
     D D#GLS#                         5  0
     D D#GQS#                         3  0
      *
      *          ------------------------------------------
      *          Marker
      *          ------------------------------------------
     D MARK            DS
     D D#TXR2                         9    INZ('<A NAME="')
     D D2QLS#                         5  0
     D D2QSQ#                         3  0
     D D#TXR3                        11    INZ('">Question')
     D D3QLS#                 30     34  0
     D D3QSQ#                         3  0
     D D#TXR4                         7    INZ('> </A>')
      *
      *          ------------------------------------------
      *          REF
      *          ------------------------------------------
     D REF             DS
     D D#TXC2                        10    INZ('<A HREF="#')
     D D4QLS#                         5  0
     D D4QSQ#                         3  0
     D D#TXC3                        11    INZ('">Continue')
     D D5QLS#                 31     35  0
     D D5QSQ#                         3  0
     D D#TXC4                         6    INZ(' </A>')
      *
      *          ------------------------------------------
      *          Customer option.
      *          ------------------------------------------
     D CUSOPT          DS
     D D#TX01                         8    INZ('<OPTION>')
     D D#CUSC                        10
      *
      *          ------------------------------------------
      *          Contact option
      *          ------------------------------------------
     D CONOPT          DS
     D CON001                         8    INZ('<OPTION>')
     D D#CCTC                        10
      *
      *          ------------------------------------------
      *          Employees
      *          ------------------------------------------
     D USIOPT          DS
     D USI001                         8    INZ('<OPTION>')
     D D2USID                        10
      *
      *          ------------------------------------------
      *          Call source code
      *          ------------------------------------------
     D CSC             DS
     D D#TX1Q                        13    INZ('<INPUT TYPE="')
     D D#TXAQ                        13    INZ('RADIO"    NAM')
     D D#TXBQ                         7    INZ('E="CSRC')
     D D#TXQQ                        12    INZ('" VALUE=" ">')
     D  D#CSRC                        1    OVERLAY(D#TXQQ:10)
      *
      *          ------------------------------------------
      *          Question
      *          ------------------------------------------
     D ANS             DS
     D D#TX12                        13    INZ('<INPUT TYPE="')
     D D#TXA2                        13    INZ('RADIO"    NAM')
     D D#TXB2                         3    INZ('E="')
     D D#QLS#                         5  0
     D D#QSQ#                         3  0
     D D#TX13                        14    INZ('" VALUE="000">')
     D  D#ANS#                        3  0 OVERLAY(D#TX13:10)
      *
      *          ------------------------------------------
      *          Hidden HelpDesk field
      *          ------------------------------------------
     D HED             DS
     D D#TXQ2                        13    INZ('<INPUT TYPE="')
     D D#TQQ2                        13    INZ('HIDDEN"   NAM')
     D D#TSQ2                         9    INZ('E="HEDC" ')
     D D#TTQ2                        13    INZ('VALUE="     "')
     D  D#HEDC                        5    OVERLAY(D#TTQ2:8)
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
      * Prototype for OMH033
     D OMH033          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#SRCF_                             LIKE(P#SRCF)
     D P#SRCL_                             LIKE(P#SRCL)
     D P#SRCM_                             LIKE(P#SRCM)
     D P#CUDI_                             LIKE(P#CUDI)
     D P#CODI_                             LIKE(P#CODI)
     D P#CONI_                             LIKE(P#CONI)
     D P#COEI_                             LIKE(P#COEI)
     D P#HEDI_                             LIKE(P#HEDI)
     D P#CSCI_                             LIKE(P#CSCI)
      * *ENTRY Interface for Main Procedure
     D OMH033          PI
     D P#HEDC                         5
     D P#SRCF                        10
     D P#SRCL                        10
     D P#SRCM                        10
     D P#CUDI                         1
     D P#CODI                         1
     D P#CONI                         1
     D P#COEI                         1
     D P#HEDI                         1
     D P#CSCI                         1
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D I#OK            S                   LIKE(INDI)
     D IP              S              5  0
     D IT              S              5  0
     D IX              S              5  0
     D IY              S              5  0
     D P#STAT          S                   LIKE(STAT)
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
      *          Get the parameters
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Do the processing.
      *          ------------------------------------------
     C                   IF        D#PARM > *ZEROS                              B01
     C                   EXSR      SRVERW
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Close
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
     C                   EXSR      SRTTLR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRVERW - Write the source member                            *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      SRCSEQ = *ZEROS
      *
      *          ------------------------------------------
      *          Get the helpdesk mail extensions
      *          ------------------------------------------
     C     P#HEDC        CHAIN     OMMHXL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON
     C                   MOVE      *ALL'?'       HXEMAC
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Do until options must be processed.
      *          ------------------------------------------
     C                   EVAL      IX = 1
     C                   DOW       1 = 1                                        B01
     C                   SELECT                                                 B02
      *
      *          ------------------------------------------
      *          Write the customer options
      *          ------------------------------------------
     C                   WHEN      BO(IX) = '##CUSC'                            X02
     C                             AND P#CUDI = *ON
     C                   EXSR      SRCUSC
      *
      *          ------------------------------------------
      *          Contact name
      *          ------------------------------------------
     C                   WHEN      BO(IX) = '##NAMD'                            X02
     C                   IF        P#CONI = *OFF                                B03
     C                   ADD       3             IX
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Contact e-mail
      *          ------------------------------------------
     C                   WHEN      BO(IX) = '##EMAC'                            X02
     C                   IF        P#COEI = *OFF                                B03
     C                   ADD       3             IX
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Write the customer options
      *          ------------------------------------------
     C                   WHEN      BO(IX) = '##CCTC'                            X02
     C                             AND P#CODI = *ON
     C                   EXSR      SRCCTC
      *
      *          ------------------------------------------
      *          Write the call source code
      *          ------------------------------------------
     C                   WHEN      BO(IX) = '##CSRC'                            X02
     C                             AND P#CSCI = *ON
     C                   EXSR      SRCSRC
      *
      *          ------------------------------------------
      *          Write the employees.
      *          ------------------------------------------
     C                   WHEN      BO(IX) = '##USID'                            X02
     C                             AND P#HEDI = *ON
     C                   EXSR      SRUSID
      *
      *          ------------------------------------------
      *          Write the questions
      *          ------------------------------------------
     C                   WHEN      BO(IX) = '##QUES'                            X02
     C                   EXSR      SRQUES
      *
      *          ------------------------------------------
      *          Write the questions
      *          ------------------------------------------
     C                   WHEN      BO(IX) = '##HEDC'                            X02
     C                   EVAL      D#HEDC = P#HEDC
     C                   MOVEL     HED           SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
      *          ------------------------------------------
      *          Write the email address
      *          ------------------------------------------
     C                   WHEN      BO(IX) = '##MATO'                            X02
     C                   MOVEL     HXEMAC        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
      *          ------------------------------------------
      *          Write the record.
      *          ------------------------------------------
     C                   OTHER                                                  X02
     C                   MOVEL     BO(IX)        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
     C                   ENDSL                                                  E02
      *
      *          ------------------------------------------
      *          Next line.
      *          ------------------------------------------
     C                   ADD       1             IX
     C                   IF        BO(IX) = '##END'                             B02
     C                   LEAVE
     C                   ENDIF                                                  E02
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRCUSC - Write the customer options                         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCUSC        BEGSR
      *
      *          ------------------------------------------
      *          Read all records.
      *          ------------------------------------------
     C                   READ      OMCUSL1
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF                                 B01
      *
     C                   MOVE      USCUSC        D#CUSC
     C                   MOVEL(P)  CUSOPT        SRCDTA
     C     SRCDTA        CAT       USNAMD:2      SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
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
      * SRCCTC - Write the contact options                          *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCCTC        BEGSR
      *
      *          ------------------------------------------
      *          Read all records.
      *          ------------------------------------------
     C                   READ      OMCCOL1
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF                                 B01
      *
     C                   MOVEL     COCCTC        D#CCTC
     C                   MOVEL(P)  CONOPT        SRCDTA
     C     SRCDTA        CAT       COCUSC:2      SRCDTA
     C     SRCDTA        CAT       CONAMD:2      SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   READ      OMCCOL1
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
      * SRUSID - Write the employees                                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRUSID        BEGSR
      *
      *          ------------------------------------------
      *          Read all records.
      *          ------------------------------------------
     C     P#HEDC        CHAIN     OMHDEL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B01
      *
     C                   MOVE      DEUSID        D2USID
     C                   MOVEL(P)  USIOPT        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C     P#HEDC        READE     OMHDEL1
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
      * SRCSRC - Call source code                                   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCSRC        BEGSR
      *
      *          ------------------------------------------
      *          By telephone
      *          ------------------------------------------
     C                   MOVEL(P)  '<BR>'        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   EVAL      D#CSRC = '0'
     C                   MOVEL(P)  CSC           SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   MOVEL(P)  'Phone'       SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
      *          ------------------------------------------
      *          By e-mail
      *          ------------------------------------------
     C                   MOVEL(P)  '<BR>'        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   EVAL      D#CSRC = '1'
     C                   MOVEL(P)  CSC           SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   MOVEL(P)  'E-mail'      SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
      *          ------------------------------------------
      *          By fax
      *          ------------------------------------------
     C                   MOVEL(P)  '<BR>'        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   EVAL      D#CSRC = '2'
     C                   MOVEL(P)  CSC           SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   MOVEL(P)  'Fax'         SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
      *          ------------------------------------------
      *          By WWW
      *          ------------------------------------------
     C                   MOVEL(P)  '<BR>'        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   EVAL      D#CSRC = '3'
     C                   MOVEL(P)  CSC           SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   MOVEL(P)  'The web'     SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
      *          ------------------------------------------
      *          By Notes
      *          ------------------------------------------
     C                   MOVEL(P)  '<BR>'        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   EVAL      D#CSRC = '4'
     C                   MOVEL(P)  CSC           SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   MOVEL(P)  'Notes'       SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRQUES - Write the questions and answers                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRQUES        BEGSR
      *
      *          ------------------------------------------
      *          Init
      *          ------------------------------------------
     C                   EVAL      IT = 100
     C                   EVAL      IP = 100
      *
      *          ------------------------------------------
      *          Read the first question list question.
      *          ------------------------------------------
     C     P#HEDC        CHAIN     OMQLAL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Save the first element.
      *          ------------------------------------------
     C                   Z-ADD     LAQLS#        D#GLS#
     C                   Z-ADD     LAQSQ#        D#GQS#
     C                   MOVE      QLQKEY        QLQ(IT)
      *
      *          ------------------------------------------
      *          Read all possible answers in this sequence.
      *          ------------------------------------------
     C     $LA1K1        CHAIN     OMQLAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Compose the question.
      *          ------------------------------------------
     C                   MOVEL(P)  '<BR>'        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   MOVEL(P)  '<HR>'        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   MOVEL(P)  '<BR>'        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   MOVEL(P)  '<B>'         SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   Z-ADD     LAQLS#        D2QLS#
     C                   Z-ADD     LAQSQ#        D2QSQ#
     C                   Z-ADD     LAQLS#        D3QLS#
     C                   Z-ADD     LAQSQ#        D3QSQ#
     C                   MOVEL(P)  MARK          SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C     $QU1K1        CHAIN     OMHQUL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL(P)  QUQUES        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   MOVEL(P)  '<BR>'        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   MOVEL(P)  '</B>'        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   MOVE      *BLANKS       SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
      *          ------------------------------------------
      *          Compose the answers.
      *          ------------------------------------------
     C     $LA1K1        CHAIN     OMQLAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B02
      *
     C                   MOVEL(P)  '<BR>'        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   Z-ADD     LAQLS#        D#QLS#
     C                   Z-ADD     LAQSQ#        D#QSQ#
     C                   Z-ADD     LAANS#        D#ANS#
     C                   MOVEL(P)  ANS           SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C     $NS1K1        CHAIN     OMANSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL(P)  NSANSC        SRCDTA
      *
     C                   Z-ADD     LAGLS#        D4QLS#
     C                   Z-ADD     LAGQS#        D4QSQ#
     C                   Z-ADD     LAGLS#        D5QLS#
     C                   Z-ADD     LAGQS#        D5QSQ#
     C     SRCDTA        CAT       REF:2         SRCDTA
      *
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   MOVEL(P)  '<BR>'        SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
     C                   MOVE      *BLANKS       SRCDTA
     C                   ADD       1             SRCSEQ
     C                   EXCEPT    HTML
      *
      *          ------------------------------------------
      *          Move the next sequence in the array.
      *          ------------------------------------------
     C                   IF        LAGLS# <> *ZERO                              B03
     C                   Z-ADD     LAGLS#        D#GLS#
     C                   Z-ADD     LAGQS#        D#GQS#
      *
     C                   EVAL      IY = IT
     C                   EVAL      IY = %LOOKUP(QLQKEY:QLQ:IY)
     C                   IF        IY > 0
     C                   EVAL      *IN99 = *ON
     C                   ELSE
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      IY = 1
     C                   ENDIF
      *
     C                   IF        *IN99 = *OFF                                 B04
     C                   SUB       1             IT
     C                   MOVE      QLQKEY        QLQ(IT)
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read the next possible answer in this sequence
      *          ------------------------------------------
     C     $LA1K1        READE     OMQLAL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do while elements.
      *          ------------------------------------------
     C                   IF        QLQ(IP) <> *BLANKS                           B02
     C                   MOVE      QLQ(IP)       QLQKEY
     C                   Z-ADD     D#GLS#        LAQLS#
     C                   Z-ADD     D#GQS#        LAQSQ#
     C                   SUB       1             IP
     C     $LA1K1        CHAIN     OMQLAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ELSE                                                   X02
     C                   LEAVE
     C                   ENDIF                                                  E02
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * *INZSR - Program initialisation. This routine also does the *
      *          processing.                                        *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Indicator definitions
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfields
      *          ------------------------------------------
     C                   EVAL      IT = *ZEROS
     C                   EVAL      IX = *ZEROS
     C                   EVAL      IP = *ZEROS
     C                   EVAL      IY = *ZEROS
      *
      *          ------------------------------------------
      *          Keylists.
      *          ------------------------------------------
     C     $LA1K1        KLIST
     C                   KFLD                    LAHEDC
     C                   KFLD                    LAQLS#
     C                   KFLD                    LAQSQ#
      *
     C     $QU1K1        KLIST
     C                   KFLD                    LAHEDC
     C                   KFLD                    LAQUE#
      *
     C     $NS1K1        KLIST
     C                   KFLD                    LAHEDC
     C                   KFLD                    LAQUE#
     C                   KFLD                    LAANS#
      *
      *          ------------------------------------------
      *          If passed without parameters.
      *          ------------------------------------------
     C                   IF        D#PARM = *ZERO                               B01
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
      * *PSSR  - Error handler                                      *
      *                                                             *
      * Quit without warning.                                       *
      *                                                             *
      * ----------------------------------------------------------- *
      **         *PSSR     BEGSR
      *
      *          ------------------------------------------
      *          Do if error not in this routine.
      *          ------------------------------------------
      **                   SETON                     LR
      **                   RETRN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
      **                   ENDSR
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
      *          If passed without parameters.
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Output specifications                                       *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Write the HTML source
      *          ------------------------------------------
     OQHTMLSRC  EADD         HTML
     O                       SRCSEQ               6
     O                       UDATE               12
     O                       SRCDTA              92
**  Body
<HTML>
<HEAD>
<TITLE>
Incident Registration Form
</TITLE>
</HEAD>

<P>
<B>
This form is generated by the HelpDesk Manager.
</B>
<BR>

<HR ALIGN="CENTER">
<BR>

<P>
<FORM
<FORM ACTION="mailto:
##MATO
?subject=helpdesk form"
ENCTYPE="text/plain"
METHOD="POST">

##HEDC

<TABLE>
<PRE>
Customer code or department: <SELECT NAME="CUSC">
<OPTION VALUE="">Select a code
##CUSC
</SELECT>
Contact code               : <SELECT NAME="CCTC">
<OPTION VALUE="">Select a code
##CCTC
</SELECT>
##NAMD
Your name                  : <INPUT TYPE="TEXT" NAME="NAMD"
  SIZE=50 VALUE="">
##EMAC
Your e-mail address        : <INPUT TYPE="TEXT" NAME="EMAC"
  SIZE=50 VALUE="">
HelpDesk Employee          : <SELECT NAME="AUID">
<OPTION VALUE="">Select a code
##USID
</SELECT>
</PRE>
</TABLE>
<BR>

<B>
How was the incident reported?
</B>
<BR>
##CSRC

<P>
<B>
Choose the correct answer for each option and follow the link behind
the answer you have given.
</B>
<BR>
##QUES
<BR>

<A NAME="00000000">
Short description
</A>
<BR>
<INPUT TYPE="TEXT" NAME="SHID" SIZE="50" VALUE="">
<BR>

<BR>
Long description
<BR>
<TEXTAREA NAME="INCD" ROWS="20" COLS="70">
</TEXTAREA>
<BR>

<INPUT TYPE = "SUBMIT" VALUE="Submit Incident">
<INPUT TYPE = "RESET"  VALUE="Clear form">
</FORM>
</P>

<HR ALIGN="CENTER">
<BR>

<CENTER>
<ADDRESS>
Comments to W.S. Jongman
<A HREF="mailto:wimj@remain.nl">
</A>
</ADDRESS>
</CENTER>

<CENTER>
<ADDRESS>
Our address is: Remain BV, Dukatenburg 82b, 3437 AE  Nieuwegein
(Netherlands). Phone 030 6097075.
</ADDRESS>
</CENTER>

<BR>
<BR>
<HR ALIGN="CENTER">
</BODY>
</HTML>
##END
