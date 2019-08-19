     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMX122                                        *
      * Function ...: Select Answer                                 *
      * Author .....: Remain Software                               *
      * Parameters .: P#HEDC - Helpdesk code                        *
      *               P#QUE# - Question number                      *
      *               P#ANS# - Answer number                        *
      *               P#ANSC - Answer                               *
      *                                                             *
      * This program let the user select an answer                  *
      * Only one selection can be made.                             *
      *                                                             *
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMANSL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(ANSL1)
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Input pecifications                                         *
      * ----------------------------------------------------------- *
      *COPY QCPYSRC,OMSREFREN
      *
      *          ------------------------------------------
      *          The field reference file is used for field definitions
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          File information data structures in case of error.
      *          ------------------------------------------
     D ANSL1           DS            57
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  E#FILE               201    208
     D  D#QUE#               254    258  0
      *
      *          ------------------------------------------
      *          Buffer
      *          ------------------------------------------
     D P#BUF           DS          6144
      *
      *          ------------------------------------------
      *          Error buffer for API call.
      *          ------------------------------------------
     D P#ERR           DS
     D D#NRBP                         9B 0 INZ(116)
     D D#NRBA                         9B 0
     D D#MSID                         7
      *                                      16  16 RESERVED
     D D#MSGD                 17    266
      *
      *          ------------------------------------------
      *          Binary number definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFBINARY
      *
      *          ------------------------------------------
      *          Buffer for VR0101
      *          + Constant holding the length of this buffer
      *          ------------------------------------------
     D V#0101          DS
     D B1OPTI                         4B 0
     D B1ANS#                         3  0
     D B1ANSC                        80
     D L#VR11          C                   CONST(85)
      *
      *          ------------------------------------------
      *          Buffer for VR0102
      *          + Constant holding the length of this buffer
      *          ------------------------------------------
     D V#0102          DS
     D B2OPTI                         4B 0
     D L#VR12          C                   CONST(2)
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#AUTH          C                   CONST('*AUTH')
     D C#ALL           C                   CONST('*ALL ')
     D C#PNLQ          C                   CONST('OMX122D   *LIBL     ')
     D C#OPTI          C                   CONST('FMOPTI    ')
     D F12             C                   CONST(-8)
      * Prototype for OMX122
     D OMX122          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#QUE#_                             LIKE(P#QUE#)
     D P#ANS#_                             LIKE(P#ANS#)
     D P#ANSC_                             LIKE(P#ANSC)
      * Prototype for 'QUIADDLE'
     D QUIADDLE        PR                  EXTPGM('QUIADDLE')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#BUF_                              LIKEDS(P#BUF)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#VRNC_                             LIKE(P#VRNC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#OPTC_                             LIKE(P#OPTC)
     D P#LEHC_                             LIKE(P#LEHC)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'OMH901C '
     D OMH901C         PR                  EXTPGM('OMH901C')
     D D#MSID_                             LIKE(D#MSID)
     D MSGF_                               LIKE(MSGF)
     D OBJL_                               LIKE(OBJL)
     D D#MSGD_                             LIKE(D#MSGD)
      * Prototype for 'QUIADDPW'
     D QUIADDPW        PR                  EXTPGM('QUIADDPW')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#PWLC_                             LIKE(P#PWLC)
     D P#ROW#_                             LIKE(P#ROW#)
     D P#COL#_                             LIKE(P#COL#)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUICLOA'
     D QUICLOA         PR                  EXTPGM('QUICLOA')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#CLOC_                             LIKE(P#CLOC)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUIDLTL'
     D QUIDLTL         PR                  EXTPGM('QUIDLTL')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUIDSPP'
     D QUIDSPP         PR                  EXTPGM('QUIDSPP')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#FNR#_                             LIKE(P#FNR#)
     D P#PNLC_                             LIKE(P#PNLC)
     D P#RDOC_                             LIKE(P#RDOC)
     D P#ERR_                              LIKEDS(P#ERR)
     D P#USTC_                             LIKE(P#USTC)
     D P#PSC#_                             LIKE(P#PSC#)
     D P#PMQC_                             LIKE(P#PMQC)
     D P#MRKC_                             LIKE(P#MRKC)
     D P#CPOC_                             LIKE(P#CPOC)
     D P#LLEC_                             LIKE(P#LLEC)
     D P#ELEC_                             LIKE(P#ELEC)
     D P#WTM#_                             LIKE(P#WTM#)
      * Prototype for 'QUIGETLE'
     D QUIGETLE        PR                  EXTPGM('QUIGETLE')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#BUF_                              LIKEDS(P#BUF)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#VRNC_                             LIKE(P#VRNC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#POSC_                             LIKE(P#POSC)
     D P#COPC_                             LIKE(P#COPC)
     D P#SECC_                             LIKE(P#SECC)
     D P#SHNC_                             LIKE(P#SHNC)
     D P#XOPC_                             LIKE(P#XOPC)
     D P#LEHC_                             LIKE(P#LEHC)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUIOPNDA'
     D QUIOPNDA        PR                  EXTPGM('QUIOPNDA')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#PNLQ_                             LIKE(P#PNLQ)
     D P#APS#_                             LIKE(P#APS#)
     D P#ILV#_                             LIKE(P#ILV#)
     D P#FSHC_                             LIKE(P#FSHC)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUIPUTV'
     D QUIPUTV         PR                  EXTPGM('QUIPUTV')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#BUF_                              LIKEDS(P#BUF)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#VRNC_                             LIKE(P#VRNC)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUIRMVPW'
     D QUIRMVPW        PR                  EXTPGM('QUIRMVPW')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#ROPC_                             LIKE(P#ROPC)
     D P#ERR_                              LIKEDS(P#ERR)
      * Prototype for 'QUISETSC'
     D QUISETSC        PR                  EXTPGM('QUISETSC')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#ERR_                              LIKEDS(P#ERR)
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
     D P#ERR_                              LIKEDS(P#ERR)
      * *ENTRY Interface for Main Procedure
     D OMX122          PI
     D P#HEDC                         5
     D P#QUE#                         5  0
     D P#ANS#                         3  0
     D P#ANSC                       256
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D EROR01          S                   LIKE(INDI)
     D I#OPDA          S                   LIKE(INDI)
     D I#PSSR          S              1
     D IX              S              5  0
     D LOOP01          S                   LIKE(INDI)
     D P#FILE          S             57
     D P#MSTA          S             40
      *----------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *----------------------------------------------------------------------
      *
      *          ------------------------------------------
      *          Field rename because of UIM
      *          ------------------------------------------
      *OMANSR
      *             QUQLS#                          QUQLS#
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Main line                                                   *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Program parameters
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Do the processing routine
      *          ------------------------------------------
     C                   EXSR      SRVERW
      *
      *          ------------------------------------------
      *          Do the total last record routine
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRVERW - Main program processing routine                    *
      *                                                             *
      * This routine calls the first screen routine until the user  *
      * terminates the program by pressing command key F3.          *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Call screen 1 processing
      *          ------------------------------------------
     C                   EXSR      SRSC01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC01 - Screen 1 handling.                                  * /
      *                                                              * /
      * This routine handles screen 1 where the user must select     * /
      * a Employee code.                                             * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSC01        BEGSR
      *
      *          ------------------------------------------
      *          Init this screen
      *          ------------------------------------------
     C                   EXSR      SRIN01
      *
      *          ------------------------------------------
      *          Enter display loop
      *          ------------------------------------------
     C                   DOU       LOOP01 = '0'                                 B01
      *
      *          ------------------------------------------
      *          Display the confirmation window
      *          ------------------------------------------
     C                   MOVE      *BLANKS       P#PNLQ
     C                   MOVE      C#PNLQ        P#PNLQ
     C                   MOVEL     'PA01'        P#PNLC
     C                   MOVE      'Y'           P#RDOC
     C                   Z-ADD     -1            P#WTM#
     C                   MOVE      'N'           P#USTC
     C                   EXSR      SRDSPP
      *
      *          ------------------------------------------
      *          Set control indicators
      *          ------------------------------------------
     C                   EVAL      LOOP01 = *OFF
      *
      *          ------------------------------------------
      *          Call screen 2 processing if not F12.
      *          ------------------------------------------
     C                   IF        P#FNR# <> F12                                B02
     C                   EXSR      SRPR01
     C                   ENDIF                                                  E02
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Delete the list and remove the window.
      *          ------------------------------------------
     C                   EXSR      SRDLTL
     C                   EXSR      SRRPUW
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRIN01 - Intialize display 1                                 * /
      *                                                              * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Open the display application
      *          ------------------------------------------
     C                   MOVE      C#PNLQ        P#PNLQ
     C                   Z-ADD     -1            P#APS#
     C                   Z-ADD     2             P#ILV#
     C                   MOVE      'N'           P#FSHC
     C                   EXSR      SROPDA
     C                   EXSR      SRSCIM
     C                   EVAL      I#OPDA = *ON
      *
      *          ------------------------------------------
      *          Add a popup window
      *          ------------------------------------------
     C                   MOVEL     '*ROWCOL'     P#PWLC
     C                   Z-ADD     6             P#ROW#
     C                   Z-ADD     15            P#COL#
     C                   EXSR      SRAPUW
      *
      *          ------------------------------------------
      *          Fill the parameter list for SRADLE
      *          ------------------------------------------
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'VR0101'      P#VRNC
     C                   MOVEL     'LD0101'      P#LSTC
     C                   MOVE      'FRST'        P#OPTC
     C                   MOVE      *BLANKS       P#LEHC
      *
      *          ------------------------------------------
      *          Read all Employee
      *          ------------------------------------------
     C     $NS1K1        CHAIN     OMANSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   DOW       *IN99 = *OFF                                 B01
     C                   EVAL      B1OPTI = *ZEROS
     C                   Z-ADD     NSANS#        B1ANS#
     C                   MOVEL     NSANSC        B1ANSC
      *
      *          ------------------------------------------
      *          Add list entry
      *          ------------------------------------------
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRADLE
     C                   MOVE      'NEXT'        P#OPTC
      *
      *          ------------------------------------------
      *          Read next
      *          ------------------------------------------
     C     $NS1K1        READE     OMANSL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPR01 - Process screen 1.                                   * /
      *                                                              * /
      * This routine gets the first selected list entry.             * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          Set search value.
      *          ------------------------------------------
     C                   EVAL      B2OPTI = 1
     C                   MOVEL     V#0102        P#BUF
     C                   Z-ADD     L#VR12        P#LOB#
     C                   MOVEL     'VR0102'      P#VRNC
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Get a list entry.
      *          ------------------------------------------
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'VR0101'      P#VRNC
     C                   MOVEL     'LD0101  '    P#LSTC
     C                   MOVE      'FSLT'        P#POSC
     C                   MOVE      'Y'           P#COPC
     C                   MOVEL     'EQ'          P#SECC
     C                   MOVE      C#OPTI        P#SECC
     C                   MOVE      *BLANKS       P#SHNC
     C                   MOVE      'N'           P#XOPC
     C                   EXSR      SRGTLE
     C                   MOVEL     P#BUF         V#0101
      *
      *          ------------------------------------------
      *          If found then fill the parameters
      *          ------------------------------------------
     C                   IF        P#LEHC <> '****'                             B01
     C     $NS1K2        CHAIN     OMANSR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   Z-ADD     NSANS#        P#ANS#
     C                   MOVEL     NSANSC        P#ANSC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRADLE - Add list entry                                      * /
      *                                                              * /
      * This routine calls the add list entry API (QUIADDLE).        * /
      *                                                              * /
      * Input        P#HNDC  Handle code                             *
      *              P#BUF   Variable buffer                         *
      *              P#LOB#  Length of buffer                        *
      *              P#VRNC  Variable record name                    *
      *              P#LSTC  List code                               *
      *              P#OPTC  Option                                  *
      *                                                              *
      * Output     - P#LEHC  List entry handle                       *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRADLE        BEGSR
      *
      *          ------------------------------------------
      *          Add list multiple entry
      *          ------------------------------------------
     C                   CALLP     QUIADDLE ( P#HNDC : P#BUF : P#LOB# :
     C                             P#VRNC : P#LSTC : P#OPTC : P#LEHC :
     C                             P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NRBA <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      OBJL = '*LIBL'
     C                   CALLP     OMH901C  ( D#MSID : MSGF : OBJL : D#MSGD
     C                             )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRAPUW - Add pop-up window                                   * /
      *                                                              * /
      * This routine initiates the application for pop-up window     * /
      * display by calling the add pop-up window API.                * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *              P#PWLC  Popup window location                   *
      *              P#ROW#  Absolute row used                       *
      *              P#COL#  Absolute column used                    *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRAPUW        BEGSR
      *
      *          ------------------------------------------
      *          Add a pop up window
      *          ------------------------------------------
     C                   CALLP     QUIADDPW ( P#HNDC : P#PWLC : P#ROW# :
     C                             P#COL# : P#ERR )
      *
      *          ------------------------------------------
      *          Catch errors.
      *          ------------------------------------------
     C                   IF        D#NRBA <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      OBJL = '*LIBL'
     C                   CALLP     OMH901C  ( D#MSID : MSGF : OBJL : D#MSGD
     C                             )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRCLAP - Close application                                   * /
      *                                                              * /
      * This routine closes the application by calling the close     * /
      * application API.                                             * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      *                                                              *
      * This subroutine sets the following default parameters for    *
      * this API call.                                               *
      *                                                              *
      * P#CLOS = 'M' Perform normal close operation                  *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRCLAP        BEGSR
      *
      *          ------------------------------------------
      *          Close the display application
      *          ------------------------------------------
     C                   EVAL      P#CLOC = 'M'
     C                   CALLP     QUICLOA ( P#HNDC : P#CLOC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NRBA <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      OBJL = '*LIBL'
     C                   CALLP     OMH901C  ( D#MSID : MSGF : OBJL : D#MSGD
     C                             )
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRDLTL - Delete a list                                       * /
      *                                                              * /
      * This routine deletes a list by calling the QUIDLTL API.      * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *              P#LSTC  List code                               *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRDLTL        BEGSR
      *
      *          ------------------------------------------
      *          Close the display application
      *          ------------------------------------------
     C                   CALLP     QUIDLTL ( P#HNDC : P#LSTC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NRBA <> *ZEROS                             B01
     C                             AND D#MSID <> 'CPF6A91'
     C                             AND D#MSID <> 'CPF6A92'
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      OBJL = '*LIBL'
     C                   CALLP     OMH901C  ( D#MSID : MSGF : OBJL : D#MSGD
     C                             )
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X01
     C                   EVAL      D#MSID = *BLANKS
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRDSPP - Display panel.                                      * /
      *                                                              * /
      * This routine displays a panel by calling the display panel   * /
      * API.                                                         * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *              P#RDOC  Redisplay option  ('Y' or 'N')          *
      *              P#PNLC  Panel name                              *
      *              P#USTC  User task   'N'= New task               *
      *                                  'O'= Old task               *
      *              P#WTM#  Wait time   -1 = Indefinite wait time   *
      *                                   0 = Display panel & return *
      *                                  xx = Wait xx seconds        *
      *              P#MRKC Message key  'FRST'=Show all messages    *
      *                                  *BLANK=Don't show           *
      *                                                              *
      * Output     - P#FNR#  Requested function    0 = Enter         *
      *                                           -4 = Exit          *
      *                                           -8 = Cancel        *
      *                                          -10 = Prompt        *
      *                                          -20 = Exceed wait   *
      *                                           xx = User defined  *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * This subroutine sets the following default parameters for    *
      * this API call.                                               *
      *                                                              *
      * P#PSC# =  0         Use the message queue of program P#MSGQ  *
      * P#PMQQ =  '*CALLER' Program message queue to be used         *
      * P#CPOC =  'D'       Use default cursor positioning           *
      * P#LLEC =  'NONE'    Do not use list entry handler            *
      * P#ELEC =  'NONE'    Do not use error list entry              *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRDSPP        BEGSR
      *
      *          ------------------------------------------
      *          Display the panel
      *          ------------------------------------------
     C                   EVAL      P#PSC# = 0
     C                   EVAL      P#PMQC = '*CALLER'
     C                   EVAL      P#CPOC = 'D'
     C                   EVAL      P#LLEC = 'NONE'
     C                   EVAL      P#ELEC = 'NONE'
     C                   CALLP     QUIDSPP ( P#HNDC : P#FNR# : P#PNLC :
     C                             P#RDOC : P#ERR : P#USTC : P#PSC# :
     C                             P#PMQC : P#MRKC : P#CPOC : P#LLEC :
     C                             P#ELEC : P#WTM# )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NRBA <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      OBJL = '*LIBL'
     C                   CALLP     OMH901C  ( D#MSID : MSGF : OBJL : D#MSGD
     C                             )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGTLE - Get list entry                                      * /
      *                                                              * /
      * This routine calls the get list entry API (QUIGETLE) which   * /
      * gets an entry from the list and is used for positioning.     * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *              P#LOB#  Length of buffer                        *
      *              P#VRNC  Variable record name                    *
      *              P#LSTC  List name                               *
      *              P#POSC  Positioning option code                 *
      *              P#COPC  Copy option code                        *
      *              P#SECC  Selection criteria code                 *
      *              P#SHNC  Selection handle code                   *
      *              P#XOPC  Extend option code                      *
      *                                                              *
      * Output     - P#BUF   Buffer                                  *
      *              P#LEHC  List entry handle code                  *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * This subroutine sets the following default parameters for    *
      * this API call.                                               *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGTLE        BEGSR
      *
      *          ------------------------------------------
      *          Add list entry
      *          ------------------------------------------
     C                   CALLP     QUIGETLE ( P#HNDC : P#BUF : P#LOB# :
     C                             P#VRNC : P#LSTC : P#POSC : P#COPC :
     C                             P#SECC : P#SHNC : P#XOPC : P#LEHC :
     C                             P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NRBA <> *ZEROS                             B01
     C                   IF        D#MSID <> 'CPF6A98'                          B02
     C                             AND D#MSID <> 'CPF6A93'                      B02
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      OBJL = '*LIBL'
     C                   CALLP     OMH901C  ( D#MSID : MSGF : OBJL : D#MSGD
     C                             )
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X02
     C                   MOVE      '****'        P#LEHC
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
      * SROPDA - Open display application.                           * /
      *                                                              * /
      * This routine calls the open display application API QUIOPNDA * /
      *                                                              *
      * Input      - P#PNLQ  Qualified panelgroup name               *
      *              P#APS#  Application scope                       *
      *              P#ILV#  Interface level                         *
      *              P#FSHC  Full screen help code                   *
      *                                                              *
      * Output     - P#HNDC  Application handle                      *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * This subroutine sets the following default parameters for    *
      * this API call.                                               *
      *                                                              *
      * D#APPS = -1  Application scope is the calling program        *
      * D#ILVL =  2  Use interface level 2                           *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SROPDA        BEGSR
      *
      *          ------------------------------------------
      *          Open display application
      *          ------------------------------------------
     C                   CALLP     QUIOPNDA ( P#HNDC : P#PNLQ : P#APS# :
     C                             P#ILV# : P#FSHC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NRBA <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      OBJL = '*LIBL'
     C                   CALLP     OMH901C  ( D#MSID : MSGF : OBJL : D#MSGD
     C                             )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPUTV - Put dialog variable                                 * /
      *                                                              * /
      * This routine calls the put dialog variable API (QUIPUTV)     * /
      * which updates a variable record.                             * /
      *                                                              * /
      * Input      - P#HNDC  Application handle                      *
      *              P#BUF   Input buffer                            *
      *              P#LOB#  Length of buffer                        *
      *              P#VRNC  Variable record name                    *
      *                                                              *
      * Output     - *NONE                                           *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPUTV        BEGSR
      *
      *          ------------------------------------------
      *          Add list entry
      *          ------------------------------------------
     C                   CALLP     QUIPUTV ( P#HNDC : P#BUF : P#LOB# :
     C                             P#VRNC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NRBA <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      OBJL = '*LIBL'
     C                   CALLP     OMH901C  ( D#MSID : MSGF : OBJL : D#MSGD
     C                             )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRPUW - Remove pop-up window                                * /
      *                                                              * /
      * This routine removes the most recent added pop-up window.    * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *              P#ROPC  Remove option                           *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRPUW        BEGSR
      *
      *          ------------------------------------------
      *          Remove the window
      *          ------------------------------------------
     C                   EVAL      P#ROPC = 'L'
     C                   CALLP     QUIRMVPW ( P#HNDC : P#ROPC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NRBA <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      OBJL = '*LIBL'
     C                   CALLP     OMH901C  ( D#MSID : MSGF : OBJL : D#MSGD
     C                             )
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSCIM - Set screen image                                    * /
      *                                                              * /
      * This routine sets the screen image.                          * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSCIM        BEGSR
      *
      *          ------------------------------------------
      *          Remove the window
      *          ------------------------------------------
     C                   CALLP     QUISETSC ( P#HNDC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NRBA <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      OBJL = '*LIBL'
     C                   CALLP     OMH901C  ( D#MSID : MSGF : OBJL : D#MSGD
     C                             )
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
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      IX = *ZEROS
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Other API Parameter Definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      *          ------------------------------------------
     C     $901C2        PLIST
     C                   PARM                    D#MSID
     C                   PARM      'QCPFMSG'     MSGF
     C                   PARM      '*LIBL'       OBJL
     C                   PARM                    D#MSGD
     C                   EVAL      IX = *ZEROS
      *
      *          ------------------------------------------
      *          Keylist for answer file
      *          ------------------------------------------
     C     $NS1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#QUE#
      *
     C     $NS1K2        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#QUE#
     C                   KFLD                    B1ANS#
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
      *          Close application
      *          ------------------------------------------
     C                   IF        I#OPDA = *ON                                 B01
     C                   EXSR      SRCLAP
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
     C                   WHEN      E#FILE = 'OMANSL1 '                          X02
     C                   MOVEL     ANSL1         P#FILE
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
      *          Else exit immediately
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
     C                   EXSR      SRTTLR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
