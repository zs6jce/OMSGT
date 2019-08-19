     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMX1372.                                      *
      * Function ...: Select Helpdesk.                              *
      * Author .....: Remain Software                               *
      * Parameters .: P#TXID - Text ID                              *
      *               P#INCD - Text description                     *
      *               P#ACTC - Action code                          *
      *                                                             *
      * This program let the user select an text code.              *
      * Only one selection can be made.                             *
      * Nothing is selected if the user presses F12.                *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used files                                                  *
      * ----------------------------------------------------------- *
      *                                                             *
      * OMTIPL1 - Predefined texts                                  *
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
      * LOOP01 - Indicator to handle first screen main loop.        *
      * EROR01 - Indicator to handle first screen error loop.       *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMTIPL1   IF   E           K DISK
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Input pecifications                                         *
      * ----------------------------------------------------------- *
      *
      *COPY QCPYSRC,OMSREFREN
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
     D  D#USID               254    263
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
     D B1TXID                         7
     D B1INCD                        30
     D L#VR11          C                   CONST(39)
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
     D C#PNLQ          C                   CONST('OMX137D2  *LIBL     ')
     D C#OPTI          C                   CONST('FMOPTI    ')
     D F12             C                   CONST(-8)
      * Prototype for OMX1372
     D OMX1372         PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#TXID_                             LIKE(P#TXID)
     D P#INCD_                             LIKE(P#INCD)
     D P#ACTC_                             LIKE(P#ACTC)
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
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
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
      * *ENTRY Interface for Main Procedure
     D OMX1372         PI
     D P#HEDC                         5
     D P#TXID                         7
     D P#INCD                        70
     D P#ACTC                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D EROR01          S                   LIKE(INDI)
     D I#OPDA          S                   LIKE(INDI)
     D IX              S              5  0
     D LOOP01          S                   LIKE(INDI)
     D W#TXID          S              7
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
      * an environment code.                                         * /
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
      *          Read all helpdesk functions
      *          ------------------------------------------
     C                   MOVE      *LOVAL        W#TXID
     C     $IP1K1        SETLL     OMTIPL1
     C     P#HEDC        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF                                 B01
     C                   IF        W#TXID <> IPTXID
     C                   MOVEL     IPTXID        W#TXID
     C                   EVAL      B1OPTI = *ZEROS
     C                   MOVE      IPTXID        B1TXID
     C                   MOVEL(P)  IPINCD        B1INCD
      *
      *          ------------------------------------------
      *          Add list entry
      *          ------------------------------------------
     C                   MOVEL     V#0101        P#BUF
     C                   EXSR      SRADLE
     C                   MOVE      'NEXT'        P#OPTC
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Read next
      *          ------------------------------------------
     C     P#HEDC        READE     OMTIPL1
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
     C                   EVAL      P#TXID = B1TXID
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
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      *          ------------------------------------------
     C     $IP1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    IPTXID
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
     C                   IF        I#OPDA = *ON
     C                   EXSR      SRCLAP
     C                   ENDIF
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
