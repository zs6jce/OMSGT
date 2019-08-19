     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH042                                        *
      * Function ...: Mail template maintenance                     *
      * Author .....: Remain Software                               *
      * Module .....: HelpDesk                                      *
      * Parameters .: P#HEDC    -  HelpDesk code                    *
      *               P#CUSC    -  Customer                         *
      *               P#CCTC    -  Contact                          *
      *               P#ACTC    -  Action                           *
      *               P#MTDI    -  Default indication               *
      *               P#STAT    -  Status                           *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMTEML1   UF A E           K DISK    INFSR(*PSSR)
     F                                     INFDS(TEML1)
     FOMMHXL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(MHXL1)
     FOMMSX     IF   E             DISK    INFSR(*PSSR)
     F                                     INFDS(MSX)
     FOMH042D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
     F                                     SFILE(OMH04201:RRN01)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension spec's                                             *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(12)
     D MSD             S              7    DIM(12) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
      *
     D NCA             S              2    DIM(99) CTDATA PERRCD(1)
     D CCA             S             11    DIM(99) CTDATA PERRCD(1)
      *
     D NUM             S              2    DIM(99)
     D COD             S             11    DIM(99)
     D SAV             S             11    DIM(99)
     D TXT             S             50    DIM(99)
     D TEM             S             10    DIM(99)
     D OVR             S             10    DIM(99)
     D NEW             S              1    DIM(99)
     D FLT             S              1    DIM(99)
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
      *          File information data structures
      *          ------------------------------------------
     D TEML1           DS            57
     D MHXL1           DS            57
     D MSX             DS            57
      *
      *          ------------------------------------------
      *          Screen information data structure
      *          ------------------------------------------
     D INFO            DS
     D  STATUS           *STATUS
     D  D#CRC#               378    379B 0
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  E#FILE               201    208
     D  PGMQ             *PROC
      *
      *          ------------------------------------------
      *          Helpdesk data structures
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
      *          Number for message formatting
      *          ------------------------------------------
     D                 DS
     D D#DEC#                         5  0
     D  D#DECA                        5    OVERLAY(D#DEC#)
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#NONE          C                   CONST('*NONE')
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#NOTF          C                   CONST('*NOT FOUND')
     D C#DFT           C                   CONST('*DFT      ')
     D C#SYS           C                   CONST('*SYSTEM   ')
     D C#CUSC          C                   CONST('*CUSTOMER ')
     D C#CCTC          C                   CONST('*CONTACT  ')
     D C#HEDC          C                   CONST('*HELPDESK ')
      * Prototype for OMH042
     D OMH042          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#CUSC_                             LIKE(P#CUSC)
     D P#CCTC_                             LIKE(P#CCTC)
     D P#ACTC_                             LIKE(P#ACTC)
     D P#MTDI_                             LIKE(P#MTDI)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH902C'
     D OMH902C         PR                  EXTPGM('OMH902C')
     D PGMQ_                               LIKE(PGMQ)
      *
      *          ------------------------------------------
      *          Parameter list for OMH942C (Retrieve member descr)
      * Prototype for 'OMH901C'
     D OMH901C         PR                  EXTPGM('OMH901C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGDTA_                             LIKE(MSGDTA)
      *
      *          ------------------------------------------
      *          Parameter list for OMH902C (Clear message queue)
      * Prototype for 'OMH909C'
     D OMH909C         PR                  EXTPGM('OMH909C')
     D P#OBJL_                             LIKE(P#OBJL)
     D P#OBJC_                             LIKE(P#OBJC)
     D P#MBRC_                             LIKE(P#MBRC)
     D OBJT_                               LIKE(OBJT)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH029 (Get info)
      * Prototype for 'OMH942C'
     D OMH942C         PR                  EXTPGM('OMH942C')
     D P#OBJC_                             LIKE(P#OBJC)
     D P#OBJL_                             LIKE(P#OBJL)
     D P#MBRC_                             LIKE(P#MBRC)
     D P#MBRA_                             LIKE(P#MBRA)
     D P#MBRD_                             LIKE(P#MBRD)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH904C (Get message data text)
      * Prototype for 'OMH029'
     D OMH029          PR                  EXTPGM('OMH029')
     D P#OPTI_                             LIKE(P#OPTI)
     D P2HEDC_                             LIKE(P2HEDC)
     D P#SUL#_                             LIKE(P#SUL#)
     D P#STAT_                             LIKE(P#STAT)
     D D#D7HD_                             LIKE(D#D7HD)
     D D#D8HD_                             LIKE(D#D8HD)
      *
      *          ------------------------------------------
      *          OMTEM Template file
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
      * Prototype for 'OMH904C'
     D OMH904C         PR                  EXTPGM('OMH904C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGTXT_                             LIKE(MSGTXT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH909C (Check member)
      * *ENTRY Interface for Main Procedure
     D OMH042          PI
     D P#HEDC                         5
     D P#CUSC                        10
     D P#CCTC                        10
     D P#ACTC                         2
     D P#MTDI                         1
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D EROR01          S                   LIKE(INDI)
     D EROR02          S                   LIKE(INDI)
     D I#PSSR          S              1
     D K#ACTC          S                   LIKE(ACTC)
     D K#CCTC          S                   LIKE(CCTC)
     D K#CUSC          S                   LIKE(CUSC)
     D K#HEDC          S                   LIKE(HEDC)
     D LOOP01          S                   LIKE(INDI)
     D LOOP02          S                   LIKE(INDI)
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#FILE          S             57
     D P#MBRA          S             10
     D P#MBRC          S             10
     D P#MBRD          S             50
     D P#MSTA          S             40
     D P#OBJC          S             10
     D P#OBJL          S             10
     D P#OPTI          S                   LIKE(INDI)
     D P#STRD          S                   LIKE(STRD)
     D P#SUL#          S                   LIKE(SUL#)
     D P1HEDC          S                   LIKE(HEDC)
     D P1STRC          S                   LIKE(STRC)
     D P1STRD          S                   LIKE(STRD)
     D P2HEDC          S                   LIKE(HEDC)
     D RRN01           S              5  0
     D S#HEDC          S                   LIKE(HEDC)
     D W               S              2  0
     D W#CRC#          S              5  0
     D W#FLTI          S                   LIKE(INDI)
     D X               S              2  0
     D Y               S              2  0
     D Z               S              2  0
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
      *          Do the processing routine if authorised
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      P#STAT = C#NORM
     C                   EXSR      SRVERW
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Do the total last record routine
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
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRVERW - Main program processing routine                    *
      *                                                             *
      * This routine calls the first screen routine until the field *
      * P#STAT is filled.                                           *
      *                                                             *
      * ----------------------------------------------------------- *
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
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC01 - Work with employees                                *
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
     C                   DOU       LOOP01 = '0'                                 B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR01 = '0'                                 B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH04203
      *
      *          ------------------------------------------
      *          Display the screen indicating: no templates
      *          ------------------------------------------
     C                   IF        *IN81 = *ON                                  B03
     C                             AND X = 99
     C                             OR P#MTDI < '0'
     C                             OR P#MTDI > '4'
     C                   EXFMT     OMH04204
     C                   EVAL      *IN40 = *OFF
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN90 = *ON
     C                   EXFMT     OMH04202
     C                   READC     OMH04201
     C                   EVAL      *IN99 = %EOF
     C                   IF        *IN99 = *OFF
     C                   EVAL      *IN40 = *ON
     C                   ENDIF
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          CMD-12 is CMD-3 is exit
      *          ------------------------------------------
     C                   IF        *INKL = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
     C                   IF        *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
     C                   IF        *IN26 = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   EVAL      W#CRC# = D#CRC#
      *
      *          ------------------------------------------
      *          Clear message queue
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Setof screen error indicators
      *          ------------------------------------------
     C                   IF        W#FLTI = '0'                                 B03
     C                   EVAL      EROR01 = *OFF
     C                   ENDIF                                                  E03
     C                   EVAL      *IN70 = *OFF
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      *IN72 = *OFF
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
      *          Edit
      *          ------------------------------------------
     C                   IF        *IN40 = *ON
     C                   EXSR      SRED01
     C                   ENDIF
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors or command key
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do processing
      *          ------------------------------------------
     C                   IF        *INKC = *OFF                                 B02
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
      * SRIN01 - Screen 1 initialisation                            *
      *                                                             *
      * This routine blanks the input fields on the screen.         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields
      *          ------------------------------------------
     C                   MOVEL     P#HEDC        FMHEDC
     C                   MOVEL     P#CUSC        FMCUSC
     C                   MOVEL     P#CCTC        FMCCTC
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
      * SRRF01 - Work with questions refresh                        *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      RRN01 = *ZEROS
     C                   EVAL      X = *ZEROS
     C                   EVAL      Z = *ZEROS
     C                   CLEAR                   NEW
     C                   CLEAR                   SAV
     C                   CLEAR                   TXT
     C                   CLEAR                   TEM
     C                   CLEAR                   OVR
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH04202
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Initialize the counter of the number of fills
      *          of each stage of the subfile-filling
      *          ------------------------------------------
     C                   EVAL      X = 99
     C                   MOVE      *LOVAL        K#ACTC
     C                   Z-ADD     1             CRRN01
     C                   EVAL      *IN90 = *ON
      *
      *          ------------------------------------------
      *          Specific view required?
      *          ------------------------------------------
     C                   IF        P#CCTC <> *BLANKS                            B01
     C                             AND P#MTDI = '4'
     C                             OR P#CCTC <> *BLANKS
     C                             AND P#MTDI = '1'
     C                             OR P#CCTC = *BLANKS
      *
      *          ------------------------------------------
      *          Fill in the system defaults
      *          ------------------------------------------
     C                   EVAL      K#HEDC = *BLANKS
     C                   EVAL      K#CUSC = *BLANKS
     C                   EVAL      K#CCTC = *BLANKS
      *
      *          ------------------------------------------
      *          Read first system record
      *          ------------------------------------------
     C     $EM1K4        SETLL     OMTEML1
     C     $EM1K3        READE     OMTEML1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Fill from file as long as possible, write
      *          ------------------------------------------
     C                   DOW       *IN99 <> *ON                                 B02
     C                   ADD       1             Z
     C                   SUB       1             X
     C                   EVAL      Y = 1
     C                   EVAL      Y = %LOOKUP(EMACTC:NUM:Y)
     C                   IF        Y > 0
     C                   EVAL      *IN98 = *ON
     C                   ELSE
     C                   EVAL      *IN98 = *OFF
     C                   EVAL      Y = 1
     C                   ENDIF
     C                   EVAL      SAV(Z) = COD(Y)
     C                   MOVEL     EMMATM        TEM(Z)
     C                   EVAL      NEW(Z) = '3'
     C                   EVAL      OVR(Z) = C#SYS
     C     $EM1K3        READE     OMTEML1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Specific view required?
      *          ------------------------------------------
     C                   IF        P#CCTC <> *BLANKS                            B01
     C                             AND P#MTDI = '2'
     C                             OR P#CCTC <> *BLANKS
     C                             AND P#MTDI = '1'
     C                             OR P#CCTC = *BLANKS
      *
      *          ------------------------------------------
      *          Fill in the customers defaults
      *          ------------------------------------------
     C                   IF        P#CUSC <> *BLANKS                            B02
     C                   EVAL      K#HEDC = *BLANKS
     C                   EVAL      K#CCTC = *BLANKS
     C                   EVAL      K#CUSC = P#CUSC
      *
      *          ------------------------------------------
      *          Read first template record for the customer
      *          ------------------------------------------
     C     $EM1K4        SETLL     OMTEML1
     C     $EM1K3        READE     OMTEML1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Fill from file as long as possible, write
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B03
     C                   SUB       1             X
     C                   EVAL      Y = 1
     C                   EVAL      W = 1
     C                   EVAL      Y = %LOOKUP(EMACTC:NUM:Y)
     C                   IF        Y > 0
     C                   EVAL      *IN98 = *ON
     C                   ELSE
     C                   EVAL      *IN98 = *OFF
     C                   EVAL      Y = 1
     C                   ENDIF
     C                   EVAL      W = %LOOKUP(COD(Y):SAV:W)
     C                   IF        W > 0
     C                   EVAL      *IN98 = *ON
     C                   ELSE
     C                   EVAL      *IN98 = *OFF
     C                   EVAL      W = 1
     C                   ENDIF
     C                   IF        *IN98 = *OFF                                 B04
     C                   ADD       1             Z
     C                   EVAL      SAV(Z) = COD(Y)
     C                   MOVEL     EMMATM        TEM(Z)
     C                   EVAL      OVR(Z) = C#CUSC
     C                   EVAL      NEW(Z) = '1'
     C                   ELSE                                                   X04
     C                   EVAL      SAV(W) = COD(Y)
     C                   MOVEL     EMMATM        TEM(W)
     C                   EVAL      OVR(W) = C#CUSC
     C                   EVAL      NEW(W) = '1'
     C                   ENDIF                                                  E04
     C     $EM1K3        READE     OMTEML1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Specific view required?
      *          ------------------------------------------
     C                   IF        P#CCTC <> *BLANKS                            B01
     C                             AND P#MTDI = '0'
     C                             OR P#CCTC <> *BLANKS
     C                             AND P#MTDI = '1'
     C                             OR P#CCTC = *BLANKS
      *
      *          ------------------------------------------
      *          Fill in the helpdesk or contact defaults
      *          ------------------------------------------
     C                   IF        P#HEDC <> *BLANKS                            B02
     C                             OR P#CUSC <> *BLANKS
     C                             OR P#CUSC <> *BLANKS
      *
     C                   EVAL      K#HEDC = P#HEDC
     C                   EVAL      K#CUSC = P#CUSC
     C                   EVAL      K#CCTC = P#CCTC
     C     $EM1K4        SETLL     OMTEML1
     C     $EM1K3        READE     OMTEML1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Fill from file as long as possible, write
      *          ------------------------------------------
     C                   DOW       *IN99 <> *ON                                 B03
     C                   EVAL      W = 1
     C                   EVAL      Y = 1
     C                   EVAL      Y = %LOOKUP(EMACTC:NUM:Y)
     C                   IF        Y > 0
     C                   EVAL      *IN98 = *ON
     C                   ELSE
     C                   EVAL      *IN98 = *OFF
     C                   EVAL      Y = 1
     C                   ENDIF
      *
     C                   EVAL      W = %LOOKUP(COD(Y):SAV:W)
     C                   IF        W > 0
     C                   EVAL      *IN98 = *ON
     C                   ELSE
     C                   EVAL      *IN98 = *OFF
     C                   EVAL      W = 1
     C                   ENDIF
     C                   IF        *IN98 = *OFF                                 B04
     C                   SUB       1             X
     C                   ADD       1             Z
     C                   EVAL      SAV(Z) = COD(Y)
     C                   MOVEL     EMMATM        TEM(Z)
     C                   EVAL      TXT(Z) = P#MBRD
     C                   IF        *IN52 = *ON
     C                   EVAL      NEW(Z) = '2'
     C                   ENDIF
     C                   IF        *IN50 = *ON
     C                   EVAL      NEW(Z) = '0'
     C                   ENDIF
     C                   IF        *IN52 = *ON
     C                   EVAL      OVR(Z) = C#HEDC
     C                   ENDIF
     C                   IF        *IN50 = *ON
     C                   EVAL      OVR(Z) = C#CCTC
     C                   ENDIF
     C                   ELSE                                                   X04
     C                   IF        *IN52 = *ON
     C                   EVAL      NEW(W) = '2'
     C                   ENDIF
     C                   IF        *IN50 = *ON
     C                   EVAL      NEW(W) = '0'
     C                   ENDIF
     C                   IF        *IN52 = *ON
     C                   EVAL      OVR(W) = C#HEDC
     C                   ENDIF
     C                   IF        *IN50 = *ON
     C                   EVAL      OVR(W) = C#CCTC
     C                   ENDIF
     C                   MOVEL     EMMATM        TEM(W)
     C                   EVAL      TXT(W) = P#MBRD
     C                   ENDIF                                                  E04
     C     $EM1K3        READE     OMTEML1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Write the remaing subfile records blank if not display
      *          or delete
      *          ------------------------------------------
     C                   IF        *IN82 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C                   EVAL      Y = 1
     C                   DOW       NUM(Y) <> *BLANKS                            B02
     C     COD(Y)        LOOKUP    SAV                                    98
     C                   IF        *IN98 <> *ON                                 B03
     C                   ADD       1             Z
     C                   EVAL      SAV(Z) = COD(Y)
     C                   EVAL      NEW(Z) = 'X'
     C                   EVAL      %SUBST(TEM(Z):1:5) = C#NONE
     C                   ENDIF                                                  E03
     C                   ADD       1             Y
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill and write the subfile
      *          ------------------------------------------
     C                   EVAL      Z = 1
     C                   DOW       NEW(Z) <> ' '                                B01
     C                   SUB       1             X
     C                   MOVEL     SAV(Z)        FMACPV
     C                   MOVEL     TEM(Z)        FMMATM
     C                   MOVEL     TXT(Z)        FMDESC
     C                   MOVEL     OVR(Z)        FMOVRD
      *
      *          ------------------------------------------
      *          Retrieve the member text
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMDESC
      *
     C                   IF        FMMATM <> C#NONE                             B02
      *
     C                   IF        HXMATL <> *BLANKS                            B03
     C                             AND P#HEDC <> *BLANKS
     C                   MOVEL     FMMATM        P#MBRC
     C                   MOVEL     HXMATF        P#OBJC
     C                   MOVEL     HXMATL        P#OBJL
     C                   EXSR      SRMBRD
     C                   ENDIF                                                  E03
      *
     C                   IF        SXMATL <> *BLANKS                            B03
     C                             AND HXMATL <> *BLANKS
     C                             AND P#HEDC <> *BLANKS
     C                             AND P#STAT <> C#NORM
     C                             OR SXMATL <> *BLANKS                         B03
     C                             AND HXMATL = *BLANKS
     C                   MOVEL     FMMATM        P#MBRC
     C                   MOVEL     SXMATF        P#OBJC
     C                   MOVEL     SXMATL        P#OBJL
     C                   EXSR      SRMBRD
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
     C                   ADD       1             Z
     C                   ADD       1             RRN01
     C                   WRITE     OMH04201
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Send the message if delete
      *          ------------------------------------------
     C                   IF        X <> 99                                      B01
     C                   IF        *IN80 = *ON
     C                             AND *IN81 = *ON
     C                             AND *IN82 = *OFF
     C                   EVAL      MSGID = 'DLT0001'
     C                   EXSR      SNDMSG
     C                   ENDIF
     C                   ENDIF                                                  E01
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
      * SRED01 - Work with Employee edit                            *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          Clear arrays
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Initialize counters
      *          ------------------------------------------
     C                   EVAL      X = 1
     C                   EVAL      Y = 1
      *
      *          ------------------------------------------
      *          Read whole subfile
      *          ------------------------------------------
     C                   DOW       NEW(X) <> ' '                                B01
     C     X             CHAIN     OMH04201
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Check wether action code exists, store
      *          ------------------------------------------
     C                   IF        FMMATM = *BLANKS                             B02
     C                   MOVEL     C#NONE        FMMATM
     C                   ELSE                                                   X02
      *
     C                   IF        FMMATM <> TEM(X)                             B03
     C                   IF        *IN50 = *ON
     C                   EVAL      NEW(X) = '0'
     C                   ENDIF
     C                   IF        *IN51 = *ON
     C                   EVAL      NEW(X) = '1'
     C                   ENDIF
     C                   IF        *IN52 = *ON
     C                   EVAL      NEW(X) = '2'
     C                   ENDIF
     C                   IF        *IN53 = *ON
     C                   EVAL      NEW(X) = '3'
     C                   ENDIF
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
     C                   IF        FMMATM = C#NONE                              B02
     C                   EVAL      NEW(X) = 'X'
     C                   ELSE                                                   X02
      *
     C                   IF        NEW(X) = 'X'                                 B03
     C                   IF        *IN50 = *ON
     C                   EVAL      NEW(X) = '0'
     C                   ENDIF
     C                   IF        *IN51 = *ON
     C                   EVAL      NEW(X) = '1'
     C                   ENDIF
     C                   IF        *IN52 = *ON
     C                   EVAL      NEW(X) = '2'
     C                   ENDIF
     C                   IF        *IN53 = *ON
     C                   EVAL      NEW(X) = '3'
     C                   ENDIF
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   MOVEL     FMMATM        TEM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      X = 1
     C                   EVAL      RRN01 = 1
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH04202
     C                   EVAL      *IN91 = *OFF
     C                   EVAL      W#FLTI = '0'
      *
      *          ------------------------------------------
      *          Write the rearranged way
      *          ------------------------------------------
     C                   DOW       NEW(X) <> ' '                                B01
     C                   MOVEL     SAV(X)        FMACPV
     C                   MOVEL     TEM(X)        FMMATM
      *
      *          ------------------------------------------
      *          Write the rearranged way
      *          ------------------------------------------
     C                   WRITE     OMH04201
     C                   ADD       1             X
     C                   ADD       1             RRN01
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR01 - Work with Status Process                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          If change then loop again
      *          ------------------------------------------
     C                   IF        *IN40 = *ON                                  B01
     C                   EVAL      LOOP01 = *ON
      *
      *          ------------------------------------------
      *          Write
      *          ------------------------------------------
     C                   EVAL      X = 1
     C                   IF        *IN82 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Delete
      *          ------------------------------------------
     C                   IF        *IN81 = *ON                                  B03
     C                   DOW       NEW(X) <> ' '                                B04
     C                   IF        NEW(X) = 'N'                                 B05
     C                   EVAL      Y = 1
     C                   EVAL      Y = %LOOKUP(SAV(X):COD:Y)
     C                   IF        Y > 0
     C                   EVAL      *IN99 = *ON
     C                   ELSE
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      Y = 1
     C                   ENDIF
     C                   EVAL      K#ACTC = NUM(Y)
     C     $EM1K4        CHAIN     OMTEMR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B06
     C     $EM1K4        DELETE    OMTEMR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
     C                   ADD       1             X
     C                   ENDDO                                                  E04
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Update
      *          ------------------------------------------
     C                   MOVEL     *LOVAL        K#ACTC
     C     $EM1K4        SETLL     OMTEMR
     C     $EM1K3        READE     OMTEMR
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 <> *ON                                 B04
     C                   DELETE    OMTEMR
     C     $EM1K3        READE     OMTEMR
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E04
     C                   DOW       NEW(X) <> ' '                                B04
     C                   IF        NEW(X) = '0'                                 B05
     C                             AND *IN50 = *ON
     C                             OR NEW(X) = '1'
     C                             AND *IN51 = *ON
     C                             OR NEW(X) = '2'
     C                             AND *IN52 = *ON
     C                             OR NEW(X) = '3'
     C                             AND *IN53 = *ON
     C                   EXSR      SRFILL
     C                   WRITE     OMTEMR
     C                   ENDIF                                                  E05
     C                   ADD       1             X
     C                   ENDDO                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Rebuild the subfile
      *          ------------------------------------------
     C                   EXSR      SRRF01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRFILL - Fill the record                                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRFILL        BEGSR
      *
      *          ------------------------------------------
      *          Fill
      *          ------------------------------------------
     C                   MOVEL     P#HEDC        EMHEDC
     C                   MOVEL     P#CUSC        EMCUSC
     C                   MOVEL     P#CCTC        EMCCTC
     C                   EVAL      Y = 1
     C                   EVAL      Y = %LOOKUP(SAV(X):COD:Y)
     C                   IF        Y > 0
     C                   EVAL      *IN99 = *ON
     C                   ELSE
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      Y = 1
     C                   ENDIF
     C                   MOVEL     NUM(Y)        EMACTC
     C                   IF        TEM(X) = C#DFT                               B01
     C                   MOVE      *BLANKS       EMMATM
     C                   ELSE                                                   X01
     C                   MOVE      TEM(X)        EMMATM
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRMBRD - Get member description                             *
      *                                                             *
      * This route fills FMDESC                                     *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRMBRD        BEGSR
      *
      *          ------------------------------------------
      *          Get the text if the member exists
      *          ------------------------------------------
     C                   EVAL      OBJT = '*FILE'
     C                   CALLP     OMH909C ( P#OBJL : P#OBJC : P#MBRC :
     C                             OBJT : P#STAT )
      *
     C                   IF        P#STAT = C#NORM                              B01
     C                   CALLP     OMH942C ( P#OBJC : P#OBJL : P#MBRC :
     C                             P#MBRA : P#MBRD : P#STAT )
     C                   IF        P#STAT = C#NORM                              B02
     C                   MOVEL     P#MBRD        FMDESC
     C                   ELSE                                                   X02
     C                   MOVEL(P)  C#NOTF        FMDESC
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
      *          Save field definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Keyfield definition
      *          ------------------------------------------
     C                   EVAL      K#HEDC = P#HEDC
     C                   EVAL      K#CUSC = P#CUSC
     C                   EVAL      K#CCTC = P#CCTC
      *
      *          ------------------------------------------
      *          Set screen indicators
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      P#HEDC <> *BLANKS                            X01
     C                   EVAL      *IN(50) = *OFF
     C                   EVAL      *IN(51) = *OFF
     C                   EVAL      *IN(52) = *ON
     C                   EVAL      *IN(53) = *OFF
     C                   WHEN      P#CCTC <> *BLANKS                            X01
     C                   EVAL      *IN(50) = *ON
     C                   EVAL      *IN(51) = *OFF
     C                   EVAL      *IN(52) = *OFF
     C                   EVAL      *IN(53) = *OFF
     C                   WHEN      P#CUSC <> *BLANKS                            X01
     C                   EVAL      *IN(50) = *OFF
     C                   EVAL      *IN(51) = *ON
     C                   EVAL      *IN(52) = *OFF
     C                   EVAL      *IN(53) = *OFF
     C                   OTHER                                                  X01
     C                   EVAL      *IN(50) = *OFF
     C                   EVAL      *IN(51) = *OFF
     C                   EVAL      *IN(52) = *OFF
     C                   EVAL      *IN(53) = *ON
     C                   ENDSL                                                  E01
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
     C                   PARM      'OMHMSG'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGDTA
      *
      *          ------------------------------------------
      *          Parameter list for OMH902C (Clear message queue)
      *          ------------------------------------------
     C     $X902C        PLIST
     C                   PARM                    PGMQ
      *
      *          ------------------------------------------
      *          Parameter list for OMH942C (Retrieve member descr)
      *          ------------------------------------------
     C     $X942C        PLIST
     C                   PARM                    P#OBJC
     C                   PARM                    P#OBJL
     C                   PARM                    P#MBRC
     C                   PARM                    P#MBRA
     C                   PARM                    P#MBRD
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH904C (Get message data text)
      *          ------------------------------------------
     C     $X904C        PLIST
     C                   PARM                    MSGID
     C                   PARM                    MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGTXT
      *
      *          ------------------------------------------
      *          Parameter list for OMH909C (Check member)
      *          ------------------------------------------
     C     $X909C        PLIST
     C                   PARM                    P#OBJL
     C                   PARM                    P#OBJC
     C                   PARM                    P#MBRC
     C                   PARM      '*FILE'       OBJT
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH029 (Get info)
      *          ------------------------------------------
     C     $MH029        PLIST
     C                   PARM                    P#OPTI
     C                   PARM      P#HEDC        P2HEDC
     C                   PARM                    P#SUL#
     C                   PARM                    P#STAT
     C                   PARM                    D#D7HD
     C                   PARM                    D#D8HD
      *
      *          ------------------------------------------
      *          OMTEM Template file
      *          ------------------------------------------
     C     $EM1K4        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#CUSC
     C                   KFLD                    K#CCTC
     C                   KFLD                    K#ACTC
     C     $EM1K3        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#CUSC
     C                   KFLD                    K#CCTC
      *
      *          ------------------------------------------
      *          Select mode, initialize screen indicator
      *          ------------------------------------------
     C                   EVAL      *IN80 = *OFF                                  Change
     C                   EVAL      *IN81 = *OFF                                  Change
     C                   EVAL      *IN82 = *OFF                                  Change
     C                   EVAL      EROR01 = '0'
     C                   SELECT                                                 B01
     C                   WHEN      P#ACTC = '20'                                X01
     C                   EVAL      *IN80 = *ON                                   Add
     C                   WHEN      P#ACTC = '40'                                X01
     C                   EVAL      *IN80 = *ON                                   Delete
     C                   EVAL      *IN81 = *ON                                   Delete
     C                   WHEN      P#ACTC = '50'                                X01
     C                   EVAL      *IN80 = *ON                                   Display
     C                   EVAL      *IN81 = *ON                                   Display
     C                   EVAL      *IN82 = *ON                                   Display
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          Retrieve authorization
      *          ------------------------------------------
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '9'
     C                   EVAL      P#STAT = C#NORM
     C                   EVAL      P2HEDC = P#HEDC
     C                   CALLP     OMH029 ( P#OPTI : P2HEDC : P#SUL# :
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
     C                   MOVEL     P#HEDC        FMHEDC
      *
      *          ------------------------------------------
      *          Get message data
      *          ------------------------------------------
     C                   EXSR      SRGVAR
      *
      *          ------------------------------------------
      *          Fill action code data arrays
      *          ------------------------------------------
     C                   EVAL      X = 1
     C                   CLEAR                   NUM
     C                   CLEAR                   COD
     C                   DOW       NCA(X) <> *ZEROS                             B02
     C                   EVAL      NUM(X) = NCA(X)
     C                   EVAL      COD(X) = CCA(X)
     C                   ADD       1             X
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Retrieve mail PF-SRC en library
      *          ------------------------------------------
     C                   CLEAR                   HXMATL
     C                   CLEAR                   SXMATL
     C                   IF        *IN52 = *ON
     C     P#HEDC        CHAIN     OMMHXR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF
     C     1             CHAIN     OMMSXR
     C                   EVAL      *IN99 = NOT %FOUND
      *
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
     C                   WHEN      E#FILE = 'OMH042D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMTEML1 '                          X02
     C                   MOVEL     TEML1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMMHXL1 '                          X02
     C                   MOVEL     MHXL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMMSX   '                          X02
     C                   MOVEL     MSX           P#FILE
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
      *          ------------------------------------------
      *          Error in *PSSR, exit immediately.
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   EVAL      *INLR = *ON
     C                   RETURN
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
      *          End normal
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
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
      * It is called one time per program execution.                 *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGVAR        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init.
      *          ------------------------------------------
     C                   EVAL      %SUBST(MSGF:1:6) = 'OMHMSG'
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 1                                  B01
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
     C                   EVAL      MSGGET = '1'
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
**
STR0001 - status route                          1
** NCA & CCA
09                         Reopen the API that handles the entity
15                         Allocate the entity to a user
20                         Change the entity
23                         Connect this entity to another one
25                         Accumulate this entity to a leading other entity
27                         Disconnect the entity
30                         Copy the entity
33                         Increase the priority of the entity
29                         Change the status of the entity
42                         Set the status of the incident to "completed"
60                         Add an entity to an appropriate environment
85                         Remove the allocation of the entity
87                         Promote to OMS request
99                         Close the API that handles the entity
00                  ** END OF TABLE
** NCA & CCA
*REOPEN                    Reopen the API that handles the entity
*ALLOCATE                  Allocate the entity to a user
*CHANGE                    Change the entity
*CONNECT                   Connect this entity to another one
*ACCUMULATE                Accumulate this entity to a leading other entity
*DISCONNECT                Disconnect the entity
*COPY                      Copy the entity
*ESCALATE                  Increase the priority of the entity
*STATUS                    Change the status of the entity
*COMPLETE                  Set the status of the incident to "completed"
*ADD                       Add an entity to an appropriate environment
*DEALLOCATE                Remove the allocation of the entity
*OMS                       Promote to OMS request
*CLOSE                     Close the API that handles the entity

