     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH049                                        *
      * Function ...: Enter incident predefined text                *
      * Author .....: Remain Software                               *
      * Parameters .: P#HEDC - HelpDesk code                        *
      *               P#TXID - Text id                              *
      *               P#HDR# - Header number                        *
      *               P#ACTC - 20 or 60                             *
      *               P#STAT - Return status                        *
      *                                                             *
      * This program lets the user display and change the incident  *
      * text file.                                                  *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMTIPL1   UF A E           K DISK    INFDS(TIPL1)
     F                                     INFSR(*PSSR)
     FOMH049D   CF   E             WORKSTN INFDS(INFO)
     F                                     DEVID(P#DEVC)
     F                                     INFSR(*PSSR)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension specifications                                     *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * TXT   Text                                                   *
      * OPT   Options                                                *
      * STX   Subfile text                                           *
      * SOP   Subfile options                                        *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(1)
     D MSD             S              7    DIM(1) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
     D STX             S             70    DIM(9999)
     D SOP             S              2    DIM(9999)
      *COPY QCPYSRC,OMSREFREN
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Input specifications                                        *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Field reference file used to define fields
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          Screen information data structure
      *          ------------------------------------------
     D INFO            DS
     D  STATUS           *STATUS
     D  D#CRC#               378    379B 0
      *
      *          ------------------------------------------
      *          Date convertor.
      *          ------------------------------------------
     D                 DS
     D D#DATE                         7  0
     D  D#CC                          1  0 OVERLAY(D#DATE)
     D   D#CCC                        1    OVERLAY(D#CC)
     D  D#YY                          2  0 OVERLAY(D#DATE:2)
     D  D#MM                          2  0 OVERLAY(D#DATE:4)
     D  D#DD                          2  0 OVERLAY(D#DATE:6)
     D                 DS
     D D#YEAR                         4  0
      *
      *          ------------------------------------------
      *          File information data structures
      *          ------------------------------------------
     D TIPL1           DS            57
      *
      *          ------------------------------------------
      *          Includes
      *          ------------------------------------------
      /COPY QAPILESRC,DEFBINARY
      /COPY QAPILESRC,ERR_BUFFER
      *
      *          ------------------------------------------
      *          Program status data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  E#FILE               201    208
     D  D#USID               254    263
      *
      *          ------------------------------------------
      *          Text
      *          ------------------------------------------
     D                 DS
     D  TXT                    1    560
     D                                     DIM(8)
     D  FMTX01                 1     70
     D  FMTX02                71    140
     D  FMTX03               141    210
     D  FMTX04               211    280
     D  FMTX05               281    350
     D  FMTX06               351    420
     D  FMTX07               421    490
     D  FMTX08               491    560
     D  OPT                  561    576
     D                                     DIM(8)
     D  FMOP01               561    562
     D  FMOP1#               562    562  0
     D  FMOP02               563    564
     D  FMOP2#               564    564  0
     D  FMOP03               565    566
     D  FMOP3#               566    566  0
     D  FMOP04               567    568
     D  FMOP4#               568    568  0
     D  FMOP05               569    570
     D  FMOP5#               570    570  0
     D  FMOP06               571    572
     D  FMOP6#               572    572  0
     D  FMOP07               573    574
     D  FMOP7#               574    574  0
     D  FMOP08               575    576
     D  FMOP8#               576    576  0
      *
      *          ------------------------------------------
      *          Option code
      *          ------------------------------------------
     D                 DS
     D D#OPTC                         1
     D  D#OPT#                        1  0 OVERLAY(D#OPTC)
      *
      *          ------------------------------------------
      *          Constants.
      *          ------------------------------------------
     D C#CNCL          C                   CONST('*CNCL')
     D C#EXIT          C                   CONST('*EXIT')
     D C#TERM          C                   CONST('*TERM')
      * Prototype for OMH049
     D OMH049          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#TXID_                             LIKE(P#TXID)
     D P#HDR#_                             LIKE(P#HDR#)
     D P#ITXT_                             LIKE(P#ITXT)
     D P#SUL#_                             LIKE(P#SUL#)
     D P#ACTC_                             LIKE(P#ACTC)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH902C'
     D OMH902C         PR                  EXTPGM('OMH902C')
     D PGMQ_                               LIKE(PGMQ)
      *
      *          ------------------------------------------
      *          Parameter list for OMH904C (Get message data text)
      * Prototype for 'OMH901C'
     D OMH901C         PR                  EXTPGM('OMH901C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGDTA_                             LIKE(MSGDTA)
      *
      *          ------------------------------------------
      *          Parameter list for OMH902C (Clear message queue)
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
      *          Parameter list for OMH908C (Convert date to format)
      * *ENTRY Interface for Main Procedure
     D OMH049          PI
     D P#HEDC                         5
     D P#TXID                         7
     D P#HDR#                         5  0
     D P#ITXT                         1
     D P#SUL#                         1
     D P#ACTC                         2
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D p#ith#          S              3P 0
     D s#ix            S              5P 0
     D s#rr01          S              4P 0
     D w#sydt          S              7P 0
     D w#sytm          S              6P 0
     D w#tels          S              4P 0
     D CRRN01          S              4  0
     D DO_X            S              7  0
     D EROR01          S                   LIKE(INDI)
     D I#PSSR          S              1
     D I#READ          S                   LIKE(INDI)
     D IX              S              5  0
     D IY              S              5  0
     D IZ              S              5  0
     D LOOP01          S                   LIKE(INDI)
     D MA_X            S              5  0
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D NC              S              3  0
     D P#FILE          S             57
     D P#INC#          S                   LIKE(INC#)
     D P#INDT          S              6  0
     D P#INFM          S              1
     D P#MSTA          S             40
     D P#OPTI          S              1
     D P#OTDT          S              6  0
     D P#OTFM          S              1
     D RRN01           S              4  0
     D W#INS#          S              5  0
     D W#POS#          S              5  0
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
      *          Exit if no parameters
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
      *          Do the total last record routine
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * CLRMSG - Clear message subfile                               * /
      *                                                              * /
      * This routine calls a subprogram wich clears the message      * /
      * subfile                                                      * /
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
      * SNDMSG - Send message                                        * /
      *                                                              * /
      * This routine calls a subprogram which sends a message to the *
      * program message queue                                        *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SNDMSG        BEGSR
      *
      *          ------------------------------------------
      *          Call Program sender
      *          ------------------------------------------
     C                   EVAL      MSGF = 'OMHAPI'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C  ( MSGID : MSGF : MSGLIB :
     C                             MSGDTA )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ----------------------------------------------------------- *
      * SRVERW - Main program processing routine                    *
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
      * SRSC01 - Screen 1 processing routine                        *
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
      *          Normal window handling.
      *          ------------------------------------------
     C                   IF        I#READ <> *ON
      *
      *                    --------------------------------
      *                    Display the message subfile
      *                    --------------------------------
     C                   WRITE     MSGCTL
      *
      *                    --------------------------------
      *                    Display
      *                    --------------------------------
     C                   EXFMT     OMH04901
      *
      *          ------------------------------------------
      *          Read from delayed device
      *          ------------------------------------------
     C                   ELSE
     C                   READ(E)   OMH04901
     C                   EVAL      *IN99 = %ERROR OR %EOF
     C                   EVAL      I#READ = *OFF
     C                   ENDIF
      *
      *          ------------------------------------------
      *          F12 is exit
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Move the display into the text.
      *          ------------------------------------------
     C                   EXSR      SRFI01
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
     C                   EVAL      *IN(80) = *OFF
     C                   EVAL      EROR01 = *OFF
     C                   EVAL      LOOP01 = *OFF
      *
      *          ------------------------------------------
      *          Command key processing
      *          F5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B03
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F11=Edit/Type
      *          ------------------------------------------
     C                   IF        *INKK = *ON                                  B04
     C                   EVAL      EROR01 = *ON
     C                   EVAL      *IN11 = (*IN11 <> *ON)
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          *IN25=Roll up
      *          ------------------------------------------
     C                   IF        *IN25 = *ON                                  B05
     C                   EXSR      SRRU01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          *IN26=Roll down
      *          ------------------------------------------
     C                   IF        *IN26 = *ON                                  B06
     C                   EXSR      SRRD01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X06
      *
      *          ------------------------------------------
      *          Do edit routine
      *          ------------------------------------------
     C                   EXSR      SRED01
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
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRFI01 - Fill this page.                                    *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRFI01        BEGSR
      *
      *          ------------------------------------------
      *          Fill the subfile
      *          ------------------------------------------
     C                   EVAL      IX = CRRN01
      *
     C                   EVAL      STX(IX) = TXT(1)
     C                   EVAL      SOP(IX) = OPT(1)
     C                   ADD       1             IX
      *
     C                   EVAL      STX(IX) = TXT(2)
     C                   EVAL      SOP(IX) = OPT(2)
     C                   ADD       1             IX
      *
     C                   EVAL      STX(IX) = TXT(3)
     C                   EVAL      SOP(IX) = OPT(3)
     C                   ADD       1             IX
      *
     C                   EVAL      STX(IX) = TXT(4)
     C                   EVAL      SOP(IX) = OPT(4)
     C                   ADD       1             IX
      *
     C                   EVAL      STX(IX) = TXT(5)
     C                   EVAL      SOP(IX) = OPT(5)
     C                   ADD       1             IX
      *
     C                   EVAL      STX(IX) = TXT(6)
     C                   EVAL      SOP(IX) = OPT(6)
     C                   ADD       1             IX
      *
     C                   EVAL      STX(IX) = TXT(7)
     C                   EVAL      SOP(IX) = OPT(7)
     C                   ADD       1             IX
      *
     C                   EVAL      STX(IX) = TXT(8)
     C                   EVAL      SOP(IX) = OPT(8)
      *
      *          ------------------------------------------
      *          Determine the last used record.
      *          ------------------------------------------
     C                   EVAL      IX = RRN01 + 8
     C                   DOW       IX > *ZERO                                   B01
     C                   IF        STX(IX) <> *BLANKS                           B02
     C                   Z-ADD     IX            RRN01
     C                   LEAVE
     C                   ENDIF                                                  E02
     C                   SUB       1             IX
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
      *          81 off = Add/Change                                *
      *          81 on  = Display                                   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Fill screenfield
      *          ------------------------------------------
     C                   MOVE      P#TXID        FMTXID
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
      * SRRF01 - Screen 1 refresh                                   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF01        BEGSR
      *
      *          ------------------------------------------
      *          Init the records
      *          ------------------------------------------
     C                   EVAL      *IN78 = *OFF
     C                   EVAL      *IN79 = *OFF
     C                   EVAL      *IN80 = *OFF
     C                   MOVEL     P#ITXT        FMITXT
     C                   MOVE      P#SUL#        FMSUL#
     C                   MOVE      *BLANKS       FMSITD
      *
      *          ------------------------------------------
      *          Get all lines.
      *          ------------------------------------------
     C                   EVAL      IX = 0 - 1
     C                   EVAL      SOP = *BLANKS
     C                   EVAL      STX = *BLANKS
     C     $IP1K1        SETLL     OMTIPL1
     C     $IP1K1        READE(N)  OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF                                 B01
     C                   ADD       1             IX
     C                   IF        IX = 0
     C                   MOVEL     IPINCD        FMSITD
     C                   ELSE
     C                   MOVE      IPINCD        STX(IX)
     C                   ENDIF
     C     $IP1K1        READE(N)  OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Fill display.
      *          ------------------------------------------
     C                   Z-ADD     IX            RRN01
     C                   EVAL      CRRN01 = 1
      *
      *          ------------------------------------------
      *          Roll up.
      *          ------------------------------------------
     C                   EXSR      SRRU01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRU01 - Roll up screen 1                                   *
      *                                                             *
      * This routine is called if the user pressed roll up          *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRU01        BEGSR
      *
      *          ------------------------------------------
      *          Increment the current relatieve record number
      *          if rollup.
      *          ------------------------------------------
     C                   IF        *IN25 = *ON
     C                   ADD       8             CRRN01
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Fill the subfile fields
      *          ------------------------------------------
     C                   EVAL      IX = CRRN01
     C                   FOR       IY = 1 TO 8                                  B01
     C                   EVAL      TXT(IY) = STX(IX)
     C                   EVAL      OPT(IY) = SOP(IX)
     C                   ADD       1             IX
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          If greater then increment the records
      *          ------------------------------------------
     C                   IF        CRRN01 > RRN01                               B01
     C     CRRN01        ADD       8             RRN01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Setof
      *          ------------------------------------------
     C                   EVAL      *IN25 = *OFF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRD01 - Roll down screen 1.                                *
      *                                                             *
      * This routine is called if the user pressed roll down.       *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRD01        BEGSR
      *
      *          ------------------------------------------
      *          Decrement the current relatieve record number
      *          if rolldown.
      *          ------------------------------------------
     C                   IF        *IN26 = *ON
     C                   SUB       8             CRRN01
     C                   ENDIF
     C                   IF        CRRN01 < 1                                   B01
     C                   EVAL      CRRN01 = 1
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the subfile.
      *          ------------------------------------------
     C                   EXSR      SRRU01
      *
      *          ------------------------------------------
      *          Setof
      *          ------------------------------------------
     C                   EVAL      *IN26 = *OFF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED01 - Screen 1 edit                                      *
      *                                                             *
      * This routine checks if the option fields in the subfile     *
      * where correctly filled.                                     *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          The text type
      *          ------------------------------------------
     C                   IF        FMITXT < '1'                                 B01
     C                             OR FMITXT > '4'
     C                   EVAL      MSGID = 'OMQ502E'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN78 = *ON
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The support level
      *          ------------------------------------------
     C                   IF        FMSUL# < '0'                                 B01
     C                             OR FMSUL# > '9'
     C                   EVAL      MSGID = 'OMQ502E'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN79 = *ON
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          The short description
      *          ------------------------------------------
     C                   IF        FMSITD = *BLANKS                             B01
     C                   EVAL      MSGID = 'OMQ502C'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN80 = *ON
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR01 - Write back the text in the subfile.                *
      *                                                             *
      * This routine writes the incident text file.                 *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          If changes then always reloop
      *          ------------------------------------------
     C                   MOVE      *IN40         LOOP01
      *
      *          ------------------------------------------
      *          If addition or changition.
      *          ------------------------------------------
     C                   IF        P#ACTC = '20'                                B01
     C                             OR P#ACTC = '60'
      *
      *          ------------------------------------------
      *          Check the subfile options.
      *          ------------------------------------------
     C                   IF        *IN11 = *ON                                  B02
      *
     C                   FOR       IX = 1 TO RRN01                              B03
     C                   IF        SOP(IX) <> *BLANKS                           B04
      *
      *          ------------------------------------------
      *          The second position of the option must be
      *          blanks or numeric.
      *          ------------------------------------------
     C                   EVAL      D#OPTC = %SUBST(SOP(IX):2:1)
     C                   IF        D#OPTC = *BLANKS                             B05
     C                             OR D#OPTC >= '1'
     C                             AND D#OPTC <= '9'
      *
     C                   IF        D#OPTC = *BLANKS                             B06
     C                   EVAL      D#OPTC = '1'
     C                   ENDIF                                                  E06
     C                   EVAL      %SUBST(SOP(IX):2:1) = ' '                    NOT *BLANKS!
      *
     C                   EVAL      S#IX = IX
     C                   SELECT                                                 B06
      *          ------------------------------------------
      *          Insert?
      *          ------------------------------------------
     C                   WHEN      SOP(IX) = 'I '                               X06
     C                   EVAL      SOP(IX) = *BLANKS
     C                   EVAL      W#INS# = D#OPT#
     C                   EVAL      W#POS# = IX
     C                   EVAL      LOOP01 = *ON
     C                   EXSR      SRINS
     C                   SUB       1             S#IX
      *
      *          ------------------------------------------
      *          Delete?
      *          ------------------------------------------
     C                   WHEN      SOP(IX) = 'D '                               X06
     C                   EVAL      SOP(IX) = *BLANKS
     C                   EVAL      W#INS# = D#OPT#
     C                   EVAL      W#POS# = IX
     C                   EVAL      LOOP01 = *ON
     C                   EXSR      SRDEL
     C                   SUB       1             S#IX
      *
      *          ------------------------------------------
      *          Otherwise ignore
      *          ------------------------------------------
     C                   OTHER                                                  X06
     C                   EVAL      SOP(IX) = *BLANKS
     C                   EVAL      LOOP01 = *ON
     C                   ENDSL                                                  E06
      *
      *          ------------------------------------------
      *          Restore the counter
      *          ------------------------------------------
     C                   EVAL      IX = S#IX
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDFOR                                                 E03
      *
      *          ------------------------------------------
      *          Rebuild the display
      *          ------------------------------------------
     C                   EXSR      SRRU01
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Get the last RRN.
      *          ------------------------------------------
     C                   EVAL      IX = 1 + RRN01
     C                   EVAL      STX(IX) = *BLANKS
     C                   DOW       STX(IX) = *BLANKS                            B02
     C                   SUB       1             IX                       99
     C                   IF        *IN99 = *ON
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO                                                  E02
     C                   Z-ADD     IX            RRN01
      *
      *          ------------------------------------------
      *          If nothing then remove everything
      *          ------------------------------------------
      ******     RRN01     IFEQ *ZEROS                     B02
      ***        *IN99     DOUEQ*ON                        B03
      ***        $IP1K1    DELETOMTIPL1              99
      **N99                MOVE *ON       LOOP01
      ****                 ENDDO                           E03
      *
      *          ------------------------------------------
      *          Else write
      *          ------------------------------------------
      ****                 ELSE                            X02
     C     $IP1K1        SETLL     OMTIPL1
     C     $IP1K1        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   MOVE      P#HEDC        IPHEDC
     C                   MOVE      P#TXID        IPTXID
     C     RRN01         ADD       1             W#TELS
     C                   FOR       IX = 1 TO W#TELS                             B03
     C     IX            MULT      10            IPLIN#
      *
     C     IX            SUB       1             IZ
     C                   SELECT
     C                   WHEN      IX = 1
     C                   IF        IPINCD <> FMSITD                             B04
     C                             AND LOOP01 <> *ON
     C                   EVAL      LOOP01 = *ON
     C                   ENDIF                                                  E04
     C                   OTHER
     C                   IF        IPINCD <> STX(IZ)                            B04
     C                             AND LOOP01 <> *ON
     C                   EVAL      LOOP01 = *ON
     C                   ENDIF                                                  E04
     C                   ENDSL
      *
     C                   IF        IX = 1
     C                   MOVEL     FMSITD        IPINCD
     C                   ELSE
     C                   MOVE      STX(IZ)       IPINCD
     C                   ENDIF                                                  E04
     C                   Z-ADD     P#HDR#        IPHDR#
     C                   MOVE      FMITXT        IPITXT
     C                   MOVE      FMSUL#        IPSUL#
      *
     C                   IF        *IN99 = *ON
     C                   WRITE     OMTIPR
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMTIPR
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C     $IP1K1        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
     C                   ENDFOR                                                 E03
      *
      *          ------------------------------------------
      *          Delete the rest of the file
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF
     C                   EVAL      LOOP01 = *ON
     C                   ENDIF
     C                   DOW       *IN99 = *OFF                                 B03
     C                   DELETE    OMTIPR
     C     $IP1K1        READE     OMTIPL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E03
      *******              ENDIF                           E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRDEL  - Delete a number of lines                           *
      *                                                             *
      * This routine deletes a number of lines from the subfile.    *
      *                                                             *
      * W#INS#   - Number to delete                                 *
      * W#POS#   - Position to delete in.                           *
      * RRN01    - Last record in the array                         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRDEL         BEGSR
      *
      *          ------------------------------------------
      *          Delete W#INS# lines
      *          ------------------------------------------
     C                   EVAL      IY = W#POS#
     C     W#INS#        ADD       W#POS#        IX
     C                   DOW       IY <> RRN01                                  B01
     C                   EVAL      STX(IY) = STX(IX)
     C                   EVAL      SOP(IY) = SOP(IX)
     C                   ADD       1             IY
     C                   ADD       1             IX
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Set new top
      *          ------------------------------------------
     C     RRN01         SUB       W#INS#        RRN01
     C                   EVAL      IX = RRN01 + 1
     C                   FOR       MA_X = IX TO %ELEM(STX)
     C                   EVAL      STX(MA_X) = *BLANKS
     C                   ENDFOR
     C                   FOR       MA_X = IX TO %ELEM(SOP)
     C                   EVAL      SOP(MA_X) = *BLANKS
     C                   ENDFOR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRINS  - Insert a number of lines.                          *
      *                                                             *
      * This routine inserts a number of lines into the subfile.    *
      *                                                             *
      * W#INS#   - Number to insert                                 *
      * W#POS#   - Position to insert in.                           *
      * RRN01    - Last record in the array                         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRINS         BEGSR
      *
      *          ------------------------------------------
      *          Insert W#INS# lines
      *          ------------------------------------------
     C                   EVAL      IY = RRN01
     C     RRN01         ADD       W#INS#        IX
     C     RRN01         ADD       W#INS#        RRN01
     C                   DOU       IY < W#POS#                                  B01
     C                   EVAL      STX(IX) = STX(IY)
     C                   EVAL      SOP(IX) = SOP(IY)
     C                   SUB       1             IY
     C                   SUB       1             IX
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Blank those lines.
      *          ------------------------------------------
     C                   EVAL      IY = W#POS#
     C                   FOR       DO_X = 1 TO W#INS#                           B01
     C                   EVAL      STX(IY) = *BLANKS
     C                   EVAL      SOP(IY) = *BLANKS
     C                   ADD       1             IY
     C                   ENDFOR                                                 E01
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
      *          Explicit definitions
      *          ------------------------------------------
     C                   EVAL      RRN01 = *ZEROS
     C                   EVAL      CRRN01 = *ZEROS
     C                   EVAL      MS = *ZEROS
     C                   EVAL      IX = *ZEROS
     C                   EVAL      IY = *ZEROS
     C                   EVAL      NC = *ZEROS
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
      *          Save field definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      W#INS# = *ZEROS
     C                   EVAL      W#POS# = *ZEROS
      *
      *          ------------------------------------------
      *          Keyfield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          API parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (Send message)
      *          ------------------------------------------
     C     $X901C        PLIST
     C                   PARM                    MSGID
     C                   PARM      'OMHAPI'      MSGF
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
      *          Parameter list for OMH904C (Get message data text)
      *          ------------------------------------------
     C     $X904C        PLIST
     C                   PARM                    MSGID
     C                   PARM      'OMHMSG'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGTXT
      *
      *          ------------------------------------------
      *          Parameter list for OMH908C (Convert date to format)
      *          ------------------------------------------
     C     $X908C        PLIST
     C                   PARM                    P#OPTI
     C                   PARM                    P#INDT
     C                   PARM                    P#OTDT
     C                   PARM                    P#INFM
     C                   PARM                    P#OTFM
     C                   PARM                    P#STAT
     C                   EVAL      P#OPTI = 'C'
      *
      *          ------------------------------------------
      *          OMTIPL1 Incident text detail
      *          ------------------------------------------
     C     $IP1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#TXID
     C                   KFLD                    P#HDR#
      *
      *          ------------------------------------------
      *          Exit if no parameters
      *          ------------------------------------------
     C                   IF        D#PARM = *ZEROS                              B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get message data
      *          ------------------------------------------
     C                   EXSR      SRGVAR
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
      * ----------------------------------------------------------- *
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
      *
     C                   WHEN      E#FILE = 'OMTIPL1 '                          X02
     C                   MOVEL     TIPL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMH049D '                          X02
     C                   MOVEL     INFO          P#FILE
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
      *          Else exit immediately
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Exit
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#TERM
     C                   EXSR      SRTTLR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRTTLR - Total last record processing                       *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          If this was a normal call
      *          ------------------------------------------
     C                   IF        D#PARM > *ZEROS                              B01
      *
      *          ------------------------------------------
      *          Fill return status
      *          ------------------------------------------
     C                   SELECT                                                 B02
     C                   WHEN      *INKC = *ON                                  X02
     C                   EVAL      P#STAT = C#EXIT
     C                   EVAL      D#PARM = *ZEROS
     C                   WHEN      *INKL = *ON                                  X02
     C                   EVAL      P#STAT = C#CNCL
     C                   ENDSL                                                  E02
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If this was a closing call
      *          ------------------------------------------
     C                   IF        D#PARM = *ZEROS                              B01
      *
      *          ------------------------------------------
      *          Seton last record
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Return to calling program
      *          ------------------------------------------
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
      * SRGVAR  -  Get variable message text                         *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGVAR        BEGSR
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 0                                  B01
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Compile-time arrays                                          *
      * ------------------------------------------------------------ *
**
