     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH023                                        *
      * Function ...: Enter incident text                           *
      * Author .....: Remain Software                               *
      * Parameters .: P#HEDC - HelpDesk code                        *
      *               P#INC# - Incident number                      *
      *               P#ITH# - Incident text header sequence        *
      *               P#ACTC - 20, 50, 55 and 60.                   *
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
     FOMTIHL1   UF A E           K DISK    INFDS(TIHL1)
     F                                     INFSR(*PSSR)
     FOMTIDL1   UF A E           K DISK    INFDS(TIDL1)
     F                                     INFSR(*PSSR)
     FOMTIPL1   IF   E           K DISK    INFDS(TIPL1)
     F                                     INFSR(*PSSR)
     FOMH023D   CF   E             WORKSTN INFDS(INFO)
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
     D POS             S              5  0 DIM(9)
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
     D TIHL1           DS            57
     D TIDL1           DS            57
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
     D C#MORE          C                   CONST('More...')
     D C#BOTM          C                   CONST('Bottom')
      * Prototype for OMH023
     D OMH023          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#INC#_                             LIKE(P#INC#)
     D P#ITH#_                             LIKE(P#ITH#)
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
      * Prototype for 'OMX1372'
     D OMX1372         PR                  EXTPGM('OMX1372')
     D P#HEDC_                             LIKE(P#HEDC)
     D P#TXID_                             LIKE(P#TXID)
     D P#INCD_                             LIKE(P#INCD)
     D P#STAT_                             LIKE(P#STAT)
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
     D OMH023          PI
     D P#HEDC                         5
     D P#INC#                        10
     D P#ITH#                         3  0
     D P#ACTC                         2
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D s#ix            S              5P 0
     D s#rr01          S              4P 0
     D w#sydt          S              7P 0
     D w#sytm          S              6P 0
     D CRRN01          S              4  0
     D DO_X            S              7  0
     D DO_X2           S              7  0
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
     D P#HDR#          S              5  0
     D P#INCD          S                   LIKE(INCD)
     D P#INDT          S              6  0
     D P#INFM          S              1
     D P#MSTA          S             40
     D P#OPTI          S              1
     D P#OTDT          S              6  0
     D P#OTFM          S              1
     D P#TXID          S              7
     D RRNPOS          S              5  0
     D RRN01           S              4  0
     D W#INDT          S              7  0
     D W#INS#          S              5  0
     D W#INYR          S              2  0
     D W#ITH#          S              5  0
     D W#POS#          S              5  0
     D W#WRTI          S              1
     D X               S              5  0
     D Y               S              5  0
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
      *          If this is "display maybe change" then make it
      *          change if this is the user which owns the text.
      *          ------------------------------------------
     C                   IF        P#ACTC = '55'                                B01
     C     $IH1K2        CHAIN     OMTIHL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B02
      *
     C                   Z-ADD     IHINDT        D#DATE
     C                   IF        D#CC = 0                                     B03
     C     1900          ADD       D#YY          D#YEAR
     C                   ELSE                                                   X03
     C     2000          ADD       D#YY          D#YEAR
     C                   ENDIF                                                  E03
      *
     C                   IF        IHUSID = D#USID                              B03
     C                             AND D#YEAR = *YEAR
     C                             AND D#MM = *MONTH
     C                             AND D#DD = *DAY
     C                   EVAL      P#ACTC = '20'
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
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
      *          Send the display while the incident is beeing
      *          created.
      *          ------------------------------------------
     C                   IF        P#INC# = *BLANKS
     C                   EVAL      I#READ = *ON
     C                   WRITE     OMH02301
     C                   RETURN
     C                   ENDIF
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
     C     T2301         TAG
     C                   EXFMT     OMH02301
      *
      *          ------------------------------------------
      *          Read from delayed device
      *          ------------------------------------------
     C                   ELSE
     C                   READ(E)   OMH02301
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
      *          F18 is select
      *          ------------------------------------------
     C                   IF        *IN18 = *ON                                  B03
     C                   EXSR      SRSELT
      *                    EXSR SRSELC
     C                   GOTO      T2301
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
      *          Fill the incident number
      *          ------------------------------------------
     C                   IF        P#INC# = *BLANKS
     C                   MOVEL     '*N/A'        FMINC#
     C                   ELSE
     C                   MOVE      P#INC#        FMINC#
      *
      *          ------------------------------------------
      *          Add mode, Initialize a header record.
      *          ------------------------------------------
     C                   IF        P#ACTC = '60'                                B01
      *
     C     $IH1K1        SETGT     OMTIHL1
     C     $IH1K1        READPE(N) OMTIHL1
     C                   EVAL      *IN99 = %EOF
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVE      P#HEDC        IHHEDC
     C                   MOVE      P#INC#        IHINC#
     C                   Z-ADD     1             IHITH#
     C                   ELSE                                                   X02
     C     IHITH#        ADD       1             IHITH#
     C                   ENDIF                                                  E02
      *
     C                   Z-ADD     IHITH#        P#ITH#
      *
      *          ------------------------------------------
      *          Calculate the system date.
      *          ------------------------------------------
     C                   EVAL      D#DATE = *ZEROS
     C                   EVAL      D#YY = UYEAR
     C                   EVAL      D#MM = UMONTH
     C                   EVAL      D#DD = UDAY
     C                   IF        D#YY < 39                                    B02
     C                   EVAL      D#CC = 1
     C                   ENDIF                                                  E02
     C                   Z-ADD     D#DATE        IHINDT
     C                   TIME                    IHINTM
     C                   MOVE      D#USID        IHUSID
     C                   MOVE      '1'           IHITXT
     C                   MOVE      '0'           IHSUL#
     C                   MOVE      *BLANKS       IHSITD
      *
      *          ------------------------------------------
      *          Write a header record.
      *          ------------------------------------------
     C                   WRITE     OMTIHR
     C                   ENDIF                                                  E01
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Do refresh subroutine
      *          ------------------------------------------
     C                   EXSR      SRRF01
      *
      *          ------------------------------------------
      *          No actions can be done if this is display.
      *          ------------------------------------------
     C                   IF        P#ACTC = '50'                                B01
     C                             OR P#ACTC = '55'
     C                   EVAL      *IN81 = *ON
     C                   ELSE                                                   X01
     C                   EVAL      *IN81 = *OFF
     C                   ENDIF                                                  E01
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
     C                   MOVE      '1'           FMITXT
     C                   MOVE      '0'           FMSUL#
     C                   MOVE      *BLANKS       FMSITD
      *
      *          ------------------------------------------
      *          Get the record if not add.
      *          ------------------------------------------
     C                   IF        P#ACTC <> '60'                               B01
     C     $IH1K2        CHAIN(N)  OMTIHL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      P#STAT = C#TERM
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
      *
     C                   MOVE      IHITXT        FMITXT
     C                   MOVE      IHSUL#        FMSUL#
     C                   MOVE      IHSITD        FMSITD
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get all lines.
      *          ------------------------------------------
     C                   EVAL      IX = *ZEROS
     C                   EVAL      SOP = *BLANKS
     C                   EVAL      STX = *BLANKS
     C     $ID1K1        SETLL     OMTIDL1
     C     $ID1K1        READE(N)  OMTIDL1
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF                                 B01
     C                   ADD       1             IX
     C                   MOVE      IDINCD        STX(IX)
     C     $ID1K1        READE(N)  OMTIDL1
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
     C                   EVAL      RRNPOS = 0
     C                   IF        *IN25 = *ON
     C                   EVAL      RRNPOS = CRRN01 + 8
     C                   ENDIF
     C                   IF        *IN26 = *ON
     C                   EVAL      RRNPOS = CRRN01 - 8
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Roll down only if relative rec. nr pos.
      *          lower than reletive record nr.
      *          When IF condition not used, roll down will
      *          be infinit/unlimited.
      *          ------------------------------------------
      *******    RRNPOS    IFLE RRN01
     C                   MOVE      *BLANKS       FMBOOC
     C                   MOVE      C#MORE        FMBOOC
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
      *******              ENDIF                           E01
      *
     C                   IF        *IN25 = *ON
     C     RRNPOS        ADD       8             RRNPOS
     C                   ENDIF
     C                   IF        *IN26 = *ON
     C     RRNPOS        SUB       8             RRNPOS
     C                   ENDIF
     C                   IF        RRNPOS >= RRN01
     C                             OR RRN01 <= 8
     C                   MOVE      *BLANKS       FMBOOC
     C                   MOVE      C#BOTM        FMBOOC
     C                   ENDIF
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
      *          Copy?
      *          ------------------------------------------
     C                   WHEN      SOP(IX) = 'C '                               X06
     C                   EVAL      SOP(IX) = *BLANKS
     C                   EVAL      W#INS# = D#OPT#
     C                   EVAL      W#POS# = IX
     C                   EVAL      LOOP01 = *ON
     C                   EXSR      SRCPY
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
      *          Write the new header values.
      *          ------------------------------------------
     C     $IH1K2        CHAIN     OMTIHL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      P#STAT = C#TERM
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
      *
     C                   MOVE      FMITXT        IHITXT
     C                   MOVE      FMSUL#        IHSUL#
     C                   MOVE      FMSITD        IHSITD
     C                   UPDATE    OMTIHR
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
     C                   IF        RRN01 = *ZEROS                               B02
     C                   DOU       *IN99 = *ON                                  B03
     C     $ID1K1        DELETE    OMTIDL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF
     C                   EVAL      LOOP01 = *ON
     C                   ENDIF
     C                   ENDDO                                                  E03
      *
      *          ------------------------------------------
      *          Else write
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C     $ID1K1        SETLL     OMTIDL1
     C     $ID1K1        READE     OMTIDL1
     C                   EVAL      *IN99 = %EOF
     C                   MOVE      IHHEDC        IDHEDC
     C                   MOVE      IHINC#        IDINC#
     C                   MOVE      IHITH#        IDITH#
     C                   FOR       IX = 1 TO RRN01                              B03
     C     IX            MULT      10            IDSEQ#
     C                   MOVE      IHITXT        IDITXT
      *
     C                   IF        IDINCD <> STX(IX)                            B04
     C                             AND LOOP01 <> *ON
     C                   EVAL      LOOP01 = *ON
     C                   ENDIF                                                  E04
      *
     C                   MOVE      STX(IX)       IDINCD
     C                   IF        *IN99 = *ON
     C                   WRITE     OMTIDR
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMTIDR
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C     $ID1K1        READE     OMTIDL1
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
     C                   DELETE    OMTIDR
     C     $ID1K1        READE     OMTIDL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E03
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
      * SRSELC - Select fixed text                                  *
      *                                                             *
      * This routine selects a fixed text                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSELC        BEGSR
      *
      *          ------------------------------------------
      *          Call the program
      *          ------------------------------------------
      *                    CALL 'OMX1372'
      *                    PARM           P#HEDC
      *                    PARM           P#TXID  7
      *                    PARM           P#INCD
      *                    PARM           P#STAT
      *
      *          ------------------------------------------
      *          Retrieve the data
      *          ------------------------------------------
     C                   EVAL      P#HDR# = 0
     C                   EVAL      W#ITH# = 0
     C     $IP1K2        SETLL     OMTIPR
     C     $IP1K1        READE     OMTIPR
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 <> *ON
      *
      *          ------------------------------------------
      *          Fill and recognize the header
      *          ------------------------------------------
     C                   IF        IPHDR# <> P#HDR#
     C                   Z-ADD     IPHDR#        P#HDR#
     C                   MOVEL     P#HEDC        IHHEDC
     C                   MOVEL     P#INC#        IHINC#
      *
      *          ------------------------------------------
      *          First time: check existing record
      *          ------------------------------------------
     C                   IF        W#ITH# = 0
     C                   Z-ADD     99999         P#ITH#
     C     $IH1K2        SETGT     OMTIHR
     C     $IH1K1        READPE    OMTIHR
     C                   EVAL      *IN98 = %EOF
     C                   IF        *IN98 = *OFF
     C                   IF        IHSITD = *BLANKS
     C                   MOVEL(P)  IPINCD        IHSITD
     C                   MOVEL     IPITXT        IHITXT
     C                   MOVEL     IPSUL#        IHSUL#
     C                   MOVEL(P)  IPINCD        IHSITD
     C                   UPDATE    OMTIHR
     C                   Z-ADD     IHITH#        W#ITH#
     C                   EVAL      W#WRTI = *ON
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   IF        W#WRTI = *OFF
     C     W#ITH#        ADD       1             IHITH#
     C                   MOVEL(P)  IPINCD        IHSITD
      *
      *          ------------------------------------------
      *          First time: check existing record, write
      *          ------------------------------------------
     C                   MOVEL     IPITXT        IHITXT
     C                   Z-ADD     *DAY          IHINDT
     C                   EVAL      W#INDT = 100 * *MONTH
     C                   ADD       W#INDT        IHINDT
     C                   Z-ADD     *YEAR         W#INYR
     C                   EVAL      W#INDT = 10000 * W#INYR
     C                   ADD       W#INDT        IHINDT
     C                   IF        W#INYR < 40
     C                   ADD       1000000       IHINDT
     C                   ENDIF
     C                   TIME                    IHINTM
     C                   MOVEL     IPSUL#        IHSUL#
     C                   MOVEL(P)  IPINCD        IHSITD
     C                   WRITE     OMTIHR
     C                   Z-ADD     IHITH#        W#ITH#
     C                   ELSE
     C                   EVAL      W#WRTI = *OFF
     C                   ENDIF
     C                   ELSE
      *
      *          ------------------------------------------
      *          Fill and the details
      *          ------------------------------------------
     C                   MOVEL     P#HEDC        IDHEDC
     C                   MOVEL     P#INC#        IDINC#
     C                   Z-ADD     IHITH#        IDITH#
     C                   MOVEL     IPITXT        IDITXT
     C     IPLIN#        SUB       10            IDSEQ#
     C                   MOVEL(P)  IPINCD        IDINCD
     C                   WRITE     OMTIDR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Volgende lees in standaardteksten
      *          ------------------------------------------
     C     $IP1K1        READE     OMTIPR
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO
     C                   EVAL      P#ACTC = '61'
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSELT - Select fixed text                                  *
      *                                                             *
      * This routine selects a fixed text                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSELT        BEGSR
      *
      *          ------------------------------------------
      *          Call the program
      *          ------------------------------------------
     C                   CALLP     OMX1372 ( P#HEDC : P#TXID : P#INCD :
     C                             P#STAT )
      *          ------------------------------------------
      *          Check if more then one predifined text
      *          ------------------------------------------
     C                   EVAL      P#HDR# = 20
     C     $IP1K2        SETLL     OMTIPR
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF
      *          ------------------------------------------
      *          Retrieve the data
      *          ------------------------------------------
     C                   EVAL      X = 0
     C                   EVAL      P#HDR# = 0
     C                   EVAL      W#ITH# = 0
     C     $IP1K2        SETLL     OMTIPR
     C     $IP1K1        READE     OMTIPR
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 <> *ON
      *
      *          ------------------------------------------
      *          Fill and recognize the header and text
      *          ------------------------------------------
     C                   ADD       1             X
     C                   SELECT
     C                   WHEN      IPLIN# = 10
     C                   MOVEL(P)  IPINCD        FMSITD
     C                   WHEN      IPLIN# = 20
     C                   MOVEL(P)  IPINCD        FMTX01
     C                   WHEN      IPLIN# = 30
     C                   MOVEL(P)  IPINCD        FMTX02
     C                   WHEN      IPLIN# = 40
     C                   MOVEL(P)  IPINCD        FMTX03
     C                   WHEN      IPLIN# = 50
     C                   MOVEL(P)  IPINCD        FMTX04
     C                   WHEN      IPLIN# = 60
     C                   MOVEL(P)  IPINCD        FMTX05
     C                   WHEN      IPLIN# = 70
     C                   MOVEL(P)  IPINCD        FMTX06
     C                   WHEN      IPLIN# = 80
     C                   MOVEL(P)  IPINCD        FMTX07
     C                   WHEN      IPLIN# = 90
     C                   MOVEL(P)  IPINCD        FMTX08
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Vul de array voor Page Down
      *          ------------------------------------------
     C                   IF        X > 1
     C     X             SUB       1             Y
     C                   MOVEL(P)  IPINCD        STX(Y)
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Volgende lees in standaardteksten
      *          ------------------------------------------
     C     $IP1K1        READE     OMTIPR
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO
     C                   EVAL      P#ACTC = '60'
     C                   Z-ADD     X             RRN01
     C                   IF        X > 8
     C                   MOVE      C#MORE        FMBOOC
     C                   ELSE
     C                   MOVE      *BLANKS       FMBOOC
     C                   ENDIF
      *
     C                   ELSE
     C                   EXSR      SRSELC
     C                   ENDIF
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
      * SRCPY  - Copy number of lines.                              *
      *                                                             *
      * This routine inserts a number of lines into the subfile.    *
      *                                                             *
      * W#INS#   - Number to copy                                   *
      * W#POS#   - Position to copy to.                             *
      * RRN01    - Last record in the array                         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCPY         BEGSR
      *
      *          ------------------------------------------
      *          Copy W#INS# lines
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
      *          Copy those lines.
      *          ------------------------------------------
     C                   EVAL      IY = W#POS#
     C                   EVAL      IZ = W#POS#
     C                   FOR       DO_X2 = 1 TO W#INS#                          B01
     C                   EVAL      STX(IY) = STX(IZ)
     C                   EVAL      SOP(IY) = SOP(IZ)
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
     C                   EVAL      IZ = *ZEROS
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
      *          OMTIHL1 Incident text header
      *          ------------------------------------------
     C     $IH1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#INC#
      *
     C     $IH1K2        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#INC#
     C                   KFLD                    P#ITH#
      *
     C     $IH1K3        KLIST
     C                   KFLD                    IHHEDC
     C                   KFLD                    IHINC#
     C                   KFLD                    IHITH#
      *
      *          ------------------------------------------
      *          OMTIDL1 Incident text detail
      *          ------------------------------------------
     C     $ID1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#INC#
     C                   KFLD                    P#ITH#
      *
      *          ------------------------------------------
      *          OMTIPL1 Predefined text
      *          ------------------------------------------
     C     $IP1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#TXID
      *
      *          ------------------------------------------
      *          OMTIPL1 Predefined text
      *          ------------------------------------------
     C     $IP1K2        KLIST
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
     C                   WHEN      E#FILE = 'OMTIHL1 '                          X02
     C                   MOVEL     TIHL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMTIPL1 '                          X02
     C                   MOVEL     TIPL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMTIDL1 '                          X02
     C                   MOVEL     TIDL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMH023D '                          X02
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
      *          If no texts then delete the header
      *          ------------------------------------------
     C                   IF        P#ACTC = '60'                                B02
     C                             OR P#ACTC = '20'
     C     $ID1K1        SETLL     OMTIDL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B03
     C     $IH1K3        CHAIN     OMTIHL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        IHSITD = *BLANKS                             B04
     C                   IF        *IN99 = *OFF
     C                   DELETE    OMTIHR
     C                   ENDIF
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
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
