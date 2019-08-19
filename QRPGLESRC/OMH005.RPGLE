     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH005                                        *
      * Function ...: Severity code maintenance                     *
      * Module .....: Help desk                                     *
      *                                                             *
      * Author .....: Remain Software                               *
      * Action .....: Creation.                                     *
      *                                                             *
      * Parameters .: HEDC, STAT                                    *
      *                                                             *
      * This program maintains the severity code table              *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators                                             *
      * ----------------------------------------------------------- *
      *                                                             *
      * 70-77 - Screen field error indicators                       *
      * 80-82 - Maintenance action indicators                       *
      *                                                             *
      * 92 - subfile end indicator                                  *
      * 99 - General purpose indicator.                             *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMHDSL2   UF A E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HDSL2)
     FOMHDSL1   IF   E           K DISK
     F                                     RENAME(OMHDSR:OMHDS1)
     F                                     INFSR(*PSSR)
     F                                     INFDS(HDSL1)
     FOMH005D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
     F                                     SFILE(OMH00501:RRN01)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(10)
     D MSD             S              7    DIM(7) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
      * ------------------------------------------------------------ *
      *                                                              *
      * DFT   Indicates per help desk code and severity type whether *
      *       default *YES is allowed: severity code = array index,  *
      *       values Y and N; up to 9 elements are possible (now 3)  *
      * ------------------------------------------------------------ *
     D DFT             S              1    DIM(10)
      *
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
      *          File information data structures
      *          ------------------------------------------
     D HDSL1           DS            57
     D HDSL2           DS            57
      *
      *          ------------------------------------------
      *          Screen information data structure
      *          ------------------------------------------
     D INFO            DS
     D  STATUS           *STATUS
     D  D#CRC#               378    379B 0
      *
      *          ------------------------------------------
      *          Format subfile option.
      *          ------------------------------------------
     D                 DS
     D W#OPTI                         2
     D  D#OPP1                        1    OVERLAY(W#OPTI)
     D  D#OPP2                        1    OVERLAY(W#OPTI:2)
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  E#FILE               201    208
     D  PGMQ             *PROC
      *
      *          ------------------------------------------
      *          Helpdesk data structures.
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
      *          Workfield
      *          ------------------------------------------
     D                 DS
     D FMTABC                         2
     D  W#TABC                        1    OVERLAY(FMTABC:2)
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#SFL1          C                   CONST(8)
      * Prototype for OMH005
     D OMH005          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#TABC_                             LIKE(P#TABC)
     D P1STAT_                             LIKE(P1STAT)
      * Prototype for 'OMH902C'
     D OMH902C         PR                  EXTPGM('OMH902C')
     D PGMQ_                               LIKE(PGMQ)
      *
      *          ------------------------------------------
      *          Parameter list for OMH904C (Get message data text)
      * Prototype for 'OMH901C2'
     D OMH901C         PR                  EXTPGM('OMH901C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGDTA_                             LIKE(MSGDTA)
      *
      *          ------------------------------------------
      *          Parameter list for OMH902C (Clear message queue)
      * Prototype for 'OMX105'
     D OMX105          PR                  EXTPGM('OMX105')
     D FMHEDC_                             LIKE(FMHEDC)
     D HEDD_                               LIKE(HEDD)
      * Prototype for 'OMH029'
     D OMH029          PR                  EXTPGM('OMH029')
     D P#OPTI_                             LIKE(P#OPTI)
     D P#HEDC_                             LIKE(P#HEDC)
     D P#SUL#_                             LIKE(P#SUL#)
     D P#STAT_                             LIKE(P#STAT)
     D D#D7HD_                             LIKE(D#D7HD)
     D D#D8HD_                             LIKE(D#D8HD)
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      * Prototype for 'OMH904C'
     D OMH904C         PR                  EXTPGM('OMH904C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGTXT_                             LIKE(MSGTXT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH905C (Get user profile)
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
     D OMH005          PI
     D P#HEDC                         5
     D P#TABC                         2
     D P1STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D s#rr01          S              5P 0
     D EOF             S                   LIKE(INDI)
     D EROR01          S                   LIKE(INDI)
     D EROR02          S                   LIKE(INDI)
     D I#PSSR          S              1
     D IX              S              5  0
     D IY              S              1  0
     D K#SEW#          S                   LIKE(DSSEW#)
     D LOOP01          S                   LIKE(INDI)
     D LOOP02          S                   LIKE(INDI)
     D MA_X            S              5  0
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#FILE          S             57
     D P#GRCL          S             10
     D P#GRID          S             10
     D P#HEDD          S                   LIKE(HEDD)
     D P#INKC          S                   LIKE(INDI)
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#OPTI          S                   LIKE(INDI)
     D P#PMQC          S             10
     D P#STAT          S                   LIKE(STAT)
     D P#SUL#          S                   LIKE(SUL#)
     D P#USCL          S             10
     D P#USDC          S             50
     D P#USID          S             10
     D RRN01           S              5  0
     D S#HEDC          S                   LIKE(HEDC)
     D S#IN40          S                   LIKE(*IN40)
     D S#SEVC          S                   LIKE(DSSEVC)
     D S#SEW#          S                   LIKE(DSSEW#)
     D S#TABC          S                   LIKE(TABC)
     D SEL             S                   LIKE(INDI)
     D W#CRC1          S              5  0
     D W#EOFI          S                   LIKE(INDI)
     D W#OPT1          S                   LIKE(FMOPT1)
     D W#SAV#          S              5  0
     D W#VALC          S              5  0
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
      *          Work with Incident severities
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B01
     C                   EXSR      SRSC01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Exit
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
     C                   CALLP     OMH901C ( MSGID : MSGF : MSGLIB :
     C                             MSGDTA )
     C                   FOR       MA_X = 1 TO %ELEM(MC)
     C                   EVAL      MC(MA_X) = *BLANK
     C                   ENDFOR
     C                   EVAL      MSGDTA = *BLANK
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * PROMPT - Prompt a field                                      * /
      *                                                              * /
      * This routine prompts a field.                                * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     PROMPT        BEGSR
      *
      *          ------------------------------------------
      *          Display 1.
      *          HelpDesk code
      *          ------------------------------------------
     C                   IF        FMFMTC = 'OMH00502'                          B01
     C                             AND FMFLDC = 'FMHEDC'                        B02
     C                             OR FMFMTC = 'OMH00505'                       B01
     C                             AND FMFLDC = 'FMHEDC'                        B02
     C                   MOVE      FMHEDC        S#HEDC
     C                   CALLP     OMX105 ( FMHEDC : HEDD )
     C                   IF        FMHEDC <> S#HEDC                             B02
     C                   EXSR      SRCHGH
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Invalid prompt location.
      *          ------------------------------------------
     C                   EVAL      *IN71 = *ON
     C                   EVAL      MSGID = 'PMT0002'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC01 - Work with severity code                            *
      *                                                             *
      * This routine displays the first screen where the user can   *
      * select a severity code for maintenance actions.             *   *
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
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH00503
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        RRN01 = *ZEROS                               B03
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   IF        *IN90 = *ON
     C                   EXFMT     OMH00502
     C                   ENDIF
     C                   IF        *IN90 = *OFF
     C                   EXFMT     OMH00505
     C                   ENDIF
     C                   IF        *INKC = *ON
     C                   EVAL      P#STAT = '*EXIT'
     C                   EXSR      SRTTLR
     C                   ENDIF
     C                   IF        *INKL = *ON
     C                   EVAL      P#STAT = '*CNCL'
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   EVAL      W#CRC1 = D#CRC#
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
     C                   EVAL      EROR01 = *OFF
     C                   EVAL      LOOP01 = *OFF
      *
      *          ------------------------------------------
      *          Command key processing.
      *          F4=Prompt
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   EXSR      PROMPT
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B04
     C                             OR *IN41 = *ON                               B03
     C                   EVAL      P#TABC = FMTABC
     C                   Z-ADD     999           FMPOSI
     C                   EXSR      SRIN01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Change the HelpDesk if required.
      *          ------------------------------------------
     C                   IF        S#HEDC <> FMHEDC                             B05
     C                   EXSR      SRCHGH
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          F6=Add Function
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B06
     C                   MOVE      *BLANKS       W#OPT1
     C                   EXSR      SRSC02
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X06
      *
      *          ------------------------------------------
      *          22=Roll up
      *          ------------------------------------------
     C                   IF        *IN22 = *ON                                  B07
     C                   EXSR      SRRU01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X07
      *
      *          ------------------------------------------
      *          Do edit routine
      *          ------------------------------------------
     C                   EXSR      SRED01
     C                   ENDIF                                                  E07
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
      *          Do processing if subfile records
      *          ------------------------------------------
     C                   IF        RRN01 <> *ZERO                               B02
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
      * SRIN01 - Work with severity codes init                      *   *
      *                                                             *
      * Init the work with severity codes display.                  *   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Fill default screen value
      *          ------------------------------------------
     C                   IF        P#TABC <> '01'                               B01
     C                             AND P#TABC <> '02'
     C                             AND P#TABC <> '03'
     C                   EVAL      FMTABC = '01'
     C                   ELSE                                                   X01
     C                   EVAL      FMTABC = P#TABC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Clear fields
      *          ------------------------------------------
     C                   Z-ADD     999           FMPOSI
     C                   EVAL      S#HEDC = P#HEDC
     C                   Z-ADD     999           K#SEW#
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
      * SRRD01 - Read next severity code Record                     *   *
      *                                                             *
      * This routine reads the next severity code record            *   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRD01        BEGSR
      *
      *          ------------------------------------------
      *          Do it
      *          ------------------------------------------
     C     $HDSK4        READE(N)  OMHDSL2
     C                   EVAL      *IN99 = %EOF
     C                   EVAL      EOF = *IN99
      *
      *          ------------------------------------------
      *          Save the key.
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF
     C                   Z-ADD     DSSEW#        K#SEW#
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSF01 - Build work with severity codes subfile                 *
      *                                                             *
      * This routine builds one page of subfile records             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSF01        BEGSR
      *
      *          ------------------------------------------
      *          Read first interface record
      *          ------------------------------------------
     C     $HDSK2        SETLL     OMHDSL2
     C                   EXSR      SRRD01
      *
      *          ------------------------------------------
      *          Do while not end of application file
      *          ------------------------------------------
     C                   EVAL      IX = *ZEROS
     C                   DOW       EOF = *OFF                                   B01
     C                             AND IX < C#SFL1
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPT1
     C                   MOVEL     DSSEVC        FMSEVC
     C                   MOVEL     DSDESC        FMDESC
     C                   Z-ADD     DSSEW#        FMSEW#
     C                   IF        DSDFTI = '1'                                 B02
     C                   MOVE      '*YES '       FMSTAT
     C                   ELSE                                                   X02
     C                   MOVE      *BLANKS       FMSTAT
     C                   ENDIF                                                  E02
     C                   ADD       1             RRN01
     C                   ADD       1             IX
     C                   WRITE     OMH00501
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C                   EXSR      SRRD01
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          If end of file then no more records (SFLEND)
      *          ------------------------------------------
     C                   IF        EOF = *ON                                    B01
     C                   EVAL      *IN92 = *OFF
     C                   ELSE                                                   X01
     C                   EVAL      *IN92 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF01 - Work with severity code refresh                        *
      *                                                             *
      * This routine is called in the screen initialisation, and    *
      * when the user pressed F5 from the first screen.             *
      * This routine builds the display with initial values.        *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   MOVE      S#HEDC        FMHEDC
     C                   EVAL      RRN01 = *ZEROS
      *
      *          ------------------------------------------
      *          Fill the key, reset screenfield
      *          ------------------------------------------
     C                   Z-ADD     FMPOSI        K#SEW#
     C                   EVAL      FMPOSI = 0
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH00502
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Build the subfile
      *          ------------------------------------------
     C                   EXSR      SRSF01
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
      * SRRU01 - Roll up next screen                                *
      *                                                             *
      * This routine is called if the user pressed the roll up key  *
      * in the screen. The next x - records are read.               *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRU01        BEGSR
      *
      *          ------------------------------------------
      *          Build the Subfile
      *          ------------------------------------------
     C                   EXSR      SRSF01
      *
      *          ------------------------------------------
      *          Calculate current record number
      *          ------------------------------------------
     C     W#CRC1        ADD       C#SFL1        CRRN01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED01 - Work with severity codes refresh                   *   *
      *                                                             *
      * This routine checks if the option fields in the subfile     *
      * where correctly filled. A severity code can be              *   *
      * changed (2), deleted (4) or viewed (5)                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
     C                   EVAL      CRRN01 = *ZEROS
     C                   EVAL      W#SAV# = RRN01
     C                   EVAL      W#VALC = *ZEROS
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   IF        RRN01 > *ZERO                                B01
     C                   READC     OMH00501
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Do if option is not blank
      *          ------------------------------------------
     C                   IF        FMOPT1 <> *BLANKS                            B03
      *
      *          ------------------------------------------
      *          Format the subfile option.
      *          ------------------------------------------
     C                   MOVEL     FMOPT1        W#OPTI
     C                   IF        D#OPP2 = *BLANKS                             B04
     C                   EVAL      D#OPP2 = D#OPP1
     C                   EVAL      D#OPP1 = *BLANKS
     C                   MOVEL     W#OPTI        FMOPT1
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If option is valid, then check the number of valid
      *          codes. *IN93 is the subfile next change indicator
      *          ------------------------------------------
     C                   IF        FMOPT1 = ' 2'                                B04
     C                             OR FMOPT1 = ' 4'
     C                             OR FMOPT1 = ' 5'
      *
      *          ------------------------------------------
      *          Option is valid
      *          ------------------------------------------
     C     $HDSK1        SETLL     OMHDSL2
     C                   EVAL      *IN99 = %EQUAL
      *
      *          ------------------------------------------
      *          Record no longer active
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF                                 B05
     C                   EVAL      MSGID = 'FILE002'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X05
     C                   ADD       1             W#VALC
     C                   ENDIF                                                  E05
      *
      *          ------------------------------------------
      *          Option not valid
      *          ------------------------------------------
     C                   ELSE                                                   X04
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EVAL      MSGDTA = VAR(1)
     C                   EVAL      MSGID = 'OPTE001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If an error was found, then position on the page of
      *          the first error
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B04
     C                             AND *IN70 = *ON
     C                   Z-ADD     RRN01         CRRN01
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Update subfile
      *          ------------------------------------------
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH00501
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   UPDATE    OMH00501
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH00501
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If subfile must be positioned, and no errors occured
      *          then position it.
      *          ------------------------------------------
     C                   EVAL      RRN01 = W#SAV#
     C                   IF        CRRN01 = *ZEROS                              B01
     C                             AND FMPOSI <> 0
     C                             OR *IN41 = *ON
      *
      *          ------------------------------------------
      *          Fill the correct key and call the refresh routine
      *          ------------------------------------------
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
      *
      *          ------------------------------------------
      *          Fill the current relative record on the page to
      *          display.
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   IF        CRRN01 = *ZEROS                              B02
     C                   Z-ADD     W#CRC1        CRRN01
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
      * SRPR01 - Work with severity codes refresh                   *   *
      *                                                             *
      * This routine processes the display.                         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      S#RR01 = RRN01
      *
      *          ------------------------------------------
      *          Read subfile next changed record
      *          If records in subfile and valid codes found.
      *          ------------------------------------------
     C                   IF        RRN01 > *ZEROS                               B01
     C                             AND W#VALC > *ZEROS
     C                   READC     OMH00501
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Read subfile until valid option found
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          If options typed then reloop.
      *          ------------------------------------------
     C                   EVAL      LOOP01 = *ON
     C                   EVAL      W#CRC1 = RRN01
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPT1        W#OPT1
     C                   MOVE      *BLANKS       FMOPT1
     C                   UPDATE    OMH00501
      *
      *          ------------------------------------------
      *          Do action if option is 2 or 4 (well, 5)
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      W#OPT1 = ' 2'
     C                   EXSR      SRSC02
     C                   WHEN      W#OPT1 = ' 4'
     C                   EXSR      SRSC02
     C                   WHEN      W#OPT1 = ' 5'
     C                   EXSR      SRSC02
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Read next changed if no end requested.
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                   LEAVE
     C                   ELSE                                                   X03
     C                   READC     OMH00501
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If end of file found, then end loop
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Reset subfile relative record number
      *          ------------------------------------------
     C                   Z-ADD     W#CRC1        CRRN01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Reset record pointers
      *          ------------------------------------------
     C                   EVAL      RRN01 = S#RR01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC02 - Screen 2 processing routine                        *
      *                                                             *
      * This routine displays the second screen. The change, add    *
      * and delete options, are combined into one screen, controled *
      * with indicator 80 & 81.                                     *
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 off = Change                                    *
      * 80 on  & 81 on  & 82 off = Delete                           *
      * 80 on  & 81 on  & 82 on = Display                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC02        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EXSR      SRIN02
      *
      *          ------------------------------------------
      *          Do while no end
      *          ------------------------------------------
     C                   DOU       LOOP02 = *OFF                                B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR02 = *OFF                                B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the screen
      *          ------------------------------------------
     C                   EXFMT     OMH00504
     C                   IF        *INKC = *ON
     C                   EVAL      P#STAT = '*EXIT'
     C                   EXSR      SRTTLR
     C                   ENDIF
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
     C                   EVAL      EROR02 = *OFF
     C                   EVAL      LOOP02 = *OFF
      *
      *          ------------------------------------------
      *          Command key processing.
      *          F5=Refresh
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B03
     C                   EXSR      SRRF02
     C                   EVAL      EROR02 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Do edit routine if not F12.
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B04
     C                   EXSR      SRED02
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do processing if not F12
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B02
     C                   EXSR      SRPR02
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
      * SRIN02 - Screen 2 initialisation                            *
      *                                                             *
      * This routine blanks the input fields on the screen.         *
      * Pending on the required action. The change, add and delete  *
      * options are combined into one screen, controlled by indica- *
      * tor 80 and 81.                                              *
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 off = Change                                    *
      * 80 on  & 81 on  & 82 off = Delete                           *
      * 80 on  & 81 on  & 82 on = Display                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN02        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init.
      *          ------------------------------------------
     C                   EVAL      S#IN40 = *OFF
      *
      *          ------------------------------------------
      *          Determine the action
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B01
     C                   EVAL      *IN(80) = *OFF
     C                   EVAL      *IN(81) = *OFF
     C                   EVAL      *IN(82) = *OFF
     C                   ELSE                                                   X01
     C                   IF        W#OPT1 = ' 2'                                B02
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *OFF
     C                   ELSE                                                   X02
     C                   IF        W#OPT1 = ' 5'                                B03
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *ON
     C                   EVAL      *IN(82) = *ON
     C                   ELSE                                                   X03
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *ON
     C                   EVAL      *IN(82) = *OFF
      *
      *          ------------------------------------------
      *          Display deletion message
      *          ------------------------------------------
     C                   EVAL      MSGID = 'DLT0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Set fields if addition
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
      *
      *                    --------------------------------
      *                    No copy, clear the fields.
      *                    --------------------------------
     C                   MOVE      *BLANKS       FMSEVC
     C                   MOVE      *BLANKS       FMDESC
     C                   MOVE      *BLANKS       FMSTAT
     C                   MOVE      ' 0'          FMDFTI
     C                   EVAL      FMSEW# = 0
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Get the file if no addition, save keyfield
      *          ------------------------------------------
     C     $HDSK1        CHAIN(N)  OMHDSL2
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL     DSSEVC        FMSEVC
     C                   MOVEL     DSSEVC        S#SEVC
     C                   MOVEL     DSDESC        FMDESC
     C                   Z-ADD     DSSEW#        FMSEW#
     C                   Z-ADD     DSSEW#        S#SEW#
     C                   MOVEL     DSDFTI        FMDFTI
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check wether default indicator can be set
      *          ------------------------------------------
     C                   TESTN                   W#TABC               77
     C                   IF        *IN77 = *ON                                  B01
     C                   MOVE      W#TABC        IY
     C                   IF        DFT(IY) = 'N'                                B02
     C                             AND FMDFTI <> '1'
     C                   EVAL      *IN77 = *ON
     C                   ELSE                                                   X02
     C                   EVAL      *IN77 = *OFF
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
     C                   EVAL      IY = 9
     C                   EVAL      *IN77 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF02 - Screen 2 refresh                                   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF02        BEGSR
      *
      *                    --------------------------------
      *                    Add: clear the fields.
      *                    --------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C                   MOVE      *BLANKS       FMSEVC
     C                   MOVE      *BLANKS       FMDESC
     C                   MOVE      *BLANKS       FMSTAT
     C                   MOVE      ' 0'          FMDFTI
     C                   EVAL      FMSEW# = 0
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Refresh screen
      *          ------------------------------------------
     C                   MOVEL     DSSEVC        FMSEVC
     C                   MOVEL     DSDESC        FMDESC
     C                   IF        DSDFTI = '1'                                 B02
     C                   MOVE      '*YES '       FMSTAT
     C                   ELSE                                                   X02
     C                   MOVE      *BLANKS       FMSTAT
     C                   ENDIF                                                  E02
     C                   Z-ADD     DSSEW#        FMSEW#
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED02 - Screen 2 edit                                      *
      *                                                             *
      * This routine checks if the several attributes are  filled   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED02        BEGSR
      *
      *          ------------------------------------------
      *          Test if addition.
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
      *
      *          ------------------------------------------
      *          Check if the severity code code exists.
      *          ------------------------------------------
     C                   IF        FMSEVC <> S#SEVC                             B02
     C     $HDSK         SETLL     OMHDS1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B03
     C                   EVAL      MSGID = 'FILE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN70 = *ON
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Test if not delete.
      *          ------------------------------------------
     C                   IF        *IN81 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Check if code is filled
      *          ------------------------------------------
     C                   IF        FMSEVC = *BLANKS                             B02
     C                   EVAL      MSGDTA = VAR(2)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN72 = *ON
     C                   EVAL      EROR02 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if weight is filled
      *          ------------------------------------------
     C                   IF        FMSEW# = 0                                   B02
     C                   EVAL      MSGDTA = VAR(3)
     C                   EVAL      MSGID = 'ZERE001'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN73 = *ON
     C                   EVAL      EROR02 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Description not be blanks
      *          ------------------------------------------
     C                   IF        FMDESC = *BLANKS                             B02
     C                   EVAL      MSGDTA = VAR(4)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN74 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Default code should be 0 or 1
      *          ------------------------------------------
     C                   IF        FMDFTI <> '0'                                B02
     C                             AND FMDFTI <> '1'                            B02
     C                   EVAL      MSGID = 'HLP0010'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN75 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if the severity code code exists.
      *          ------------------------------------------
     C                   IF        FMSEVC <> S#SEVC                             B02
     C     $HDSK         SETLL     OMHDS1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B03
     C                   EVAL      MSGID = 'FILE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN70 = *ON
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
      * SRPR02 - Process screen 2                                   *
      *                                                             *
      * This routine updates the application file                   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR02        BEGSR
      *
      *          ------------------------------------------
      *          Reloop if fields where changed.
      *          ------------------------------------------
     C                   MOVE      *IN40         LOOP02
      *
      *          ------------------------------------------
      *          Delete the file if wanted.
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B01
     C                             AND *IN81 = *ON
     C                   IF        *IN82 = *OFF                                 B02
     C     $HDSK1        DELETE    OMHDSR
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Udate default indication
      *          ------------------------------------------
     C                   IF        FMDFTI = '1'                                 B03
     C                   EVAL      DFT(IY) = 'Y'
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Do only if fields where changed.
      *          ------------------------------------------
     C                   IF        *IN40 = *ON                                  B02
      *
      *          ------------------------------------------
      *          Do only if keyfield has changed in CHANGE mode
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B03
     C                   IF        FMSEW# <> S#SEW#                             B04
     C                             OR FMSEVC <> S#SEVC                          B02
     C     $HDSK3        DELETE    OMHDSR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Get the file
      *          ------------------------------------------
     C     $HDSK1        CHAIN     OMHDSL2
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Udate default indication
      *          ------------------------------------------
     C                   IF        FMDFTI = '1'                                 B03
     C                   EVAL      DFT(IY) = 'N'
     C                   ELSE                                                   X03
     C                   IF        FMDFTI <> DSDFTI                             B04
     C                   EVAL      DFT(IY) = 'Y'
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Fill the file
      *          ------------------------------------------
     C                   MOVEL     FMHEDC        DSHEDC
     C                   MOVEL     FMTABC        DSTABC
     C                   MOVEL     FMSEVC        DSSEVC
     C                   MOVEL     FMDESC        DSDESC
     C                   MOVEL     FMDFTI        DSDFTI
     C                   Z-ADD     FMSEW#        DSSEW#
      *
      *          ------------------------------------------
      *          Write or update
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMHDSR
     C                   ENDIF
     C                   IF        *IN99 = *ON
     C                   WRITE     OMHDSR
     C                   MOVEL     FMSEVC        S#SEVC
     C                   ENDIF
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Force UPDATE mode if in ADD mode.
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B02
     C                             AND *IN81 = *OFF
     C                   EVAL      *IN80 = *ON
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
      * SRCHGH - Change the Helpdesk                                *
      *                                                             *
      * This routine changes the helpdesk                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCHGH        BEGSR
      *
      *          ------------------------------------------
      *          If the HelpDesk code is blanks, then reset it.
      *          ------------------------------------------
     C                   IF        FMHEDC = *BLANKS                             B01
     C                   MOVE      S#HEDC        FMHEDC
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Change the helpdesk
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '9'
     C                   MOVE      FMHEDC        P#HEDC
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD )
      *
      *          ------------------------------------------
      *          If errors, reset
      *          ------------------------------------------
     C                   IF        P#STAT <> C#NORM                             B02
     C                   EVAL      *IN71 = *ON
     C                   EVAL      *IN72 = *ON
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Fill screen fields from parameters
      *          ------------------------------------------
     C                   MOVE      P#HEDC        FMHEDC
     C                   MOVEL     D8HEDD        FMHEDD
     C                   EXSR      SRIN01
     C                   EVAL      EROR01 = *ON
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
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Keyfield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Savefield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      W#VALC = *ZERO
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
      *          Parameter list for OMH904C (Get message data text)
      *          ------------------------------------------
     C     $X904C        PLIST
     C                   PARM                    MSGID
     C                   PARM      'OMHMSG'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGTXT
      *
      *          ------------------------------------------
      *          Parameter list for OMH905C (Get user profile)
      *          ------------------------------------------
     C     $X905C        PLIST
     C                   PARM                    P#USID
     C                   PARM                    P#USCL
     C                   PARM                    P#GRID
     C                   PARM                    P#GRCL
     C                   PARM                    P#USDC
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          OMHDSL2 severity code File
      *          ------------------------------------------
     C     $HDSK         KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMTABC
     C                   KFLD                    FMSEVC
      *
     C     $HDSK1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMTABC
     C                   KFLD                    FMSEW#
     C                   KFLD                    FMSEVC
      *
     C     $HDSK2        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMTABC
     C                   KFLD                    K#SEW#
      *
     C     $HDSK3        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMTABC
     C                   KFLD                    S#SEW#
     C                   KFLD                    S#SEVC
      *
     C     $HDSK4        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMTABC
      *
     C     $HDSK5        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    S#TABC
      *
      *          ------------------------------------------
      *          Retrieve authorization
      *          ------------------------------------------
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '9'
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD )
      *
      *          ------------------------------------------
      *          Continue if authorization ok
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B01
      *
      *          ------------------------------------------
      *          Fill screen fields from parameters
      *          ------------------------------------------
     C                   MOVE      P#HEDC        FMHEDC
     C                   MOVEL     D8HEDD        FMHEDD
      *
      *          ------------------------------------------
      *          Get message data
      *          ------------------------------------------
     C                   EXSR      SRGVAR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check default
      *          ------------------------------------------
     C                   MOVE      *LOVAL        S#TABC
     C                   EVAL      DFT = 'Y'
     C     $HDSK5        SETLL     OMHDSR
     C                   DOW       W#EOFI <> 'E'                                B01
     C     P#HEDC        READE     OMHDSR
     C                   EVAL      *IN99 = %EOF
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      W#EOFI = 'E'
     C                   ELSE                                                   X02
     C                   IF        DSDFTI = '1'                                 B03
     C                   MOVE      DSTABC        IY
     C                   EVAL      DFT(IY) = 'N'
     C                   ENDIF                                                  E03
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
      * SRTTLR - Total last record processing                       *
      *                                                             *
      * This routine sets the last record indicator on.             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRTTLR        BEGSR
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
      * It is called one time per program execution. If the system   *
      * choose to do so, it can be paged out. Thats why it is here.  *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGVAR        BEGSR
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 7                                  B01
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
     C                   WHEN      E#FILE = 'OMH005D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMHDSL1 '                          X02
     C                   MOVEL     HDSL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHDSL2 '                          X02
     C                   MOVEL     HDSL2         P#FILE
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
**
OPT0001  -  '2' or '4'                        1
HLP0008  -  Code                              2
HLP0009  -  Weight                            3
HLP0003  -  Description                       4
