     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH011                                        *
      * Function ...: Customer maintenance                          *
      * Module .....: Help desk                                     *
      *                                                             *
      * Author .....: Remain Software                               *
      * Action .....: Creation.                                     *
      *                                                             *
      * Parameters .: *NONE                                         *
      *                                                             *
      * This program maintains the help desk customer file          *
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
      * 83 - Show More Options
      * 92 - subfile end indicator                                  *
      * 99 - General purpose indicator.                             *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMCUSL1   UF A E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(CUSL1)
     FOMCCOL2   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(CCOL2)
     FOMH011D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
     F                                     SFILE(OMH01101:RRN01)
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
     D CUSL1           DS            57
     D CCOL2           DS            57
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
      *          Named constants
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#EXIT          C                   CONST('*EXIT')
     D C#CNCL          C                   CONST('*CNCL')
     D C#NO            C                   CONST('*NO')
     D C#SFL1          C                   CONST(11)
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
      * Prototype for 'OMQUSRCU'
     D OMQUSRCU        PR                  EXTPGM('OMQUSRCU')
     D P#CUSC_                             LIKE(P#CUSC)
     D P#CCTC_                             LIKE(P#CCTC)
     D P#ACTC_                             LIKE(P#ACTC)
     D P#USF1_                             LIKE(P#USF1)
     D P#F1ST_                             LIKE(P#F1ST)
     D P#USF2_                             LIKE(P#USF2)
     D P#F2ST_                             LIKE(P#F2ST)
     D P#USF3_                             LIKE(P#USF3)
     D P#F3ST_                             LIKE(P#F3ST)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Check if the exit program exists
      * Prototype for 'OMH047'
     D OMH047          PR                  EXTPGM('OMH047')
     D P#CUSC_                             LIKE(P#CUSC)
     D P#NAMD_                             LIKE(P#NAMD)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      * Prototype for 'OMH041C'
     D OMH041C         PR                  EXTPGM('OMH041C')
     D FMCUSC_                             LIKE(FMCUSC)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH012'
     D OMH012          PR                  EXTPGM('OMH012')
     D P#CUSC_                             LIKE(P#CUSC)
     D P#CCTC_                             LIKE(P#CCTC)
     D P#STAT_                             LIKE(P#STAT)
      *
      *
      *          ------------------------------------------
      *          Parameter list for OMH047 (Rename customer)
      * Prototype for 'OMH029'
     D OMH029          PR                  EXTPGM('OMH029')
     D P#OPTI_                             LIKE(P#OPTI)
     D P#HEDC_                             LIKE(P#HEDC)
     D P#SUL#_                             LIKE(P#SUL#)
     D P#STAT_                             LIKE(P#STAT)
     D D#D7HD_                             LIKE(D#D7HD)
     D D#D8HD_                             LIKE(D#D8HD)
     D P#CIM_                              LIKE(P#CIM)
      *
      *          ------------------------------------------
      *          OMCCOL2 Customer Contact positioning
      * Prototype for 'OMH906C'
     D OMH906C         PR                  EXTPGM('OMH906C')
     D P#OBJC_                             LIKE(P#OBJC)
     D P#OBJL_                             LIKE(P#OBJL)
     D P#OBJT_                             LIKE(P#OBJT)
     D OBJD_                               LIKE(OBJD)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH029 (Get info)
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
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D p#usf3          S             15P 0
     D s#rr01          S              5P 0
     D EOF             S                   LIKE(INDI)
     D EROR01          S                   LIKE(INDI)
     D EROR02          S                   LIKE(INDI)
     D I#CUPG          S                   LIKE(INDI)
     D I#PSSR          S              1
     D IX              S              5  0
     D K#CNTI          S                   LIKE(COCNTI)
     D K#CUSC          S                   LIKE(USCUSC)
     D LOOP01          S                   LIKE(INDI)
     D LOOP02          S                   LIKE(INDI)
     D MA_X            S              5  0
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#ACTC          S                   LIKE(ACTC)
     D P#CCTC          S                   LIKE(CCTC)
     D P#CIM           S              4
     D P#CUSC          S                   LIKE(CUSC)
     D P#FILE          S             57
     D P#F1ST          S                   LIKE(STAT)
     D P#F2ST          S                   LIKE(STAT)
     D P#F3ST          S                   LIKE(STAT)
     D P#GRCL          S             10
     D P#GRID          S             10
     D P#HEDC          S                   LIKE(HEDC)
     D P#INKC          S                   LIKE(INDI)
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#NAMD          S                   LIKE(NAMD)
     D P#OBJC          S                   LIKE(OBJC)
     D P#OBJL          S                   LIKE(OBJL)
     D P#OBJT          S                   LIKE(OBJT)
     D P#OPTI          S                   LIKE(INDI)
     D P#PMQC          S             10
     D P#STAT          S                   LIKE(STAT)
     D P#SUL#          S                   LIKE(SUL#)
     D P#USCL          S             10
     D P#USDC          S             50
     D P#USF1          S                   LIKE(USF1)
     D P#USF2          S                   LIKE(USF2)
     D P#USID          S             10
     D P1STAT          S                   LIKE(STAT)
     D RRN01           S              5  0
     D S#CUSC          S                   LIKE(USCUSC)
     D S#IN40          S                   LIKE(*IN40)
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
      *          Work with Application Relations.
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
     C                   WRITE     OMH01103
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        RRN01 = *ZEROS                               B03
     C                   EXFMT     OMH01105
     C                   ELSE                                                   X03
     C                   IF        *IN23 = *ON
     C                   EVAL      *IN90 = *OFF
     C                   ENDIF
     C                   IF        *IN23 = *OFF
     C                   EVAL      *IN90 = *ON
     C                   ENDIF
     C                   EXFMT     OMH01102
     C                   ENDIF                                                  E03
     C                   IF        *INKC = *ON
     C                   EVAL      P#STAT = C#EXIT
     C                   EXSR      SRTTLR
     C                   ENDIF
     C                   IF        *INKL = *ON
     C                   EVAL      P#STAT = C#CNCL
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
      *          F5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B03
     C                   MOVE      *BLANKS       FMPOSI
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Command key processing.
      *          F6=Add Function
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B04
     C                   MOVE      *BLANKS       W#OPT1
     C                   EXSR      SRSC02
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          22=Roll up
      *          ------------------------------------------
     C                   IF        *IN22 = *ON                                  B05
     C                   EXSR      SRRU01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
      *          ------------------------------------------
      *          23=More options
      *          ------------------------------------------
     C                   IF        *IN23 = *ON                                  B05
     C                   EXSR      SRMROP
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          Do edit routine
      *          ------------------------------------------
     C                   EXSR      SRED01
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
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
      *          Clear fields
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMPOSI
      *          ------------------------------------------
      *          Show options starting at 2, 4, 5,,,
      *          ------------------------------------------
     C                   EVAL      *IN83 = *ON
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
     C                   READ(N)   OMCUSL1
     C                   EVAL      *IN99 = %EOF
     C                   EVAL      EOF = *IN99
      *
      *          ------------------------------------------
      *          Save the key.
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF
     C                   MOVEL     USCUSC        K#CUSC
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
     C     K#CUSC        SETLL     OMCUSL1
     C                   EXSR      SRRD01
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   EVAL      IX = *ZEROS
     C                   DOW       EOF = *OFF                                   B01
     C                             AND IX < C#SFL1
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPT1
     C                   MOVEL     USCUSC        FMCUSC
     C                   MOVEL     USNAMD        FMNAMS
     C     $CCOK1        SETLL     OMCCOR
     C     USCUSC        READE     OMCCOR
     C                   EVAL      *IN99 = %EOF
     C                   IF        *IN99 = *OFF
     C                   MOVEL     CONAMD        FMNAMC
     C                   MOVEL     COTEL#        FMTEL#
     C                   ELSE
     C                   MOVEL     *BLANKS       FMNAMC
     C                   MOVEL     *BLANKS       FMTEL#
     C                   ENDIF
     C                   ADD       1             RRN01
     C                   ADD       1             IX
     C                   WRITE     OMH01101
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
     C                   EVAL      RRN01 = *ZEROS
      *
      *          ------------------------------------------
      *          Fill the key, reset screenfield
      *          ------------------------------------------
     C                   MOVEL     FMPOSI        K#CUSC
     C                   MOVE      *BLANKS       FMPOSI
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH01102
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
      * where correctly filled. A customer can be                   *   *
      * changed (2), deleted (4) or viewed (5), or the contacts     *
      * can be maintained.                                          *
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
     C                   READC     OMH01101
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
     C                             OR FMOPT1 = ' 7'
     C                             OR FMOPT1 = ' 8'
     C                             OR FMOPT1 = '12'
      *
      *          ------------------------------------------
      *          Option is valid
      *          ------------------------------------------
     C     FMCUSC        SETLL     OMCUSL1
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
     C                   UPDATE    OMH01101
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   UPDATE    OMH01101
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH01101
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
     C                             AND FMPOSI <> *BLANKS
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
     C                   READC     OMH01101
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
     C                   UPDATE    OMH01101
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
     C                   WHEN      W#OPT1 = ' 7'
     C                   EXSR      SRSC07
     C                   WHEN      W#OPT1 = ' 8'
     C                   EXSR      SRMEXT
     C                   WHEN      W#OPT1 = '12'
     C                   EXSR      SRCONT
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Read next changed if no end requested.
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                   LEAVE
     C                   ELSE                                                   X03
     C                   READC     OMH01101
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
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRMROP - Display more options routine                       *
      *                                                             *
      * This routine displays more options when the users presses   *
      * F23 by toggling *IN83.                                      *
      *                                                             *
      *  83 on  : Start at option 2                                 *
      *  83 off : Display other options                             *
      * ----------------------------------------------------------- *
     C     SRMROP        BEGSR
     C                   IF        *IN83 = *ON
     C                   EVAL      *IN83 = *OFF
     C                   ELSE
     C                   EVAL      *IN83 = *ON
     C                   ENDIF
      *          ------------------------------------------
      *          Make sure the same page is redisplayed
      *          Cursor inside subfile -> FMRRN
      *          Cursor outside subfile -> D#CRC#
      *          ------------------------------------------
      *          FMRRN     IFNE 0
      *                    Z-ADDFMRRN     CRRN01
      *                    ELSE
      *                    Z-ADDD#CRC#    CRRN01
      *                    ENDIF
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
     C                   EXFMT     OMH01104
     C                   IF        *INKC = *ON
     C                   EVAL      P#STAT = C#EXIT
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
      * tor 80 and 81. If you want to allow change of the keyfield  *
      * CUSC as well, you only have to change the displayfile       *
      * 'B' instead of 'O'                                          *
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
     C                   MOVE      *BLANKS       FMUSF1
     C                   MOVE      *BLANKS       FMUSF2
     C                   MOVE      *ZEROS        FMUSF3
     C                   MOVE      *BLANKS       FMNAMD
     C                   MOVE      *BLANKS       FMADST
     C                   MOVE      *BLANKS       FMZIPC
     C                   MOVE      *BLANKS       FMCUSC
     C                   MOVE      *BLANKS       FMNAMD
     C                   MOVE      *BLANKS       FMTEL#
     C                   MOVE      *BLANKS       FMCOUD
     C                   MOVE      *BLANKS       FMADRE
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Get the file if no addition, save keyfield
      *          ------------------------------------------
     C     FMCUSC        CHAIN(N)  OMCUSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL     USUSF1        FMUSF1
     C                   MOVEL     USUSF2        FMUSF2
     C                   Z-ADD     USUSF3        FMUSF3
     C                   MOVEL     USNAMD        FMNAMD
     C                   MOVEL     USADST        FMADST
     C                   MOVEL     USZIPC        FMZIPC
     C                   MOVEL     USCUSC        FMCUSC
     C                   MOVEL     USCUSC        S#CUSC
     C                   MOVEL     USCOUD        FMCOUD
     C                   MOVEL     USADRE        FMADRE
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
     C                   MOVE      *BLANKS       FMUSF1
     C                   MOVE      *BLANKS       FMUSF2
     C                   EVAL      FMUSF3 = 0
     C                   MOVE      *BLANKS       FMNAMD
     C                   MOVE      *BLANKS       FMADST
     C                   MOVE      *BLANKS       FMZIPC
     C                   MOVE      *BLANKS       FMCUSC
     C                   MOVE      *BLANKS       FMCOUD
     C                   MOVE      *BLANKS       FMADRE
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Refresh screen
      *          ------------------------------------------
     C                   MOVEL     USUSF1        FMUSF1
     C                   MOVEL     USUSF2        FMUSF2
     C                   Z-ADD     USUSF3        FMUSF3
     C                   MOVEL     USNAMD        FMNAMD
     C                   MOVEL     USADST        FMADST
     C                   MOVEL     USZIPC        FMZIPC
     C                   MOVEL     USCUSC        FMCUSC
     C                   MOVEL     USCUSC        S#CUSC
     C                   MOVEL     USCOUD        FMCOUD
     C                   MOVEL     USADRE        FMADRE
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
     C                   IF        FMCUSC <> S#CUSC                             B02
     C     FMCUSC        SETLL     OMCUSL1
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
     C                   IF        FMCUSC = *BLANKS                             B02
     C                   EVAL      MSGDTA = VAR(2)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN72 = *ON
     C                   EVAL      EROR02 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if name is filled
      *          ------------------------------------------
     C                   IF        FMNAMD = *BLANKS                             B02
     C                   EVAL      MSGDTA = VAR(3)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN73 = *ON
     C                   EVAL      EROR02 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if the customer code exists.
      *          ------------------------------------------
     C                   IF        FMCUSC <> S#CUSC                             B02
     C     FMCUSC        SETLL     OMCUSL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B03
     C                   EVAL      MSGID = 'FILE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN70 = *ON
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Do the user field checking if the program exists
      *          ------------------------------------------
     C                   IF        I#CUPG = *ON                                 B02
      *
      *                    --------------------------------
      *                    Call the program in the correct mode
      *                    --------------------------------
     C                   IF        *IN80 = *OFF
     C                             AND *IN81 = *OFF
     C                   EVAL      P#ACTC = '60'
     C                   ENDIF
     C                   IF        *IN80 = *ON
     C                             AND *IN81 = *OFF
     C                   EVAL      P#ACTC = '20'
     C                   ENDIF
      *
     C                   EVAL      P#CUSC = FMCUSC
     C                   EVAL      P#CCTC = *BLANKS
     C                   EVAL      P#USF1 = FMUSF1
     C                   EVAL      P#F1ST = '*NORM'
     C                   EVAL      P#USF2 = FMUSF2
     C                   EVAL      P#F2ST = '*NORM'
     C                   EVAL      P#USF3 = FMUSF3
     C                   EVAL      P#F3ST = '*NORM'
     C                   EVAL      P#STAT = '*NORM'
     C                   CALLP     OMQUSRCU ( P#CUSC : P#CCTC : P#ACTC :
     C                             P#USF1 : P#F1ST : P#USF2 : P#F2ST :
     C                             P#USF3 : P#F3ST : P#STAT )
      *
      *                    --------------------------------
      *                    If exit then exist.
      *                    --------------------------------
     C                   IF        P#STAT = C#EXIT                              B03
     C                   EVAL      *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
      *
      *                    --------------------------------
      *                    Check the fields.
      *                    --------------------------------
     C                   EVAL      *IN74 = (P#F1ST <> C#NORM)
     C                   IF        *IN74 = *ON
     C                   EVAL      EROR02 = *ON
     C                   ENDIF
     C                   MOVE      P#USF1        FMUSF1
     C                   EVAL      *IN75 = (P#F2ST <> C#NORM)
     C                   IF        *IN75 = *ON
     C                   EVAL      EROR02 = *ON
     C                   ENDIF
     C                   MOVE      P#USF1        FMUSF1
     C                   EVAL      *IN76 = (P#F3ST <> C#NORM)
     C                   IF        *IN76 = *ON
     C                   EVAL      EROR02 = *ON
     C                   ENDIF
      *
      *                    --------------------------------
      *                    Fill the fields with their new values
      *                    --------------------------------
     C                   MOVE      P#USF1        FMUSF1
     C                   MOVE      P#USF2        FMUSF2
     C                   MOVE      P#USF3        FMUSF3
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
     C     FMCUSC        DELETE    OMCUSR
     C                   EVAL      *IN99 = NOT %FOUND
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
     C                   IF        FMCUSC <> S#CUSC                             B04
     C     S#CUSC        DELETE    OMCUSR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Get the file
      *          ------------------------------------------
     C     FMCUSC        CHAIN     OMCUSL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill the file
      *          ------------------------------------------
     C                   MOVEL     FMUSF1        USUSF1
     C                   MOVEL     FMUSF2        USUSF2
     C                   Z-ADD     FMUSF3        USUSF3
     C                   MOVEL     FMNAMD        USNAMD
     C                   MOVEL     FMADST        USADST
     C                   MOVEL     FMZIPC        USZIPC
     C                   MOVEL     FMCUSC        USCUSC
     C                   MOVEL     FMCOUD        USCOUD
     C                   MOVEL     FMADRE        USADRE
      *
      *          ------------------------------------------
      *          Write or update
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMCUSR
     C                   ENDIF
     C                   IF        *IN99 = *ON
     C                   WRITE     OMCUSR
     C                   MOVEL     FMCUSC        S#CUSC
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
      * SRSC07 - Rename customer
      *
      * This routine calls the rename customer program              *
      *
      * ----------------------------------------------------------- *
     C     SRSC07        BEGSR
      *
      *          ------------------------------------------
      *          Do the call
      *          ------------------------------------------
     C     FMCUSC        CHAIN(N)  OMCUSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL     USCUSC        P#CUSC
     C                   MOVEL     USNAMD        P#NAMD
     C                   CALLP     OMH047 ( P#CUSC : P#NAMD : P#STAT )
     C                   SELECT
     C                   WHEN      P#STAT = C#EXIT
     C                   EXSR      SRTTLR
     C                   ENDSL
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRMEXT - Mail extensions                                    *
      *                                                             *
      * This routine calls the mail extensions maintenance program  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRMEXT        BEGSR
      *
      *          ------------------------------------------
      *          Do the call
      *          ------------------------------------------
     C                   CALLP     OMH041C ( FMCUSC : P#STAT )
      *
      *          ------------------------------------------
      *          If return status is exit, then exit
      *          ------------------------------------------
     C                   IF        P#STAT = C#EXIT                              B01
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
      * SRCONT - Contacts maintenance                               *
      *                                                             *
      * This routine calls the contacts maintenance program         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCONT        BEGSR
      *
      *          ------------------------------------------
      *          Do the call
      *          ------------------------------------------
     C                   MOVEL     FMCUSC        P#CUSC
     C                   CALLP     OMH012 ( P#CUSC : P#CCTC : P#STAT )
      *
      *          ------------------------------------------
      *          If return status is exit, then exit
      *          ------------------------------------------
     C                   IF        P#STAT = C#EXIT                              B01
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          If return status is Cancel, then force end
      *          of subfile processing
      *          ------------------------------------------
     C                   IF        P#STAT = C#CNCL                              B02
     C                   EVAL      *INKL = *ON
     C                   ENDIF                                                  E02
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
      *          Savefield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Keyfield definition
      *          ------------------------------------------
     C                   MOVE      *BLANKS       K#CUSC
     C                   MOVE      '1'           K#CNTI
     C                   MOVE      *BLANKS       FMPOSI
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
      *          Parameter list for OMH012 (contact maintenance)
      *          ------------------------------------------
     C     $MH012        PLIST
     C                   PARM                    P#CUSC
     C                   PARM                    P#CCTC
     C                   PARM                    P#STAT
      *
      *
      *          ------------------------------------------
      *          Parameter list for OMH047 (Rename customer)
      *          ------------------------------------------
     C     $MH047        PLIST
     C                   PARM                    P#CUSC
     C                   PARM                    P#NAMD
     C                   PARM                    P#STAT
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
      *          Parameter list for OMH906C (Check object)
      *          ------------------------------------------
     C     $X906C        PLIST
     C                   PARM                    P#OBJC
     C                   PARM                    P#OBJL
     C                   PARM                    P#OBJT
     C                   PARM                    OBJD
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH029 (Get info)
      *          ------------------------------------------
     C     $MH029        PLIST
     C                   PARM                    P#OPTI
     C                   PARM      *BLANKS       P#HEDC
     C                   PARM                    P#SUL#
     C                   PARM                    P#STAT
     C                   PARM                    D#D7HD
     C                   PARM                    D#D8HD
     C                   PARM      C#NO          P#CIM
      *
      *          ------------------------------------------
      *          OMCCOL2 Customer Contact positioning
      *          ------------------------------------------
     C     $CCOK1        KLIST
     C                   KFLD                    USCUSC
     C                   KFLD                    K#CNTI
      *
      *          ------------------------------------------
      *          Retrieve authorization
      *          ------------------------------------------
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = 'Y'
     C                   EVAL      P#STAT = C#NORM
     C                   EVAL      P#HEDC = *BLANKS
     C                   EVAL      P#CIM = C#NO
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD : P#CIM )
      *
      *          ------------------------------------------
      *          Continue if authorization ok
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B01
      *
      *          ------------------------------------------
      *          Fill screen fields from parameters
      *          ------------------------------------------
      *************        MOVE P#HEDC    FMHEDC
      *************        MOVELD8HEDD    FMHEDD
      *
      *          ------------------------------------------
      *          Get message data
      *          ------------------------------------------
     C                   EXSR      SRGVAR
      *
      *          ------------------------------------------
      *          Check user fields
      *          ------------------------------------------
     C     $USRCU        PLIST
     C                   PARM      FMCUSC        P#CUSC
     C                   PARM      *BLANKS       P#CCTC
     C                   PARM                    P#ACTC
     C                   PARM      FMUSF1        P#USF1
     C                   PARM      '*NORM'       P#F1ST
     C                   PARM      FMUSF2        P#USF2
     C                   PARM      '*NORM'       P#F2ST
     C                   PARM      FMUSF3        P#USF3
     C                   PARM      '*NORM'       P#F3ST
     C                   PARM      '*NORM'       P#STAT
      *
      *          ------------------------------------------
      *          Check if the exit program exists
      *          ------------------------------------------
     C                   EVAL      I#CUPG = *OFF
     C                   EVAL      P#OBJC = 'OMQUSRCU'
     C                   EVAL      P#OBJL = '*LIBL'
     C                   EVAL      P#OBJT = '*PGM'
     C                   CALLP     OMH906C ( P#OBJC : P#OBJL : P#OBJT :
     C                             OBJD : P#STAT )
     C                   IF        P#STAT = C#NORM                              B02
     C                   EVAL      I#CUPG = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   EVAL      P#STAT = '*NORM'
     C                   ENDIF                                                  E01
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
      *
     C                   WHEN      E#FILE = 'OMCUSL1 '                          X02
     C                   MOVEL     CUSL1         P#FILE
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
OPT0056  -  '2', '4', '5'...                  1
HLP0008  -  Code                              2
HLP0011  -  Name                              3
