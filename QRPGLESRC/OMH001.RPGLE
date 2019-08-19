      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH001                                        *
      * Function ...: Help desk maintenance                         *
      * Module .....: Help desk                                     *
      *                                                             *
      * Parameters .: *NONE                                         *
      *                                                             *
      * This program maintains the help desk file                   *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators                                             *
      * ----------------------------------------------------------- *
      *                                                             *
      * 70-79 - Screen field error indicators                       *
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
     FOMHDDL1   UF A E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HDDL1)
     FOMHDEL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HDEL1)
     FOMINCL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(INCL1)
     FOMH001D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
     F                                     SFILE(OMH00101:RRN01)
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
     D HDDL1           DS            57
     D HDEL1           DS            57
     D INCL1           DS            57
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
     D  #SPGML                81     90
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
     D C#QHTM          C                   CONST('QHTMLSRC')
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
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
      * Prototype for 'OMH905C'
     D OMH905C         PR                  EXTPGM('OMH905C')
     D P#USID_                             LIKE(P#USID)
     D P#USCL_                             LIKE(P#USCL)
     D P#GRID_                             LIKE(P#GRID)
     D P#GRCL_                             LIKE(P#GRCL)
     D P#USDC_                             LIKE(P#USDC)
     D P#STAT_                             LIKE(P#STAT)
      *
      * Prototype for 'OMH909C'
     D OMH909C         PR                  EXTPGM('OMH909C')
     D P#OBJL_                             LIKE(P#OBJL)
     D P#OBJC_                             LIKE(P#OBJC)
     D P#MBRC_                             LIKE(OBJC)
     D P#OBJT_                             LIKE(P#OBJT)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH029C (Retrieve HD)
      * Prototype for 'OMH033C'
     D OMH033C         PR                  EXTPGM('OMH033C')
     D FMHEDC_                             LIKE(FMHEDC)
     D P#SRCF_                             LIKE(P#SRCF)
     D P#SRCL_                             LIKE(P#SRCL)
     D P#SRCM_                             LIKE(P#SRCM)
     D P#CUDI_                             LIKE(P#CUDI)
     D P#CODI_                             LIKE(P#CODI)
     D P#CONI_                             LIKE(P#CONI)
     D P#COEI_                             LIKE(P#COEI)
     D P#HEDI_                             LIKE(P#HEDI)
     D P#CSCI_                             LIKE(P#CSCI)
      *
      *          ------------------------------------------
      *          OMHDDL1 Help Desk File
      * Prototype for 'OMH040'
     D OMH040          PR                  EXTPGM('OMH040')
     D FMHEDC_                             LIKE(FMHEDC)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH002'
     D OMH002          PR                  EXTPGM('OMH002')
     D FMHEDC_                             LIKE(FMHEDC)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH029'
     D OMH029          PR                  EXTPGM('OMH029')
     D P#OPTI_                             LIKE(P#OPTI)
     D P#HEDC_                             LIKE(HEDC)
     D P#SUL#_                             LIKE(P#SUL#)
     D P#STAT_                             LIKE(P#STAT)
     D D#D7HD_                             LIKE(D#D7HD)
      *
      *          ------------------------------------------
      *          Parameter list for OMH033C (Create HTML page)
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
     D s#rr01          S              5P 0
     D EOF             S                   LIKE(*IN99)
     D EROR01          S                   LIKE(*IN99)
     D EROR02          S                   LIKE(*IN99)
     D EROR03          S                   LIKE(*IN99)
     D FMUSID          S             10
     D GRID            S             10
     D I#PSSR          S              1
     D IX              S              5  0
     D K#HEDC          S                   LIKE(DDHEDC)
     D LOOP01          S                   LIKE(*IN99)
     D LOOP02          S                   LIKE(*IN99)
     D LOOP03          S                   LIKE(*IN99)
     D MA_X            S              5  0
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#CODI          S                   LIKE(INDI)
     D P#COEI          S                   LIKE(INDI)
     D P#CONI          S                   LIKE(INDI)
     D P#CSCI          S                   LIKE(INDI)
     D P#CUDI          S                   LIKE(INDI)
     D P#FILE          S             57
     D P#GRCL          S             10
     D P#GRID          S             10
     D P#HEDC          S                   LIKE(HEDC)
     D P#HEDI          S                   LIKE(INDI)
     D P#MBRC          S                   LIKE(OBJC)
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#OBJC          S                   LIKE(OBJC)
     D P#OBJL          S                   LIKE(OBJL)
     D P#OBJT          S                   LIKE(OBJT)
     D P#OPTI          S                   LIKE(INDI)
     D P#PMQC          S             10
     D P#SRCF          S                   LIKE(SRCF)
     D P#SRCL          S                   LIKE(SRCL)
     D P#SRCM          S                   LIKE(SRCM)
     D P#STAT          S                   LIKE(STAT)
     D P#SUL#          S                   LIKE(SUL#)
     D P#USCL          S             10
     D P#USDC          S             50
     D P#USID          S             10
     D RRN01           S              5  0
     D S#IN40          S                   LIKE(*IN40)
     D SEL             S                   LIKE(*IN99)
     D USCL            S              7
     D W#CRC1          S              5  0
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
     C                   CALLP     OMH901c ( MSGID : MSGF : MSGLIB :
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
      * SRSC01 - Work with help deskd                               *
      *                                                             *
      * This routine displays the first screen where the user can   *
      * select a help desk for maintenance actions.                 *
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
     C                   WRITE     OMH00103
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        RRN01 = *ZEROS                               B03
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   EXFMT     OMH00102
     C                   IF        *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
     C                   IF        *INKL = *ON
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
      *
      *          ------------------------------------------
      *          Command key processing.
      *          F23=More options
      *          ------------------------------------------
     C                   IF        *IN23 = *ON                                  B04
     C                   IF        *IN41 = *ON
     C                   EVAL      *IN41 = *OFF
     C                   ELSE
     C                   EVAL      *IN41 = *ON
     C                   ENDIF
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Do edit routine
      *          ------------------------------------------
     C                   EXSR      SRED01
     C                   ENDIF                                                  E05
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
      * SRIN01 - Work with help desks init                          *
      *                                                             *
      * Init the work with help desks display.                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMPOSI
     C                   EVAL      *IN41 = *OFF
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
      * SRRD01 - Read next Help Desk Record                         *
      *                                                             *
      * This routine reads the next help desk record                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRD01        BEGSR
      *
      *          ------------------------------------------
      *          Do it
      *          ------------------------------------------
     C                   READ(N)   OMHDDL1
     C                   EVAL      *IN99 = %EOF
     C                   EVAL      EOF = *IN99
      *
      *          ------------------------------------------
      *          Save the key.
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF
     C                   MOVE      DDHEDC        K#HEDC
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSF01 - Build work with help desks subfile                 *
      *                                                             *
      * This routine builds one page of subfile records             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSF01        BEGSR
      *
      *          ------------------------------------------
      *          Read first interface record
      *          ------------------------------------------
     C     $HDDK2        SETLL     OMHDDL1
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
     C                   MOVE      DDHEDC        FMHEDC
     C                   MOVE      DDHEDD        FMHEDD
     C                   MOVE      DDHDMC        FMHDMC
     C                   MOVEL     DDCCTN        FMCCTN
     C                   MOVEL     DDCINC        FMCINC
     C                   MOVEL     DDCTFP        FMCTFP
     C                   MOVEL     DDCTFL        FMCTFL
     C                   MOVEL     DDINFP        FMINFP
     C                   MOVEL     DDINFL        FMINFL
     C                   Z-ADD     DDICTN        FMICTN
     C                   Z-ADD     DDIINC        FMIINC
     C                   MOVEL     DDISFL        FMISFL
     C                   ADD       1             RRN01
     C                   ADD       1             IX
     C                   WRITE     OMH00101
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
      * SRRF01 - Work with help desk refresh                        *
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
      *          Fill the key.
      *          ------------------------------------------
     C                   MOVE      FMPOSI        K#HEDC
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH00102
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
      * in the screen. The next ten records are read.               *
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
      * SRED01 - Work with help desks refresh                       *
      *                                                             *
      * This routine checks if the option fields in the subfile     *
      * where correctly filled.                                     *
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
     C                   READC     OMH00101
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
     C                             OR FMOPT1 = ' 3'
     C                             OR FMOPT1 = ' 4'
     C                             OR FMOPT1 = ' 5'
     C                             OR FMOPT1 = ' 6'                             HTML
     C                             OR FMOPT1 = ' 8'                             Mail ext.
     C                             OR FMOPT1 = '12'                             Employees
      *
      *          ------------------------------------------
      *          Option is valid
      *          ------------------------------------------
     C                   IF        W#OPT1 <> ' 3'                               B05
     C     $HDDK1        SETLL     OMHDDL1
     C                   EVAL      *IN99 = %EQUAL
     C                   ELSE                                                   X05
     C                   EVAL      *IN99 = *ON
     C                   ENDIF                                                  E05
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
     C                   MOVEL     FMOPT1        MSGDTA
     C                   EVAL      MSGID = 'OPTE021'
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
     C                   UPDATE    OMH00101
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   UPDATE    OMH00101
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH00101
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
     C                   MOVE      *BLANKS       FMPOSI
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
      * SRPR01 - Work with help desks refresh                       *
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
     C                   READC     OMH00101
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
     C                   UPDATE    OMH00101
      *
      *          ------------------------------------------
      *          If option 3, then seton the add key.
      *          ------------------------------------------
     C                   IF        W#OPT1 = ' 3'                                B03
     C                   EVAL      *INKF = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Do action if option is 2, 3, 4, 5 or 12
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      W#OPT1 = ' 2'
     C                   EXSR      SRSC02
     C                   WHEN      W#OPT1 = ' 3'
     C                   EXSR      SRSC02
     C                   WHEN      W#OPT1 = ' 4'
     C                   EXSR      SRSC02
     C                   WHEN      W#OPT1 = ' 5'
     C                   EXSR      SRSC02
     C                   WHEN      W#OPT1 = ' 6'
     C                   EXSR      SRSC03
     C                   WHEN      W#OPT1 = ' 8'
     C                   EXSR      SRMEXT
     C                   WHEN      W#OPT1 = '12'
     C                   EXSR      SRWEMP
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Read next changed if no end requested.
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                   LEAVE
     C                   ELSE                                                   X03
     C                   READC     OMH00101
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
     C                   EXFMT     OMH00104
     C                   IF        *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Clear message queue
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          If prompt caused 40 to be turned off, then it is saved
      *          ------------------------------------------
     C                   IF        *IN40 = *OFF                                 B03
     C                   EVAL      *IN40 = S#IN40
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If prompt then save 40.
      *          ------------------------------------------
     C                   EVAL      S#IN40 = *INKD
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
      *          F4=Prompt
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   EVAL      EROR02 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Command key processing.
      *          F5=Refresh
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B04
     C                   EXSR      SRRF02
     C                   EVAL      EROR02 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Do edit routine if not F12.
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B05
     C                   EXSR      SRED02
     C                   ENDIF                                                  E05
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
     C                   IF        W#OPT1 <> ' 3'                               B02
     C                   MOVE      *BLANKS       FMHEDC
     C                   MOVE      *BLANKS       FMHEDD
     C                   MOVE      *BLANKS       FMISFL
     C                   MOVEL     *BLANKS       FMHDMC
     C                   MOVEL     *BLANKS       FMCCTN
     C                   MOVEL     *BLANKS       FMCINC
     C                   MOVEL     *BLANKS       FMCTFP
     C                   MOVEL     *BLANKS       FMCTFL
     C                   MOVEL     *BLANKS       FMINFP
     C                   MOVEL     *BLANKS       FMINFL
     C                   EVAL      FMICTN = 0
     C                   EVAL      FMIINC = 0
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Get the file if no addition
      *          ------------------------------------------
     C     $HDDK1        CHAIN(N)  OMHDDL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      DDHEDC        FMHEDC
     C                   MOVE      DDHEDD        FMHEDD
     C                   MOVE      DDHDMC        FMHDMC
     C                   MOVE      DDISFL        FMISFL
     C                   MOVEL     DDCCTN        FMCCTN
     C                   MOVEL     DDCINC        FMCINC
     C                   MOVEL     DDCTFP        FMCTFP
     C                   MOVEL     DDCTFL        FMCTFL
     C                   MOVEL     DDINFP        FMINFP
     C                   MOVEL     DDINFL        FMINFL
     C                   Z-ADD     DDICTN        FMICTN
     C                   Z-ADD     DDIINC        FMIINC
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
      * This screen is called for future processing.                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF02        BEGSR
      *
      *                    --------------------------------
      *                    Add, no copy: clear the fields
      *                    --------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C                             AND W#OPT1 <> ' 3'                           B02
     C                   MOVE      *BLANKS       FMHEDC
     C                   MOVE      *BLANKS       FMHEDD
     C                   MOVE      *BLANKS       FMISFL
     C                   MOVEL     *BLANKS       FMHDMC
     C                   MOVEL     *BLANKS       FMCCTN
     C                   MOVEL     *BLANKS       FMCINC
     C                   MOVEL     *BLANKS       FMCTFP
     C                   MOVEL     *BLANKS       FMCTFL
     C                   MOVEL     *BLANKS       FMINFP
     C                   MOVEL     *BLANKS       FMINFL
     C                   EVAL      FMICTN = 0
     C                   EVAL      FMIINC = 0
     C                   ELSE                                                   X01
      *
      *                    --------------------------------
      *                    Copy: get the file
      *                    --------------------------------
     C                   IF        W#OPT1 = ' 3'                                B02
     C     $HDDK1        CHAIN(N)  OMHDDL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF
      *
      *                    --------------------------------
      *                    Refresh screen
      *                    --------------------------------
     C                   MOVE      DDHEDC        FMHEDC
     C                   MOVE      DDHEDD        FMHEDD
     C                   MOVE      DDHDMC        FMHDMC
     C                   MOVE      DDISFL        FMISFL
     C                   MOVEL     DDCCTN        FMCCTN
     C                   MOVEL     DDCINC        FMCINC
     C                   MOVEL     DDCTFP        FMCTFP
     C                   MOVEL     DDCTFL        FMCTFL
     C                   MOVEL     DDINFP        FMINFP
     C                   MOVEL     DDINFL        FMINFL
     C                   Z-ADD     DDICTN        FMICTN
     C                   Z-ADD     DDIINC        FMIINC
     C                   ENDIF
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
      * This routine checks if the several Function attributes are  *
      * filled.                                                     *
      * exsists in the security file.                               *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED02        BEGSR
      *
      *          ------------------------------------------
      *          Check if deletion is allowed
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B01
     C                             AND *IN81 = *ON
     C                             AND *IN82 = *OFF
     C     FMHEDC        CHAIN     OMHDER
     C                   EVAL      *IN98 = NOT %FOUND
     C     FMHEDC        CHAIN     OMINCR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN98 = *OFF                                 B02
     C                             OR *IN99 = *OFF
     C                   EVAL      MSGID = 'DLT0030'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   ENDIF                                                  E01
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Test if addition.
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
      *
      *          ------------------------------------------
      *          Check if the help desk code exists.
      *          ------------------------------------------
     C     FMHEDC        SETLL     OMHDDR
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      MSGID = 'FILE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN70 = *ON
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Test if not delete
      *          ------------------------------------------
     C                   IF        *IN81 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Check if code is filled
      *          ------------------------------------------
     C                   IF        FMHEDC = *BLANKS                             B02
     C                   EVAL      MSGDTA = VAR(2)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR02 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if description is filled
      *          ------------------------------------------
     C                   IF        FMHEDD = *BLANKS                             B02
     C                   EVAL      MSGDTA = VAR(3)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN71 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if manager is filled
      *          ------------------------------------------
     C                   IF        FMHDMC = *BLANKS                             B02
     C                   EVAL      MSGDTA = VAR(4)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN72 = *ON
     C                   EVAL      EROR02 = *ON
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Check if the user id. exists
      *          ------------------------------------------
     C                   MOVEL     FMHDMC        P#USID
     C                   CALLP     OMH905C ( P#USID : P#USCL : P#GRID :
     C                             P#GRCL : P#USDC : P#STAT )
     C                   IF        P#STAT <> C#NORM                             B03
     C                   EVAL      MSGID = 'USRE003'
     C                   EVAL      *IN72 = *ON
     C                   EVAL      EROR02 = *ON
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if call track number is filled
      *          ------------------------------------------
     C                   IF        FMCCTN = *BLANKS                             B02
     C                   EVAL      MSGDTA = VAR(5)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN73 = *ON
     C                   EVAL      EROR02 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Increment may not be zero
      *          ------------------------------------------
     C                   IF        FMICTN <= 0                                  B02
     C                   EVAL      MSGDTA = VAR(7)
     C                   EVAL      MSGID = 'ZERE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN74 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if incident number is filled
      *          ------------------------------------------
     C                   IF        FMCINC = *BLANKS                             B02
     C                   EVAL      MSGDTA = VAR(6)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN75 = *ON
     C                   EVAL      EROR02 = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Increment may not be zero
      *          ------------------------------------------
     C                   IF        FMIINC <= 0                                  B02
     C                   EVAL      MSGDTA = VAR(7)
     C                   EVAL      MSGID = 'ZERE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR02 = *ON
     C                   EVAL      *IN76 = *ON
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
     C                   EVAL      LOOP02 = *IN40
      *
      *          ------------------------------------------
      *          Delete the file if wanted.
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B01
     C                             AND *IN81 = *ON
     C                   IF        *IN82 = *OFF                                 B02
     C     $HDDK1        DELETE    OMHDDR
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
      *          Get the file
      *          ------------------------------------------
     C     $HDDK1        CHAIN     OMHDDL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill the file
      *          ------------------------------------------
     C                   MOVE      FMHEDC        DDHEDC
     C                   MOVE      FMHEDD        DDHEDD
     C                   MOVE      FMHDMC        DDHDMC
     C                   MOVE      FMISFL        DDISFL
     C                   MOVEL     FMCCTN        DDCCTN
     C                   MOVEL     FMCINC        DDCINC
     C                   MOVEL     FMCTFP        DDCTFP
     C                   MOVEL     FMCTFL        DDCTFL
     C                   MOVEL     FMINFP        DDINFP
     C                   MOVEL     FMINFL        DDINFL
     C                   Z-ADD     FMICTN        DDICTN
     C                   Z-ADD     FMIINC        DDIINC
      *
      *          ------------------------------------------
      *          Add or update the file.
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B03
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMHDDR
     C                   ENDIF
     C                   ELSE                                                   X03
     C                   IF        *IN99 = *ON
     C                   WRITE     OMHDDR
     C                   ENDIF
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Force UPDATE mode if in ADD mode.
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B03
     C                             AND *IN81 = *OFF
     C                   EVAL      *IN80 = *ON
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
      * SRSC03 - Create HTML form                                   *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC03        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EXSR      SRIN03
      *
      *          ------------------------------------------
      *          Do while no end
      *          ------------------------------------------
     C                   DOU       LOOP03 = *OFF                                B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR03 = *OFF                                B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the screen
      *          ------------------------------------------
     C                   EXFMT     OMH00105
     C                   IF        *INKC = *ON
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
     C                   EVAL      *IN(77) = *OFF
     C                   EVAL      *IN(78) = *OFF
     C                   EVAL      *IN(79) = *OFF
     C                   EVAL      EROR03 = *OFF
     C                   EVAL      LOOP03 = *OFF
      *
      *          ------------------------------------------
      *          Command key processing.
      *          F5=Refresh
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B04
     C                   EXSR      SRRF03
     C                   EVAL      EROR03 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Do edit routine if not F12.
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B05
     C                   EXSR      SRED03
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
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
     C                   EXSR      SRPR03
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
      * SRIN03 - Screen 3 initialisation                            *
      *                                                             *
      * This routine blanks the input fields on the screen.         *
      * Pending on the required action.                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN03        BEGSR
      *
      *          ------------------------------------------
      *          Do refresh
      *          ------------------------------------------
     C                   EXSR      SRRF03
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF03 - Screen 3 refresh                                   *
      *                                                             *
      * Refresh dynamic fields                                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF03        BEGSR
      *
      *          ------------------------------------------
      *          Refesh the fields
      *          ------------------------------------------
     C                   MOVEL     C#QHTM        FMSRCF
     C                   MOVEL     #SPGML        FMSRCL
     C                   MOVEL(P)  FMHEDC        FMSRCM
     C                   MOVE      *ON           FMCUDI
     C                   MOVE      *ON           FMCODI
     C                   MOVE      *ON           FMCONI
     C                   MOVE      *ON           FMCOEI
     C                   MOVE      *ON           FMHEDI
     C                   MOVE      *ON           FMCSCI
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED03 - Screen 3 edit                                      *
      *                                                             *
      * This routine checks if the several atributes are correctly  *
      * filled.                                                     *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED03        BEGSR
      *
      *          ------------------------------------------
      *          The source file must exist.
      *          ------------------------------------------
     C                   MOVEL     FMSRCL        P#OBJL
     C                   MOVEL     FMSRCF        P#OBJC
     C                   EVAL      P#MBRC = *BLANKS
     C                   EVAL      P#OBJT = '*FILE'
     C                   CALLP     OMH909C ( P#OBJL : P#OBJC : P#MBRC :
     C                             P#OBJT : P#STAT )
     C                   IF        P#STAT <> C#NORM
     C                   EVAL      *IN70 = *ON
     C                   EVAL      *IN71 = *ON
     C                   EVAL      MSGID = 'OBJE002'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR03 = *ON
     C                   ELSE                                                   E01
      *
      *          ------------------------------------------
      *          The member may not exist if not F22.
      *          ------------------------------------------
     C                   MOVEL     FMSRCM        P#MBRC
     C                   EVAL      P#OBJT = '*FILE'
     C                   CALLP     OMH909C ( P#OBJL : P#OBJC : P#MBRC :
     C                             P#OBJT : P#STAT )
     C                   IF        P#STAT = C#NORM
     C                             AND *INKW = *OFF
     C                   EVAL      *IN72 = *ON
     C                   EVAL      MSGID = 'MBRE011'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR03 = *ON
     C                   ENDIF                                                  E01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Customer pull down must be '0' or '1'
      *          ------------------------------------------
     C                   IF        FMCUDI <> *ON
     C                             AND FMCUDI <> *OFF
     C                   EVAL      *IN73 = *ON
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR03 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Contact pull down must be '0' or '1'
      *          ------------------------------------------
     C                   IF        FMCODI <> *ON
     C                             AND FMCODI <> *OFF
     C                   EVAL      *IN74 = *ON
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR03 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Contact name must be '0' or '1'
      *          ------------------------------------------
     C                   IF        FMCONI <> *ON
     C                             AND FMCONI <> *OFF
     C                   EVAL      *IN75 = *ON
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR03 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Contact e-mail must be '0' or '1'
      *          ------------------------------------------
     C                   IF        FMCOEI <> *ON
     C                             AND FMCOEI <> *OFF
     C                   EVAL      *IN76 = *ON
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR03 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Employees drop down must be '0' or '1'
      *          ------------------------------------------
     C                   IF        FMHEDI <> *ON
     C                             AND FMHEDI <> *OFF
     C                   EVAL      *IN77 = *ON
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR03 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Call source code must be '0' or '1'
      *          ------------------------------------------
     C                   IF        FMCSCI <> *ON
     C                             AND FMCSCI <> *OFF
     C                   EVAL      *IN78 = *ON
     C                   EVAL      MSGID = 'CHO0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR03 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR03 - Process screen 3                                   *
      *                                                             *
      * This routine does the creation.                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR03        BEGSR
      *
      *          ------------------------------------------
      *          Call the HTML creator
      *          ------------------------------------------
     C                   EVAL      P#SRCF = FMSRCF
     C                   EVAL      P#SRCL = FMSRCL
     C                   EVAL      P#SRCM = FMSRCM
     C                   EVAL      P#CUDI = FMCUDI
     C                   EVAL      P#CODI = FMCODI
     C                   EVAL      P#CONI = FMCONI
     C                   EVAL      P#COEI = FMCOEI
     C                   EVAL      P#HEDI = FMHEDI
     C                   EVAL      P#CSCI = FMCSCI
     C                   CALLP     OMH033C ( FMHEDC : P#SRCF : P#SRCL :
     C                             P#SRCM : P#CUDI : P#CODI : P#CONI :
     C                             P#COEI : P#HEDI : P#CSCI )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRMEXT - Work with mail extensions                          *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRMEXT        BEGSR
      *
      *          ------------------------------------------
      *          Mail extensions
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH040 ( FMHEDC : P#STAT )
      *
      *          ------------------------------------------
      *          Exit? then exit
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      P#STAT = '*EXIT'
     C                   EXSR      SRTTLR
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Cancel
      *          ------------------------------------------
     C                   IF        P#STAT = '*CNCL'
     C                   EVAL      *INKL = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRWEMP - Work with employees                                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRWEMP        BEGSR
      *
      *          ------------------------------------------
      *          Work with employees
      *          ------------------------------------------
     C                   CALLP     OMH002 ( FMHEDC : P#STAT )
      *
      *          ------------------------------------------
      *          Exit? then exit
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      P#STAT = '*EXIT'
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
      *
      *          ------------------------------------------
      *          Keyfield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      W#VALC = *ZERO
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
      *          Parameter list for OMH909C (Check object)
      *          ------------------------------------------
     C     $X909C        PLIST
     C                   PARM                    P#OBJL
     C                   PARM                    P#OBJC
     C                   PARM                    P#MBRC
     C                   PARM                    P#OBJT
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH029C (Retrieve HD)
      *          ------------------------------------------
     C     $MH029        PLIST
     C                   PARM                    P#OPTI
     C                   PARM                    P#HEDC
     C                   PARM                    P#SUL#
     C                   PARM                    P#STAT
     C                   PARM                    D#D7HD
      *
      *          ------------------------------------------
      *          Parameter list for OMH033C (Create HTML page)
      *          ------------------------------------------
     C     $H033C        PLIST
     C                   PARM                    FMHEDC
     C                   PARM      FMSRCF        P#SRCF
     C                   PARM      FMSRCL        P#SRCL
     C                   PARM      FMSRCM        P#SRCM
     C                   PARM      FMCUDI        P#CUDI
     C                   PARM      FMCODI        P#CODI
     C                   PARM      FMCONI        P#CONI
     C                   PARM      FMCOEI        P#COEI
     C                   PARM      FMHEDI        P#HEDI
     C                   PARM      FMCSCI        P#CSCI
      *
      *          ------------------------------------------
      *          OMHDDL1 Help Desk File
      *          ------------------------------------------
     C     $HDDK1        KLIST
     C                   KFLD                    FMHEDC
      *
     C     $HDDK2        KLIST
     C                   KFLD                    K#HEDC
      *
      *          ------------------------------------------
      *          Check if there are helpdesk records.
      *          ------------------------------------------
     C                   READ      OMHDDL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Retrieve authorisation for secofr
      *          ------------------------------------------
     C                   IF        *IN99 = *ON
     C                   EVAL      P#OPTI = '1'
     C                   EVAL      P#SUL# = 'X'
     C                   EVAL      P#HEDC = *BLANKS
     C                   EVAL      P#STAT = *BLANKS
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD )
     C                   ELSE
      *
      *          ------------------------------------------
      *          Retrieve global authorisation
      *          ------------------------------------------
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '9'
     C                   EVAL      P#HEDC = *BLANKS
     C                   EVAL      P#STAT = *BLANKS
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD )
     C                   ENDIF                                                  geen else!
      *
      *          ------------------------------------------
      *          Continue if authorisation ok
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B01
      *
      *          ------------------------------------------
      *          Get message data
      *          ------------------------------------------
     C                   EXSR      SRGVAR
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
     C                   WHEN      E#FILE = 'OMH001D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMHDDL1 '                          X02
     C                   MOVEL     HDDL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHDEL1 '                          X02
     C                   MOVEL     HDDL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMINCL1 '                          X02
     C                   MOVEL     HDDL1         P#FILE
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
OPT0027  -  '2', '3', '4', '5' or '12'        1
HLP0002  -  Helpdesk Code                     2
HLP0003  -  Description                       3
HLP0005  -  Manager                           4
HLP0006  -  Call track number                 5
HLP0007  -  Incident number                   6
HLP0004  -  Increment                         7
