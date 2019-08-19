     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ------------------------------------------------------------ *
      * COMPILATION PARAMETERS                                       *
      * ------------------------------------------------------------ *
      *                                                              *
      * This program should be compiled with the following options   *
      *                                                              *
      *      COMMIT(*NONE)                                           *
      *      ALWCPYDTA(*OPTIMIZE)                                    *
      *                                                              *
      * ------------------------------------------------------------ *
      * Description                                                  *
      * ------------------------------------------------------------ *
      *                                                              *
      * Program ....: OMH027                                         *
      * Function ...: Incident search                                *
      * Author .....: Remain Software                               *
      * Module .....: HelpDesk                                       *
      * Parameters .: P#HEDC    -  HelpDesk Code                     *
      *               P#STAT    -  Return Status                     *
      *                                                              *
      * This program is used to search the text blocks of incidents  *
      *                                                              *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * File specifications                                          *
      * ------------------------------------------------------------ *
     FOMINCL1   IF   E           K DISK    INFDS(INCL1)
     F                                     INFSR(*PSSR)
     FOMH027W1  IF   E           K DISK    INFDS(H027W1)
     F                                     INFSR(*PSSR)
     FOMH027D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
     F                                     SFILE(OMH02702:RRN02)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension specifications                                     *
      * ------------------------------------------------------------ *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm    *
      * SRI   Search data input                                      *
      * SRO   Search data output                                     *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(4)
     D MSD             S              7    DIM(4) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
     D WRD             S              1    DIM(31)
      *COPY QCPYSRC,OMSREFREN
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Input specifications                                         *
      * ------------------------------------------------------------ *
      *
      *          ------------------------------------------
      *          Field reference file used to define fields
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          File information data structures
      *          ------------------------------------------
     D INCL1           DS            57
     D H027W1          DS            57
      *
      *          ------------------------------------------
      *          Screen information data structure
      *          ------------------------------------------
     D INFO            DS
     D  STATUS           *STATUS
     D  D#CRC#               378    379B 0
      *
      *          ------------------------------------------
      *          Program status data structure
      *          ------------------------------------------
     D PGM            SDS
     D  E#FILE               201    208
     D  D#USID               254    263
     D  PGMQ             *PROC
      *
      *          ------------------------------------------
      *          Helpdesk data structures
      *          ------------------------------------------
      /COPY QCPYLESRC,OMHLPDS
      *
      *          ------------------------------------------
      *          Error buffer for API call
      *          ------------------------------------------
      /COPY QAPILESRC,ERR_BUFFER
      *
      *          ------------------------------------------
      *          Binary number definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFBINARY
      *
      *          ------------------------------------------
      *          Copy member for OMQUSIIN - Incident user interface
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQUSIINDS
      *
      *          ------------------------------------------
      *          Copy member for OMQCHGIN - Incident change
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQCHGINDS
      *
      *          ------------------------------------------
      *          Copy member for OMQPRTIN - Incident print
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQPRTINDS
      *
      *          ------------------------------------------
      *          Parameter: Function key buffer OMQUSIIN
      *          ------------------------------------------
     D I2FUN           DS            34    OCCURS(24)
     D I2KEY#                         4B 0
     D I2TYPC                         1
     D I2TOMC                         1
     D I2KEYD                        30
     D  I2KMID                        7    OVERLAY(I2KEYD)
     D  I2KMFC                       10    OVERLAY(I2KEYD:8)
     D  I2KMLC                       10    OVERLAY(I2KEYD:18)
      *
      *          ------------------------------------------
      *          Data structure will all current selections
      *          Array SRI is filled implicitly by using
      *          this data structure
      *          ------------------------------------------
     D D#SELC          DS           113
     D  FMHEDC                 1      5
     D  FMSRC1                 6     57
     D  FMSRC2                58    109
     D  FMITXT               110    110
     D  FMWRDS               111    111
     D  FMOPER               112    112
     D  FMTXTS               113    113
     D  SRI                    6    109
     D                                     DIM(104)
      *
      *          ------------------------------------------
      *          Data structure to build a date field
      *          ------------------------------------------
     D                 DS
     D D#DATE                         7  0
     D  D#CC                          1  0 OVERLAY(D#DATE)
     D  D#YY                          2  0 OVERLAY(D#DATE:2)
     D  D#MM                          2  0 OVERLAY(D#DATE:4)
     D  D#DD                          2  0 OVERLAY(D#DATE:6)
      *
      *          ------------------------------------------
      *          Output array with subfields
      *          ------------------------------------------
     D                 DS           155
     D  SRO                    1    155
     D                                     DIM(5)
     D  SRO01                  1     31
     D  SRO02                 32     63
     D  SRO03                 64     95
     D  SRO04                 96    127
     D  SRO05                128    155
      *
      *          ------------------------------------------
      *          Constants used to translate lower case to
      *          upper case
      *          ------------------------------------------
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#INTL          C                   CONST('V3R0M0')
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#EXIT          C                   CONST('*EXIT')
     D C#CNCL          C                   CONST('*CNCL')
     D C#ALL           C                   CONST('*ALL ')
     D C#NONE          C                   CONST('*NONE')
     D C#NERR          C                   CONST('000000')
     D C#HGTM          C                   CONST(235959)
      * Prototype for OMH027
     D OMH027          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#INC#_                             LIKE(P#INC#)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH902C'
     D OMH902C         PR                  EXTPGM('OMH902C')
     D PGMQ_                               LIKE(PGMQ)
      *
      *          ------------------------------------------
      * Prototype for 'OMH901C'
     D OMH901C         PR                  EXTPGM('OMH901C')
     D I2MSID_                             LIKE(I2MSID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D I2MSGD_                             LIKE(I2MSGD)
      *
      *          ------------------------------------------
      *          Parameter list for OMH902C
      * Prototype for 'OMX105'
     D OMX105          PR                  EXTPGM('OMX105')
     D FMHEDC_                             LIKE(FMHEDC)
     D HEDD_                               LIKE(HEDD)
      * Prototype for 'OMQPRTIN'
     D OMQPRTIN        PR                  EXTPGM('OMQPRTIN')
     D I3INTL_                             LIKE(I3INTL)
     D I3ACTC_                             LIKE(I3ACTC)
     D I3BUF_                              LIKE(I3BUF)
     D I3PRC_                              LIKE(I3PRC)
     D I3ERR_                              LIKE(I3ERR)
     D I3FLD_                              LIKE(I3FLD)
      *
      *          ------------------------------------------
      *          Parameter list for OMQUSIIN
      * Prototype for 'OMQPRTIN'
     D OMQPRTIN_001    PR                  EXTPGM('OMQPRTIN')
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
      *          Parameter list for OMH961
      * Prototype for 'OMH961'
     D OMH961          PR                  EXTPGM('OMH961')
     D PGM_                                LIKEDS(PGM)
     D P#FILE_                             LIKE(P#FILE)
      *
      *          ------------------------------------------
      *          Parameter list for OMQCHGIN
      * Prototype for 'QMHMOVPM'
     D QMHMOVPM        PR                  EXTPGM('QMHMOVPM')
     D P#MSKC_                             LIKE(P#MSKC)
     D P#MSTA_                             LIKE(P#MSTA)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#PMQC_                             LIKE(P#PMQC)
     D P#PSC#_                             LIKE(P#PSC#)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'OMQUSIIN'
     D OMQUSIIN        PR                  EXTPGM('OMQUSIIN')
      * Prototype for 'OMQCHGIN'
     D OMQCHGIN        PR                  EXTPGM('OMQCHGIN')
      * Prototype for 'OMH904C'
     D OMH904C         PR                  EXTPGM('OMH904C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGTXT_                             LIKE(MSGTXT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH933C
      * *ENTRY Interface for Main Procedure
     D OMH027          PI
     D P#HEDC                         5
     D P#INC#                        10
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D w#crc#          S              5P 0
     D w#ix            S              5P 0
     D w#sav#          S              5P 0
     D w#valc          S              5P 0
     D EROR01          S                   LIKE(*INLR)
     D EROR02          S                   LIKE(*INLR)
     D I#CHGI          S                   LIKE(INDI)
     D I#MSGI          S                   LIKE(INDI)
     D I#PRTI          S                   LIKE(INDI)
     D I#PSSR          S              1
     D I#USII          S                   LIKE(INDI)
     D LOOP01          S                   LIKE(*INLR)
     D LOOP02          S                   LIKE(*INLR)
     D MA_X            S              5  0
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#FILE          S             57
     D P#MSTA          S             40
     D P#OPTI          S                   LIKE(INDI)
     D P#SUL#          S                   LIKE(SUL#)
     D RRN02           S              5  0
     D S               S              4  0
     D S#HEDC          S                   LIKE(HEDC)
     D S#INC#          S                   LIKE(INC#)
     D S#OCCR          S              5  0
     D S#SELC          S                   LIKE(D#SELC)
     D W#COUN          S              5  0
     D W#ERR           S              6
     D W#INC#          S                   LIKE(INC#)
     D W#INCD          S                   LIKE(INCD)
     D W#ITXT          S                   LIKE(ITXT)
     D W#OMS           S                   LIKE(INDI)
     D W#OPTS          S                   LIKE(FMOPTS)
     D W#SRO           S                   LIKE(SRO01)
     D W#TEST          S             31
     D W#WORD          S                   LIKE(*INLR)
     D X               S              4  0
     D Y               S              4  0
     D Z               S              4  0
      *----------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *----------------------------------------------------------------------
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Main line                                                    *
      * ------------------------------------------------------------ *
      *
      *          ------------------------------------------
      *          Entry parameters
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Do the processing routine if authorised
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B01
     C                   EXSR      SRVERW
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do the total last record routine
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * CLRMSG - Clear message subfile                               *
      *                                                              *
      * This routine calls a subprogram wich clears the message      *
      * subfile.                                                     *
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
      * SNDMSG - Send message                                        *
      *                                                              *
      * This routine calls a subprogram which sends a message to the *
      * program message queue.                                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SNDMSG        BEGSR
      *
      *          ------------------------------------------
      *          Call message sender
      *          ------------------------------------------
     C                   EVAL      MSGF = 'OMHMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( MSGID : MSGF : MSGLIB :
     C                             MSGDTA )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SNDAPI - Send message from OMHAPI                            *
      *                                                              *
      * This routine calls a subprogram which sends a message to the *
      * program message queue.                                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SNDAPI        BEGSR
      *
      *          ------------------------------------------
      *          Call message sender
      *          ------------------------------------------
     C                   EVAL      MSGF = 'OMHAPI'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( I2MSID : MSGF : MSGLIB :
     C                             I2MSGD )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * PROMPT - Prompt a field                                      * /
      *                                                              * /
      * This routine prompts a field                                 * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     PROMPT        BEGSR
      *
      *          ------------------------------------------
      *          Display 1 - HelpDesk code
      *          ------------------------------------------
     C                   IF        FMFMTC = 'OMH02701'                          B01
     C                             AND FMFLDC = 'FMHEDC'
     C                             AND *IN80 = *OFF
     C                   EVAL      S#HEDC = FMHEDC
     C                   CALLP     OMX105 ( FMHEDC : HEDD )
     C                   IF        FMHEDC <> S#HEDC                             B02
     C                   EXSR      SRCHGH
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Else (invalid prompt location)
      *          ------------------------------------------
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Send message
      *          ------------------------------------------
     C                   EVAL      MSGID = 'PMT0002'
     C                   EXSR      SNDMSG
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRVERW - Main program processing routine                     *
      *                                                              *
      * ------------------------------------------------------------ *
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC01 - Enter search data                                   *
      *                                                              *
      * ------------------------------------------------------------ *
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
      *          Fill comparison fields
      *          ------------------------------------------
     C                   MOVEA     *IN(70)       W#ERR
      *
      *          ------------------------------------------
      *          Cursor positioning - C#NERR = '000000'
      *          ------------------------------------------
     C                   IF        W#ERR = C#NERR                               B03
     C                   Z-ADD     7             FMROW#
     C                   Z-ADD     28            FMCOL#
     C                   ELSE                                                   X03
     C                   EVAL      FMROW# = *ZEROS
     C                   EVAL      FMCOL# = *ZEROS
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the screen
      *          ------------------------------------------
     C                   EXFMT     OMH02701
      *
      *          ------------------------------------------
      *          CMD-3 is exit
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B03
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Clear message queue
      *          ------------------------------------------
     C                   EXSR      CLRMSG
      *
      *          ------------------------------------------
      *          Setof screen error indicators
      *          ------------------------------------------
     C                   EVAL      EROR01 = *OFF
     C                   EVAL      LOOP01 = *OFF
     C                   EVAL      *IN(70) = *OFF
     C                   EVAL      *IN(71) = *OFF
     C                   EVAL      *IN(72) = *OFF
     C                   EVAL      *IN(73) = *OFF
     C                   EVAL      *IN(74) = *OFF
     C                   EVAL      *IN(75) = *OFF
      *
      *          ------------------------------------------
      *          CMD-4=Prompt
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   EXSR      PROMPT
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          CMD-5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B04
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Change the HelpDesk if required
      *          ------------------------------------------
     C                   IF        S#HEDC <> FMHEDC                             B05
     C                   EXSR      SRCHGH
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          Edit if not CMD-12 and selections were
      *          changed
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B06
     C                             AND D#SELC <> S#SELC
     C                   EXSR      SRED01
     C                   ENDIF                                                  E06
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors or CMD key
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do processing if not CMD-12 and selections
      *          were changed
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B02
     C                             AND D#SELC <> S#SELC
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRIN01 - Enter search data - Initialization                  *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Set indicator to control if changing of
      *          the helpdesk code should be allowed.
      *          When P#INC# is filled, this means that the
      *          program is called from another program,
      *          and changing should not be allowed.
      *          P#INC# filled --> *IN80 on --> linking of
      *          incident allowed / change of helpdesk not
      *          allowed
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      P#INC# = *BLANKS                             X01
     C                   EVAL      *IN80 = *OFF
     C                   OTHER                                                  X01
     C                   EVAL      *IN80 = *ON
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          Set save field for HelpDesk code
      *          ------------------------------------------
     C                   EVAL      S#HEDC = P#HEDC
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRF01 - Enter search data - Refresh                         *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRF01        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields
      *          ------------------------------------------
     C                   EVAL      FMHEDC = S#HEDC
     C                   EVAL      FMSRC1 = *BLANKS
     C                   EVAL      FMSRC2 = *BLANKS
     C                   EVAL      FMITXT = '0'
     C                   EVAL      FMWRDS = '1'
     C                   EVAL      FMOPER = '2'
     C                   EVAL      FMTXTS = '3'
      *
      *          ------------------------------------------
      *          Fill save field with current selections
      *          ------------------------------------------
     C                   MOVE      D#SELC        S#SELC
      *
      *          ------------------------------------------
      *          Reset error comparision field
      *          ------------------------------------------
     C                   EVAL      W#ERR = C#NERR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRED01 - Enter search data - Edit                            *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          Check fields with search data
      *          ------------------------------------------
     C                   IF        FMSRC1 <> *BLANKS                            B01
     C                             OR FMSRC2 <> *BLANKS                         B01
      *
      *          ------------------------------------------
      *          Initialize workfields and output array
      *          ------------------------------------------
     C                   EVAL      W#WORD = *OFF
     C                   EVAL      SRO = *BLANKS
     C                   EVAL      Z = *ZEROS
     C                   EVAL      Y = *ZEROS
      *
      *          ------------------------------------------
      *          If word lenght longer than 30 skip filling array
      *          ------------------------------------------
     C                   FOR       X = 1 TO 104
     C                   IF        SRI(X) <> *BLANKS                            B03
     C                   ADD       1             Y
     C                   ENDIF
     C                   IF        Y > 30
     C                   EVAL      MSGID = 'MH02702'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   EVAL      *IN71 = *ON
     C                   GOTO      LENERR
     C                   ENDIF
     C                   IF        SRI(X) = *BLANKS
     C                   EVAL      Y = *ZEROS
     C                   ENDIF
     C                   ENDFOR
      *
      *          ------------------------------------------
      *          Process all elements in the input array
      *          ------------------------------------------
     C                   FOR       X = 1 TO 104                                 B02
      *
      *          ------------------------------------------
      *          If element in input array is filled
      *          ------------------------------------------
     C                   IF        SRI(X) <> *BLANKS                            B03
      *
      *          ------------------------------------------
      *          If 'In word' is off
      *          ------------------------------------------
     C                   IF        W#WORD = *OFF                                B04
      *
      *          ------------------------------------------
      *          Set the sequence number of the new word
      *          ------------------------------------------
     C                   ADD       1             Z
      *
      *          ------------------------------------------
      *          If this is the 6th word
      *          ------------------------------------------
     C                   IF        Z > 5                                        B05
      *
      *          ------------------------------------------
      *          Leave loop
      *          ------------------------------------------
     C                   LEAVE
      *
      *          ------------------------------------------
      *          Else (word 1 to 5)
      *          ------------------------------------------
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          Blank word array, set first element to %
      *          Set starting index to 1 or 2, depending on
      *          value for 'Consider words to be'
      *          1=Substrings 2=Complete words
      *          ------------------------------------------
     C                   EVAL      WRD = *BLANKS
     C                   EVAL      WRD(1) = '%'
     C                   IF        FMWRDS = '1'                                 B06
     C                   EVAL      Y = 1
     C                   ELSE                                                   X06
     C                   EVAL      Y = 2
     C                   ENDIF                                                  E06
     C                   EVAL      W#WORD = *ON                                 E05
      *
     C                   ENDIF                                                  E05
      *
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Fill Yth element in word array
      *          ------------------------------------------
     C                   ADD       1             Y
     C                   EVAL      WRD(Y) = %XLATE(LO:UP:SRI(X))
      *
      *          ------------------------------------------
      *          Else (element in input array is empty)
      *          ------------------------------------------
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          If 'In word' is still on, then move word
      *          array to Zth element of SRO
      *          ------------------------------------------
     C                   IF        W#WORD = *ON                                 B04
     C                   IF        FMWRDS = '1'                                 B05
     C     Y             ADD       1             S
     C                   ELSE                                                   X05
     C     Y             ADD       2             S
     C                   ENDIF                                                  E05
     C                   FOR       MA_X = S TO %ELEM(WRD)
     C                   EVAL      WRD(MA_X) = '%'
     C                   ENDFOR
     C                   MOVEA     WRD           SRO(Z)
     C                   EVAL      W#WORD = *OFF
     C                   ENDIF                                                  E04
      *
     C                   ENDIF                                                  E03
      *
     C                   ENDFOR                                                 E02
      *
      *          ------------------------------------------
      *          If number of words is greater than 5, then
      *          send message
      *          ------------------------------------------
     C                   IF        Z > 5                                        B02
     C                   EVAL      MSGID = 'MH02701'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   EVAL      *IN71 = *ON
     C                   ENDIF                                                  E02
      *          ------------------------------------------
      *          If word lenght longer than 30 skip filling array
      *          ------------------------------------------
     C     LENERR        TAG
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check field text type
      *          ------------------------------------------
     C                   IF        FMITXT < '0'                                 B01
     C                             OR FMITXT > '4'                              B03
     C                   EVAL      MC(1) = FMITXT
     C                   MOVEA     VAR(1)        MC(11)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'VAL0005'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   EVAL      *IN72 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check field word or substring
      *          ------------------------------------------
     C                   IF        FMWRDS < '1'                                 B01
     C                             OR FMWRDS > '2'                              B03
     C                   EVAL      MC(1) = FMWRDS
     C                   MOVEA     VAR(2)        MC(11)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'VAL0005'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   EVAL      *IN73 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check field Boolean operator
      *          ------------------------------------------
     C                   IF        FMOPER < '1'                                 B01
     C                             OR FMOPER > '2'                              B03
     C                   EVAL      MC(1) = FMOPER
     C                   MOVEA     VAR(3)        MC(11)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'VAL0005'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   EVAL      *IN74 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check field Texts to search
      *          ------------------------------------------
     C                   IF        FMTXTS < '1'                                 B01
     C                             OR FMTXTS > '3'                              B03
     C                   EVAL      MC(1) = FMTXTS
     C                   MOVEA     VAR(4)        MC(11)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'VAL0005'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   EVAL      *IN75 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPR01 - Enter search data - Process                         *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          Process if source date was entered
      *          ------------------------------------------
     C                   IF        FMSRC1 <> *BLANKS                            B01
     C                             OR FMSRC2 <> *BLANKS
      *
      *          ------------------------------------------
      *          Force loop of first screen
      *          ------------------------------------------
     C                   EVAL      LOOP01 = *ON
      *
      *          ------------------------------------------
      *          Enter loop to call screen 2 processing
      *          ------------------------------------------
     C                   EXSR      SRSC02
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Put search words in save field
      *          ------------------------------------------
     C                   MOVE      D#SELC        S#SELC
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC02 - Work with search results                            *
      *                                                              *
      * ------------------------------------------------------------ *
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
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH02704
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if
      *          records found
      *          ------------------------------------------
     C                   IF        RRN02 = *ZEROS                               B03
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   EXFMT     OMH02703
      *
      *          ------------------------------------------
      *          CMD-3 is exit
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B03
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
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
     C                   EVAL      *IN70 = *OFF
     C                   EVAL      EROR02 = *OFF
     C                   EVAL      LOOP02 = *OFF
      *
      *          ------------------------------------------
      *          CMD-5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B03
     C                   EXSR      SRRF02
     C                   EVAL      EROR02 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Edit if not CMD-12
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B04
     C                   EXSR      SRED02
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors or CMD key
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do processing if not CMD-12
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
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRIN02 - Work with search results - Initialization           *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRIN02        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMPOS
      *
      *          ------------------------------------------
      *          Build search field
      *          ------------------------------------------
     C                   EXSR      SRSRCH
      *
      *          ------------------------------------------
      *          Delete all previous records
      *          ------------------------------------------
     C/EXEC SQL
     C+ DELETE FROM OMH027W1
     C+   WHERE WWUSID = :D#USID
     C/END-EXEC
      *
      *          ------------------------------------------
      *          Actual searching is done in different
      *          subroutines
      *          ------------------------------------------
     C                   IF        FMOPER = '1'                                 B01
      *
     C                   EXSR      SRAND
      *
     C                   ELSE                                                   X01
      *
     C                   EXSR      SROR
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do refresh subroutine
      *          ------------------------------------------
     C                   EXSR      SRRF02
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRAND  - Handle search with AND operand                      *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRAND         BEGSR
      *
      *          ------------------------------------------
      *          If only one search word
      *          ------------------------------------------
     C                   IF        Z = 1                                        B01
      *
      *          ------------------------------------------
      *          Search OMTID or OMTIH depending on FMTXTS
      *          ------------------------------------------
     C                   IF        FMTXTS = '1'                                 B02
     C                             OR FMTXTS = '3'                              B02
     C/EXEC SQL
     C+ INSERT INTO OMH027W1 (WWUSID, WWINC#, WWCN5#)
     C+    SELECT :D#USID, IDINC#, COUNT(*)
     C+    FROM OMTID
     C+    WHERE IDHEDC = :P#HEDC
     C+      AND IDITXT LIKE :W#ITXT
     C+      AND ' ' || TRANSLATE(IDINCD) || ' ' LIKE :SRO01
     C+    GROUP BY IDINC#
     C/END-EXEC
     C                   ENDIF                                                  E02
     C                   IF        FMTXTS = '2'                                 B02
     C                             OR FMTXTS = '3'                              B02
     C/EXEC SQL
     C+ INSERT INTO OMH027W1 (WWUSID, WWINC#, WWCN5#)
     C+    SELECT :D#USID, IHINC#, COUNT(*)
     C+    FROM OMTIH
     C+    WHERE IHHEDC = :P#HEDC
     C+      AND IHITXT LIKE :W#ITXT
     C+      AND ' ' || TRANSLATE(IHSITD) || ' ' LIKE :SRO01
     C+    GROUP BY IHINC#
     C/END-EXEC
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Else (more than one search word)
      *          ------------------------------------------
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Delete records from second workfile
      *          ------------------------------------------
     C/EXEC SQL
     C+ DELETE FROM OMH027W2
     C+   WHERE WWUSID = :D#USID
     C/END-EXEC
      *
      *          ------------------------------------------
      *          First selection - output to OMH027W2
      *          ------------------------------------------
     C                   IF        FMTXTS = '1'                                 B02
     C                             OR FMTXTS = '3'                              B02
     C/EXEC SQL
     C+ INSERT INTO OMH027W2 (WWUSID, WWINC#, WWCN5#)
     C+    SELECT :D#USID, IDINC#, COUNT(*)
     C+    FROM OMTID
     C+    WHERE IDHEDC = :P#HEDC
     C+      AND IDITXT LIKE :W#ITXT
     C+      AND ' ' || TRANSLATE(IDINCD) || ' ' LIKE :SRO01
     C+    GROUP BY IDINC#
     C/END-EXEC
     C                   ENDIF                                                  E02
     C                   IF        FMTXTS = '2'                                 B02
     C                             OR FMTXTS = '3'                              B02
     C/EXEC SQL
     C+ INSERT INTO OMH027W2 (WWUSID, WWINC#, WWCN5#)
     C+    SELECT :D#USID, IHINC#, COUNT(*)
     C+    FROM OMTIH
     C+    WHERE IHHEDC = :P#HEDC
     C+      AND IHITXT LIKE :W#ITXT
     C+      AND ' ' || TRANSLATE(IHSITD) || ' ' LIKE :SRO01
     C+    GROUP BY IHINC#
     C/END-EXEC
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Repeat for second word to the last word
      *          ------------------------------------------
     C                   FOR       X = 2 TO Z                                   B02
      *
      *          ------------------------------------------
      *          Empty the workfile holding the final
      *          results
      *          ------------------------------------------
     C/EXEC SQL
     C+ DELETE FROM OMH027W1
     C+   WHERE WWUSID = :D#USID
     C/END-EXEC
      *
      *          ------------------------------------------
      *          Other selections - output to OMH027W1 -
      *          incident number should be in OMH027W2
      *          ------------------------------------------
     C                   EVAL      W#SRO = SRO(X)
      *
     C                   IF        FMTXTS = '1'                                 B03
     C                             OR FMTXTS = '3'                              B03
     C/EXEC SQL
     C+ INSERT INTO OMH027W1 (WWUSID, WWINC#, WWCN5#)
     C+    SELECT :D#USID, IDINC#, COUNT(*)
     C+    FROM OMTID
     C+    WHERE IDHEDC = :P#HEDC
     C+      AND IDITXT LIKE :W#ITXT
     C+      AND ' ' || TRANSLATE(IDINCD) || ' ' LIKE :W#SRO
     C+      AND :D#USID CONCAT IDINC# IN
     C+          (SELECT WWUSID CONCAT WWINC# FROM OMH027W2)
     C+    GROUP BY IDINC#
     C/END-EXEC
     C                   ENDIF                                                  E03
     C                   IF        FMTXTS = '2'                                 B03
     C                             OR FMTXTS = '3'                              B03
     C/EXEC SQL
     C+ INSERT INTO OMH027W1 (WWUSID, WWINC#, WWCN5#)
     C+    SELECT :D#USID, IHINC#, COUNT(*)
     C+    FROM OMTIH
     C+    WHERE IHHEDC = :P#HEDC
     C+      AND IHITXT LIKE :W#ITXT
     C+      AND ' ' || TRANSLATE(IHSITD) || ' ' LIKE :W#SRO
     C+      AND :D#USID CONCAT IHINC# IN
     C+          (SELECT WWUSID CONCAT WWINC# FROM OMH027W2)
     C+    GROUP BY IHINC#
     C/END-EXEC
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If this was not the last word, then clear
      *          OMH027W2 and copy contents of OMH027W1 to
      *          OMH027W2. File OMH027W1 will be cleared
      *          in the beginning of the DO loop.
      *          ------------------------------------------
     C                   IF        X < Z                                        B03
      *
     C/EXEC SQL
     C+ DELETE FROM OMH027W2
     C+   WHERE WWUSID = :D#USID
     C/END-EXEC
      *
     C/EXEC SQL
     C+ INSERT INTO OMH027W2 (WWUSID, WWINC#, WWCN5#)
     C+    SELECT WWUSID, WWINC#, WWCN5#
     C+    FROM OMH027W1
     C+    WHERE WWUSID = :D#USID
     C/END-EXEC
      *
     C                   ENDIF                                                  E03
      *
     C                   ENDFOR                                                 E02
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SROR   - Handle search with OR operand                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SROR          BEGSR
      *
     C                   IF        FMTXTS = '1'                                 B01
     C                             OR FMTXTS = '3'                              B03
     C/EXEC SQL
     C+ INSERT INTO OMH027W1 (WWUSID, WWINC#, WWCN5#)
     C+    SELECT :D#USID, IDINC#, COUNT(*)
     C+    FROM OMTID
     C+    WHERE IDHEDC = :P#HEDC
     C+      AND IDITXT LIKE :W#ITXT
     C+      AND (' ' || TRANSLATE(IDINCD) || ' ' LIKE :SRO01
     C+      OR   ' ' || TRANSLATE(IDINCD) || ' ' LIKE :SRO02
     C+      OR   ' ' || TRANSLATE(IDINCD) || ' ' LIKE :SRO03
     C+      OR   ' ' || TRANSLATE(IDINCD) || ' ' LIKE :SRO04
     C+      OR   ' ' || TRANSLATE(IDINCD) || ' ' LIKE :SRO05)
     C+    GROUP BY IDINC#
     C/END-EXEC
     C                   ENDIF                                                  E01
      *
     C                   IF        FMTXTS = '2'                                 B01
     C                             OR FMTXTS = '3'                              B03
     C/EXEC SQL
     C+ INSERT INTO OMH027W1 (WWUSID, WWINC#, WWCN5#)
     C+    SELECT :D#USID, IHINC#, COUNT(*)
     C+    FROM OMTIH
     C+    WHERE IHHEDC = :P#HEDC
     C+      AND IHITXT LIKE :W#ITXT
     C+      AND (' ' || TRANSLATE(IHSITD) || ' ' LIKE :SRO01
     C+      OR   ' ' || TRANSLATE(IHSITD) || ' ' LIKE :SRO02
     C+      OR   ' ' || TRANSLATE(IHSITD) || ' ' LIKE :SRO03
     C+      OR   ' ' || TRANSLATE(IHSITD) || ' ' LIKE :SRO04
     C+      OR   ' ' || TRANSLATE(IHSITD) || ' ' LIKE :SRO05)
     C+    GROUP BY IHINC#
     C/END-EXEC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRF02 - Work with search results - Refresh                  *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRF02        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      RRN02 = *ZEROS
      *
      *          ------------------------------------------
      *          Initialize save field
      *          ------------------------------------------
     C                   EVAL      S#INC# = *BLANKS
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH02703
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Initial read in workfile OMH027W1
      *          ------------------------------------------
     C     $027K1        SETLL     OMH027W1
     C     $027K1        READE     OMH027W1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Process only if different incident number
      *          ------------------------------------------
     C                   IF        WWINC# <> S#INC#                             B02
      *
      *          ------------------------------------------
      *          Save incident number
      *          ------------------------------------------
     C                   MOVE      WWINC#        S#INC#                         B01
      *
      *          ------------------------------------------
      *          Get incident description
      *          ------------------------------------------
     C     $NC1K1        CHAIN     OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B03
     C                   MOVEL(P)  NCSHID        FMINCD
     C                   ELSE                                                   X03
     C                   MOVEL(P)  '?'           FMINCD
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Fill other subfile fields and write record
      *          ------------------------------------------
     C                   MOVEL     *BLANKS       FMOPTS
     C                   MOVEL     WWINC#        FMINC#
     C                   Z-ADD     WWCN5#        FMOCCR
     C                   ADD       1             RRN02
     C                   WRITE     OMH02702
      *
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     $027K1        READE     OMH027W1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          End of loop
      *          ------------------------------------------
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Fill current subfile relative record
      *          number to display first subfile page
      *          ------------------------------------------
     C                   Z-ADD     1             CRRN02
      *
      *          ------------------------------------------
      *          Save overall relative record number
      *          ------------------------------------------
     C                   EVAL      W#SAV# = RRN02
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRED02 - Work with search results - Edit                     *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRED02        BEGSR
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
     C                   EVAL      W#VALC = *ZEROS
     C                   EVAL      CRRN02 = *ZEROS
     C                   EVAL      I#MSGI = *OFF
      *
      *          ------------------------------------------
      *          If subfile contains records
      *          ------------------------------------------
     C                   IF        RRN02 > *ZEROS                               B01
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   READC(E)  OMH02702
     C                   EVAL      *IN99 = %ERROR OR %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Do if option is not blank
      *          ------------------------------------------
     C                   IF        FMOPTS <> *BLANKS                            B03
      *
      *          ------------------------------------------
      *          If option is valid, then check the number
      *          of valid codes. Options 1 and 8 are only
      *          valid if the parameter P#INC# is filled,
      *          which is indicated by indicator 80
      *          ------------------------------------------
     C                   IF        FMOPTS = '1'                                 B04
     C                             AND *IN80 = *ON
     C                             OR FMOPTS = '5'
     C                             OR FMOPTS = '6'
     C                             OR FMOPTS = '8'                              B04
     C                             AND *IN80 = *ON
      *
      *          ------------------------------------------
      *          Update number of valid options
      *          ------------------------------------------
     C                   ADD       1             W#VALC
      *
      *          ------------------------------------------
      *          Else (option not valid)
      *          ------------------------------------------
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Reverse screen fields and indicate error
      *          ------------------------------------------
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR02 = *ON
      *
      *          ------------------------------------------
      *          Send the message if not yet sent
      *          ------------------------------------------
     C                   IF        I#MSGI = *OFF                                B05
     C                   MOVEL(P)  FMOPTS        MSGDTA
     C                   EVAL      MSGID = 'OPTE021'
     C                   EXSR      SNDMSG
     C                   EVAL      I#MSGI = *ON
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If an error was found, then position on
      *          the page of the first error
      *          ------------------------------------------
     C                   IF        CRRN02 = *ZEROS                              B04
     C                             AND *IN70 = *ON
     C                   Z-ADD     RRN02         CRRN02
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Update subfile
      *          ------------------------------------------
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH02702
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset option field
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   UPDATE    OMH02702
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH02702
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          If there has been no error
      *          ------------------------------------------
     C                   IF        CRRN02 = *ZEROS                              B02
      *
      *          ------------------------------------------
      *          If position-to was filled, then reposition
      *          ------------------------------------------
     C                   IF        FMPOS <> *BLANKS                             B03
      *
      *          ------------------------------------------
      *          Process subfile to find selected item
      *          ------------------------------------------
     C                   FOR       W#IX = 1 TO W#SAV#                           B04
     C     W#IX          CHAIN     OMH02702
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        FMINC# >= FMPOS                              B05
     C                   Z-ADD     W#IX          CRRN02
     C                   EVAL      W#IX = W#SAV#
     C                   MOVE      *BLANKS       FMPOS
     C                   EVAL      EROR02 = *ON
     C                   ENDIF                                                  E05
     C                   ENDFOR                                                 E04
      *
     C                   IF        FMPOS <> *BLANKS                             B04
     C                   MOVE      *BLANKS       FMPOS
     C                   Z-ADD     W#SAV#        CRRN02
     C                   EVAL      EROR02 = *ON
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          No positioning requested
      *          ------------------------------------------
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Fill the current relative record on the
      *          page to display
      *          ------------------------------------------
     C                   Z-ADD     W#CRC#        CRRN02
      *
     C                   ENDIF                                                  E03
      *
     C                   ENDIF                                                  E02
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPR02 - Work with search results - Process                  *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPR02        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Read subfile next changed record if
      *          records in subfile and valid codes found
      *          ------------------------------------------
     C                   IF        RRN02 > *ZEROS                               B01
     C                             AND W#VALC > *ZEROS
     C                   READC     OMH02702
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Read subfile until valid option found
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Loop again for options where typed
      *          ------------------------------------------
     C                   EVAL      LOOP02 = *ON
     C                   EVAL      W#CRC# = RRN02
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPTS        W#OPTS
     C                   MOVE      *BLANKS       FMOPTS
     C                   UPDATE    OMH02702
      *
      *          ------------------------------------------
      *          Do action depending on selected option
      *          ------------------------------------------
     C                   SELECT                                                 B03
      *
     C                   WHEN      W#OPTS = '1'                                 X03
     C                   EXSR      SRINAC
      *
     C                   WHEN      W#OPTS = '5'                                 X03
     C                   EXSR      SRINDS
     C                   IF        I2MSTP = 'E'                                 B04
     C                   LEAVE
     C                   ENDIF                                                  E04
      *
     C                   WHEN      W#OPTS = '6'                                 X03
     C                   EXSR      SRINPR
     C                   IF        I3MSTP = 'E'                                 B04
     C                   LEAVE
     C                   ENDIF                                                  E04
      *
     C                   WHEN      W#OPTS = '8'                                 X03
     C                   EXSR      SRINLN
      *
     C                   ENDSL                                                  E03
      *
      *          ------------------------------------------
      *          Exit if CMD-3
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B03
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Leave if CMD-12
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                   LEAVE
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next changed
      *          ------------------------------------------
     C                   READC     OMH02702
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If end of file found, then end loop
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Reset subfile relative record number
      *          ------------------------------------------
     C                   Z-ADD     W#CRC#        CRRN02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSRCH - Build search field for declare cursor               *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSRCH        BEGSR
      *
      *          ------------------------------------------
      *          If Text type is all, then fill W#ITXT with
      *          a catch-all value
      *          ------------------------------------------
     C                   IF        FMITXT = '0'                                 B01
     C                   EVAL      W#ITXT = '%'
     C                   ELSE                                                   X01
     C                   EVAL      W#ITXT = FMITXT
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Format search words
      *          ------------------------------------------
     C                   FOR       X = 1 TO 5                                   B01
      *
      *          ------------------------------------------
      *          Move word array in field to test contents
      *          ------------------------------------------
     C                   MOVEA     SRO(X)        W#TEST
      *
      *          ------------------------------------------
      *          If word array is not empty
      *          ------------------------------------------
     C                   IF        W#TEST = *BLANKS                             B02
      *
      *          ------------------------------------------
      *          If operator is AND, then fill field with
      *          catch-all value
      *          ------------------------------------------
     C                   IF        FMOPER = '1'                                 B03
     C                   MOVE      *ALL'%'       W#TEST
     C                   EVAL      SRO(X) = W#TEST
     C                   ENDIF                                                  E03
      *
     C                   ENDIF                                                  E02
      *
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRINAC - Accumulate incident                                 *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRINAC        BEGSR
      *
      *          ------------------------------------------
      *          Fill all fields in the buffer
      *          ------------------------------------------
     C                   MOVE      P#HEDC        I4HEDC
     C                   MOVEL     P#INC#        I4INC#
     C                   MOVE      FMINC#        I4AIN#
      *
      *          ------------------------------------------
      *          Fill action code
      *          ------------------------------------------
     C                   MOVE      '25'          I4ACTC
      *
      *          ------------------------------------------
      *          Fill the processing options in I4PRC
      *          ------------------------------------------
     C                   MOVE      *OFF          I4EOP
     C                   MOVE      *ON           I4UPD
     C                   MOVE      *ON           I4UPB
     C                   MOVE      *BLANKS       I4PCM
     C                   MOVE      *BLANKS       I4CCM
     C                   MOVE      '0'           I4RCM
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
     C                   EVAL      I#CHGI = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#CHGI = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If OMQCHGIN ended without an error
      *          ------------------------------------------
     C                   IF        I4MSTP = *BLANKS                             B01
      *
      *          ------------------------------------------
      *          If actual accumulated incident number is
      *          equal to the selected incident number,
      *          then send 'normal' message
      *          ------------------------------------------
     C                   IF        I4AIN# = FMINC#                              B02
     C                   MOVEA     P#INC#        MC(1)
     C                   MOVEA     FMINC#        MC(11)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'ACC0002'
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          If actual accumulated incident number is
      *          not equal to the selected incident number,
      *          then send 'special' message
      *          ------------------------------------------
     C                   MOVEA     P#INC#        MC(1)
     C                   MOVEA     FMINC#        MC(11)
     C                   MOVEA     I4AIN#        MC(21)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'ACC0003'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E02
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRINDS - Display incident                                    *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRINDS        BEGSR
      *
      *          ------------------------------------------
      *          Fill key fields in I2KEY
      *          ------------------------------------------
     C                   MOVEL     P#HEDC        I2KHED
     C                   MOVEL     FMINC#        I2KINC
      *
      *          ------------------------------------------
      *          Fill function keys
      *          ------------------------------------------
     C                   EVAL      I2FUN# = 0
      *
      *          ------------------------------------------
      *          Fill action code for *DISPLAY
      *          ------------------------------------------
     C                   MOVE      '50'          I2ACTC
      *
      *          ------------------------------------------
      *          Fill the processing options in I2PRCA
      *          ------------------------------------------
     C                   MOVE      *OFF          I2EOP
     C                   MOVE      *OFF          I2RDSP
     C                   MOVE      *OFF          I2FILL
     C                   MOVE      *ON           I2PHPD
      *
      *          ------------------------------------------
      *          Call OMQUSIIN
      *          ------------------------------------------
     C                   CALL      'OMQUSIIN'    $USIIN                   99
     C                   IF        *IN99 = *ON
     C                   EVAL      I#USII = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#USII = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          F3=Exit
      *          ------------------------------------------
     C                   IF        I2FKU# = 3                                   B01
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          F12=Cancel
      *          ------------------------------------------
     C                   IF        I2FKU# = 12                                  B02
     C                   EVAL      *INKL = *ON
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Send the API message if needed.
      *          ------------------------------------------
     C                   IF        I2MSTP <> *BLANK                             B03
     C                   EXSR      SNDAPI
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRINPR - Print incident                                      *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRINPR        BEGSR
      *
      *          ------------------------------------------
      *          Fill action code
      *          ------------------------------------------
     C                   MOVE      '00'          I3ACTC
      *
      *          ------------------------------------------
      *          Fill fields in the processing array
      *          ------------------------------------------
     C                   IF        W#OMS = *BLANKS                              B01
     C                   EXSR      SROMS
     C                   ENDIF                                                  E01
     C                   SELECT                                                 B01
     C                   WHEN      W#OMS = *ON                                  X01
     C                   MOVE      *ON           I3PRR
     C                   MOVE      *ON           I3PFI
     C                   MOVE      *ON           I3PSI
     C                   WHEN      W#OMS = *OFF                                 X01
     C                   MOVE      *OFF          I3PRR
     C                   MOVE      *OFF          I3PFI
     C                   MOVE      *OFF          I3PSI
     C                   ENDSL                                                  E01
     C                   MOVE      *OFF          I3EOP
     C                   MOVE      *ON           I3PIR
     C                   MOVE      *ON           I3LOD
     C                   MOVE      *ON           I3PIP
     C                   MOVE      *OFF          I3PCP
     C                   MOVE      *OFF          I3PEP
     C                   MOVE      *ON           I3QAH
     C                   MOVE      '9'           I3PIT
     C                   MOVE      *ON           I3CLP
      *
      *          ------------------------------------------
      *          Instruct API to send messages
      *          ------------------------------------------
     C                   MOVEL(P)  PGMQ          I3MSQ
      *
      *          ------------------------------------------
      *          Build date of today
      *          ------------------------------------------
     C                   EVAL      D#YY = UYEAR
     C                   EVAL      D#MM = UMONTH
     C                   EVAL      D#DD = UDAY
      *
      *          ------------------------------------------
      *          Fill century digit
      *          ------------------------------------------
     C                   EVAL      *IN99 = (D#YY <= 40)
     C                   MOVE      *IN99         D#CC
      *
      *          ------------------------------------------
      *          Fill fields in the buffer
      *          ------------------------------------------
     C                   MOVEL(P)  FMHEDC        I3HEDC
     C                   MOVEL(P)  FMINC#        I3INC
     C                   MOVEL(P)  '*ALL'        I3CTN
     C                   EVAL      I3IDTF = *ZEROS
     C                   Z-ADD     D#DATE        I3IDTT
     C                   EVAL      I3ITMF = *ZEROS
     C                   Z-ADD     C#HGTM        I3ITMT
     C                   EVAL      I3CDTF = *ZEROS
     C                   Z-ADD     D#DATE        I3CDTT
     C                   MOVEL(P)  '*ALL'        I3STRC
     C                   MOVEL(P)  '*'           I3SULC
     C                   MOVEL(P)  '*ALL'        I3SEV1
     C                   MOVEL(P)  '*ALL'        I3SEV2
     C                   MOVEL(P)  '*ALL'        I3SEV3
     C                   EVAL      I3PRIF = *ZEROS
     C                   Z-ADD     *HIVAL        I3PRIT
     C                   MOVEL(P)  '*ALL'        I3RUID
     C                   MOVEL(P)  '*ALL'        I3AUID
     C                   MOVEL(P)  '*ALL'        I3ITPC
     C                   MOVEL(P)  '*ALL'        I3USF1
     C                   MOVEL(P)  '*ALL'        I3USF2
     C                   Z-ADD     -1            I3USF3
     C                   MOVEL(P)  '*GE'         I3ESCD
     C                   EVAL      I3ESC# = *ZEROS
     C                   EVAL      I3ESDT = *ZEROS
     C                   MOVEL(P)  C#NONE        I3ALSC
     C                   MOVEL(P)  C#ALL         I3STAT
     C                   MOVEL(P)  C#ALL         I3CUSC
     C                   MOVEL(P)  C#ALL         I3CCTC
      *
      *          ------------------------------------------
      *          Call the print API
      *          ------------------------------------------
     C                   EVAL      I3INTL = 'V3R0M0  '
     C                   CALLP(E)  OMQPRTIN ( I3INTL : I3ACTC : I3BUF :
     C                             I3PRC : I3ERR : I3FLD )
     C                   EVAL      *IN99 = %ERROR
     C                   IF        *IN99 = *ON
     C                   EVAL      I#PRTI = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#PRTI = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Send printed request if no error found
      *          If error found call OMQPRTIN again to
      *          close program and force end of processing
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF                                 B01
     C                             AND I3MSTP <> 'E'
     C                   MOVEA     FMINC#        MC(1)
     C                   MOVEA     FMHEDC        MC(11)
     C                   MOVEA     MC(1)         MSGDTA
     C                   EVAL      MSGID = 'PRT0011'
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X01
     C                   CALLP     OMQPRTIN_001()
     C                   EVAL      *INKL = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRINLN - Link incident                                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRINLN        BEGSR
      *
      *          ------------------------------------------
      *          Fill all fields in the buffer
      *          ------------------------------------------
     C                   MOVE      P#HEDC        I4HEDC
     C                   MOVEL     P#INC#        I4INC#
     C                   MOVE      FMHEDC        I4THED
     C                   MOVE      FMINC#        I4TINC
      *
      *          ------------------------------------------
      *          Fill action code
      *          ------------------------------------------
     C                   MOVE      '23'          I4ACTC
      *
      *          ------------------------------------------
      *          Fill the processing options in I4PRC
      *          ------------------------------------------
     C                   MOVE      *OFF          I4EOP
     C                   MOVE      *ON           I4UPD
     C                   MOVE      *OFF          I4UPB
     C                   MOVE      *BLANKS       I4PCM
     C                   MOVE      *BLANKS       I4CCM
     C                   MOVE      *BLANKS       I4RCM
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
     C                   EVAL      I#CHGI = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      I#CHGI = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          If OMQCHGIN ended without an error
      *          ------------------------------------------
     C                   IF        I4MSTP = *BLANKS                             B01
      *
      *          ------------------------------------------
      *          Send message that incident was success-
      *          fully linked
      *          ------------------------------------------
     C                   MOVEA     P#INC#        MC(1)
     C                   MOVEA     FMINC#        MC(11)
     C                   MOVEA     MC            MSGDTA
     C                   EVAL      MSGID = 'LNK0016'
     C                   EXSR      SNDMSG
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
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
      *          If the HelpDesk code is blanks, then reset
      *          ------------------------------------------
     C                   IF        FMHEDC = *BLANKS                             B01
     C                   EVAL      FMHEDC = S#HEDC
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Change the helpdesk
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '0'
     C                   EVAL      P#HEDC = FMHEDC
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD )
      *
      *          ------------------------------------------
      *          If errors, reset
      *          ------------------------------------------
     C                   IF        P#STAT <> C#NORM                             B02
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Fill screen fields from parameters
      *          ------------------------------------------
     C                   EVAL      FMHEDC = P#HEDC
     C                   MOVEL     D8HEDD        FMHEDD
      *
      *          ------------------------------------------
      *          Set save field for HelpDesk code
      *          ------------------------------------------
     C                   EVAL      S#HEDC = P#HEDC
      *
      *          ------------------------------------------
      *          Force loop of first screen 1
      *          ------------------------------------------
     C                   EVAL      EROR01 = *ON
      *
     C                   ENDIF                                                  E02
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SROMS  - Check if OMS is installed on the system             *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SROMS         BEGSR
      *
      *          ------------------------------------------
      *          Call program to check if OMS is installed
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          If return status is not normal, then OMS
      *          is not installed.
      *          ------------------------------------------
     C                   EVAL      W#OMS = *OFF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * *INZSR - Program initialisation                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Explicit numeric field definitions
      *          ------------------------------------------
     C                   EVAL      S#OCCR = *ZEROS
     C                   EVAL      W#COUN = *ZEROS
     C                   EVAL      RRN02 = *ZEROS
     C                   EVAL      MS = *ZEROS
     C                   EVAL      S = *ZEROS
     C                   EVAL      X = *ZEROS
     C                   EVAL      Y = *ZEROS
     C                   EVAL      Z = *ZEROS
      *
      *          ------------------------------------------
      *          Explicit character field definitions
      *          ------------------------------------------
     C                   EVAL      W#TEST = *BLANKS
     C                   EVAL      W#ERR = *BLANKS
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
      *          Savefields
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Indicator definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          API parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C
      *          ------------------------------------------
     C     $X901C        PLIST
     C                   PARM                    MSGID
     C                   PARM      'OMHMSG'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGDTA
      *
     C     $X901$        PLIST
     C                   PARM                    I2MSID
     C                   PARM      'OMHAPI'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    I2MSGD
      *
      *          ------------------------------------------
      *          Parameter list for OMH902C
      *          ------------------------------------------
     C     $X902C        PLIST
     C                   PARM                    PGMQ
      *
      *          ------------------------------------------
      *          Parameter list for OMH904C
      *          ------------------------------------------
     C     $X904C        PLIST
     C                   PARM                    MSGID
     C                   PARM                    MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGTXT
      *
      *          ------------------------------------------
      *          Parameter list for OMH933C
      *          ------------------------------------------
     C     $X933C        PLIST
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH961
      *          ------------------------------------------
     C     $MX961        PLIST
     C                   PARM                    PGM
     C                   PARM                    P#FILE
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
      *          Parameter list for OMQPRTIN
      *          ------------------------------------------
     C     $PRTIN        PLIST
     C                   PARM      'V3R0M0  '    I3INTL
     C                   PARM                    I3ACTC
     C                   PARM                    I3BUF
     C                   PARM                    I3PRC
     C                   PARM                    I3ERR
     C                   PARM                    I3FLD
      *
      *          ------------------------------------------
      *          Parameter list for OMQUSIIN
      *          ------------------------------------------
     C     $USIIN        PLIST
     C                   PARM      C#INTL        I2INTL
     C                   PARM                    I2ACTC
     C                   PARM                    I2INC
     C                   PARM                    I2PRCA
     C                   PARM                    I2ERR
     C                   PARM                    I2FUN#
     C                   PARM                    I2FUN
     C                   PARM                    I2FKU#
     C                   PARM                    I2CFLC
     C                   PARM                    I2KEY
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
      *          OMINCL1 Incident file
      *          ------------------------------------------
     C     $NC1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    WWINC#
      *
      *          ------------------------------------------
      *          OMH027W1 Workfile
      *          ------------------------------------------
     C     $027K1        KLIST
     C                   KFLD                    D#USID
      *
      *          ------------------------------------------
      *          Retrieve authorization
      *          ------------------------------------------
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '0'
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
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
     C                   EVAL      FMHEDC = P#HEDC
     C                   MOVEL     D8HEDD        FMHEDD
      *
      *          ------------------------------------------
      *          Get message data
      *          ------------------------------------------
     C                   EXSR      SRGVAR
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * *PSSR  - Standard RPG error handler                          *
      *                                                              *
      * ------------------------------------------------------------ *
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
     C                   WHEN      E#FILE = 'OMINCL1'                           X02
     C                   MOVEL     INCL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMH027W1'                          X02
     C                   MOVEL     H027W1        P#FILE
      *
     C                   WHEN      E#FILE = 'OMH027D '                          X02
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
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRTTLR - Total last record processing                        *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          Close called programs if necessary
      *          ------------------------------------------
     C                   IF        I#PRTI = *ON                                 B01
     C                   CALLP     OMQPRTIN_001()
     C                   ENDIF                                                  E01
      *
     C                   IF        I#USII = *ON                                 B01
     C                   CALLP     OMQUSIIN()
     C                   ENDIF                                                  E01
      *
     C                   IF        I#CHGI = *ON                                 B01
     C                   CALLP     OMQCHGIN()
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Delete all remaining records
      *          ------------------------------------------
     C/EXEC SQL
     C+ DELETE FROM OMH027W1
     C+   WHERE WWUSID = :D#USID
     C/END-EXEC
      *
      *          ------------------------------------------
      *          Fill return status
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B01
     C                   EVAL      P#STAT = C#EXIT
     C                   ELSE                                                   X01
     C                   IF        *INKL = *ON                                  B02
     C                   EVAL      P#STAT = C#CNCL
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Seton last record and return
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
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
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      %SUBST(MSGF:1:6) = 'OMHMSG'
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 4                                  B01
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
TXT0001 - text type                             1
WRD0001 - word or substring                     2
OPR0001 - Boolean operator                      3
TXT0002 - texts to search                       4
