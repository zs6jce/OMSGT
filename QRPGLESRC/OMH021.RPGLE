     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH021                                        *
      * Function ...: Incident log inquiry                          *
      * Author .....: Remain Software                               *
      * Parameters .: P#HEDC - HelpDesk code                        *
      *               P#INC# - Incident number                      *
      *               P#OUTC - Output option code (*, *PRINT)       *
      *               P#STAT - Return status                        *
      *                                                             *
      * This program lets the user inquire the incident log file.   *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMHILL1   IF   E           K DISK    INFDS(HILL1)
     F                                     INFSR(*PSSR)
     FOMHDDL1   IF   E           K DISK    INFDS(HDDL1)
     F                                     INFSR(*PSSR)
     FOMH021D   CF   E             WORKSTN INFDS(INFO)
     F                                     SFILE(OMH02101:RRN01)
     F                                     SFILE(OMH02104:RRN02)
     F                                     INFSR(*PSSR)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension specifications                                     *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * FT    Formatted message text                                 *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(1)
     D MSD             S              7    DIM(1) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
     D FT              S              1    DIM(8000)
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
      *          Helpdesk data structures.
      *          ------------------------------------------
      /COPY QCPYLESRC,OMHLPDS
      *
      *          ------------------------------------------
      *          File information data structures
      *          ------------------------------------------
     D HILL1           DS            57
     D HDDL1           DS            57
      *
      *          ------------------------------------------
      *          Program status data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  E#FILE               201    208
      *
      *          ------------------------------------------
      *          Line size
      *          ------------------------------------------
     D P#LSZ#          DS
     D W#LSZ#                         9B 0
      *
      *          ------------------------------------------
      *          Number of lines
      *          ------------------------------------------
     D P#NOL#          DS
     D W#NOL#                         9B 0
      *
      *          ------------------------------------------
      *          Number of first level lines
      *          ------------------------------------------
     D P#NFL#          DS
     D W#NFL#                         9B 0
      *
      *          ------------------------------------------
      *          Number of second level lines
      *          ------------------------------------------
     D P#NSL#          DS
     D W#NSL#                         9B 0
      *
      *          ------------------------------------------
      *          Severity level
      *          ------------------------------------------
     D P#SEVL          DS
     D W#SEVL                         9B 0
      *
      *          ------------------------------------------
      *          Severity level
      *          ------------------------------------------
     D P#JOB#          DS
     D W#JOB#                         6  0
      *
      *          ------------------------------------------
      *          P#MSI - RTVM0200 Format
      *
      *          The following table lists the fields in the RTVM0200
      *          format.
      *          Since the length of the message text, message help
      *          (second level text) and the default replay vary,
      *          these buffers are not included in this member
      *          This buffer must be copied into the program
      *          and adjusted to suit your specific needs.
      *
      *          DM2BR# (B4)   Bytes returned to buffer
      *          DM2BA# (B4)   Bytes available for buffer
      *          DM2MS# (B4)   Message severity
      *          DM2AI# (B4)   Alert index
      *          DM2AOC (C9)   Alert option code
      *          DM2LIC (C1)   Log indicator
      *          DM2R1  ( 2)   Reserved
      *          DM2RR# (B4)   Bytes of default reply returned
      *          DM2RA# (B4)   Bytes available for default reply
      *          DM2MR# (B4)   Length of message returned
      *          DM2MA# (B4)   Lentgh available for message
      *          DM2HR# (B4)   Length of message help returned
      *          DM2HA# (B4)   Length available for message help
      *          DM2DR  (*)    Default reply
      *          DM2MT  (*)    Message text buffer
      *          DM2MH  (*)    Message help buffer
      *          ------------------------------------------
     D P#MSI           DS
     D DM2BR#                         9B 0
     D DM2BA#                         9B 0
     D DM2MS#                         9B 0
     D DM2AI#                         9B 0
     D DM2AOC                         9
     D DM2LIC                         1
     D DM2R1                          2
     D DM2RR#                         9B 0
     D DM2RA#                         9B 0
     D DM2MR#                         9B 0
     D DM2MA#                         9B 0
     D DM2HR#                         9B 0
     D DM2HA#                         9B 0
     D RTNTXT                        80
      *
      *          ------------------------------------------
      *          Error buffer for API call
      *
      *          The number of bytes provided is an input parameter
      *          indicating the length of the error buffer in the
      *          calling program. This field is set to 272 thus
      *          providing a 256 byte message data field.
      *
      *          D#NBP# (B4)   Number of bytes provided
      *          D#NBA# (B4)   Number of bytes available
      *          D#MSID (A7)   Message id
      *                 (A1)   Reserved
      *          D#MSGD (A250) Message data
      *          ------------------------------------------
     D P#ERR           DS
     D D#NBP#                         9B 0 INZ(272)
     D D#NBA#                         9B 0
     D D#MSID                         7
     D D#MSGD                 17    272
      *
      *          ------------------------------------------
      *          API binary parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFBINARY
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#LIBL          C                   CONST('*LIBL     ')
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
      *
     D C#EXIT          C                   CONST('*EXIT')
     D C#CNCL          C                   CONST('*CNCL')
      *
     D C#09            C                   CONST('*REOPEN')
     D C#15            C                   CONST('*ALLOC')
     D C#20            C                   CONST('*CHANGE')
     D C#23            C                   CONST('*CONNEC')
     D C#25            C                   CONST('*ACCUM')
     D C#27            C                   CONST('*DISCON')
     D C#29            C                   CONST('*STATUS')
     D C#30            C                   CONST('*COPY')
     D C#33            C                   CONST('*ESCAL')
     D C#40            C                   CONST('*DELET')
     D C#42            C                   CONST('*COMP')
     D C#60            C                   CONST('*ADD')
     D C#65            C                   CONST('*PAGER')
     D C#85            C                   CONST('*DEALC')
     D C#87            C                   CONST('*OMS')
     D C#99            C                   CONST('*CLOSE')
      * Prototype for OMH021
     D OMH021          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#INC#_                             LIKE(P#INC#)
     D P#OUTC_                             LIKE(P#OUTC)
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
      * Prototype for 'OMH908C'
     D OMH908C         PR                  EXTPGM('OMH908C')
     D P#OPTI_                             LIKE(P#OPTI)
     D P#INDT_                             LIKE(P#INDT)
     D P#OTDT_                             LIKE(P#OTDT)
     D P#INFM_                             LIKE(P#INFM)
     D P#OTFM_                             LIKE(P#OTFM)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMX020'
     D OMX020          PR                  EXTPGM('OMX020')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGDTA_                             LIKE(MSGDTA)
     D P#LSZ#_                             LIKEDS(P#LSZ#)
     D P#SEVL_                             LIKEDS(P#SEVL)
     D FT_                                 LIKE(FT) DIM(8000)
     D P#NOL#_                             LIKEDS(P#NOL#)
     D P#NFL#_                             LIKEDS(P#NFL#)
     D P#NSL#_                             LIKEDS(P#NSL#)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (Send message)
      * Prototype for 'QMHRTVM'
     D QMHRTVM         PR                  EXTPGM('QMHRTVM')
     D P#MSI_                              LIKEDS(P#MSI)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#FMTC_                             LIKE(P#FMTC)
     D P#MIDC_                             LIKE(P#MIDC)
     D P#MSGQ_                             LIKE(P#MSGQ)
     D MSGDTA_                             LIKE(MSGDTA)
     D P#MDL#_                             LIKE(P#MDL#)
     D P#RSVC_                             LIKE(P#RSVC)
     D P#RFCC_                             LIKE(P#RFCC)
     D P#ERR_                              LIKEDS(P#ERR)
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
      *          Retrieve authorization
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
     D OMH021          PI
     D P#HEDC                         5
     D P#INC#                        10
     D P#OUTC                         6
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D s#indt          S              7P 0
     D s#intm          S              6P 0
     D s#rr01          S              5P 0
     D w#last          S              5P 0
     D w#valc          S              5P 0
     D EOF01           S                   LIKE(INDI)
     D EOF02           S                   LIKE(INDI)
     D EROR01          S                   LIKE(INDI)
     D EROR02          S                   LIKE(INDI)
     D I#LIND          S                   LIKE(INDI)
     D I#PSSR          S              1
     D IX              S              5  0
     D LOOP01          S                   LIKE(INDI)
     D LOOP02          S                   LIKE(INDI)
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#FILE          S             57
     D P#INDT          S              6  0
     D P#INFM          S              1
     D P#MSTA          S             40
     D P#OPTI          S              1
     D P#OTDT          S              6  0
     D P#OTFM          S              1
     D P#SUL#          S                   LIKE(SUL#)
     D RRN01           S              5  0
     D RRN02           S              5  0
     D W#CRC#          S              5  0
     D W#MSGI          S              1
     D W#OPTI          S                   LIKE(FMOPTI)
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
     C                   EVAL      MSGF = 'OMHMSG'
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
      * ------------------------------------------------------------ *
      * SRFUTY - Convert date From user to YYMMDD format             * /
      *                                                              * /
      * This routine calls a subprogram which converts a date to     * /
      * user format                                                  * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRFUTY        BEGSR
      *
      *          ------------------------------------------
      *          Convert dates
      *          ------------------------------------------
     C                   EVAL      P#OTDT = *ZEROS
     C                   EVAL      P#INFM = '?'
     C                   EVAL      P#OTFM = 'Y'
     C                   CALLP     OMH908C ( P#OPTI : P#INDT : P#OTDT :
     C                             P#INFM : P#OTFM : P#STAT )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRFYTU - Convert date From YYMMDD format to user format      * /
      *                                                              * /
      * This routine calls a subprogram which converts a date to     * /
      * user format                                                  * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRFYTU        BEGSR
      *
      *          ------------------------------------------
      *          Convert dates
      *          ------------------------------------------
     C                   EVAL      P#OTDT = *ZEROS
     C                   EVAL      P#INFM = 'Y'
     C                   EVAL      P#OTFM = '?'
     C                   CALLP     OMH908C ( P#OPTI : P#INDT : P#OTDT :
     C                             P#INFM : P#OTFM : P#STAT )
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
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH02103
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        RRN01 = *ZEROS                               B03
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   EXFMT     OMH02102
      *
      *          ------------------------------------------
      *          F3 and F12 is exit
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B03
     C                             OR *INKL = *ON
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
      *          *IN25=Roll up
      *          ------------------------------------------
     C                   IF        *IN25 = *ON                                  B04
     C                   EXSR      SRRU01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          F18=Bottom
      *          ------------------------------------------
     C                   IF        *INKS = *ON                                  B05
     C                   EXSR      SRBOT
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          F17=Top
      *          ------------------------------------------
     C                   IF        *INKR = *ON                                  B06
     C                   Z-ADD     1             CRRN01
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
     C                   IF        *INKL = *OFF                                 B02
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
      * ----------------------------------------------------------- *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Fill screen fields with parameter values
      *          This is done here (and not in *INZSR)
      *          because parameters can change between
      *          calls and *INZSR is executed only once.
      *          ------------------------------------------
     C                   MOVE      P#HEDC        FMHEDC
     C                   MOVE      P#INC#        FMINC#
      *
      *          ------------------------------------------
      *          Get HelpDesk description
      *          ------------------------------------------
     C     P#HEDC        CHAIN     OMHDDR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL     DDHEDD        FMHEDD
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
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      RRN01 = *ZEROS
     C                   EVAL      IX = *ZEROS
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH02102
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Position on first record
      *          ------------------------------------------
     C     $IL1K1        SETLL     OMHILL1
      *
      *          ------------------------------------------
      *          Read first record
      *          ------------------------------------------
     C     $IL1K1        READE     OMHILL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Fill end-of-file indicator
      *          ------------------------------------------
     C                   EVAL      EOF01 = *IN99
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       EOF01 = *OFF                                 B01
     C                             AND IX < 10
      *
      *          ------------------------------------------
      *          Clear the message data fields
      *          ------------------------------------------
     C                   EVAL      MSGDTA = *BLANKS
     C                   EVAL      MC = *BLANKS
      *
      *          ------------------------------------------
      *          Fill the subfile, get the message
      *          ------------------------------------------
     C                   MOVE      ILMSID        MSGID
     C                   MOVEL     ILMSGF        P#MSGQ
     C                   MOVEA     ILMSGD        MC(1)
      *
      *          ------------------------------------------
      *          Format the message with the message data
      *          ------------------------------------------
     C                   MOVEA     MC            MSGDTA
     C                   EXSR      SRRTVM
      *
      *          ------------------------------------------
      *          Extract formatted message
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMMSGT
     C                   EVAL      %SUBST(FMMSGT:1:DM2MR#) = %SUBST(RTNTXT:
     C                             1:DM2MR#)
      *
      *          ------------------------------------------
      *          Fill field Abbreviated action code
      *          ------------------------------------------
     C                   EXSR      SRABAC
      *
      *          ------------------------------------------
      *          Fill other subfile fields
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      ILUSID        FMUSID
      *
      *          ------------------------------------------
      *          Fill subfile record hidden fields
      *          ------------------------------------------
     C                   MOVEA     MC            FMMSGD
     C                   MOVE      ILMSID        FMMSID
     C                   MOVE      ILMSGF        FMMSGF
     C                   Z-ADD     ILINDT        FMINDT
     C                   Z-ADD     ILINTM        FMINTM
     C                   ADD       1             RRN01
     C                   ADD       1             IX
      *
     C                   WRITE     OMH02101
      *
      *          ------------------------------------------
      *          Save fields
      *          ------------------------------------------
     C                   Z-ADD     FMINDT        S#INDT
     C                   Z-ADD     FMINTM        S#INTM
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     $IL1K1        READE     OMHILL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Fill end-of-file indicator
      *          ------------------------------------------
     C                   EVAL      EOF01 = *IN99
      *
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Fill current subfile relative record
      *          number to display first subfile page
      *          ------------------------------------------
     C                   Z-ADD     1             CRRN01
      *
      *          ------------------------------------------
      *          Save overall relative record number
      *          ------------------------------------------
     C                   EVAL      S#RR01 = RRN01
      *
      *          ------------------------------------------
      *          Seton subfile end indicator
      *          ------------------------------------------
     C                   IF        EOF01 = *ON                                  B01
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
      * SRRU01 - Roll up screen 1                                   *
      *                                                             *
      * This routine is called if the user pressed roll up          *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRU01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      RRN01 = S#RR01
     C                   EVAL      IX = *ZEROS
      *
      *          ------------------------------------------
      *          Position after record that was last added
      *          ------------------------------------------
     C     $IL1K2        SETGT     OMHILL1
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     $IL1K1        READE     OMHILL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Fill end-of-file indicator
      *          ------------------------------------------
     C                   EVAL      EOF01 = *IN99
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       EOF01 = *OFF                                 B01
     C                             AND IX < 10
      *
      *          ------------------------------------------
      *          Clear the message data fields
      *          ------------------------------------------
     C                   EVAL      MSGDTA = *BLANKS
     C                   EVAL      MC = *BLANKS
      *
      *          ------------------------------------------
      *          Fill the subfile, get the message
      *          ------------------------------------------
     C                   MOVE      ILMSID        MSGID
     C                   MOVEL     ILMSGF        P#MSGQ
     C                   MOVEA     ILMSGD        MC(1)
      *
      *          ------------------------------------------
      *          Format the message with the message data
      *          ------------------------------------------
     C                   MOVEA     MC            MSGDTA
     C                   EXSR      SRRTVM
      *
      *          ------------------------------------------
      *          Extract formatted message
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMMSGT
     C                   EVAL      %SUBST(FMMSGT:1:DM2MR#) = %SUBST(RTNTXT:
     C                             1:DM2MR#)
      *
      *          ------------------------------------------
      *          Fill field Abbreviated action code
      *          ------------------------------------------
     C                   EXSR      SRABAC
      *
      *          ------------------------------------------
      *          Fill other subfile fields
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      ILUSID        FMUSID
      *
      *          ------------------------------------------
      *          Fill subfile record hidden fields
      *          ------------------------------------------
     C                   MOVEA     MC            FMMSGD
     C                   MOVE      ILMSID        FMMSID
     C                   MOVE      ILMSGF        FMMSGF
     C                   Z-ADD     ILINDT        FMINDT
     C                   Z-ADD     ILINTM        FMINTM
     C                   ADD       1             RRN01
     C                   ADD       1             IX
      *
     C                   WRITE     OMH02101
      *
      *          ------------------------------------------
      *          Save fields
      *          ------------------------------------------
     C                   Z-ADD     FMINDT        S#INDT
     C                   Z-ADD     FMINTM        S#INTM
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     $IL1K1        READE     OMHILL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Fill end-of-file indicator
      *          ------------------------------------------
     C                   EVAL      EOF01 = *IN99
      *
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   EVAL      S#RR01 = RRN01
      *
      *          ------------------------------------------
      *          Fill current subfile relative record number
      *          to display the page just filled
      *          ------------------------------------------
     C                   IF        IX > *ZEROS                                  B01
     C     W#CRC#        ADD       10            CRRN01
     C                   IF        CRRN01 > RRN01                               B02
     C                   Z-ADD     RRN01         CRRN01
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If end of file then no more records (SFLEND)
      *          ------------------------------------------
     C                   IF        EOF01 = *ON                                  B01
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
      * SRABAC - Fill field Abbreviated action code                 *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRABAC        BEGSR
      *
      *          ------------------------------------------
      *          Fill field according to action code
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      ILACTC = '09'                                X01
     C                   MOVEL(P)  C#09          FMACTA
     C                   WHEN      ILACTC = '15'                                X01
     C                   MOVEL(P)  C#15          FMACTA
     C                   WHEN      ILACTC = '20'                                X01
     C                   MOVEL(P)  C#20          FMACTA
     C                   WHEN      ILACTC = '23'                                X01
     C                   MOVEL(P)  C#23          FMACTA
     C                   WHEN      ILACTC = '25'                                X01
     C                   MOVEL(P)  C#25          FMACTA
     C                   WHEN      ILACTC = '27'                                X01
     C                   MOVEL(P)  C#27          FMACTA
     C                   WHEN      ILACTC = '29'                                X01
     C                   MOVEL(P)  C#29          FMACTA
     C                   WHEN      ILACTC = '30'                                X01
     C                   MOVEL(P)  C#30          FMACTA
     C                   WHEN      ILACTC = '33'                                X01
     C                   MOVEL(P)  C#33          FMACTA
     C                   WHEN      ILACTC = '40'                                X01
     C                   MOVEL(P)  C#40          FMACTA
     C                   WHEN      ILACTC = '42'                                X01
     C                   MOVEL(P)  C#42          FMACTA
     C                   WHEN      ILACTC = '60'                                X01
     C                   MOVEL(P)  C#60          FMACTA
     C                   WHEN      ILACTC = '65'                                X01
     C                   MOVEL(P)  C#65          FMACTA
     C                   WHEN      ILACTC = '85'                                X01
     C                   MOVEL(P)  C#85          FMACTA
     C                   WHEN      ILACTC = '87'                                X01
     C                   MOVEL(P)  C#87          FMACTA
     C                   WHEN      ILACTC = '99'                                X01
     C                   MOVEL(P)  C#99          FMACTA
     C                   ENDSL                                                  E01
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
      * where correctly filled. The subfile options allowed are     *
      * '5' to display a detailed message description.              *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
     C                   EVAL      W#VALC = *ZEROS
     C                   EVAL      CRRN01 = *ZEROS
     C                   EVAL      W#MSGI = *OFF
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   READC(E)  OMH02101
     C                   EVAL      *IN99 = %ERROR OR %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Do if option is not blank
      *          ------------------------------------------
     C                   IF        FMOPTI <> *BLANKS                            B02
      *
      *          ------------------------------------------
      *          If option is valid, then check the number of valid
      *          codes. *IN93 is the subfile next change indicator
      *          ------------------------------------------
     C                   IF        FMOPTI = '5'                                 B03
     C                   ADD       1             W#VALC
      *
      *          ------------------------------------------
      *          If option is other than valid, then field is in error
      *          Reverse screen fields to indicate this
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
      *
      *          ------------------------------------------
      *          Send the message which tells what's wrong
      *          ------------------------------------------
     C                   IF        W#MSGI = *OFF                                B04
     C                   MOVEL     FMOPTI        MSGDTA
     C                   EVAL      MSGID = 'OPTE021'
     C                   EXSR      SNDMSG
     C                   EVAL      W#MSGI = *ON
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If an error was found, then position on the page of
      *          the first error
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B03
     C                             AND *IN70 = *ON
     C                   Z-ADD     RRN01         CRRN01
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Update subfile
      *          ------------------------------------------
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH02101
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   UPDATE    OMH02101
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC(E)  OMH02101
     C                   EVAL      *IN99 = %ERROR OR %EOF
      *
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Fill the current relative record on the page to
      *          display
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B01
     C                   Z-ADD     W#CRC#        CRRN01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR01 - Process screen 1                                   *
      *                                                             *
      * This routine displays a selected message by calling the     *
      * third screen routines                                       *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          If records in subfile and valid codes found
      *          ------------------------------------------
     C                   IF        RRN01 > *ZEROS                               B01
     C                             AND W#VALC > *ZEROS
      *
      *          ------------------------------------------
      *          Read subfile next changed record
      *          ------------------------------------------
     C                   READC     OMH02101
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Read subfile until valid option found
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Reloop for options where changed
      *          ------------------------------------------
     C                   EVAL      LOOP01 = *ON
     C                   EVAL      W#CRC# = RRN01
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPTI        W#OPTI
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    OMH02101
      *
      *          ------------------------------------------
      *          Do action if option is '5'
      *          ------------------------------------------
     C                   IF        W#OPTI <> *BLANKS                            B03
     C                   EXSR      SRSC02
     C                   IF        *INKC = *ON                                  B04
     C                             OR *INKL = *ON
     C                   LEAVE
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next changed
      *          ------------------------------------------
     C                   READC     OMH02101
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If end of file found, then end loop
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Indicate which record should be displayed
      *          ------------------------------------------
     C                   Z-ADD     W#CRC#        CRRN01
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
      * SRSC02 - Screen 2 processing routine                        *
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
      *          If subfile is empty, then write record to
      *          clear some lines on the display
      *          ------------------------------------------
     C                   IF        RRN02 = *ZEROS                               B03
     C                   Z-ADD     1             LIN
     C                   Z-ADD     1             COL
     C                   WRITE     OMH021CL
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH02106
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        RRN02 = *ZEROS                               B03
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   Z-ADD     FMINDT        FMIND6
     C                   EXFMT     OMH02105
      *
      *          ------------------------------------------
      *          Exit is exit
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
      *          Re-display init
      *          ------------------------------------------
     C                   EVAL      EROR02 = *OFF
     C                   EVAL      LOOP02 = *OFF
      *
      *          ------------------------------------------
      *          Command key processing
      *          F5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B03
     C                   Z-ADD     1             CRRN02
     C                   EVAL      EROR02 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors or command key
      *          ------------------------------------------
     C                   ENDDO                                                  E02
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
      * ----------------------------------------------------------- *
     C     SRIN02        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      RRN02 = *ZEROS
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   Z-ADD     FMINDT        FMIND6
     C                   WRITE     OMH02105
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Call the refresh
      *          ------------------------------------------
     C                   EXSR      SRRF02
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
      *          ------------------------------------------
      *          Format the date to user format
      *          ------------------------------------------
     C                   Z-ADD     FMINDT        P#INDT
     C                   EXSR      SRFYTU
     C                   Z-ADD     P#OTDT        FMINDT
     C                   IF        FMINDT < 40
     C                   ADD       1000000       FMINDT
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Call the message formatter
      *          ------------------------------------------
     C                   MOVE      FMMSID        MSGID
     C                   MOVEL     FMMSGD        MSGDTA
     C                   MOVE      FMMSGF        MSGF
     C                   EVAL      W#LSZ# = 79
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMX020 ( MSGID : MSGF : MSGLIB : MSGDTA :
     C                              P#LSZ# : P#SEVL : FT : P#NOL# : P#NFL# :
     C                              P#NSL# : P#STAT )
      *
      *          ------------------------------------------
      *          The message is formatted and it exists in
      *          array FT. The number of lines exist in W#NOL#.
      *          ------------------------------------------
     C                   IF        W#NOL# > *ZEROS                              B01
     C                   EVAL      IX = 1
     C                   FOR       RRN02 = 1 TO W#NOL#                          B02
     C                   MOVEA     FT(IX)        FMSECL
     C                   ADD       W#LSZ#        IX
     C                   WRITE     OMH02104
     C                   ENDFOR                                                 E02
      *
      *          ------------------------------------------
      *          Set currenct record number
      *          ------------------------------------------
     C                   Z-ADD     1             CRRN02
      *
      *          ------------------------------------------
      *          No data found
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   EVAL      CRRN02 = *ZEROS
     C                   EVAL      RRN02 = *ZEROS
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Setof subfile end
      *          ------------------------------------------
     C                   EVAL      *IN92 = *OFF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRBOT  - F18=Bottom                                         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRBOT         BEGSR
      *
      *          ------------------------------------------
      *          If not allready end of file
      *          ------------------------------------------
     C                   IF        EOF02 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          93 is used for FRCDTA and LOCK
      *          ------------------------------------------
     C                   EVAL      *IN93 = *ON
      *
      *          ------------------------------------------
      *          Send 'Please wait ...' message
      *          ------------------------------------------
     C                   EVAL      MSGID = 'WAI0001'
     C                   EXSR      SNDMSG
      *
      *          ------------------------------------------
      *          Display the message and setof FRCDTA & LOCK indicator
      *          ------------------------------------------
     C                   WRITE     MSGCTL
     C                   Z-ADD     FMINDT        FMIND6
     C                   WRITE     OMH02105
     C                   EXSR      CLRMSG
     C                   EVAL      *IN93 = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Position on last subfile record
      *          ------------------------------------------
     C                   Z-ADD     RRN02         CRRN02
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPRTL - Print the log                                      *
      *                                                             *
      * This routine is called in the first screen routine. It calls*
      * the log printer OMH0212.                                    *
      *                                                             *
      * ----------------------------------------------------------- *
      ******     SRPRTL    BEGSR
      *
      *          ------------------------------------------
      *          Call the program
      *          ------------------------------------------
      ******               Z-ADDFMJOB#    W#JOB#
      ******               CALL 'OMH0212' $I0042
      *
      *          ------------------------------------------
      *          Send the message
      *          ------------------------------------------
      ******               MOVEAFMJOBN    MC,1
      ******               MOVEAFMUSID    MC,11
      ******               Z-ADDFMJOB#    W#JOB#
      ******               MOVEAP#JOB#    MC,21
      ******               MOVEAMC        MSGDTA
      ******               MOVE 'LOG0002' MSGID
      ******               EXSR SNDMSG
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
      ******               ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRTVM - Retrieve message                                    * /
      *                                                              * /
      * The retrieve message (QMHRTVM) API retrieves the message     * /
      * definition of a predefined message. It consists of the text  * /
      * of the message and other information such as the second      * /
      * level text and the default reply for the message. You can    * /
      * use this API to copy the text of predefined messages into a  * /
      * program.                                                     * /
      *                                                              * /
      * Author .....: Remain Software                               *
      *              *USE for the message file library               * /
      *                                                              * /
      * Input      - P#LOB#  Buffer length of message information    *
      *              P#FMTC  Format name                             *
      *              P#MIDC  Message id                              *
      *              P#MSGQ  Message file qualifier                  *
      *              P#MSD   Message data buffer                     *
      *              P#MDL#  Message data lenght                     *
      *              P#RSVC  Replace substition variables            *
      *                      *NO  - Do not use the message data      *
      *                             but return the &x fields.        *
      *                      *YES - Substitute the message data      *
      *              P#RFCC  Return format control characters        *
      *                      *NO  - Do not return the characters     *
      *                             &B, &N and &P.                   *
      *                      *YES - Return them into the text.       *
      *                                                              *
      * Output     - P#MSI   Message information buffer see the      *
      *                      RTVM0100 and RTVM0200 members for       *
      *                      details.                                *
      *              P#ERR   Error buffer                            *
      *                                                              *
      * Destroys   -                                                 *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRTVM        BEGSR
      *
      *          ------------------------------------------
      *          Retrieve message
      *          ------------------------------------------
     C                   EVAL      P#LOB# = 115
     C                   EVAL      P#FMTC = 'RTVM0200'
     C                   EVAL      P#MIDC = MSGID
     C                   EVAL      P#MDL# = 256
     C                   EVAL      P#RSVC = '*YES'
     C                   EVAL      P#RFCC = '*NO'
     C                   CALLP     QMHRTVM ( P#MSI : P#LOB# : P#FMTC :
     C                             P#MIDC : P#MSGQ : MSGDTA : P#MDL# :
     C                             P#RSVC : P#RFCC : P#ERR )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
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
     C                   EVAL      RRN02 = *ZEROS
     C                   EVAL      MS = *ZEROS
     C                   EVAL      IX = *ZEROS
      *
      *          ------------------------------------------
      *          Indicator definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
     C                   EVAL      P#OUTC = *BLANKS
      *
      *          ------------------------------------------
      *          Save field definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
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
      *          OMX020 retrieve formatted message.
      *          ------------------------------------------
     C     $MX020        PLIST
     C                   PARM                    MSGID
     C                   PARM                    MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGDTA
     C                   PARM                    P#LSZ#
     C                   PARM                    P#SEVL
     C                   PARM                    FT
     C                   PARM                    P#NOL#
     C                   PARM                    P#NFL#
     C                   PARM                    P#NSL#
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (Send message)
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
      *          OMHILL1 Incident log file
      *          ------------------------------------------
     C     $IL1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#INC#
      *
     C     $IL1K2        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#INC#
     C                   KFLD                    S#INDT
     C                   KFLD                    S#INTM
      *
      *          ------------------------------------------
      *          Exit if no parameters
      *          ------------------------------------------
     C                   IF        D#PARM = *ZEROS                              B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the message qualifier (last 10 pos)
      *          ------------------------------------------
     C                   MOVE      C#LIBL        P#MSGQ
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
      *          Retrieve authorization
      *          ------------------------------------------
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '0'
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
      *
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
     C                   WHEN      E#FILE = 'OMHILL1 '                          X02
     C                   MOVEL     HILL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHDDL1 '                          X02
     C                   MOVEL     HDDL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMH021D '                          X02
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
      *          Error in *PSSR, exit immediately.
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
      *          Free the message formatter
      *          ------------------------------------------
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
      *          (*INLR is (temporarily) always set on)
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
