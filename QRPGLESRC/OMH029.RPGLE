      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Copyright ..: Remain BV, Netherlands. All Rights Reserved.  *
      *                                                             *
      * Program ....: OMH029.                                       *
      * Function ...: Manage helpdesk info.                         *
      * Author .....: Remain Software                               *
      * Parameters .: P#OPTI, requested action                      *
      *               P#HEDC, Helpdesk code                         *
      *               P#SUL#, In=required, Out=Actual auth code.    *
      *               P#STAT, program return status.                *
      *               P#DT1,  System info                optional   *
      *               P#DT2,  Helpdesk info              optional   *
      *               P#CIM,  Check Incident Monitor     optional   *
      *                       default = *YES                        *
      *                                                             *
      * OPTI=1  Cross helpdesk check                                *
      * OPTI=2  Activate a helpdesk      (Ask when none received)   *
      * OPTI=3  Retrieve helpdesk info                              *
      * OPTI=4  Reset current Helpdesk                              *
      *                                                             *
      * For opti 2 and 3 the OMHDT return parameters are filled     *
      * when available.                                             *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used files                                                  *
      * ----------------------------------------------------------- *
      *                                                             *
      * OMHDDL1 - Helpdesk file (I)                                 *
      * OMHDEL1 - Employee file (I).                                *
      * OMH029D - Display file (I/O).                               *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Sub programs                                                *
      * ----------------------------------------------------------- *
      *                                                             *
      * OMH901C - Send message to message queue.                    *
      * OMH902C - Clear message queue.                              *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators                                             *
      * ----------------------------------------------------------- *
      *                                                             *
      * KC - Command key F3 pressed.                                *
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
     FOMHDDL1   IF   E           K DISK    INFDS(HDDL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMHDEL1   IF   E           K DISK    INFDS(HDEL1)
     F                                     USROPN
     F                                     INFSR(*PSSR)
     FOMH029D   CF   E             WORKSTN INFDS(INFO)
     F                                     USROPN
     F                                     INFSR(*PSSR)
      * ----------------------------------------------------------- *
      /EJECT
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Input pecifications                                         *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  D#OBJC                 1     10
     D  D#OBJL                81     90
     D  E#FILE               201    208
     D  P#USID               254    263
      *
      *          ------------------------------------------
      *          File information data structures
      *          ------------------------------------------
     D HDDL1           DS            57
     D HDEL1           DS            57
     D INFO            DS            57
      *
      *          ------------------------------------------
      *          The field reference file
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          Get helpdesk data structures.
      *          ------------------------------------------
      /COPY QCPYLESRC,OMHLPDS
      *
      *          ------------------------------------------
      *          API parameter definitions
      *          ------------------------------------------
      /COPY QAPILESRC,DEFBINARY
      *
      *          ------------------------------------------
      *          Error buffer for API calls
      *          ------------------------------------------
      /COPY QAPILESRC,ERR_BUFFER
      *
      *          ------------------------------------------
      *          P#JBI - JOBI0100 Format
      *
      *          The following table lists the fields in the JOBI0100
      *          (job information, basic performance info) format.
      *          This buffer must be copied into the program
      *          and adjusted to suit your specific needs.
      *
      *          JI1BR# (B4)   Bytes returned to buffer
      *          JI1BA# (B4)   Bytes available for buffer
      *          JI1JNC (C10)  Job name code
      *          JI1JUC (C10)  Job user code
      *          JI1J#C (C6)   Job number code
      *          JI1IJC (C16)  Internal job identifier code
      *          JI1JSC (C10)  Job status code
      *          JI1JTC (C1)   Job type code
      *          JI1STC (C1)   Job subtype code
      *          JI1R1  (C2)   Reserved
      *          JI1RP# (B4)   Run priority
      *          JI1TS# (B4)   Time slice
      *          JI1DW# (B4)   Default wait
      *          JI1PUC (C10)  Purge code
      *          ------------------------------------------
     D P#JBI1          DS
     D JI1BR#                         9B 0
     D JI1BA#                         9B 0
     D JI1JNC                        10
     D JI1JUC                        10
     D JI1J#C                         6
     D JI1IJC                        16
     D JI1JSC                        10
     D JI1JTC                         1
     D JI1STC                         1
     D JI1R1                          2
     D JI1RP#                         9B 0
     D JI1TS#                         9B 0
     D JI1DW#                         9B 0
     D JI1PUC                        10
      *
      *          ------------------------------------------
      *          P#RMI - RCVM0100 Format
      *
      *          The following table lists the fields in the RCVM0100
      *          format.
      *          Since the length of the message text and message help
      *          varies, these buffers are not included in the copy
      *          member. This buffer must be copied into the program
      *          and adjusted to suit your specific needs.
      *
      *          RM1BR# (B4)   Bytes returned to buffer
      *          RM1BA# (B4)   Bytes available for buffer
      *          RM1MS# (B4)   Message severity
      *          RM1MIC (A7)   Message identifier code
      *          RM1MTC (A2)   Message type code
      *          RM1MKC (A4)   Message key code
      *          RM1R1  (A15)  Reserved
      *          RM1MR# (B4)   Message data bytes returned to buffer
      *          RM1MA# (B4)   Message data bytes available in buffer
      *          RM1MD  (*)    Message data buffer
      *          ------------------------------------------
     D P#RMI           DS
     D RM1BR#                         9B 0
     D RM1BA#                         9B 0
     D RM1MS#                         9B 0
     D RM1MIC                         7
     D RM1MTC                         2
     D RM1MKC                         4
     D RM1R1                         15
     D RM1MR#                         9B 0
     D RM1MA#                         9B 0
     D RM1MD                         15
     D  RM1RS                         5    OVERLAY(RM1MD)
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#EXIT          C                   CONST('*EXIT')
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#AUTH          C                   CONST('*AUTH')
     D C#NOTF          C                   CONST('*NOTF')
     D C#GLNV          C                   CONST('*GLNV')
     D C#GLEX          C                   CONST('*GLEX')
     D C#MLNV          C                   CONST('*MLNV')
     D C#MLEX          C                   CONST('*MLEX')
     D C#SECO          C                   CONST('QSECOFR   ')
     D C#SECA          C                   CONST('QSECADM   ')
     D C#SECC          C                   CONST('*SECOFR   ')
     D C#ADMC          C                   CONST('*SECADM   ')
     D C#NONE          C                   CONST('*NONE     ')
     D C#OMSG          C                   CONST('OMHMSG    *LIBL     ')
     D C#OMSU          C                   CONST('OMHUSR    QTEMP     ')
     D C#CRTO          C                   CONST('Created by OMS      ')
     D C#PGM           C                   CONST('*PGM')
     D C#USP           C                   CONST('*USRSPC')
     D C#YES           C                   CONST('*YES')
     D C#NO            C                   CONST('*NO')
      * Prototype for 'OMX105'
     D OMX105          PR                  EXTPGM('OMX105')
     D FMHEDC_                             LIKE(FMHEDC)
     D HEDD_                               LIKE(HEDD)
      * Prototype for 'OMH902C'
     D OMH902C         PR                  EXTPGM('OMH902C')
     D PGMQ_                               LIKE(PGMQ)
      * Prototype for 'OMH901C'
     D OMH901C         PR                  EXTPGM('OMH901C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGDTA_                             LIKE(MSGDTA)
      * Prototype for 'QUSCRTUS'
     D QUSCRTUS        PR                  EXTPGM('QUSCRTUS')
     D P#USPQ_                             LIKE(P#USPQ)
     D P#EXAC_                             LIKE(P#EXAC)
     D P#ISZ#_                             LIKE(P#ISZ#)
     D P#IVAC_                             LIKE(P#IVAC)
     D P#PAUC_                             LIKE(P#PAUC)
     D P#USPD_                             LIKE(P#USPD)
     D P#RPLC_                             LIKE(P#RPLC)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'OMH905C'
     D OMH905C         PR                  EXTPGM('OMH905C')
     D D7USID_                             LIKE(D7USID)
     D D7USCL_                             LIKE(D7USCL)
     D D7GRID_                             LIKE(D7GRID)
     D D7GRCL_                             LIKE(D7GRCL)
     D D7USDC_                             LIKE(D7USDC)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'QUSCHGUS'
     D QUSCHGUS        PR                  EXTPGM('QUSCHGUS')
     D P#USPQ_                             LIKE(P#USPQ)
     D P#STP#_                             LIKE(P#STP#)
     D P#LOB#_                             LIKE(P#LOB#)
     D D#D7HD_                             LIKE(D#D7HD)
     D P#FRCC_                             LIKE(P#FRCC)
      * Prototype for 'QUSRTVUS'
     D QUSRTVUS        PR                  EXTPGM('QUSRTVUS')
     D P#USPQ_                             LIKE(P#USPQ)
     D P#STP#_                             LIKE(P#STP#)
     D P#LOB#_                             LIKE(P#LOB#)
     D D#D7HD_                             LIKE(D#D7HD)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'QUSRJOBI'
     D QUSRJOBI        PR                  EXTPGM('QUSRJOBI')
     D P#JBI1_                             LIKEDS(P#JBI1)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#FMTC_                             LIKE(P#FMTC)
     D P#JOBN_                             LIKE(P#JOBN)
     D P#IJIC_                             LIKE(P#IJIC)
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
      * Prototype for 'QMHSNDPM'
     D QMHSNDPM        PR                  EXTPGM('QMHSNDPM')
     D P#MIDC_                             LIKE(P#MIDC)
     D P#MSGQ_                             LIKE(P#MSGQ)
     D P#MSD_                              LIKE(P#MSD)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#MSTC_                             LIKE(P#MSTC)
     D P#PMQC_                             LIKE(P#PMQC)
     D P#PSC#_                             LIKE(P#PSC#)
     D P#MSKC_                             LIKE(P#MSKC)
     D P#ERR_                              LIKE(P#ERR)
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D s#seq#          S              5P 0
     D EROR01          S                   LIKE(*IN99)
     D I#DSP           S                   LIKE(*IN99)
     D I#HDD           S                   LIKE(*IN99)
     D I#HDE           S                   LIKE(*IN99)
     D I#PDL           S                   LIKE(*IN99)
     D I#PSSR          S              1
     D I#TTLR          S                   LIKE(*IN99)
     D K#BLNK          S                   LIKE(USID)
     D K#USID          S                   LIKE(USID)
     D LOOP01          S                   LIKE(*IN99)
     D MSGDTA          S            256
     D* MSGF            S             10
     D MSGID           S              7
     D MSGLIB          S             10
     D NUL             S              3  0
     D P#CIM           S              4
     D P#CURA          S                   LIKE(INDI)
     D P#DT1           S                   LIKE(D#D7HD)
     D P#DT2           S                   LIKE(D#D8HD)
     D P#FILE          S             57
     D P#HEDC          S                   LIKE(HEDC)
     D P#IJIC          S             16
     D P#INKC          S                   LIKE(*IN99)
     D P#JOBN          S             30
     D P#MSD           S             50
     D P#MSTA          S             40
     D P#OBJC          S            - 2    LIKE(OBJC)
     D P#OBJL          S                   LIKE(OBJL)
     D P#OBJT          S                   LIKE(OBJT)
     D P#OPTI          S                   LIKE(*IN99)
     D P#OWN           S                   LIKE(USID)
     D P#STAT          S                   LIKE(STAT)
     D P#SUL#          S                   LIKE(SUL#)
     D S#CIM           S                   LIKE(P#CIM)
     D S#HEDC          S                   LIKE(HEDC)
     D S#HED2          S                   LIKE(HEDC)
     D S#STAT          S                   LIKE(P#STAT)
     D S#SUL#          S                   LIKE(SUL#)
     D S#SUL2          S                   LIKE(SUL#)
     D W#CHED          S                   LIKE(HEDC)
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
     C     *ENTRY        PLIST
     C                   PARM                    P#OPTI
     C                   PARM                    P#HEDC
     C                   PARM                    P#SUL#
     C                   PARM                    P#STAT
     C                   PARM                    P#DT1
     C                   PARM                    P#DT2
     C                   PARM                    P#CIM
      *
      *          ------------------------------------------
      *          Initialize fields
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
      *
      *          ------------------------------------------
      *          Initialize CIM with *YES if is was not
      *          specified.
      *          ------------------------------------------
     C                   IF        D#PARM >= 7
     C                   EVAL      S#CIM = P#CIM
     C                   ELSE
     C                   EVAL      S#CIM = C#YES
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Processing depends on OPTI
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      P#OPTI = '1'
     C                   EXSR      SRPROC
     C                   WHEN      P#OPTI = '2'
     C                   EXSR      SRGETH
     C                   WHEN      P#OPTI = '3'
     C                   EXSR      SRGETH
     C                   WHEN      P#OPTI = '4'
     C                   EXSR      SRRESH
     C                   OTHER
     C                   EXSR      SREROR
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Close the program
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRGETH - Get helpdesk data (option 2 & 3)                   *  /
      *                                                             *
      * Get the helpdesk data for the specified helpdesk.           *
      * If the helpdesk is not filled, the current helpdesk         *
      * is retrieved. If no such helpdesk exists an error is        *
      * returned. If the option is '2', the helpdesk is made the    *
      * active helpdesk.                                            *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRGETH        BEGSR
      *
      *          ------------------------------------------
      *          Retrieve the current helpdesk code.
      *          ------------------------------------------
     C                   EXSR      SRGETS
      *
      *          ------------------------------------------
      *          Check if the incident monitor is running. If
      *          not then exit.
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          If this is a blank helpdesk, then fill it
      *          with the current helpdesk.
      *          ------------------------------------------
     C                   IF        P#HEDC = *BLANKS                             B01
      *
      *                    --------------------------------
      *                    Exit if an error occured and option '3'
      *                    --------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B02
     C                             AND P#OPTI = '3'
     C                   EVAL      P#STAT = C#NOTF
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Exit if no helpdesk active and retrieve only
      *                    --------------------------------
     C                   IF        D#NBA# = *ZEROS                              B02
     C                             AND D7HEDC = *BLANKS
     C                             AND P#OPTI = '3'
     C                   EVAL      P#STAT = C#NOTF
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Fill the current application.
      *                    --------------------------------
     C                   IF        D#NBA# = *ZEROS                              B02
     C                   MOVE      D7HEDC        P#HEDC
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Ask the user for the Helpdesk.
      *          ------------------------------------------
     C                   IF        P#HEDC = *BLANK                              B01
     C                             AND P#OPTI = '2'
      *
      *          ------------------------------------------
      *          Open the display and the application file
      *          ------------------------------------------
     C                   IF        I#DSP <> *ON                                 B02
     C                   OPEN      OMH029D
     C                   EVAL      I#DSP = *ON
     C                   ENDIF                                                  E02
      *
     C                   IF        I#HDD <> *ON                                 B02
     C                   OPEN      OMHDDL1
     C                   EVAL      I#HDD = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Enter loop to call screen 1 processing
      *          ------------------------------------------
     C                   EXSR      SRSC01
      *
      *          ------------------------------------------
      *          Quit when P#STAT is not *NORM
      *          ------------------------------------------
     C                   IF        P#STAT <> C#NORM                             B02
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill the entered helpdesk code
      *          ------------------------------------------
     C                   MOVE      FMHEDC        P#HEDC
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the data belonging to this helpdesk.
      *          ------------------------------------------
     C                   EXSR      SRPROC
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
      * This routine displays the first screen where the user must  *
      * enter the application code. If the application code does    *
      * not exist an appropiate message is send.                    *
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
      *          Display the format
      *          ------------------------------------------
     C                   EXFMT     OMH02901
      *
      *          ------------------------------------------
      *          F12 = F3
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                             OR *INKC = *ON                               B03
     C                   EVAL      P#STAT = C#EXIT
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
     C                   EVAL      *IN70 = *OFF
     C                   EVAL      EROR01 = '0'
     C                   EVAL      LOOP01 = '0'
      *
      *          ------------------------------------------
      *          Do edit routine if not F3.
      *          ------------------------------------------
     C                   IF        *INKD = *OFF                                 B03
     C                   EXSR      SRED01
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          F4 For List  entered
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   CALLP     OMX105 ( FMHEDC : HEDD )
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors found
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Do processing to fill status field
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
     C                   MOVE      *BLANKS       FMHEDC
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
      * This routine checks if the application code is correct.     *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          Check if application code exists
      *          ------------------------------------------
     C     FMHEDC        SETLL     OMHDDL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 <> *ON                                 B01
     C                   EVAL      MSGID = 'HLPE002'
     C                   EVAL      *IN70 = *ON
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = '1'
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
      * This routine fills the return data structure.               *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          If F3 pressed, then end
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B01
     C                   EVAL      P#STAT = C#TERM
     C                   ELSE                                                   X01
     C                   EVAL      P#STAT = C#NORM
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
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
     C                   CALLP     OMH901C ( MSGID : MSGF : MSGLIB : MSGDTA
     C                             )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPROC - Process this helpdesks data.                       *  /
      *                                                             *
      * This routine checks if the user is authorized. If not then  *
      * a window is shown if option '2' and interactive.            *
      * If not option '2' or in batch then return.                  *
      *                                                             *
      * If authorized then fill the parameters and return.          *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPROC        BEGSR
      *
      *          ------------------------------------------
      *          If not option '1'
      *          ------------------------------------------
     C                   IF        P#OPTI <> '1'                                B01
      *
      *          ------------------------------------------
      *          Get the helpdesk record
      *          ------------------------------------------
     C                   IF        I#HDD <> *ON                                 B02
     C                   OPEN      OMHDDL1
     C                   EVAL      I#HDD = *ON
     C                   ENDIF                                                  E02
     C     P#HEDC        CHAIN     OMHDDL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      P#STAT = C#NOTF
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if authorized.
      *          ------------------------------------------
     C                   IF        I#HDE <> *ON                                 B02
     C                   OPEN      OMHDEL1
     C                   EVAL      I#HDE = *ON
     C                   ENDIF                                                  E02
      *
     C                   MOVE      D7USID        K#USID
     C     $DE1K1        CHAIN     OMHDEL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *                    --------------------------------
      *                    Retrieve group authorisation.
      *                    --------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                             AND D7GRID <> C#NONE
     C                             AND D7GRID <> *BLANK
     C                   MOVE      D7GRID        K#USID
     C     $DE1K1        CHAIN     OMHDEL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Retrieve user class authorisation.
      *                    --------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   MOVE      D7USCL        K#USID
     C     $DE1K1        CHAIN     OMHDEL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Retrieve group class authorisation.
      *                    --------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                             AND D7GRCL <> C#NONE
     C                             AND D7GRCL <> *BLANK
     C                   MOVE      D7GRCL        K#USID
     C     $DE1K1        CHAIN     OMHDEL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Just give top level autority in this case
      *                    --------------------------------
     C                   IF        *IN99 = *ON
     C                   MOVE      '9'           DESUL#                         AJU
     C                   ENDIF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check if authorised for this Helpdesk.
      *          X means global authority.
      *          ------------------------------------------
     C**                 IF        P#SUL# = 'X'                                 B01
     C**                 IF        D7USCL = C#SECC                              B02
     C**                           OR D7USCL = C#ADMC
     C**                           OR D7GRCL = C#SECC
     C**                           OR D7GRCL = C#ADMC
     C**                 EVAL      P#STAT = C#NORM
     C**                 ELSE                                                   X02
     C**                 EVAL      P#STAT = C#AUTH
     C**                 ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Y means the system bosses and '9' users
      *          ------------------------------------------
     C**                 ELSE                                                   X01
     C**                 IF        P#SUL# = 'Y'                                 B02
     C**                 IF        D7USID = DDHDMC                              B03
     C**                           OR D7USCL = C#SECC
     C**                           OR D7USCL = C#ADMC
     C**                           OR D7GRCL = C#SECC
     C**                           OR D7GRCL = C#ADMC
     C**                           OR DESUL# = '9'
     C**                 EVAL      P#STAT = C#NORM
     C**                 ELSE                                                   X03
     C**                 EVAL      P#STAT = C#AUTH
     C**                 ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Otherwise the authorization must be as requested.
      *          allow everyone as the superboss for now
      *          ------------------------------------------
     C                   IF        P#SUL# = 'X'                                 B01
     C                             or p#sul# = 'Y'
     C                   EVAL      P#STAT = C#NORM
     C                   ELSE                                                   X02
     C                   IF        P#SUL# > DESUL#                              B03
     C                   EVAL      P#STAT = C#AUTH
     C                   ELSE                                                   X03
     C                   EVAL      P#STAT = C#NORM
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C**                 ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If not authorised, display the not authorised window
      *          if not in batch and opti = '2'
      *          ------------------------------------------
     C                   IF        P#STAT = C#AUTH                              B01
     C                             AND JI1JTC = 'I'
     C                   IF        I#DSP <> *ON                                 B02
     C                   OPEN      OMH029D
     C                   EVAL      I#DSP = *ON
     C                   ENDIF                                                  E02
     C                   EXFMT     OMH02902
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the current helpdesk and priority if '2'
      *          ------------------------------------------
     C                   IF        P#OPTI = '2'                                 B01
     C                             AND P#STAT = C#NORM
     C                   MOVE      P#HEDC        D7HEDC
     C                   MOVE      DESUL#        D7SUL#
     C                   EXSR      SRUPDS
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill the standard parameters.
      *          ------------------------------------------
     C                   MOVE      DESUL#        P#SUL#
      *
      *                    --------------------------------
      *                    Fill the optional parameters
      *                    --------------------------------
     C                   EXSR      SRPARM
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRESH - Reset the current helpdesk  (option 4)             *  /
      *                                                             *
      * This routine clears the current helpdesk.                   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRESH        BEGSR
      *
      *          ------------------------------------------
      *          Get the current values
      *          ------------------------------------------
     C                   EXSR      SRGETS
     C                   MOVE      *BLANKS       D7HEDC
     C                   MOVE      '0'           D7SUL#
     C                   EXSR      SRUPDS
      *
      *          ------------------------------------------
      *          Set return status.
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SREROR - Invalid OPTI received                                 /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SREROR        BEGSR
      *
      *          ------------------------------------------
      *          Fill the return parameters
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#TERM
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRCRTS - Create the user space                               * /
      *                                                              * /
      * ------------------------------------------------------------ *
     C     SRCRTS        BEGSR
      *
      *          ------------------------------------------
      *          Create the user space
      *          ------------------------------------------
     C                   EVAL      P#USPQ = C#OMSU
     C                   EVAL      P#EXAC = 'OMS'
     C                   EVAL      P#ISZ# = 2048
     C                   EVAL      P#IVAC = ' '
     C                   EVAL      P#PAUC = '*EXCLUDE'
     C                   EVAL      P#USPD = C#CRTO
     C                   EVAL      P#RPLC = '*NO'
     C                   CALLP     QUSCRTUS ( P#USPQ : P#EXAC : P#ISZ# :
     C                             P#IVAC : P#PAUC : P#USPD : P#RPLC :
     C                             P#ERR )
      *
      *          ------------------------------------------
      *          Exit if an error occurred.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      P#STAT = C#TERM
     C     10            DIV       NUL           NUL
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *
      *
      *          ------------------------------------------
      *          Retrieve the user information and fill it in the space
      *          ------------------------------------------
     C                   EVAL      D7USID = P#USID
     C                   CALLP     OMH905C ( D7USID : D7USCL : D7GRID :
     C                             D7GRCL : D7USDC : P#STAT )
     C                   IF        P#STAT <> C#NORM                             B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
     C                   MOVE      *BLANKS       D7HEDC
     C                   MOVE      '0'           D7SUL#
      *
     C                   EVAL      P#USPQ = C#OMSU
     C                   EVAL      P#STP# = 1
     C                   EVAL      P#LOB# = L#D7HD
     C                   EVAL      P#FRCC = '0'
     C                   CALLP(E)  QUSCHGUS ( P#USPQ : P#STP# : P#LOB# :
     C                             D#D7HD : P#FRCC )
     C                   EVAL      *IN99 = %ERROR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGETS - Get the contents of the space                       * /
      *                                                              * /
      * ------------------------------------------------------------ *
     C     SRGETS        BEGSR
      *
      *          ------------------------------------------
      *          Retrieve the current helpdesk code.
      *          ------------------------------------------
     C                   EVAL      P#USPQ = C#OMSU
     C                   EVAL      P#STP# = 1
     C                   EVAL      P#LOB# = L#D7HD
     C                   CALLP(E)  QUSRTVUS ( P#USPQ : P#STP# : P#LOB# :
     C                             D#D7HD : P#ERR )
     C                   EVAL      *IN99 = %ERROR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRUPDS - Update the space                                    * /
      *                                                              * /
      * ------------------------------------------------------------ *
     C     SRUPDS        BEGSR
      *
      *          ------------------------------------------
      *          Update it.
      *          ------------------------------------------
     C                   EVAL      P#USPQ = C#OMSU
     C                   EVAL      P#STP# = 1
     C                   EVAL      P#LOB# = L#D7HD
     C                   EVAL      P#FRCC = '0'
     C                   CALLP(E)  QUSCHGUS ( P#USPQ : P#STP# : P#LOB# :
     C                             D#D7HD : P#FRCC )
     C                   EVAL      *IN99 = %ERROR
      *
      *          ------------------------------------------
      *          Exit if an error occurred.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      P#STAT = C#TERM
     C     10            DIV       NUL           NUL
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
      * SRPARM - Fill the parameters                                *  /
      *                                                             *
      * This option fills the optional parameters. The parameters   *
      * to fill depends on the number of parameters passed.         *
      *                                                             *
      * D#PARM => 5  - HelpDesk portion                             *
      *        =  6  - User portion                                 *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPARM        BEGSR
      *
      *          ------------------------------------------
      *          If 5 or more parameters.
      *          ------------------------------------------
     C                   IF        D#PARM >= 5                                  B01
      *
      *          ------------------------------------------
      *          Fill the user parametes
      *          ------------------------------------------
     C                   MOVE      D#D7HD        P#DT1
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If 6 or more parameters.
      *          ------------------------------------------
     C                   IF        D#PARM >= 6                                  B01
     C                             AND P#OPTI <> '1'
      *
      *          ------------------------------------------
      *          Open the Helpdesk file if not open.
      *          ------------------------------------------
     C                   IF        I#HDD <> *ON                                 B02
     C                   OPEN      OMHDDL1
     C                   EVAL      I#HDD = *ON
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill the helpdesk parameters.
      *          ------------------------------------------
     C                   IF        P#HEDC <> DDHEDC                             B02
     C     P#HEDC        CHAIN     OMHDDL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF                                                  E02
      *
     C                   MOVE      DDHEDC        D8HEDC
     C                   MOVE      DDHEDD        D8HEDD
     C                   MOVE      DDHDMC        D8HDMC
     C                   MOVE      DDCTFP        D8CTFP
     C                   MOVE      DDCTFL        D8CTFL
     C                   MOVE      DDINFP        D8INFP
     C                   MOVE      DDINFL        D8INFL
     C                   MOVE      *BLANKS       D8DUM1
     C                   MOVEL     D#D8HD        P#DT2
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * *INZSR - Program initialization                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Indicator definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Key fields
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Save fields
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Work fields
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          API parameter definitions
      *          ------------------------------------------
     C/COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      *          ------------------------------------------
     C     $901C         PLIST
     C                   PARM                    MSGID
     C                   PARM      'OMHMSG'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGDTA
      *
      *          ------------------------------------------
      *          Parameter list for OMH902C (Clear message queue)
      *          ------------------------------------------
     C     $902C         PLIST
     C                   PARM                    PGMQ
      *
      *          ------------------------------------------
      *          Parameter list for OMX101  (Select Application)
      *          ------------------------------------------
     C     $X105         PLIST
     C                   PARM                    FMHEDC
     C                   PARM                    HEDD
      *
      *          ------------------------------------------
      *          Parameter list for OMH905C (Get user profile)
      *          ------------------------------------------
     C     $X905C        PLIST
     C                   PARM      P#USID        D7USID
     C                   PARM                    D7USCL
     C                   PARM                    D7GRID
     C                   PARM                    D7GRCL
     C                   PARM                    D7USDC
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH971C (Change owner)
      *          ------------------------------------------
     C     $X971C        PLIST
     C                   PARM                    P#OBJC
     C                   PARM                    P#OBJT
     C                   PARM                    P#OBJL
     C                   PARM                    P#OWN
     C                   PARM                    P#CURA
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH972C (Get owner)
      *          ------------------------------------------
     C     $X972C        PLIST
     C                   PARM                    P#OBJC
     C                   PARM                    P#OBJT
     C                   PARM                    P#OBJL
     C                   PARM                    P#OWN
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH043C2 (Check incident monitor)
      *          ------------------------------------------
     C     $043C2        PLIST
     C                   PARM      '0'           INDI
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          OMHDEL1 security file key list
      *          ------------------------------------------
     C     $DE1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    K#USID
      *
      *          ------------------------------------------
      *          Get the job information.
      *          ------------------------------------------
     C                   EVAL      P#LOB# = 86
     C                   EVAL      P#FMTC = 'JOBI0100'
     C                   EVAL      P#JOBN = '*'
     C                   EVAL      P#IJIC = *BLANKS
     C                   CALLP     QUSRJOBI ( P#JBI1 : P#LOB# : P#FMTC :
     C                             P#JOBN : P#IJIC )
      *
      *          ------------------------------------------
      *          Get the current application.
      *          ------------------------------------------
     C                   EVAL      P#USPQ = C#OMSU
     C                   EVAL      P#STP# = 1
     C                   EVAL      P#LOB# = L#D7HD
     C                   CALLP(E)  QUSRTVUS ( P#USPQ : P#STP# : P#LOB# :
     C                             D#D7HD : P#ERR )
     C                   EVAL      *IN99 = %ERROR
      *
      *          ------------------------------------------
      *          Create the user space if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   IF        D#MSID = 'CPF9801'                           B02
     C                   EXSR      SRCRTS
     C                   ELSE                                                   X02
     C                   EVAL      P#STAT = C#TERM
     C     10            DIV       NUL           NUL
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Save the requested authorisation code.
      *          ------------------------------------------
     C                   EVAL      S#SUL# = P#SUL#
      *
      *          ------------------------------------------
      *          Assume normal ending.
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
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
      * calling program and the SRTTLR routine is invoked.          *
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
     C                   WHEN      E#FILE = 'OMH029D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMHDDL1 '                          X02
     C                   MOVEL     HDDL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHDEL1 '                          X02
     C                   MOVEL     HDEL1         P#FILE
     C                   ENDSL
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
      *          Set return status
      *          ------------------------------------------
     C                   IF        D#PARM > *ZEROS
     C                   EVAL      P#STAT = C#TERM
     C                   ENDIF
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
      * This routine fills the data area which is used in the       *
      * CL program OMX001C. That program calls this program if this *
      * is the first time an user uses an OMS program.              *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          If not recursive calling.
      *          ------------------------------------------
     C                   IF        I#TTLR <> *ON                                B01
      *
      *          ------------------------------------------
      *          If not normal return.
      *          ------------------------------------------
     C                   IF        P#STAT <> C#NORM                             B02
     C                             AND P#OPTI = '2'
      *
      *                    --------------------------------
      *                    Not authorized.
      *                    --------------------------------
     C                   IF        P#STAT = C#AUTH                              B03
     C                   IF        S#SUL# <> 'X'                                B04
     C                   MOVE      'AUTE017'     P#MIDC
     C                   ELSE                                                   X04
     C                   MOVE      'AUTE016'     P#MIDC
     C                   ENDIF                                                  E04
     C                   EVAL      %SUBST(P#MSD:1:5) = P#HEDC
     C                   Z-ADD     5             P#LOB#
     C                   ENDIF                                                  E03
      *
      *                    --------------------------------
      *                    Not found.
      *                    --------------------------------
     C                   IF        P#STAT = C#NOTF                              B03
     C                   MOVE      'HLPE002'     P#MIDC
     C                   EVAL      P#LOB# = *ZEROS
     C                   ENDIF                                                  E03
      *
      *                    --------------------------------
      *                    Exit key pressed.
      *                    --------------------------------
     C                   IF        P#STAT = C#EXIT                              B03
     C                   MOVE      'FUNE001'     P#MIDC
     C                   EVAL      P#LOB# = *ZEROS
     C                   ENDIF                                                  E03
      *
      *                    --------------------------------
      *                    Other error.
      *                    --------------------------------
     C                   IF        P#STAT = C#TERM                              B03
     C                   MOVE      'HLPE010'     P#MIDC
     C                   EVAL      %SUBST(P#MSD:1:5) = P#HEDC
     C                   Z-ADD     5             P#LOB#
     C                   ENDIF                                                  E03
      *
      *                    --------------------------------
      *                    Send the message.
      *                    --------------------------------
     C                   EVAL      P#MSGQ = C#OMSG
     C                   EVAL      P#MSTC = '*INFO'
     C                   EVAL      P#PMQC = '*'
     C                   EVAL      P#PSC# = 1
     C                   CALLP     QMHSNDPM ( P#MIDC : P#MSGQ : P#MSD :
     C                             P#LOB# : P#MSTC : P#PMQC : P#PSC# :
     C                             P#MSKC : P#ERR )
      *
      *          ------------------------------------------
      *          If the previous application was filled, and not
      *          equal to this application.
      *          ------------------------------------------
     C                   IF        S#HEDC <> *BLANKS                            B03
     C                             AND S#HEDC <> P#HEDC                         B03
     C                             AND P#OPTI = '2'
     C                   EVAL      S#HED2 = P#HEDC
     C                   EVAL      P#HEDC = S#HEDC
     C                   EVAL      S#SUL2 = P#SUL#
     C                   EVAL      P#SUL# = S#SUL#
     C                   EVAL      S#STAT = P#STAT
     C                   EVAL      P#STAT = C#NORM
     C                   EVAL      I#TTLR = *ON
     C                   EXSR      SRGETH
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If this is a restore, then move the requested HEDC
      *          in the parameters.
      *          ------------------------------------------
     C                   IF        I#TTLR = *ON                                 B01
     C                   EVAL      P#HEDC = S#HED2
     C                   EVAL      P#SUL# = S#SUL2
      *
      *          ------------------------------------------
      *          If not normal, then the old helpdesk could not
      *          be restored. Send a warning.
      *          ------------------------------------------
     C                   IF        P#STAT <> C#NORM                             B02
     C                             AND P#OPTI = '2'
      *
      *                    --------------------------------
      *                    Send the message.
      *                    --------------------------------
     C                   EVAL      P#MIDC = 'WAR0015'
     C                   EVAL      P#MSGQ = C#OMSG
     C                   EVAL      P#MSD = S#HEDC
     C                   EVAL      P#LOB# = 4
     C                   EVAL      P#MSTC = '*INFO'
     C                   EVAL      P#PMQC = '*'
     C                   EVAL      P#PSC# = 1
     C                   CALLP     QMHSNDPM ( P#MIDC : P#MSGQ : P#MSD :
     C                             P#LOB# : P#MSTC : P#PMQC : P#PSC# :
     C                             P#MSKC : P#ERR )
     C                   ENDIF                                                  E02
      *
     C                   EVAL      P#STAT = S#STAT
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
      * ------------------------------------------------------------ *
