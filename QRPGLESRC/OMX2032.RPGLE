     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMX2032                                       *
      * Function ...: User option maintenance exit program          *
      * Module .....: HelpDesk                                      *
      * Author .....: Remain Software                               *
      * Parameters .: P#ILPL - Single parameter for the exit pgm    *
      *                                                             *
      * The incomplete list handler.                                *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used files                                                  *
      * ----------------------------------------------------------- *
      * OMHUOL1  - User option file                                 *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators                                             *
      * ----------------------------------------------------------- *
      *                                                             *
      * 99     - General purpose indicator.                         *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Files                                                       *
      * ----------------------------------------------------------- *
     FOMHUOL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     USROPN
     F                                     INFDS(HUOL1)
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Input specifications                                        *
      * ----------------------------------------------------------- *
      *
      *COPY QCPYSRC,OMSREFREN
      *          ------------------------------------------
      *          Reference file is used for field definition
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          File information data structures
      *          ------------------------------------------
     D HUOL1           DS            57
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  E#MSID                40     46
     D  E#FILE               201    208
     D  D#USID               254    263
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
      *          Input buffer (7Kb)
      *          ------------------------------------------
     D P#BUF           DS          7168
      *
      *          ------------------------------------------
      *          Buffer for VR0001
      *          Constant holding the length of this buffer
      *          ------------------------------------------
     D V#0001          DS
     D FMCSUL                 61     61
     D L#VR01          C                   CONST(61)
      *
      *          ------------------------------------------
      *          Data buffer
      *          ------------------------------------------
     D V#0103          DS
     D UOUOPC                         2
     D UOUOPD                        30
     D UOSUL#                         1
     D UOPGMN                        10
     D UOPGML                        10
     D UOPRMI                         1
     D UOINTL                         8
     D FMOPTI                         4B 0
      *
     D L#VR13          C                   CONST(64)
      *
      *          ------------------------------------------
      *          Incomplete list exit program single parameter.
      *
      *          ILSLV# (B4)     Structure level
      *          ILRES1 (A8)     Reserved
      *          ILTOC# (B4)     Type of call
      *          ILHNDC (A10)    Application handle
      *          ILRES2 (A10)    Reserved
      *          ILLSTC (A10)    List code
      *          ILILD# (B4)     Incomplete list direction
      *          ILNER# (B4)     Number of Entries Required
      *          ------------------------------------------
     D P#ILPL          DS
     D ILSLV#                         9B 0
     D ILTOC#                 13     16B 0
     D ILHNDC                         8
     D ILLSTC                 35     44
     D ILILD#                         9B 0
     D ILNER#                         9B 0
      * Prototype for 'QUIADDLE'
     D QUIADDLE        PR                  EXTPGM('QUIADDLE')
     D P#HNDC_                             LIKE(P#HNDC)
     D V#0103_                             LIKEDS(V#0103)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#VRNC_                             LIKE(P#VRNC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#OPTC_                             LIKE(P#OPTC)
     D P#LEHC_                             LIKE(P#LEHC)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'QUIGETV'
     D QUIGETV         PR                  EXTPGM('QUIGETV')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#BUF_                              LIKEDS(P#BUF)
     D P#LOB#_                             LIKE(P#LOB#)
     D P#VRNC_                             LIKE(P#VRNC)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'OMH901C '
     D OMH901C         PR                  EXTPGM('OMH901C')
     D D#MSID_                             LIKE(D#MSID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D D#MSGD_                             LIKE(D#MSGD)
      *
      *          ------------------------------------------
      *          Fill the panelgroup parameters
      * Prototype for 'QUISETLA'
     D QUISETLA        PR                  EXTPGM('QUISETLA')
     D P#HNDC_                             LIKE(P#HNDC)
     D P#LSTC_                             LIKE(P#LSTC)
     D P#LCOC_                             LIKE(P#LCOC)
     D P#PDVC_                             LIKE(P#PDVC)
     D P#DPAC_                             LIKE(P#DPAC)
     D P#ATAC_                             LIKE(P#ATAC)
     D P#ERR_                              LIKE(P#ERR)
      * Prototype for 'OMH961'
     D OMH961          PR                  EXTPGM('OMH961')
     D PGM_                                LIKEDS(PGM)
     D FILE_                               LIKE(FILE)
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
     D EOFAPP          S                   LIKE(INDI)
     D EOFSYS          S                   LIKE(INDI)
     D FILE            S             57
     D I#APP           S                   LIKE(INDI)
     D I#EOF1          S                   LIKE(INDI)
     D I#OPTO          S                   LIKE(INDI)
     D I#PSSR          S              1
     D I#SYS           S                   LIKE(INDI)
     D IX              S              9  0
     D K#OBJT          S                   LIKE(OBJT)
     D MSGLIB          S             10
     D P#MSTA          S             40
     D W#NEP           S              9  0
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
      *          Get the parameter
      *          ------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    P#ILPL
      *
      *          ------------------------------------------
      *          If no parameters then exit.
      *          ------------------------------------------
     C                   IF        D#PARM <> 1                                  B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check what to do.
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      ILTOC# = 6
     C                   EXSR      SRINCL
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Return to caller
      *          ------------------------------------------
     C                   RETURN
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRINCL - Incomplete list handler                             * /
      *                                                              * /
      * This routine handles the incomplete list.                    * /
      *                                                              *
      * This program reads records from the request file and adds    *
      * them to the list.                                            *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRINCL        BEGSR
      *
      *          ------------------------------------------
      *          Check if the file is open. If not then open it.
      *          ------------------------------------------
     C                   IF        I#OPTO <> *ON                                B01
     C                   EVAL      I#OPTO = *ON
     C                   OPEN      OMHUOL1
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do number of entries required
      *          ------------------------------------------
     C                   EVAL      IX = *ZEROS
     C                   DOW       IX <> ILNER#
      *
      *          ------------------------------------------
      *          Read a record from the file.
      *          ------------------------------------------
     C                   READ      OMHUOL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If this is the last entry then set list to complete
      *          and return.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   EXSR      SRCMPL
     C                   RETURN
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Add list entry if authorised and active.
      *          ------------------------------------------
     C                   IF        FMCSUL = '9'                                 B02
     C                             OR FMCSUL >= UOSUL#
     C                   EVAL      FMOPTI = *ZEROS
     C                   EXSR      SRADLE
     C                   ADD       1             IX
     C                   ENDIF                                                  E02
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRCMPL - Mark list as complete                               * /
      *                                                              * /
      * This routine marks the list as beeing complete by calling    * /
      * the set list attribute (QUISETLA) API.                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRCMPL        BEGSR
      *
      *          ------------------------------------------
      *          Mark a complete list.
      *          ------------------------------------------
     C                   MOVE      'ALL '        P#LCOC
     C                   MOVEL     '*SAME '      P#PDVC
     C                   MOVE      'SAME'        P#DPAC
     C                   MOVE      'Y'           P#ATAC
     C                   EXSR      SRSLAT
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRADLE - Add list entry                                      * /
      *                                                              * /
      * The add list entry (QUIADDLE) API adds one new entry to a    * /
      * list.                                                        * /
      *                                                              * /
      * Input        P#HNDC  Handle code                             *
      *              P#BUF   Variable buffer                         *
      *              P#LOB#  Length of buffer                        *
      *              P#VRNC  Variable record name                    *
      *              P#LSTC  List code                               *
      *              P#OPTC  Option                                  *
      *              P#ERR   Error parameter data structure          *
      *                                                              *
      * Output     - P#LEHC  List entry handle                       *
      *              P#ERR#  Record count                            *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRADLE        BEGSR
      *
      *          ------------------------------------------
      *          Add list entry
      *          ------------------------------------------
     C                   EVAL      P#LOB# = L#VR13
     C                   EVAL      P#VRNC = 'VR0103'
     C                   EVAL      P#OPTC = 'LAST'
     C                   CALLP     QUIADDLE ( P#HNDC : V#0103 : P#LOB# :
     C                             P#VRNC : P#LSTC : P#OPTC : P#LEHC :
     C                             P#ERR )
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGETV - Get dialog variable                                 * /
      *                                                              * /
      * This routine calls the get dialog variable API (QUIGETV)     * /
      * which gets a variable record.                                * /
      *                                                              *
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
     C     SRGETV        BEGSR
      *
      *          ------------------------------------------
      *          Get dialog variable
      *          ------------------------------------------
     C                   CALLP     QUIGETV ( P#HNDC : P#BUF : P#LOB# :
     C                             P#VRNC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C  ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
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
      * SRSLAT - Set list attributes                                 * /
      *                                                              * /
      * This routine calls the set list attribute API (QUISETLA)     * /
      * which sets the list attributes.                              * /
      *                                                              * /
      * Input      - P#HNDC  Application handle                      *
      *              P#LSTC  List name                               *
      *              P#LCOC  List contents code                      *
      *              P#PDVC  Program dialog variable                 *
      *              P#DPAC  Display position attribute              *
      *              P#ATAC  Allow trim attribute code               *
      *                                                              *
      * Output     - *NONE                                           *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSLAT        BEGSR
      *
      *          ------------------------------------------
      *          Add list entry
      *          ------------------------------------------
     C                   CALLP     QUISETLA ( P#HNDC : P#LSTC : P#LCOC :
     C                             P#PDVC : P#DPAC : P#ATAC : P#ERR )
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C                   IF        D#NBA# <> *ZEROS                             B01
     C                   EVAL      MSGF = 'QCPFMSG'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C  ( D#MSID : MSGF : MSGLIB :
     C                             D#MSGD )
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
      * SRINIT - Program initialisation                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          If no parameters then exit.
      *          ------------------------------------------
     C                   IF        D#PARM <> 1                                  B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      W#NEP = *ZEROS
     C                   EVAL      IX = *ZEROS
      *
      *          ------------------------------------------
      *          Indicator definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Keyfield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          API parameter definition.
      *
      *          All fields start with 'P#', followed by a three
      *          character meaningfull code and end with;
      *
      *          # -  4 byte binary number
      *          Q - 20 byte qualifier
      *          C - xx byte code field
      *          D - xx byte description field
      *            - Buffer field
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      *          ------------------------------------------
     C     $901C2        PLIST
     C                   PARM                    D#MSID
     C                   PARM      'QCPFMSG'     MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    D#MSGD
      *
      *          ------------------------------------------
      *          Fill the panelgroup parameters
      *          ------------------------------------------
     C                   MOVE      ILHNDC        P#HNDC
     C                   MOVE      ILLSTC        P#LSTC
      *
      *          ------------------------------------------
      *          Get the authorisation code
      *          ------------------------------------------
     C                   MOVEL     'VR0001'      P#VRNC
     C                   Z-ADD     L#VR01        P#LOB#
     C                   EXSR      SRGETV
     C                   MOVEL     P#BUF         V#0001
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
      * calling program and the *CANCL procedure is invoked.        *
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
     C                   IF        E#FILE = 'OMHUOL1 '                          B02
     C                   MOVEL     HUOL1         FILE
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Call the error handler
      *          ------------------------------------------
     C                   CALLP     OMH961 ( PGM : FILE )
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
      *          Close
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
      * SRTTLR - Close the program                                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          Close and return.
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
