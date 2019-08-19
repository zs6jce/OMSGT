
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH961                                        *
      * Function ...: Error handler                                 *
      * Author .....: Remain                                        *
      * Parameters .: P#PSDS  Program status data structure of      *
      *                       calling program.                      *
      *                                                             *
      * This program prints the status data structure of the        *
      * calling program.                                            *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used files                                                  *
      * ----------------------------------------------------------- *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Sub programs                                                *
      * ----------------------------------------------------------- *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators                                             *
      * ----------------------------------------------------------- *
      *                                                             *
      * 99 - General purpose indicator.                             *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Arrays                                                      *
      * MSD   Contains variable message id (compile time array)     *
      * VAR   Contains variable message text                        *
      * FT    Formatted message text                                *
      * FSC   File status codes                                     *
      * FSD   File status descriptions                              *
      * PSC   Program status codes                                  *
      * PSD   Program status descriptions                           *
      * ----------------------------------------------------------- *
     D MC              S              1    DIM(256)
     D MSD             S              7    DIM(20) CTDATA PERRCD(1)
     D VAR             S            256    DIM(20)
     D FT              S              1    DIM(8000)
     D FSC             S              5    DIM(38) CTDATA PERRCD(1)
     D FSD             S             70    DIM(38) CTDATA PERRCD(1)
     D PSC             S              5    DIM(30) CTDATA PERRCD(1)
     D PSD             S             70    DIM(30) CTDATA PERRCD(1)
      * ----------------------------------------------------------- *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Input pecifications                                         *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          The field reference file is used for field definitions
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREF)
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  D#JOBN               244    253
     D  D#USID               358    367
     D  D#JOB#               264    269  0
      *
      *          ------------------------------------------
      *          Buffer
      *          ------------------------------------------
     D P#BUF           DS          6144
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
      *          Buffer for VR0101
      *          + Constant holding the length of this buffer
      *          ------------------------------------------
     D V#0101          DS
     D  B1PGMC                 1     10    INZ('OMH961')
     D  B1JOBN                11     20
     D  B1USID                21     30
     D  B1JOB#                31     36  0
     D L#VR11          C                   CONST(36)
      *
      *          ------------------------------------------
      *          Buffer for VR0102
      *          + Constant holding the length of this buffer
      *          ------------------------------------------
     D V#0102          DS           429
     D  FMPGMC                 1     10
     D  FMPSTC                11     15
     D  FMPSTD               300    369
     D L#VR12          C                   CONST(369)
      *
      *          ------------------------------------------
      *          Buffer for VR0103
      *          + Constant holding the length of this buffer
      *          ------------------------------------------
     D P#0103          DS            57
     D V#0103          DS           127
     D  FMFSTC                11     15
     D  FMFSTD                58    127
     D L#VR13          C                   CONST(127)
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#PNLQ          C                   CONST('OMH961P   *LIBL     ')
     D C#PDFQ          C                   CONST('QSYSPRT   *LIBL     ')
     D C#MSGQ          C                   CONST('OMHMSG    *LIBL     ')
     D C#NONE          C                   CONST('*NONE')
     D C#CMDC          C                   CONST('OMSDUMP   ')
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Main line                                                   *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Program parameters
      *          ------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    V#0102
     C                   PARM                    P#0103
      *
      *          ------------------------------------------
      *          Do processing
      *          ------------------------------------------
     C                   EXSR      SRVERW
      *
      *          ------------------------------------------
      *          Do the total last record routine
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRVERW - Processing                                         *
      *                                                             *
      * This routine prints the error report.                       *
      * ----------------------------------------------------------- *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Look the program status code up.
      *          ------------------------------------------
     C                   Z-ADD     1             IX                5 0
     C     FMPSTC        LOOKUP    PSC(IX)                                99
     C   99              MOVE      PSD(IX)       FMPSTD
     C  N99              MOVEL(P)  '*?*'         FMPSTD
      *
      *          ------------------------------------------
      *          Fill the data.
      *          ------------------------------------------
     C                   MOVEL     V#0102        P#BUF
     C                   Z-ADD     L#VR12        P#LOB#
     C                   MOVEL     'VR0102'      P#VRNC
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Fill the data.
      *          ------------------------------------------
     C     D#PARM        IFGT      1                                            B01
      *
      *                    --------------------------------
      *                    Look the file status code up.
      *                    --------------------------------
     C                   Z-ADD     1             IX                5 0
     C                   MOVEL     P#0103        V#0103
     C     FMFSTC        LOOKUP    FSC(IX)                                99
     C   99              MOVE      FSD(IX)       FMFSTD
     C  N99              MOVEL(P)  '*?*'         FMFSTD
      *
     C                   MOVEL     V#0103        P#BUF
     C                   Z-ADD     L#VR13        P#LOB#
     C                   MOVEL     'VR0103'      P#VRNC
     C                   EXSR      SRPUTV
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Print the data
      *          ------------------------------------------
     C                   MOVEL     'PA02'        P#PNLC
     C                   MOVE      'N'           P#EJOC
     C                   EXSR      SRPRTP
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRCLAP - Close application                                   * /
      *                                                              * /
      * This routine closes the application by calling the close     * /
      * application API.                                             * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      *                                                              *
      * This subroutine sets the following default parameters for    *
      * this API call.                                               *
      *                                                              *
      * P#CLOS = 'M' Perform normal close operation                  *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRCLAP        BEGSR
      *
      *          ------------------------------------------
      *          Close the display application
      *          ------------------------------------------
     C                   CALL      'QUICLOA'
     C                   PARM                    P#HNDC
     C                   PARM      'M'           P#CLOC
     C                   PARM                    P#ERR
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C     D#MSID        IFNE      *BLANKS                                      B01
     C                   CALL      'OMH901C'     $901C2
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSPGM - Send program message                                * /
      *                                                              * /
      * This routine calls the send program message API (QMHSNDPM).  * /
      *                                                              * /
      * Input      - P#MIDC  Message id code                         *
      *              P#MSGQ  Message file qualifier                  *
      *              P#MDT   Message data or impromptu text buffer   *
      *              P#MDL#  Length of buffer                        *
      *              P#MSTC  Message type code                       *
      *              P#PMQC  Program message queue qualifier         *
      *              P#PSC#  Program stack counter                   *
      *              P#ERR   Error buffer                            *
      *                                                              *
      * Output       P#MSKC  Message key code                        *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSPGM        BEGSR
      *
      *          ------------------------------------------
      *          Send program message
      *          ------------------------------------------
     C                   CALL      'QMHSNDPM'
     C                   PARM                    P#MIDC
     C                   PARM                    P#MSGQ
     C                   PARM                    P#MDT
     C                   PARM                    P#MDL#
     C                   PARM                    P#MSTC
     C                   PARM                    P#PMQC
     C                   PARM                    P#PSC#
     C                   PARM                    P#MSKC
     C                   PARM                    P#ERR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SROPPA - Open print application.                             * /
      *                                                              * /
      * This routine calls the open print application API QUIOPNPA   * /
      *                                                              *
      * Input      - P#PNLQ  Qualified panelgroup name               *
      *              P#APS#  Application scope                       *
      *              P#ILV#  Interface level                         *
      *              P#PDFQ  Printer device file qualifier           *
      *              P#AFNC  Alternate file name                     *
      *              P#SODC  Share open data path code               *
      *              P#USDT  User data                               *
      *                                                              *
      * Output     - P#HNDC  Application handle                      *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SROPPA        BEGSR
      *
      *          ------------------------------------------
      *          Open print application
      *          ------------------------------------------
     C                   CALL      'QUIOPNPA'
     C                   PARM                    P#HNDC
     C                   PARM                    P#PNLQ
     C                   PARM                    P#APS#
     C                   PARM                    P#ILV#
     C                   PARM                    P#PDFQ
     C                   PARM                    P#AFNC
     C                   PARM                    P#SODC
     C                   PARM                    P#USDT
     C                   PARM                    P#ERR
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C     D#MSID        IFNE      *BLANKS                                      B01
     C                   CALL      'OMH901C'     $901C2
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
      * SRPRTP - Print panel.                                        * /
      *                                                              * /
      * This routine prints a panel by calling the QUIPRTP API.      * /
      *                                                              *
      * Input      - P#HNDC  Application handle                      *
      *              P#PNLC  Panel name                              *
      *              P#EJOC  Eject option                            *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPRTP        BEGSR
      *
      *          ------------------------------------------
      *          Print the panel
      *          ------------------------------------------
     C                   CALL      'QUIPRTP'
     C                   PARM                    P#HNDC
     C                   PARM                    P#PNLC
     C                   PARM                    P#EJOC
     C                   PARM                    P#ERR
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C     D#MSID        IFNE      *BLANKS                                      B01
     C                   CALL      'OMH901C'     $901C2
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
      * SRPUTV - Put dialog variable                                 * /
      *                                                              * /
      * This routine calls the put dialog variable API (QUIPUTV)     * /
      * which updates a variable record.                             * /
      *                                                              * /
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
     C     SRPUTV        BEGSR
      *
      *          ------------------------------------------
      *          Put dialog variable
      *          ------------------------------------------
     C                   CALL      'QUIPUTV'
     C                   PARM                    P#HNDC
     C                   PARM                    P#BUF
     C                   PARM                    P#LOB#
     C                   PARM                    P#VRNC
     C                   PARM                    P#ERR
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C     D#MSID        IFNE      *BLANKS                                      B01
     C                   CALL      'OMH901C'     $901C2
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
      * *INZSR - Program initialisation                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Use 99
      *          ------------------------------------------
     C   99              SETOFF                                       99
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
     C     *LIKE         DEFINE    IRPC          P#IRPC
      *
      *          ------------------------------------------
      *          Other API Parameter Definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter list for OMH904C (Get message data text)
      *          ------------------------------------------
     C     $X904C        PLIST
     C                   PARM                    MSGID             7
     C                   PARM      'OMHSSP'      MSGF
     C                   PARM      '*LIBL'       MSGLIB           10
     C                   PARM                    MSGTXT          132
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      *          ------------------------------------------
     C     $901C2        PLIST
     C                   PARM                    D#MSID
     C                   PARM      'QCPFMSG'     MSGF
     C                   PARM      '*LIBL'       OBJL
     C                   PARM                    D#MSGD
      *
      *          ------------------------------------------
      *          Open the print application
      *          ------------------------------------------
     C                   MOVE      C#PNLQ        P#PNLQ
     C                   Z-ADD     -1            P#APS#
     C                   Z-ADD     2             P#ILV#
     C                   MOVE      C#PDFQ        P#PDFQ
     C                   MOVEL     C#CMDC        P#AFNC
     C                   MOVE      'N'           P#SODC
     C                   MOVEL     C#NONE        P#USDT
     C                   EXSR      SROPPA
      *
      *          ------------------------------------------
      *          Get the messages
      *          ------------------------------------------
     C                   EXSR      SRGVAR
      *
      *          ------------------------------------------
      *          Fill the prolog data
      *          ------------------------------------------
     C                   MOVE      D#JOB#        B1JOB#
     C                   MOVE      D#JOBN        B1JOBN
     C                   MOVE      D#USID        B1USID
      *
     C                   MOVEL     V#0101        P#BUF
     C                   Z-ADD     L#VR11        P#LOB#
     C                   MOVEL     'VR0101'      P#VRNC
     C                   EXSR      SRPUTV
      *
      *          ------------------------------------------
      *          Print the header.
      *          ------------------------------------------
     C                   MOVEL     'PA01'        P#PNLC
     C                   MOVE      'Y'           P#EJOC
     C                   EXSR      SRPRTP
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
      *          Send cancel message
      *          ------------------------------------------
     C                   MOVE      'ERR0017'     P#MIDC
     C                   MOVE      C#MSGQ        P#MSGQ
     C                   MOVEL     V#0102        P#MDT            10
     C                   Z-ADD     10            P#MDL#
     C                   MOVEL(P)  '*DIAG'       P#MSTC
     C                   MOVEL     '*     '      P#PMQC
     C                   Z-ADD     1             P#PSC#
     C                   EXSR      SRSPGM
      *
      *          ------------------------------------------
      *          Send cancel message again to log it
      *          ------------------------------------------
     C                   MOVEL     '*INFO'       P#MSTC
     C                   Z-ADD     0             P#PSC#
     C                   EXSR      SRSPGM
      *
      *          ------------------------------------------
      *          Log it.
      *          ------------------------------------------
     C                   CALL      'OMH928C'
      *
      *          ------------------------------------------
      *          Close the application.
      *          ------------------------------------------
     C                   EXSR      SRCLAP
      *
      *          ------------------------------------------
      *          Seton last record
      *          ------------------------------------------
     C                   SETON                                        LR
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
      *          Get all message id texts
      *          ------------------------------------------
     C                   DO        20            MS                5 0          B01
     C                   MOVE      MSD(MS)       MSGID
      *
      *          ------------------------------------------
      *          Call subprogram
      *          ------------------------------------------
     C     MSGID         IFNE      *BLANKS                                      B02
     C                   CALL      'OMH904C'     $X904C
     C                   END                                                    E02
      *
      *          ------------------------------------------
      *          Fill message array
      *          ------------------------------------------
     C                   MOVEL     MSGTXT        VAR(MS)
     C                   END                                                    E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Array MSD                                                    *
      * ------------------------------------------------------------ *
**
MX96101  -  Print header
**
00000
00002
00011
00012
00013
01011
01021
01031
01041
01051
01052
01071
01121
01122
01123
01124
01125
01126
01201
01211
01215
01216
01217
01218
01221
01231
01235
01241
01251
01255
01261
01281
01282
01285
01286
01287
01299
01331
**
No exception/error.
Function key used to end display.
End of file on a read (input).
No-record-found condition on a CHAIN, SETLL and SETGT operations
Subfile is full on WRITE operation.
Undefined record type.
Tried to write a record that already exists.
Match field out of sequence.
Array/table load sequence error.
Excess entries in array/table file.
Clearing of table prior to dump of data failed.
Numeric sequence error.
No indicator on the DDS keyword for Print key.
No indicator on the DDS keyword for Roll-up key.
No indicator on the DDS keyword for Roll-down key.
No indicator on the DDS keyword for Clear key.
No indicator on the DDS keyword for Help key.
No indicator on the DDS keyword for Home key.
Record mismatch detected on input.
I/O operation to a closed file.
OPEN issued to a file already opened.
Error on an implicit OPEN/CLOSE operation.
Error on an explicit OPEN/CLOSE operation.
Record already locked.
Update operation attempted without a prior read.
Error on SPECIAL file.
Error in PRTCTL space or skip entries.
Record number not found.
Permanent I/O error occurred.
Session or device error occurred. Recovery may be possible.
Attempt to exceed maximum number of acquired devices.
Operation to unacquired device.
Job ending with controlled option.
Attempt to acquire a device already acquired.
Attempt to open shared file with SAVDS or IND options.
Response indicators overlap IND indicators
Other I/O error detected.
Wait time exceeded for READ from WORKSTN file.
**
00000
00001
00100
00101
00102
00121
00122
00123
00202
00211
00221
00231
00232
00233
00299
00333
00401
00402
00411
00412
00413
00414
00415
00421
00431
00432
00907
00970
09998
09999
**
No exception/error occurred.
Called program returned with the LR indicator on.
Value out of range for string operation.
Negative square root.
Divide by zero.
Array index not valid.
OCUR outside of range.
Reset attempted during initialization step of program.
Called program failed; halt indicator (H1 through H9) not on.
Program specified on CALL or FREE not found
Called program tried to use a parameter that was not passed
Called program returned with halt indicator on
Halt indicator on in this program
Halt indicator on when RETRN operation run
RPG/400 formatted dump failed
Error on DSPLY operation
Data area specified on IN/OUT not found
*PDA not valid for non-prestart job
Data area type or length does not match
Data area not locked for output
Error on IN/OUT operation
User not authorized to use data area
User not authorized to change data area
Error on UNLCK operation
Data area previously locked by another program
Data area locked by program in the same process
Decimal data error (digit or sign not valid)
The level number of the compiler is mismatching.
Internal failure in RPG/400 compiler or in run-time subroutine
Program exception in system routine.
